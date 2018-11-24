{-# LANGUAGE FlexibleContexts, CPP #-}

------------------------------------------------------------------------------
-- |
-- Module: Configuration
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Wed Nov 21, 2018 23:13
--
--
-- Parsing configuration files
--
------------------------------------------------------------------------------


module Configuration (readConfig, readDefaultConfig) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Number (int)
import Text.ParserCombinators.Parsec.Perm ((<|?>), (<$?>), permute)
import Control.Monad.IO.Class (liftIO)

import System.Posix.Files
import System.FilePath ((</>))
import System.Environment
import System.Directory (getHomeDirectory)

import Xmobar.Config

import qualified Xmobar.Config as C

#if defined XFT || defined UTF8
import qualified System.IO as S (readFile,hGetLine)
#endif

readFileSafe :: FilePath -> IO String
#if defined XFT || defined UTF8
readFileSafe = S.readFile
#else
readFileSafe = readFile
#endif

stripComments :: String -> String
stripComments =
  unlines . map (drop 5 . strip False . (replicate 5 ' '++)) . lines
    where strip m ('-':'-':xs) = if m then "--" ++ strip m xs else ""
          strip m ('"':xs) = '"': strip (not m) xs
          strip m (x:xs) = x : strip m xs
          strip _ [] = []

-- | Parse the config, logging a list of fields that were missing and replaced
-- by the default definition.
parseConfig :: String -> Either ParseError (C.Config,[String])
parseConfig = runParser parseConf fields "Config" . stripComments
    where
      parseConf = do
        many space
        sepEndSpc ["Config","{"]
        x <- perms
        eof
        s <- getState
        return (x,s)

      perms = permute $ C.Config
              <$?> pFont <|?> pFontList <|?> pWmClass <|?> pWmName
              <|?> pBgColor <|?> pFgColor
              <|?> pPosition <|?> pTextOffset <|?> pTextOffsets
              <|?> pIconOffset <|?> pBorder
              <|?> pBdColor <|?> pBdWidth <|?> pAlpha <|?> pHideOnStart
              <|?> pAllDesktops <|?> pOverrideRedirect <|?> pPickBroadest
              <|?> pLowerOnStart <|?> pPersistent <|?> pIconRoot
              <|?> pCommands <|?> pSepChar <|?> pAlignSep <|?> pTemplate


      fields    = [ "font", "additionalFonts","bgColor", "fgColor"
                  , "wmClass", "wmName", "sepChar"
                  , "alignSep" , "border", "borderColor" ,"template"
                  , "position" , "textOffset", "textOffsets", "iconOffset"
                  , "allDesktops", "overrideRedirect", "pickBroadest"
                  , "hideOnStart", "lowerOnStart", "persistent", "iconRoot"
                  , "alpha", "commands"
                  ]

      pFont = strField C.font "font"
      pFontList = strListField C.additionalFonts "additionalFonts"
      pWmClass = strField C.wmClass "wmClass"
      pWmName = strField C.wmName "wmName"
      pBgColor = strField C.bgColor "bgColor"
      pFgColor = strField C.fgColor "fgColor"
      pBdColor = strField C.borderColor "borderColor"
      pSepChar = strField C.sepChar "sepChar"
      pAlignSep = strField C.alignSep "alignSep"
      pTemplate = strField C.template "template"

      pTextOffset = readField C.textOffset "textOffset"
      pTextOffsets = readIntList C.textOffsets "textOffsets"
      pIconOffset = readField C.iconOffset "iconOffset"
      pPosition = readField C.position "position"
      pHideOnStart = readField C.hideOnStart "hideOnStart"
      pLowerOnStart = readField C.lowerOnStart "lowerOnStart"
      pPersistent = readField C.persistent "persistent"
      pBorder = readField C.border "border"
      pBdWidth = readField C.borderWidth "borderWidth"
      pAllDesktops = readField C.allDesktops "allDesktops"
      pOverrideRedirect = readField C.overrideRedirect "overrideRedirect"
      pPickBroadest = readField C.pickBroadest "pickBroadest"
      pIconRoot = readField C.iconRoot "iconRoot"
      pAlpha = readField C.alpha "alpha"

      pCommands = field C.commands "commands" readCommands

      staticPos = do string "Static"
                     wrapSkip (string "{")
                     p <- many (noneOf "}")
                     wrapSkip (string "}")
                     string ","
                     return ("Static {"  ++ p  ++ "}")
      tillFieldEnd = staticPos <|> many (noneOf ",}\n\r")

      commandsEnd  = wrapSkip (string "]") >> (string "}" <|> notNextRun)
      notNextRun = do {string ","
                      ; notFollowedBy $ wrapSkip $ string "Run"
                      ; return ","
                      }
      readCommands = manyTill anyChar (try commandsEnd) >>=
                        read' commandsErr . flip (++) "]"
      strField e n = field e n strMulti

      strMulti = scan '"'
          where
            scan lead = do
                spaces
                char lead
                s <- manyTill anyChar (rowCont <|> unescQuote)
                (char '"' >> return s) <|> fmap (s ++) (scan '\\')
            rowCont    = try $ char '\\' >> string "\n"
            unescQuote = lookAhead (noneOf "\\") >> lookAhead (string "\"")

      strListField e n = field e n strList
      strList = do
        spaces
        char '['
        list <- sepBy (strMulti >>= \x -> spaces >> return x) (char ',')
        spaces
        char ']'
        return list

      wrapSkip   x = many space >> x >>= \r -> many space >> return r
      sepEndSpc    = mapM_ (wrapSkip . try . string)
      fieldEnd     = many $ space <|> oneOf ",}"
      field  e n c = (,) (e C.defaultConfig) $
                     updateState (filter (/= n)) >> sepEndSpc [n,"="] >>
                     wrapSkip c >>= \r -> fieldEnd >> return r
      readField a n = field a n $ tillFieldEnd >>= read' n

      readIntList d n = field d n intList
      intList = do
        spaces
        char '['
        list <- sepBy (spaces >> int >>= \x-> spaces >> return x) (char ',')
        spaces
        char ']'
        return list

      read' d s = case reads s of
                    [(x, _)] -> return x
                    _ -> fail $ "error reading the " ++ d ++ " field: " ++ s

commandsErr :: String
commandsErr = "commands: this usually means that a command could not" ++
              "\nbe parsed." ++
              "\nThe error could be located at the begining of the command" ++
              "\nwhich follows the offending one."

-- | Reads the configuration files or quits with an error
readConfig :: FilePath -> String -> IO (Config,[String])
readConfig f usage = do
  file <- liftIO $ fileExist f
  s <- liftIO $ if file then readFileSafe f else error $
                  f ++ ": file not found!\n" ++ usage
  either (\err -> error $ f ++
                    ": configuration file contains errors at:\n" ++ show err)
         return $ parseConfig s

xdgConfigDir :: IO String
xdgConfigDir = do env <- getEnvironment
                  case lookup "XDG_CONFIG_HOME" env of
                       Just val -> return val
                       Nothing  -> fmap (</> ".config") getHomeDirectory

xmobarConfigDir :: IO FilePath
xmobarConfigDir = fmap (</> "xmobar") xdgConfigDir

getXdgConfigFile :: IO FilePath
getXdgConfigFile = fmap (</> "xmobarrc") xmobarConfigDir

-- | Read default configuration file or load the default config
readDefaultConfig :: String -> IO (Config,[String])
readDefaultConfig usage = do
  xdgConfigFile <- getXdgConfigFile
  xdgConfigFileExists <- liftIO $ fileExist xdgConfigFile
  home <- liftIO $ getEnv "HOME"
  let defaultConfigFile = home ++ "/.xmobarrc"
  defaultConfigFileExists <- liftIO $ fileExist defaultConfigFile
  if xdgConfigFileExists
    then readConfig xdgConfigFile usage
    else if defaultConfigFileExists
         then readConfig defaultConfigFile usage
         else return (defaultConfig,[])