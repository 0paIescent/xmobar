------------------------------------------------------------------------------
-- |
-- Module: Xmobar.App.Main
-- Copyright: (c) 2018 Jose Antonio Ortega Ruiz
-- License: BSD3-style (see LICENSE)
--
-- Maintainer: jao@gnu.org
-- Stability: unstable
-- Portability: portable
-- Created: Sun Nov 25, 2018 21:53
--
--
-- Support for creating executable main functions
--
------------------------------------------------------------------------------


module Xmobar.App.Main (xmobar, xmobarMain) where

import Control.Concurrent.Async (Async, cancel)
import Control.Exception (bracket)
import Control.Monad (unless)

import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.List (intercalate)
import System.Posix.Process (executeFile)
import System.Environment (getArgs)
import System.FilePath
import System.FilePath.Posix (takeBaseName, takeDirectory)

import Graphics.X11.Xlib

import Xmobar.Config.Types
import Xmobar.Config.Parse
import Xmobar.System.Signal (setupSignalHandler, withDeferSignals)
import Xmobar.Run.Template
import Xmobar.X11.Types
import Xmobar.X11.Text
import Xmobar.X11.Window
import Xmobar.App.Opts
import Xmobar.App.EventLoop (startLoop, startCommand)
import Xmobar.App.Compile (recompile)
import Xmobar.App.Config

xmobar :: Config -> IO ()
xmobar conf = withDeferSignals $ do
  initThreads
  d <- openDisplay ""
  fs    <- initFont d (font conf)
  fl    <- mapM (initFont d) (additionalFonts conf)
  cls   <- mapM (parseTemplate (commands conf) (sepChar conf))
                (splitTemplate (alignSep conf) (template conf))
  sig   <- setupSignalHandler
  bracket (mapM (mapM $ startCommand sig) cls)
          cleanupThreads
          $ \vars -> do
    (r,w) <- createWin d fs conf
    let ic = Map.empty
        to = textOffset conf
        ts = textOffsets conf ++ replicate (length fl) (-1)
    startLoop (XConf d r w (fs:fl) (to:ts) ic conf) sig vars

cleanupThreads :: [[([Async ()], a)]] -> IO ()
cleanupThreads vars =
  for_ (concat vars) $ \(asyncs, _) ->
    for_ asyncs cancel

buildLaunch :: Bool -> Bool -> FilePath -> IO ()
buildLaunch verb force p = do
  let exec = takeBaseName p
      dir = takeDirectory p
  recompile dir exec force verb
  executeFile (dir </> exec) False [] Nothing

xmobar' :: Config -> [String] -> IO ()
xmobar' cfg defs = do
  unless (null defs || not (verbose cfg)) $ putStrLn $
    "Fields missing from config defaulted: " ++ intercalate "," defs
  xmobar cfg

xmobarMain :: IO ()
xmobarMain = do
  args <- getArgs
  (flags, rest) <- getOpts args
  cf <- case rest of
          (c:_) -> return (Just c)
          _ -> xmobarConfigFile
  case cf of
    Nothing -> case rest of
                (c:_) -> error $ c ++ ": file not found"
                _ -> xmobar defaultConfig
    Just p -> do d <- doOpts defaultConfig flags
                 r <- readConfig d p
                 case r of
                   Left _ -> buildLaunch (verbose d) (forceRecompile flags) p
                   Right (c, defs) -> xmobar' c defs
