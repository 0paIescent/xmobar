{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Plugins.Monitors.Mem
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Jose A. Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A memory monitor for Xmobar
--
-----------------------------------------------------------------------------

module Xmobar.Plugins.Monitors.Mem (memConfig, runMem, totalMem, usedMem) where

import Xmobar.Plugins.Monitors.Common
import qualified Data.Map as M
import System.Console.GetOpt
#ifdef FREEBSD
import System.BSD.Sysctl (sysctlReadInt)
import Control.Applicative (liftA2)
import Control.Monad (liftM)
#endif

data MemOpts = MemOpts
  { usedIconPattern :: Maybe IconPattern
  , freeIconPattern :: Maybe IconPattern
  , availableIconPattern :: Maybe IconPattern
  }

defaultOpts :: MemOpts
defaultOpts = MemOpts
  { usedIconPattern = Nothing
  , freeIconPattern = Nothing
  , availableIconPattern = Nothing
  }

options :: [OptDescr (MemOpts -> MemOpts)]
options =
  [ Option "" ["used-icon-pattern"] (ReqArg (\x o ->
     o { usedIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["free-icon-pattern"] (ReqArg (\x o ->
     o { freeIconPattern = Just $ parseIconPattern x }) "") ""
  , Option "" ["available-icon-pattern"] (ReqArg (\x o ->
     o { availableIconPattern = Just $ parseIconPattern x }) "") ""
  ]

memConfig :: IO MConfig
memConfig = mkMConfig
       "Mem: <usedratio>% (<cache>M)" -- template
       ["usedbar", "usedvbar", "usedipat", "freebar", "freevbar", "freeipat",
        "availablebar", "availablevbar", "availableipat",
        "usedratio", "freeratio", "availableratio",
        "total", "free", "buffer", "cache", "available", "used"] -- available replacements

sysctlReadFloat :: String -> IO Float
sysctlReadFloat k = fmap fromIntegral $ sysctlReadInt k

totalMEM :: Float -> Float -> Float
totalMEM chip phys
    | (phys / 8 - 1) == 0 = totalMEM (chip * 2) (phys / 2)
    | otherwise = (phys / chip + 1 ) * chip

parseMEM :: IO [Float]
#ifdef FREEBSD
-- Okay so in the end we'll have a [IO Float] right? and then it just needs to go to IO [Float] yeah?
parseMEM = do
    let total = fmap (totalMEM 1.0) $ sysctlReadFloat "hw.physmem"
        pagesize = sysctlReadFloat "hw.pagesize" :: IO Float
	page = \n -> liftA2 (*) pagesize n
	free = page $ sysctlReadFloat "vm.stats.vm.v_free_count"
        inactive = page $ sysctlReadFloat "vm.stats.vm.v_inactive_count"

        info = [ total
	       --, inactive
               --, fromIntegral $ sysctlReadInt "vm.stats.vm.v_free_count"
	       --, fromIntegral $ sysctlReadInt "vm.stats.vm.v_"
	       ]
        
    return []
#else
parseMEM =
    do file <- readFile "/proc/meminfo"
       let content = map words $ take 8 $ lines file
           info = M.fromList $ map (\line -> (head line, (read $ line !! 1 :: Float) / 1024)) content
           [total, free, buffer, cache] = map (info M.!) ["MemTotal:", "MemFree:", "Buffers:", "Cached:"]
           available = M.findWithDefault (free + buffer + cache) "MemAvailable:" info
           used = total - available
           usedratio = used / total
           freeratio = free / total
           availableratio = available / total
       return [usedratio, freeratio, availableratio, total, free, buffer, cache, available, used]
#endif

totalMem :: IO Float
totalMem = fmap ((*1024) . (!!1)) parseMEM

usedMem :: IO Float
usedMem = fmap ((*1024) . (!!6)) parseMEM

formatMem :: MemOpts -> [Float] -> Monitor [String]
formatMem opts (r:fr:ar:xs) =
    do let f = showDigits 0
           mon i x = [showPercentBar (100 * x) x, showVerticalBar (100 * x) x, showIconPattern i x]
       sequence $ mon (usedIconPattern opts) r
           ++ mon (freeIconPattern opts) fr
           ++ mon (availableIconPattern opts) ar
           ++ map showPercentWithColors [r, fr, ar]
           ++ map (showWithColors f) xs
formatMem _ _ = replicate 10 `fmap` getConfigValue naString

runMem :: [String] -> Monitor String
runMem argv =
    do m <- io parseMEM
       opts <- io $ parseOptsWith options defaultOpts argv
       l <- formatMem opts m
       parseTemplate l
