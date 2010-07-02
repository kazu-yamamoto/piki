{-# LANGUAGE CPP #-}

module Main where

import Data.List
import System.Environment
import System.IO
import Html

----------------------------------------------------------------

version :: String
version = "0.4.0"

printVersion :: IO ()
printVersion = putStrLn . (++ " version " ++ version) =<< getProgName

printUsage :: IO ()
printUsage  = putStrLn . (++ " template [file]") =<< getProgName

----------------------------------------------------------------

main :: IO ()
main = do
#if __GLASGOW_HASKELL__ >= 611
    hSetEncoding stdin utf8
    hSetEncoding stdout utf8
#endif
    args <- getArgs
    let opts = filter ("-" `isPrefixOf`) args
        files = filter (not.isPrefixOf "-") args
    case opts of
      []        -> doPikiWith files
      ["-v"]    -> printVersion
      _         -> printUsage

readFileU8 :: FilePath -> IO String
readFileU8 file = do
    h <- openFile file ReadMode
#if __GLASGOW_HASKELL__ >= 611
    hSetEncoding h utf8
#endif
    hGetContents h

doPikiWith :: [FilePath] -> IO ()
doPikiWith [template,input] = do
    tmp <- readFileU8 template
    inp <- readFileU8 input
    putStr $ toHTML tmp inp

doPikiWith [template] = do
    tmp <- readFileU8 template
    inp <- getContents
    putStr $ toHTML tmp inp

doPikiWith _ = printUsage
