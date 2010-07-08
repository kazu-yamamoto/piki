{-# LANGUAGE CPP #-}

module Main where

import Control.Applicative
import Html
import Markdown
import System.Console.GetOpt
import System.Environment
import System.IO

----------------------------------------------------------------

version :: String
version = "0.4.0"

printVersion :: IO ()
printVersion = putStrLn $ "piki version " ++ version

printUsage :: a
printUsage = error $ usageInfo usage options
  where
    usage = "\n"
         ++ "  piki template.html [input.piki]\n"
         ++ "  piki -m [input.piki]\n"

----------------------------------------------------------------

data Mode = HTML | Markdown | Version

options :: [OptDescr Mode]
options =
    [ Option ['m'] ["markdown"] (NoArg Markdown) "produce Markdown" 
    , Option ['v'] ["version"]  (NoArg Version) "print version"
    ]

compilerOpts :: [String] -> (Mode, [String])
compilerOpts argv = case getOpt Permute options argv of
    ([m],n,[]) -> (m,n)
    ([], n,[]) -> (HTML,n)
    _          -> printUsage

----------------------------------------------------------------

setUTF8 :: Handle -> IO Handle
setUTF8 h = do
#if __GLASGOW_HASKELL__ >= 611
    hSetEncoding h utf8
#endif
    return h

readFileU8 :: FilePath -> IO String
readFileU8 file = openFile file ReadMode >>= setUTF8 >>= hGetContents

----------------------------------------------------------------

doHtml :: [FilePath] -> IO ()
doHtml [template,input] = do
    tmp <- readFileU8 template
    inp <- readFileU8 input
    putStr $ toHTML tmp inp
doHtml [template] = do
    tmp <- readFileU8 template
    inp <- getContents
    putStr $ toHTML tmp inp
doHtml _ = printUsage

----------------------------------------------------------------

doMD :: [FilePath] -> IO ()
doMD [input] = do
    inp <- readFileU8 input
    putStr $ toMD inp
doMD [] = do
    inp <- getContents
    putStr $ toMD inp
doMD _ = printUsage

----------------------------------------------------------------
main :: IO ()
main = do
    setUTF8 stdin
    setUTF8 stdout
    (m,files) <- compilerOpts <$> getArgs
    case m of
        Version  -> printVersion
        Markdown -> doMD files
        HTML     -> doHtml files

