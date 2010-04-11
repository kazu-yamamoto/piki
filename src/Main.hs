{-# LANGUAGE CPP #-}

module Main where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.IO
import Piki

----------------------------------------------------------------

version :: String
version = "0.3.0"

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
    putStr $ doPiki tmp inp

doPikiWith [template] = do
    tmp <- readFileU8 template
    inp <- getContents
    putStr $ doPiki tmp inp

doPikiWith _ = printUsage

----------------------------------------------------------------

doPiki :: String -> String -> String
doPiki tmp inp = let (ttl,body) = piki inp
                     title = fromMaybe "" ttl
                 in replace tmp [("body",body),("title",title)]

replace :: String -> [(String, String)] -> String
replace tmpl params =
        case break (== '$') tmpl of
          (s, ('$':cs)) -> s ++ replaced ++ replace (chop rest) params
              where
                (var, rest) = span isAlpha cs
                replaced = fromMaybe ('$':var) (lookup var params)
                chop ('\n':xs) = xs
                chop xs        = xs
          (s, _)     -> s
