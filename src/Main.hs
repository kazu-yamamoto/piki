module Main where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import qualified System.IO.UTF8 as U8
import Piki

----------------------------------------------------------------

version :: String
version = "0.3"

printVersion :: IO ()
printVersion = putStrLn . (++ " version " ++ version) =<< getProgName

printUsage :: IO ()
printUsage  = putStrLn . (++ " template [file]") =<< getProgName
                  

----------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let opts = filter ("-" `isPrefixOf`) args
        files = filter (not.isPrefixOf "-") args
    case opts of
      []        -> doPikiWith files
      ["-v"]    -> printVersion
      _         -> printUsage

doPikiWith :: [FilePath] -> IO ()
doPikiWith [template,input] = do
    tmp <- U8.readFile template
    inp <- U8.readFile input
    U8.putStr $ doPiki tmp inp

doPikiWith [template] = do
    tmp <- U8.readFile template
    inp <- U8.getContents
    U8.putStr $ doPiki tmp inp

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
