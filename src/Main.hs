module Main where

import Control.Applicative
import qualified Data.Text.Lazy.IO as L
import qualified Data.Version as V
import Html
import Markdown
import qualified Paths_piki as V
import Piki
import System.Console.GetOpt
import System.Environment

----------------------------------------------------------------

version :: String
version = V.showVersion V.version

printVersion :: IO ()
printVersion = putStrLn $ "piki version " ++ version

printUsage :: a
printUsage = error $ usageInfo usage options
  where
    usage =
        "\n"
            ++ "  piki template.html [input.piki]\n"
            ++ "  piki -m [input.piki]\n"

----------------------------------------------------------------

data Mode = HTML | Markdown | Version | Debug

options :: [OptDescr Mode]
options =
    [ Option ['m'] ["markdown"] (NoArg Markdown) "produce Markdown"
    , Option ['v'] ["version"] (NoArg Version) "print version"
    , Option ['d'] ["debug"] (NoArg Debug) "print internal data"
    ]

compilerOpts :: [String] -> (Mode, [String])
compilerOpts argv = case getOpt Permute options argv of
    ([m], n, []) -> (m, n)
    ([], n, []) -> (HTML, n)
    _ -> printUsage

----------------------------------------------------------------

doHtml :: [FilePath] -> IO ()
doHtml [template, input] = toHTML <$> L.readFile template <*> L.readFile input >>= L.putStr
doHtml [template] = toHTML <$> L.readFile template <*> L.getContents >>= L.putStr
doHtml _ = printUsage

----------------------------------------------------------------

doMD :: [FilePath] -> IO ()
doMD [input] = toMD <$> L.readFile input >>= L.putStr
doMD [] = toMD <$> L.getContents >>= L.putStr
doMD _ = printUsage

----------------------------------------------------------------
main :: IO ()
main = do
    (m, files) <- compilerOpts <$> getArgs
    case m of
        Version -> printVersion
        Markdown -> doMD files
        HTML -> doHtml files
        Debug -> piki <$> L.getContents >>= print
