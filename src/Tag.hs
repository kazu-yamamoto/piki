module Tag ((###),(//),(///),(\\),(\\\),solo) where

infix 6 ###, //, ///, \\, \\\ -- higher than ++

open :: String -> String
open tag = "<" ++ tag ++ ">"

close :: String -> String
close tag = "</" ++ tag ++ ">"

(//) :: String -> String -> String
str // tag = open tag ++ str ++ close tag ++ "\n"

(\\) :: String -> String -> String
str \\ tag = open tag ++ "\n" ++ str ++ close tag ++ "\n"

(###) :: String -> (String,[(String,String)]) -> String
str ### (tag,avs) = open (tag ++ attributes avs) ++ str ++ close tag

(///) :: String -> (String,[(String,String)]) -> String
str /// (tag,avs) = open (tag ++ attributes avs) ++ str ++ close tag ++ "\n"

(\\\) :: String -> (String,[(String,String)]) -> String
str \\\ (tag,avs) = open (tag ++ attributes avs) ++ "\n" ++ str ++ close tag ++ "\n"

solo :: (String,[(String,String)]) -> String
solo (tag,avs) = "<" ++ tag ++ attributes avs ++ " />\n"

attributes :: [(String,String)] -> String
attributes avs = concatMap (\(a,v) -> " " ++ a ++ "=\"" ++ v ++ "\"") avs
