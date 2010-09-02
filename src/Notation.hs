module Notation where

---------------------------------------------------------------

pikiReserved :: String
pikiReserved = "#=*-+?!@{}>|"

pikiHr,pikiComment,pikiTitle,pikiUl,pikiOl,pikiDlT,pikiDlD,pikiImg,pikiAOpen,pikiAClose,pikiDivOpen,pikiDivClose,pikiEscape,pikiTable :: Char
pikiPreOpen,pikiPreClose :: String

pikiComment  = '#'
pikiHr       = '='
pikiTitle    = '*'
pikiUl       = '-'
pikiOl       = '+'
pikiDlT      = '?' -- ?タイトル
pikiDlD      = '!' -- !説明
pikiImg      = '@' -- @タイトル ファイル名
pikiAOpen    = '[' -- [タイトル URL]
pikiAClose   = ']'
pikiDivOpen  = '{' -- {class or {ID
pikiDivClose = '}'
pikiTable    = '|'
pikiPreOpen  = ">|"
pikiPreClose = "|<"

pikiEscape   = '\\'
