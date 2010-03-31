module HtmlText (fromText, getTitleFiles, reference) where

--import Data.Char
import Notation
import Html
import Tag
import LineParser

----------------------------------------------------------------

fromText :: String -> LineParser String
fromText str = case parse text "fromText" str of
                 Right cooked -> return cooked
                 Left  _      -> fail ": link or quote error"

----------------------------------------------------------------

text :: Parser String
text = do spaces
          ss <- many (link <|> shrinkSpaces <|> rawtext)
          return $ concat ss

link :: Parser String
link = do char pikiAOpen
          title <- word
          space
          url <- word
          char pikiAClose
          return $ (reference title) ### ("a",[("href",url)])

shrinkSpaces :: Parser String
shrinkSpaces = do many1 space
                  try (do eof; return "") <|> return " "

rawtext :: Parser String
rawtext = do c  <- anyChar
             if c == pikiEscape
               then do c' <- anyChar; return [c']
               else return $ referenceChar c

----------------------------------------------------------------

word :: Parser String
word = quoted <|> unquoted

quoted :: Parser String
quoted = do char '"'
            str <- many1 $ noneOf "\t\n[]\""
            char '"'
            return str

unquoted :: Parser String
unquoted = many1 $ noneOf " \t\n[]\""

----------------------------------------------------------------

{-
getTitleFiles :: String -> Either ParseError [Image]
getTitleFiles = parse titleFiles "getTitleFiles"
-}

getTitleFiles :: String -> LineParser [Image]
getTitleFiles str = case parse titleFiles "getTitleFiles" str of
                      Right imgs -> return imgs
                      Left  _    -> fail ": no '@ title file'"

titleFiles :: Parser [Image]
titleFiles = do spaces
                tfs <- sepBy1 titleFile spaces
                return tfs

titleFile :: Parser Image
titleFile = do title <- word
               spaces
               file <- word
               return $ Image (reference title) file

----------------------------------------------------------------

reference :: String -> String
reference = concatMap referenceChar

referenceChar :: Char -> String
referenceChar '<' = "&lt;"
referenceChar '>' = "&gt;"
referenceChar '&' = "&amp;"
referenceChar '"' = "&quot;"
referenceChar c = [c]
