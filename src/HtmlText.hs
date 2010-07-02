module HtmlText (
    fromText
  , getTitleFiles
  , reference
  ) where

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
text = concat <$> (spaces *> contents)
  where
    contents = many (link <|> shrinkSpaces <|> rawtext)

link :: Parser String
link = do
    char pikiAOpen
    title <- word
    space
    url <- word
    char pikiAClose
    return $ reference title %%% ("a",[("href",url)])

shrinkSpaces :: Parser String
shrinkSpaces = do
    many1 space
    try (do eof; return "") <|> return " "

rawtext :: Parser String
rawtext = do
    c <- anyChar
    if c == pikiEscape
        then singleton <$> anyChar
        else return $ referenceChar c
  where
    singleton c = [c]

----------------------------------------------------------------

word :: Parser String
word = quoted <|> unquoted

quoted :: Parser String
quoted = open *> inside <* close
  where
    open   = char '"'
    inside = many1 $ noneOf "\t\n[]\""
    close  = char '"'

unquoted :: Parser String
unquoted = many1 $ noneOf " \t\n[]\""

----------------------------------------------------------------

getTitleFiles :: String -> LineParser [Image]
getTitleFiles str = case parse titleFiles "getTitleFiles" str of
                      Right imgs -> return imgs
                      Left  _    -> fail ": no '@ title file'"

titleFiles :: Parser [Image]
titleFiles = spaces *> sepBy1 titleFile spaces

titleFile :: Parser Image
titleFile = Image <$> (reference <$> word) <*> (spaces *> word)

----------------------------------------------------------------

reference :: String -> String
reference = concatMap referenceChar

referenceChar :: Char -> String
referenceChar '<' = "&lt;"
referenceChar '>' = "&gt;"
referenceChar '&' = "&amp;"
referenceChar '"' = "&quot;"
referenceChar c = [c]
