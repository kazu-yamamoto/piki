module CharParser (
    getText
  , getTitleFiles
  ) where

import LineParser
import Notation
import Types

----------------------------------------------------------------

getText :: String -> LineParser XString
getText str = case parse text "fromText" str of
    Right cooked -> return cooked
    Left  _      -> fail ": link or quote error"

text :: Parser XString
text = spaces *> contents
  where
    contents = many (link <|> shrinkSpaces <|> rawtext)

link :: Parser PString
link = A <$> (open *> word) <*> (spaces *> word <* close)
  where
    open = char pikiAOpen
    close = char pikiAClose

shrinkSpaces :: Parser PString
shrinkSpaces = do
    many1 space
    try (Null <$ eof) <|> return (R ' ')

rawtext :: Parser PString
rawtext = do
    c <- anyChar
    if c == pikiEscape
        then do
            e <- anyChar
            return $ E e
        else return $ R c

----------------------------------------------------------------

getTitleFiles :: String -> LineParser [Image]
getTitleFiles str = case parse titleFiles "getTitleFiles" str of
                      Right imgs -> return imgs
                      Left  _    -> fail ": no '@ title file'"

titleFiles :: Parser [Image]
titleFiles = spaces *> sepBy1 titleFile spaces

titleFile :: Parser Image
titleFile = Image <$> word <*> (spaces *> word)

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
