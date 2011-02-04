module CharParser (
    getText
  , getTitleFiles
  , getTitleFileURLs
  ) where

import qualified Data.Text.Lazy as L
import LineParser
import Notation
import Types

----------------------------------------------------------------

getText :: L.Text -> LineParser XText
getText txt = case parse text "fromText" txt of
    Right cooked -> return cooked
    Left  _      -> fail ": link or quote error"

text :: Parser XText
text = spaces *> contents
  where
    contents = many (link <|> shrinkSpaces <|> rawtext)

link :: Parser PText
link = A <$> (open *> word) <*> (spaces *> word <* close)
  where
    open = char pikiAOpen
    close = char pikiAClose

shrinkSpaces :: Parser PText
shrinkSpaces = do
    many1 space
    try (Null <$ eof) <|> return (R ' ')

rawtext :: Parser PText
rawtext = do
    c <- anyChar
    if c == pikiEscape
        then do
            e <- anyChar
            return $ E e
        else return $ R c

----------------------------------------------------------------

getTitleFiles :: L.Text -> LineParser [Image]
getTitleFiles str = case parse titleFiles "getTitleFiles" str of
    Right imgs -> return imgs
    Left  _    -> fail ": illegal '@ title file'"

titleFiles :: Parser [Image]
titleFiles = spaces *> sepBy1 titleFile spaces

titleFile :: Parser Image
titleFile = Image <$> word <*> (spaces *> word) <*> pure Nothing

----------------------------------------------------------------

getTitleFileURLs :: L.Text -> LineParser [Image]
getTitleFileURLs str = case parse titleFileURLs "getTitleFileURLs" str of
    Right imgs -> return imgs
    Left  _    -> fail ": illegal '@@ title file url'"

titleFileURLs :: Parser [Image]
titleFileURLs = spaces *> sepBy1 titleFileURL spaces

titleFileURL :: Parser Image
titleFileURL = Image <$> word <*> (spaces *> word) <*> (Just <$> (spaces *> word))

----------------------------------------------------------------

word :: Parser L.Text
word = quoted <|> unquoted

quoted :: Parser L.Text
quoted = L.pack <$> (open *> inside <* close)
  where
    open   = char '"'
    inside = many1 $ noneOf "\t\n[]\""
    close  = char '"'

unquoted :: Parser L.Text
unquoted = L.pack <$> (many1 $ noneOf " \t\n[]\"")
