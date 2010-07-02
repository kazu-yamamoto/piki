module HtmlText (reference) where

reference :: String -> String
reference = concatMap referenceChar

referenceChar :: Char -> String
referenceChar '<' = "&lt;"
referenceChar '>' = "&gt;"
referenceChar '&' = "&amp;"
referenceChar '"' = "&quot;"
referenceChar c = [c]
