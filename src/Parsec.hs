{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parsec (
      module Control.Applicative
    , module Text.Parsec
    , module Text.Parsec.Error
    , Parser, LineParser
    ) where

import Control.Applicative hiding (many,optional,(<|>))
import qualified Data.Text.Lazy as L
import Text.Parsec hiding (satisfy, ParseError, errorPos)
import Text.Parsec.Error
import Text.Parsec.Prim

type Parser = Parsec L.Text ()
type LineParser = Parsec [L.Text] ()

instance Monad m => Stream L.Text m Char where
    uncons = return . L.uncons
