{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Parsec (module AP, Parser, LineParser) where

import Control.Applicative as AP hiding (many, optional, (<|>))
import qualified Data.Text.Lazy as L
import Text.Parsec as AP hiding (ParseError, errorPos, satisfy)
import Text.Parsec.Error as AP

type Parser = Parsec L.Text ()
type LineParser = Parsec [L.Text] ()
