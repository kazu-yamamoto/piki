{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_parsec
#define MIN_VERSION_parsec(x,y,z) 0
#endif

#if MIN_VERSION_parsec(3,0,0)
module Parsec (
      module Control.Applicative
    , module Text.Parsec
    , module Text.Parsec.Error
    , module Text.Parsec.String
    ) where

import Control.Applicative hiding (many,optional,(<|>))
import Text.Parsec hiding (satisfy)
import Text.Parsec.Error
import Text.Parsec.String
#else
module Parsec (
      module Text.ParserCombinators.Parsec
    , module Text.ParserCombinators.Parsec.Error
    , (<$>), (<$), (<*>), (<*), (*>), pure
    ) where

import Control.Monad (ap, liftM)
import Text.ParserCombinators.Parsec hiding (satisfy)
import Text.ParserCombinators.Parsec.Error hiding (ParseError, errorPos)

{-
  GenParser cannot be an instance of Applicative and Alternative
  due to the overlapping instances error, sigh!
-}

(<$>) :: Monad m => (a -> b) -> m a -> m b
(<$>) = liftM

(<$) :: Monad m => a -> m b -> m a
a <$ m = m >> return a

(<*>) :: Monad m => m (a -> b) -> m a -> m b
(<*>) = ap

(*>) :: Monad m => m a -> m b -> m b
(*>) = (>>)

(<*) :: Monad m => m a -> m b -> m a
m1 <* m2 = do x <- m1
              m2
              return x

pure :: Monad m => a -> m a
pure = return

infixl 4 <$>, <$, <*>, <*, *>
#endif
