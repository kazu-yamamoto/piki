module Builder (
    Builder, (+++), toB, mcatmap, build
  , module Data.Monoid
  ) where

import Data.Monoid
import Data.String
import qualified Data.Text.Lazy as L

data Builder = Builder (L.Text -> L.Text)

instance IsString Builder where
    fromString s = Builder (L.pack s +++)

instance Monoid Builder where
    mempty = Builder id
    mappend (Builder x) (Builder y) = Builder (x . y)

(+++) :: Monoid m => m -> m -> m
(+++) = mappend
infixr 5 +++

toB :: L.Text -> Builder
toB txt = Builder (txt `L.append`)

mcatmap :: (a -> Builder) -> [a] -> Builder
mcatmap f = mconcat . map f

build :: Builder -> L.Text
build (Builder f) = f L.empty
