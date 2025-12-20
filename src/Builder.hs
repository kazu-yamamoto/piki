module Builder (
    Builder,
    (+++),
    toB,
    mcatmap,
    build,
    module Data.Monoid,
) where

import Data.Monoid
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder

(+++) :: Monoid m => m -> m -> m
(+++) = mappend
infixr 5 +++

toB :: L.Text -> Builder
toB = fromLazyText

mcatmap :: (a -> Builder) -> [a] -> Builder
mcatmap f = mconcat . map f

build :: Builder -> L.Text
build = toLazyText
