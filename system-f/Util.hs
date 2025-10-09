module Util where

import Control.Monad.RWS (RWST)
import Data.Maybe (fromJust)
import Data.Monoid (getFirst)
import Errors (TypeError)

type Result = Either TypeError

type RSF r s = RWST r () s Result

info :: (Foldable f) => f a -> a
info = fromJust . getFirst . foldMap pure
