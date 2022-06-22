module Codec.MIME.Boundary (
  Boundary (..),
) where

import Control.Monad.Random
import Relude.Extra (prev)
import System.Random.Stateful

-- | Get a list of results by repeatedly applying a monadic action to an initial value.
applyNM :: (Monad m) => Int -> a -> (a -> m b) -> m [b] -> m [b]
applyNM 0 _ _ mbs = mbs
applyNM n a f mbs = do
  b <- f a
  bs <- mbs
  applyNM (prev n) a f $ pure (b : bs)

-- |
-- A wrapper around 'Text' such that the 'Uniform' instance generates
-- a sequence of ten ASCII alphanumeric characters.
newtype Boundary = Boundary Text

instance Uniform Boundary where
  uniformM g =
    Boundary . toText . mapMaybe (ls !!?)
      <$> applyNM 10 g (uniformRM (0, l)) (pure [])
   where
    ls = fold [['A' .. 'Z'], ['a' .. 'z'], ['0' .. '9']]
    l = prev $ length ls

instance UniformRange Boundary where uniformRM _ = uniformM

instance Random Boundary
