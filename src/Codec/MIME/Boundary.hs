module Codec.MIME.Boundary (
  Boundary (..),
) where

import Relude.Extra (prev)
import System.Random.Stateful (
  Random,
  StatefulGen,
  Uniform (..),
  UniformRange (..),
 )

-- | Get a list of results by iterating a monadic action.
applyNM :: (Monad m) => Int -> a -> (a -> m b) -> m [b] -> m [b]
applyNM n a f = case n of
  0 -> id
  _ -> applyNM (prev n) a f . liftA2 (:) (f a)

-- |
-- A wrapper around 'Text' such that the 'Uniform' instance generates
-- a sequence of ten ASCII alphanumeric characters.
newtype Boundary = Boundary Text

instance Uniform Boundary where
  uniformM :: StatefulGen g m => g -> m Boundary
  uniformM g =
    Boundary . toText . mapMaybe (ls !!?)
      <$> applyNM 10 g (uniformRM (0, l)) (pure [])
   where
    ls = fold [['A' .. 'Z'], ['a' .. 'z'], ['0' .. '9']]
    l = prev $ length ls

instance UniformRange Boundary where
  uniformRM :: StatefulGen g m => (Boundary, Boundary) -> g -> m Boundary
  uniformRM _ = uniformM

instance Random Boundary
