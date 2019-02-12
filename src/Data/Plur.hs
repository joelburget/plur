module Data.Plur (Plur(..), count) where

import Control.Applicative
import Data.Foldable
import Data.Monoid
import Data.Semigroup as Semigroup
import Data.Traversable

-- | Plurality monad: Zero, one, or at least two.
data Plur a
  = Zero
  | One !a
  | Two !a !a
  deriving (Show, Eq, Ord)

instance Functor Plur where
  fmap f plur = case plur of
    Zero      -> Zero
    One a     -> One (f a)
    Two a1 a2 -> Two (f a1) (f a2)

instance Applicative Plur where
  pure = One

  Zero      <*> _    = Zero
  _         <*> Zero = Zero
  One f     <*> x    = fmap f x
  Two f1 f2 <*> x    = fmap f1 x `mappend` fmap f2 x

instance Monad Plur where
  return = pure
  x >>= f = case x of
    Zero      -> Zero
    One x1    -> f x1
    Two x1 x2 -> f x1 `mappend` f x2

instance Foldable Plur where
  foldMap f plur = case plur of
    Zero      -> mempty
    One a     -> f a
    Two a1 a2 -> f a1 `mappend` f a2

instance Traversable Plur where
  traverse f plur = case plur of
    Zero      -> pure Zero
    One a     -> One <$> f a
    Two a1 a2 -> Two <$> f a1 <*> f a2

instance Semigroup (Plur a) where
  r1      <> Zero    = r1
  Zero    <> r2      = r2
  One x   <> One y   = Two x y
  One x   <> Two y _ = Two x y
  Two x y <> _       = Two x y

instance Monoid (Plur a) where
  mempty  = Zero
  mappend = (Semigroup.<>)

-- | Erase occurrences, resulting in @0@, @1@, or @2@.
count :: Plur a -> Int
count plur = case plur of
  Zero    -> 0
  One _   -> 1
  Two _ _ -> 2
