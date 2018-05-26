{-# language LambdaCase #-}
module Data.Plur (Plur(..), count) where

import Data.Monoid
import Data.Semigroup

-- Plurality monad: Zero, one, or at least two.
data Plur a
  = Zero
  | One a
  | Two a a
  deriving (Show, Eq, Ord)

instance Functor Plur where
  fmap f = \case
    Zero      -> Zero
    One a     -> One (f a)
    Two a1 a2 -> Two (f a1) (f a2)

instance Applicative Plur where
  pure = One

  (<*>) Zero _        = Zero
  (<*>) _ Zero        = Zero
  (<*>) (One f) x     = fmap f x
  (<*>) (Two f1 f2) x = fmap f1 x <> fmap f2 x

instance Monad Plur where
  (>>=) x f = case x of
    Zero      -> Zero
    One x1    -> f x1
    Two x1 x2 -> f x1 <> f x2

instance Foldable Plur where
  foldMap f = \case
    Zero      -> mempty
    One a     -> f a
    Two a1 a2 -> f a1 <> f a2

instance Traversable Plur where
  traverse f = \case
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
  mempty = Zero

count :: Plur a -> Int
count = \case
  Zero    -> 0
  One _   -> 1
  Two _ _ -> 2
