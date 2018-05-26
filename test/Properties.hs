module Main (main) where

import Control.Applicative
import Data.Plur

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

instance Arbitrary a => Arbitrary (Plur a) where
  arbitrary = oneof
    [ pure Zero
    , One <$> arbitrary
    , Two <$> arbitrary <*> arbitrary
    ]
  shrink (Two a b) = [One a, One b]
  shrink _         = []

instance Eq a => EqProp (Plur a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (functor     (One ('x', 'y', 'z')))
  quickBatch (applicative (One ('x', 'y', 'z')))
  quickBatch (monad       (One ('x', 'y', 'z')))
  quickBatch (traversable (One ('x', 'y', "z")))
  quickBatch (semigroup   (One ('x', 'y', 'z')))
  quickBatch (monoid      (One 'x'))
