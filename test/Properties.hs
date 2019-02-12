module Main (main) where

import Control.Applicative
import Data.Plur
import System.Exit
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Hedgehog
import Hedgehog.Classes

genPlur :: Gen x -> Gen (Plur x)
genPlur gen = Gen.choice
  [ pure Zero
  , One <$> gen
  , Two <$> gen <*> gen
  ]

main :: IO ()
main = do
  result <- lawsCheckMany
    [ ("Plur",
      [ functorLaws genPlur
      , applicativeLaws genPlur
      , monadLaws genPlur
      , traversableLaws genPlur
      , semigroupLaws $ genPlur Gen.unicode
      , monoidLaws $ genPlur Gen.unicode
      ])
    ]
  exitWith $ if result then ExitSuccess else ExitFailure 1
