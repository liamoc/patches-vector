
module Data.Patch.InternalSpec where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

import           Data.Patch.Internal
import           Test.Util ()

spec :: Spec
spec = do
  describe "applicable" $ do
    it "mempty is always applicable" $ do
      property $ \d ->
        applicable (mempty :: Patch Int) d
