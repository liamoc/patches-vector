
module Test.UtilSpec where

import           Data.Monoid
import           Test.Hspec
import           Test.QuickCheck

import           Data.Patch.Internal
import           Test.Util

spec :: Spec
spec = do
  describe "patchesFrom" $ do
    it "creates applicable patches" $ do
      property $ \d ->
        forAll (patchesFrom d) $ \p -> applicable p d

    it "creates patches that are right-composable with mempty" $ do
      property $ \d ->
        forAll (patchesFrom d) $ \p -> composable mempty p

    it "creates patches that are left-composable with mempty" $ do
      property $ \d ->
        forAll (patchesFrom d) $ \p -> composable p mempty

  describe "historyFrom" $ do
    it "creates patches that are applicable in sequence" $ do
      property $ \d ->
        forAll (historyFrom d 2) $ \[p, q] ->
          applicable p d && applicable q (apply p d)

    it "creates patches that are applicable in merged form" $ do
      property $ \d ->
        forAll (historyFrom d 2) $ \[p, q] ->
          applicable (p <> q) d

    it "creates patches that are composable" $ do
      property $ \d ->
        forAll (historyFrom d 2) $ \[p, q] -> composable p q
