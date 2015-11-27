{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Util where

import           Control.Applicative
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import           Test.QuickCheck

import           Data.Patch

nonEmpty :: Vector a -> Bool
nonEmpty = (>0) . Vector.length

editsTo :: Arbitrary a => Vector a -> Gen (Edit a)
editsTo v = do
  i <- choose (0, Vector.length v -1)
  c <- elements [const (Insert i), \o _ -> Delete i o, Replace i]
  x <- arbitrary
  return $ c (v Vector.! i) x

patchesFrom' :: (Eq a, Arbitrary a) => Vector a -> Gen (Patch a)
patchesFrom' v | Vector.length v > 0 = fromList <$> listOf (editsTo v)
patchesFrom' _ | otherwise           = fromList <$> listOf (Insert 0 <$> arbitrary)

patchesFrom :: Vector Int -> Gen (Patch Int)
patchesFrom = patchesFrom'

divergingPatchesFrom :: Vector Int -> Gen (Patch Int, Patch Int)
divergingPatchesFrom v = (,) <$> patchesFrom v <*> patchesFrom v

historyFrom :: Vector Int -> Int -> Gen [Patch Int]
historyFrom _ 0 = return []
historyFrom d m = do
  p <- patchesFrom d
  r <- historyFrom (apply p d) $ m - 1
  return (p:r)

instance Arbitrary a => Arbitrary (Vector a) where
  arbitrary = Vector.fromList <$> listOf arbitrary
