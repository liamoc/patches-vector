import Criterion.Main
import Data.Patch
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Debug.Trace
import qualified Data.Vector as Vector
import Data.Vector (Vector)
editsTo :: Arbitrary a => Vector a -> Gen (Edit a)
editsTo v = do
  i <- choose (0, Vector.length v -1)
  c <- elements [const (Insert i), \o _ -> Delete i o, Replace i]
  x <- arbitrary
  return $ c (v Vector.! i) x

patchesFrom' :: (Eq a, Arbitrary a) => Vector a -> Gen (Patch a)
patchesFrom' v | Vector.length v > 0 = fromList <$> listOf (editsTo v)
               | otherwise           = fromList <$> listOf (Insert 0 <$> arbitrary)
patchesFrom :: Vector Int -> Gen (Patch Int)
patchesFrom = patchesFrom'

instance Arbitrary a => Arbitrary (Vector a) where arbitrary = Vector.fromList <$> listOf arbitrary

qcgen :: QCGen
qcgen = mkQCGen 19835315

gen :: Int -> (Patch Int, Vector Int)
gen i = let
          doc = unGen arbitrary qcgen i :: Vector Int
          patch = unGen (patchesFrom doc) qcgen i
        in traceShow (length $ toList patch, length doc) (patch,doc)

main :: IO ()
main =
  defaultMain [ bgroup "apply" [ bench "1" $ nf (uncurry apply) (gen 500)
                               , bench "2" $ nf (uncurry apply) (gen 1000)
                               , bench "3" $ nf (uncurry apply) (gen 2000)
                               , bench "4" $ nf (uncurry apply) (gen 4000)
                               ]
              ]
