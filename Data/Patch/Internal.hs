{-# LANGUAGE BangPatterns, Trustworthy #-}
-- | For day-to-day use, please see "Data.Patch"
module Data.Patch.Internal where
import Data.Monoid
import Data.Ord
import qualified Data.List as List
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Data.Vector.Distance
import Lens.Micro
import Control.Applicative
import Data.Function
-- $setup
-- >>> import Test.QuickCheck
-- >>> :{
-- let
--   nonEmpty :: Vector a -> Bool
--   nonEmpty = (>0) . Vector.length
--   editsTo :: Arbitrary a => Vector a -> Gen (Edit a)
--   editsTo v = do
--     i <- choose (0, Vector.length v -1)
--     c <- elements [const (Insert i), \o _ -> Delete i o, Replace i]
--     x <- arbitrary
--     return $ c (v Vector.! i) x
--   patchesFrom' :: (Eq a, Arbitrary a) => Vector a -> Gen (Patch a)
--   patchesFrom' v | Vector.length v > 0 = fromList <$> listOf (editsTo v)
--   patchesFrom' _ | otherwise           = fromList <$> listOf (Insert 0 <$> arbitrary)
--   patchesFrom :: Vector Int -> Gen (Patch Int)
--   patchesFrom = patchesFrom'
--   historyFrom d 0 = return []
--   historyFrom d m = do
--     p <- patchesFrom d
--     r <- historyFrom (apply p d) $ m - 1
--     return (p:r)
-- :}
--
-- >>> :set -XScopedTypeVariables
-- >>> instance Arbitrary a => Arbitrary (Vector a) where arbitrary = Vector.fromList <$> listOf arbitrary
--
-- Blah
--

-- $doctest_sucks
-- prop> forAll (patchesFrom d) $ \ x -> read (show x) == x

-- | A /patch/ is a collection of edits performed to a /document/, in this case a 'Vector'. They are
--   implemented as a list
--   of 'Edit', and can be converted to and from raw lists of edits using 'toList' and 'fromList'
--   respectively.
--
--   Patches form a group (a 'Monoid' with inverses), where the inverse element can be computed with
--   'inverse' and the group operation is /composition/ of patches. Applying @p1 <> p2@ is the
--   same as applying @p1@ /then/ @p2@ (see 'apply'). This composition operator may produce structurally
--   different patches depending on associativity, however the patches are guaranteed to be /equivalent/
--   in the sense that the resultant document will be the same when they are applied.
--
-- prop> forAll (patchesFrom d) $ \a -> a <> mempty == a
--
-- prop> forAll (patchesFrom d) $ \a -> mempty <> a == a
--
-- prop> forAll (historyFrom d 3) $ \[a, b, c] -> apply (a <> (b <> c)) d == apply ((a <> b) <> c) d
--
-- The indices of the 'Edit' s are all based on the /original document/, so:
--
-- >>> apply (fromList [Insert 0 'a', Insert 1 'b']) (Vector.fromList "123")
-- "a1b23"
--
-- >>> apply (fromList [Insert 0 'a', Insert 0 'b']) (Vector.fromList "123")
-- "ab123"
--
-- Note that the first 'Insert' didn't introduce an offset for the second.
newtype Patch a = Patch [Edit a] deriving (Eq)

instance Show a => Show (Patch a) where
  show (Patch ls) = "fromList " ++ show ls

instance (Eq a, Read a) => Read (Patch a) where
  readsPrec _  ('f':'r':'o':'m':'L':'i':'s':'t':' ':r) = map (\(a,s) -> (fromList a, s)) $ reads r
  readsPrec _  _ = []
-- | An 'Edit' is a single alteration of the vector, either inserting, removing, or replacing an element.
--
-- Useful optics are provided below, for the 'index', the 'old' element, and the 'new' element.
data Edit a = Insert  Int a -- ^ @Insert i x@ inserts the element @x@ at position @i@.
            | Delete  Int a -- ^ @Delete i x@ deletes the element @x@ from position @i@.
            | Replace Int a a -- ^ @Replace i x x'@ replaces the element @x@ at position @i@ with @x'@.
            deriving (Show, Read, Eq)

-- | Compute the inverse of a patch, such that:
--
-- prop> forAll (patchesFrom d) $ \p -> p <> inverse p == mempty
--
-- prop> forAll (patchesFrom d) $ \p -> inverse p <> p == mempty
--
-- prop> forAll (patchesFrom d) $ \p -> inverse (inverse p) == p
--
-- prop> forAll (historyFrom d 2) $ \[p, q] -> inverse (p <> q) == inverse q <> inverse p
--
-- prop> forAll (patchesFrom d) $ \p -> inverse mempty == mempty
inverse :: Patch a -> Patch a
inverse (Patch ls) = Patch $ snd $ List.mapAccumL go 0 ls
  where
    go :: Int -> Edit a -> (Int, Edit a)
    go off (Insert i x) = (off + 1, Delete (off + i) x)
    go off (Delete i x) = (off - 1, Insert (off + i) x)
    go off (Replace i a b) = (off, Replace (off + i) b a)

-- | A lens for the index where an edit is to be performed.
--
-- prop> nonEmpty d ==> forAll (editsTo d) $ \e -> set index v e ^. index == v
--
-- prop> nonEmpty d ==> forAll (editsTo d) $ \e -> set index (e ^. index) e == e
--
-- prop> nonEmpty d ==> forAll (editsTo d) $ \e -> set index v' (set index v e) == set index v' e
index :: Lens' (Edit a) Int
index f (Insert i a) = fmap (flip Insert a) $ f i
index f (Delete i a) = fmap (flip Delete a) $ f i
index f (Replace i a b) = fmap (\i' -> Replace i' a b) $ f i

-- | A traversal for the old element to be replaced/deleted. Empty in the case of an @Insert@.
old :: Traversal' (Edit a) a
old _ (Insert i a) = pure $ Insert i a
old f (Delete i a) = Delete i <$> f a
old f (Replace i a b) = Replace i <$> f a <*> pure b

-- | A traversal for the new value to be inserted or replacing the old value. Empty in the case of a @Delete@.
new :: Traversal' (Edit a) a
new f (Insert i a) = Insert i <$> f a
new _ (Delete i a) = pure $ Delete i a
new f (Replace i a b) = Replace i <$> pure a <*> f b

-- | Convert a patch to a list of edits.
toList :: Patch a -> [Edit a]
toList (Patch a) = a

-- | Directly convert a list of edits to a patch, without sorting edits by index, and resolving contradictory
-- edits. Use this function if you know that the input list is already a wellformed patch.
unsafeFromList :: [Edit a] -> Patch a
unsafeFromList = Patch

-- | Convert a list of edits to a patch, making sure to eliminate conflicting edits and sorting by index.
fromList :: Eq a => [Edit a] -> Patch a
fromList = Patch . concat . map normalise . List.groupBy ((==) `on` (^. index)) . List.sortBy (comparing (^. index))

-- | Internal: Eliminate conflicting edits
normalise :: [Edit a] -> [Edit a]
normalise grp = let (inserts, deletes, replaces) = partition3 grp
                 in normalise' inserts deletes replaces
  where partition3 (x@(Insert  {}):xs) = let (i,d,r) = partition3 xs in (x:i,d,r)
        partition3 (x@(Delete  {}):xs) = let (i,d,r) = partition3 xs in (i,x:d,r)
        partition3 (x@(Replace {}):xs) = let (i,d,r) = partition3 xs in (i,d,x:r)
        partition3 [] = ([],[],[])

        normalise' (Insert _ x:is) (Delete i y:ds) rs = normalise' is ds (Replace i y x : rs)
        normalise' is [] rs = is ++ take 1 rs
        normalise' [] (d:ds) _  = [d]

instance Eq a => Monoid (Patch a) where
  mempty = Patch []
  mappend (Patch a) (Patch b) = Patch $ merge a b (0 :: Int)
    where
      merge [] ys  off  = map (over index (+ off)) ys
      merge xs []  _    = xs
      merge (x:xs) (y:ys) off = let
          y' = over index (+ off) y
        in case comparing (^. index) x y' of
         LT -> x  : merge xs (y:ys) (off + offset x)
         GT -> y' : merge (x:xs) ys off
         EQ -> case (x,y') of
             (Delete i o, Insert _ n) -> replace i o n $ merge xs ys (off + offset x)
             (Delete {}, _) -> x : merge xs (y:ys) (off + offset x)
             (_, Insert {}) -> y' : merge (x:xs) ys off
             (Replace i o _, Replace _ _ o') -> replace i o o' $ merge xs ys off
             (Replace i o _, Delete {}) -> Delete i o : merge xs ys off
             (Insert i _, Replace _ _ o') -> Insert i o' : merge xs ys (off + offset x)
             (Insert {}, Delete {}) -> merge xs ys (off + offset x)

      offset (Insert {}) = -1
      offset (Delete {}) = 1
      offset (Replace {}) = 0
      replace i o n | o == n = id
      replace i o n | otherwise = (Replace i o n :)

-- | Apply a patch to a document.
--
-- Technically, 'apply' is a /monoid morphism/ to the monoid of endomorphisms @Vector a -> Vector a@,
-- and that's how we can derive the following two laws:
--
-- prop> forAll (historyFrom d 2) $ \[a, b] -> apply b (apply a d) == apply (a <> b) d
--
-- prop> apply mempty d == d
--
apply :: Patch a -> Vector a -> Vector a
apply (Patch s) i = Vector.concat $ go s [i] 0
  where go [] v _ = v
        go (a : as) v x
          | x' <- a ^. index
          = let (prefix, rest)
                  | x' > x    = splitVectorListAt (x' - x) v
                  | otherwise = ([], v)
                conclusion (Insert  _   e) = Vector.singleton e : go as rest x'
                conclusion (Delete  _   _) = go as (drop1 rest) (x' + 1)
                conclusion (Replace _ _ e) = go as (Vector.singleton e : drop1 rest) (x')
             in prefix ++ conclusion a
        drop1 :: [Vector a] -> [Vector a] 
        drop1 [] = []
        drop1 (v:vs) | Vector.length v > 0 = Vector.drop 1 v : vs
        drop1 (_:vs) | otherwise           = drop1 vs
        splitVectorListAt :: Int -> [Vector a] -> ([Vector a], [Vector a])
        splitVectorListAt _ [] = ([],[])
        splitVectorListAt j (v:vs) | j < Vector.length v = let (v1,v2) = Vector.splitAt j v in ([v1],v2:vs)
                                   | otherwise           = let (p1,p2) = splitVectorListAt (j - Vector.length v) vs
                                                            in (v:p1, p2)

-- | Compute the difference between two documents, using the Wagner-Fischer algorithm. O(mn) time and space.
--
-- prop> apply (diff d e) d == e
--
-- prop> apply (diff d e) d == apply (inverse (diff e d)) d
--
-- prop> apply (diff a b <> diff b c) a == apply (diff a c) a
--
diff :: Eq a => Vector a -> Vector a -> Patch a
diff v1 v2 = let (_ , s) = leastChanges params v1 v2
              in unsafeFromList $ adjust 0 $ s
  where
    adjust _ [] = []
    adjust !o (Insert i x:rest) = Insert (i+o) x : adjust (o-1) rest
    adjust !o (Delete i x:rest) = Delete (i+o) x : adjust (o+1) rest
    adjust !o (Replace i x x':rest) = Replace (i+o) x x' : adjust o rest
    params :: Eq a => Params a (Edit a) (Sum Int)
    params = Params { equivalent     = (==)
                    , delete         = \i c    -> Delete  i c
                    , insert         = \i c    -> Insert  i c
                    , substitute     = \i c c' -> Replace i c c'
                    , cost           = \_      -> Sum 1
                    , positionOffset = \x -> case x of
                                               Delete {} -> 0
                                               _         -> 1
                    }
