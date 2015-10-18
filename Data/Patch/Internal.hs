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
--   divergingPatchesFrom :: Vector Int -> Gen (Patch Int, Patch Int)
--   divergingPatchesFrom v = (,) <$> patchesFrom v <*> patchesFrom v
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
-- >>> Vector.toList $ apply (fromList [Insert 0 'a', Insert 1 'b']) (Vector.fromList "123")
-- "a1b23"
--
-- >>> Vector.toList $ apply (fromList [Insert 0 'a', Insert 0 'b']) (Vector.fromList "123")
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
--
-- prop> forAll (patchesFrom d) $ \p -> applicable (inverse p) (apply p d)
--
-- prop> forAll (patchesFrom d) $ \p -> composable p (inverse p)
--
-- prop> forAll (patchesFrom d) $ \p -> composable (inverse p) p
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
        normalise' [] (d:_) _  = [d]
        normalise' _ _ _ = error "Impossible!"

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
      replace _ o n | o == n = id
      replace i o n | otherwise = (Replace i o n :)

-- | Returns true if a patch can be safely applied to a document, that is,
--   @applicable p d@ holds when @d@ is a valid source document for the patch @p@.
--
-- prop> applicable mempty d
-- prop> forAll (patchesFrom d) $ \p -> applicable p d
-- prop> forAll (historyFrom d 2) $ \[p, q] -> applicable p d && applicable q (apply p d)
-- prop> forAll (historyFrom d 2) $ \[p, q] -> applicable (p <> q) d
applicable :: (Eq a) => Patch a -> Vector a -> Bool
applicable (Patch s) i = all applicable' s
  where
    applicable' (Insert x _)     = x <= Vector.length i
    applicable' (Delete x c)     = case i Vector.!? x of
                                    Just c' | c == c' -> True
                                    _ -> False
    applicable' (Replace x c _)  = case i Vector.!? x of
                                    Just c' | c == c' -> True
                                    _ -> False

-- | Returns true if a patch can be validly composed with another.
--   That is, @composable p q@ holds if @q@ can be validly applied after @p@.
--
--   prop> forAll (patchesFrom d) $ \p -> composable mempty p
--   prop> forAll (patchesFrom d) $ \p -> composable p mempty
--   prop> forAll (historyFrom d 2) $ \[p, q] -> composable p q
composable :: Eq a => Patch a -> Patch a -> Bool
composable (Patch a) (Patch b) = go a b (0 :: Int)
    where
      go [] _ _  = True
      go _ [] _    = True
      go (x:xs) (y:ys) off = let
          y' = over index (+ off) y
        in case comparing (^. index) x y' of
         LT -> go xs (y:ys) (off + offset x)
         GT -> go (x:xs) ys off
         EQ -> case (x,y') of
             (Delete {}, Insert {}) -> go xs ys (off + offset x)
             (Delete {}, _) -> go xs (y:ys) (off + offset x)
             (_, Insert {}) -> go (x:xs) ys off
             (Replace _ _ o, Replace _ n _) -> o == n && go xs ys off
             (Replace _ _ o, Delete _ n) -> o == n && go xs ys off
             (Insert _ o, Replace _ n _) -> o == n && go xs ys (off + offset x)
             (Insert _ o, Delete _ n) -> o == n && go xs ys (off + offset x)
      offset (Insert {}) = -1
      offset (Delete {}) = 1
      offset (Replace {}) = 0

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



-- | Given two diverging patches @p@ and @q@, @transform m p q@ returns
--   a pair of updated patches @(p',q')@ such that @q <> p'@ and
--   @p <> q'@ are equivalent patches that incorporate the changes
--   of /both/ @p@ and @q@, up to merge conflicts, which are handled by
--   the provided function @m@.
--
--   This is the standard @transform@ function of Operational Transformation
--   patch resolution techniques.
--
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith ours p q in apply (p <> q') d == apply (q <> p') d
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith ours p q in applicable p' (apply q d) && applicable q' (apply p d)
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith ours p q in composable p q' && composable q p'
--
--   This function is commutative iff @m@ is commutative.
--
--   prop> forAll (divergingPatchesFrom d) $ \(p,q) -> let (p', q') = transformWith (*) p q; (q'', p'') = transformWith (*) q p in p' == p'' && q' == q''
--
--   prop> forAll (patchesFrom d) $ \ p -> transformWith (*) mempty p == (mempty, p)
--   prop> forAll (patchesFrom d) $ \ p -> transformWith (*) p mempty == (p, mempty)
--   Some example conflict strategies are provided below.
transformWith :: (Eq a) => (a -> a -> a) -> Patch a -> Patch a -> (Patch a, Patch a)
transformWith conflict (Patch p) (Patch q)
  = let (a', b') = go p 0 q 0
    in  (Patch a', Patch b')
  where
    go [] _ [] _ = ([],[])
    go xs a [] _ = (map (over index (+ a)) xs, [])
    go [] _ ys b = ([], map (over index (+ b)) ys)
    go (x:xs) a (y:ys) b = 
      case comparing (^. index) x y of
        LT -> over _1 (over index (+ a) x:) $ go xs a (y:ys) (b + offset x)
        GT -> over _2 (over index (+ b) y:) $ go (x:xs) (a + offset y) ys b
        EQ -> case (x, y) of
           _ | x == y -> go xs (a + offset y) ys (b + offset x)
           (Insert i nx, Insert _ ny ) 
             -> let n = conflict nx ny
                 in cons2 (Replace (i + a) ny n, Replace (i + b) nx n)
                          (go xs (a + offset y) ys (b + offset x))
           (Replace i _ nx, Replace _ _ ny)
             -> let n = conflict nx ny
                 in cons2 (Replace (i + a) ny n, Replace (i + b) nx n)
                          (go xs a ys b)
           (Insert {}, _) -> over _1 (over index (+ a) x:) $ go xs a (y:ys) (b + offset x)
           (_, Insert {}) -> over _2 (over index (+ b) y:) $ go (x:xs) (a + offset y) ys b
           (Replace i _ nx, Delete  {})
             -> over _2 (over index (+ b) (Delete i nx):) $ go xs (a + offset y) ys b
           (Delete  {}, Replace i _ ny)
             -> over _1 (over index (+ a) (Delete i ny):) $ go xs a ys (b + offset x)
           (Delete  {}, Delete  {}) -> go xs (a + offset y) ys (b + offset x)
    offset (Insert {})  =  1
    offset (Delete {})  = -1
    offset (Replace {}) =  0
    cons2 (x,y) (xs, ys) = (x:xs, y:ys)

-- | Resolve a conflict by always using the left-hand side
ours :: a -> a -> a
ours = const

-- | Resolve a conflict by always using the right-hand side
theirs :: a -> a -> a
theirs = const

-- | A convenience version of 'transformWith' which resolves conflicts using 'mappend'.
transform :: (Eq a, Monoid a) => Patch a -> Patch a -> (Patch a, Patch a)
transform = transformWith (<>)

-- | Compute the difference between two documents, using the Wagner-Fischer algorithm. O(mn) time and space.
--
-- prop> apply (diff d e) d == e
--
-- prop> apply (diff d e) d == apply (inverse (diff e d)) d
--
-- prop> apply (diff a b <> diff b c) a == apply (diff a c) a
--
-- prop> applicable (diff a b) a
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



-- | The four different ways a hunk may have been manipulated.
data HunkStatus = Inserted | Deleted | Replaced | Unchanged deriving (Eq, Show, Read)

-- | The type for a series of hunks; a patch as it may be displayed to a user.
type Hunks a = [(Vector a, HunkStatus)]

-- | Render a patch on a document as a list of change hunks. Good for displaying
--   a patch to a user.
--
--   prop> forAll (patchesFrom d) $ \p -> Vector.concat (map fst (filter ((/= Deleted) . snd) (hunks p d))) == apply p d
hunks :: Patch a -> Vector a -> Hunks a
hunks (Patch s) i = map eachGroup $ List.groupBy ((==) `on` snd) $ go s i 0
  where go [] v _ | Vector.null v = []
                  | otherwise     = [(v, Unchanged)]
        go (a : as) v x
          | x' <- a ^. index
          = let (prefix, rest) = Vector.splitAt (x' - x) v
                hunk (Insert _ c) = (Vector.singleton c, Inserted)
                hunk (Replace _ _ c) = (Vector.singleton c, Replaced)
                hunk (Delete _ c) = (Vector.singleton c, Deleted)
                offset (Insert {}) = 0
                offset _ = 1
             in (if x' > x then ((prefix,Unchanged) :) else id) $ hunk a : go as (Vector.drop (offset a) rest) (x' + offset a)
        eachGroup r@((_,st):_) = (Vector.concat (map fst r), st)
        eachGroup [] = error "impossible!"
