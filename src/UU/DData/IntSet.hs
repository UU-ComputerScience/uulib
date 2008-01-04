{-# OPTIONS -cpp -fglasgow-exts #-}
--------------------------------------------------------------------------------
{-| Module      :  IntSet
    Copyright   :  (c) Daan Leijen 2002
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

  An efficient implementation of integer sets.
  
  1) The 'filter' function clashes with the "Prelude". 
      If you want to use "IntSet" unqualified, this function should be hidden.

      > import Prelude hiding (filter)
      > import IntSet

      Another solution is to use qualified names. 

      > import qualified IntSet
      >
      > ... IntSet.fromList [1..5]

      Or, if you prefer a terse coding style:

      > import qualified IntSet as S
      >
      > ... S.fromList [1..5]

  2) The implementation is based on /big-endian patricia trees/. This data structure 
  performs especially well on binary operations like 'union' and 'intersection'. However,
  my benchmarks show that it is also (much) faster on insertions and deletions when 
  compared to a generic size-balanced set implementation (see "Set").
   
  *  Chris Okasaki and Andy Gill,  \"/Fast Mergeable Integer Maps/\",
     Workshop on ML, September 1998, pages 77--86, <http://www.cse.ogi.edu/~andy/pub/finite.htm>

  *  D.R. Morrison, \"/PATRICIA -- Practical Algorithm To Retrieve Information
     Coded In Alphanumeric/\", Journal of the ACM, 15(4), October 1968, pages 514--534.

  3) Many operations have a worst-case complexity of /O(min(n,W))/. This means that the
    operation can become linear in the number of elements 
    with a maximum of /W/ -- the number of bits in an 'Int' (32 or 64). 
-}
---------------------------------------------------------------------------------}
module UU.DData.IntSet  ( 
            -- * Set type
              IntSet          -- instance Eq,Show

            -- * Operators
            , (\\)

            -- * Query
            , isEmpty
            , size
            , member
            , subset
            , properSubset
            
            -- * Construction
            , empty
            , single
            , insert
            , delete
            
            -- * Combine
            , union, unions
            , difference
            , intersection
            
            -- * Filter
            , filter
            , partition
            , split
            , splitMember

            -- * Fold
            , fold

            -- * Conversion
            -- ** List
            , elems
            , toList
            , fromList
            
            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList
                        
            -- * Debugging
            , showTree
            , showTreeWith
            ) where


import Prelude hiding (lookup,filter)
import Bits 
import Int

{-
-- just for testing
import QuickCheck 
import List (nub,sort)
import qualified List
-}


#ifdef __GLASGOW_HASKELL__
{--------------------------------------------------------------------
  GHC: use unboxing to get @shiftRL@ inlined.
--------------------------------------------------------------------}
#if __GLASGOW_HASKELL__ >= 503
import GHC.Word
import GHC.Exts ( Word(..), Int(..), shiftRL# )
#else
import Word
import GlaExts ( Word(..), Int(..), shiftRL# )
#endif


type Nat = Word

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w

shiftRL :: Nat -> Int -> Nat
shiftRL (W# x) (I# i)
  = W# (shiftRL# x i)

#elif __HUGS__
{--------------------------------------------------------------------
 Hugs: 
 * raises errors on boundary values when using 'fromIntegral'
   but not with the deprecated 'fromInt/toInt'. 
 * Older Hugs doesn't define 'Word'.
 * Newer Hugs defines 'Word' in the Prelude but no operations.
--------------------------------------------------------------------}
import Word

type Nat = Word32   -- illegal on 64-bit platforms!

natFromInt :: Int -> Nat
natFromInt i = fromInt i

intFromNat :: Nat -> Int
intFromNat w = toInt w

shiftRL :: Nat -> Int -> Nat
shiftRL x i   = shiftR x i

#else
{--------------------------------------------------------------------
  'Standard' Haskell
  * A "Nat" is a natural machine word (an unsigned Int)
--------------------------------------------------------------------}
import Word

type Nat = Word

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w

shiftRL :: Nat -> Int -> Nat
shiftRL w i   = shiftR w i

#endif

infixl 9 \\ --

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
-- | /O(n+m)/. See 'difference'.
(\\) :: IntSet -> IntSet -> IntSet
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Types  
--------------------------------------------------------------------}
-- | A set of integers.
data IntSet = Nil
            | Tip !Int
            | Bin !Prefix !Mask !IntSet !IntSet

type Prefix = Int
type Mask   = Int

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the set empty?
isEmpty :: IntSet -> Bool
isEmpty Nil   = True
isEmpty other = False

-- | /O(n)/. Cardinality of the set.
size :: IntSet -> Int
size t
  = case t of
      Bin p m l r -> size l + size r
      Tip y -> 1
      Nil   -> 0

-- | /O(min(n,W))/. Is the value a member of the set?
member :: Int -> IntSet -> Bool
member x t
  = case t of
      Bin p m l r 
        | nomatch x p m -> False
        | zero x m      -> member x l
        | otherwise     -> member x r
      Tip y -> (x==y)
      Nil   -> False
    
-- 'lookup' is used by 'intersection' for left-biasing
lookup :: Int -> IntSet -> Maybe Int
lookup x t
  = case t of
      Bin p m l r 
        | nomatch x p m -> Nothing
        | zero x m      -> lookup x l
        | otherwise     -> lookup x r
      Tip y 
        | (x==y)    -> Just y
        | otherwise -> Nothing
      Nil -> Nothing

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty :: IntSet
empty
  = Nil

-- | /O(1)/. A set of one element.
single :: Int -> IntSet
single x
  = Tip x

{--------------------------------------------------------------------
  Insert
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Add a value to the set. When the value is already
-- an element of the set, it is replaced by the new one, ie. 'insert'
-- is left-biased.
insert :: Int -> IntSet -> IntSet
insert x t
  = case t of
      Bin p m l r 
        | nomatch x p m -> join x (Tip x) p t
        | zero x m      -> Bin p m (insert x l) r
        | otherwise     -> Bin p m l (insert x r)
      Tip y 
        | x==y          -> Tip x
        | otherwise     -> join x (Tip x) y t
      Nil -> Tip x

-- right-biased insertion, used by 'union'
insertR :: Int -> IntSet -> IntSet
insertR x t
  = case t of
      Bin p m l r 
        | nomatch x p m -> join x (Tip x) p t
        | zero x m      -> Bin p m (insert x l) r
        | otherwise     -> Bin p m l (insert x r)
      Tip y 
        | x==y          -> t
        | otherwise     -> join x (Tip x) y t
      Nil -> Tip x

-- | /O(min(n,W))/. Delete a value in the set. Returns the
-- original set when the value was not present.
delete :: Int -> IntSet -> IntSet
delete x t
  = case t of
      Bin p m l r 
        | nomatch x p m -> t
        | zero x m      -> bin p m (delete x l) r
        | otherwise     -> bin p m l (delete x r)
      Tip y 
        | x==y          -> Nil
        | otherwise     -> t
      Nil -> Nil


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
-- | The union of a list of sets.
unions :: [IntSet] -> IntSet
unions xs
  = foldlStrict union empty xs


-- | /O(n+m)/. The union of two sets. 
union :: IntSet -> IntSet -> IntSet
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = union1
  | shorter m2 m1  = union2
  | p1 == p2       = Bin p1 m1 (union l1 l2) (union r1 r2)
  | otherwise      = join p1 t1 p2 t2
  where
    union1  | nomatch p2 p1 m1  = join p1 t1 p2 t2
            | zero p2 m1        = Bin p1 m1 (union l1 t2) r1
            | otherwise         = Bin p1 m1 l1 (union r1 t2)

    union2  | nomatch p1 p2 m2  = join p1 t1 p2 t2
            | zero p1 m2        = Bin p2 m2 (union t1 l2) r2
            | otherwise         = Bin p2 m2 l2 (union t1 r2)

union (Tip x) t = insert x t
union t (Tip x) = insertR x t  -- right bias
union Nil t     = t
union t Nil     = t


{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference between two sets. 
difference :: IntSet -> IntSet -> IntSet
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = difference1
  | shorter m2 m1  = difference2
  | p1 == p2       = bin p1 m1 (difference l1 l2) (difference r1 r2)
  | otherwise      = t1
  where
    difference1 | nomatch p2 p1 m1  = t1
                | zero p2 m1        = bin p1 m1 (difference l1 t2) r1
                | otherwise         = bin p1 m1 l1 (difference r1 t2)

    difference2 | nomatch p1 p2 m2  = t1
                | zero p1 m2        = difference t1 l2
                | otherwise         = difference t1 r2

difference t1@(Tip x) t2 
  | member x t2  = Nil
  | otherwise    = t1

difference Nil t     = Nil
difference t (Tip x) = delete x t
difference t Nil     = t



{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The intersection of two sets. 
intersection :: IntSet -> IntSet -> IntSet
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = intersection1
  | shorter m2 m1  = intersection2
  | p1 == p2       = bin p1 m1 (intersection l1 l2) (intersection r1 r2)
  | otherwise      = Nil
  where
    intersection1 | nomatch p2 p1 m1  = Nil
                  | zero p2 m1        = intersection l1 t2
                  | otherwise         = intersection r1 t2

    intersection2 | nomatch p1 p2 m2  = Nil
                  | zero p1 m2        = intersection t1 l2
                  | otherwise         = intersection t1 r2

intersection t1@(Tip x) t2 
  | member x t2  = t1
  | otherwise    = Nil
intersection t (Tip x) 
  = case lookup x t of
      Just y  -> Tip y
      Nothing -> Nil
intersection Nil t = Nil
intersection t Nil = Nil



{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
properSubset :: IntSet -> IntSet -> Bool
properSubset t1 t2
  = case subsetCmp t1 t2 of 
      LT -> True
      ge -> False

subsetCmp t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = subsetCmpLt
  | p1 == p2       = subsetCmpEq
  | otherwise      = GT  -- disjoint
  where
    subsetCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = subsetCmp t1 l2
                | otherwise         = subsetCmp t1 r2
    subsetCmpEq = case (subsetCmp l1 l2, subsetCmp r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    other   -> LT

subsetCmp (Bin p m l r) t  = GT
subsetCmp (Tip x) (Tip y)  
  | x==y       = EQ
  | otherwise  = GT  -- disjoint
subsetCmp (Tip x) t        
  | member x t = LT
  | otherwise  = GT  -- disjoint
subsetCmp Nil Nil = EQ
subsetCmp Nil t   = LT

-- | /O(n+m)/. Is this a subset?
subset :: IntSet -> IntSet -> Bool
subset t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then subset t1 l2
                                                      else subset t1 r2)                     
  | otherwise      = (p1==p2) && subset l1 l2 && subset r1 r2
subset (Bin p m l r) t  = False
subset (Tip x) t        = member x t
subset Nil t            = True


{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter :: (Int -> Bool) -> IntSet -> IntSet
filter pred t
  = case t of
      Bin p m l r 
        -> bin p m (filter pred l) (filter pred r)
      Tip x 
        | pred x    -> t
        | otherwise -> Nil
      Nil -> Nil

-- | /O(n)/. partition the set according to some predicate.
partition :: (Int -> Bool) -> IntSet -> (IntSet,IntSet)
partition pred t
  = case t of
      Bin p m l r 
        -> let (l1,l2) = partition pred l
               (r1,r2) = partition pred r
           in (bin p m l1 r1, bin p m l2 r2)
      Tip x 
        | pred x    -> (t,Nil)
        | otherwise -> (Nil,t)
      Nil -> (Nil,Nil)


-- | /O(log n)/. The expression (@split x set@) is a pair @(set1,set2)@
-- where all elements in @set1@ are lower than @x@ and all elements in
-- @set2@ larger than @x@.
split :: Int -> IntSet -> (IntSet,IntSet)
split x t
  = case t of
      Bin p m l r
        | zero x m  -> let (lt,gt) = split x l in (lt,union gt r)
        | otherwise -> let (lt,gt) = split x r in (union l lt,gt)
      Tip y 
        | x>y       -> (t,Nil)
        | x<y       -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Int -> IntSet -> (Bool,IntSet,IntSet)
splitMember x t
  = case t of
      Bin p m l r
        | zero x m  -> let (found,lt,gt) = splitMember x l in (found,lt,union gt r)
        | otherwise -> let (found,lt,gt) = splitMember x r in (found,union l lt,gt)
      Tip y 
        | x>y       -> (False,t,Nil)
        | x<y       -> (False,Nil,t)
        | otherwise -> (True,Nil,Nil)
      Nil -> (False,Nil,Nil)


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over the elements of a set in an unspecified order.
--
-- > sum set   = fold (+) 0 set
-- > elems set = fold (:) [] set
fold :: (Int -> b -> b) -> b -> IntSet -> b
fold f z t
  = foldR f z t

foldR :: (Int -> b -> b) -> b -> IntSet -> b
foldR f z t
  = case t of
      Bin p m l r -> foldR f (foldR f z r) l
      Tip x       -> f x z
      Nil         -> z
          
{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/. The elements of a set.
elems :: IntSet -> [Int]
elems s
  = toList s

{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(n)/. Convert the set to a list of elements.
toList :: IntSet -> [Int]
toList t
  = fold (:) [] t

-- | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: IntSet -> [Int]
toAscList t   
  = -- NOTE: the following algorithm only works for big-endian trees
    let (pos,neg) = span (>=0) (foldR (:) [] t) in neg ++ pos

-- | /O(n*min(n,W))/. Create a set from a list of integers.
fromList :: [Int] -> IntSet
fromList xs
  = foldlStrict ins empty xs
  where
    ins t x  = insert x t

-- | /O(n*min(n,W))/. Build a set from an ascending list of elements.
fromAscList :: [Int] -> IntSet 
fromAscList xs
  = fromList xs

-- | /O(n*min(n,W))/. Build a set from an ascending list of distinct elements.
fromDistinctAscList :: [Int] -> IntSet
fromDistinctAscList xs
  = fromList xs


{--------------------------------------------------------------------
  Eq 
--------------------------------------------------------------------}
instance Eq IntSet where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2

equal :: IntSet -> IntSet -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2) 
equal (Tip x) (Tip y)
  = (x==y)
equal Nil Nil = True
equal t1 t2   = False

nequal :: IntSet -> IntSet -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2) 
nequal (Tip x) (Tip y)
  = (x/=y)
nequal Nil Nil = False
nequal t1 t2   = True

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show IntSet where
  showsPrec d s  = showSet (toList s)

showSet :: [Int] -> ShowS
showSet []     
  = showString "{}" 
showSet (x:xs) 
  = showChar '{' . shows x . showTail xs
  where
    showTail []     = showChar '}'
    showTail (x:xs) = showChar ',' . shows x . showTail xs

{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: IntSet -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
 the tree that implements the set. If @hang@ is
 @True@, a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is true, an extra wide version is shown.
-}
showTreeWith :: Bool -> Bool -> IntSet -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Bool -> [String] -> [String] -> IntSet -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBin p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip x
          -> showsBars lbars . showString " " . shows x . showString "\n" 
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: Bool -> [String] -> IntSet -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p m l r
          -> showsBars bars . showString (showBin p m) . showString "\n" . 
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip x
          -> showsBars bars . showString " " . shows x . showString "\n" 
      Nil -> showsBars bars . showString "|\n" 
      
showBin p m
  = "*" -- ++ show (p,m)

showWide wide bars 
  | wide      = showString (concat (reverse bars)) . showString "|\n" 
  | otherwise = id

showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node

node           = "+--"
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars


{--------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------}
{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: Prefix -> IntSet -> Prefix -> IntSet -> IntSet
join p1 t1 p2 t2
  | zero p1 m = Bin p m t1 t2
  | otherwise = Bin p m t2 t1
  where
    m = branchMask p1 p2
    p = mask p1 m

{--------------------------------------------------------------------
  @bin@ assures that we never have empty trees within a tree.
--------------------------------------------------------------------}
bin :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin p m l Nil = l
bin p m Nil r = r
bin p m l r   = Bin p m l r

  
{--------------------------------------------------------------------
  Endian independent bit twiddling
--------------------------------------------------------------------}
zero :: Int -> Mask -> Bool
zero i m
  = (natFromInt i) .&. (natFromInt m) == 0

nomatch,match :: Int -> Prefix -> Mask -> Bool
nomatch i p m
  = (mask i m) /= p

match i p m
  = (mask i m) == p

mask :: Int -> Mask -> Prefix
mask i m
  = maskW (natFromInt i) (natFromInt m)


{--------------------------------------------------------------------
  Big endian operations  
--------------------------------------------------------------------}
maskW :: Nat -> Nat -> Prefix
maskW i m
  = intFromNat (i .&. (complement (m-1) `xor` m))

shorter :: Mask -> Mask -> Bool
shorter m1 m2
  = (natFromInt m1) > (natFromInt m2)

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  = intFromNat (highestBitMask (natFromInt p1 `xor` natFromInt p2))
  
{----------------------------------------------------------------------
  Finding the highest bit (mask) in a word [x] can be done efficiently in
  three ways:
  * convert to a floating point value and the mantissa tells us the 
    [log2(x)] that corresponds with the highest bit position. The mantissa 
    is retrieved either via the standard C function [frexp] or by some bit 
    twiddling on IEEE compatible numbers (float). Note that one needs to 
    use at least [double] precision for an accurate mantissa of 32 bit 
    numbers.
  * use bit twiddling, a logarithmic sequence of bitwise or's and shifts (bit).
  * use processor specific assembler instruction (asm).

  The most portable way would be [bit], but is it efficient enough?
  I have measured the cycle counts of the different methods on an AMD 
  Athlon-XP 1800 (~ Pentium III 1.8Ghz) using the RDTSC instruction:

  highestBitMask: method  cycles
                  --------------
                   frexp   200
                   float    33
                   bit      11
                   asm      12

  highestBit:     method  cycles
                  --------------
                   frexp   195
                   float    33
                   bit      11
                   asm      11

  Wow, the bit twiddling is on today's RISC like machines even faster
  than a single CISC instruction (BSR)!
----------------------------------------------------------------------}

{----------------------------------------------------------------------
  [highestBitMask] returns a word where only the highest bit is set.
  It is found by first setting all bits in lower positions than the 
  highest bit and than taking an exclusive or with the original value.
  Allthough the function may look expensive, GHC compiles this into
  excellent C code that subsequently compiled into highly efficient
  machine code. The algorithm is derived from Jorg Arndt's FXT library.
----------------------------------------------------------------------}
highestBitMask :: Nat -> Nat
highestBitMask x
  = case (x .|. shiftRL x 1) of 
     x -> case (x .|. shiftRL x 2) of 
      x -> case (x .|. shiftRL x 4) of 
       x -> case (x .|. shiftRL x 8) of 
        x -> case (x .|. shiftRL x 16) of 
         x -> case (x .|. shiftRL x 32) of   -- for 64 bit platforms
          x -> (x `xor` (shiftRL x 1))


{--------------------------------------------------------------------
  Utilities 
--------------------------------------------------------------------}
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)


{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
testTree :: [Int] -> IntSet
testTree xs   = fromList xs
test1 = testTree [1..20]
test2 = testTree [30,29..10]
test3 = testTree [1,4,6,89,2323,53,43,234,5,79,12,9,24,9,8,423,8,42,4,8,9,3]

{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance Arbitrary IntSet where
  arbitrary = do{ xs <- arbitrary
                ; return (fromList xs)
                }


{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x
  = (insert x empty == single x)

prop_InsertDelete :: Int -> IntSet -> Property
prop_InsertDelete k t
  = not (member k t) ==> delete k (insert k t) == t


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionInsert :: Int -> IntSet -> Bool
prop_UnionInsert x t
  = union t (single x) == insert x t

prop_UnionAssoc :: IntSet -> IntSet -> IntSet -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: IntSet -> IntSet -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == union t2 t1)

prop_Diff :: [Int] -> [Int] -> Bool
prop_Diff xs ys
  =  toAscList (difference (fromList xs) (fromList ys))
    == List.sort ((List.\\) (nub xs)  (nub ys))

prop_Int :: [Int] -> [Int] -> Bool
prop_Int xs ys
  =  toAscList (intersection (fromList xs) (fromList ys))
    == List.sort (nub ((List.intersect) (xs)  (ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [0..n::Int]
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs
  = (sort (nub xs) == toAscList (fromList xs))
-}

