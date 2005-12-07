--------------------------------------------------------------------------------
{-| Module      :  IntBag
    Copyright   :  (c) Daan Leijen 2002
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

  An efficient implementation of bags of integers on top of the "IntMap" module. 

  Many operations have a worst-case complexity of /O(min(n,W))/. This means that the
  operation can become linear in the number of elements  with a maximum of /W/ 
  -- the number of bits in an 'Int' (32 or 64). For more information, see
  the references in the "IntMap" module.
-}
---------------------------------------------------------------------------------}
module UU.DData.IntBag ( 
            -- * Bag type
              IntBag          -- instance Eq,Show
            
            -- * Operators
            , (\\)

            -- *Query
            , isEmpty
            , size
            , distinctSize
            , member
            , occur

            , subset
            , properSubset
            
            -- * Construction
            , empty
            , single
            , insert
            , insertMany
            , delete
            , deleteAll
            
            -- * Combine
            , union
            , difference
            , intersection
            , unions
            
            -- * Filter
            , filter
            , partition

            -- * Fold
            , fold
            , foldOccur
           
            -- * Conversion
            , elems

            -- ** List
            , toList
            , fromList

            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList

            -- ** Occurrence lists
            , toOccurList
            , toAscOccurList
            , fromOccurList
            , fromAscOccurList

            -- ** IntMap
            , toMap
            , fromMap
            , fromOccurMap
            
            -- * Debugging
            , showTree
            , showTreeWith
            ) where

import Prelude   hiding  (map,filter)
import qualified Prelude (map,filter)

import qualified UU.DData.IntMap as M

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ 

-- | /O(n+m)/. See 'difference'.
(\\) ::  IntBag -> IntBag -> IntBag
b1 \\ b2 = difference b1 b2

{--------------------------------------------------------------------
  IntBags are a simple wrapper around Maps, 'Map.Map'
--------------------------------------------------------------------}
-- | A bag of integers.
newtype IntBag  = IntBag (M.IntMap Int)

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is the bag empty?
isEmpty :: IntBag -> Bool
isEmpty (IntBag m)  
  = M.isEmpty m

-- | /O(n)/. Returns the number of distinct elements in the bag, ie. (@distinctSize bag == length (nub (toList bag))@).
distinctSize :: IntBag -> Int
distinctSize (IntBag m)     
  = M.size m

-- | /O(n)/. The number of elements in the bag.
size :: IntBag -> Int
size b
  = foldOccur (\x n m -> n+m) 0 b

-- | /O(min(n,W))/. Is the element in the bag?
member ::  Int -> IntBag -> Bool
member x m
  = (occur x m > 0)

-- | /O(min(n,W))/. The number of occurrences of an element in the bag.
occur ::  Int -> IntBag -> Int
occur x (IntBag m)
  = case M.lookup x m of
      Nothing -> 0
      Just n  -> n

-- | /O(n+m)/. Is this a subset of the bag? 
subset ::  IntBag -> IntBag -> Bool
subset (IntBag m1) (IntBag m2)
  = M.subsetBy (<=) m1 m2

-- | /O(n+m)/. Is this a proper subset? (ie. a subset and not equal)
properSubset ::  IntBag -> IntBag -> Bool
properSubset b1 b2
  = subset b1 b2 && (b1 /= b2)

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. Create an empty bag.
empty :: IntBag
empty
  = IntBag (M.empty)

-- | /O(1)/. Create a singleton bag.
single :: Int -> IntBag
single x 
  = IntBag (M.single x 0)
    
{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(min(n,W))/. Insert an element in the bag.
insert ::  Int -> IntBag -> IntBag
insert x (IntBag m)          
  = IntBag (M.insertWith (+) x 1 m)

-- | /O(min(n,W))/. The expression (@insertMany x count bag@)
-- inserts @count@ instances of @x@ in the bag @bag@.
insertMany ::  Int -> Int -> IntBag -> IntBag
insertMany x count (IntBag m)          
  = IntBag (M.insertWith (+) x count m)

-- | /O(min(n,W))/. Delete a single element.
delete ::  Int -> IntBag -> IntBag
delete x (IntBag m)
  = IntBag (M.updateWithKey f x m)
  where
    f x n  | n > 0     = Just (n-1)
           | otherwise = Nothing

-- | /O(min(n,W))/. Delete all occurrences of an element.
deleteAll ::  Int -> IntBag -> IntBag
deleteAll x (IntBag m)
  = IntBag (M.delete x m)

{--------------------------------------------------------------------
  Combine
--------------------------------------------------------------------}
-- | /O(n+m)/. Union of two bags. The union adds the elements together.
--
-- > IntBag\> union (fromList [1,1,2]) (fromList [1,2,2,3])
-- > {1,1,1,2,2,2,3}
union ::  IntBag -> IntBag -> IntBag
union (IntBag t1) (IntBag t2)
  = IntBag (M.unionWith (+) t1 t2)

-- | /O(n+m)/. Intersection of two bags.
--
-- > IntBag\> intersection (fromList [1,1,2]) (fromList [1,2,2,3])
-- > {1,2}
intersection ::  IntBag -> IntBag -> IntBag
intersection (IntBag t1) (IntBag t2)
  = IntBag (M.intersectionWith min t1 t2)

-- | /O(n+m)/. Difference between two bags.
--
-- > IntBag\> difference (fromList [1,1,2]) (fromList [1,2,2,3])
-- > {1}
difference   ::  IntBag -> IntBag -> IntBag
difference (IntBag t1) (IntBag t2)
  = IntBag (M.differenceWithKey f t1 t2)
  where
    f x n m  | n-m > 0   = Just (n-m)
             | otherwise = Nothing

-- | The union of a list of bags.
unions ::  [IntBag] -> IntBag
unions bags
  = IntBag (M.unions [m | IntBag m <- bags])

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy some predicate.
filter ::  (Int -> Bool) -> IntBag -> IntBag
filter p (IntBag m)
  = IntBag (M.filterWithKey (\x n -> p x) m)

-- | /O(n)/. Partition the bag according to some predicate.
partition ::  (Int -> Bool) -> IntBag -> (IntBag,IntBag)
partition p (IntBag m)
  = (IntBag l,IntBag r)
  where
    (l,r) = M.partitionWithKey (\x n -> p x) m

{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over each element in the bag.
fold :: (Int -> b -> b) -> b -> IntBag -> b
fold f z (IntBag m)
  = M.foldWithKey apply z m
  where
    apply x n z  | n > 0     = apply x (n-1) (f x z)
                 | otherwise = z

-- | /O(n)/. Fold over all occurrences of an element at once. 
-- In a call (@foldOccur f z bag@), the function @f@ takes
-- the element first and than the occur count.
foldOccur :: (Int -> Int -> b -> b) -> b -> IntBag -> b
foldOccur f z (IntBag m)
  = M.foldWithKey f z m

{--------------------------------------------------------------------
  List variations 
--------------------------------------------------------------------}
-- | /O(n)/. The list of elements.
elems :: IntBag -> [Int]
elems s
  = toList s

{--------------------------------------------------------------------
  Lists 
--------------------------------------------------------------------}
-- | /O(n)/. Create a list with all elements.
toList :: IntBag -> [Int]
toList s
  = toAscList s

-- | /O(n)/. Create an ascending list of all elements.
toAscList :: IntBag -> [Int]
toAscList (IntBag m)
  = [y | (x,n) <- M.toAscList m, y <- replicate n x]


-- | /O(n*min(n,W))/. Create a bag from a list of elements.
fromList ::  [Int] -> IntBag 
fromList xs
  = IntBag (M.fromListWith (+) [(x,1) | x <- xs])

-- | /O(n*min(n,W))/. Create a bag from an ascending list.
fromAscList :: [Int] -> IntBag 
fromAscList xs
  = IntBag (M.fromAscListWith (+) [(x,1) | x <- xs])

-- | /O(n*min(n,W))/. Create a bag from an ascending list of distinct elements.
fromDistinctAscList :: [Int] -> IntBag 
fromDistinctAscList xs
  = IntBag (M.fromDistinctAscList [(x,1) | x <- xs])

-- | /O(n)/. Create a list of element\/occurrence pairs.
toOccurList :: IntBag -> [(Int,Int)]
toOccurList b
  = toAscOccurList b

-- | /O(n)/. Create an ascending list of element\/occurrence pairs.
toAscOccurList :: IntBag -> [(Int,Int)]
toAscOccurList (IntBag m)
  = M.toAscList m

-- | /O(n*min(n,W))/. Create a bag from a list of element\/occurrence pairs.
fromOccurList ::  [(Int,Int)] -> IntBag
fromOccurList xs
  = IntBag (M.fromListWith (+) (Prelude.filter (\(x,i) -> i > 0) xs))

-- | /O(n*min(n,W))/. Create a bag from an ascending list of element\/occurrence pairs.
fromAscOccurList ::  [(Int,Int)] -> IntBag
fromAscOccurList xs
  = IntBag (M.fromAscListWith (+) (Prelude.filter (\(x,i) -> i > 0) xs))

{--------------------------------------------------------------------
  Maps
--------------------------------------------------------------------}
-- | /O(1)/. Convert to an 'IntMap.IntMap' from elements to number of occurrences.
toMap   :: IntBag -> M.IntMap Int
toMap (IntBag m)
  = m

-- | /O(n)/. Convert a 'IntMap.IntMap' from elements to occurrences into a bag.
fromMap ::  M.IntMap Int -> IntBag
fromMap m
  = IntBag (M.filter (>0) m)

-- | /O(1)/. Convert a 'IntMap.IntMap' from elements to occurrences into a bag.
-- Assumes that the 'IntMap.IntMap' contains only elements that occur at least once.
fromOccurMap :: M.IntMap Int -> IntBag
fromOccurMap m
  = IntBag m

{--------------------------------------------------------------------
  Eq, Ord
--------------------------------------------------------------------}
instance Eq (IntBag) where
  (IntBag m1) == (IntBag m2)  = (m1==m2) 
  (IntBag m1) /= (IntBag m2)  = (m1/=m2)

{--------------------------------------------------------------------
  Show
--------------------------------------------------------------------}
instance Show (IntBag) where
  showsPrec d b  = showSet (toAscList b)

showSet :: Show a => [a] -> ShowS
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
-- | /O(n)/. Show the tree structure that implements the 'IntBag'. The tree
-- is shown as a compressed and /hanging/.
showTree :: IntBag -> String
showTree bag
  = showTreeWith True False bag

-- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
-- the tree that implements the bag. The tree is shown /hanging/ when @hang@ is @True@ 
-- and otherwise as a /rotated/ tree. When @wide@ is @True@ an extra wide version
-- is shown.
showTreeWith :: Bool -> Bool -> IntBag -> String
showTreeWith hang wide (IntBag m)
  = M.showTreeWith hang wide m

