--------------------------------------------------------------------------------
{-| Module      :  Queue
    Copyright   :  (c) Daan Leijen 2002
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

  An efficient implementation of queues (FIFO buffers). Based on:

  * Chris Okasaki, \"/Simple and Efficient Purely Functional Queues and Deques/\",
    Journal of Functional Programming 5(4):583-592, October 1995.
-}
---------------------------------------------------------------------------------}
module UU.DData.Queue ( 
            -- * Queue type
              Queue          -- instance Eq,Show

            -- * Operators
            , (<>)
            
            -- * Query
            , isEmpty
            , length
            , head
            , tail
            , front

            -- * Construction
            , empty
            , single
            , insert
            , append
            
            -- * Filter
            , filter
            , partition

            -- * Fold
            , foldL
            , foldR
           
            -- * Conversion
            , elems

            -- ** List
            , toList
            , fromList
            ) where

import qualified Prelude as P (length,filter)
import Prelude   hiding       (length,head,tail,filter)
import qualified List

-- just for testing
-- import QuickCheck 

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixr 5 <>

-- | /O(n)/. Append two queues, see 'append'.
(<>) :: Queue a -> Queue a -> Queue a
s <> t
  = append s t

{--------------------------------------------------------------------
  Queue.
  Invariants for @(Queue xs ys zs)@:
  * @length ys <= length xs@
  * @length zs == length xs - length ys@
--------------------------------------------------------------------}
-- A queue of elements @a@.
data Queue a  = Queue [a] [a] [a]

{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}

-- | /O(1)/. Is the queue empty?
isEmpty :: Queue a -> Bool
isEmpty (Queue xs ys zs)
  = null xs

-- | /O(n)/. The number of elements in the queue.
length :: Queue a -> Int
length (Queue xs ys zs)
  = P.length xs + P.length ys

-- | /O(1)/. The element in front of the queue. Raises an error
-- when the queue is empty.
head :: Queue a -> a
head (Queue xs ys zs)
  = case xs of
      (x:xx)  -> x
      []      -> error "Queue.head: empty queue"

-- | /O(1)/. The tail of the queue.
-- Raises an error when the queue is empty.
tail :: Queue a -> Queue a
tail (Queue xs ys zs)
  = case xs of
      (x:xx)  -> queue xx ys zs
      []      -> error "Queue.tail: empty queue"

-- | /O(1)/. The head and tail of the queue.
front :: Queue a -> Maybe (a,Queue a)
front (Queue xs ys zs)
  = case xs of
      (x:xx)  -> Just (x,queue xx ys zs)
      []      -> Nothing


{--------------------------------------------------------------------
  Construction  
--------------------------------------------------------------------}
-- | /O(1)/. The empty queue.
empty :: Queue a
empty 
  = Queue [] [] []

-- | /O(1)/. A queue of one element.
single :: a -> Queue a
single x
  = Queue [x] [] [x]

-- | /O(1)/. Insert an element at the back of a queue.
insert :: a -> Queue a -> Queue a
insert x (Queue xs ys zs)
  = queue xs (x:ys) zs


-- | /O(n)/. Append two queues.
append :: Queue a -> Queue a -> Queue a
append (Queue xs1 ys1 zs1) (Queue xs2 ys2 zs2)
  = Queue (xs1++xs2) (ys1++ys2) (zs1++zs2)

{--------------------------------------------------------------------
  Filter
--------------------------------------------------------------------}
-- | /O(n)/. Filter elements according to some predicate.
filter :: (a -> Bool) -> Queue a -> Queue a
filter pred (Queue xs ys zs)
  = balance xs' ys'
  where
    xs' = P.filter pred xs
    ys' = P.filter pred ys

-- | /O(n)/. Partition the elements according to some predicate.
partition :: (a -> Bool) -> Queue a -> (Queue a,Queue a)
partition pred (Queue xs ys zs)
  = (balance xs1 ys1, balance xs2 ys2)
  where
    (xs1,xs2) = List.partition pred xs
    (ys1,ys2) = List.partition pred ys


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over the elements from left to right (ie. head to tail).
foldL :: (b -> a -> b) -> b -> Queue a -> b
foldL f z (Queue xs ys zs)
  = foldr (flip f) (foldl f z xs) ys

-- | /O(n)/. Fold over the elements from right to left (ie. tail to head).
foldR :: (a -> b -> b) -> b -> Queue a -> b
foldR f z (Queue xs ys zs)
  = foldr f (foldl (flip f) z ys) xs


{--------------------------------------------------------------------
  Conversion
--------------------------------------------------------------------}
-- | /O(n)/. The elements of a queue.
elems :: Queue a -> [a]
elems q
  = toList q

-- | /O(n)/. Convert to a list.
toList :: Queue a -> [a]
toList (Queue xs ys zs)
  = xs ++ reverse ys

-- | /O(n)/. Convert from a list.
fromList :: [a] -> Queue a
fromList xs
  = Queue xs [] xs


{--------------------------------------------------------------------
  instance Eq, Show
--------------------------------------------------------------------}
instance Eq a => Eq (Queue a) where
  q1 == q2  = toList q1 == toList q2

instance Show a => Show (Queue a) where
  showsPrec d q  = showsPrec d (toList q)


{--------------------------------------------------------------------
  Smart constructor:
  Note that @(queue xs ys zs)@ is always called with 
    @(length zs == length xs - length ys + 1)@. and thus
  @rotate@ is always called when @(length xs == length ys+1)@.
--------------------------------------------------------------------}
balance :: [a] -> [a] -> Queue a
balance xs ys
  = Queue qs [] qs
  where
    qs = xs ++ reverse ys

queue :: [a] -> [a] -> [a] -> Queue a
queue xs ys (z:zs) = Queue xs ys zs
queue xs ys []     = Queue qs [] qs
                   where
                     qs = rotate xs ys []

-- @(rotate xs ys []) == xs ++ reverse ys)@ 
rotate :: [a] -> [a] -> [a] -> [a]
rotate []     [y]    zs  = y:zs
rotate (x:xs) (y:ys) zs  = x:rotate xs ys (y:zs) 
rotate xs     ys     zs  = error "Queue.rotate: unbalanced queue"


valid :: Queue a -> Bool
valid (Queue xs ys zs)
  = (P.length zs == P.length xs - P.length ys) && (P.length ys <= P.length xs)

{-
{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 10000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced queues
--------------------------------------------------------------------}
instance Arbitrary a => Arbitrary (Queue a) where
  arbitrary = do{ qs <- arbitrary
                ; let (ys,xs) = splitAt (P.length qs `div` 2) qs
                ; return (Queue xs ys (xs ++ reverse ys))
                }


prop_Valid :: Queue Int -> Bool
prop_Valid q
  = valid q

prop_InsertLast :: [Int] -> Property
prop_InsertLast xs
  = not (null xs) ==> head (foldr insert empty xs) == last xs

prop_InsertValid :: [Int] -> Bool
prop_InsertValid xs
  = valid (foldr insert empty xs)

prop_Queue :: [Int] -> Bool
prop_Queue xs
  = toList (foldl (flip insert) empty xs) == foldr (:) [] xs
  
prop_List :: [Int] -> Bool
prop_List xs
  = toList (fromList xs) == xs

prop_TailValid :: [Int] -> Bool
prop_TailValid xs
  = valid (tail (foldr insert empty (1:xs)))
-}

