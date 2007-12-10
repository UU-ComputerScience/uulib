
import UU.DData.MultiSet

import Test.QuickCheck

type MInt = MultiSet Int

-- Can a singleton multiset constructed from a singleton list?
propSingle :: Int -> Bool
propSingle x = single x == fromList [x]

-- Singleton multisets have one occurrence of an element.
propSingle2 :: Int -> Bool
propSingle2 x = occur x (single x) == 1

-- An element occurring once has occurrence zero if deleted.
propDelete :: Int -> Bool
propDelete x = (occur x $ delete x $ fromList [x]) == 0

-- An element with 0 occurrences does not show in the occurrences list.
propDelete2 :: Int -> Bool
propDelete2 x = (toOccurList $ delete x $ fromList [x]) == []

-- Insertion of 0-occurring elements
propInsert :: Int -> Bool
propInsert x = (occur x $ insertMany x 0 empty) == 0

-- Deletiong of an element with one occurrence yields a valid multiset.
propValid :: Int -> Bool
propValid x = valid $ delete x $ fromList [x]

-- Union of non-disjoint multisets adds the occurrences of overlapping elements.
propUnion :: Bool
propUnion =  (toOccurList $ unions [fromList "abbc", fromList "aabdd"]) == [('a',3),('b',3),('c',1),('d',2)]

-- This property fails if for example the result of delete has the following occurrence list [('a',0)]
-- while equality assumes that zero-occurring elements do not appear in that list.
propEq :: Bool
propEq = delete 'a' (fromList "a") == empty

-- A bit silly, most properties do not really need quickcheck
testMM = do
       quickCheck propSingle
       quickCheck propSingle2
       quickCheck propDelete
       quickCheck propDelete2
       quickCheck propInsert
       quickCheck propValid
       quickCheck propUnion
       quickCheck propEq

