--------------------------------------------------------------------------------
{-| Module      :  Seq
    Copyright   :  (c) Daan Leijen 2002
    License     :  BSD-style

    Maintainer  :  daan@cs.uu.nl
    Stability   :  provisional
    Portability :  portable

  An implementation of John Hughes's efficient catenable sequence type. A lazy sequence
  @Seq a@ can be concatenated in /O(1)/ time. After
  construction, the sequence in converted in /O(n)/ time into a list.
-}
---------------------------------------------------------------------------------}
module UU.DData.Seq( -- * Type
            Seq
            -- * Operators
          , (<>)

            -- * Construction
          , empty
          , single
          , cons
          , append

            -- * Conversion
          , toList
          , fromList
          ) where


{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixr 5 <>

-- | /O(1)/. Append two sequences, see 'append'.
(<>) :: Seq a -> Seq a -> Seq a
s <> t
  = append s t

{--------------------------------------------------------------------
  Type
--------------------------------------------------------------------}
-- | Sequences of values @a@.
newtype Seq a = Seq ([a] -> [a])

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. Create an empty sequence.
empty :: Seq a
empty
  = Seq (\ts -> ts)

-- | /O(1)/. Create a sequence of one element.
single :: a -> Seq a
single x
  = Seq (\ts -> x:ts)

-- | /O(1)/. Put a value in front of a sequence.
cons :: a -> Seq a -> Seq a
cons x (Seq f)
  = Seq (\ts -> x:f ts)

-- | /O(1)/. Append two sequences.
append :: Seq a -> Seq a -> Seq a
append (Seq f) (Seq g)
  = Seq (\ts -> f (g ts))


{--------------------------------------------------------------------
  Conversion
--------------------------------------------------------------------}
-- | /O(n)/. Convert a sequence to a list.
toList :: Seq a -> [a]
toList (Seq f)
  = f []

-- | /O(n)/. Create a sequence from a list.
fromList :: [a] -> Seq a
fromList xs
  = Seq (\ts -> xs++ts)








