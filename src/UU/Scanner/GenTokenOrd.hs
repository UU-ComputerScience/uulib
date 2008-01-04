module UU.Scanner.GenTokenOrd() where

import UU.Scanner.GenToken(GenToken(..))

instance (Eq key, Eq tp) => Eq (GenToken key tp val) where
   Reserved x    _ == Reserved y    _ = x == y
   ValToken tx _ _ == ValToken ty _ _ = tx == ty
   _               == _               = False
   
instance (Ord key, Ord tp) => Ord (GenToken key tp val) where
  compare (Reserved x    _) (Reserved y    _) = compare x y
  compare (Reserved _    _) _                 = LT
  compare (ValToken tx _ _) (ValToken ty _ _) = compare tx ty
  compare _              _                    = GT

