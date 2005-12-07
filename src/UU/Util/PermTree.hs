module UU.Util.PermTree where 

import Monad(ap,liftM2)

------------------------------------------------------------------------------------
-- data type for permutation trees
------------------------------------------------------------------------------------

data Perms p a  = Choice (Maybe a) [Branch p a]
data Branch p a = forall x . Br  (p x) (Perms p  (x->a))   

------------------------------------------------------------------------------------
-- definition of fmap on permutation trees
------------------------------------------------------------------------------------

instance Functor (Perms p) where
  fmap f (Choice e bs) = Choice (fmap f e) (map (fmap f) bs) 

instance Functor (Branch p) where
  fmap f (Br p ps) = Br p (fmap (f.) ps)

------------------------------------------------------------------------------------
-- add single parser to permutation tree
------------------------------------------------------------------------------------

{-
ap :: Maybe (a->b)-> Maybe a -> Maybe b
ap (Just f) (Just x) = Just (f x)
ap _        _        = Nothing
-}

add :: Maybe a -> p a -> Perms p (a->b) -> Perms p b
add da pa tab@(Choice dab bsab) = let empty = dab `ap` da
                                      insert (Br px txab) = Br px (add da pa (fmap flip txab))
                                  in Choice empty (Br pa tab:map insert bsab)   
                            
                      
------------------------------------------------------------------------------------
-- permutation construction combinators
------------------------------------------------------------------------------------
empty         :: a -> Perms p a
empty x       =  Choice (Just x) [] 


(<$$>)        :: (a->b) -> p a -> Perms p b
f <$$> p      =  empty f <||> p     

(<$?>)        :: (a->b) -> (a, p a) -> Perms p b
f <$?> (e,p)  =  empty f <|?> (e,p) 

(<||>)        :: Perms p (a->b) -> p a -> Perms p b
ps <||> p     =  add Nothing p ps

(<|?>)        :: Perms p (a->b) -> (a, p a) -> Perms p b
ps <|?> (e,p) =  add (Just e) p ps 


