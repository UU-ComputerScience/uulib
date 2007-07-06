module UU.Parsing.Perms(Perms(), pPerms, pPermsSep, succeedPerms, (~*~), (~$~)) where

import UU.Parsing
import Maybe

-- =======================================================================================
-- ===== PERMUTATIONS ================================================================
-- =======================================================================================

newtype Perms p a = Perms (Maybe (p a), [Br p a])
data Br p a = forall b. Br (Perms p (b -> a)) (p b)

instance IsParser p s => Functor (Perms p) where
  fmap f (Perms (mb, bs)) = Perms (fmap (f<$>) mb, map (fmap f) bs)

instance IsParser p s => Functor (Br p) where
  fmap f (Br perm p) = Br (fmap (f.) perm) p 

(~*~) :: IsParser p s => Perms p (a -> b) -> p a -> Perms p b
perms ~*~ p = perms `add` (getzerop p, getonep p)

(~$~) :: IsParser p s => (a -> b) -> p a -> Perms p b
f     ~$~ p = succeedPerms f ~*~ p

succeedPerms :: IsParser p s => a -> Perms p a
succeedPerms x = Perms (Just (pLow x), []) 

add :: IsParser p s => Perms p (a -> b) -> (Maybe (p a),Maybe (p a)) -> Perms p b
add b2a@(Perms (eb2a, nb2a)) bp@(eb, nb)
 =  let changing :: IsParser p s => (a -> b) -> Perms p a -> Perms p b
        f `changing` Perms (ep, np) = Perms (fmap (f <$>) ep, [Br ((f.) `changing` pp) p | Br pp p <- np])
    in Perms
      ( do { f <- eb2a
           ; x <- eb
           ; return (f <*>  x)
           }
      ,  (case nb of
          Nothing     -> id
          Just pb     -> (Br b2a  pb:)
        )[ Br ((flip `changing` c) `add`  bp) d |  Br c d <- nb2a]
      )

pPerms :: IsParser p s => Perms p a -> p a 
pPerms (Perms (empty,nonempty))
 = foldl (<|>) (fromMaybe pFail empty) [ (flip ($)) <$> p <*> pPerms pp
                                       | Br pp  p <- nonempty
                                       ]

pPermsSep :: IsParser p s => p x -> Perms p a -> p a
pPermsSep (sep :: p z) perm = p2p (pSucceed ()) perm
 where  p2p :: IsParser p s => p x -> Perms p a -> p a
        p2p fsep (Perms (mbempty, nonempties)) = 
                let empty          = fromMaybe  pFail mbempty
                    pars (Br t p)  = flip ($) <$ fsep <*> p <*> p2p sep t
                in foldr (<|>) empty (map pars nonempties)              
        p2p_sep =  p2p sep                   
