module UU.Parsing.Merge((<||>), pMerged, list_of) where

import UU.Parsing

-- ==== merging
-- e.g. chars_digs = cat3 `pMerged` (list_of pDig <||> list_of pL <||> list_of pU)
--      parsing "12abCD1aV" now returns "121abaCDV", so the sequence of
-- recognised elements is stored in three lists, which are then passed to cat3

(<||>) :: IsParser p s => (c,p (d -> d),e -> f -> g) -> (h,p (i -> i),g -> j -> k) -> ((c,h),p ((d,i) -> (d,i)),e -> (f,j) -> k)
(pe, pp, punp) <||> (qe, qp, qunp)
 =( (pe, qe)
  , (\f (pv, qv) -> (f pv, qv)) <$> pp
              <|>
    (\f (pv, qv) -> (pv, f qv)) <$> qp
  , \f (x, y) -> qunp (punp f x) y
  )

pMerged :: IsParser p s => c -> (d,p (d -> d),c -> d -> e) -> p e
sem `pMerged` (units, alts, unp)
 = let pres = alts <*> pres `opt` units
   in unp sem <$> pres

list_of :: IsParser p s => p c -> ([d],p ([c] -> [c]),e -> e)
list_of p = ([], (:) <$> p, id)
