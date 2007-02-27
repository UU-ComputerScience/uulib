module UU.Parsing.Derived where

import UU.Parsing.Interface

infixl 2 <?>
infixl 4  <**>, <??>, <+>
infixl 2 `opt`
infixl 5 <..>


-- =======================================================================================
-- ===== CHECKING ========================================================================
-- =======================================================================================
-- | Checks if the parser accepts epsilon.
acceptsepsilon :: (IsParser p s) => p v -> Bool
acceptsepsilon p       = case getzerop p of {Nothing -> False; _ -> True}

mnz :: (IsParser p s) => p v -> t -> String -> t
mnz p v comb
   = if( acceptsepsilon p)
     then   usererror ("The combinator <" ++ comb ++ "> from <Derived.hs>is called with a parser that accepts the empty string.\n"
                    ++
                   "The library cannot handle the resulting left recursive formulation (which is ambiguous too).\n"
                 --  ++
                 --  (case getfirsts p of
                 --   ESeq []  ->  "There are no other alternatives for this parser"
                 --   d        ->  "The other alternatives of this parser may start with:\n"++ show d
                  ) --)
     else v
-- =======================================================================================
-- ===== START OF PRELUDE DEFINITIONS ========== =========================================
-- =======================================================================================

-- | Parses the specified range, see also 'pRange'.
-- 
-- Example:
-- 
-- > pDig = 'a' <..> 'z'
(<..>) :: (IsParser p s) => s -> s -> p s
a <..> b   = pRange a (Range a b)

pExcept :: (IsParser p s, Symbol s, Ord s, Eq (SymbolR s)) => (s, s, s) -> [s] -> p s
pExcept (l,r,err) elems = let ranges = filter (/= EmptyR) (Range l r `except` elems)
                          in if null ranges then pFail
                             else foldr (<|>) pFail (map (pRange err) ranges)



-- | Optionally recognize parser 'p'.
-- 
-- If 'p' can be recognized, the return value of 'p' is used. Otherwise,
-- the value 'v' is used. Note that opt is greedy, if you do not want
-- this use @... <|> pSucceed v@  instead. Furthermore, 'p' should not
-- recognise the empty string.
opt ::  (IsParser p s) => p a -> a -> p a
p `opt` v       = mnz p (p  <|> pLow v)  "opt"  
                                                
                                                

-- =======================================================================================
-- ===== Special sequential compositions =========================================
-- =======================================================================================
asList ::  (IsParser p s) =>  Expecting s -> p v -> p v
asList  exp = setfirsts (ESeq [EStr "(",  exp, EStr  " ...)*"])

asList1 :: (IsParser p s) => Expecting s -> p v -> p v
asList1 exp = setfirsts (ESeq [EStr "(",  exp, EStr  " ...)+"])

asOpt :: (IsParser p s) => Expecting s -> p v -> p v
asOpt   exp = setfirsts (ESeq [EStr "( ", exp, EStr  " ...)?"])

-- | Parses the sequence of 'pa' and 'pb', and combines them as a tuple.
(<+>) :: (IsParser p s) => p a -> p b -> p (a, b)
pa <+> pb       = (,) <$> pa <*> pb

-- | Suppose we have a parser a with two alternatives that both start
-- with recognizing a non-terminal p, then we will typically rewrite:
--
-- > a =     f <$> p <*> q 
-- >     <|> g <$> p <*> r 
--
-- into: 
--
-- > a = p <**> (f <$$> q <|> g <$$> r)
(<**>) :: (IsParser p s) => p a -> p (a -> b) -> p b
p <**> q        = (\ x f -> f x) <$> p <*> q

(<$$>) :: (IsParser p s) => (a -> b -> c) -> p b -> p (a -> c)
f <$$> p        = pSucceed (flip f) <*> p

(<??>) :: (IsParser p s) => p a -> p (a -> a) -> p a
p <??> q        = p <**> (q `opt` id)

(<?>) :: (IsParser p s) => p v -> String -> p v
p <?>  str      = setfirsts  (EStr str) p

-- | This can be used to parse 'x' surrounded by 'l' and 'r'.
-- 
-- Example:
--
-- > pParens = pPacked pOParen pCParen
pPacked :: (IsParser p s) => p a -> p b1 -> p b -> p b
pPacked l r x   =   l *>  x <*   r

-- =======================================================================================
-- ===== Iterating ps ===============================================================
-- =======================================================================================
pFoldr_ng :: (IsParser p s) => (a -> a1 -> a1, a1) -> p a -> p a1
pFoldr_ng      alg@(op,e)     p = mnz p (asList (getfirsts p) pfm) "pFoldr_ng"
                                  where pfm = (op <$> p <*> pfm)  <|> pSucceed e
pFoldr_gr :: (IsParser p s) => (a -> b -> b, b) -> p a -> p b
pFoldr_gr      alg@(op,e)     p = mnz p (asList (getfirsts p) pfm) "pFoldr_gr"
                                  where pfm = (op <$> p <*> pfm) `opt` e
pFoldr :: (IsParser p s) =>(a -> b -> b, b) -> p a -> p b
pFoldr         alg            p = pFoldr_gr alg p

pFoldr1_gr :: (IsParser p s) => (v -> b -> b, b) -> p v -> p b
pFoldr1_gr     alg@(op,e)     p = asList1 (getfirsts p) (op <$> p <*> pFoldr_gr  alg p)
pFoldr1_ng ::  (IsParser p s) => (v -> b -> b, b) -> p v -> p b
pFoldr1_ng     alg@(op,e)     p = asList1 (getfirsts p) (op <$> p <*> pFoldr_ng  alg p)
pFoldr1 :: (IsParser p s) => (v -> b -> b, b) -> p v -> p b
pFoldr1        alg            p = pFoldr1_gr alg  p

pFoldrSep_gr :: (IsParser p s) => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep_gr   alg@(op,e) sep p = mnz sepp (asList (getfirsts p)((op <$> p <*> pFoldr_gr alg sepp) `opt` e )) "pFoldrSep_gr (both args)"
                                  where sepp = sep *> p
pFoldrSep_ng :: (IsParser p s) => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep_ng   alg@(op,e) sep p = mnz sepp (asList (getfirsts p)((op <$> p <*> pFoldr_ng alg sepp)  <|>  pSucceed e)) "pFoldrSep_ng (both args)"
                                  where sepp = sep *> p
pFoldrSep ::  (IsParser p s) => (v -> b -> b, b) -> p a -> p v -> p b
pFoldrSep      alg        sep p = pFoldrSep_gr alg sep p

pFoldr1Sep_gr :: (IsParser p s) => (a -> b -> b, b) -> p a1 -> p a -> p b
pFoldr1Sep_gr  alg@(op,e) sep p = if acceptsepsilon sep then mnz p pfm "pFoldr1Sep_gr (both arguments)" else pfm
                                  where pfm = op <$> p <*> pFoldr_gr alg (sep *> p)
pFoldr1Sep_ng :: (IsParser p s) => (a -> b -> b, b) -> p a1 -> p a -> p b
pFoldr1Sep_ng  alg@(op,e) sep p = if acceptsepsilon sep  then mnz p pfm "pFoldr1Sep_ng (both arguments)" else pfm
                                  where pfm = op <$> p <*> pFoldr_ng alg (sep *> p)
pFoldr1Sep :: (IsParser p s) => (a -> b -> b, b) -> p a1 -> p a -> p b
pFoldr1Sep     alg        sep p = pFoldr1Sep_gr alg sep p

list_alg :: (a -> [a] -> [a], [a1])
list_alg = ((:), [])

pList_gr ::  (IsParser p s) => p a -> p [a]
pList_gr        p = pFoldr_gr     list_alg   p
pList_ng :: (IsParser p s) =>  p a -> p [a]
pList_ng        p = pFoldr_ng     list_alg   p
pList ::  (IsParser p s) => p a -> p [a]
pList           p = pList_gr p

pList1_gr ::  (IsParser p s) => p a -> p [a]
pList1_gr       p = pFoldr1_gr    list_alg   p
pList1_ng :: (IsParser p s) => p a -> p [a]
pList1_ng       p = pFoldr1_ng    list_alg   p
pList1 :: (IsParser p s) =>  p a -> p [a]
pList1          p = pList1_gr                p

pListSep_gr :: (IsParser p s) => p a1 -> p a -> p [a]
pListSep_gr   s p = pFoldrSep_gr  list_alg s p
pListSep_ng :: (IsParser p s) => p a1 -> p a -> p [a]
pListSep_ng   s p = pFoldrSep_ng  list_alg s p
pListSep :: (IsParser p s) => p a -> p a1 -> p [a1]
pListSep      s p = pListSep_gr            s p

pList1Sep_gr :: (IsParser p s) => p a1 -> p a -> p [a]
pList1Sep_gr  s p = pFoldr1Sep_gr list_alg s p
pList1Sep_ng :: (IsParser p s) =>  p a1 -> p a -> p [a]
pList1Sep_ng  s p = pFoldr1Sep_ng list_alg s p
pList1Sep :: (IsParser p s) =>p a -> p a1 -> p [a1]
pList1Sep     s p = pList1Sep_gr          s p

pChainr_gr :: (IsParser p s) => p (c -> c -> c) -> p c -> p c
pChainr_gr op x    =  if acceptsepsilon op then mnz x r "pChainr_gr (both arguments)" else r
                   where r = x <??> (flip <$> op <*> r)
pChainr_ng :: (IsParser p s) => p (a -> a -> a) -> p a -> p a
pChainr_ng op x    =  if acceptsepsilon op then mnz x r "pChainr_ng (both arguments)" else r
                   where r = x <**> ((flip <$> op <*> r)  <|> pSucceed id)
pChainr :: (IsParser p s) => p (c -> c -> c) -> p c -> p c
pChainr    op x    = pChainr_gr op x

pChainl_gr :: (IsParser p s) => p (c -> c -> c) -> p c -> p c
pChainl_gr op x    =  if acceptsepsilon op then mnz x r "pChainl_gr (both arguments)" else r
                      where
                       r      = (f <$> x <*> pList_gr (flip <$> op <*> x) )
                       f x [] = x
                       f x (func:rest) = f (func x) rest

pChainl_ng :: (IsParser p s) => p (c -> c -> c) -> p c -> p c
pChainl_ng op x    =  if acceptsepsilon op then mnz x r "pChainl_ng (both arguments)" else r
                   where
                    r      = (f <$> x <*> pList_ng (flip <$> op <*> x) )
                    f x [] = x
                    f x (func:rest) = f (func x) rest
pChainl :: (IsParser p s) => p (c -> c -> c) -> p c -> p c
pChainl    op x    = pChainl_gr op x

-- | Parses using any of the parsers in the list 'l'.
--
-- Warning: 'l' may not be an empty list.
pAny :: (IsParser p s) =>(a -> p a1) -> [a] -> p a1
pAny  f l = if null l then usererror "pAny: argument may not be empty list" else foldr1 (<|>) (map f l)

-- | Parses any of the symbols in 'l'.
pAnySym :: (IsParser p s) =>[s] -> p s
pAnySym l = pAny pSym l -- used to be called pAnySym

pToks :: (IsParser p s) => [s] -> p [s]
pToks []     = pSucceed []
pToks (a:as) = (:) <$> pSym a <*> pToks as

pLocate :: (IsParser p s) => [[s]] -> p [s]
pLocate list = pAny pToks list
