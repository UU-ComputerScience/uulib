module UU.Parsing.Machine where
import UU.Util.BinaryTrees 
import UU.Parsing.MachineInterface

pDynE v = anaDynE v
pDynL v = anaDynL v

-- ==========================================================================================
-- ===== BASIC PARSER TYPE  =================================================================
-- =======================================================================================

newtype RealParser    state        s p a = P(forall r' r'' . (a -> r'' -> r') ->
                                                        (state -> Steps r'' s p) ->  state -> Steps r'           s p)

newtype RealRecogn    state        s p   = R(forall r . (state -> Steps r   s p) ->  state -> Steps r            s p)

newtype RealAccept    state result s p a = A(forall r . (state -> Steps r   s p) ->  state -> Steps (result a r) s p)

newtype ParsRec       state result s p a = PR  ( RealParser  state        s p a
                                               , RealRecogn  state        s p
                                               , RealAccept  state result s p a
                                               )
                                             
mkPR (P p, R r) = PR (P p, R r, A (p acceptR))

{-# INLINE unP #-}
{-# INLINE unR #-}
unP  (P  p) = p
unR  (R  p) = p

parseRecbasic :: (inp -> Steps (out c d) sym pos) 
              -> ParsRec inp out sym pos a 
              -> inp 
              -> Steps (out a (out c d)) sym pos
parseRecbasic eof (PR ( P rp, rr, A ra))  inp = (ra eof inp)

parsebasic :: (inp -> Steps (out c d) sym pos) 
           -> AnaParser inp out sym pos a 
           -> inp 
           -> Steps (out a (out c d)) sym pos
parsebasic eof (pp) inp
 = parseRecbasic eof (pars pp) inp 

-- =======================================================================================
-- ===== CORE PARSERS ====================================================================
-- ======================================================================================= 
libAccept :: (OutputState a, InputState b s p) => ParsRec b a s p s
libAccept            = mkPR (P (\ acc k state ->
                                case splitState state of
                                ({-#L-} s, ss {-L#-})  -> OkVal (acc s) (k ss))
                            ,R (\ k state ->
                                case splitState state of
                                ({-#L-} s, ss {-L#-})  ->   Ok (k ss))
                            )
libInsert  c sym  firsts =mkPR( P (\acc k state ->  let msg = Msg  firsts 
                                                                     (getPosition state)
                                                                     (Insert sym)            
                                                    in StRepair c msg (val (acc sym) (k (reportError msg state))))
                              , R (\    k state ->  let msg = Msg  firsts 
                                                                     (getPosition state)
                                                                     (Insert sym)       
                                                    in StRepair c msg (k (reportError msg state)))
                              )
{-# INLINE libSeq  #-}
{-# INLINE libSeqL #-}
{-# INLINE libSeqR #-}
{-# INLINE libDollar #-}
{-# INLINE libDollarL #-}
{-# INLINE libDollarR #-}
{-# INLINE libSucceed #-}

libSucceed v                                 =mkPR( P (\ acc -> let accv = val (acc v) in {-# SCC "machine" #-} \ k state -> accv (k state))
                                                  , R id
                                                  )
libSeq  (PR (P pp, R pr, _)) ~(PR (P qp, R qr, A qa)) =mkPR ( P (\ acc -> let p = pp (nextR acc) in {-# SCC "machine" #-} \k state -> p (qa k) state)
                                                            , R ( pr.qr)
                                                            )
libDollar f                   (PR (P qp, R qr, _   )) = mkPR ( P (\ acc -> {-# SCC "machine" #-} qp (acc.f))
                                                             , R qr
                                                             )
libDollarL f                  (PR (P qp, R qr, _   )) = mkPR ( P (\ acc -> let accf = val (acc f) in {-# SCC "machine" #-} \ k state -> qr (\ inp -> accf ( k inp)) state)
                                                             , R qr
                                                             )
libDollarR f                   (PR (P qp, R qr, _ )) = mkPR (P  qp, R qr)

libSeqL (PR (P pp, R pr, _ )) ~(PR (P qp, R qr , _ )) = mkPR ( P (\acc -> let p = pp acc in {-# SCC "machine" #-}\k state -> p (qr k) state)
                                                             , R (pr.qr)
                                                             )
libSeqR (PR (P pp, R pr, _ )) ~(PR (P qp, R qr, _ )) = mkPR  ( P (\acc -> let q = qp acc in {-# SCC "machine" #-}\k state -> pr (q k) state)
                                                             , R (pr.qr)
                                                             )
libOr   (PR (P pp, R pr,_ ))   (PR (P qp, R qr, _ )) = mkPR  ( P (\ acc -> let p = pp acc
                                                                               q = qp acc
                                                                           in {-# SCC "machine" #-} \ k state   -> p  k state `libBest` q  k state)
                                                             , R (\                                   k state   -> pr k state `libBest` qr k state)
                                                             )
libFail :: OutputState a => ParsRec b a c p d
libFail                                      = mkPR ( P (\ _ _  _  -> (usererror  "calling an always failing parser"    ))
                                                    , R (\   _  _  -> (usererror  "calling an always failing recogniser"))
                                                    )
      


starting :: Steps a s p -> Expecting s
starting (StRepair _ m _ ) = getStart m
starting (Best l _  _ )    = starting l
starting _                 = systemerror "UU.Parsing.Machine" "starting"

{-# INLINE hasSuccess #-}
hasSuccess :: Steps a s p -> Bool
hasSuccess (StRepair _ _ _ ) = False
hasSuccess (Best     _ _ _ ) = False 
hasSuccess _                 = True

getStart (Msg st _ _) = st

addToMessage (Msg exp pos act) more = Msg (more `eor` exp) pos act


addexpecting more  (StRepair    cost   msg   rest) = StRepair cost (addToMessage msg more) rest
addexpecting more  (Best     l    sel           r) = Best (addexpecting more   l)
                                                          (addexpecting more sel) 
                                                          (addexpecting more   r)
addexpecting more  (OkVal v rest                 ) =  systemerror "UU_Parsing" ("addexpecting: OkVal")
addexpecting more  (Ok   _                       ) =  systemerror "UU_Parsing" ("addexpecting: Ok")
addexpecting more  (Cost _ _                     ) =  systemerror "UU_Parsing" ("addexpecting: Cost")
addexpecting more  _                               =  systemerror "UU_Parsing" ("addexpecting: other")


eor :: Ord a => Expecting a -> Expecting a -> Expecting a
eor p  q  = EOr (merge (tolist p) (tolist q))
            where merge x@(l:ll) y@(r:rr) = case compare l r of
                                            LT -> l:( ll `merge`  y)
                                            GT -> r:( x  `merge` rr)
                                            EQ -> l:( ll `merge` rr)
                  merge l [] = l
                  merge [] r = r
                  tolist (EOr l) = l
                  tolist x       = [x]

-- =======================================================================================
-- ===== SELECTING THE BEST RESULT  ======================================================
-- =======================================================================================
-- INV: the first argument should be the shorter insertion
libBest :: Ord s => Steps b s p -> Steps b s p -> Steps b s p
libBest ls rs = libBest' ls rs id id

libBest' :: Ord s => Steps b s p -> Steps c s p -> (b -> d) -> (c -> d) -> Steps d s p
libBest' (OkVal v ls) (OkVal w rs) lf rf = Ok (libBest' ls rs (lf.v) (rf.w))
libBest' (OkVal v ls) (Ok      rs) lf rf = Ok (libBest' ls rs (lf.v)  rf   )
libBest' (Ok      ls) (OkVal w rs) lf rf = Ok (libBest' ls rs  lf    (rf.w))
libBest' (Ok      ls) (Ok      rs) lf rf = Ok (libBest' ls rs  lf     rf   )
libBest' (OkVal v ls) _            lf rf = OkVal (lf.v) ls 
libBest' _            (OkVal w rs) lf rf = OkVal (rf.w) rs 
libBest' (Ok      ls) _            lf rf = OkVal lf ls           
libBest' _            (Ok      rs) lf rf = OkVal rf rs   
libBest' l@(Cost i ls ) r@(Cost j rs ) lf rf
 | i =={-#L-} j = Cost i (libBest' ls rs lf rf)
 | i <{-#L-} j  = Cost i (val lf ls)
 | i >{-#L-} j  = Cost j (val rf rs)
libBest' l@(Cost i ls)     _                 lf rf = Cost i (val lf ls)
libBest' _                 r@(Cost j rs)     lf rf = Cost j (val rf rs)
libBest' l@(NoMoreSteps v) _                 lf rf = NoMoreSteps (lf v)
libBest' _                 r@(NoMoreSteps w) lf rf = NoMoreSteps (rf w)
libBest' l                 r                 lf rf = libCorrect l r lf rf

lib_correct :: Ord s => (b -> c -> Steps d s p) -> (b -> c -> Steps d s p) -> b -> c -> Steps d s p
lib_correct p q = \k inp -> libCorrect (p k inp) ( q k inp) id id

libCorrect :: Ord s => Steps a s p -> Steps c s p -> (a -> d) -> (c -> d) -> Steps d s p
libCorrect ls rs lf rf
 =  let (ToBeat _ choice) = traverse 
                            (traverse (ToBeat 999{-#L-} (val lf newleft)) 
                                  (val lf, newleft,  0{-#L-}) 4{-#L-})
                                  (val rf, newright, 0{-#L-}) 4{-#L-} 
        newleft    = addexpecting (starting rs) ls
        newright   = addexpecting (starting ls) rs
    in Best (val lf newleft)
            choice
            (val rf newright)

data ToBeat a = ToBeat Int{-#L-} a

traverse :: ToBeat (Steps a s p) -> (Steps v s p -> Steps a s p, Steps v s p, Int{-L#-}) -> Int{-L#-} -> ToBeat (Steps a s p)
traverse b@(ToBeat bv br) (f, s, v)              0{-#L-} = {- trace ("comparing " ++ show bv ++ " with " ++ show v ++ "\n") $ -}
                                                           if bv <={-#L-} v 
                                                           then b 
                                                           else ToBeat v (f s)
traverse b@(ToBeat bv br) (f, Ok      l, v)            n = {- trace ("adding" ++ show n ++ "\n") $-} traverse b (f.Ok     , l, v - n + 4) (n -{-#L-} 1{-#L-})
traverse b@(ToBeat bv br) (f, OkVal w l, v)            n = {- trace ("adding" ++ show n ++ "\n") $-} traverse b (f.OkVal w, l, v - n + 4) (n -{-#L-} 1{-#L-})
traverse b@(ToBeat bv br) (f, Cost i  l, v)            n = if i +{-#L-} v >={-#L-} bv 
                                                           then b 
                                                           else traverse b (f.Cost i, l, i +{-#L-} v) n
traverse b@(ToBeat bv br) (f, Best l _ r, v)           n = traverse (traverse b (f, l, v) n) (f, r, v) n
traverse b@(ToBeat bv br) (f, StRepair i msgs r, v)    n = if i +{-#L-} v >={-#L-} bv then b 
                                                           else traverse b (f.StRepair i msgs, r, i +{-#L-} v) (n -{-#L-} 1{-#L-})
traverse b@(ToBeat bv br) (f, t@(NoMoreSteps _), v)    n = if bv <={-#L-} v then b else ToBeat v (f t)
-- =======================================================================================
-- ===== DESCRIPTORS =====================================================================
-- =======================================================================================
data AnaParser  state result s p a
 = AnaParser { pars     :: ParsRec state result s p a
             , leng     :: Nat
             , zerop    :: Maybe (Bool, Either a (ParsRec state result s p a))
             , onep     :: OneDescr state  result s p a
             } -- deriving Show
data OneDescr  state result s p a
 = OneDescr  { firsts   :: Expecting s
             , table    :: [(SymbolR s, TableEntry state result s p a)]
             } -- deriving Show
             
data TableEntry state result s p a = TableEntry (ParsRec  state result s p a) (Expecting s -> ParsRec state result s p a)
-- =======================================================================================
-- ===== ANALYSING COMBINATORS ===========================================================
-- =======================================================================================
anaFail :: OutputState a => AnaParser b a c p d
anaFail = AnaParser { pars    = libFail
                    , leng    = Infinite
                    , zerop   = Nothing
                    , onep    = noOneParser
                    }
noOneParser = OneDescr (EOr []) []

pEmpty p zp = AnaParser { pars    = p
                        , leng    = Zero
                        , zerop   = Just zp
                        , onep    = noOneParser
                        }

anaSucceed  v = pEmpty (libSucceed v) (False, Left v)
anaLow      v = pEmpty (libSucceed v) (True,  Left v)
anaDynE     p = pEmpty p              (False, Right p)
anaDynL     p = pEmpty p              (True , Right p)
--anaDynN  fi len range p = mkParser  Nothing (OneDescr len fi [(range, p)]) 

anaOr ld@(AnaParser _ ll zl ol)  rd@(AnaParser _ lr zr or)
 = mkParser newlength newZeroDescr newOneDescr 
   where (newlength, maybeswap) = ll `nat_min` lr
         newZeroDescr  = case zl of {Nothing -> zr
                                    ;_       -> case zr of {Nothing -> zl
                                                           ;_       -> usererror ("Two empty alternatives")
                                    }                      }
         newOneDescr   =  maybeswap orOneOneDescr ol or False

{-# INLINE anaSeq #-}

anaSeq libdollar libseq comb (AnaParser  pl ll zl ol)  ~rd@(AnaParser pr lr zr or)
 = case zl of
   Just (b, zp ) -> let newZeroDescr = seqZeroZero zl zr   libdollar libseq comb
                        newOneDescr = let newOneOne  = mapOnePars (   `libseq` pr) ol
                                          newZeroOne = case zp of
                                                       Left  f -> mapOnePars (f `libdollar`   )  or
                                                       Right p -> mapOnePars (p `libseq`      )  or
                                      in orOneOneDescr newZeroOne newOneOne  b -- left one is shortest
                    in mkParser lr newZeroDescr newOneDescr
   _            ->  AnaParser  (pl `libseq` pr) (ll `nat_add` lr) Nothing  (mapOnePars (`libseq` pr) ol)

seqZeroZero Nothing             _                    _          _      _   = Nothing
seqZeroZero _                   Nothing              _          _      _   = Nothing 
seqZeroZero (Just (llow, left)) (Just (rlow, right))  libdollar libseq comb
    = Just      ( llow || rlow
               , case left of
                 Left  lv  -> case right of
                              Left  rv -> Left (comb lv rv)
                              Right rp -> Right (lv `libdollar` rp)
                 Right lp  -> case right of
                              Left  rv  -> Right (lp `libseq` libSucceed rv)
                              Right rp  -> Right (lp `libseq` rp)
               )

orOneOneDescr ~(OneDescr fl tl) ~(OneDescr fr tr)  b
                  = let keystr          = map fst tr
                        lefttab         = if b then [r | r@(k,_) <- tl, not (k `elem` keystr)] else tl
                    in OneDescr (fl `eor` fr) (lefttab ++ tr)

anaCostRange _        _     EmptyR = anaFail
anaCostRange ins_cost ins_sym range
  = mkParser (Succ Zero) Nothing ( OneDescr  (ESym range) [(range, TableEntry  libAccept 
                                                                              (libInsert ins_cost ins_sym)
                                                         )]) 

--anaCostSym   i ins sym = pCostRange i ins (Range sym sym)

anaGetFirsts (AnaParser  p l z od) = firsts od

anaSetFirsts newexp (AnaParser  _ l zd od)
 = mkParser l zd (od{firsts = newexp })

-- =======================================================================================
-- ===== UTILITIES ========================================================================
-- =======================================================================================
mapOnePars fp    ~(OneDescr   fi t) = OneDescr  fi [ (k, TableEntry (fp p) (fp.corr))
                                                   | (k, TableEntry     p      corr ) <- t
                                                   ]

-- =======================================================================================
-- ===== MKPARSER ========================================================================
-- =======================================================================================
mkParser length zd ~descr@(OneDescr firsts tab) -- pattern matching should be lazy for lazy computation of length for empty parsers
 = let parstab    = foldr1 mergeTables  [[(k, p)]| (k, TableEntry p _) <- tab]
       mkactualparser getp 
         = let ptab = [(k, (getp pr) )| (k, pr) <- parstab]
               find       = case  ptab of
                            [(s1,  p1)]                      ->  ({-# SCC "Locating" #-}\ s -> if r1 s then Just p1 else Nothing )                                           
                                                                where  r1 = symInRange s1
                            [(s1,  p1), (s2, p2)]            -> ({-# SCC "Locating" #-} \ s -> if r1 s then Just p1 else 
                                                                                               if r2 s then Just p2 else Nothing) 
                                                                where  r1 = symInRange s1
                                                                       r2 = symInRange s2
                            [(s1,  p1), (s2, p2), (s3, p3)]  -> ({-# SCC "Locating" #-}\ s -> if r1 s then Just p1 else 
                                                                                              if r2 s then Just p2 else 
                                                                                              if r3 s then Just p3 else Nothing)
                                                                where  r1 = symInRange s1
                                                                       r2 = symInRange s2
                                                                       r3 = symInRange s3                                           
                            _           -> lookupSym (tab2tree ptab)
               zerop      = getp (case zd of
                                 Nothing           -> libFail
                                 Just (_, Left v)  -> libSucceed v
                                 Just (_, Right p) -> p
                                 )
-- SDS/AD 20050603: only the shortest alternative in possible corrections now is taken
--               insertsyms = foldr1 lib_correct [   getp (pr firsts)| (_ , TableEntry _ pr) <- tab    ]
               insertsyms = head [   getp (pr firsts)| (_ , TableEntry _ pr) <- tab    ]
               correct k inp
                 = case splitState inp of
                       ({-#L-} s, ss {-L#-}) -> let { msg = Msg firsts (getPosition inp) (Delete s)
                                                    ; newinp = reportError msg ss
                                                    }
                                                in libCorrect (StRepair (deleteCost s) msg (result k newinp))
                                                              (insertsyms k inp) id id
               result = if null tab then zerop
                        else case zd of
                        Nothing        ->({-# SCC "mkParser1" #-}\k inp -> 
                                         case splitStateE inp of
                                                    Left' s ss -> case find s of 
                                                                  Just p  ->  p k inp
                                                                  Nothing -> correct k inp
                                                    Right' ss  -> insertsyms   k ss)
                        Just (True, _) ->({-# SCC "mkParser2" #-}\k inp -> 
                                         case splitStateE inp of
                                                    Left' s ss -> case find s of 
                                                                  Just p  -> p k inp 
                                                                  Nothing -> let r = zerop k inp 
                                                                             in if hasSuccess r then r else libCorrect r (correct k inp) id id
                                                    Right'  ss -> zerop k ss)
                        Just (False, _) ->({-# SCC "mkParser3" #-}\k inp -> 
                                          case splitStateE inp of
                                                    Left' s ss -> case find s of 
                                                                  Just p  -> p k inp `libBest` zerop k inp
                                                                  Nothing -> let r = zerop k inp 
                                                                             in if hasSuccess r then r else libCorrect r (correct k inp) id id
                                                    Right' ss  -> zerop k ss)
           in result
       res    = mkPR (P ( \ acc ->  mkactualparser (\ (PR (P p, _  , _)) -> p acc))
                     ,R (           mkactualparser (\ (PR (_  , R p, _)) -> p    ))
                     )            
   in AnaParser res length zd descr
   
-- =======================================================================================
-- ===== MINIMAL LENGTHS (lazily formulated) =============================================
-- =======================================================================================
data Nat = Zero
         | Succ Nat
         | Infinite
         deriving (Eq, Show)

nat_le Zero      _        = True
nat_le _         Zero     = False
nat_le Infinite  _        = False
nat_le _         Infinite = True
nat_le (Succ l) (Succ r) = nat_le l r

nat_min Infinite   r          = (r, flip) 
nat_min l          Infinite   = (l, id)
nat_min Zero       _          = (Zero, id)
nat_min _          Zero       = (Zero, flip) 
nat_min (Succ ll)  (Succ rr)  = let (v, fl) = ll `nat_min` rr in (Succ v, fl)

nat_add Infinite  _ = Infinite
nat_add Zero      r = r
nat_add (Succ l)  r = Succ (nat_add l r)
-- =======================================================================================
-- ===== CHOICE STRUCTURES   =============================================================
-- =======================================================================================
mergeTables l []  = l
mergeTables [] r  = r
mergeTables lss@(l@(le@(Range a b),ct ):ls) rss@(r@(re@(Range c d),ct'):rs)
 = let ct'' =  ct `libOr` ct'
   in  if      c<a then   mergeTables rss lss     -- swap
       else if b<c then l:mergeTables ls  rss     -- disjoint case
       else if a<c then (Range a (symBefore c),ct) :mergeTables ((Range c b,ct):ls)             rss
       else if b<d then (Range a b,ct'')           :mergeTables ((Range (symAfter b) d,ct'):rs) ls
       else if b>d then mergeTables rss lss
                   else (le,ct'') : mergeTables ls rs-- equals

-- =======================================================================================
-- ===== WRAPPING AND MAPPING ==============================================================
-- =======================================================================================

libMap :: OutputState result =>
             (forall r r'' . (b -> r -> r'') -> state -> Steps (a, r) s p -> ( state, Steps  r'' s p)) 
          -> (forall r     .                    state -> Steps (   r) s p -> ( state, Steps  r   s p))
          -> ParsRec state result s p a -> ParsRec state result s p b
libMap f f' (PR (P p, R r, _))  = mkPR ( P(\acc -> let pp   = p (,)
                                                       facc = f acc 
                                                   in \ k instate  -> let inresult = pp k outstate
                                                                          (outstate, outresult) = facc instate inresult
                                                                      in outresult
                                          )
                                       , R(\ k instate  -> let inresult = r k outstate
                                                               (outstate, outresult) = f' instate inresult
                                                           in outresult)
                                       )

pMap ::    OutputState result =>
             (forall r r'' . (b -> r -> r'') -> state -> Steps (a, r) s p -> ( state, Steps r'' s p)) 
          -> (forall r     .                    state -> Steps (   r) s p -> ( state, Steps r   s p))
          ->  AnaParser state result s p a -> AnaParser state result s p b

pMap f f'  (AnaParser p l z o) = AnaParser (libMap f f' p)
                                           l
                                          (case z of
                                           Nothing     -> Nothing
                                           Just (b, v) -> Just (b, case v of
                                                                   Left w   -> Right (libMap f f' (libSucceed w))
                                                                   Right pp -> Right (libMap f f' pp)))
                                          (mapOnePars (libMap f f')  o)


libWrap :: OutputState result =>
           (forall r r'' .  (b -> r -> r'') 
                                    -> state 
                                    -> Steps (a, r) s p
                                    -> (state -> Steps r s p) 
                                    -> (state, Steps r'' s p, state -> Steps r s p))
           -> (forall r        .   state 
                                -> Steps r s p 
                                -> (state -> Steps r s p) 
                                -> (state, Steps r s p, state -> Steps r s p)) 
           -> ParsRec state result s p a -> ParsRec state result s p b
libWrap f f' (PR (P p, R r, _)) = mkPR ( P(\ acc -> let pp = p (,)
                                                        facc = f acc
                                                    in \ k instate  -> let (stl, ar, str2rr) = facc instate rl k
                                                                           rl                = pp str2rr stl
                                                                       in  ar
                                     )
                                  , R(\ k instate  -> let (stl, ar, str2rr) = f' instate rl k
                                                          rl                = r str2rr stl
                                                      in  ar)
                                  )

pWrap ::    OutputState result 
           => (forall r  r'' .   (b -> r -> r'') 
                                    -> state
                                    -> Steps (a, r) s p 
                                    -> (state -> Steps r s p) 
                                    -> (state, Steps r'' s p, state -> Steps r s p))
           -> (forall r        .   state  
                                -> Steps r s p 
                                -> (state -> Steps r s p) 
                                -> (state, Steps r s p, state -> Steps r s p)) 
           -> AnaParser state result s p a -> AnaParser state result s p b

pWrap f f'  (AnaParser p l z o) = AnaParser (libWrap f f' p)
                                          l
                                          (case z of
                                           Nothing     -> Nothing
                                           Just (b, v) -> Just (b, case v of
                                                                   Left w   -> Right (libWrap f f' (libSucceed w))
                                                                   Right pp -> Right (libWrap f f' pp)))
                                          (mapOnePars (libWrap f f')  o)



-- =======================================================================================
-- ===== BINARY SEARCH TREES =============================================================
-- =======================================================================================

lookupSym :: Ord a => BinSearchTree (SymbolR a, b) -> a -> Maybe b
lookupSym = btFind symRS 
