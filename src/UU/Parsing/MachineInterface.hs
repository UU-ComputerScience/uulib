

module UU.Parsing.MachineInterface where
import GHC.Prim

-- | The 'InputState' class contains the interface that the AnaParser
-- parsers expect for the input. A minimal complete instance definition
-- consists of 'splitStateE', 'splitState' and 'getPosition'.
class InputState state s pos | state -> s, state -> pos where
 -- | Splits the state in a strict variant of 'Either', with 'Left'' if a symbol
 --   can be split off and 'Right'' if none can
 splitStateE :: state             -> Either' state s
 -- | Splits the state in the first symbol and the remaining state
 splitState  :: state             -> (# s, state #)
 -- | Gets the current position in the input
 getPosition :: state             -> pos
 -- | Reports an error
 reportError :: Message s pos     -> state -> state
 reportError _ = id
 -- | Modify the state as the result of inserting a symbol 's' in the input.
 -- The symbol that has already been considered as having been inserted 
 -- is passed. It should normally not be added to the state.
 insertSymbol :: s                -> state -> state
 insertSymbol _ = id
 -- | Modify the state as the result of deleting a symbol 's' from the input.
 -- The symbol that has already been deleted from the input state is passed.
 -- It should normally not be deleted from the state.
 deleteSymbol :: s                -> state -> state
 deleteSymbol _ = id
 {-# INLINE splitStateE #-}
 {-# INLINE splitState  #-}
 {-# INLINE insertSymbol  #-}
 {-# INLINE deleteSymbol  #-}

class OutputState r  where
  acceptR      ::                     v                   -> rest        -> r v rest
  nextR        ::  (a -> rest  -> rest') -> (b -> a)      -> (r b rest)  -> rest'
  {-# INLINE acceptR #-}
  {-# INLINE nextR   #-}

class Symbol s where
 deleteCost :: s -> Int#
 symBefore  :: s -> s
 symAfter   :: s -> s
 deleteCost b = 5#
 symBefore  = error "You should have made your token type an instance of the Class Symbol. eg by defining symBefore = pred"
 symAfter   = error "You should have made your token type an instance of the Class Symbol. eg by defining symAfter  = succ"

data Either' state s = Left' !s (state )
                     | Right' (state )

-- =======================================================================================
-- ===== STEPS ===========================================================================
-- =======================================================================================
data Steps val s p 
             = forall a . OkVal           (a -> val)                                (Steps a   s p)
             |            Ok         {                                       rest :: Steps val s p}
             |            Cost       {costing::Int#                        , rest :: Steps val s p}
             |            StRepair   {costing::Int#  , m :: !(Message s p) , rest :: Steps val s p}
             |            Best       (Steps val s p) (Steps val s p) ( Steps val s p)
             |            NoMoreSteps val
data Action s  =  Insert s
               |  Delete s 
               |  Other  String

val :: (a -> b) -> Steps a s p -> Steps b s p

val f (OkVal a rest) = OkVal (f.a) rest
val f (Ok      rest) = OkVal  f rest
val f (Cost i  rest) = Cost i (val f rest)
val f (StRepair c m r) = StRepair c m (val f r)
val f (Best l s     r) = Best (val f l) (val f s) (val f r)
val f (NoMoreSteps v)  = NoMoreSteps (f v)

evalSteps :: Steps a s p -> a
evalSteps (OkVal v  rest    ) = v (evalSteps rest)
evalSteps (Ok       rest    ) =    evalSteps rest
evalSteps (Cost  _  rest    ) =    evalSteps rest
evalSteps (StRepair _ msg rest    ) =    evalSteps rest
evalSteps (Best _   rest  _) =  evalSteps rest
evalSteps (NoMoreSteps v    ) =  v


getMsgs :: Steps a s p -> [Message s p]
getMsgs (OkVal _        rest) = getMsgs rest
getMsgs (Ok             rest) = getMsgs rest
getMsgs (Cost _         rest) = getMsgs rest
getMsgs (StRepair _ m   rest) = m:getMsgs rest
getMsgs (Best _ m   _)        = getMsgs m
getMsgs (NoMoreSteps _      ) = []

data Message sym pos = Msg (Expecting sym) !pos (Action sym) 
-- Msg (String, String, Expecting s) -- action, position, expecting 
instance (Eq s, Show s) => Show (Expecting s) where
 show (ESym     s)   = show s
 show (EStr   str)   = str
 show (EOr     [])   = "Nothing expected "
 show (EOr    [e])   = show e
 show (EOr  (e:ee))  = show e ++ " or " ++ show (EOr ee)
 show (ESeq  seq)    = concat (map show seq)

instance (Eq s, Show s, Show p) => Show (Message s p) where
 show (Msg expecting position action)  
   =  "\n?? Error      : " ++ show position ++
      "\n?? Expecting  : " ++ show expecting ++
      "\n?? Repaired by: "  ++ show action ++"\n"

instance Show s => Show (Action s) where
  show (Insert s) = "inserting: " ++ show s 
  show (Delete s) = "deleting: "  ++ show s 
  show (Other s)  = s 
data Expecting s = ESym (SymbolR s)
                 | EStr String
                 | EOr  [Expecting s]
                 | ESeq [Expecting s]
                 deriving (Ord, Eq)
-- =======================================================================================
-- ===== SYMBOLS and RANGES ==============================================================
-- =======================================================================================

data  SymbolR s  =  Range !s !s | EmptyR deriving (Eq,Ord)

instance (Eq s,Show s) => Show (SymbolR s) where
 show EmptyR      = "the empty range"
 show (Range a b) = if a == b then show a else show a ++ ".." ++ show b


mk_range             l    r =  if l > r then EmptyR else Range l r

symInRange (Range l r) = if l == r then (l==)
                                   else (\ s ->  s >= l && s <= r)

symRS (Range l r)
  = if l == r then (compare l)
    else (\ s -> if      s < l then GT
                 else if s > r then LT
                 else               EQ)

range `except` elems
 = foldr removeelem [range] elems
   where removeelem elem ranges = [r | ran <- ranges, r <- ran `minus` elem]
         EmptyR          `minus` _    = []
         ran@(Range l r) `minus` elem = if symInRange ran elem
                                        then [mk_range l (symBefore elem), mk_range (symAfter elem) r]
                                        else [ran]
-- =======================================================================================
-- ===== TRACING  and ERRORS  and MISC ===================================================
-- =======================================================================================
usererror   m = error ("Your grammar contains a problem:\n" ++ m)
systemerror modname m
  = error ("I apologise: I made a mistake in my design. This should not have happened.\n"
                       ++
           " Please report: " ++ modname ++": " ++ m ++ " to doaitse@cs.uu.nl\n")

           
