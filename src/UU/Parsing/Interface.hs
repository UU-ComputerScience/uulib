module UU.Parsing.Interface 
       ( AnaParser, pWrap, pMap
       , module UU.Parsing.MachineInterface
       , module UU.Parsing.Interface
       ) where

import UU.Parsing.Machine
import UU.Parsing.MachineInterface
--import IOExts
import System.IO.Unsafe
import System.IO
-- ==================================================================================
-- ===== PRIORITIES ======================================================================
-- =======================================================================================
infixl 3 <|>
infixl 4 <*>, <$> 
infixl 4 <$, <*, *>


-- =======================================================================================
-- ===== ANAPARSER INSTANCES =============================================================
-- =======================================================================================
type Parser s = AnaParser [s] Pair s (Maybe s)
-- =======================================================================================
-- ===== PARSER CLASSES ==================================================================
-- =======================================================================================

-- | The 'IsParser' class contains the base combinators with which
-- to write parsers. A minimal complete instance definition consists of
-- definitions for '(<*>)', '(<|>)', 'pSucceed', 'pLow', 'pFail', 
-- 'pCostRange', 'pCostSym', 'getfirsts', 'setfirsts', 'getzerop' 
-- and 'setzerop'.
class  IsParser p s | p -> s where
  -- | Sequential composition. Often used in combination with <$>.
  (<*>) :: p (a->b) -> p a -> p b
  (<* ) :: p a      -> p b -> p a
  ( *>) :: p a      -> p b -> p b
  -- | Applies the function f to the result of p after parsing p.
  (<$>) :: (a->b)   -> p a -> p b
  (<$ ) :: b        -> p a -> p b
  pSucceed :: a -> p a
  pLow     :: a -> p a
  f <$> p = pSucceed f <*> p
  f <$  q = pSucceed f <*  q
  p <*  q = pSucceed       const  <*> p <*> q
  p  *> q = pSucceed (flip const) <*> p <*> q
  -- | Alternative combinator.
  (<|>) :: p a -> p a -> p a
  -- | This parser always fails.
  pFail :: p a
  pCostRange   :: Int{-#L-} -> s -> SymbolR s -> p s
  pCostSym     :: Int{-#L-} -> s -> s         -> p s
  -- | Parses a symbol.
  pSym         ::                   s         -> p s
  pRange       ::              s -> SymbolR s -> p s
  getfirsts    :: p v -> Expecting s
  setfirsts    :: Expecting s -> p v ->  p v
  pSym a       =  pCostSym   5{-#L-} a a
  pRange       =  pCostRange 5{-#L-}
  getzerop     ::              p v -> Maybe (p v)
  getonep      :: p v -> Maybe (p v)


instance (Ord s, Symbol s, InputState state s p, OutputState result) => IsParser (AnaParser state result s p) s   where
  (<*>) p q = anaSeq libDollar  libSeq  ($) p q
  (<* ) p q = anaSeq libDollarL libSeqL const p q
  ( *>) p q = anaSeq libDollarR libSeqR (flip const) p q
  pSucceed =  anaSucceed
  pLow     =  anaLow
  (<|>) =  anaOr
  pFail = anaFail
  pCostRange   = anaCostRange
  pCostSym i ins sym = anaCostRange i ins (mk_range sym sym)
  getfirsts    = anaGetFirsts
  setfirsts    = anaSetFirsts
  getzerop  p  = case zerop p of
                 Nothing     -> Nothing
                 Just (b,e)  -> Just p { pars = libSucceed `either` id $ e
                                       , leng = Zero
                                       , onep = noOneParser
                                       }
  getonep   p = let tab = table (onep p)
                in if null tab then Nothing else Just (mkParser (leng p) Nothing (onep p))

instance InputState [s] s (Maybe s) where
 splitStateE []     = Right' []
 splitStateE (s:ss) = Left'  s ss
 splitState  (s:ss) = ({-#L-} s, ss{-L#-})
 getPosition []     = Nothing
 getPosition (s:ss) = Just s


instance OutputState Pair  where
  acceptR            = Pair
  nextR       acc    = \ f   ~(Pair a r) -> acc  (f a) r  
  
pCost :: (OutputState out, InputState inp sym pos, Symbol sym, Ord sym) 
      => Int -> AnaParser inp out sym pos ()
pCost x = pMap f f' (pSucceed ())
  where f  acc inp steps = (inp, Cost x (val (uncurry acc) steps))
        f'     inp steps = (inp, Cost x steps)

getInputState :: (InputState a c d, Symbol c, Ord c, OutputState b)=>AnaParser a b c d a
getInputState = pMap f g (pSucceed id)
  where f acc inp steps = (inp, val (acc inp . snd) steps)
        g = (,)

handleEof input = case splitStateE input
                   of Left'  s  ss  ->  StRepair (deleteCost s)  
                                                 (Msg (EStr "end of file") (getPosition input) 
                                                                   (Delete s)
                                                 ) 
                                                 (handleEof ss)
                      Right' ss      ->  NoMoreSteps (Pair ss ())

parse :: (Symbol s, InputState inp s pos) 
      => AnaParser inp Pair s pos a 
      -> inp 
      -> Steps (Pair a (Pair inp ())) s pos
parse = parsebasic handleEof


parseIOMessage :: ( Symbol s, InputState inp s p) 
               => (Message s p -> String) 
               -> AnaParser inp Pair s p a 
               -> inp 
               -> IO a
parseIOMessage showMessage p inp
 = do  (Pair v final) <- evalStepsIO showMessage (parse p inp) 
       final `seq` return v -- in order to force the trailing error messages to be printed
       
parseIOMessageN :: ( Symbol s, InputState inp s p) 
               => (Message s p -> String) 
               -> Int
               -> AnaParser inp Pair s p a 
               -> inp 
               -> IO a
parseIOMessageN showMessage n p inp
 = do  (Pair v final) <- evalStepsIO' showMessage n (parse p inp) 
       final `seq` return v -- in order to force the trailing error messages to be printed

data Pair a r = Pair a r

evalStepsIO :: (Message s p -> String) 
            ->  Steps b s p 
            -> IO b
evalStepsIO showMessage = evalStepsIO' showMessage (-1)      
       
evalStepsIO' :: (Message s p -> String) 
            -> Int
            ->  Steps b s p 
            -> IO b
evalStepsIO' showMessage n (steps :: Steps b s p) = eval n steps
  where eval                      :: Int -> Steps a s p -> IO a
        eval 0 steps               = return (evalSteps steps)
        eval n steps = case steps of
          OkVal v        rest -> do arg <- unsafeInterleaveIO (eval n rest)
                                    return (v arg)
          Ok             rest -> eval n rest
          Cost  _        rest -> eval n rest
          StRepair _ msg rest -> do hPutStr stderr (showMessage msg)
                                    eval (n-1) rest
          Best _   rest   _   -> eval n rest
          NoMoreSteps v       -> return v
