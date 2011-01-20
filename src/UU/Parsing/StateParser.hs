{-# LANGUAGE  MagicHash,
              UnboxedTuples,
              ScopedTypeVariables #-}

module UU.Parsing.StateParser(StateParser(..)) where
import GHC.Prim
import UU.Parsing.MachineInterface
import UU.Parsing.Machine(AnaParser, ParsRec(..),RealParser(..),RealRecogn(..), mkPR, anaDynE)

instance (InputState inp s p) => InputState (inp, state) s p where
  splitStateE (inp, st) = case splitStateE inp of
                  Left'   x xs   -> Left'  x (xs, st)
                  Right'  xs     -> Right'   (xs, st)
  splitState  (inp, st) = case splitState inp of
                  (# x,xs #) -> (# x, (xs, st) #)
  getPosition (inp, _) = getPosition inp

class StateParser p st | p -> st where
  change :: (st -> st) -> p st -- return the old state
  set    :: st -> p st
  set x = change (const x)
  get    :: p st
  get = change id

fconst x y = y

instance (InputState inp s p ,OutputState out) =>
          StateParser (AnaParser (inp, st) out s p) st where
  get = anaDynE (mkPR (rp,rr))
    where f addRes k state =  (val (addRes (snd state)) (k state))
          rp = P f
          rr = R (f fconst )
          
  change ch = anaDynE (mkPR (rp,rr))
    where f addRes k state = case state of (inp, st) -> val (addRes st) (k (inp, ch st))
          rp = P f 
          rr = R (f fconst)

newtype Errors s p = Errors [[Message s p]]

