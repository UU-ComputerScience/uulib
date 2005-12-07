module UU.Scanner.GenTokenSymbol() where

import UU.Scanner.GenToken(GenToken(..))
import UU.Parsing.MachineInterface(Symbol(..))

instance Symbol (GenToken key tp val) where
  deleteCost (Reserved _ _) = 5
  deleteCost _              = 5
