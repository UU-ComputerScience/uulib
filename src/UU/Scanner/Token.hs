module UU.Scanner.Token where

import UU.Scanner.GenToken(GenToken(..)) 
import UU.Scanner.Position(Pos) 

type Token = GenToken String EnumValToken String

data EnumValToken
  = TkVarid
  | TkConid
  | TkString
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkFraction
  | TkTextnm
  | TkTextln 
  | TkOp
  | TkConOp
  | TkError
  deriving (Eq, Ord)

reserved                :: String -> Pos -> Token
reserved                =  Reserved 

valueToken              :: EnumValToken -> String -> Pos -> Token
valueToken              =  ValToken 

errToken                :: String -> Pos -> Token
errToken                =  valueToken TkError 

