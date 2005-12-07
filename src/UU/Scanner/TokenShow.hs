module UU.Scanner.TokenShow() where

import UU.Scanner.Token(Token,EnumValToken(..))
import UU.Scanner.Position(Pos(..))
import UU.Scanner.GenToken(GenToken(..))

instance Show Token where
  showsPrec _ token
    = showString
       (case token of
         Reserved key      pos -> "symbol "      ++ key ++ maybeshow pos
         ValToken tp val   pos -> show tp ++ " " ++ val ++ maybeshow pos
       )
instance Show EnumValToken where
 show tp = case tp of       
  TkOp         -> "operator"  
  TkConOp      -> "con operator"            
  TkString     -> "string"              
  TkChar       -> "character"            
  TkInteger8   -> "octal integer"         
  TkInteger10  -> "decimal Integer"       
  TkInteger16  -> "hexadecimal integer"   
  TkFraction   -> "fraction (float,...)"   
  TkVarid      -> "lower case identifier" 
  TkConid      -> "upper case identifier" 
  TkTextnm     -> "text name"             
  TkTextln     -> "text lines"             
  TkError      -> "error in scanner:"   
  
maybeshow :: Pos -> String
maybeshow (Pos l c fn) | l <= 0 || c <= 0 =  ""
                       | otherwise        =  " at line " ++ show l
                                          ++ ", column " ++ show c
                                          ++ " of file " ++ show fn

