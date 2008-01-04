module UU.Scanner.GenToken where

import UU.Scanner.Position(Pos)

data GenToken key tp val =  Reserved !key !Pos
                         |  ValToken !tp val !Pos    
                 
position :: GenToken k t v -> Pos
position tok = case tok of
                   Reserved _ p   -> p
                   ValToken _ _ p -> p

