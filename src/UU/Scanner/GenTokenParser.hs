module UU.Scanner.GenTokenParser where
import GHC.Base
import UU.Parsing.Interface(IsParser(pCostSym, pSym, (<$>)))
import UU.Scanner.GenToken(GenToken(..))
import UU.Scanner.Position(Pos, noPos)


pCostReserved'          :: IsParser p (GenToken key tp val) 
                        => Int -> key -> p (GenToken key tp val)
pCostReserved' (I# c) key
                        =  let tok = Reserved key noPos 
                           in  pCostSym c tok tok 

pReserved'              :: IsParser p (GenToken key tp val) 
                        => key -> p (GenToken key tp val)
pReserved' key          =  let tok = Reserved key noPos 
                           in  pSym tok 

pCostValToken'          :: IsParser p (GenToken key tp val) 
                        => Int -> tp -> val -> p (GenToken key tp val)
pCostValToken' (I# c) tp val
                        =  let tok = ValToken tp val noPos 
                           in  pCostSym c tok tok 

pValToken'              :: IsParser p (GenToken key tp val) 
                        => tp -> val -> p (GenToken key tp val)
pValToken' tp val       =  let tok = ValToken tp val noPos 
                           in  pSym tok 


pCostReserved           :: IsParser p (GenToken key tp val) 
                        => Int -> key -> p Pos
pCostReserved c key     =  let getPos x = case x of
                                Reserved _   p -> p
                                ValToken _ _ p -> p
                           in getPos <$> pCostReserved' c key
                          
pCostValToken           :: IsParser p (GenToken key tp val) 
                        => Int -> tp -> val -> p (val,Pos)
pCostValToken c tp val  =  let getVal x = case x of
                                ValToken _ v p -> (v,p)
                                _              -> error "pValToken: cannot get value of Reserved"
                           in getVal <$> pCostValToken' c tp val

pReserved               :: IsParser p (GenToken key tp val) 
                        => key -> p Pos
pReserved               =  pCostReserved 5

pValToken               :: IsParser p (GenToken key tp val) 
                        => tp -> val -> p (val,Pos)
pValToken               =  pCostValToken 5

pValTokenNoPos          :: IsParser p (GenToken key tp val) 
                        => tp -> val -> p val
pValTokenNoPos tp val   =  fst <$> pValToken tp val                          

