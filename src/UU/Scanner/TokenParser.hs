module UU.Scanner.TokenParser where

import UU.Parsing.Interface(IsParser(..))
import UU.Parsing.Derived(pListSep, pPacked)
import UU.Scanner.Position(Pos)
import UU.Scanner.GenTokenParser(pReserved, pValToken)
import UU.Scanner.Token(Token,EnumValToken(..))

-------------------------------------------------------------------------
-- IsParsers for  Symbols
-------------------------------------------------------------------------

pKeyPos           :: IsParser p Token => String -> p Pos
pKeyPos  keyword  =  pReserved keyword


pSpecPos          :: IsParser p Token => Char -> p Pos
pSpecPos s        =  pReserved [s]

pKey              :: IsParser p Token => String -> p String
pKey  key         =  key <$ pKeyPos key

pSpec             :: IsParser p Token => Char -> p String 
pSpec c           =  [c] <$ pSpecPos c
      
pStringPos, pCharPos,
  pInteger8Pos, pInteger10Pos, pInteger16Pos, pFractionPos,
  pVaridPos, pConidPos,
  pTextnmPos, pTextlnPos, pIntegerPos, pVarsymPos, pConsymPos  :: IsParser p Token => p (String,Pos)

pStringPos     =   pValToken TkString    ""        
pCharPos       =   pValToken TkChar      "\NUL"    
pInteger8Pos   =   pValToken TkInteger8  "0"       
pInteger10Pos  =   pValToken TkInteger10 "0"       
pInteger16Pos  =   pValToken TkInteger16 "0"
pFractionPos   =   pValToken TkFraction  "0.0"
pVaridPos      =   pValToken TkVarid     "<identifier>" 
pConidPos      =   pValToken TkConid     "<Identifier>" 
pConsymPos     =   pValToken TkConOp 	 "<conoperator>"
pVarsymPos     =   pValToken TkOp        "<operator>" 
pTextnmPos     =   pValToken TkTextnm    "<name>"       
pTextlnPos     =   pValToken TkTextln    "<line>"     
pIntegerPos    =   pInteger10Pos

pString, pChar,
  pInteger8, pInteger10, pInteger16, pFraction,
  pVarid, pConid,
  pTextnm, pTextln, pInteger, pVarsym, pConsym  :: IsParser p Token => p String

pString        = fst <$> pStringPos        
pChar          = fst <$> pCharPos          
pInteger8      = fst <$> pInteger8Pos      
pInteger10     = fst <$> pInteger10Pos     
pInteger16     = fst <$> pInteger16Pos     
pFraction      = fst <$> pFractionPos     
pVarid         = fst <$> pVaridPos         
pConid         = fst <$> pConidPos         
pVarsym        = fst <$> pVarsymPos  
pConsym        = fst <$> pConsymPos       
pTextnm        = fst <$> pTextnmPos       
pTextln        = fst <$> pTextlnPos            
pInteger       = fst <$> pIntegerPos       
  
pComma, pSemi, pOParen, pCParen, pOBrack, pCBrack, pOCurly, pCCurly
   :: IsParser p Token => p String

pComma  = pSpec ','
pSemi   = pSpec ';'
pOParen = pSpec '('
pCParen = pSpec ')'
pOBrack = pSpec '['
pCBrack = pSpec ']'
pOCurly = pSpec '{'
pCCurly = pSpec '}'

pCommaPos, pSemiPos, pOParenPos, pCParenPos, pOBrackPos, pCBrackPos, pOCurlyPos, pCCurlyPos
   :: IsParser p Token => p Pos

pCommaPos  = pSpecPos ','
pSemiPos   = pSpecPos ';'
pOParenPos = pSpecPos '('
pCParenPos = pSpecPos ')'
pOBrackPos = pSpecPos '['
pCBrackPos = pSpecPos ']'
pOCurlyPos = pSpecPos '{'
pCCurlyPos = pSpecPos '}'

pCommas ::  IsParser p Token => p a -> p [a]
pSemics ::  IsParser p Token => p a -> p [a]
pParens ::  IsParser p Token => p a -> p a
pBracks ::  IsParser p Token => p a -> p a
pCurly  ::  IsParser p Token => p a -> p a

pCommas  = pListSep pComma
pSemics  = pListSep pSemi
pParens  = pPacked pOParen pCParen
pBracks  = pPacked pOBrack pCBrack
pCurly   = pPacked pOCurly pCCurly

pParens_pCommas :: IsParser p Token => p a -> p [a]
pBracks_pCommas :: IsParser p Token => p a -> p [a]
pCurly_pSemics  :: IsParser p Token => p a -> p [a]

pParens_pCommas = pParens.pCommas
pBracks_pCommas = pBracks.pCommas
pCurly_pSemics  = pCurly .pSemics

