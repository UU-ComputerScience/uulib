{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- Fast, Error Correcting Parser Combinators; Version: see Version History in same directory.
 - Copyright:  S. Doaitse Swierstra
               Department of Computer Science
               Utrecht University
               P.O. Box 80.089
               3508 TB UTRECHT  
               the Netherlands
               swierstra@cs.uu.nl
-}

{- file: bibtex6.hs
   A parser for BibTeX
   using the UU parsing combinators
   Piet van Oostrum, Atze Dijkstra, Doaitse Swierstra (April 22, 2001)
-}
module Bibtex where
import UU.Parsing
import Char

newtype IS s = IS (Int,Int,[s])

instance InputState (IS Char) Char (Maybe String) where
 splitStateE (IS (l, p, []    )) = Right' (IS(l,p,[]))
 splitStateE (IS (l, p, (s:ss))) = Left'  s  (if s == '\n' then  IS(l+1, 1, ss) else IS(l, p+1, ss))
 splitState  (IS (l, p, (s:ss))) = ({-L-} s, (if s == '\n' then  IS(l+1, 1, ss) else IS(l, p+1, ss)) {-R-})
 getPosition (IS (l, p, []    )) = Nothing
 getPosition (IS (l, p, (s:ss))) = Just (" before " ++ show s ++ " at line: " ++show l ++ " column: " ++ show p)

instance Symbol Char where
 symBefore = pred
 symAfter  = succ
 

parsebib filename -- e.g. parsebib "btxdoc.bib"
  = let  showMessage (Msg expecting position action)  
          =  let pos = case position of
                           Nothing -> "at end of file"
                           Just s  -> case action of 
                                Insert _ -> "before " ++ show s
                                Delete t -> "at " ++ show t  
             in "\n?? Error      : " ++ pos ++
                "\n?? Expecting  : " ++ show expecting ++
                "\n?? Repaired by: " ++ show action ++ "\n" 
    in do input <- readFile filename
          res   <- parseIOMessage showMessage  pBibData (IS (1,1,input))
          putStr ("\nResult:" ++ show (length res) ++ " bib items were parsed\n")
-- =======================================================================================
-- ===== DATA TYPES ======================================================================
-- =======================================================================================
type BibData   = [ BibEntry]

data BibEntry  = Entry     String  (String, [Field])  -- kind keyword fieldlist
	       | Comment   String
	       | Preamble  [ValItem]
	       | StringDef Field
               deriving Show

type Field    = (String, [ValItem])

data ValItem  = StringVal String          
	      | IntVal    Int
	      | NameUse   String
	      deriving Show
-- =======================================================================================
-- ===== PARSERS =========================================================================
-- =======================================================================================
-- pBibData parses a list of BiBTex entries separated by garbage
-- a @ signifies the start of a new entry
pBibData    = pChainr ((\ entry  _ right -> entry:right) <$> pBibEntry)
                      ( [] <$ pList (allChars `pExcept` "@"))

pBibEntry   
 =  (   Entry     <$ pAt <*> pName           <*> pOpenClose (   pKeyName       <*  pSpec ','
		                                                          <+> pListSep_ng pComma pField 
                                                            <* (pComma `opt` ' '))
    <|> Comment   <$ pAt <*  pKey "comment"  <*> (  pCurly (pList (allChars `pExcept` "}"))
	                                               <|> pParen (pList (allChars `pExcept` ")"))
                                                 )
    <|> Preamble  <$ pAt <*  pKey "preamble" <*> pOpenClose pValItems 
    <|> StringDef <$ pAt <*  pKey "string"   <*> pOpenClose pField
    ) 

pField     =  pName <* pSpec '=' <+> pValItems

pValItems  =  pList1Sep (pSpec '#') (   StringVal   <$> pString 
	                                   <|> int_or_name <$> pName
                                    )
              where int_or_name s = if all isDigit s
                                    then IntVal.(read::String->Int) $ s
                                    else NameUse s
-- =======================================================================================
-- ===== LEXICAL STUFF ===================================================================
-- =======================================================================================
pLAYOUT :: AnaParser (IS Char) Pair Char (Maybe String) String

pLAYOUT = pList (pAnySym " \t\r\n")
pSpec c = pSym c  <* pLAYOUT

pParen      p = pPacked (pSpec '(') (pSpec ')') p
pCurly      p = pPacked (pSpec '{') (pSpec '}') p
pOpenClose  p = pParen p <|> pCurly p 
pComma        = pCostSym  4 ',' ',' <* pLAYOUT
pAt           = pSpec '@'

allChars = (chr 1, chr 127, ' ')

pName     = pList1 ('a'<..>'z' <|> 'A'<..>'Z' <|> '0'<..>'9'  <|> pAnySym "-_/") <* pLAYOUT
pKeyName  = pList1 ((chr 33, chr 127, ' ') `pExcept` ",=@"                     ) <* pLAYOUT

pKey [s]     = lift <$> (pSym s <|> pSym (toUpper s)) <*  pLAYOUT
pKey (s:ss)  = (:)  <$> (pSym s <|> pSym (toUpper s)) <*> pKey ss
pKey []      = usererror "Scanner: You cannot have empty reserved words!"

pString 
 = let  curlyStrings  = stringcons <$> pSym '{' <*> pConc pStringWord <*> pSym '}'
        pStringWordDQ = lift       <$> pStringCharDQ <|> curlyStrings
        pStringWord   = lift       <$> pStringChar   <|> curlyStrings
        pStringCharDQ = allChars `pExcept` "\"{}"
        pStringChar   = pStringCharDQ <|> pSym '\"'
        pConc         = pFoldr ((++),[]) 
        stringcons c1 ss c2 = [c1] ++ ss ++ [c2]
   in (   pSym '"' *> pConc pStringWordDQ <* pSym '"'
	     <|> pSym '{' *> pConc pStringWord   <* pSym '}'
      ) <* pLAYOUT

lift c              = [c]

