module Examples where

import UU.Parsing

instance Symbol Char where
  symBefore = pred

a, b, c :: P Char
a = pSym 'a'
b = pSym 'b'
c = pSym 'c'

type P = AnaParser [Char] Pair Char (Maybe Char) 

test p inp = do result <- parseIOMessage show p inp
                putStrLn (show result)

t1 = test a "a"
t2 = test b "a"
t3 = test (pToks "xyz" ) "xy"
t4 = test (pToks "xyz" ) "xz"

pChar = 'a' <..> 'z'
pIdent = pList pChar

if_as_ident   = ((("This is the identifier: ") ++) <$> pIdent)
if_as_keyword = ((("This is the keyword: ")    ++) <$> pToks "if")
t5 = test if_as_ident   "if"
t6 = test if_as_keyword "if"

t7 = test (if_as_ident <* pCost 1 <|> if_as_keyword ) "if"
