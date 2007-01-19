module Examples where

import UU.Parsing
import UU.Parsing.CharParser

a = pSym 'a'
b = pSym 'b'
c = pSym 'c'

test p inp = do result <- parseIO p inp
                putStrLn (show result)

ta = test a "a"
tb = test b "a"
tc = test c "abc"

t3 = test (pToks "xyz" ) "xy"
t4 = test (pToks "xyz" ) "xz"

pChar = 'a' <..> 'z'
pIdent = pList pChar

if_as_ident   = ((("This is the identifier: ") ++) <$> pIdent)
if_as_keyword = ((("This is the keyword: ")    ++) <$> pToks "if")
t5 = test if_as_ident   "if"
t6 = test if_as_keyword "if"

t7 = test (if_as_ident <* pCost 2 <|> if_as_keyword <* pCost 2) "if"
