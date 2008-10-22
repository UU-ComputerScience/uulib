module Main where

-- import the the library functions from uulib
import UU.Parsing
import UU.Scanner

-- import our custom made Alex-scanner
import Scanner


-- Some boilerplate code to use the parser
-- Give `parsetokens' your parser and a list of tokens, returned by the `scanTokens'
-- function exported by the Scanner module, then you get either a list of error
-- messages in case of a parse error, or the parse tree.
type TokenParser a = Parser Token a

parseTokens :: TokenParser a -> [Token] -> Either [String] a
parseTokens p tks
  = if null msgs
    then final `seq` Right v
    else Left (map show msgs)
  where
    steps = parse p tks
    msgs  = getMsgs steps
    (Pair v final) = evalSteps steps


-- define a parse tree
data Expr
  = Identifier String
  | Integer Int
  | String String
  | Plus Expr Expr
  | Times Expr Expr
  | Let String  -- variable
        Expr    --   = expr
        Expr    -- body
  deriving Show


-- write a parser for it
-- Note: * make sure that the parser is not left recursive
--           (to the left is never a pExpr, or always a terminal first)
--       * make sure that the parser is not ambiguous
--           (by introducing priority levels for the operators)


-- Term -> let var = Expr in Expr
pExpr :: TokenParser Expr
pExpr
  =   (\_ x _ e _ b -> Let x e b) <$> pKey "let" <*> pVarid <*> pKey "=" <*> pExpr <*> pKey "in" <*> pExpr
  <|> pMult

-- Expr -> Factor | Factor * Expr
pMult :: TokenParser Expr
pMult
  =   pFactor 
  <|> (\l _ r -> Times l r) <$> pFactor <*> pKey "*" <*> pExpr

-- Factor -> Term | Term * Factor
pFactor :: TokenParser Expr
pFactor
  =   pTerm
  <|> (\l _ r -> Plus l r) <$> pTerm <*> pKey "+" <*> pFactor

-- Term -> var
-- Term -> String
-- Term -> Int
-- Term -> (Expr)
pTerm :: TokenParser Expr
pTerm
  =   Identifier <$> pVarid
  <|> (Integer . read) <$> pInteger16
  <|> (String  . read) <$> pString
  <|> (\_ e _ -> e) <$> pKey "(" <*> pExpr <*> pKey ")"


-- test it
main :: IO ()
main
  = let res = parseTokens pExpr (tokenize "nofile" "let x = 3 in x+x")
    in case res of
         Left errs -> mapM_ putStrLn errs
         Right tree -> putStrLn $ show tree
