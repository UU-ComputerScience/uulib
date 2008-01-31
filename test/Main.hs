{-# OPTIONS_GHC -fglasgow-exts -fallow-undecidable-instances #-}
module Main where

import UU.Parsing
import System


data Token
  = TkIdent String
  | TkKeyword String
  | TkString String


instance Show Token where
  show (TkIdent _)   = "identifier"
  show (TkKeyword s) = s
  show (TkString _)  = "literal string"

instance Eq Token where
  (TkIdent _)   == (TkIdent _)   = True
  (TkString _)  == (TkString _)  = True
  (TkKeyword s) == (TkKeyword r) = s == r
  _             == _             = False

instance Ord Token where
  compare (TkIdent _)   (TkIdent _)   = EQ
  compare (TkIdent _)   (TkKeyword _) = LT
  compare (TkIdent _)   (TkString _)  = LT
  compare (TkKeyword s) (TkKeyword r) = compare s r
  compare (TkKeyword _) (TkString _)  = LT
  compare (TkString _)  (TkString _)  = EQ
  compare _             _             = GT

instance Symbol Token



pIdent :: Parser Token String
pIdent = (\(TkIdent s) -> s) <$> pSym (TkIdent "invented_identifier")

pKey :: String -> Parser Token String
pKey sToMatch = (\(TkKeyword s) -> s) <$> pSym (TkKeyword sToMatch)

pString :: Parser Token String
pString = (\(TkString s) -> s) <$> pSym (TkString "\"invented string \"")


runParser :: Show a => Parser Token a -> [Token] -> IO String
runParser p inp
  = do result <- parseIO p inp
       return (show result)


testTokens :: [Token]
testTokens = [TkKeyword "begin", TkString "hello world", TkKeyword "end"]

testParser :: Parser Token Int
testParser = length <$ pKey "begin" <*> pString <* pKey "end"

test :: IO ()
test = do s <- runParser testParser testTokens 
          if s == "11"
           then exitWith ExitSuccess
           else exitFailure

main :: IO ()
main = test

