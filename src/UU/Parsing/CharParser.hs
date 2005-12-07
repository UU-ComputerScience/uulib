module UU.Parsing.CharParser where

import UU.Parsing.Interface
import UU.Scanner.Position


type CharParser = AnaParser Input Pair Char Pos

instance Symbol Char where
 symBefore    = pred
 symAfter     = succ
 deleteCost _ = 5

data Input = Input String !Pos

instance InputState Input Char Pos where
  splitStateE (Input inp pos) = 
        case inp of
          ('\CR':      xs) -> case xs of
                                ('\LF' : _ ) -> Left' '\CR' (Input xs pos)
                                _            -> Left' '\CR' (Input xs (newl pos))
          ('\LF':      xs) -> Left' '\LF' (Input xs (newl   pos))
          ('\n' :      xs) -> Left' '\n'  (Input xs (newl pos))
          ('\t' :      xs) -> Left' '\t' (Input xs (tab    pos))
          (x    :      xs) -> Left' x    (Input xs (advc 1 pos))
          []               -> Right'     (Input [] pos)
            
  splitState  (Input inp pos) =  
        case inp of
          ('\CR':      xs) -> case xs of
                                ('\LF' : _ ) -> ('\CR', Input xs pos)
                                _            -> ('\CR', Input xs (newl pos))
          ('\LF':      xs) -> ( '\LF', Input xs (newl   pos))
          ('\n' :      xs) -> ( '\n' , Input xs (newl   pos))
          ('\t' :      xs) -> ( '\t' , Input xs (tab    pos))
          (x    :      xs) -> ( x    , Input xs (advc 1 pos))

  getPosition (Input inp pos) = pos

parseString :: CharParser a 
            -> [Char] 
            -> Steps (Pair a (Pair Input ())) Char Pos
parseString p txt = parse p ((Input txt (initPos "")))

parseStringIO :: (Message Char Pos -> String) 
              -> CharParser a 
              -> [Char] 
              -> IO a
parseStringIO showM p txt = parseIOMessage showM p (Input txt (initPos ""))

parseFile :: (Message Char Pos -> String) -> CharParser a -> [Char] -> IO a
parseFile showM p filename = do txt <- readFile filename
                                parseIOMessage showM p (Input txt (initPos filename))
