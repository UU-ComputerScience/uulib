module UU.Parsing.Offside( parseOffside 
                         , pBlock 
                         , pBlock1 
                         , pOffside 
                         , pOpen 
                         , pClose 
                         , pSeparator 
                         , scanOffside 
                         , OffsideSymbol(..)
                         , OffsideInput
                         , Stream
                         , OffsideParser(..)
                         ) where
                         
import UU.Parsing.Interface
import UU.Parsing.Machine
import UU.Parsing.Derived(opt, pFoldr1Sep,pList,pList1, pList1Sep)
import UU.Scanner.Position

data OffsideSymbol s = 
                Symbol s
              | SemiColon
              | CloseBrace
              | OpenBrace
              deriving (Ord,Eq,Show)


scanOffside :: (InputState i s p, Position p, Eq s) 
            =>  s ->  s -> s -> [s] -> i -> OffsideInput i s p  
scanOffside mod open close triggers ts = start ts []
 where
 isModule  t = t == mod 
 isOpen    t = t == open
 isClose   t = t == close
 isTrigger t = t `elem` triggers
 end ts    = Off (getPosition ts) (End ts)
 cons :: p -> OffsideSymbol s -> OffsideInput i s p -> OffsideInput i s p
 cons p s r =  Off p (Cons s r) Nothing  
 start = case splitStateE ts of
          Left' t _ | not (isModule t || isOpen t) -> implicitL 0 (column (getPosition ts) )
          _                                        -> layoutL   0
 
 -- L (<n>:ts) (m:ms) 	= ; : (L ts (m:ms)) 	if m = n 
 --	                = } : (L (<n>:ts) ms) 	if n < m 
 -- L (<n>:ts) ms 	= L ts ms 
 startlnL l n ts (m:ms) | m == n  = cons (getPosition ts) SemiColon  (layoutL (line (getPosition ts)) ts (m:ms))    
                          | n <  m  = cons (getPosition ts) CloseBrace (startlnL l n ts ms)
 startlnL l n ts ms               = layoutL (line (getPosition ts))  ts ms
 -- L  ({n}:ts)  (m:ms) = { : (L  ts (n:m:ms))     if n > m    (Note  1) 
 -- L  ({n}:ts)  []     = { : (L  ts [n])          if n > 0    (Note  1) 
 -- L  ({n}:ts)  ms     = { : } : (L  (<n>:ts) ms) (Note  2) 
 implicitL l n ts (m:ms) | n > m  = cons (getPosition ts) OpenBrace (layoutL (line (getPosition ts)) ts (n:m:ms))
 implicitL l n ts []     | n > 0  = cons (getPosition ts) OpenBrace (layoutL (line (getPosition ts)) ts [n])
 implicitL l n ts ms              = cons (getPosition ts) OpenBrace (cons (getPosition ts) CloseBrace (startlnL l n ts ms))
 layoutL ln ts ms | ln /= sln = startln (column pos) ts ms
                    | otherwise     = sameln ts ms
      
  where sln = line pos
        pos   = getPosition ts
        layout    = layoutL ln      
        implicit  = implicitL ln
        startln = startlnL ln    
        -- If a let ,where ,do , or of keyword is not followed by the lexeme {,  
        -- the token {n} is inserted after the keyword, where nis the indentation of
        -- the  next lexeme if there is one, or 0 if the end of file has been reached. 
        aftertrigger ts ms = case splitStateE ts of
                                Left' t _  | isOpen t  -> layout ts ms
                                           | otherwise -> implicit (column(getPosition ts)) ts ms
                                Right' _               -> implicit 0 ts ms


        -- L  ( }:ts)  (0:ms) = } : (L  ts ms)          (Note  3) 
        --              L  ( }:ts)  ms     = parse-error             (Note  3), matching of implicit/explicit braces is handled by parser
        -- L  ( {:ts)  ms     = {: (L  ts (0:ms))       (Note  4) 
        -- L  (t:ts)  (m:ms)  = }: (L  (t:ts)  ms)      if  m /= 0  and  parse-error(t) (Note  5) 
        -- L  (t:ts)  ms      = t : (L  ts ms) 
        sameln tts ms = case splitStateE tts of
              Left'  t ts  ->
                        let tail
                              | isTrigger t  = aftertrigger ts ms
                              | isClose t    = case ms of
                                                         0:rs -> layout ts rs
                                                         _    -> layout ts ms

                              | isOpen t    = layout ts (0:ms)
                              | otherwise   = layout ts ms
                            parseError = case ms of
                                           m:ms  | m /= 0 -> Just (layout tts ms)
                                           _              -> Nothing
                        in  Off pos (Cons (Symbol t) tail) parseError
              Right' rest -> endofinput pos rest ms
          where pos = getPosition tts
{-
        sameln tts ms = case splitStateE tts of
                Left'  t ts  | isTrigger t -> cons pos (Symbol t) (aftertrigger ts ms)
                             | isClose t   -> cons pos (Symbol t) 
                                                (case ms of
                                                   0:ms -> layout ts ms
                                                   _    -> layout ts ms
                                                )   
                             | isOpen t    -> cons pos (Symbol t) (layout ts (0:ms))                            
                             | otherwise   -> let parseError = case ms of
                                                                  m:ms  | m /= 0 -> Just (layout tts ms)
                                                                  _              -> Nothing
                                              in Off pos (Cons (Symbol t) (layout ts ms)) parseError
                Right' rest -> endofinput pos rest ms
          where pos = getPosition tts                        
-}

        -- L  []  []          = [] 
        -- L  []  (m:ms)      = } : L  []  ms           if m /=0   (Note  6) 
        --                    = L [] ms, if m == 0 (this is an error, the parser should yield a parse error, if this situation occurs)
        endofinput pos rest []                 = Off pos (End rest) Nothing
        endofinput pos rest (m:ms) | m /= 0    = cons pos CloseBrace (endofinput pos rest ms)
                                   | otherwise = endofinput pos rest ms


data Stream inp s p = Cons (OffsideSymbol s) (OffsideInput inp s p) 
                    | End inp

data OffsideInput inp s p = Off p (Stream inp s p) (Maybe (OffsideInput inp s p))

instance InputState inp s p => InputState (OffsideInput inp s p) (OffsideSymbol s) p where
  splitStateE inp@(Off p stream _) = case stream of
                                     Cons s rest -> Left' s rest
                                     _           -> Right' inp                                 
  splitState (Off _ stream _) = 
           case stream of
            Cons s rest -> (s ,rest)                        

  getPosition (Off pos _ _ ) = pos
  
instance Symbol s => Symbol (OffsideSymbol s) where
  deleteCost s = case s of
                  Symbol s   -> deleteCost s
                  SemiColon  -> 5
                  OpenBrace  -> 5
                  CloseBrace -> 5
  symBefore s = case s of
                 Symbol s   -> Symbol (symBefore s)
                 SemiColon  -> error "Symbol.symBefore SemiColon"
                 OpenBrace  -> error "Symbol.symBeforeOpenBrace"
                 CloseBrace -> error "Symbol.symBefore CloseBrace"
  symAfter s = case s of
                 Symbol s   -> Symbol (symAfter s)
                 SemiColon  -> error "Symbol.symAfter SemiColon"
                 OpenBrace  -> error "Symbol.symAfter OpenBrace"
                 CloseBrace -> error "Symbol.symAfter CloseBrace"

newtype OffsideParser i o s p a  = OP (AnaParser (OffsideInput i s p) o (OffsideSymbol s) p a)        

instance  (Symbol s, Ord s, InputState i s p, OutputState o) => IsParser (OffsideParser i o s p) s where
  (<*>) = operator (<*>)
  (<* ) = operator (<* )
  ( *>) = operator ( *>)
  (<|>) = operator (<|>)
  (<$>) = operatorr (<$>)
  (<$ ) = operatorr (<$ )
  pSucceed = OP . pSucceed
  pLow     = OP . pLow
  pFail    = OP pFail
  pCostRange c s (Range l r) = OP (getSymbol <$> pCostRange c (Symbol s) (Range (Symbol l)(Symbol r)))  
  pCostSym   c s t           = OP (getSymbol <$> pCostSym c (Symbol s) (Symbol t))  
  pSym   s                   = OP (getSymbol <$> pSym (Symbol s))  
  pRange s (Range l r)       = OP (getSymbol <$> pRange (Symbol s) (Range (Symbol l)(Symbol r)))  
  getfirsts  (OP p)          = removeSymbol (getfirsts p)
  setfirsts  exp (OP p)      = OP (setfirsts (addSymbol exp) p)
  getzerop  (OP p)           = fmap OP (getzerop p)
  getonep   (OP p)           = fmap OP (getonep p)

removeSymbol exp = case exp of
        ESym (Range (Symbol l) (Symbol r)) -> ESym (Range l r)
        ESym _                             -> EOr []
        EStr txt                           -> EStr txt
        EOr  exps                          -> EOr  (map removeSymbol exps)
        ESeq exps                          -> ESeq (map removeSymbol exps)

addSymbol exp = case exp of
        ESym (Range l r) -> ESym (Range (Symbol l) (Symbol r))
        EStr txt         -> EStr txt
        EOr  exps        -> EOr  (map addSymbol exps)
        ESeq exps        -> ESeq (map addSymbol exps)

getSymbol (Symbol s) = s

operator  f (OP p) (OP q) = OP (f p q)
operatorr f g (OP p) = OP (f g p)

pSeparator :: (OutputState o, InputState i s p, Position p, Symbol s, Ord s) 
           => OffsideParser i o s p ()
pSeparator = OP (() <$ pCostSym 5 SemiColon SemiColon)

pClose, pOpen :: (OutputState o, InputState i s p, Position p, Symbol s, Ord s) 
           => OffsideParser i o s p ()
           

pClose = OP (pWrap f g ( () <$ pSym CloseBrace) )
  where g state steps1 k = (state,ar,k)
          where ar = case state of
                               Off _ _ (Just state') -> let steps2 = k state'
                                                        in if not (hasSuccess steps1) && hasSuccess steps2 then steps2 else steps1
                               _                     -> steps1
            
        f acc state steps k = let (stl,ar,str2rr) = g state (val snd steps)  k
                              in (stl ,val (acc ()) ar , str2rr )

pOpen  = OP (() <$ pSym OpenBrace) 

pOffside :: (InputState i s p, OutputState o, Position p, Symbol s, Ord s) 
         => OffsideParser i o s p x 
         -> OffsideParser i o s p y 
         -> OffsideParser i o s p a 
         -> OffsideParser i o s p a 
         -> OffsideParser i o s p a
pOffside open close bodyE bodyI = 
       open *> bodyE <* close
   <|> pOpen *> bodyI <* pClose
   
pBlock :: (InputState i s p, OutputState o, Position p, Symbol s, Ord s) 
       => OffsideParser i o s p x 
       -> OffsideParser i o s p y 
       -> OffsideParser i o s p z 
       -> OffsideParser i o s p a 
       -> OffsideParser i o s p [a]
pBlock open sep close p =  pOffside open close explicit implicit
 where elem = (:) <$> p `opt` id
       sep' = () <$ sep        
       elems s = ($[]) <$> pFoldr1Sep ((.),id) s elem
       explicit = elems sep'
       implicit = elems (sep' <|> pSeparator)

pBlock1 :: (InputState i s p, OutputState o, Position p, Symbol s, Ord s) 
       => OffsideParser i o s p x 
       -> OffsideParser i o s p y 
       -> OffsideParser i o s p z 
       -> OffsideParser i o s p a 
       -> OffsideParser i o s p [a]
pBlock1 open sep close p =  pOffside open close explicit implicit
 where sep'    = () <$ sep
       elems s = pList s *> pList1Sep (pList1 s) p <* pList s
       explicit = elems sep'
       implicit = elems (sep' <|> pSeparator)

parseOffside :: (Symbol s, InputState i s p, Position p) 
             => OffsideParser i Pair s p a 
             -> OffsideInput i s p
             -> Steps (a, OffsideInput i s p) (OffsideSymbol s) p
parseOffside (OP p) inp = val fromPair (parse p inp)
  where fromPair (Pair x (Pair y _)) = (x,y)
