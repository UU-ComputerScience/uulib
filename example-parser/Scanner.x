{
-- alex scanner for use with uulib
--   compile with alex -o Scanner.hs -g Scanner.x
module Scanner(tokenize) where

import UU.Scanner
}

$litChar   = [^[\" \\]]
$identChar = [a-zA-Z0-9\'_]


tokens :-
  $white+                                      ;                               -- whitespace
  "--".*                                       ;                               -- comment

  \" ($litChar | \\ \\ | \\ \" )* \"           { valueToken TkString }         -- string
  [0-9]+                                       { valueToken TkInteger16 }      -- int
  
  ( let | in )                                 { reserved }                    -- reserved keywords
  [\(\)\=]                                     { reserved }                    -- reserved symbols
  [\+\*]                                       { reserved }                    -- operators

  [a-zA-Z] $identChar*                         { valueToken TkVarid }          -- identifier


{
-- boilerplate code needed for Alex
type AlexInput = (Pos, String)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "alexInputPrevChar: there is no need to go back in the input."

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (_, []) = Nothing
alexGetChar (p, (c:cs))
  = let p' = adv p c
    in Just (c, (p', cs))

-- use the Alex scanner to generate a list of tokens for the uulib token parsers
tokenize :: String -> String -> [Token]
tokenize filename str
  = go (initpos, str)
  where
    initpos = Pos 1 1 filename
    
    go inp@(pos, cs)
      = case alexScan inp 0 of
          AlexEOF         -> []
          AlexError inp'  -> valueToken TkError [head cs] pos : go inp'
          AlexSkip inp' _ -> go inp'
          AlexToken inp' len act -> act (take len cs) pos : go inp'
}

