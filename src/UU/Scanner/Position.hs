module UU.Scanner.Position where

type Line     = Int
type Column   = Int
type Filename = String


class Position p where 
  line   :: p -> Line
  column :: p -> Column
  file   :: p -> Filename


instance Position Pos where
   line   (Pos l _ _) = l
   column (Pos _ c _) = c
   file   (Pos _ _ f) = f

data Pos = Pos !Line !Column Filename 

instance Show Pos where
  show (Pos l c f) | l == (-1) = ""
                   | otherwise = let file = if null f then "" else show f
                                     lc = "(line " ++ show l ++ ", column " ++ show c ++")"
                                 in file ++ lc
initPos :: FilePath -> Pos
initPos fn = Pos 1 1 fn

noPos :: Pos
noPos = Pos (-1) (-1) ""

advl ::  Line -> Pos ->Pos
advl i (Pos l c f) = (Pos (l+i) 1 f)

advc :: Column -> Pos ->  Pos
advc i (Pos l c f) = (Pos l (c+i) f)

adv :: Pos -> Char -> Pos
adv pos c = case c of
  '\t' -> advc (tabWidth (column pos)) pos
  '\n' -> advl 1 pos
  _    -> advc 1 pos

updPos :: Char -> Pos -> Pos
updPos x = case x of
 '\n' -> newl
 '\t' -> tab
 _    -> advc 1

tab              :: Pos -> Pos
tab  (Pos l c f) =  Pos l (c+tabWidth c) f

newl :: Pos ->Pos
newl =  advl 1

tabWidth :: Column -> Int
tabWidth c = 8 - ((c-1) `mod` 8)


updPos' :: Char -> Pos -> (Pos -> a) -> a
updPos' c p cont = p `seq` cont (updPos c p)

advc' :: Int -> Pos -> (Pos -> a) -> a
advc' i p cont = p `seq` cont (advc i p)

tab' :: Pos -> (Pos -> a) -> a
tab'  p cont = p `seq` cont (tab p)

newl' :: Pos -> (Pos -> a) -> a
newl' p cont = p `seq` cont (newl p)
