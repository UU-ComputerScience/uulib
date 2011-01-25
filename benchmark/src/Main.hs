-- | Some benchmarks for UULIB.
module Main(main) where

import UU.Parsing
import UU.Parsing.Derived
import UU.Parsing.CharParser
import UU.Parsing.Interface
import UU.Parsing.MachineInterface

import Control.DeepSeq
import Criterion.Types
import Progression.Main

-- | 26 Choices
uulibP :: Parser Char String
uulibP = pList $ pAny pSym ['a'..'z']

input :: Int -> String
input s = concat $ replicate s difficultString

difficultString :: String
difficultString = "abcdefadsjkhdasjkdasjhkdsakjdsajkdsafklfddsfajklyrrtttryytuuyttyuuytuytyuuyiuyiiuyfuyfdfsajksdfakldlsdfklsljkasxcvmdfsndfsjkldfskjdfsjkghi"

mkBench :: Int -> (String, IO ())
mkBench s = (nm,m) where
  nm = show s
  m  = do let str = input s
          deepseq str (return ())
          a <- getResult (touch <$> uulibP) str
          seq a (return ())

touch :: [a] -> ()
touch []     = ()
touch (x:xs) = seq x (touch xs)

getResult :: Parser Char a -> String -> IO a
getResult p str = 
  return $ unpack $ evalSteps $ parse p str where unpack (Pair v _) = v
  -- parseIO p str

sizes :: [Int]
sizes = [20,50,100,400,800,1000,1500,2000,3000]

main :: IO ()
main = defaultMain $ bgroup "uulib" $ map (uncurry bench) $ map mkBench sizes
