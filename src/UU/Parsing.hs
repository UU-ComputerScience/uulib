module UU.Parsing( module UU.Parsing.Derived
                 , module UU.Parsing.Interface
                 , parseIO
                 ) where

import UU.Parsing.Derived
import UU.Parsing.Interface

parseIO :: (Eq s, Show s, Symbol s) => Parser s a -> [s] -> IO a
parseIO = parseIOMessage showMessage 
  where showMessage (Msg expecting position action)  
          =  let pos = case position of
                           Nothing -> "at end of file"
                           Just s  -> case action of 
                                Insert _ -> "before " ++ show s
                                Delete t -> "at " ++ show t  
             in "\n?? Error      : " ++ pos ++
                "\n?? Expecting  : " ++ show expecting ++
                "\n?? Repaired by: " ++ show action ++ "\n"                

