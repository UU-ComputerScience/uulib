module UU.Util.Utils where

newtype Id x = Id x

cross :: (a->c) -> (b->d) -> (a,b) -> (c,d)
cross f g (x,y) = (f x, g y)

split :: (a->b) -> (a->c) -> a -> (b,c)
split f g x = (f x,g x)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thd3 :: (a,b,c) -> c
thd3 (_,_,c) = c