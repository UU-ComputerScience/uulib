module UU.Parsing.TraceWrap
  ( wtrace
  )
  where

import Debug.Trace

wtrace :: String -> a -> a
wtrace s v = {- trace s -} v
{-# NOINLINE wtrace #-}
