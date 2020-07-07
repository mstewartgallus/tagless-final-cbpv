module Callcc where

import Types

class Callcc t where
  int :: Integer -> t Data Integer
  add :: t Data Integer -> t Data Integer -> t Code (F Integer)
  lam :: (t Data a -> t Code b) -> t Code (a -> b)
  mp :: t Code (a -> b) -> t Data a -> t Code b
  letTo :: t Code (F a) -> (t Data a -> t Code b) -> t Code b
  returns :: t Data a -> t Code (F a)
  catch :: (t Data (Stack a) -> t Code Void) -> t Code a
  throw :: t Data (Stack a) -> t Code a -> t Code Void
