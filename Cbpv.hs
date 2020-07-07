module Cbpv where

import Types

class Cbpv t where
  int :: Integer -> t Data Integer
  add :: t Data Integer -> t Data Integer -> t Code (F Integer)
  lam :: (t Data a -> t Code b) -> t Code (a -> b)
  mp :: t Code (a -> b) -> t Data a -> t Code b
  letTo :: t Code (F a) -> (t Data a -> t Code b) -> t Code b
  returns :: t Data a -> t Code (F a)
  thunk :: t Code a -> t Data (U a)
  force :: t Data (U a) -> t Code a
