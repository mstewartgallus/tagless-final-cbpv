module Term where

import Types

class Term t where
  int :: Integer -> t (F Integer)
  add :: t (F Integer) -> t (F Integer) -> t (F Integer)
  lam :: (t a -> t b) -> t (Fn a b)
  mp :: t (Fn a b) -> t a -> t b
