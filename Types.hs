module Types where

newtype Code = Code Code

newtype Data = Data Data

data Stack a = Stack

newtype Void = Void Void

type U a = Stack (F (Stack a))

newtype F a = F (F a)

type Fn a b = U a -> b
