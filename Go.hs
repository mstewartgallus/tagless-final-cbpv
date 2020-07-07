module Go where

import qualified Callcc
import qualified Cbpv
import Control.Monad.State.Strict
import qualified Term
import Types

newtype AsCbpv t x = AsCbpv (t Code x)

instance Cbpv.Cbpv t => Term.Term (AsCbpv t) where
  int x = AsCbpv (Cbpv.returns (Cbpv.int x))
  add (AsCbpv x) (AsCbpv y) = AsCbpv
    $ Cbpv.letTo x
    $ \x' ->
      Cbpv.letTo y $ \y' ->
        Cbpv.add x' y'
  lam f =
    AsCbpv
      ( Cbpv.lam
          ( \x -> case f (AsCbpv (Cbpv.force x)) of
              AsCbpv c -> c
          )
      )
  mp (AsCbpv f) (AsCbpv x) = AsCbpv (Cbpv.mp f (Cbpv.thunk x))

newtype AsCallcc t tag x = AsCallcc ((t tag x -> t Code Void) -> t Code Void)

instance Callcc.Callcc t => Cbpv.Cbpv (AsCallcc t) where
  int x = AsCallcc $ \k -> k $ Callcc.int x
  add (AsCallcc x) (AsCallcc y) = AsCallcc $ \k ->
    x $ \x' ->
      y $ \y' ->
        k $ Callcc.add x' y'
  lam f = AsCallcc $ \k ->
    k
      $ Callcc.lam
      $ \x -> Callcc.catch $ \ret -> case f (AsCallcc $ \l -> l x) of
        AsCallcc c -> c $ \c' ->
          Callcc.throw ret c'
  mp (AsCallcc f) (AsCallcc x) = AsCallcc $ \k -> f $ \f' -> x $ \x' -> k $ Callcc.mp f' x'
  returns (AsCallcc x) = AsCallcc $ \k -> x $ \x' -> k $ Callcc.returns x'
  force (AsCallcc thunk) = AsCallcc $ \k ->
    thunk $ \thunk' ->
      k $ Callcc.catch $ \v ->
        Callcc.throw thunk' (Callcc.returns v)
  thunk (AsCallcc code) = AsCallcc $ \returner ->
    code $ \code' ->
      Callcc.letTo (Callcc.catch returner) $
        \binder -> Callcc.throw binder code'
  letTo (AsCallcc x) f = AsCallcc $ \k ->
    x $ \x' ->
      Callcc.letTo x' $ \val ->
        case f (AsCallcc $ \l -> l val) of
          AsCallcc f' -> f' k

genSym :: State Integer String
genSym = do
  x <- get
  put (x + 1)
  return ("v" ++ show x)

newtype PP tag a = PP {pp :: State Integer String}

instance Callcc.Callcc PP where
  int x = PP $ pure $ show x
  add (PP x) (PP y) = PP $ do
    x' <- x
    y' <- y
    pure $ "(" ++ x' ++ " + " ++ y' ++ ")"
  lam f = PP $ do
    v <- genSym
    body <- pp (f (PP $ pure v))
    pure $ "\\" ++ v ++ " ->\n" ++ body
  mp (PP f) (PP x) = PP $ do
    f' <- f
    x' <- x
    pure $ x' ++ "\n" ++ f'
  letTo (PP x) f = PP $ do
    v <- genSym
    x' <- x
    body <- pp (f (PP $ pure v))
    pure $ x' ++ " to " ++ v ++ ".\n" ++ body
  returns (PP value) = PP $ do
    value' <- value
    pure $ "return " ++ value'
  catch f = PP $ do
    v <- genSym
    body <- pp (f (PP $ pure v))
    pure $ "catch " ++ v ++ ".\n" ++ body
  throw (PP stack) (PP x) = PP $ do
    stack' <- stack
    x' <- x
    pure $ "throw " ++ stack' ++ ".\n" ++ x'

fn :: Term.Term t => t (F Integer)
fn = Term.mp (Term.lam $ \x -> (x `Term.add` x) `Term.add` (x `Term.add` x)) (Term.int 5)

fnCbpv :: Cbpv.Cbpv t => t Code (F Integer)
fnCbpv = case fn of
  AsCbpv c -> c

fnCallcc :: Callcc.Callcc t => t Code (F Integer)
fnCallcc = case fnCbpv of
  AsCallcc c -> Callcc.catch $ \k ->
    c $ \c' ->
      Callcc.throw k c'

fnPP :: String
fnPP = case fnCallcc of
  PP str -> evalState str 0

main :: IO ()
main = putStrLn fnPP
