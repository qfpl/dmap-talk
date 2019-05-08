{-# LANGUAGE GADTs #-}
module GADT.Win where

data AST a where
  IntLit :: Int -> AST Int
  BoolLit :: Bool -> AST Bool
  Lam :: (a -> AST b) -> AST (a -> b)
  Ap :: AST (a -> b) -> AST a -> AST b

eval :: AST a -> Maybe a
eval (IntLit x) =
  Just x
eval (BoolLit x) =
  Just x
eval (Ap (Lam f) x) = do
  x' <- eval x
  eval (f x')
eval _ =
  Nothing

collatzStep :: AST (Int -> Int)
collatzStep = Lam $ \x -> IntLit $
  if even x
  then x `div` 2
  else 3 * x + 1

collatz :: AST (Int -> Int)
collatz = Lam $ \x ->
  if x == 1
  then IntLit 1
  else Ap collatz (Ap collatzStep (IntLit x))

testMe :: AST Int
testMe =
  Ap collatz (IntLit 7)
