module TypeLambda where

import Prelude
import Data.List
import Data.Map

data Typed t = 
  VarType t
  | TermType (Typed t) (Typed t)
  deriving Eq

instance Show t => Show (Typed t) where
  show (VarType t) = show t
  show (TermType t1 t2) = "(" ++ (show t1) ++ "->" ++ (show t2) ++ ")"

data Term a = 
      Var a
    | Lambda a (Term a)
    | Apply (Term a) (Term a)
    deriving Eq

instance Show a => Show (Term a) where
    show (Var v)       = show v
    show (Lambda n t)  = "/|" ++ (show n) ++ "." ++ (show t)
    show (Apply t1 t2) = "(" ++ (show t1) ++ " " ++ (show t2) ++ ")"

hindley_milner_typing::Ord a => Eq a => Eq t => Term a -> Map a t -> Typed t
hindley_milner_typing (Var a1) m = VarType (m ! a1) 
hindley_milner_typing (Lambda a1 t1) m = (TermType (VarType (m ! a1)) (hindley_milner_typing t1 m))
hindley_milner_typing (Apply t1 t2) m = TermType (hindley_milner_typing t1 m) (hindley_milner_typing t2 m)

