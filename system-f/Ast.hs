module Ast where

newtype Name = Name String
    deriving (Eq)

instance Show Name where
    show (Name t) = t

data Expr
    = EVar Name
    | EApp Expr Expr
    | EAbs Name Type Expr
    | -- | type application: e [T]
      ETApp Expr Type
    | -- | type abstraction: Λa. e
      ETAbs Name Expr
    | -- | type annotation: e : T
      EAnn Expr Type
    | EInt Int
    | EBool Bool
    | -- | let n = e1 in e2
      ELet Name Expr Expr
    | -- | if e1 then e2 else e3
      EIf Expr Expr Expr
    | -- | e1 `op` e2
      EBinOp BinOp Expr Expr
    deriving (Show)

data BinOp
    = Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Eq
    | Neq
    | Lt
    | Le
    | Gt
    | Ge
    deriving (Show)

data Type
    = TVar Name
    | -- | existential type var: ^α
      TETVar Name
    | TArrow Type Type
    | -- | ∀α. A
      TForall Name Type
    | TInt
    | TBool
    deriving (Eq)

instance Show Type where
    show (TVar n) = show n
    show (TETVar n) = "^" <> show n
    show (TArrow t1@(TArrow _ _) t2) = "(" <> show t1 <> ") → " <> show t2
    show (TArrow t1 t2) = show t1 <> " → " <> show t2
    show (TForall n t) = "∀" <> show n <> ". " <> show t
    show TInt = "Int"
    show TBool = "Bool"
