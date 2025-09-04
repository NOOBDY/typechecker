module Ast (
    Lit (..),
    Expr (..),
    Type (..),
    Name
) where

import Data.ByteString.Lazy.Char8 (ByteString)

type Name = ByteString

data Lit
    = LInt Int
    | LBool Bool
    deriving (Show, Eq, Ord)

data Expr
    = EVar Name
    | EApp Expr Expr
    | EAbs Name Expr
    | ELet Name Expr Expr
    | ELit Lit
    | ETuple [Expr]
    deriving (Show, Eq, Ord)

data Type
    = TVar Name
    | TArrow Type Type
    | TInt
    | TBool
    | TTuple [Type]
    deriving (Show, Eq, Ord)
