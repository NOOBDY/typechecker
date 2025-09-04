-- vim: set ft=haskell

{
module Parser (parseExpr, parseError) where

import qualified Ast as Ast
import qualified Lexer as L

import qualified Data.ByteString.Lazy.Char8 as BS
}

%name parseExpr expr
%tokentype { L.Token }
%error { parseError }

%token
    int     { L.TInt _ }
    ident   { L.TIdent _ }
    bool    { L.TBool _ }
    let     { L.TLet }
    in      { L.TIn }
    '\\'    { L.TLambda }
    '('     { L.TLPar }
    ')'     { L.TRPar }
    ','     { L.TComma }
    '='     { L.TEquals }
    '->'    { L.TArrow }

-- %left ident
%left '->'
%left '\\'
%left '('
%left ','

%%

name :: { Ast.Name }
    : ident     { (\(L.TIdent name) -> name) $1 }


expr :: { Ast.Expr }
    : int                                   { (\(L.TInt int) -> Ast.ELit (Ast.LInt int)) $1 }
    | bool                                  { (\(L.TBool bool) -> Ast.ELit (Ast.LBool bool)) $1 }
    | name                                  { Ast.EVar $1 }
    | '\\' name '->' expr                   { Ast.EAbs $2 $4 }
    | expr expr                             { Ast.EApp $1 $2 }
    | let name '=' expr in expr             { Ast.ELet $2 $4 $6 }
    | '(' exprs ')'                         { Ast.ETuple $2 }
    | '(' expr ')'                          { $2 }

exprs_rev
    : exprs ',' expr    { $3 : $1 }
    | expr              { [$1] }

exprs
    : exprs_rev         { reverse $1 }

{
parseError tok = error $ "parser error on: " <> show tok
}
