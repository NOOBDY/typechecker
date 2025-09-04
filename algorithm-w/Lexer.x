-- vim: set ft=haskell

{
module Lexer
    ( alexScanTokens
    , Token (..)
    ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%wrapper "basic-bytestring"

$white = [\ \t\n\r]
$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [$alpha $digit \_]

tokens :-

$white+                 ;
"--".*                  ;
True                    { \_ -> TBool True }
False                   { \_ -> TBool False }
let                     { \_ -> TLet }
in                      { \_ -> TIn }
"->"                    { \_ -> TArrow }
"\"                     { \_ -> TLambda }
"("                     { \_ -> TLPar }
")"                     { \_ -> TRPar }
","                     { \_ -> TComma }
"="                     { \_ -> TEquals }
$digit+                 { \s -> TInt (read $ BS.unpack s) }
$alpha [$alphanum]*     { \s -> TIdent s }

{
data Token
    = TIdent ByteString
    | TInt Int
    | TBool Bool
    | TLet
    | TIn
    | TArrow
    | TLambda
    | TLPar
    | TRPar
    | TComma
    | TEquals
    deriving (Show, Eq)
}
