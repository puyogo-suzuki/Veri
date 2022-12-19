module Veri.AST where

type VariableIdent = String

data Epsilon =
    Variable VariableIdent
    | Asterisk
    | Sort
    | App Epsilon Epsilon
    | Lambda VariableIdent Epsilon Epsilon
    | Pi VariableIdent Epsilon Epsilon
    | Const String [Epsilon]
    deriving Show