module Structures where

import Prelude

import Data.Int (decimal, toStringAs)

type Ident = String

data TermType   = Nat
                | Bool
                | Pair TermType TermType
                | Func TermType TermType

data BinopCode = Add | Sub | Mult | Div | Lt | Gt | Eq | Ne | And | Or
data UnopCode = Not | Negate

data Term   = T_true
            | T_false
            | T_num Int
            | T_if Term Term Term
            | T_pair Term Term
            | T_fst Term
            | T_snd Term

            | T_binop BinopCode Term Term
            | T_unop UnopCode Term

            | T_var Ident
            | T_func Ident TermType Term
            | T_app Term Term
            | T_let Ident TermType Term Term

instance showTerm' :: Show Term where
    show = showTerm 

instance showType' :: Show TermType where
    show = showType 


derive instance eqTermType :: Eq TermType
derive instance eqTerm :: Eq Term
derive instance eqBinop :: Eq BinopCode
derive instance eqUnop :: Eq UnopCode


showTerm ∷ Term → String
showTerm t = case t of
            T_true -> "_true"
            T_false -> "_false"
            T_num v -> "(_num " <> (toStringAs decimal v) <> ")"
            T_fst e1 -> "(_fst " <> showTerm e1 <> ")"
            T_snd e1 -> "(_snd " <> showTerm e1 <> ")"
            T_pair e1 e2 -> "(_pair " <> showTerm e1 <> ", " <> showTerm e2 <> ")"
            T_if t1 t2 t3 -> "(_if " <> 
                            showTerm t1 <> " " <>
                            showTerm t2 <> " " <>
                            showTerm t3 <> ")"

            T_var indent-> "(_var " <> indent <> ")" 
            T_app e1 e2 -> "(_app " <> showTerm e1 <> " " <> showTerm e2 <> ")"              
            T_func ident t1 e1 -> "(_func " <> ident <> " " <> showType t1 <> " " <> showTerm e1 <> ")"
            T_let ident t1 e1 e2 -> "(_let " <> ident <> " " <> showType t1 
                                    <> " " <> showTerm e1 
                                    <> " in " <> showTerm e2 <> ")"

            T_binop code e1 e2 -> "(_binOp " <> show code <> " " <> showTerm e1 <> " " <> showTerm e2 <> ")"
            T_unop code e1-> "(_unOp " <> show code <> " " <> showTerm e1 <> ")"



showType ∷ TermType → String
showType t = case t of
            Nat -> "Nat"
            Bool -> "Bool"
            Pair t1 t2 -> "(" <> showType t1 <> " X " <> showType t2 <> ")"
            Func t1 t2 -> "(" <> showType t1 <> " -> " <> showType t2 <> ")"

instance showBinop :: Show BinopCode where
  show Add = " + "
  show Sub = " - "
  show Mult = "*"
  show Div = "/"
  show Lt = " < "
  show Gt = " > "
  show Eq = " == "
  show Ne = " != "
  show Or = " || "
  show And = " && "

instance showUnop :: Show UnopCode where
  show Negate = "-"
  show Not = "~"

