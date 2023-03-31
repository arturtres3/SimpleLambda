module Structures where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:), union)

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

            | T_natRec Term Term Term
            | T_var Ident
            | T_func Ident TermType Term
            | T_app Term Term
            | T_let Ident TermType Term Term

            | T_func_system Ident TermType Term
            | T_var_system Ident

            | T_error

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

            T_natRec e1 e2 e3 -> "(_natRec " <> showTerm e1  <> " " <> showTerm e2 <> " " <> showTerm e3 <> ")"

            T_func_system ident t1 e1 -> "(_func " <> ident <> " " <> showType t1 <> " " <> showTerm e1 <> ")"
            T_var_system indent-> "(_var " <> indent <> ")" 

            T_error -> "ERRO" 



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


makeNatural:: Int -> String 
makeNatural 0 = "x"
makeNatural n = "(f " <> (makeNatural (n-1)) <> ")"

listTermsUsed :: Term -> (List String) -> List String 
listTermsUsed expr l = case expr of 
    T_true          -> union ("true":Nil) l
    T_false         -> union ("false":Nil) l
    T_if e1 e2 e3   -> union (union (union (union ("if":Nil) l) (listTermsUsed e1 l))(listTermsUsed e2 l )) (listTermsUsed e3 l)
    T_natRec e1 e2 e3   -> union (union (union (union ("natRec":Nil) l) (listTermsUsed e1 l))(listTermsUsed e2 l )) (listTermsUsed e3 l)

    T_pair e1 e2    -> union (union (union ("pair":Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_fst e1        -> union (union ("fst":Nil) l) (listTermsUsed e1 l)
    T_snd e1        -> union (union ("snd":Nil) l) (listTermsUsed e1 l)

    T_func _ _ e1   -> union (listTermsUsed e1 l) l 
    T_app e1 e2     -> union (union (listTermsUsed e1 l) l) (listTermsUsed e2 l)
    T_let _ _ e1 e2 -> union (union (listTermsUsed e1 l) l) (listTermsUsed e2 l)

    T_unop Not e1      -> union (union ("not":Nil) l) (listTermsUsed e1 l)
    T_binop Add e1 e2  -> union (union (union ("add" :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop And e1 e2  -> union (union (union ("and" :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Or  e1 e2  -> union (union (union ("or"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Div e1 e2  -> union (union (union ("div" :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Mult e1 e2 -> union (union (union ("mult":Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )

    T_binop Eq  e1 e2  -> union (union (union ("isZero":"pair":"fst":"snd":"succ":"sub":"and":"eq"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Ne  e1 e2  -> union (union (union ("isZero":"pair":"fst":"snd":"succ":"sub":"and":"not":"ne"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Gt  e1 e2  -> union (union (union ("isZero":"pair":"fst":"snd":"succ":"sub":"not":"gt"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Lt  e1 e2  -> union (union (union ("isZero":"pair":"fst":"snd":"succ":"sub":"not":"lt"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )

    T_binop Sub e1 e2  -> union (union (union ("pair":"fst":"snd":"succ":"sub":Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    _                  -> l