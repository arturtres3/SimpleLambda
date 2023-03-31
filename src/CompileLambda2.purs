module CompileLambda2 where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Structures (BinopCode(..), Term(..), TermType(..), UnopCode(..), listTermsUsed, makeNatural)
import TypeSystem (Env, emptyEnv, typeInfer, update)
import TermLibrary (eqTerm, neTerm, gtTerm, ltTerm, subTerm)


makeTypesL2 :: Maybe TermType -> String
makeTypesL2 t = case t of
                Just Bool -> "(forall C,C->C->C)"
                Just Nat  -> "(forall C, (C -> C) -> C -> C)"

                Just (Pair t1 t2) -> "(forall C, (" 
                                        <> makeTypesL2 (Just t1) <> " -> "
                                        <> makeTypesL2 (Just t2) <> " -> C) -> C) " 


                Just (Func t1 t2) -> "(" <> makeTypesL2 (Just t1) <> "->" <> makeTypesL2 (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"

makeTypesL2Defs :: Maybe TermType -> String
makeTypesL2Defs t = case t of
                Just Bool -> "Bool"
                Just Nat  -> "Nat"

                Just (Pair t1 t2) -> "(forall C, (" 
                                        <> makeTypesL2Defs (Just t1) <> " -> "
                                        <> makeTypesL2Defs (Just t2) <> " -> C) -> C) " 


                Just (Func t1 t2) -> "(" <> makeTypesL2Defs (Just t1) <> "->" <> makeTypesL2Defs (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"


termToL2 ∷ Term -> Env -> String
termToL2 expr env = case expr of
            T_true  -> "(\\\\C.\\a:C.\\b:C.a)"
            T_false -> "(\\\\C.\\a:C.\\b:C.b)"
            T_var id -> "x_" <> id
            T_num n -> "(\\\\C.\\f:C->C.\\x:C." <> (makeNatural n) <> ")"

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesL2 (Just t_var) <> ". " 
                                  <> termToL2 e1 (update env id t_var) <> ")"

            T_func_system id t_var e1 -> "(\\" <> id <> ": " <> makeTypesL2 (Just t_var) <> ". " 
                                  <> termToL2 e1 (update env id t_var) <> ")"
            T_var_system id -> id

            T_app e1 e2 -> "(" <> termToL2 e1 env <> " " <> termToL2 e2 env <> ")"

            T_let id t_var e1 e2 -> termToL2 (T_app (T_func id t_var e2) e1) env


            (T_if e1 e2 e3) ->   "((\\\\D.\\c:(forall C,C->C->C).\\a:D.\\b:D.(c[D]) a b)" 
                                <> " [" 
                                <> (makeTypesL2 $ typeInfer env e2)
                                <> "] " 
                                <> termToL2 e1 env <> " "
                                <> termToL2 e2 env <> " "
                                <> termToL2 e3 env <> ")"

            (T_pair e1 e2) -> "((\\\\A. \\\\B. \\a: A. \\b: B. \\\\C. \\f: A->B->C. f a b)"
                              <> " ["
                              <> (makeTypesL2 $ typeInfer env e1)
                              <> "]["
                              <> (makeTypesL2 $ typeInfer env e2)
                              <> "] "
                              <> termToL2 e1 env <> " "
                              <> termToL2 e2 env <> ")"
                  
            T_fst e1 ->  "((\\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [A] (\\a: A.\\b: B. a))"
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypesL2 $ Just t1) <> "][" <> (makeTypesL2 $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToL2 e1 env <> ")"
                    
            T_snd e1 -> "((\\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [B] (\\a: A.\\b: B. b))"
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypesL2 $ Just t1) <> "][" <> (makeTypesL2 $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToL2 e1 env <> ")"


            T_binop Add e1 e2 -> "((\\n: (forall C, (C -> C) -> C -> C). \\m: (forall C, (C -> C) -> C -> C). \\\\C. \\f: C -> C. \\x :C. m [C] f (n [C] f x)) "
                                <> termToL2 e1 env <> " "
                                <> termToL2 e2 env <> ")"
            
            T_binop Mult e1 e2 -> "((\\n: (forall C, (C -> C) -> C -> C). \\m: (forall C, (C -> C) -> C -> C). \\\\C. \\f: C -> C. \\x :C. n [C] (m [C] f) x) "
                                <> termToL2 e1 env <> " "
                                <> termToL2 e2 env <> ")"
            
            T_binop And e1 e2 -> "((\\c:(forall C,C->C->C).\\a:(forall C,C->C->C).\\b:(forall C,C->C->C). (c[(forall C,C->C->C)]) a b)"
                                <> termToL2 e1 env <> " "
                                <> termToL2 e2 env <> " "
                                <> "(\\\\C.\\a:C.\\b:C.b))"

            T_binop Or e1 e2 -> "((\\c:(forall C,C->C->C).\\a:(forall C,C->C->C).\\b:(forall C,C->C->C). (c[(forall C,C->C->C)]) a b)"
                                <> termToL2 e1 env <> " "
                                <> "(\\\\C.\\a:C.\\b:C.a) "
                                <> termToL2 e2 env <> ")"

            T_unop Not e1 -> "((\\c:(forall C,C->C->C).\\a:(forall C,C->C->C).\\b:(forall C,C->C->C). (c[(forall C,C->C->C)]) a b)"
                                <> termToL2 e1 env <> " "
                                <> "(\\\\C.\\a:C.\\b:C.b) "
                                <> "(\\\\C.\\a:C.\\b:C.a))"

--             T_binop Sub e1 e2 -> "((\\n: " <> makeTypesL2 (Just Nat) <> ". \\m: " <> makeTypesL2 (Just Nat) <> ". "
--                                 <> "m [" <> makeTypesL2 (Just Nat) <> "] "
-- 
--                                 -- predecessor
--                                 <> "(\\n: " <> makeTypesL2 (Just Nat) <> ". "
--                                 <> "(\\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [A] (\\a: A.\\b: B. a))" -- fst
--                                 <> "[" <> makeTypesL2 (Just Nat) <> "][" <> makeTypesL2 (Just Nat) <> "]"
--                                 <> "( n [" <> makeTypesL2 (Just (Pair Nat Nat)) <> "]"
--                                 <> termToL2 shiftIncTerm env 
--                                 <> termToL2 (T_pair (T_num 0) (T_num 0)) env 
--                                 <> "))" 
-- 
--                                 <> " n)"
--                                 <> termToL2 e1 env <> " "
--                                 <> termToL2 e2 env <> ")"

            T_binop Sub e1 e2 -> "(" <> termToL2 subTerm env <> " "
                                    <> termToL2 e1 env <> " "
                                    <> termToL2 e2 env <> ")"
            
            T_binop Eq e1 e2 -> "(" <> termToL2 eqTerm env <> " "
                                    <> termToL2 e1 env <> " "
                                    <> termToL2 e2 env <> ")"

            T_binop Ne e1 e2 -> "(" <> termToL2 neTerm env <> " "
                                    <> termToL2 e1 env <> " "
                                    <> termToL2 e2 env <> ")"

            T_binop Gt e1 e2 -> "(" <> termToL2 gtTerm env <> " "
                                    <> termToL2 e1 env <> " "
                                    <> termToL2 e2 env <> ")"

            T_binop Lt e1 e2 -> "(" <> termToL2 ltTerm env <> " "
                                    <> termToL2 e1 env <> " "
                                    <> termToL2 e2 env <> ")"

            T_natRec e1 e2 e3 -> "(" <> termToL2 e1 env
                                <> " [" <> (makeTypesL2 $ typeInfer env e3) <> "] "
                                <> termToL2 e2 env <> " "
                                <> termToL2 e3 env <> ")"


            _ -> "INCOMPLETO"


termToL2Defs ∷ Term -> Env -> String
termToL2Defs expr env = case expr of
            T_true  -> "true"
            T_false -> "false"
            T_var id -> "x_" <> id
            T_num n -> toStringAs decimal n 

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesL2Defs (Just t_var) <> ". " 
                                  <> termToL2Defs e1 (update env id t_var) <> ")"

            T_app e1 e2 -> "(" <> termToL2Defs e1 env <> " " <> termToL2Defs e2 env <> ")"

            T_let id t_var e1 e2 -> termToL2Defs (T_app (T_func id t_var e2) e1) env


            (T_if e1 e2 e3) ->   "(if " 
                                <> "[" 
                                <> (makeTypesL2Defs $ typeInfer env e2)
                                <> "] " 
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> " "
                                <> termToL2Defs e3 env <> ")"

            (T_pair e1 e2) -> "(pair "
                              <> "["
                              <> (makeTypesL2Defs $ typeInfer env e1)
                              <> "]["
                              <> (makeTypesL2Defs $ typeInfer env e2)
                              <> "] "
                              <> termToL2Defs e1 env <> " "
                              <> termToL2Defs e2 env <> ")"
                  
            T_fst e1 ->  "(fst "
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> "[" <> (makeTypesL2Defs $ Just t1) <> "][" <> (makeTypesL2Defs $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToL2Defs e1 env <> ")"
                    
            T_snd e1 -> "(snd "
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> "[" <> (makeTypesL2Defs $ Just t1) <> "][" <> (makeTypesL2Defs $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToL2Defs e1 env <> ")"


            T_binop Add e1 e2 -> "(add "
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> ")"
            
            T_binop Mult e1 e2 -> "(mult "
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> ")"
            
            T_binop And e1 e2 -> "(and "
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> ")"

            T_binop Or e1 e2 -> "(or "
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> ")"

            T_unop Not e1 -> "(not "
                                <> termToL2Defs e1 env <> ")"

            T_binop Sub e1 e2 -> "(sub "
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> ")"

            T_binop Eq e1 e2 -> "(eq "
                                    <> termToL2Defs e1 env <> " "
                                    <> termToL2Defs e2 env <> ")"
            
            T_binop Ne e1 e2 -> "(ne "
                                    <> termToL2Defs e1 env <> " "
                                    <> termToL2Defs e2 env <> ")"

            T_binop Gt e1 e2 -> "(gt "
                                    <> termToL2Defs e1 env <> " "
                                    <> termToL2Defs e2 env <> ")"

            T_binop Lt e1 e2 -> "(lt "
                                    <> termToL2Defs e1 env <> " "
                                    <> termToL2Defs e2 env <> ")"

            T_natRec e1 e2 e3 -> "(natRec ["
                                <> (makeTypesL2Defs $ typeInfer env e3) <> "] "
                                <> termToL2Defs e1 env <> " "
                                <> termToL2Defs e2 env <> " "
                                <> termToL2Defs e3 env <> ")"

            _ -> "INCOMPLETO"


makeDefL2 :: String -> String
makeDefL2 str = case str of 
    "true"  -> "  true    = \\\\C. \\a: C. \\b: C. a;"
    "false" -> "  false   = \\\\C. \\a: C. \\b: C. b;"
    "if"    -> "  if      = \\\\D. \\c: Bool. \\a: D. \\b: D. c [D] a b;"
    "pair"  -> "  pair    = \\\\A. \\\\B. \\a: A. \\b: B. \\\\C. \\f: A->B->C. f a b;"
    "fst"   -> "  fst     = \\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [A] (\\a: A.\\b: B. a);"
    "snd"   -> "  snd     = \\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [B] (\\a: A.\\b: B. b);"

    "add"   -> "  add     = \\n: Nat. \\m: Nat. \\\\C. \\f: C -> C. \\x :C. m [C] f (n [C] f x);"
    "mult"  -> "  mult    = \\n: Nat. \\m: Nat. \\\\C. \\f: C -> C. \\x :C. n [C] (m [C] f) x;"
    "and"   -> "  and     = \\a: Bool. \\b: Bool. a [Bool] b (\\\\C. \\a: C. \\b: C. b);"
    "or"    -> "  or      = \\a: Bool. \\b: Bool. a [Bool] (\\\\C. \\a: C. \\b: C. a) b;"
    "not"   -> "  not     = \\a: Bool. a [Bool] (\\\\C. \\a: C. \\b: C. b) (\\\\C. \\a: C. \\b: C. a);"

    "succ"  -> "  succ    = \\n: Nat. \\\\C. \\f: C -> C. \\x :C. f (n [C] f x);"
    "sub"   -> "  sub     = \\n: Nat. \\m:Nat. m [Nat] (\\n: Nat. fst [Nat] [Nat] (n [(forall C, (Nat -> Nat -> C) -> C)] (\\p: (forall C, (Nat -> Nat -> C) -> C). (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)))) (pair [Nat] [Nat] 0 0))) n;"

    "isZero"-> "  isZero  = \\n:Nat. n [Bool] (\\b: Bool. (\\\\C. \\a: C. \\b: C. b)) (\\\\C. \\a: C. \\b: C. a);"

    "eq"    -> "  eq     = \\n:Nat. \\m:Nat. and (isZero (sub n m)) (isZero (sub m n));"
    "ne"    -> "  ne     = \\n:Nat. \\m:Nat. not (and (isZero (sub n m)) (isZero (sub m n)));"

    "gt"    -> "  gt     = \\n:Nat. \\m:Nat. not (isZero (sub n m)) ;"
    "lt"    -> "  lt     = \\n:Nat. \\m:Nat. not (isZero (sub m n)) ;"

    "natRec"-> "  natRec  = \\\\C. \\n:Nat. \\step: C -> C. \\init:C. n [C] step init;"

    _       -> "?"


makeDefsUsed :: (List String) -> String 
makeDefsUsed Nil = "\n"
makeDefsUsed (str : tail) = makeDefL2 str <> "\n" <> makeDefsUsed tail

makeDefsBlock :: (List String) -> String 
makeDefsBlock l =  "typedef\n"
    <>   "  Bool          = forall C, C -> C -> C;\n"
    <>   "  Nat           = forall C, (C -> C) -> C -> C;\n"

    <> "end\n"
    <> "let\n"

    <> makeDefsUsed l 

    <> "in\n\n"


makeL2 :: Term → String
makeL2 expr = case typeInfer emptyEnv expr of 
                    Just _ -> termToL2 expr emptyEnv
                    Nothing -> if (expr == T_error) then "Sintaxe Incorreta" else "Erro de Tipo"

makeL2Defs :: Term → String
makeL2Defs expr = case typeInfer emptyEnv expr of 
                    Just _ -> makeDefsBlock (listTermsUsed expr Nil) <> termToL2Defs expr emptyEnv
                    Nothing -> if (expr == T_error) then "Sintaxe Incorreta" else "Erro de Tipo"