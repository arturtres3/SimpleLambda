module CompileOmega where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Structures (BinopCode(..), Term(..), TermType(..), UnopCode(..), listTermsUsed, makeNatural)
import TermLibrary (eqTerm, neTerm, gtTerm, ltTerm, subTerm)
import TypeSystem (Env, emptyEnv, typeInfer, update)
      

makeTypesOmega :: Maybe TermType -> String
makeTypesOmega t = case t of
                Just Bool -> "(forall C:*,C->C->C)"
                Just Nat  -> "(forall C:*, (C -> C) -> C -> C)"

                Just (Pair t1 t2) -> "((\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) " 
                                      <> makeTypesOmega (Just t1) <> " "
                                      <> makeTypesOmega (Just t2) <> 
                                      ")"

                Just (Func t1 t2) -> "(" <> makeTypesOmega (Just t1) <> "->" <> makeTypesOmega (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"

makeTypesOmegaDefs :: Maybe TermType -> String
makeTypesOmegaDefs t = case t of
                Just Bool -> "Bool"
                Just Nat  -> "Nat"

                Just (Pair t1 t2) -> "(Pair " 
                                      <> makeTypesOmegaDefs (Just t1) <> " "
                                      <> makeTypesOmegaDefs (Just t2) <> 
                                      ")"

                Just (Func t1 t2) -> "(" <> makeTypesOmegaDefs (Just t1) <> "->" <> makeTypesOmegaDefs (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"


termToOmega ∷ Term -> Env -> String
termToOmega expr env = case expr of
            T_true  -> "(\\\\C:*.\\a:C.\\b:C.a)"
            T_false -> "(\\\\C:*.\\a:C.\\b:C.b)"
            T_var id -> "x_" <> id
            T_num n -> "(\\\\C:*.\\f:C->C.\\x:C." <> (makeNatural n) <> ")"

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesOmega (Just t_var) <> ". " 
                                  <> termToOmega e1 (update env id t_var) <> ")"

            T_func_system id t_var e1 -> "(\\" <> id <> ": " <> makeTypesOmega (Just t_var) <> ". " 
                                  <> termToOmega e1 (update env id t_var) <> ")"
            T_var_system id -> id

            T_app e1 e2 -> "(" <> termToOmega e1 env <> " " <> termToOmega e2 env <> ")"

            T_let id t_var e1 e2 -> termToOmega (T_app (T_func id t_var e2) e1) env


            (T_if e1 e2 e3) ->   "((\\\\D:*.\\c:(forall C:*,C->C->C).\\a:D.\\b:D.(c[D]) a b)" 
                                <> " [" 
                                <> (makeTypesOmega $ typeInfer env e2)
                                <> "] " 
                                <> termToOmega e1 env <> " "
                                <> termToOmega e2 env <> " "
                                <> termToOmega e3 env <> ")"

            (T_pair e1 e2) -> "((\\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b)"
                              <> " ["
                              <> (makeTypesOmega $ typeInfer env e1)
                              <> "]["
                              <> (makeTypesOmega $ typeInfer env e2)
                              <> "] "
                              <> termToOmega e1 env <> " "
                              <> termToOmega e2 env <> ")"
                  
            T_fst e1 ->  "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))"
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypesOmega $ Just t1) <> "][" <> (makeTypesOmega $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToOmega e1 env <> ")"
                    
            T_snd e1 -> "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [B] (\\a: A.\\b: B. b))"
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypesOmega $ Just t1) <> "][" <> (makeTypesOmega $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToOmega e1 env <> ")"


            T_binop Add e1 e2 -> "((\\n: (forall C:*, (C -> C) -> C -> C). \\m: (forall C:*, (C -> C) -> C -> C). \\\\C:*. \\f: C -> C. \\x :C. m [C] f (n [C] f x)) "
                                <> termToOmega e1 env <> " "
                                <> termToOmega e2 env <> ")"
            
            T_binop Mult e1 e2 -> "((\\n: (forall C:*, (C -> C) -> C -> C). \\m: (forall C:*, (C -> C) -> C -> C). \\\\C:*. \\f: C -> C. \\x :C. n [C] (m [C] f) x) "
                                <> termToOmega e1 env <> " "
                                <> termToOmega e2 env <> ")"
            
            T_binop And e1 e2 -> "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)"
                                <> termToOmega e1 env <> " "
                                <> termToOmega e2 env <> " "
                                <> "(\\\\C:*.\\a:C.\\b:C.b))"

            T_binop Or e1 e2 -> "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)"
                                <> termToOmega e1 env <> " "
                                <> "(\\\\C:*.\\a:C.\\b:C.a) "
                                <> termToOmega e2 env <> ")"

            T_unop Not e1 -> "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)"
                                <> termToOmega e1 env <> " "
                                <> "(\\\\C:*.\\a:C.\\b:C.b) "
                                <> "(\\\\C:*.\\a:C.\\b:C.a))"

--             T_binop Sub e1 e2 -> "((\\n: " <> makeTypesOmega (Just Nat) <> ". \\m: " <> makeTypesOmega (Just Nat) <> ". "
--                                 <> "m [" <> makeTypesOmega (Just Nat) <> "] "
-- 
--                                 -- predecessor
--                                 <> "(\\n: " <> makeTypesOmega (Just Nat) <> ". "
--                                 <> "(\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))" -- fst
--                                 <> "[" <> makeTypesOmega (Just Nat) <> "][" <> makeTypesOmega (Just Nat) <> "]"
--                                 <> "( n [" <> makeTypesOmega (Just (Pair Nat Nat)) <> "]"
--                                 <> termToOmega shiftIncTerm env 
--                                 <> termToOmega (T_pair (T_num 0) (T_num 0)) env 
--                                 <> "))" 
-- 
--                                 <> " n)"
--                                 <> termToOmega e1 env <> " "
--                                 <> termToOmega e2 env <> ")"

            T_binop Sub e1 e2 -> "(" <> termToOmega subTerm env <> " "
                                    <> termToOmega e1 env <> " "
                                    <> termToOmega e2 env <> ")"

            T_binop Eq e1 e2 -> "(" <> termToOmega eqTerm env <> " "
                                    <> termToOmega e1 env <> " "
                                    <> termToOmega e2 env <> ")"

            T_binop Ne e1 e2 -> "(" <> termToOmega neTerm env <> " "
                                    <> termToOmega e1 env <> " "
                                    <> termToOmega e2 env <> ")"

            T_binop Gt e1 e2 -> "(" <> termToOmega gtTerm env <> " "
                                    <> termToOmega e1 env <> " "
                                    <> termToOmega e2 env <> ")"

            T_binop Lt e1 e2 -> "(" <> termToOmega ltTerm env <> " "
                                    <> termToOmega e1 env <> " "
                                    <> termToOmega e2 env <> ")"

            T_natRec e1 e2 e3 -> "(" <> termToOmega e1 env
                                <> " [" <> (makeTypesOmega $ typeInfer env e3) <> "] "
                                <> termToOmega e2 env <> " "
                                <> termToOmega e3 env <> ")"


            _ -> "INCOMPLETO"


termToOmegaDefs ∷ Term -> Env -> String
termToOmegaDefs expr env = case expr of
            T_true  -> "true"
            T_false -> "false"
            T_var id -> "x_" <> id
            T_num n -> toStringAs decimal n

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesOmegaDefs (Just t_var) <> ". " 
                                  <> termToOmegaDefs e1 (update env id t_var) <> ")"

            T_app e1 e2 -> "(" <> termToOmegaDefs e1 env <> " " <> termToOmegaDefs e2 env <> ")"

            T_let id t_var e1 e2 -> termToOmegaDefs (T_app (T_func id t_var e2) e1) env


            (T_if e1 e2 e3) ->   "(if " 
                                <> " [" 
                                <> (makeTypesOmegaDefs $ typeInfer env e2)
                                <> "] " 
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> " "
                                <> termToOmegaDefs e3 env <> ")"

            (T_pair e1 e2) -> "(pair "
                              <> " ["
                              <> (makeTypesOmegaDefs $ typeInfer env e1)
                              <> "]["
                              <> (makeTypesOmegaDefs $ typeInfer env e2)
                              <> "] "
                              <> termToOmegaDefs e1 env <> " "
                              <> termToOmegaDefs e2 env <> ")"
                  
            T_fst e1 ->  "(fst "
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypesOmegaDefs $ Just t1) <> "][" <> (makeTypesOmegaDefs $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToOmegaDefs e1 env <> ")"
                    
            T_snd e1 -> "(snd "
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " [" <> (makeTypesOmegaDefs $ Just t1) <> "][" <> (makeTypesOmegaDefs $ Just t2) <> "] "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToOmegaDefs e1 env <> ")"


            T_binop Add e1 e2 -> "(add "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"
            
            T_binop Mult e1 e2 -> "(mult "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"
            
            T_binop And e1 e2 -> "(and "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_binop Or e1 e2 -> "(or "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_unop Not e1 -> "(not "
                                <> termToOmegaDefs e1 env <> ")"

            T_binop Sub e1 e2 -> "(sub "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_binop Eq e1 e2 -> "(eq "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_binop Ne e1 e2 -> "(ne "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_binop Gt e1 e2 -> "(gt "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_binop Lt e1 e2 -> "(lt "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            T_natRec e1 e2 e3 -> "(natRec ["
                                <> (makeTypesOmegaDefs $ typeInfer env e3) <> "] "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> " "
                                <> termToOmegaDefs e3 env <> ")"

            _ -> "INCOMPLETO"

makeDefOmega :: String -> String
makeDefOmega str = case str of 
    "true"  -> "  true    = \\\\C:*. \\a: C. \\b: C. a;"
    "false" -> "  false   = \\\\C:*. \\a: C. \\b: C. b;"
    "if"    -> "  if      = \\\\D:*. \\c: Bool. \\a: D. \\b: D. c [D] a b;"
    "pair"  -> "  pair    = \\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b;"
    "fst"   -> "  fst     = \\\\A:*. \\\\B:*. \\p: Pair A B. p [A] (\\a: A.\\b: B. a);"
    "snd"   -> "  snd     = \\\\A:*. \\\\B:*. \\p: Pair A B. p [B] (\\a: A.\\b: B. b);"

    "add"   -> "  add     = \\n: Nat. \\m: Nat. \\\\C:*. \\f: C -> C. \\x :C. m [C] f (n [C] f x);"
    "mult"  -> "  mult    = \\n: Nat. \\m: Nat. \\\\C:*. \\f: C -> C. \\x :C. n [C] (m [C] f) x;"
    "and"   -> "  and     = \\a: Bool. \\b: Bool. a [Bool] b (\\\\C:*. \\a: C. \\b: C. b);"
    "or"    -> "  or      = \\a: Bool. \\b: Bool. a [Bool] (\\\\C:*. \\a: C. \\b: C. a) b;"
    "not"   -> "  not     = \\a: Bool. a [Bool] (\\\\C:*. \\a: C. \\b: C. b) (\\\\C:*. \\a: C. \\b: C. a);"

    "succ"  -> "  succ    = \\n: Nat. \\\\C:*. \\f: C -> C. \\x :C. f (n [C] f x);"
    "sub"   -> "  sub     = \\n: Nat. \\m:Nat. m [Nat] (\\n: Nat. fst [Nat] [Nat] (n [Pair Nat Nat] (\\p: Pair Nat Nat. (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)))) (pair [Nat] [Nat] 0 0))) n;"

    "isZero"-> "  isZero  = \\n:Nat. n [Bool] (\\b: Bool. (\\\\C:*. \\a: C. \\b: C. b)) (\\\\C:*. \\a: C. \\b: C. a);"

    "eq"    -> "  eq     = \\n:Nat. \\m:Nat. and (isZero (sub n m)) (isZero (sub m n));"
    "ne"    -> "  ne     = \\n:Nat. \\m:Nat. not (and (isZero (sub n m)) (isZero (sub m n)));"

    "gt"    -> "  gt     = \\n:Nat. \\m:Nat. not (isZero (sub n m)) ;"
    "lt"    -> "  lt     = \\n:Nat. \\m:Nat. not (isZero (sub m n)) ;"

    "natRec"-> "  natRec  = \\\\C:*. \\n:Nat. \\step: C -> C. \\init:C. n [C] step init;"

    _ -> "?"



makeDefsUsed :: (List String) -> String 
makeDefsUsed Nil = "\n"
makeDefsUsed (str : tail) = makeDefOmega str <> "\n" <> makeDefsUsed tail

makeDefsBlock :: (List String) -> String 
makeDefsBlock l =  "typedef\n"
    <>   "  Bool    = forall C:*, C -> C -> C;\n"
    <>   "  Nat     = forall C:*, (C -> C) -> C -> C;\n"
    <>   "  Pair    = \\A:*, \\B:*, forall C:*, (A -> B -> C) -> C;\n"
    -- <>   "  Or            = \\A:*, \\B:*, forall C:*, (A -> C) -> (B -> C) -> C;\n"
    <> "end\n"
    <> "let\n"

    <> makeDefsUsed l 

    <> "in\n\n"


makeLOmega :: Term → String
makeLOmega expr = case typeInfer emptyEnv expr of 
                    Just _ -> termToOmega expr emptyEnv
                    Nothing -> if (expr == T_error) then "Sintaxe Incorreta" else "Erro de Tipo"


makeLOmegaDefs :: Term → String
makeLOmegaDefs expr = case typeInfer emptyEnv expr of 
                    Just _ -> makeDefsBlock (listTermsUsed expr Nil) <> termToOmegaDefs expr emptyEnv
                    Nothing -> if (expr == T_error) then "Sintaxe Incorreta" else "Erro de Tipo"



