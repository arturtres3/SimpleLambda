module CompileLambdaC where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Int (decimal, toStringAs)
import Structures (BinopCode(..), Term(..), TermType(..), UnopCode(..), listTermsUsed)
import TypeSystem (Env, emptyEnv, typeInfer, update)


shiftIncTerm :: Term
shiftIncTerm = (T_func "p" (Pair Nat Nat) 
                        (T_pair (T_snd (T_var "p"))  
                        (T_binop  Add  (T_snd (T_var "p")) (T_num 1)))
                    ) 

makeNatural:: Int -> String 
makeNatural 0 = "x"
makeNatural n = "(f " <> (makeNatural (n-1)) <> ")"

makeTypesLC :: Maybe TermType -> String
makeTypesLC t = case t of
                Just Bool -> "(||C:*.C->C->C)"
                Just Nat  -> "(||C:*. (C -> C) -> C -> C)"

                Just (Pair t1 t2) -> "((\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) " 
                                      <> makeTypesLC (Just t1) <> " "
                                      <> makeTypesLC (Just t2) <> 
                                      ")"

                Just (Func t1 t2) -> "(" <> makeTypesLC (Just t1) <> "->" <> makeTypesLC (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"

makeTypesLCDefs :: Maybe TermType -> String
makeTypesLCDefs t = case t of
                Just Bool -> "Bool"
                Just Nat  -> "Nat"

                Just (Pair t1 t2) -> "(And " 
                                      <> makeTypesLCDefs (Just t1) <> " "
                                      <> makeTypesLCDefs (Just t2) <> 
                                      ")"

                Just (Func t1 t2) -> "(" <> makeTypesLCDefs (Just t1) <> "->" <> makeTypesLCDefs (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"


termToLC ∷ Term -> Env -> String
termToLC expr env = case expr of
            T_true  -> "(\\C:*.\\a:C.\\b:C.a)"
            T_false -> "(\\C:*.\\a:C.\\b:C.b)"
            T_var id -> "x_" <> id
            T_num n -> "(\\C:*.\\f:C->C.\\x:C." <> (makeNatural n) <> ")"

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesLC (Just t_var) <> ". " 
                                  <> termToLC e1 (update env id t_var) <> ")"

            T_app e1 e2 -> "(" <> termToLC e1 env <> " " <> termToLC e2 env <> ")"

            T_let id t_var e1 e2 -> termToLC (T_app (T_func id t_var e2) e1) env


            (T_if e1 e2 e3) ->   "((\\D:*.\\c:(|| C:*.C->C->C).\\a:D.\\b:D.(c D) a b)" 
                                <> " " 
                                <> (makeTypesLC $ typeInfer env e2)
                                <> " " 
                                <> termToLC e1 env <> " "
                                <> termToLC e2 env <> " "
                                <> termToLC e3 env <> ")"

            (T_pair e1 e2) -> "((\\A:*. \\B:*. \\a: A. \\b: B. \\C:*. \\f: A->B->C. f a b)"
                              <> " "
                              <> (makeTypesLC $ typeInfer env e1)
                              <> " "
                              <> (makeTypesLC $ typeInfer env e2)
                              <> " "
                              <> termToLC e1 env <> " "
                              <> termToLC e2 env <> ")"
                  
            T_fst e1 ->  "((\\A:*. \\B:*. \\p: (\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) A B. p A (\\a: A.\\b: B. a))"
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " " <> (makeTypesLC $ Just t1) <> " " <> (makeTypesLC $ Just t2) <> " "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToLC e1 env <> ")"
                    
            T_snd e1 -> "((\\A:*. \\B:*. \\p: (\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) A B. p B (\\a: A.\\b: B. b))"
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " " <> (makeTypesLC $ Just t1) <> " " <> (makeTypesLC $ Just t2) <> " "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToLC e1 env <> ")"


            T_binop Add e1 e2 -> "((\\n: (||C:*. (C -> C) -> C -> C). \\m: (||C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. m C f (n C f x)) "
                                <> termToLC e1 env <> " "
                                <> termToLC e2 env <> ")"
            
            T_binop Mult e1 e2 -> "((\\n: (||C:*. (C -> C) -> C -> C). \\m: (||C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. n C (m C f) x) "
                                <> termToLC e1 env <> " "
                                <> termToLC e2 env <> ")"
            
            T_binop And e1 e2 -> "((\\c:(||C:*.C->C->C).\\a:(||C:*.C->C->C).\\b:(||C:*.C->C->C). (c (||C:*.C->C->C)) a b)"
                                <> termToLC e1 env <> " "
                                <> termToLC e2 env <> " "
                                <> "(\\C:*.\\a:C.\\b:C.b))"

            T_binop Or e1 e2 -> "((\\c:(||C:*.C->C->C).\\a:(||C:*.C->C->C).\\b:(||C:*.C->C->C). (c (||C:*.C->C->C)) a b)"
                                <> termToLC e1 env <> " "
                                <> "(\\C:*.\\a:C.\\b:C.a) "
                                <> termToLC e2 env <> ")"

            T_unop Not e1 -> "((\\c:(||C:*.C->C->C).\\a:(||C:*.C->C->C).\\b:(||C:*.C->C->C). (c (||C:*.C->C->C)) a b)"
                                <> termToLC e1 env <> " "
                                <> "(\\C:*.\\a:C.\\b:C.b) "
                                <> "(\\C:*.\\a:C.\\b:C.a))"

            T_binop Sub e1 e2 -> "((\\n: " <> makeTypesLC (Just Nat) <> ". \\m: " <> makeTypesLC (Just Nat) <> ". "
                                <> "m " <> makeTypesLC (Just Nat) <> " "

                                -- predecessor
                                <> "(\\n: " <> makeTypesLC (Just Nat) <> ". "
                                <> "(\\A:*. \\B:*. \\p: (\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) A B. p A (\\a: A.\\b: B. a))" -- fst
                                <> " " <> makeTypesLC (Just Nat) <> "][" <> makeTypesLC (Just Nat) <> " "
                                <> "( n " <> makeTypesLC (Just (Pair Nat Nat)) <> " "
                                <> termToLC shiftIncTerm env 
                                <> termToLC (T_pair (T_num 0) (T_num 0)) env 
                                <> "))" 

                                <> " n)"
                                <> termToLC e1 env <> " "
                                <> termToLC e2 env <> ")"

            _ -> "INCOMPLETO"


termToLCDefs ∷ Term -> Env -> String
termToLCDefs expr env = case expr of
            T_true  -> "true"
            T_false -> "false"
            T_var id -> "x_" <> id
            T_num n -> toStringAs decimal n 

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesLCDefs (Just t_var) <> ". " 
                                  <> termToLCDefs e1 (update env id t_var) <> ")"

            T_app e1 e2 -> "(" <> termToLCDefs e1 env <> " " <> termToLCDefs e2 env <> ")"

            T_let id t_var e1 e2 -> termToLCDefs (T_app (T_func id t_var e2) e1) env


            (T_if e1 e2 e3) ->   "(if " 
                                <> (makeTypesLCDefs $ typeInfer env e2)
                                <> " " 
                                <> termToLCDefs e1 env <> " "
                                <> termToLCDefs e2 env <> " "
                                <> termToLCDefs e3 env <> ")"

            (T_pair e1 e2) -> "(pair "
                              <> (makeTypesLCDefs $ typeInfer env e1)
                              <> " "
                              <> (makeTypesLCDefs $ typeInfer env e2)
                              <> " "
                              <> termToLCDefs e1 env <> " "
                              <> termToLCDefs e2 env <> ")"
                  
            T_fst e1 ->  "(fst "
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " " <> (makeTypesLCDefs $ Just t1) <> " " <> (makeTypesLCDefs $ Just t2) <> " "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToLCDefs e1 env <> ")"
                    
            T_snd e1 -> "(snd "
                      <> (case typeInfer env e1 of 
                          Just (Pair t1 t2) -> " " <> (makeTypesLCDefs $ Just t1) <> " " <> (makeTypesLCDefs $ Just t2) <> " "
                          _ -> "Erro de Tipo [PARES]"
                          )
                      <> termToLCDefs e1 env <> ")"


            T_binop Add e1 e2 -> "(add "
                                <> termToLCDefs e1 env <> " "
                                <> termToLCDefs e2 env <> ")"
            
            T_binop Mult e1 e2 -> "(mult "
                                <> termToLCDefs e1 env <> " "
                                <> termToLCDefs e2 env <> ")"
            
            T_binop And e1 e2 -> "(and "
                                <> termToLCDefs e1 env <> " "
                                <> termToLCDefs e2 env <> ")"

            T_binop Or e1 e2 -> "(or "
                                <> termToLCDefs e1 env <> " "
                                <> termToLCDefs e2 env <> ")"

            T_unop Not e1 -> "(not "
                                <> termToLCDefs e1 env <> ")"

            T_binop Sub e1 e2 -> "(sub "
                                <> termToLCDefs e1 env <> " "
                                <> termToLCDefs e2 env <> ")"

            _ -> "INCOMPLETO"


makeDefLC :: String -> String
makeDefLC str = case str of 
    "true"  -> "  true    = \\C:*. \\a: C. \\b: C. a;"
    "false" -> "  false   = \\C:*. \\a: C. \\b: C. b;"
    "if"    -> "  if      = \\D:*. \\c: Bool. \\a: D. \\b: D. c D a b;"
    "pair"  -> "  pair    = \\A:*. \\B:*. \\a: A. \\b: B. \\C:*. \\f: A->B->C. f a b;"
    "fst"   -> "  fst     = \\A:*. \\B:*. \\p: And A B. p A (\\a: A.\\b: B. a);"
    "snd"   -> "  snd     = \\A:*. \\B:*. \\p: And A B. p B (\\a: A.\\b: B. b);"

    "add"   -> "  add     = \\n: Nat. \\m: Nat. \\C:*. \\f: C -> C. \\x :C. m C f (n C f x);"
    "mult"  -> "  mult    = \\n: Nat. \\m: Nat. \\C:*. \\f: C -> C. \\x :C. n C (m C f) x;"
    "and"   -> "  and     = \\a: Bool. \\b: Bool. a Bool b (\\C:*. \\a: C. \\b: C. b);"
    "or"    -> "  or      = \\a: Bool. \\b: Bool. a Bool (\\C:*. \\a: C. \\b: C. a) b;"
    "not"   -> "  not     = \\a: Bool. a Bool (\\C:*. \\a: C. \\b: C. b) (\\C:*. \\a: C. \\b: C. a);"

    "succ"  -> "  succ    = \\n: Nat. \\C:*. \\f: C -> C. \\x :C. f (n C f x);"
    "sub"   -> "  sub     = \\n: Nat. \\m:Nat. m Nat (\\n: Nat. fst Nat Nat (n (And Nat Nat) (\\p: And Nat Nat. (pair Nat Nat (snd Nat Nat p) (succ (snd Nat Nat p)))) (pair Nat Nat 0 0))) n;"

    _ -> "?"

makeDefsUsed :: (List String) -> String 
makeDefsUsed Nil = "\n"
makeDefsUsed (str : tail) = makeDefLC str <> "\n" <> makeDefsUsed tail

makeDefsBlock :: (List String) -> String 
makeDefsBlock l =  "let\n"
    <>   "  Bool          = ||C:*. C -> C -> C;\n"
    <>   "  Nat           = ||C:*. (C -> C) -> C -> C;\n"
    <>   "  And           = \\A:*. \\B:*. ||C:*. (A -> B -> C) -> C;\n"
    <>   "  Or            = \\A:*. \\B:*. ||C:*. (A -> C) -> (B -> C) -> C;\n\n"

    <> makeDefsUsed l 

    <> "in\n\n"

makeLC :: Term → String
makeLC expr = case typeInfer emptyEnv expr of 
                    Just _ -> termToLC expr emptyEnv
                    Nothing -> "Erro de Tipo"

makeLCDefs :: Term → String
makeLCDefs expr = case typeInfer emptyEnv expr of 
                    Just _ -> makeDefsBlock (listTermsUsed expr Nil) <> termToLCDefs expr emptyEnv
                    Nothing -> "Erro de Tipo"
