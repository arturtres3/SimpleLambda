module CompileLambda where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Structures (BinopCode(..), Term(..), UnopCode(..), listTermsUsed, makeNatural, makeDefsUsed)
import TermLibrary (eqTerm, neTerm, gtTerm, ltTerm, subTerm)
import TypeSystem (emptyEnv, typeInfer)


termToLambda :: Term -> String 
termToLambda expr = case expr of 
        T_true      -> "(\\a.\\b.a)"
        T_false     -> "(\\a.\\b.b)"
        T_var id -> "x_" <> id
        T_num n -> "(\\f.\\x." <> (makeNatural n) <> ")"

        T_func id _ e1 -> "(\\x_" <> id <> ". " 
                                  <> termToLambda e1 <> ")"

        T_func_system id _ e1 -> "(\\" <> id <> ". " 
                                  <> termToLambda e1 <> ")"
        T_var_system id -> id

        T_app e1 e2 -> "(" <> termToLambda e1 <> " " <> termToLambda e2 <> ")"

        T_let id t_var e1 e2 -> termToLambda (T_app (T_func id t_var e2) e1) 

        (T_if e1 e2 e3) ->   "((\\c.\\a.\\b. c a b) " 
                    <> termToLambda e1 <> " "
                    <> termToLambda e2 <> " "
                    <> termToLambda e3 <> ")"

        (T_pair e1 e2) -> "((\\a. \\b. \\c. c a b) "
                              <> termToLambda e1 <> " "
                              <> termToLambda e2 <> ")"
                  
        T_fst e1 ->  "((\\p. p (\\a.\\b. a)) "
                      <> termToLambda e1 <> ")"
                    
        T_snd e1 ->  "((\\p. p (\\a.\\b. b))     "
                      <> termToLambda e1 <> ")"
        
        T_binop Add e1 e2 -> "((\\n. \\m. \\f. \\x. m f (n f x)) "
                                <> termToLambda e1 <> " "
                                <> termToLambda e2 <> ")"
            
        T_binop Mult e1 e2 -> "((\\n. \\m.  \\f. \\x. n (m f) x) "
                                <> termToLambda e1 <> " "
                                <> termToLambda e2 <> ")"
            
        T_binop And e1 e2 -> "((\\c.\\a.\\b. c a b) "
                                <> termToLambda e1 <> " "
                                <> termToLambda e2 <> " "
                                <> "(\\a.\\b.b))"

        T_binop Or e1 e2 -> "((\\c.\\a.\\b. c a b) "
                                <> termToLambda e1 <> " "
                                <> "(\\a.\\b.a) "
                                <> termToLambda e2 <> ")"

        T_unop Not e1 -> "((\\c.\\a.\\b. c a b) "
                                <> termToLambda e1 <> " "
                                <> "(\\a.\\b.b) "
                                <> "(\\a.\\b.a))"

        T_binop Sub e1 e2 -> "(" <> termToLambda subTerm <> " "
                                    <> termToLambda e1 <> " "
                                    <> termToLambda e2 <> ")"

        T_binop Eq e1 e2 -> "(" <> termToLambda eqTerm <> " "
                                    <> termToLambda e1 <> " "
                                    <> termToLambda e2 <> ")"

        T_binop Ne e1 e2 -> "(" <> termToLambda neTerm <> " "
                                    <> termToLambda e1 <> " "
                                    <> termToLambda e2 <> ")"

        T_binop Gt e1 e2 -> "(" <> termToLambda gtTerm <> " "
                                    <> termToLambda e1 <> " "
                                    <> termToLambda e2 <> ")"

        T_binop Lt e1 e2 -> "(" <> termToLambda ltTerm <> " "
                                    <> termToLambda e1 <> " "
                                    <> termToLambda e2 <> ")"

        T_natRec e1 e2 e3 -> "(" <> termToLambda e1    <> " "
                                    <> termToLambda e2 <> " "
                                    <> termToLambda e3 <> ")"

        T_error -> "ERRO" 


termToLambdaDefs ∷ Term -> String
termToLambdaDefs expr = case expr of
        T_true  -> "true"
        T_false -> "false"
        T_var id -> "x_" <> id
        T_num n -> toStringAs decimal n

        T_func id _ e1 -> "(\\x_" <> id <> ". " 
                                <> termToLambdaDefs e1 <> ")"

        T_app e1 e2 -> "(" <> termToLambdaDefs e1 <> " " <> termToLambdaDefs e2 <> ")"

        T_let id t_var e1 e2 -> termToLambdaDefs (T_app (T_func id t_var e2) e1)


        (T_if e1 e2 e3) ->   "(if " 
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> " "
                            <> termToLambdaDefs e3 <> ")"

        (T_pair e1 e2) -> "(pair "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"
                  
        T_fst e1 ->     "(fst "
                            <> termToLambdaDefs e1 <> ")"
                    
        T_snd e1 ->     "(snd "
                            <> termToLambdaDefs e1 <> ")"


        T_binop Add e1 e2 -> "(add "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"
            
        T_binop Mult e1 e2 -> "(mult "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"
            
        T_binop And e1 e2  -> "(and "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_binop Or e1 e2  -> "(or "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_unop Not e1     -> "(not "
                            <> termToLambdaDefs e1 <> ")"

        T_binop Sub e1 e2 -> "(sub "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_binop Eq e1 e2  -> "(eq "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_binop Ne e1 e2  -> "(ne "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_binop Gt e1 e2  -> "(gt "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_binop Lt e1 e2 -> "(lt "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> ")"

        T_natRec e1 e2 e3 -> "(natRec "
                            <> termToLambdaDefs e1 <> " "
                            <> termToLambdaDefs e2 <> " "
                            <> termToLambdaDefs e3 <> ")"

        _ -> "ERRO"


makeDefLambda :: String -> String
makeDefLambda str = case str of 
    "true"  -> "  true    = \\a. \\b. a;"
    "false" -> "  false   = \\a. \\b. b;"
    "if"    -> "  if      = \\c. \\a. \\b. c a b;"
    "pair"  -> "  pair    = \\a. \\b. \\f. f a b;"
    "fst"   -> "  fst     = \\p. p (\\a.\\b. a);"
    "snd"   -> "  snd     = \\p. p (\\a.\\b. b);"

    "add"   -> "  add     = \\n. \\m. \\f. \\x. m f (n f x);"
    "mult"  -> "  mult    = \\n. \\m. \\f. \\x. n (m f) x;"
    "and"   -> "  and     = \\a. \\b. a b (\\a. \\b. b);"
    "or"    -> "  or      = \\a. \\b. a (\\a. \\b. a) b;"
    "not"   -> "  not     = \\a. a (\\a. \\b. b) (\\a. \\b. a);"

    "succ"  -> "  succ    = \\n. \\f. \\x. f (n f x);"
    "sub"   -> "  sub     = \\n. \\m. m (\\n. fst (n (\\p. (pair (snd p) (succ (snd p)))) (pair 0 0))) n;"

    "isZero"-> "  isZero  = \\n. n (\\b. (\\a. \\b. b)) (\\a. \\b. a);"

    "eq"    -> "  eq     = \\n. \\m. and (isZero (sub n m)) (isZero (sub m n));"
    "ne"    -> "  ne     = \\n. \\m. not (and (isZero (sub n m)) (isZero (sub m n)));"

    "gt"    -> "  gt     = \\n. \\m. not (isZero (sub n m)) ;"
    "lt"    -> "  lt     = \\n. \\m. not (isZero (sub m n)) ;"

    "natRec"-> "  natRec  = \\n. \\step. \\init. n step init;"

    _ -> "?"


makeDefsBlock :: (List String) -> String 
makeDefsBlock l =  
        (case l of
            (_:_) -> "let\n" <> (makeDefsUsed makeDefLambda l)  <> "in\n\n"
            Nil   -> "\n\n")



makeLambda :: Term → String
makeLambda expr = case typeInfer emptyEnv expr of 
                    Just _ -> termToLambda expr
                    Nothing -> if (expr == T_error) then "Sintaxe Incorreta" else "Erro de Tipo"

makeLambdaDefs :: Term → String
makeLambdaDefs expr = case typeInfer emptyEnv expr of 
                    Just _ -> makeDefsBlock (listTermsUsed expr Nil) <> termToLambdaDefs expr
                    Nothing -> if (expr == T_error) then "Sintaxe Incorreta" else "Erro de Tipo"