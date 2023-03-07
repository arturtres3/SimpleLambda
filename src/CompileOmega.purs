module CompileOmega where

import Prelude

import Data.Int (decimal, toStringAs)
import Data.List (List(..), (:), union)
import Data.Maybe (Maybe(..))
import Structures (BinopCode(..), Term(..), TermType(..), UnopCode(..))
import TypeSystem (Env, emptyEnv, typeInfer, update)

-- import Structures (Term(..), TermType(..))
-- import TypeSystem (typeInfer)

shiftIncTerm :: Term
shiftIncTerm = (T_func "p" (Pair Nat Nat) 
                        (T_pair (T_snd (T_var "p"))  
                        (T_binop  Add  (T_snd (T_var "p")) (T_num 1)))
                    ) 

contains :: (List String) -> String -> Boolean
contains Nil _ = false 
contains (str : tail) target = if (str == target) then true else (contains tail target)

listTermsUsed :: Term -> (List String) -> List String 
listTermsUsed expr l = case expr of 
    T_true          -> union ("true":Nil) l
    T_false         -> union ("false":Nil) l
    T_if e1 e2 e3   -> union (union (union (union ("if":Nil) l) (listTermsUsed e1 l))(listTermsUsed e2 l )) (listTermsUsed e3 l)

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
    T_binop Eq  e1 e2  -> union (union (union ("eq"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Ne  e1 e2  -> union (union (union ("ne"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Gt  e1 e2  -> union (union (union ("gt"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    T_binop Lt  e1 e2  -> union (union (union ("lt"  :Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )

    T_binop Sub e1 e2  -> union (union (union ("pair":"fst":"snd":"succ":"sub":Nil) l) (listTermsUsed e1 l)) (listTermsUsed e2 l )
    _                  -> l


testTypes :: Maybe TermType -> String
testTypes t = case t of
                Just Bool -> "Bool"
                Just Nat  -> "Nat"
                Just (Pair t1 t2) -> "(And " 
                                      <> testTypes (Just t1) <> 
                                      " " 
                                      <> testTypes (Just t2) <> 
                                      ")"
                Nothing -> "ERRO DE TIPO"

                _ -> "INCOMPLETO"
      

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

                Just (Pair t1 t2) -> "(And " 
                                      <> makeTypesOmegaDefs (Just t1) <> " "
                                      <> makeTypesOmegaDefs (Just t2) <> 
                                      ")"

                Just (Func t1 t2) -> "(" <> makeTypesOmegaDefs (Just t1) <> "->" <> makeTypesOmegaDefs (Just t2) <> ")"
                Nothing -> "ERRO DE TIPO"

        
makeNatural:: Int -> String 
makeNatural 0 = "x"
makeNatural n = "(f " <> (makeNatural (n-1)) <> ")"

termToOmega ∷ Term -> Env -> String
termToOmega expr env = case expr of
            T_true  -> "(\\\\C:*.\\a:C.\\b:C.a)"
            T_false -> "(\\\\C:*.\\a:C.\\b:C.b)"
            T_var id -> "x_" <> id
            T_num n -> "(\\\\C:*.\\f:C->C.\\x:C." <> (makeNatural n) <> ")"

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> makeTypesOmega (Just t_var) <> ". " 
                                  <> termToOmega e1 (update env id t_var) <> ")"

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

            T_binop Sub e1 e2 -> "((\\n: " <> makeTypesOmega (Just Nat) <> ". \\m: " <> makeTypesOmega (Just Nat) <> ". "
                                <> "m [" <> makeTypesOmega (Just Nat) <> "] "

                                -- predecessor
                                <> "(\\n: " <> makeTypesOmega (Just Nat) <> ". "
                                <> "(\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))" -- fst
                                <> "[" <> makeTypesOmega (Just Nat) <> "][" <> makeTypesOmega (Just Nat) <> "]"
                                <> "( n [" <> makeTypesOmega (Just (Pair Nat Nat)) <> "]"
                                <> termToOmega shiftIncTerm env 
                                <> termToOmega (T_pair (T_num 0) (T_num 0)) env 
                                <> "))" 

                                <> " n)"
                                <> termToOmega e1 env <> " "
                                <> termToOmega e2 env <> ")"

-- eq            = \n:Nat. \m:Nat. and (isZero (sub n m)) (isZero (sub m n));
-- eq2           =  \n:Nat. \m:Nat. and ((\n:Nat. n [Bool] (\b: Bool. false) true) (sub n m)) ((\n:Nat. n [Bool] (\b: Bool. false) true) (sub m n));

            -- T_binop Eq e1 e2 -> 

            _ -> "INCOMPLETO"


termToOmegaDefs ∷ Term -> Env -> String
termToOmegaDefs expr env = case expr of
            T_true  -> "true"
            T_false -> "false"
            T_var id -> "x_" <> id
            T_num n -> toStringAs decimal n--"(\\\\C:*.\\f:C->C.\\x:C." <> (makeNatural n) <> ")"

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

            (T_pair e1 e2) -> "(pair"
                              <> " ["
                              <> (makeTypesOmegaDefs $ typeInfer env e1)
                              <> "]["
                              <> (makeTypesOmegaDefs $ typeInfer env e2)
                              <> "] "
                              <> termToOmegaDefs e1 env <> " "
                              <> termToOmegaDefs e2 env <> ")"
                  
            T_fst e1 ->  "(fst"
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

-- eq            = \n:Nat. \m:Nat. and (isZero (sub n m)) (isZero (sub m n));

            T_binop Eq e1 e2 -> "(eq "
                                <> termToOmegaDefs e1 env <> " "
                                <> termToOmegaDefs e2 env <> ")"

            _ -> "INCOMPLETO"

makeDef :: String -> String
makeDef str = case str of 
    "true"  -> "  true    = \\\\C:*. \\a: C. \\b: C. a;"
    "false" -> "  false   = \\\\C:*. \\a: C. \\b: C. b;"
    "if"    -> "  if      = \\\\D:*. \\c: Bool. \\a: D. \\b: D. c [D] a b;"
    "pair"  -> "  pair    = \\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b;"
    "fst"   -> "  fst     = \\\\A:*. \\\\B:*. \\p: And A B. p [A] (\\a: A.\\b: B. a);"
    "snd"   -> "  snd     = \\\\A:*. \\\\B:*. \\p: And A B. p [B] (\\a: A.\\b: B. b);"

    "add"   -> "  add     = \\n: Nat. \\m: Nat. \\\\C:*. \\f: C -> C. \\x :C. m [C] f (n [C] f x);"
    "mult"  -> "  mult    = \\n: Nat. \\m: Nat. \\\\C:*. \\f: C -> C. \\x :C. n [C] (m [C] f) x;"
    "and"   -> "  and     = \\a: Bool. \\b: Bool. a [Bool] b (\\\\C:*. \\a: C. \\b: C. b);"
    "or"    -> "  or      = \\a: Bool. \\b: Bool. a [Bool] (\\\\C:*. \\a: C. \\b: C. a) b;"
    "not"   -> "  not     = \\a: Bool. a [Bool] (\\\\C:*. \\a: C. \\b: C. b) (\\\\C:*. \\a: C. \\b: C. a);"

    "succ"  -> "  succ    = \\n: Nat. \\\\C:*. \\f: C -> C. \\x :C. f (n [C] f x);"
    "sub"   -> "  sub     = \\n: Nat. \\m:Nat. m [Nat] (\\n: Nat. fst [Nat] [Nat] (n [And Nat Nat] (\\p: And Nat Nat. (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)))) (pair [Nat] [Nat] 0 0))) n;"

    _ -> "?"


defs :: String 
defs = "typedef\n"
    <>   "  Bool          = forall C:*, C -> C -> C;\n"
    <>   "  Nat           = forall C:*, (C -> C) -> C -> C;\n"
    <>   "  And           = \\A:*, \\B:*, forall C:*, (A -> B -> C) -> C;\n"
    <>   "  Or            = \\A:*, \\B:*, forall C:*, (A -> C) -> (B -> C) -> C;\n"
    <> "end\n"

    <> "let\n"
    <>   "  true          = \\\\C:*. \\a: C. \\b: C. a;\n"
    <>   "  false         = \\\\C:*. \\a: C. \\b: C. b;\n"
    <>   "  if            = \\\\D:*. \\c: Bool. \\a: D. \\b: D. c [D] a b;\n"
    <>   "  and           = \\a: Bool. \\b: Bool. if [Bool] a b false;\n"
    <>   "  or            = \\a: Bool. \\b: Bool. if [Bool] a true b;\n"
    <>   "  not           = \\b: Bool. if [Bool] b false true;\n\n"

    <>   "  pair          = \\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b;\n"
    <>   "  fst           = \\\\A:*. \\\\B:*. \\p: And A B. p [A] (\\a: A.\\b: B. a) ;\n"
    <>   "  snd           = \\\\A:*. \\\\B:*. \\p: And A B. p [B] (\\a: A.\\b: B. b) ;\n\n"

    -- <>   "  left          = \\\\A:*. \\\\B:*. \\a: A. \\\\C:*. \\f: A->C. \g: B->C. f a;\n"
    -- <>   "  right         = \\\\A:*. \\\\B:*. \\b: B. \\\\C:*. \\f: A->C. \g: B->C. g b;\n"
    -- <>   "  case          = \\\\A:*. \\\\B:*. \\\\D:*. \\u: Either A B. \\f: A->D. \g: B->D. u [D] f g;\n\n"

    <>   "  succ          = \\n: Nat. \\\\C:*. \\f: C -> C. \\x :C. f (n [C] f x);\n "
    <>   "  add           = \\n: Nat. \\m: Nat. n [Nat] succ m;\n"
    <>   "  double        = \\n: Nat. n [Nat] succ n;\n"
    <>   "  mult          = \\n: Nat. \\m: Nat. m [Nat] (\\p: Nat. add n p) 0;\n\n"

    <>   "  shiftInc      = \\p: Product Nat Nat. (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)));\n"
    <>   "  pred          = \\n: Nat. fst [Nat] [Nat] (n [And Nat Nat] shiftInc (pair [Nat] [Nat] 0 0));\n"
    <>   "  sub           = \\n: Nat. \\m: Nat. m [Nat] pred n;\n"
    <>   "  isZero        = \\n:Nat. n [Bool] (\\b: Bool. false) true;\n"

    <>   "  eq            = \\n:Nat. \\m:Nat. and (isZero (sub n m)) (isZero (sub m n));\n"
    -- <>   "  gt            = \\n:Nat. \\m:Nat. (isZero (sub n m)) ;\n"
    
    <> "in\n\n"


makeDefsUsed :: (List String) -> String 
makeDefsUsed Nil = "\nin\n\n"
makeDefsUsed (str : tail) = makeDef str <> "\n" <> makeDefsUsed tail

makeDefsBlock :: (List String) -> String 
makeDefsBlock l =  "typedef\n"
    <>   "  Bool          = forall C:*, C -> C -> C;\n"
    <>   "  Nat           = forall C:*, (C -> C) -> C -> C;\n"
    <>   "  And           = \\A:*, \\B:*, forall C:*, (A -> B -> C) -> C;\n"
    <>   "  Or            = \\A:*, \\B:*, forall C:*, (A -> C) -> (B -> C) -> C;\n"
    <> "end\n"

    <> "let\n"
    <> makeDefsUsed l 


makeLOmega :: Term → String
makeLOmega expr = case typeInfer emptyEnv expr of 
                    Just _ -> termToOmega expr emptyEnv
                    Nothing -> "Erro de Tipo"


makeLOmegaDefs :: Term → String
makeLOmegaDefs expr = case typeInfer emptyEnv expr of 
                    Just _ -> defs <> termToOmegaDefs expr emptyEnv
                    Nothing -> "Erro de Tipo"

makeLOmegaDefsUsed :: Term → String
makeLOmegaDefsUsed expr = case typeInfer emptyEnv expr of 
                    Just _ -> makeDefsBlock (listTermsUsed expr Nil) <> termToOmegaDefs expr emptyEnv
                    Nothing -> "Erro de Tipo"



