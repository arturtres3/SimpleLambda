module CompileSTLC where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..), (:))
import Data.Int (decimal, toStringAs)
import Structures (BinopCode(..), Term(..), TermType(..), UnopCode(..), listTermsUsed)
import TypeSystem (Env, emptyEnv, typeInfer, update)

makeNatural:: Int -> String
makeNatural 0 = "x"
makeNatural n = "(f " <> (makeNatural (n-1)) <> ")"

typesSTLC :: Maybe TermType -> String
typesSTLC t = case t of
                      Just Bool -> "(A->A->A)" -- "Bool"
                      Just Nat  -> "((A->A)->A->A)"   -- "Nat"

                      Just (Func t1 t2) -> "(" <> typesSTLC (Just t1) <> "->" <> typesSTLC (Just t2) <> ")"

                      Just (Pair t1 t2) -> if t1 /= t2 
                                          then "Pares devem ter o mesmo tipo"
                                          else (let t1String = typesSTLC (Just t1) in 
                                          "((" <> t1String <> " -> " <> t1String <> " -> " <> t1String <> ") -> " <> t1String <> ")")
                      Nothing -> "A"

typesSTLCDefs :: Maybe TermType -> String
typesSTLCDefs t = case t of
                      Just Bool -> "(A->A->A)" -- "Bool"
                      Just Nat  -> "((A->A)->A->A)"   -- "Nat"

                      Just (Func t1 t2) -> "(" <> typesSTLC (Just t1) <> "->" <> typesSTLC (Just t2) <> ")"

                      Just (Pair _ _) -> "((A->A->A) -> ((A->A)->A->A))"
                      Nothing -> "A"


makePairTypeSTLC :: String -> String 
makePairTypeSTLC t = "((" <> t <> "->" <> t <> "->" <> t <> ") -> " <> t <> ")"

makeBooleanTypeSTLC :: String -> String 
makeBooleanTypeSTLC t = "(" <> t <> "->" <> t <> "->" <> t <> ")"
                      
makeIfSTLC :: String -> String 
makeIfSTLC t = "(\\b: (" <> t <> " -> " <> t <> " -> " <> t <> "). \\e1: " <> t <> ". \\e2: " <> t <> ". b e1 e2)"

makePairSTLC :: String -> String
makePairSTLC t = "(\\e1: " <> t <> 
                 ". \\e2: " <> t <>
                 ". \\b: (" <> t <> " -> " <> t <> " -> " <> t <> "). b e1 e2)"

termToSTLC :: Term -> (Maybe TermType) -> Env -> String
termToSTLC expr t env = case expr of 
            T_true  -> "(\\a:" <> typesSTLC t  <> ".\\b:" <> typesSTLC t <> ".a)"
            T_false  -> "(\\a:" <> typesSTLC t <> ".\\b:" <> typesSTLC t <> ".b)"

            T_binop And e1 e2 -> "((\\bin1: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
                                 "\\bin2: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
                                 "\\a: " <> (typesSTLC t) <> ". " <>
                                 "\\b: " <> (typesSTLC t) <> ". " <>
                                 "bin1 (bin2 a b) b) " <>
                                 termToSTLC e1 t env <> " " <>
                                 termToSTLC e2 t env <> ")"
            
            T_binop Or e1 e2 -> "((\\bin1: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
                                 "\\bin2: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
                                 "\\a: " <> (typesSTLC t) <> ". " <>
                                 "\\b: " <> (typesSTLC t) <> ". " <>
                                 "bin1 a (bin2 a b)) " <>
                                 termToSTLC e1 t env <> " " <>
                                 termToSTLC e2 t env <> ")"

            T_unop Not e1 -> "((\\bin: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
                                 "\\a: " <> (typesSTLC t) <> ". " <>
                                 "\\b: " <> (typesSTLC t) <> ". " <>
                                 "bin b a) " <>
                                 termToSTLC e1 t env <> ")"


            T_var id -> "x_" <> id

            T_func id t_var e1 -> "(\\x_" <> id <> ": " <> typesSTLC (Just t_var) <> ". " 
                                  <> termToSTLC e1 t (update env id t_var) <> ")"

            T_app e1 e2 -> "(" <> termToSTLC e1 t env <> " " <> termToSTLC e2 t env <> ")"

            T_let id t_var e1 e2 -> termToSTLC (T_app (T_func id t_var e2) e1) t env
            

            T_if e1 e2 e3 -> (let t1 = (typeInfer env e2) in 
                              case t of 
                                Nothing ->
                                    "(" <> makeIfSTLC (typesSTLC t1) <> " "
                                    <> termToSTLC e1 t1 env <> " "
                                    <> termToSTLC e2 t env <> " "
                                    <> termToSTLC e3 t env <> ")"
                                  
                                Just received_t ->
                                        case t1 of 
                                            Just (Pair _ _) -> ( let bool_t = (Func received_t (Func received_t received_t)) in
                                                "(" <> makeIfSTLC  (typesSTLC (Just (Pair bool_t bool_t))) <> " "
                                                <> termToSTLC e1 (Just (Pair bool_t bool_t)) env <> " "
                                                <> termToSTLC e2 (Just received_t) env <> " "  
                                                <> termToSTLC e3 (Just received_t) env <> ")"
                                            )
                                                
                                            _ -> 
                                                "(" <> makeIfSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> " "
                                                <> termToSTLC e1 (Just (Func received_t (Func received_t received_t))) env <> " "
                                                <> termToSTLC e2 (Just received_t) env <> " "
                                                <> termToSTLC e3 (Just received_t) env <> ")"
                                )

            T_pair e1 e2 ->   (let t1 = (typeInfer env e1) in 
                               let t2 = (typeInfer env e2) in 
                               if t1 == t2 then 
                               case t of 
                                  Nothing ->
                                      ("(" <> makePairSTLC (typesSTLC t1) <> " "
                                      <> termToSTLC e1 t env <> " "
                                      <> termToSTLC e2 t env <> ")")
                                  Just received_t -> 
                                      ("(" <> makePairSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> " "
                                      <> termToSTLC e1 (Just received_t) env <> " "
                                      <> termToSTLC e2 (Just received_t) env <> ")")

                              else "ERRO valores no par devem ser do mesmo tipo"
                              )

            T_fst e1 -> case (typeInfer env e1) of 
                        Just (Pair t1 t2) -> 
                                  if t1 == t2 then
                                  case t of 
                                    Just received_t -> -- tipo par de booleanos em e1 de T_if
                                      "((\\p: " <> 
                                      makePairTypeSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> 
                                      ". p " <>
                                      termToSTLC T_true (Just (Func received_t (Func received_t received_t))) env <> ")"
                                      <> termToSTLC e1 (Just received_t) env <> ")"

                                    Nothing ->  -- pares normais
                                      "((\\p: " <> typesSTLC (typeInfer env e1) <> ". p " <>
                                      (termToSTLC T_true (Just t1) env) <> ")"
                                      <> termToSTLC e1 t env <> ")"

                                  else "ERRO valores no par devem ser do mesmo tipo"
                        _ -> "ERRO de tipo, fst deve receber par"

            T_snd e1 -> case (typeInfer env e1) of 
                        Just (Pair t1 t2) -> 
                                  if t1 == t2 then
                                  case t of 
                                    Just received_t -> -- tipo par de booleanos em e1 de T_if
                                      "((\\p: " <> 
                                      makePairTypeSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> 
                                      ". p " <>
                                      termToSTLC T_false (Just (Func received_t (Func received_t received_t))) env <> ")"
                                      <> termToSTLC e1 t env <> ")"

                                    Nothing ->  -- pares normais
                                      "((\\p: " <> typesSTLC (typeInfer env e1) <> ". p " <>
                                      (termToSTLC T_false (Just t1) env) <> ")"
                                      <> termToSTLC e1 t env <> ")"

                                  else "ERRO valores no par devem ser do mesmo tipo"
                        _ -> "ERRO de tipo, fst deve receber par"

            T_num n -> "(\\f:A->A.\\x:A." <> (makeNatural n) <> ")"
            T_binop Add e1 e2 -> "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A. \\x:A. n1 f (n2 f x))"
                                 <> termToSTLC e1 t env <> " "
                                 <> termToSTLC e2 t env <> ")"
            T_binop Mult e1 e2 -> "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A.\\x:A. n1 (n2 f) x)"
                                 <> termToSTLC e1 t env <> " "
                                 <> termToSTLC e2 t env <> ")"
            _ -> "incompleto"

lastType :: Maybe TermType -> Maybe TermType
lastType t = case t of 
            Just Nat -> Just Nat
            Just Bool -> Just Bool

            Just (Pair t1 _) -> lastType (Just t1)
            Just (Func _ t2) -> lastType (Just t2)
            Nothing -> Nothing 

termToSTLCSimple :: Term -> Env -> String
termToSTLCSimple expr env = case expr of 
            T_true  -> "(\\a:A.\\b:A.a)"
            T_false -> "(\\a:A.\\b:A.b)"
            T_var id -> "x_" <> id

            T_if e1 e2 e3 -> "((\\b:(A->A->A). \\e1:A. \\e2:A. b e1 e2) " <>
                                termToSTLCSimple e1 env <> " " <>
                                termToSTLCSimple e2 env <> " " <>
                                termToSTLCSimple e3 env <> ")"

            -- notas sobre a gereação de código 
            -- Como STLC não tem polimorfismo, cada tipo precisa de uma definição diferente de par.
            -- A biblioteca definida para STLC foi limitada em tipos de pares de Naturais  apenas
            -- para criar pares de tipos compostos (Pares, Funções, ...) usar STLC ext
            T_pair e1 e2 -> (let t1 = (typeInfer env e1) in 
                                let t2 = (typeInfer env e2) in 
                                if t1 == t2 then 
                                    ("((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\b:(A->A->A).\\f:A->A.\\x:A. b (n1 f x) (n2 f x)) "  -- <> makePairSTLC (typesSTLC (lastType t1)) <> " "
                                    <> termToSTLCSimple e1 env <> " "
                                    <> termToSTLCSimple e2 env <> ")")
                                else 
                                    "ERRO valores no par devem ser do mesmo tipo"
                            )

            T_fst e1 ->  case (typeInfer env e1) of 
                        Just (Pair t1 t2) -> 
                                    if t1 == t2 then
                                        "((\\p: (A->A->A) -> ((A->A)->A->A). \\f:A->A.\\x:A. (p " -- <> makePairTypeSTLC (typesSTLC (lastType (typeInfer env e1))) <> ". p "
                                        <> (termToSTLCSimple T_true env) <> ") f x)"
                                        <> termToSTLCSimple e1 env <> ")"

                                    else "ERRO valores no par devem ser do mesmo tipo"
                        _ -> "ERRO de tipo, fst deve receber par"

            T_snd e1 ->  case (typeInfer env e1) of 
                        Just (Pair t1 t2) -> 
                                    if t1 == t2 then
                                        "((\\p: (A->A->A) -> ((A->A)->A->A). \\f:A->A.\\x:A. (p " -- <> makePairTypeSTLC (typesSTLC (lastType (typeInfer env e1))) <> ". p "
                                        <> (termToSTLCSimple T_false env) <> ") f x)"
                                        <>  termToSTLCSimple e1 env <> ")"

                                    else "ERRO valores no par devem ser do mesmo tipo"
                        _ -> "ERRO de tipo, fst deve receber par"


            T_app e1 e2 -> "(" <> termToSTLCSimple e1 env <> " " <> termToSTLCSimple e2 env <> ")"

            T_let id t_var e1 e2 ->  termToSTLCSimple (T_app (T_func id t_var e2) e1) env

            T_func id t_var e1 ->   "(\\x_" <> id <> ": " <> typesSTLC (Just t_var) <> ". " 
                                    <> termToSTLCSimple e1 (update env id t_var) <> ")"

            T_num n -> toStringAs decimal n 

            T_binop Add e1 e2 -> "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A. \\x:A. n1 f (n2 f x))"
                                 <> termToSTLCSimple e1 env <> " "
                                 <> termToSTLCSimple e2 env <> ")"
            T_binop Mult e1 e2 -> "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A.\\x:A. n1 (n2 f) x)"
                                 <> termToSTLCSimple e1 env <> " "
                                 <> termToSTLCSimple e2 env <> ")"

            T_binop And e1 e2 -> "((\\bin1: (A->A->A). \\bin2: (A->A->A). \\a: A. \\b: A. bin1 (bin2 a b) b) " 
                                 <> termToSTLCSimple e1 env <> " "
                                 <> termToSTLCSimple e2 env <> ")"
            
            T_binop Or e1 e2 -> "((\\bin1: (A->A->A). \\bin2: (A->A->A). \\a: A. \\b: A. bin1 a (bin2 a b)) "
                                 <> termToSTLCSimple e1 env <> " "
                                 <> termToSTLCSimple e2 env <> ")"

            T_unop Not e1 -> "((\\bin: (A->A->A). \\a: A. \\b: A. bin b a) " 
                                 <> termToSTLCSimple e1 env <> ")"

            _ -> "incompleto"

termToSTLCDefs :: Term -> Env -> String
termToSTLCDefs expr env = case expr of 
            T_true  -> "true"
            T_false -> "false"
            T_var id -> "x_" <> id

            T_if e1 e2 e3 -> "(if " <>
                                termToSTLCDefs e1 env <> " " <>
                                termToSTLCDefs e2 env <> " " <>
                                termToSTLCDefs e3 env <> ")"

            -- notas sobre a gereação de código 
            -- Como STLC não tem polimorfismo, cada tipo precisa de uma definição diferente de par.
            -- A biblioteca definida para STLC foi limitada em tipos de pares de Naturais  apenas
            -- para criar pares de tipos compostos (Pares, Funções, ...) usar STLC ext
            T_pair e1 e2 -> (let t1 = (typeInfer env e1) in 
                                let t2 = (typeInfer env e2) in 
                                if t1 == t2 then 
                                    ("(pair "  -- <> makePairSTLC (typesSTLC (lastType t1)) <> " "
                                    <> termToSTLCDefs e1 env <> " "
                                    <> termToSTLCDefs e2 env <> ")")
                                else 
                                    "ERRO valores no par devem ser do mesmo tipo"
                            )

            T_fst e1 ->  case (typeInfer env e1) of 
                        Just (Pair t1 t2) -> 
                                    if t1 == t2 then
                                        "(fst "
                                        <> termToSTLCDefs e1 env <> ")"

                                    else "ERRO valores no par devem ser do mesmo tipo"
                        _ -> "ERRO de tipo, fst deve receber par"

            T_snd e1 ->  case (typeInfer env e1) of 
                        Just (Pair t1 t2) -> 
                                    if t1 == t2 then
                                        "(snd "
                                        <>  termToSTLCDefs e1 env <> ")"

                                    else "ERRO valores no par devem ser do mesmo tipo"
                        _ -> "ERRO de tipo, fst deve receber par"


            T_app e1 e2 -> "(" <> termToSTLCDefs e1 env <> " " <> termToSTLCDefs e2 env <> ")"

            T_let id t_var e1 e2 ->  termToSTLCDefs (T_app (T_func id t_var e2) e1) env

            T_func id t_var e1 ->   "(\\x_" <> id <> ": " <> typesSTLCDefs (Just t_var) <> ". " 
                                    <> termToSTLCDefs e1 (update env id t_var) <> ")"

            T_num n -> toStringAs decimal n 

            T_binop Add e1 e2 -> "(add "
                                 <> termToSTLCDefs e1 env <> " "
                                 <> termToSTLCDefs e2 env <> ")"
            T_binop Mult e1 e2 -> "(mult "
                                 <> termToSTLCDefs e1 env <> " "
                                 <> termToSTLCDefs e2 env <> ")"

            T_binop And e1 e2 -> "(and " 
                                 <> termToSTLCDefs e1 env <> " "
                                 <> termToSTLCDefs e2 env <> ")"
            
            T_binop Or e1 e2 -> "(or "
                                 <> termToSTLCDefs e1 env <> " "
                                 <> termToSTLCDefs e2 env <> ")"

            T_unop Not e1 -> "(not " 
                                 <> termToSTLCDefs e1 env <> ")"

            _ -> "incompleto"


makeDefSTLC :: String -> String
makeDefSTLC str = case str of 
    "true"  -> "  true    = \\a:A.\\b:A.a;"
    "false" -> "  false   = \\a:A.\\b:A.b;"
    "if"    -> "  if      = \\b:(A->A->A). \\e1:A. \\e2:A. b e1 e2;"
    "pair"  -> "  pair    = \\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\b:(A->A->A).\\f:A->A.\\x:A. b (n1 f x) (n2 f x);"
    "fst"   -> "  fst     = \\p: (A->A->A) -> ((A->A)->A->A).\\f:A->A.\\x:A. (p (\\a:A.\\b:A.a)) f x;"
    "snd"   -> "  snd     = \\p: (A->A->A) -> ((A->A)->A->A).\\f:A->A.\\x:A. (p (\\a:A.\\b:A.b)) f x;"

    "add"   -> "  add     = \\n1:(A->A)->A->A. \\n2:(A->A)->A->A. \\f:A->A. \\x:A. n1 f (n2 f x);"
    "mult"  -> "  mult    = \\n1:(A->A)->A->A. \\n2:(A->A)->A->A. \\f:A->A. \\x:A. n1 (n2 f) x;"
    "and"   -> "  and     = \\bin1:(A->A->A). \\bin2:(A->A->A). \\a:A. \\b:A. bin1 (bin2 a b) b;"
    "or"    -> "  or      = \\bin1:(A->A->A). \\bin2:(A->A->A). \\a:A. \\b:A. bin1 a (bin2 a b);"
    "not"   -> "  not     = \\bin :(A->A->A). \\a:A. \\b:A. bin b a;"

    --"succ"  -> "  succ    = "
    --"sub"   -> "  sub     = "

    _ -> "?"

makeDefsUsed :: (List String) -> String 
makeDefsUsed Nil = "\n"
makeDefsUsed (str : tail) = makeDefSTLC str <> "\n" <> makeDefsUsed tail

makeDefsBlock :: (List String) -> String 
makeDefsBlock l =  "let\n"

    <> makeDefsUsed l 

    <> "in\n\n"


-- versão simples 
validIfSelectorsSimple :: Term -> Boolean
validIfSelectorsSimple expr = case expr of 
                          T_binop And b1 b2 -> (validIfSelectorsSimple b1) && (validIfSelectorsSimple b2)
                          T_binop Or b1 b2 -> (validIfSelectorsSimple b1) && (validIfSelectorsSimple b2)
                          T_unop Not b1 -> validIfSelectorsSimple b1
                          T_true  -> true
                          T_false -> true
                          _ -> false

canMakeSTLCSimple :: Term -> Boolean
canMakeSTLCSimple expr = case expr of 
            T_true  -> true
            T_false -> true
            T_num _ -> true
            T_var _ -> true

            T_if e1 e2 e3 -> validIfSelectorsSimple e1 && canMakeSTLCSimple e2 && canMakeSTLCSimple e3

            T_fst e1 -> canMakeSTLCSimple e1
            T_snd e1 -> canMakeSTLCSimple e1
            T_app e1 e2 -> canMakeSTLCSimple e1 && canMakeSTLCSimple e2
            T_pair e1 e2 -> canMakeSTLCSimple e1 && canMakeSTLCSimple e2
            T_binop Sub _ _ -> false
            T_binop Eq _ _ -> false
            T_binop Ne _ _ -> false
            T_binop Gt _ _ -> false
            T_binop Lt _ _ -> false
            T_unop Negate _ -> false
            T_unop _ e1 -> canMakeSTLCSimple e1
            T_binop _ e1 e2 -> canMakeSTLCSimple e1 && canMakeSTLCSimple e2
            T_let _ _ e1 e2 -> canMakeSTLCSimple e1 && canMakeSTLCSimple e2
            T_func _ _ e1 -> canMakeSTLCSimple e1

-- unica restricao de 
validIfSelector :: Term -> Boolean
validIfSelector expr = case expr of 
            T_true  -> true
            T_false -> true
            T_num _ -> true
            T_var _ -> false

            T_if e1 e2 e3 -> validIfSelector e1 && validIfSelector e2 && validIfSelector e3

            T_fst e1 -> validIfSelector e1
            T_snd e1 -> validIfSelector e1
            T_app e1 e2 -> validIfSelector e1 && validIfSelector e2
            T_pair e1 e2 -> validIfSelector e1 && validIfSelector e2
            T_binop Sub _ _ -> false
            T_binop Eq _ _ -> false
            T_binop Ne _ _ -> false
            T_binop Gt _ _ -> false
            T_binop Lt _ _ -> false
            T_unop Negate _ -> false
            T_unop _ e1 -> validIfSelector e1
            T_binop _ e1 e2 -> validIfSelector e1 && validIfSelector e2
            T_let _ _ e1 e2 -> validIfSelector e1 && validIfSelector e2
            T_func _ _ e1 -> validIfSelector e1

canMakeSTLC :: Term -> Boolean
canMakeSTLC expr = case expr of 
            T_true  -> true
            T_false -> true
            T_num _ -> true
            T_var _ -> true

            T_if e1 e2 e3 -> validIfSelector e1 && canMakeSTLC e2 && canMakeSTLC e3

            T_fst e1 -> canMakeSTLC e1
            T_snd e1 -> canMakeSTLC e1
            T_app e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
            T_pair e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
            T_binop Sub _ _ -> false
            T_binop Eq _ _ -> false
            T_binop Ne _ _ -> false
            T_binop Gt _ _ -> false
            T_binop Lt _ _ -> false
            T_unop Negate _ -> false
            T_unop _ e1 -> canMakeSTLC e1
            T_binop _ e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
            T_let _ _ e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
            T_func _ _ e1 -> canMakeSTLC e1


makeSTLC :: Term -> String 
makeSTLC expr = if canMakeSTLC expr then 
                    case typeInfer emptyEnv expr of 
                        Just _ -> termToSTLC expr Nothing emptyEnv
                        Nothing -> "Erro de Tipo"
                else
                    "O termo seletor do if não pode conter variáveis.\nNão é possível representar subtração ou comparações entre naturais em STLC."

makeSTLCDefs :: Term -> String
makeSTLCDefs expr = if canMakeSTLC expr then 
                        case typeInfer emptyEnv expr of 
                            Just _ -> makeDefsBlock (listTermsUsed expr Nil) <> termToSTLCDefs expr emptyEnv
                            Nothing -> "Erro de Tipo"
                    else
                        "Não é possível representar subtração ou comparações entre naturais em STLC. "

makeSTLCSimple :: Term -> String
makeSTLCSimple expr = if canMakeSTLC expr then 
                        case typeInfer emptyEnv expr of 
                            Just _ -> termToSTLCSimple expr emptyEnv
                            Nothing -> "Erro de Tipo"
                    else
                        "Para gerar STLC simples os seletores de if só podem conter expressões lógicas.\nNão é possível representar subtração ou comparações entre naturais em STLC. "