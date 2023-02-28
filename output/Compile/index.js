import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var testTypes = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(And " + (testTypes(new Data_Maybe.Just(t.value0.value0)) + (" " + (testTypes(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    return "INCOMPLETO";
};

// typesSTLC :: Maybe TermType -> String
// typesSTLC t = case t of 
//                       Just Bool -> "(A->A->A)"
//                       Just Nat  -> "((A->A)->A->A)"
// 
//                       Just (Func t1 t2) -> "(" <> typesSTLC (Just t1) <> "->" <> typesSTLC (Just t2) <> ")"
// 
//                       Just (Pair t1 t2) -> if t1 /= t2 
//                                           then "Pares devem ter o mesmo tipo"
//                                           else (let t1String = typesSTLC (Just t1) in 
//                                           "((" <> t1String <> " -> " <> t1String <> " -> " <> t1String <> ") -> " <> t1String <> ")")
//                       Nothing -> "A"
// 
// makePairTypeSTLC :: String -> String 
// makePairTypeSTLC t = "((" <> t <> "->" <> t <> "->" <> t <> ") -> " <> t <> ")"
// 
// makeBooleanTypeSTLC :: String -> String 
// makeBooleanTypeSTLC t = "(" <> t <> "->" <> t <> "->" <> t <> ")"
//                       
// makeIfSTLC :: String -> String 
// makeIfSTLC t = "(\\b: (" <> t <> " -> " <> t <> " -> " <> t <> "). \\e1: " <> t <> ". \\e2: " <> t <> ". b e1 e2)"
// 
// makePairSTLC :: String -> String
// makePairSTLC t = "(\\e1: " <> t <> 
//                  ". \\e2: " <> t <>
//                  ". \\b: (" <> t <> " -> " <> t <> " -> " <> t <> "). b e1 e2)"
// 
// termToSTLC :: Term -> (Maybe TermType) -> Env -> String
// termToSTLC expr t env = case expr of 
//             T_true  -> "(\\a:" <> typesSTLC t  <> ".\\b:" <> typesSTLC t <> ".a)"
//             T_false  -> "(\\a:" <> typesSTLC t <> ".\\b:" <> typesSTLC t <> ".b)"
// 
//             T_binop And e1 e2 -> "((\\bin1: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
//                                  "\\bin2: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
//                                  "\\a: " <> (typesSTLC t) <> ". " <>
//                                  "\\b: " <> (typesSTLC t) <> ". " <>
//                                  "bin1 (bin2 a b) b) " <>
//                                  termToSTLC e1 t env <> " " <>
//                                  termToSTLC e2 t env <> ")"
//             
//             T_binop Or e1 e2 -> "((\\bin1: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
//                                  "\\bin2: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
//                                  "\\a: " <> (typesSTLC t) <> ". " <>
//                                  "\\b: " <> (typesSTLC t) <> ". " <>
//                                  "bin1 a (bin2 a b)) " <>
//                                  termToSTLC e1 t env <> " " <>
//                                  termToSTLC e2 t env <> ")"
// 
//             T_unop Not e1 -> "((\\bin: " <> (makeBooleanTypeSTLC (typesSTLC t)) <> ". " <>
//                                  "\\a: " <> (typesSTLC t) <> ". " <>
//                                  "\\b: " <> (typesSTLC t) <> ". " <>
//                                  "bin b a) " <>
//                                  termToSTLC e1 t env <> ")"
// 
// 
//             T_var id -> "_" <> id
// 
//             T_func id t_var e1 -> "(\\_" <> id <> ": " <> typesSTLC (Just t_var) <> ". " 
//                                   <> termToSTLC e1 t (update env id t_var) <> ")"
// 
//             T_app e1 e2 -> termToSTLC e1 t env <> " " <> termToSTLC e2 t env
// 
//             T_let id t_var e1 e2 -> termToSTLC (T_app (T_func id t_var e2) e1) t env
//             
// 
//             T_if e1 e2 e3 -> (let t1 = (typeInfer env e2) in 
//                               case t of 
//                                 Nothing ->
//                                   "(" <> makeIfSTLC (typesSTLC t1) <> " "
//                                   <> termToSTLC e1 t1 env <> " "
//                                   <> termToSTLC e2 t env <> " "
//                                   <> termToSTLC e3 t env <> ")"
//                                   
//                                 Just received_t ->
//                                   "(" <> makeIfSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> " "
//                                   <> termToSTLC e1 (Just (Func received_t (Func received_t received_t))) env <> " "
//                                   <> termToSTLC e2 (Just received_t) env <> " "
//                                   <> termToSTLC e3 (Just received_t) env <> ")"
//                                   )
// 
//             T_pair e1 e2 ->   (let t1 = (typeInfer env e1) in 
//                                let t2 = (typeInfer env e2) in 
//                                if t1 == t2 then 
//                                case t of 
//                                   Nothing ->
//                                       ("(" <> makePairSTLC (typesSTLC t1) <> " "
//                                       <> termToSTLC e1 t1 env <> " "
//                                       <> termToSTLC e2 t2 env <> ")")
//                                   Just received_t -> 
//                                       ("(" <> makePairSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> " "
//                                       --("(" <> makePairSTLC (typesSTLC (Just (Func received_t (Func received_t received_t)))) <> " "
//                                       <> termToSTLC e1 (Just received_t) env <> " "
//                                       <> termToSTLC e2 (Just received_t) env <> ")")
// 
//                               else "ERRO valores no par devem ser do mesmo tipo"
//                               )
// 
//             T_fst e1 -> case (typeInfer env e1) of 
//                         Just (Pair t1 t2) -> 
//                                   if t1 == t2 then
//                                   case t of 
//                                     Just received_t -> -- tipo par de booleanos em e1 de T_if
//                                       "((\\p: " <> 
//                                       makePairTypeSTLC (makeBooleanTypeSTLC (typesSTLC (Just received_t))) <> 
//                                       -- (typesSTLC (Just (Pair (Func received_t (Func received_t received_t))
//                                       --                        (Func received_t (Func received_t received_t))))) <> 
//                                       ". p " <>
//                                       termToSTLC T_true (Just (Func received_t (Func received_t received_t))) env <> ")"
//                                       <> termToSTLC e1 t env <> ")"
// 
//                                     Nothing ->  -- pares normais
//                                       "((\\p: " <> typesSTLC (typeInfer env e1) <> ". p " <>
//                                       (termToSTLC T_true (Just t1) env) <> ")"
//                                       <> termToSTLC e1 t env <> ")"
// 
//                                   else "ERRO valores no par devem ser do mesmo tipo"
// 
// 
//                         _ -> "ERRO de tipo, fst deve receber par"
// 
//             T_num n -> "(\\f:A->A.\\x:A." <> (makeNatural n) <> ")"
//             T_binop Add e1 e2 -> "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A. \\x:A. n1 f (n2 f x))"
//                                  <> termToSTLC e1 t env <> " "
//                                  <> termToSTLC e2 t env <> ")"
//             T_binop Mult e1 e2 -> "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A.\\x:A. n1 (n2 f) x)"
//                                  <> termToSTLC e1 t env <> " "
//                                  <> termToSTLC e2 t env <> ")"
//             _ -> "incompleto"
// 
// -- versão simples 
// validIfSelectors :: Term -> Boolean
// validIfSelectors expr = case expr of 
//                           T_binop And b1 b2 -> (validIfSelectors b1) && (validIfSelectors b2)
//                           T_binop Or b1 b2 -> (validIfSelectors b1) && (validIfSelectors b2)
//                           T_unop Not b1 -> validIfSelectors b1
//                           T_true  -> true
//                           T_false -> true
//                           _ -> false
// 
// canMakeSTLC :: Term -> Boolean
// canMakeSTLC expr = case expr of 
//             T_true -> true
//             T_false -> true
//             T_num _ -> true
//             T_var _ -> true
// 
//             T_if e1 e2 e3 -> validIfSelectors e1 && canMakeSTLC e2 && canMakeSTLC e3
// 
//             T_fst e1 -> canMakeSTLC e1
//             T_snd e1 -> canMakeSTLC e1
//             T_unop _ e1 -> canMakeSTLC e1
//             T_app e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
//             T_pair e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
//             T_binop Sub _ _ -> false
//             T_binop _ e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
//             T_let _ _ e1 e2 -> canMakeSTLC e1 && canMakeSTLC e2
//             T_func _ _ e1 -> canMakeSTLC e1
/**
 *  
 * isIf :: Term -> Maybe Term
 * isIf expr = case expr of 
 *           T_if _ _ _ -> Just expr 
 *           _ -> Nothing 
 * 
 * findIfTerms :: Term -> Term
 * findIfTerms expr = case expr of 
 *           T_true -> Nil
 *           T_false -> Nil
 *           T_num _ -> Nil
 *           T_var _ -> Nil
 * 
 *           T_if e1 e2 e3 -> expr : (findIfTerms e1) : (findIfTerms e2) : (findIfTerms e3)
 * 
 *           T_fst e1 -> (findIfTerms e1)
 *           T_snd e1 -> (findIfTerms e1)
 *           T_unop _ e1 -> (findIfTerms e1)
 *           T_app e1 e2 -> (findIfTerms e1) : (findIfTerms e2)
 *           T_pair e1 e2 -> (findIfTerms e1) : (findIfTerms e2)
 *           T_binop _ e1 e2 -> (findIfTerms e1) : (findIfTerms e2)
 *           T_let _ _ e1 e2 -> (findIfTerms e1) : (findIfTerms e2)
 *           T_func _ _ e1 -> (findIfTerms e1)
 * 
 * 
 * isVarInIf :: Term Ident -> Boolean
 * isVarInIf expr var = case expr of 
 *             T_var str -> if str == var then true else false 
 * 
 *             T_if e1 e2 e3 -> 
 *         
 */
var makeTypesOmega = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(forall C:*,C->C->C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "(forall C:*, (C -> C) -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "((\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) " + (makeTypesOmega(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypesOmega(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    return "INCOMPLETO";
};
var makeNatural = function (v) {
    if (v === 0) {
        return "x";
    };
    return "(f " + (makeNatural(v - 1 | 0) + ")");
};
var termToOmega = function (expr) {
    if (expr instanceof Structures.T_true) {
        return "(\\\\C:*.\\a:C.\\b:C.a)";
    };
    if (expr instanceof Structures.T_false) {
        return "(\\\\C:*.\\a:C.\\b:C.b)";
    };
    if (expr instanceof Structures.T_num) {
        return "(\\\\C:*.\\f:C->C.\\x:C." + (makeNatural(expr.value0) + ")");
    };
    if (expr instanceof Structures.T_if) {
        return "((\\\\D:*.\\c:(forall C:*,C->C->C).\\a:D.\\b:D.(c[D]) a b)" + (" [" + (makeTypesOmega(TypeSystem.typeInferSimple(expr.value1)) + ("] " + (termToOmega(expr.value0) + (" " + (termToOmega(expr.value1) + (" " + (termToOmega(expr.value2) + ")"))))))));
    };
    if (expr instanceof Structures.T_pair) {
        return "((\\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b)" + (" [" + (makeTypesOmega(TypeSystem.typeInferSimple(expr.value0)) + ("][" + (makeTypesOmega(TypeSystem.typeInferSimple(expr.value1)) + ("] " + (termToOmega(expr.value0) + (" " + (termToOmega(expr.value1) + ")"))))))));
    };
    if (expr instanceof Structures.T_fst) {
        return "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))" + ((function () {
            var v = TypeSystem.typeInferSimple(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                return " [" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value1)) + "] ")));
            };
            return "Erro de Tipo [PARES]";
        })() + (termToOmega(expr.value0) + ")"));
    };
    if (expr instanceof Structures.T_snd) {
        return "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [B] (\\a: A.\\b: B. b))" + ((function () {
            var v = TypeSystem.typeInferSimple(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                return " [" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value1)) + "] ")));
            };
            return "Erro de Tipo [PARES]";
        })() + (termToOmega(expr.value0) + ")"));
    };
    return "INCOMPLETO";
};
var makeLOmega = function (expr) {
    var v = TypeSystem.typeInferSimple(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToOmega(expr);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at Compile (line 275, column 19 - line 277, column 46): " + [ v.constructor.name ]);
};
export {
    testTypes,
    makeTypesOmega,
    makeNatural,
    termToOmega,
    makeLOmega
};
