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
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesOmega(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesOmega(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at Compile (line 25, column 20 - line 36, column 42): " + [ t.constructor.name ]);
};

// _ -> "INCOMPLETO"
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
    throw new Error("Failed pattern match at Compile (line 86, column 19 - line 88, column 46): " + [ v.constructor.name ]);
};
export {
    testTypes,
    makeTypesOmega,
    makeNatural,
    termToOmega,
    makeLOmega
};
