import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Term from "../Term/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var testTypes = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof TypeSystem.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof TypeSystem.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof TypeSystem.Pair) {
        return "(And " + (testTypes(new Data_Maybe.Just(t.value0.value0)) + (" " + (testTypes(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at Compile (line 29, column 15 - line 37, column 42): " + [ t.constructor.name ]);
};
var makeTypes = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof TypeSystem.Bool) {
        return "(forall C:*,C->C->C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof TypeSystem.Nat) {
        return "(forall C:*, (C -> C) -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof TypeSystem.Pair) {
        return "((\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) " + (makeTypes(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypes(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at Compile (line 14, column 15 - line 26, column 42): " + [ t.constructor.name ]);
};
var makeNatural = function (v) {
    if (v === 0) {
        return "x";
    };
    return "(f " + (makeNatural(v - 1 | 0) + ")");
};
var termToOmega = function (expr) {
    if (expr instanceof Term.T_true) {
        return "(\\\\C:*.\\a:C.\\b:C.a)";
    };
    if (expr instanceof Term.T_false) {
        return "(\\\\C:*.\\a:C.\\b:C.b)";
    };
    if (expr instanceof Term.T_zero) {
        return "(\\\\C:*.\\f:C->C.\\x:C.x)";
    };
    if (expr instanceof Term.T_num) {
        return "(\\\\C:*.\\f:C->C.\\x:C." + (makeNatural(expr.value0) + ")");
    };
    if (expr instanceof Term.T_if) {
        return "((\\\\D:*.\\c:(forall C:*,C->C->C).\\a:D.\\b:D.(c[D]) a b)" + (" [" + (makeTypes(TypeSystem.typeInfer(expr.value1)) + ("] " + (termToOmega(expr.value0) + (" " + (termToOmega(expr.value1) + (" " + (termToOmega(expr.value2) + ")"))))))));
    };
    if (expr instanceof Term.T_pair) {
        return "((\\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b)" + (" [" + (makeTypes(TypeSystem.typeInfer(expr.value0)) + ("][" + (makeTypes(TypeSystem.typeInfer(expr.value1)) + ("] " + (termToOmega(expr.value0) + (" " + (termToOmega(expr.value1) + ")"))))))));
    };
    if (expr instanceof Term.T_fst) {
        return "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))" + ((function () {
            var v = TypeSystem.typeInfer(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof TypeSystem.Pair) {
                return " [" + (makeTypes(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypes(new Data_Maybe.Just(v.value0.value1)) + "] ")));
            };
            return "Erro de Tipo [PARES]";
        })() + (termToOmega(expr.value0) + ")"));
    };
    if (expr instanceof Term.T_snd) {
        return "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [B] (\\a: A.\\b: B. b))" + ((function () {
            var v = TypeSystem.typeInfer(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof TypeSystem.Pair) {
                return " [" + (makeTypes(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypes(new Data_Maybe.Just(v.value0.value1)) + "] ")));
            };
            return "Erro de Tipo [PARES]";
        })() + (termToOmega(expr.value0) + ")"));
    };
    return "incompleto";
};
var makeLOmega = function (expr) {
    var v = TypeSystem.typeInfer(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToOmega(expr);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at Compile (line 89, column 19 - line 91, column 46): " + [ v.constructor.name ]);
};
export {
    makeLOmega,
    makeTypes
};
