import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TermLibrary from "../TermLibrary/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(Structures.eqTerm);
var makeTypesOmegaNewSim = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(forall C:*. C -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "(forall C:*. (C -> C) -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "((\\A:*. \\B:*. forall C:*. (A -> B -> C) -> C) " + (makeTypesOmegaNewSim(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypesOmegaNewSim(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesOmegaNewSim(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesOmegaNewSim(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileOmega (line 27, column 26 - line 37, column 42): " + [ t.constructor.name ]);
};
var termToOmegaNewSim = function (expr) {
    return function (env) {
        if (expr instanceof Structures.T_true) {
            return "(\\C:*.\\a:C.\\b:C.a)";
        };
        if (expr instanceof Structures.T_false) {
            return "(\\C:*.\\a:C.\\b:C.b)";
        };
        if (expr instanceof Structures.T_var) {
            return "x_" + expr.value0;
        };
        if (expr instanceof Structures.T_num) {
            return "(\\C:*.\\f:C->C.\\x:C." + (Structures.makeNatural(expr.value0) + ")");
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (makeTypesOmegaNewSim(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmegaNewSim(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_func_system) {
            return "(\\" + (expr.value0 + (": " + (makeTypesOmegaNewSim(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmegaNewSim(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_var_system) {
            return expr.value0;
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToOmegaNewSim(expr.value0)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToOmegaNewSim(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "((\\D:*.\\c:(forall C:*.C->C->C).\\a:D.\\b:D.(c D) a b)" + (" " + (makeTypesOmegaNewSim(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToOmegaNewSim(expr.value0)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "((\\A:*. \\B:*. \\a: A. \\b: B. \\C:*. \\f: A->B->C. f a b)" + (" " + (makeTypesOmegaNewSim(TypeSystem.typeInfer(env)(expr.value0)) + (" " + (makeTypesOmegaNewSim(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToOmegaNewSim(expr.value0)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "((\\A:*. \\B:*. \\p: (\\A:*. \\B:*. forall C:*. (A -> B -> C) -> C) A B. p A (\\a: A.\\b: B. a))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesOmegaNewSim(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesOmegaNewSim(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmegaNewSim(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "((\\A:*. \\B:*. \\p: (\\A:*. \\B:*. forall C:*. (A -> B -> C) -> C) A B. p B (\\a: A.\\b: B. b))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesOmegaNewSim(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesOmegaNewSim(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmegaNewSim(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "((\\n: (forall C:*. (C -> C) -> C -> C). \\m: (forall C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. m C f (n C f x)) " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "((\\n: (forall C:*. (C -> C) -> C -> C). \\m: (forall C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. n C (m C f) x) " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "((\\c:(forall C:*.C->C->C).\\a:(forall C:*.C->C->C).\\b:(forall C:*.C->C->C). (c (forall C:*.C->C->C)) a b)" + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + (" " + "(\\C:*.\\a:C.\\b:C.b))"))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "((\\c:(forall C:*.C->C->C).\\a:(forall C:*.C->C->C).\\b:(forall C:*.C->C->C). (c (forall C:*.C->C->C)) a b)" + (termToOmegaNewSim(expr.value1)(env) + (" " + ("(\\C:*.\\a:C.\\b:C.a) " + (termToOmegaNewSim(expr.value2)(env) + ")"))));
        };
        if (expr instanceof Structures.T_unop) {
            return "((\\c:(forall C:*.C->C->C).\\a:(forall C:*.C->C->C).\\b:(forall C:*.C->C->C). (c (forall C:*.C->C->C)) a b)" + (termToOmegaNewSim(expr.value1)(env) + (" " + ("(\\C:*.\\a:C.\\b:C.b) " + "(\\C:*.\\a:C.\\b:C.a))")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(" + (termToOmegaNewSim(TermLibrary.subTerm)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(" + (termToOmegaNewSim(TermLibrary.eqTerm)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(" + (termToOmegaNewSim(TermLibrary.neTerm)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(" + (termToOmegaNewSim(TermLibrary.gtTerm)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(" + (termToOmegaNewSim(TermLibrary.ltTerm)(env) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(" + (termToOmegaNewSim(expr.value0)(env) + (" " + (makeTypesOmegaNewSim(TypeSystem.typeInfer(env)(expr.value2)) + (" " + (termToOmegaNewSim(expr.value1)(env) + (" " + (termToOmegaNewSim(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeTypesOmegaDefs = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(Pair " + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileOmega (line 40, column 24 - line 50, column 42): " + [ t.constructor.name ]);
};
var termToOmegaDefs = function (expr) {
    return function (env) {
        if (expr instanceof Structures.T_true) {
            return "true";
        };
        if (expr instanceof Structures.T_false) {
            return "false";
        };
        if (expr instanceof Structures.T_var) {
            return "x_" + expr.value0;
        };
        if (expr instanceof Structures.T_num) {
            return Data_Int.toStringAs(Data_Int.decimal)(expr.value0);
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (makeTypesOmegaDefs(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmegaDefs(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToOmegaDefs(expr.value0)(env) + (" " + (termToOmegaDefs(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToOmegaDefs(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "(if " + (" [" + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToOmegaDefs(expr.value0)(env) + (" " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "(pair " + (" [" + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value0)) + ("][" + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToOmegaDefs(expr.value0)(env) + (" " + (termToOmegaDefs(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "(fst " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " [" + (makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmegaDefs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "(snd " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " [" + (makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmegaDefs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "(add " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "(mult " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "(and " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "(or " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop) {
            return "(not " + (termToOmegaDefs(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(sub " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(eq " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(ne " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(gt " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(lt " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(natRec [" + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value2)) + ("] " + (termToOmegaDefs(expr.value0)(env) + (" " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var termToOmegaDefsNewSim = function (expr) {
    return function (env) {
        if (expr instanceof Structures.T_true) {
            return "true";
        };
        if (expr instanceof Structures.T_false) {
            return "false";
        };
        if (expr instanceof Structures.T_var) {
            return "x_" + expr.value0;
        };
        if (expr instanceof Structures.T_num) {
            return Data_Int.toStringAs(Data_Int.decimal)(expr.value0);
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (makeTypesOmegaDefs(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmegaDefsNewSim(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToOmegaDefsNewSim(expr.value0)(env) + (" " + (termToOmegaDefsNewSim(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToOmegaDefsNewSim(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "(if " + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToOmegaDefsNewSim(expr.value0)(env) + (" " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "(pair " + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value0)) + (" " + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToOmegaDefsNewSim(expr.value0)(env) + (" " + (termToOmegaDefsNewSim(expr.value1)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "(fst " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value1)) + " "));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmegaDefsNewSim(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "(snd " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesOmegaDefs(new Data_Maybe.Just(v.value0.value1)) + " "));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmegaDefsNewSim(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "(add " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "(mult " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "(and " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "(or " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop) {
            return "(not " + (termToOmegaDefsNewSim(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(sub " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(eq " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(ne " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(gt " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(lt " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(natRec " + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value2)) + (" " + (termToOmegaDefsNewSim(expr.value0)(env) + (" " + (termToOmegaDefsNewSim(expr.value1)(env) + (" " + (termToOmegaDefsNewSim(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
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
    throw new Error("Failed pattern match at CompileOmega (line 14, column 20 - line 24, column 42): " + [ t.constructor.name ]);
};
var termToOmega = function (expr) {
    return function (env) {
        if (expr instanceof Structures.T_true) {
            return "(\\\\C:*.\\a:C.\\b:C.a)";
        };
        if (expr instanceof Structures.T_false) {
            return "(\\\\C:*.\\a:C.\\b:C.b)";
        };
        if (expr instanceof Structures.T_var) {
            return "x_" + expr.value0;
        };
        if (expr instanceof Structures.T_num) {
            return "(\\\\C:*.\\f:C->C.\\x:C." + (Structures.makeNatural(expr.value0) + ")");
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (makeTypesOmega(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmega(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_func_system) {
            return "(\\" + (expr.value0 + (": " + (makeTypesOmega(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmega(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_var_system) {
            return expr.value0;
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToOmega(expr.value0)(env) + (" " + (termToOmega(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToOmega(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "((\\\\D:*.\\c:(forall C:*,C->C->C).\\a:D.\\b:D.(c[D]) a b)" + (" [" + (makeTypesOmega(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToOmega(expr.value0)(env) + (" " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "((\\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b)" + (" [" + (makeTypesOmega(TypeSystem.typeInfer(env)(expr.value0)) + ("][" + (makeTypesOmega(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToOmega(expr.value0)(env) + (" " + (termToOmega(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " [" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmega(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "((\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [B] (\\a: A.\\b: B. b))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " [" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesOmega(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToOmega(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "((\\n: (forall C:*, (C -> C) -> C -> C). \\m: (forall C:*, (C -> C) -> C -> C). \\\\C:*. \\f: C -> C. \\x :C. m [C] f (n [C] f x)) " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "((\\n: (forall C:*, (C -> C) -> C -> C). \\m: (forall C:*, (C -> C) -> C -> C). \\\\C:*. \\f: C -> C. \\x :C. n [C] (m [C] f) x) " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)" + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + (" " + "(\\\\C:*.\\a:C.\\b:C.b))"))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)" + (termToOmega(expr.value1)(env) + (" " + ("(\\\\C:*.\\a:C.\\b:C.a) " + (termToOmega(expr.value2)(env) + ")"))));
        };
        if (expr instanceof Structures.T_unop) {
            return "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)" + (termToOmega(expr.value1)(env) + (" " + ("(\\\\C:*.\\a:C.\\b:C.b) " + "(\\\\C:*.\\a:C.\\b:C.a))")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(" + (termToOmega(TermLibrary.subTerm)(env) + (" " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(" + (termToOmega(TermLibrary.eqTerm)(env) + (" " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(" + (termToOmega(TermLibrary.neTerm)(env) + (" " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(" + (termToOmega(TermLibrary.gtTerm)(env) + (" " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(" + (termToOmega(TermLibrary.ltTerm)(env) + (" " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(" + (termToOmega(expr.value0)(env) + (" [" + (makeTypesOmega(TypeSystem.typeInfer(env)(expr.value2)) + ("] " + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_error) {
            return "ERRO";
        };
        throw new Error("Failed pattern match at CompileOmega (line 54, column 24 - line 152, column 30): " + [ expr.constructor.name ]);
    };
};
var makeLOmegaNewSim = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToOmegaNewSim(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $287 = eq(expr)(Structures.T_error.value);
        if ($287) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 528, column 25 - line 530, column 97): " + [ v.constructor.name ]);
};
var makeLOmega = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToOmega(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $290 = eq(expr)(Structures.T_error.value);
        if ($290) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 523, column 19 - line 525, column 97): " + [ v.constructor.name ]);
};
var makeDefOmegaNewSim = function (str) {
    if (str === "true") {
        return "  true    = \\C:*. \\a: C. \\b: C. a;";
    };
    if (str === "false") {
        return "  false   = \\C:*. \\a: C. \\b: C. b;";
    };
    if (str === "if") {
        return "  if      = \\D:*. \\c: Bool. \\a: D. \\b: D. c D a b;";
    };
    if (str === "pair") {
        return "  pair    = \\A:*. \\B:*. \\a: A. \\b: B. \\C:*. \\f: A->B->C. f a b;";
    };
    if (str === "fst") {
        return "  fst     = \\A:*. \\B:*. \\p: Pair A B. p A (\\a: A.\\b: B. a);";
    };
    if (str === "snd") {
        return "  snd     = \\A:*. \\B:*. \\p: Pair A B. p B (\\a: A.\\b: B. b);";
    };
    if (str === "add") {
        return "  add     = \\n: Nat. \\m: Nat. \\C:*. \\f: C -> C. \\x :C. m C f (n C f x);";
    };
    if (str === "mult") {
        return "  mult    = \\n: Nat. \\m: Nat. \\C:*. \\f: C -> C. \\x :C. n C (m C f) x;";
    };
    if (str === "and") {
        return "  and     = \\a: Bool. \\b: Bool. a Bool b (\\C:*. \\a: C. \\b: C. b);";
    };
    if (str === "or") {
        return "  or      = \\a: Bool. \\b: Bool. a Bool (\\C:*. \\a: C. \\b: C. a) b;";
    };
    if (str === "not") {
        return "  not     = \\a: Bool. a Bool (\\C:*. \\a: C. \\b: C. b) (\\C:*. \\a: C. \\b: C. a);";
    };
    if (str === "succ") {
        return "  succ    = \\n: Nat. \\C:*. \\f: C -> C. \\x :C. f (n C f x);";
    };
    if (str === "sub") {
        return "  sub     = \\n: Nat. \\m:Nat. m Nat (\\n: Nat. fst Nat Nat (n (Pair Nat Nat) (\\p: Pair Nat Nat. (pair Nat Nat (snd Nat Nat p) (succ (snd Nat Nat p)))) (pair Nat Nat 0 0))) n;";
    };
    if (str === "isZero") {
        return "  isZero  = \\n:Nat. n Bool (\\b: Bool. (\\C:*. \\a: C. \\b: C. b)) (\\C:*. \\a: C. \\b: C. a);";
    };
    if (str === "eq") {
        return "  eq     = \\n:Nat. \\m:Nat. and (isZero (sub n m)) (isZero (sub m n));";
    };
    if (str === "ne") {
        return "  ne     = \\n:Nat. \\m:Nat. not (and (isZero (sub n m)) (isZero (sub m n)));";
    };
    if (str === "gt") {
        return "  gt     = \\n:Nat. \\m:Nat. not (isZero (sub n m)) ;";
    };
    if (str === "lt") {
        return "  lt     = \\n:Nat. \\m:Nat. not (isZero (sub m n)) ;";
    };
    if (str === "natRec") {
        return "  natRec  = \\C:*. \\n:Nat. \\step: C -> C. \\init:C. n C step init;";
    };
    return "?";
};
var makeDefsBlockNewSim = function (l) {
    return "typedef\x0a" + ("  Bool    = forall C:*. C -> C -> C;\x0a" + ("  Nat     = forall C:*. (C -> C) -> C -> C;\x0a" + ("  Pair    = \\A:*. \\B:*. forall C:*. (A -> B -> C) -> C;\x0a" + ("end\x0a" + (function () {
        if (l instanceof Data_List_Types.Cons) {
            return "let\x0a" + (Structures.makeDefsUsed(makeDefOmegaNewSim)(l) + "in\x0a\x0a");
        };
        if (l instanceof Data_List_Types.Nil) {
            return "\x0a\x0a";
        };
        throw new Error("Failed pattern match at CompileOmega (line 516, column 9 - line 518, column 28): " + [ l.constructor.name ]);
    })()))));
};
var makeLOmegaDefsNewSim = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlockNewSim(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToOmegaDefsNewSim(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $297 = eq(expr)(Structures.T_error.value);
        if ($297) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 539, column 29 - line 541, column 97): " + [ v.constructor.name ]);
};
var makeDefOmega = function (str) {
    if (str === "true") {
        return "  true    = \\\\C:*. \\a: C. \\b: C. a;";
    };
    if (str === "false") {
        return "  false   = \\\\C:*. \\a: C. \\b: C. b;";
    };
    if (str === "if") {
        return "  if      = \\\\D:*. \\c: Bool. \\a: D. \\b: D. c [D] a b;";
    };
    if (str === "pair") {
        return "  pair    = \\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b;";
    };
    if (str === "fst") {
        return "  fst     = \\\\A:*. \\\\B:*. \\p: Pair A B. p [A] (\\a: A.\\b: B. a);";
    };
    if (str === "snd") {
        return "  snd     = \\\\A:*. \\\\B:*. \\p: Pair A B. p [B] (\\a: A.\\b: B. b);";
    };
    if (str === "add") {
        return "  add     = \\n: Nat. \\m: Nat. \\\\C:*. \\f: C -> C. \\x :C. m [C] f (n [C] f x);";
    };
    if (str === "mult") {
        return "  mult    = \\n: Nat. \\m: Nat. \\\\C:*. \\f: C -> C. \\x :C. n [C] (m [C] f) x;";
    };
    if (str === "and") {
        return "  and     = \\a: Bool. \\b: Bool. a [Bool] b (\\\\C:*. \\a: C. \\b: C. b);";
    };
    if (str === "or") {
        return "  or      = \\a: Bool. \\b: Bool. a [Bool] (\\\\C:*. \\a: C. \\b: C. a) b;";
    };
    if (str === "not") {
        return "  not     = \\a: Bool. a [Bool] (\\\\C:*. \\a: C. \\b: C. b) (\\\\C:*. \\a: C. \\b: C. a);";
    };
    if (str === "succ") {
        return "  succ    = \\n: Nat. \\\\C:*. \\f: C -> C. \\x :C. f (n [C] f x);";
    };
    if (str === "sub") {
        return "  sub     = \\n: Nat. \\m:Nat. m [Nat] (\\n: Nat. fst [Nat] [Nat] (n [Pair Nat Nat] (\\p: Pair Nat Nat. (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)))) (pair [Nat] [Nat] 0 0))) n;";
    };
    if (str === "isZero") {
        return "  isZero  = \\n:Nat. n [Bool] (\\b: Bool. (\\\\C:*. \\a: C. \\b: C. b)) (\\\\C:*. \\a: C. \\b: C. a);";
    };
    if (str === "eq") {
        return "  eq     = \\n:Nat. \\m:Nat. and (isZero (sub n m)) (isZero (sub m n));";
    };
    if (str === "ne") {
        return "  ne     = \\n:Nat. \\m:Nat. not (and (isZero (sub n m)) (isZero (sub m n)));";
    };
    if (str === "gt") {
        return "  gt     = \\n:Nat. \\m:Nat. not (isZero (sub n m)) ;";
    };
    if (str === "lt") {
        return "  lt     = \\n:Nat. \\m:Nat. not (isZero (sub m n)) ;";
    };
    if (str === "natRec") {
        return "  natRec  = \\\\C:*. \\n:Nat. \\step: C -> C. \\init:C. n [C] step init;";
    };
    return "?";
};
var makeDefsBlock = function (l) {
    return "typedef\x0a" + ("  Bool    = forall C:*, C -> C -> C;\x0a" + ("  Nat     = forall C:*, (C -> C) -> C -> C;\x0a" + ("  Pair    = \\A:*, \\B:*, forall C:*, (A -> B -> C) -> C;\x0a" + ("end\x0a" + (function () {
        if (l instanceof Data_List_Types.Cons) {
            return "let\x0a" + (Structures.makeDefsUsed(makeDefOmega)(l) + "in\x0a\x0a");
        };
        if (l instanceof Data_List_Types.Nil) {
            return "\x0a\x0a";
        };
        throw new Error("Failed pattern match at CompileOmega (line 506, column 9 - line 508, column 28): " + [ l.constructor.name ]);
    })()))));
};
var makeLOmegaDefs = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlock(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToOmegaDefs(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $304 = eq(expr)(Structures.T_error.value);
        if ($304) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 534, column 23 - line 536, column 97): " + [ v.constructor.name ]);
};
export {
    makeTypesOmega,
    makeTypesOmegaNewSim,
    makeTypesOmegaDefs,
    termToOmega,
    termToOmegaDefs,
    makeDefOmega,
    termToOmegaNewSim,
    termToOmegaDefsNewSim,
    makeDefOmegaNewSim,
    makeDefsBlock,
    makeDefsBlockNewSim,
    makeLOmega,
    makeLOmegaNewSim,
    makeLOmegaDefs,
    makeLOmegaDefsNewSim
};
