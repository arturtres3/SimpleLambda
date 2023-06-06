import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TermLibrary from "../TermLibrary/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(Structures.eqTerm);
var makeTypesL2NewSim = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(forall C:*. C -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "(forall C:*. (C -> C) -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(forall C:*. (" + (makeTypesL2NewSim(new Data_Maybe.Just(t.value0.value0)) + (" -> " + (makeTypesL2NewSim(new Data_Maybe.Just(t.value0.value1)) + " -> C) -> C) ")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesL2NewSim(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesL2NewSim(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 27, column 23 - line 37, column 42): " + [ t.constructor.name ]);
};
var termToL2NewSim = function (expr) {
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
            return "(\\x_" + (expr.value0 + (": " + (makeTypesL2NewSim(new Data_Maybe.Just(expr.value1)) + (". " + (termToL2NewSim(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_func_system) {
            return "(\\" + (expr.value0 + (": " + (makeTypesL2NewSim(new Data_Maybe.Just(expr.value1)) + (". " + (termToL2NewSim(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_var_system) {
            return expr.value0;
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToL2NewSim(expr.value0)(env) + (" " + (termToL2NewSim(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToL2NewSim(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "((\\D:*. \\c:(forall C:*. C -> C -> C). \\a:D. \\b:D. (c D) a b)" + (" " + (makeTypesL2NewSim(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToL2NewSim(expr.value0)(env) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "((\\A:*. \\B:*. \\a: A. \\b: B. \\C:*. \\f: A->B->C. f a b)" + (" " + (makeTypesL2NewSim(TypeSystem.typeInfer(env)(expr.value0)) + (" " + (makeTypesL2NewSim(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToL2NewSim(expr.value0)(env) + (" " + (termToL2NewSim(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "((\\A:*. \\B:*. \\p: (forall C:*. (A -> B -> C) -> C). p A (\\a: A.\\b: B. a))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesL2NewSim(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesL2NewSim(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToL2NewSim(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "((\\A:*. \\B:*. \\p: (forall C:*. (A -> B -> C) -> C). p B (\\a: A.\\b: B. b))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesL2NewSim(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesL2NewSim(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToL2NewSim(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "((\\n: (forall C:*. (C -> C) -> C -> C). \\m: (forall C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. m C f (n C f x)) " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "((\\n: (forall C:*. (C -> C) -> C -> C). \\m: (forall C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. n C (m C f) x) " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "((\\c:(forall C:*. C -> C -> C). \\a:(forall C:*. C -> C -> C). \\b:(forall C:*. C -> C -> C). (c (forall C:*. C -> C -> C)) a b)" + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + (" " + "(\\C:*.\\a:C.\\b:C.b))"))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "((\\c:(forall C:*. C -> C -> C). \\a:(forall C:*. C -> C -> C). \\b:(forall C:*. C -> C -> C). (c (forall C:*. C -> C -> C)) a b)" + (termToL2NewSim(expr.value1)(env) + (" " + ("(\\C:*.\\a:C.\\b:C.a) " + (termToL2NewSim(expr.value2)(env) + ")"))));
        };
        if (expr instanceof Structures.T_unop) {
            return "((\\c:(forall C:*. C -> C -> C). \\a:(forall C:*. C -> C -> C). \\b:(forall C:*. C -> C -> C). (c (forall C:*. C -> C -> C)) a b)" + (termToL2NewSim(expr.value1)(env) + (" " + ("(\\C:*.\\a:C.\\b:C.b) " + "(\\C:*.\\a:C.\\b:C.a))")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(" + (termToL2NewSim(TermLibrary.subTerm)(env) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(" + (termToL2NewSim(TermLibrary.eqTerm)(env) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(" + (termToL2NewSim(TermLibrary.neTerm)(env) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(" + (termToL2NewSim(TermLibrary.gtTerm)(env) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(" + (termToL2NewSim(TermLibrary.ltTerm)(env) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(" + (termToL2NewSim(expr.value0)(env) + (" " + (makeTypesL2NewSim(TypeSystem.typeInfer(env)(expr.value2)) + (" " + (termToL2NewSim(expr.value1)(env) + (" " + (termToL2NewSim(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeTypesL2DefsNewSim = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(forall C:*. (" + (makeTypesL2DefsNewSim(new Data_Maybe.Just(t.value0.value0)) + (" -> " + (makeTypesL2DefsNewSim(new Data_Maybe.Just(t.value0.value1)) + " -> C) -> C) ")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesL2DefsNewSim(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesL2DefsNewSim(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 53, column 27 - line 63, column 42): " + [ t.constructor.name ]);
};
var termToNewSimL2Defs = function (expr) {
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
            return "(\\x_" + (expr.value0 + (": " + (makeTypesL2DefsNewSim(new Data_Maybe.Just(expr.value1)) + (". " + (termToNewSimL2Defs(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToNewSimL2Defs(expr.value0)(env) + (" " + (termToNewSimL2Defs(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToNewSimL2Defs(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "(if " + (makeTypesL2DefsNewSim(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToNewSimL2Defs(expr.value0)(env) + (" " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "(pair " + (makeTypesL2DefsNewSim(TypeSystem.typeInfer(env)(expr.value0)) + (" " + (makeTypesL2DefsNewSim(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToNewSimL2Defs(expr.value0)(env) + (" " + (termToNewSimL2Defs(expr.value1)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "(fst " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesL2DefsNewSim(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesL2DefsNewSim(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToNewSimL2Defs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "(snd " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesL2DefsNewSim(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesL2DefsNewSim(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToNewSimL2Defs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "(add " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "(mult " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "(and " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "(or " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop) {
            return "(not " + (termToNewSimL2Defs(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(sub " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(eq " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(ne " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(gt " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(lt " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(natRec " + (makeTypesL2DefsNewSim(TypeSystem.typeInfer(env)(expr.value2)) + (" " + (termToNewSimL2Defs(expr.value0)(env) + (" " + (termToNewSimL2Defs(expr.value1)(env) + (" " + (termToNewSimL2Defs(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeTypesL2Defs = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(forall C, (" + (makeTypesL2Defs(new Data_Maybe.Just(t.value0.value0)) + (" -> " + (makeTypesL2Defs(new Data_Maybe.Just(t.value0.value1)) + " -> C) -> C) ")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesL2Defs(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesL2Defs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 40, column 21 - line 50, column 42): " + [ t.constructor.name ]);
};
var termToL2Defs = function (expr) {
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
            return "(\\x_" + (expr.value0 + (": " + (makeTypesL2Defs(new Data_Maybe.Just(expr.value1)) + (". " + (termToL2Defs(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToL2Defs(expr.value0)(env) + (" " + (termToL2Defs(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToL2Defs(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "(if " + ("[" + (makeTypesL2Defs(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToL2Defs(expr.value0)(env) + (" " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "(pair " + ("[" + (makeTypesL2Defs(TypeSystem.typeInfer(env)(expr.value0)) + ("][" + (makeTypesL2Defs(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToL2Defs(expr.value0)(env) + (" " + (termToL2Defs(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "(fst " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return "[" + (makeTypesL2Defs(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesL2Defs(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToL2Defs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "(snd " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return "[" + (makeTypesL2Defs(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesL2Defs(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToL2Defs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "(add " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "(mult " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "(and " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "(or " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop) {
            return "(not " + (termToL2Defs(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(sub " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(eq " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(ne " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(gt " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(lt " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(natRec [" + (makeTypesL2Defs(TypeSystem.typeInfer(env)(expr.value2)) + ("] " + (termToL2Defs(expr.value0)(env) + (" " + (termToL2Defs(expr.value1)(env) + (" " + (termToL2Defs(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeTypesL2 = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(forall C,C->C->C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "(forall C, (C -> C) -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(forall C, (" + (makeTypesL2(new Data_Maybe.Just(t.value0.value0)) + (" -> " + (makeTypesL2(new Data_Maybe.Just(t.value0.value1)) + " -> C) -> C) ")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesL2(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesL2(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 14, column 17 - line 24, column 42): " + [ t.constructor.name ]);
};
var termToL2 = function (expr) {
    return function (env) {
        if (expr instanceof Structures.T_true) {
            return "(\\\\C.\\a:C.\\b:C.a)";
        };
        if (expr instanceof Structures.T_false) {
            return "(\\\\C.\\a:C.\\b:C.b)";
        };
        if (expr instanceof Structures.T_var) {
            return "x_" + expr.value0;
        };
        if (expr instanceof Structures.T_num) {
            return "(\\\\C.\\f:C->C.\\x:C." + (Structures.makeNatural(expr.value0) + ")");
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (makeTypesL2(new Data_Maybe.Just(expr.value1)) + (". " + (termToL2(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_func_system) {
            return "(\\" + (expr.value0 + (": " + (makeTypesL2(new Data_Maybe.Just(expr.value1)) + (". " + (termToL2(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_var_system) {
            return expr.value0;
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToL2(expr.value0)(env) + (" " + (termToL2(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToL2(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "((\\\\D. \\c:(forall C, C -> C -> C). \\a:D. \\b:D. (c [D]) a b)" + (" [" + (makeTypesL2(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToL2(expr.value0)(env) + (" " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "((\\\\A. \\\\B. \\a: A. \\b: B. \\\\C. \\f: A -> B -> C. f a b)" + (" [" + (makeTypesL2(TypeSystem.typeInfer(env)(expr.value0)) + ("][" + (makeTypesL2(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToL2(expr.value0)(env) + (" " + (termToL2(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "((\\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [A] (\\a: A.\\b: B. a))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " [" + (makeTypesL2(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesL2(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToL2(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "((\\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [B] (\\a: A.\\b: B. b))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " [" + (makeTypesL2(new Data_Maybe.Just(v.value0.value0)) + ("][" + (makeTypesL2(new Data_Maybe.Just(v.value0.value1)) + "] ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToL2(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "((\\n: (forall C, (C -> C) -> C -> C). \\m: (forall C, (C -> C) -> C -> C). \\\\C. \\f: C -> C. \\x :C. m [C] f (n [C] f x)) " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "((\\n: (forall C, (C -> C) -> C -> C). \\m: (forall C, (C -> C) -> C -> C). \\\\C. \\f: C -> C. \\x :C. n [C] (m [C] f) x) " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "((\\c:(forall C, C -> C -> C). \\a:(forall C, C -> C -> C). \\b:(forall C, C -> C -> C). (c [(forall C, C -> C -> C)]) a b)" + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + (" " + "(\\\\C.\\a:C.\\b:C.b))"))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "((\\c:(forall C, C -> C -> C). \\a:(forall C, C -> C -> C). \\b:(forall C, C -> C -> C). (c [(forall C, C -> C -> C)]) a b)" + (termToL2(expr.value1)(env) + (" " + ("(\\\\C.\\a:C.\\b:C.a) " + (termToL2(expr.value2)(env) + ")"))));
        };
        if (expr instanceof Structures.T_unop) {
            return "((\\c:(forall C, C -> C -> C). \\a:(forall C, C -> C -> C). \\b:(forall C, C -> C -> C). (c [(forall C, C -> C -> C)]) a b)" + (termToL2(expr.value1)(env) + (" " + ("(\\\\C.\\a:C.\\b:C.b) " + "(\\\\C.\\a:C.\\b:C.a))")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(" + (termToL2(TermLibrary.subTerm)(env) + (" " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(" + (termToL2(TermLibrary.eqTerm)(env) + (" " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(" + (termToL2(TermLibrary.neTerm)(env) + (" " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(" + (termToL2(TermLibrary.gtTerm)(env) + (" " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(" + (termToL2(TermLibrary.ltTerm)(env) + (" " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(" + (termToL2(expr.value0)(env) + (" [" + (makeTypesL2(TypeSystem.typeInfer(env)(expr.value2)) + ("] " + (termToL2(expr.value1)(env) + (" " + (termToL2(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeL2NewSim = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToL2NewSim(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $296 = eq(expr)(Structures.T_error.value);
        if ($296) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 549, column 21 - line 551, column 97): " + [ v.constructor.name ]);
};
var makeL2 = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToL2(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $299 = eq(expr)(Structures.T_error.value);
        if ($299) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 538, column 15 - line 540, column 97): " + [ v.constructor.name ]);
};
var makeDefNewSimL2 = function (str) {
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
        return "  fst     = \\A:*. \\B:*. \\p: (forall C:*. (A -> B -> C) -> C). p A (\\a: A.\\b: B. a);";
    };
    if (str === "snd") {
        return "  snd     = \\A:*. \\B:*. \\p: (forall C:*. (A -> B -> C) -> C). p B (\\a: A.\\b: B. b);";
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
        return "  sub     = \\n: Nat. \\m:Nat. m Nat (\\n: Nat. fst Nat Nat (n (forall C:*. (Nat -> Nat -> C) -> C) (\\p: (forall C:*. (Nat -> Nat -> C) -> C). (pair Nat Nat (snd Nat Nat p) (succ (snd Nat Nat p)))) (pair Nat Nat 0 0))) n;";
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
    return "typedef\x0a" + ("  Bool    = forall C:*. C -> C -> C;\x0a" + ("  Nat     = forall C:*. (C -> C) -> C -> C;\x0a" + ("end\x0a" + (function () {
        if (l instanceof Data_List_Types.Cons) {
            return "let\x0a" + (Structures.makeDefsUsed(makeDefNewSimL2)(l) + "in\x0a\x0a");
        };
        if (l instanceof Data_List_Types.Nil) {
            return "\x0a\x0a";
        };
        throw new Error("Failed pattern match at CompileLambda2 (line 531, column 9 - line 533, column 28): " + [ l.constructor.name ]);
    })())));
};
var makeL2DefsNewSim = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlockNewSim(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToNewSimL2Defs(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $306 = eq(expr)(Structures.T_error.value);
        if ($306) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 554, column 25 - line 556, column 97): " + [ v.constructor.name ]);
};
var makeDefL2 = function (str) {
    if (str === "true") {
        return "  true    = \\\\C. \\a: C. \\b: C. a;";
    };
    if (str === "false") {
        return "  false   = \\\\C. \\a: C. \\b: C. b;";
    };
    if (str === "if") {
        return "  if      = \\\\D. \\c: Bool. \\a: D. \\b: D. c [D] a b;";
    };
    if (str === "pair") {
        return "  pair    = \\\\A. \\\\B. \\a: A. \\b: B. \\\\C. \\f: A->B->C. f a b;";
    };
    if (str === "fst") {
        return "  fst     = \\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [A] (\\a: A.\\b: B. a);";
    };
    if (str === "snd") {
        return "  snd     = \\\\A. \\\\B. \\p: (forall C, (A -> B -> C) -> C). p [B] (\\a: A.\\b: B. b);";
    };
    if (str === "add") {
        return "  add     = \\n: Nat. \\m: Nat. \\\\C. \\f: C -> C. \\x :C. m [C] f (n [C] f x);";
    };
    if (str === "mult") {
        return "  mult    = \\n: Nat. \\m: Nat. \\\\C. \\f: C -> C. \\x :C. n [C] (m [C] f) x;";
    };
    if (str === "and") {
        return "  and     = \\a: Bool. \\b: Bool. a [Bool] b (\\\\C. \\a: C. \\b: C. b);";
    };
    if (str === "or") {
        return "  or      = \\a: Bool. \\b: Bool. a [Bool] (\\\\C. \\a: C. \\b: C. a) b;";
    };
    if (str === "not") {
        return "  not     = \\a: Bool. a [Bool] (\\\\C. \\a: C. \\b: C. b) (\\\\C. \\a: C. \\b: C. a);";
    };
    if (str === "succ") {
        return "  succ    = \\n: Nat. \\\\C. \\f: C -> C. \\x :C. f (n [C] f x);";
    };
    if (str === "sub") {
        return "  sub     = \\n: Nat. \\m:Nat. m [Nat] (\\n: Nat. fst [Nat] [Nat] (n [(forall C, (Nat -> Nat -> C) -> C)] (\\p: (forall C, (Nat -> Nat -> C) -> C). (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)))) (pair [Nat] [Nat] 0 0))) n;";
    };
    if (str === "isZero") {
        return "  isZero  = \\n:Nat. n [Bool] (\\b: Bool. (\\\\C. \\a: C. \\b: C. b)) (\\\\C. \\a: C. \\b: C. a);";
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
        return "  natRec  = \\\\C. \\n:Nat. \\step: C -> C. \\init:C. n [C] step init;";
    };
    return "?";
};
var makeDefsBlock = function (l) {
    return "typedef\x0a" + ("  Bool    = forall C, C -> C -> C;\x0a" + ("  Nat     = forall C, (C -> C) -> C -> C;\x0a" + ("end\x0a" + (function () {
        if (l instanceof Data_List_Types.Cons) {
            return "let\x0a" + (Structures.makeDefsUsed(makeDefL2)(l) + "in\x0a\x0a");
        };
        if (l instanceof Data_List_Types.Nil) {
            return "\x0a\x0a";
        };
        throw new Error("Failed pattern match at CompileLambda2 (line 521, column 9 - line 523, column 28): " + [ l.constructor.name ]);
    })())));
};
var makeL2Defs = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlock(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToL2Defs(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $313 = eq(expr)(Structures.T_error.value);
        if ($313) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambda2 (line 543, column 19 - line 545, column 97): " + [ v.constructor.name ]);
};
export {
    makeTypesL2,
    makeTypesL2NewSim,
    makeTypesL2Defs,
    makeTypesL2DefsNewSim,
    termToL2,
    termToL2Defs,
    makeDefL2,
    termToL2NewSim,
    termToNewSimL2Defs,
    makeDefNewSimL2,
    makeDefsBlock,
    makeDefsBlockNewSim,
    makeL2,
    makeL2Defs,
    makeL2NewSim,
    makeL2DefsNewSim
};
