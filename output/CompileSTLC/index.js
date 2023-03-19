import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var notEq = /* #__PURE__ */ Data_Eq.notEq(Structures.eqTermType);
var eq = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Structures.eqTermType));
var eq1 = /* #__PURE__ */ Data_Eq.eq(Structures.eqTermType);

// versão simples 
var validIfSelectorsSimple = function (expr) {
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
        return validIfSelectorsSimple(expr.value1) && validIfSelectorsSimple(expr.value2);
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
        return validIfSelectorsSimple(expr.value1) && validIfSelectorsSimple(expr.value2);
    };
    if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
        return validIfSelectorsSimple(expr.value1);
    };
    if (expr instanceof Structures.T_true) {
        return true;
    };
    if (expr instanceof Structures.T_false) {
        return true;
    };
    return false;
};

// unica restricao de 
var validIfSelector = function (expr) {
    if (expr instanceof Structures.T_true) {
        return true;
    };
    if (expr instanceof Structures.T_false) {
        return true;
    };
    if (expr instanceof Structures.T_num) {
        return true;
    };
    if (expr instanceof Structures.T_var) {
        return false;
    };
    if (expr instanceof Structures.T_var_system) {
        return false;
    };
    if (expr instanceof Structures.T_if) {
        return validIfSelector(expr.value0) && (validIfSelector(expr.value1) && validIfSelector(expr.value2));
    };
    if (expr instanceof Structures.T_fst) {
        return validIfSelector(expr.value0);
    };
    if (expr instanceof Structures.T_snd) {
        return validIfSelector(expr.value0);
    };
    if (expr instanceof Structures.T_app) {
        return validIfSelector(expr.value0) && validIfSelector(expr.value1);
    };
    if (expr instanceof Structures.T_pair) {
        return validIfSelector(expr.value0) && validIfSelector(expr.value1);
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
        return false;
    };
    if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Negate) {
        return false;
    };
    if (expr instanceof Structures.T_unop) {
        return validIfSelector(expr.value1);
    };
    if (expr instanceof Structures.T_binop) {
        return validIfSelector(expr.value1) && validIfSelector(expr.value2);
    };
    if (expr instanceof Structures.T_let) {
        return validIfSelector(expr.value2) && validIfSelector(expr.value3);
    };
    if (expr instanceof Structures.T_func) {
        return validIfSelector(expr.value2);
    };
    if (expr instanceof Structures.T_func_system) {
        return validIfSelector(expr.value2);
    };
    if (expr instanceof Structures.T_natRec) {
        return validIfSelector(expr.value0) && (validIfSelector(expr.value1) && validIfSelector(expr.value2));
    };
    throw new Error("Failed pattern match at CompileSTLC (line 420, column 24 - line 445, column 96): " + [ expr.constructor.name ]);
};
var typesSTLC = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(A->A->A)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "((A->A)->A->A)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (typesSTLC(new Data_Maybe.Just(t.value0.value0)) + ("->" + (typesSTLC(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        var $79 = notEq(t.value0.value0)(t.value0.value1);
        if ($79) {
            return "Pares devem ter o mesmo tipo";
        };
        var t1String = typesSTLC(new Data_Maybe.Just(t.value0.value0));
        return "((" + (t1String + (" -> " + (t1String + (" -> " + (t1String + (") -> " + (t1String + ")")))))));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "A";
    };
    throw new Error("Failed pattern match at CompileSTLC (line 13, column 15 - line 23, column 37): " + [ t.constructor.name ]);
};
var typesSTLCDefs = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(A->A->A)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "((A->A)->A->A)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (typesSTLC(new Data_Maybe.Just(t.value0.value0)) + ("->" + (typesSTLC(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "((A->A->A) -> ((A->A)->A->A))";
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "A";
    };
    throw new Error("Failed pattern match at CompileSTLC (line 26, column 19 - line 33, column 37): " + [ t.constructor.name ]);
};
var termToSTLCSimple = function (expr) {
    return function (env) {
        if (expr instanceof Structures.T_true) {
            return "(\\a:A.\\b:A.a)";
        };
        if (expr instanceof Structures.T_false) {
            return "(\\a:A.\\b:A.b)";
        };
        if (expr instanceof Structures.T_var) {
            return "x_" + expr.value0;
        };
        if (expr instanceof Structures.T_if) {
            return "((\\b:(A->A->A). \\e1:A. \\e2:A. b e1 e2) " + (termToSTLCSimple(expr.value0)(env) + (" " + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_pair) {
            var t1 = TypeSystem.typeInfer(env)(expr.value0);
            var t2 = TypeSystem.typeInfer(env)(expr.value1);
            var $97 = eq(t1)(t2);
            if ($97) {
                return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\b:(A->A->A).\\f:A->A.\\x:A. b (n1 f x) (n2 f x)) " + (termToSTLCSimple(expr.value0)(env) + (" " + (termToSTLCSimple(expr.value1)(env) + ")")));
            };
            return "ERRO valores no par devem ser do mesmo tipo";
        };
        if (expr instanceof Structures.T_fst) {
            var v = TypeSystem.typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                var $101 = eq1(v.value0.value0)(v.value0.value1);
                if ($101) {
                    return "((\\p: (A->A->A) -> ((A->A)->A->A). \\f:A->A.\\x:A. (p " + (termToSTLCSimple(Structures.T_true.value)(env) + (") f x)" + (termToSTLCSimple(expr.value0)(env) + ")")));
                };
                return "ERRO valores no par devem ser do mesmo tipo";
            };
            return "ERRO de tipo, fst deve receber par";
        };
        if (expr instanceof Structures.T_snd) {
            var v = TypeSystem.typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                var $107 = eq1(v.value0.value0)(v.value0.value1);
                if ($107) {
                    return "((\\p: (A->A->A) -> ((A->A)->A->A). \\f:A->A.\\x:A. (p " + (termToSTLCSimple(Structures.T_false.value)(env) + (") f x)" + (termToSTLCSimple(expr.value0)(env) + ")")));
                };
                return "ERRO valores no par devem ser do mesmo tipo";
            };
            return "ERRO de tipo, fst deve receber par";
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToSTLCSimple(expr.value0)(env) + (" " + (termToSTLCSimple(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToSTLCSimple(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (typesSTLC(new Data_Maybe.Just(expr.value1)) + (". " + (termToSTLCSimple(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_num) {
            return "(\\f:A->A.\\x:A." + (Structures.makeNatural(expr.value0) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A. \\x:A. n1 f (n2 f x))" + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A.\\x:A. n1 (n2 f) x)" + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "((\\bin1: (A->A->A). \\bin2: (A->A->A). \\a: A. \\b: A. bin1 (bin2 a b) b) " + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "((\\bin1: (A->A->A). \\bin2: (A->A->A). \\a: A. \\b: A. bin1 a (bin2 a b)) " + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return "((\\bin: (A->A->A). \\a: A. \\b: A. bin b a) " + (termToSTLCSimple(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_natRec) {
            return "(" + (termToSTLCSimple(expr.value0)(env) + (" " + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")))));
        };
        return "incompleto";
    };
};
var termToSTLCDefs = function (expr) {
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
        if (expr instanceof Structures.T_if) {
            return "(if " + (termToSTLCDefs(expr.value0)(env) + (" " + (termToSTLCDefs(expr.value1)(env) + (" " + (termToSTLCDefs(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_pair) {
            var t1 = TypeSystem.typeInfer(env)(expr.value0);
            var t2 = TypeSystem.typeInfer(env)(expr.value1);
            var $144 = eq(t1)(t2);
            if ($144) {
                return "(pair " + (termToSTLCDefs(expr.value0)(env) + (" " + (termToSTLCDefs(expr.value1)(env) + ")")));
            };
            return "ERRO valores no par devem ser do mesmo tipo";
        };
        if (expr instanceof Structures.T_fst) {
            var v = TypeSystem.typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                var $148 = eq1(v.value0.value0)(v.value0.value1);
                if ($148) {
                    return "(fst " + (termToSTLCDefs(expr.value0)(env) + ")");
                };
                return "ERRO valores no par devem ser do mesmo tipo";
            };
            return "ERRO de tipo, fst deve receber par";
        };
        if (expr instanceof Structures.T_snd) {
            var v = TypeSystem.typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                var $154 = eq1(v.value0.value0)(v.value0.value1);
                if ($154) {
                    return "(snd " + (termToSTLCDefs(expr.value0)(env) + ")");
                };
                return "ERRO valores no par devem ser do mesmo tipo";
            };
            return "ERRO de tipo, fst deve receber par";
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToSTLCDefs(expr.value0)(env) + (" " + (termToSTLCDefs(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToSTLCDefs(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (typesSTLCDefs(new Data_Maybe.Just(expr.value1)) + (". " + (termToSTLCDefs(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_num) {
            return Data_Int.toStringAs(Data_Int.decimal)(expr.value0);
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "(add " + (termToSTLCDefs(expr.value1)(env) + (" " + (termToSTLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "(mult " + (termToSTLCDefs(expr.value1)(env) + (" " + (termToSTLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "(and " + (termToSTLCDefs(expr.value1)(env) + (" " + (termToSTLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "(or " + (termToSTLCDefs(expr.value1)(env) + (" " + (termToSTLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return "(not " + (termToSTLCDefs(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_natRec) {
            return "(natRec " + (termToSTLCDefs(expr.value0)(env) + (" " + (termToSTLCDefs(expr.value1)(env) + (" " + (termToSTLCDefs(expr.value2)(env) + ")")))));
        };
        return "incompleto";
    };
};
var makePairTypeSTLC = function (t) {
    return "((" + (t + ("->" + (t + ("->" + (t + (") -> " + (t + ")")))))));
};
var makePairSTLC = function (t) {
    return "(\\e1: " + (t + (". \\e2: " + (t + (". \\b: (" + (t + (" -> " + (t + (" -> " + (t + "). b e1 e2)")))))))));
};
var makeIfSTLC = function (t) {
    return "(\\b: (" + (t + (" -> " + (t + (" -> " + (t + ("). \\e1: " + (t + (". \\e2: " + (t + ". b e1 e2)")))))))));
};
var makeDefSTLC = function (str) {
    if (str === "true") {
        return "  true    = \\a:A.\\b:A.a;";
    };
    if (str === "false") {
        return "  false   = \\a:A.\\b:A.b;";
    };
    if (str === "if") {
        return "  if      = \\b:(A->A->A). \\e1:A. \\e2:A. b e1 e2;";
    };
    if (str === "pair") {
        return "  pair    = \\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\b:(A->A->A).\\f:A->A.\\x:A. b (n1 f x) (n2 f x);";
    };
    if (str === "fst") {
        return "  fst     = \\p: (A->A->A) -> ((A->A)->A->A).\\f:A->A.\\x:A. (p (\\a:A.\\b:A.a)) f x;";
    };
    if (str === "snd") {
        return "  snd     = \\p: (A->A->A) -> ((A->A)->A->A).\\f:A->A.\\x:A. (p (\\a:A.\\b:A.b)) f x;";
    };
    if (str === "add") {
        return "  add     = \\n1:(A->A)->A->A. \\n2:(A->A)->A->A. \\f:A->A. \\x:A. n1 f (n2 f x);";
    };
    if (str === "mult") {
        return "  mult    = \\n1:(A->A)->A->A. \\n2:(A->A)->A->A. \\f:A->A. \\x:A. n1 (n2 f) x;";
    };
    if (str === "and") {
        return "  and     = \\bin1:(A->A->A). \\bin2:(A->A->A). \\a:A. \\b:A. bin1 (bin2 a b) b;";
    };
    if (str === "or") {
        return "  or      = \\bin1:(A->A->A). \\bin2:(A->A->A). \\a:A. \\b:A. bin1 a (bin2 a b);";
    };
    if (str === "not") {
        return "  not     = \\bin :(A->A->A). \\a:A. \\b:A. bin b a;";
    };
    if (str === "natRec") {
        return "  natRec  = \\n:Nat. \\step: A -> A. \\init:A. n step init;";
    };
    return "?";
};
var makeDefsUsed = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return "\x0a";
    };
    if (v instanceof Data_List_Types.Cons) {
        return makeDefSTLC(v.value0) + ("\x0a" + makeDefsUsed(v.value1));
    };
    throw new Error("Failed pattern match at CompileSTLC (line 369, column 1 - line 369, column 40): " + [ v.constructor.name ]);
};
var makeDefsBlock = function (l) {
    return "let\x0a" + (makeDefsUsed(l) + "in\x0a\x0a");
};
var makeBooleanTypeSTLC = function (t) {
    return "(" + (t + ("->" + (t + ("->" + (t + ")")))));
};
var termToSTLC = function (expr) {
    return function (t) {
        return function (env) {
            if (expr instanceof Structures.T_true) {
                return "(\\a:" + (typesSTLC(t) + (".\\b:" + (typesSTLC(t) + ".a)")));
            };
            if (expr instanceof Structures.T_false) {
                return "(\\a:" + (typesSTLC(t) + (".\\b:" + (typesSTLC(t) + ".b)")));
            };
            if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
                return "((\\bin1: " + (makeBooleanTypeSTLC(typesSTLC(t)) + (". " + ("\\bin2: " + (makeBooleanTypeSTLC(typesSTLC(t)) + (". " + ("\\a: " + (typesSTLC(t) + (". " + ("\\b: " + (typesSTLC(t) + (". " + ("bin1 (bin2 a b) b) " + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")))))))))))))));
            };
            if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
                return "((\\bin1: " + (makeBooleanTypeSTLC(typesSTLC(t)) + (". " + ("\\bin2: " + (makeBooleanTypeSTLC(typesSTLC(t)) + (". " + ("\\a: " + (typesSTLC(t) + (". " + ("\\b: " + (typesSTLC(t) + (". " + ("bin1 a (bin2 a b)) " + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")))))))))))))));
            };
            if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
                return "((\\bin: " + (makeBooleanTypeSTLC(typesSTLC(t)) + (". " + ("\\a: " + (typesSTLC(t) + (". " + ("\\b: " + (typesSTLC(t) + (". " + ("bin b a) " + (termToSTLC(expr.value1)(t)(env) + ")"))))))))));
            };
            if (expr instanceof Structures.T_var) {
                return "x_" + expr.value0;
            };
            if (expr instanceof Structures.T_func) {
                return "(\\x_" + (expr.value0 + (": " + (typesSTLC(new Data_Maybe.Just(expr.value1)) + (". " + (termToSTLC(expr.value2)(t)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
            };
            if (expr instanceof Structures.T_app) {
                return "(" + (termToSTLC(expr.value0)(t)(env) + (" " + (termToSTLC(expr.value1)(t)(env) + ")")));
            };
            if (expr instanceof Structures.T_let) {
                return termToSTLC(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(t)(env);
            };
            if (expr instanceof Structures.T_if) {
                var t1 = TypeSystem.typeInfer(env)(expr.value1);
                if (t instanceof Data_Maybe.Nothing) {
                    return "(" + (makeIfSTLC(typesSTLC(t1)) + (" " + (termToSTLC(expr.value0)(t1)(env) + (" " + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")))))));
                };
                if (t instanceof Data_Maybe.Just) {
                    if (t1 instanceof Data_Maybe.Just && t1.value0 instanceof Structures.Pair) {
                        var bool_t = new Structures.Func(t.value0, new Structures.Func(t.value0, t.value0));
                        return "(" + (makeIfSTLC(typesSTLC(new Data_Maybe.Just(new Structures.Pair(bool_t, bool_t)))) + (" " + (termToSTLC(expr.value0)(new Data_Maybe.Just(new Structures.Pair(bool_t, bool_t)))(env) + (" " + (termToSTLC(expr.value1)(new Data_Maybe.Just(t.value0))(env) + (" " + (termToSTLC(expr.value2)(new Data_Maybe.Just(t.value0))(env) + ")")))))));
                    };
                    return "(" + (makeIfSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (" " + (termToSTLC(expr.value0)(new Data_Maybe.Just(new Structures.Func(t.value0, new Structures.Func(t.value0, t.value0))))(env) + (" " + (termToSTLC(expr.value1)(new Data_Maybe.Just(t.value0))(env) + (" " + (termToSTLC(expr.value2)(new Data_Maybe.Just(t.value0))(env) + ")")))))));
                };
                throw new Error("Failed pattern match at CompileSTLC (line 89, column 31 - line 109, column 94): " + [ t.constructor.name ]);
            };
            if (expr instanceof Structures.T_pair) {
                var t1 = TypeSystem.typeInfer(env)(expr.value0);
                var t2 = TypeSystem.typeInfer(env)(expr.value1);
                var $218 = eq(t1)(t2);
                if ($218) {
                    if (t instanceof Data_Maybe.Nothing) {
                        return "(" + (makePairSTLC(typesSTLC(t1)) + (" " + (termToSTLC(expr.value0)(t)(env) + (" " + (termToSTLC(expr.value1)(t)(env) + ")")))));
                    };
                    if (t instanceof Data_Maybe.Just) {
                        return "(" + (makePairSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (" " + (termToSTLC(expr.value0)(new Data_Maybe.Just(t.value0))(env) + (" " + (termToSTLC(expr.value1)(new Data_Maybe.Just(t.value0))(env) + ")")))));
                    };
                    throw new Error("Failed pattern match at CompileSTLC (line 115, column 32 - line 123, column 85): " + [ t.constructor.name ]);
                };
                return "ERRO valores no par devem ser do mesmo tipo";
            };
            if (expr instanceof Structures.T_fst) {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    var $224 = eq1(v.value0.value0)(v.value0.value1);
                    if ($224) {
                        if (t instanceof Data_Maybe.Just) {
                            return "((\\p: " + (makePairTypeSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (". p " + (termToSTLC(Structures.T_true.value)(new Data_Maybe.Just(new Structures.Func(t.value0, new Structures.Func(t.value0, t.value0))))(env) + (")" + (termToSTLC(expr.value0)(new Data_Maybe.Just(t.value0))(env) + ")")))));
                        };
                        if (t instanceof Data_Maybe.Nothing) {
                            return "((\\p: " + (typesSTLC(TypeSystem.typeInfer(env)(expr.value0)) + (". p " + (termToSTLC(Structures.T_true.value)(new Data_Maybe.Just(v.value0.value0))(env) + (")" + (termToSTLC(expr.value0)(t)(env) + ")")))));
                        };
                        throw new Error("Failed pattern match at CompileSTLC (line 131, column 35 - line 142, column 68): " + [ t.constructor.name ]);
                    };
                    return "ERRO valores no par devem ser do mesmo tipo";
                };
                return "ERRO de tipo, fst deve receber par";
            };
            if (expr instanceof Structures.T_snd) {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    var $232 = eq1(v.value0.value0)(v.value0.value1);
                    if ($232) {
                        if (t instanceof Data_Maybe.Just) {
                            return "((\\p: " + (makePairTypeSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (". p " + (termToSTLC(Structures.T_false.value)(new Data_Maybe.Just(new Structures.Func(t.value0, new Structures.Func(t.value0, t.value0))))(env) + (")" + (termToSTLC(expr.value0)(t)(env) + ")")))));
                        };
                        if (t instanceof Data_Maybe.Nothing) {
                            return "((\\p: " + (typesSTLC(TypeSystem.typeInfer(env)(expr.value0)) + (". p " + (termToSTLC(Structures.T_false.value)(new Data_Maybe.Just(v.value0.value0))(env) + (")" + (termToSTLC(expr.value0)(t)(env) + ")")))));
                        };
                        throw new Error("Failed pattern match at CompileSTLC (line 150, column 35 - line 161, column 68): " + [ t.constructor.name ]);
                    };
                    return "ERRO valores no par devem ser do mesmo tipo";
                };
                return "ERRO de tipo, fst deve receber par";
            };
            if (expr instanceof Structures.T_num) {
                return "(\\f:A->A.\\x:A." + (Structures.makeNatural(expr.value0) + ")");
            };
            if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
                return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A. \\x:A. n1 f (n2 f x))" + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")));
            };
            if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
                return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A.\\x:A. n1 (n2 f) x)" + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")));
            };
            if (expr instanceof Structures.T_natRec) {
                return "(" + (termToSTLCSimple(expr.value0)(env) + (" " + (termToSTLCSimple(expr.value1)(env) + (" " + (termToSTLCSimple(expr.value2)(env) + ")")))));
            };
            return "incompleto";
        };
    };
};
var lastType = function ($copy_t) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(t) {
        if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
            $tco_done = true;
            return new Data_Maybe.Just(Structures.Nat.value);
        };
        if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
            $tco_done = true;
            return new Data_Maybe.Just(Structures.Bool.value);
        };
        if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
            $copy_t = new Data_Maybe.Just(t.value0.value0);
            return;
        };
        if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
            $copy_t = new Data_Maybe.Just(t.value0.value1);
            return;
        };
        if (t instanceof Data_Maybe.Nothing) {
            $tco_done = true;
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at CompileSTLC (line 181, column 14 - line 187, column 31): " + [ t.constructor.name ]);
    };
    while (!$tco_done) {
        $tco_result = $tco_loop($copy_t);
    };
    return $tco_result;
};
var canMakeSTLCSimple = function (expr) {
    if (expr instanceof Structures.T_true) {
        return true;
    };
    if (expr instanceof Structures.T_false) {
        return true;
    };
    if (expr instanceof Structures.T_num) {
        return true;
    };
    if (expr instanceof Structures.T_var) {
        return true;
    };
    if (expr instanceof Structures.T_var_system) {
        return true;
    };
    if (expr instanceof Structures.T_if) {
        return validIfSelectorsSimple(expr.value0) && (canMakeSTLCSimple(expr.value1) && canMakeSTLCSimple(expr.value2));
    };
    if (expr instanceof Structures.T_natRec) {
        return canMakeSTLCSimple(expr.value0) && (canMakeSTLCSimple(expr.value1) && canMakeSTLCSimple(expr.value2));
    };
    if (expr instanceof Structures.T_fst) {
        return canMakeSTLCSimple(expr.value0);
    };
    if (expr instanceof Structures.T_snd) {
        return canMakeSTLCSimple(expr.value0);
    };
    if (expr instanceof Structures.T_app) {
        return canMakeSTLCSimple(expr.value0) && canMakeSTLCSimple(expr.value1);
    };
    if (expr instanceof Structures.T_pair) {
        return canMakeSTLCSimple(expr.value0) && canMakeSTLCSimple(expr.value1);
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
        return false;
    };
    if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Negate) {
        return false;
    };
    if (expr instanceof Structures.T_unop) {
        return canMakeSTLCSimple(expr.value1);
    };
    if (expr instanceof Structures.T_binop) {
        return canMakeSTLCSimple(expr.value1) && canMakeSTLCSimple(expr.value2);
    };
    if (expr instanceof Structures.T_let) {
        return canMakeSTLCSimple(expr.value2) && canMakeSTLCSimple(expr.value3);
    };
    if (expr instanceof Structures.T_func) {
        return canMakeSTLCSimple(expr.value2);
    };
    if (expr instanceof Structures.T_func_system) {
        return canMakeSTLCSimple(expr.value2);
    };
    throw new Error("Failed pattern match at CompileSTLC (line 392, column 26 - line 416, column 57): " + [ expr.constructor.name ]);
};
var canMakeSTLC = function (expr) {
    if (expr instanceof Structures.T_true) {
        return true;
    };
    if (expr instanceof Structures.T_false) {
        return true;
    };
    if (expr instanceof Structures.T_num) {
        return true;
    };
    if (expr instanceof Structures.T_var) {
        return true;
    };
    if (expr instanceof Structures.T_var_system) {
        return true;
    };
    if (expr instanceof Structures.T_if) {
        return validIfSelector(expr.value0) && (canMakeSTLC(expr.value1) && canMakeSTLC(expr.value2));
    };
    if (expr instanceof Structures.T_natRec) {
        return canMakeSTLC(expr.value0) && (canMakeSTLC(expr.value1) && canMakeSTLC(expr.value2));
    };
    if (expr instanceof Structures.T_fst) {
        return canMakeSTLC(expr.value0);
    };
    if (expr instanceof Structures.T_snd) {
        return canMakeSTLC(expr.value0);
    };
    if (expr instanceof Structures.T_app) {
        return canMakeSTLC(expr.value0) && canMakeSTLC(expr.value1);
    };
    if (expr instanceof Structures.T_pair) {
        return canMakeSTLC(expr.value0) && canMakeSTLC(expr.value1);
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
        return false;
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
        return false;
    };
    if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Negate) {
        return false;
    };
    if (expr instanceof Structures.T_unop) {
        return canMakeSTLC(expr.value1);
    };
    if (expr instanceof Structures.T_binop) {
        return canMakeSTLC(expr.value1) && canMakeSTLC(expr.value2);
    };
    if (expr instanceof Structures.T_let) {
        return canMakeSTLC(expr.value2) && canMakeSTLC(expr.value3);
    };
    if (expr instanceof Structures.T_func) {
        return canMakeSTLC(expr.value2);
    };
    if (expr instanceof Structures.T_func_system) {
        return canMakeSTLC(expr.value2);
    };
    throw new Error("Failed pattern match at CompileSTLC (line 448, column 20 - line 472, column 51): " + [ expr.constructor.name ]);
};
var makeSTLC = function (expr) {
    var $354 = canMakeSTLC(expr);
    if ($354) {
        var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
        if (v instanceof Data_Maybe.Just) {
            return termToSTLC(expr)(Data_Maybe.Nothing.value)(TypeSystem.emptyEnv);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return "Erro de Tipo";
        };
        throw new Error("Failed pattern match at CompileSTLC (line 477, column 21 - line 479, column 50): " + [ v.constructor.name ]);
    };
    return "O termo seletor do if n\xe3o pode conter vari\xe1veis.\x0aN\xe3o \xe9 poss\xedvel representar subtra\xe7\xe3o ou compara\xe7\xf5es entre naturais em STLC.";
};
var makeSTLCDefs = function (expr) {
    var $357 = canMakeSTLC(expr);
    if ($357) {
        var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
        if (v instanceof Data_Maybe.Just) {
            return makeDefsBlock(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToSTLCDefs(expr)(TypeSystem.emptyEnv);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return "Erro de Tipo";
        };
        throw new Error("Failed pattern match at CompileSTLC (line 485, column 25 - line 487, column 54): " + [ v.constructor.name ]);
    };
    return "N\xe3o \xe9 poss\xedvel representar subtra\xe7\xe3o ou compara\xe7\xf5es entre naturais em STLC. ";
};
var makeSTLCSimple = function (expr) {
    var $360 = canMakeSTLC(expr);
    if ($360) {
        var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
        if (v instanceof Data_Maybe.Just) {
            return termToSTLCSimple(expr)(TypeSystem.emptyEnv);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return "Erro de Tipo";
        };
        throw new Error("Failed pattern match at CompileSTLC (line 493, column 25 - line 495, column 54): " + [ v.constructor.name ]);
    };
    return "Para gerar STLC simples os seletores de if s\xf3 podem conter express\xf5es l\xf3gicas.\x0aN\xe3o \xe9 poss\xedvel representar subtra\xe7\xe3o ou compara\xe7\xf5es entre naturais em STLC. ";
};
export {
    typesSTLC,
    typesSTLCDefs,
    makePairTypeSTLC,
    makeBooleanTypeSTLC,
    makeIfSTLC,
    makePairSTLC,
    termToSTLC,
    lastType,
    termToSTLCSimple,
    termToSTLCDefs,
    makeDefSTLC,
    makeDefsUsed,
    makeDefsBlock,
    validIfSelectorsSimple,
    canMakeSTLCSimple,
    validIfSelector,
    canMakeSTLC,
    makeSTLC,
    makeSTLCDefs,
    makeSTLCSimple
};
