import * as Data_Eq from "../Data.Eq/index.js";
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
    throw new Error("Failed pattern match at CompileSTLC (line 202, column 24 - line 223, column 48): " + [ expr.constructor.name ]);
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
        var $68 = notEq(t.value0.value0)(t.value0.value1);
        if ($68) {
            return "Pares devem ter o mesmo tipo";
        };
        var t1String = typesSTLC(new Data_Maybe.Just(t.value0.value0));
        return "((" + (t1String + (" -> " + (t1String + (" -> " + (t1String + (") -> " + (t1String + ")")))))));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "A";
    };
    throw new Error("Failed pattern match at CompileSTLC (line 14, column 15 - line 24, column 37): " + [ t.constructor.name ]);
};
var makePairTypeSTLC = function (t) {
    return "((" + (t + ("->" + (t + ("->" + (t + (") -> " + (t + ")")))))));
};
var makePairSTLC = function (t) {
    return "(\\e1: " + (t + (". \\e2: " + (t + (". \\b: (" + (t + (" -> " + (t + (" -> " + (t + "). b e1 e2)")))))))));
};
var makeNatural = function (v) {
    if (v === 0) {
        return "x";
    };
    return "(f " + (makeNatural(v - 1 | 0) + ")");
};
var makeIfSTLC = function (t) {
    return "(\\b: (" + (t + (" -> " + (t + (" -> " + (t + ("). \\e1: " + (t + (". \\e2: " + (t + ". b e1 e2)")))))))));
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
                return "_" + expr.value0;
            };
            if (expr instanceof Structures.T_func) {
                return "(\\_" + (expr.value0 + (": " + (typesSTLC(new Data_Maybe.Just(expr.value1)) + (". " + (termToSTLC(expr.value2)(t)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
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
                throw new Error("Failed pattern match at CompileSTLC (line 80, column 31 - line 100, column 94): " + [ t.constructor.name ]);
            };
            if (expr instanceof Structures.T_pair) {
                var t1 = TypeSystem.typeInfer(env)(expr.value0);
                var t2 = TypeSystem.typeInfer(env)(expr.value1);
                var $101 = eq(t1)(t2);
                if ($101) {
                    if (t instanceof Data_Maybe.Nothing) {
                        return "(" + (makePairSTLC(typesSTLC(t1)) + (" " + (termToSTLC(expr.value0)(t)(env) + (" " + (termToSTLC(expr.value1)(t)(env) + ")")))));
                    };
                    if (t instanceof Data_Maybe.Just) {
                        return "(" + (makePairSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (" " + (termToSTLC(expr.value0)(new Data_Maybe.Just(t.value0))(env) + (" " + (termToSTLC(expr.value1)(new Data_Maybe.Just(t.value0))(env) + ")")))));
                    };
                    throw new Error("Failed pattern match at CompileSTLC (line 106, column 32 - line 114, column 85): " + [ t.constructor.name ]);
                };
                return "ERRO valores no par devem ser do mesmo tipo";
            };
            if (expr instanceof Structures.T_fst) {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    var $107 = eq1(v.value0.value0)(v.value0.value1);
                    if ($107) {
                        if (t instanceof Data_Maybe.Just) {
                            return "((\\p: " + (makePairTypeSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (". p " + (termToSTLC(Structures.T_true.value)(new Data_Maybe.Just(new Structures.Func(t.value0, new Structures.Func(t.value0, t.value0))))(env) + (")" + (termToSTLC(expr.value0)(new Data_Maybe.Just(t.value0))(env) + ")")))));
                        };
                        if (t instanceof Data_Maybe.Nothing) {
                            return "((\\p: " + (typesSTLC(TypeSystem.typeInfer(env)(expr.value0)) + (". p " + (termToSTLC(Structures.T_true.value)(new Data_Maybe.Just(v.value0.value0))(env) + (")" + (termToSTLC(expr.value0)(t)(env) + ")")))));
                        };
                        throw new Error("Failed pattern match at CompileSTLC (line 122, column 35 - line 133, column 68): " + [ t.constructor.name ]);
                    };
                    return "ERRO valores no par devem ser do mesmo tipo";
                };
                return "ERRO de tipo, fst deve receber par";
            };
            if (expr instanceof Structures.T_snd) {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    var $115 = eq1(v.value0.value0)(v.value0.value1);
                    if ($115) {
                        if (t instanceof Data_Maybe.Just) {
                            return "((\\p: " + (makePairTypeSTLC(makeBooleanTypeSTLC(typesSTLC(new Data_Maybe.Just(t.value0)))) + (". p " + (termToSTLC(Structures.T_false.value)(new Data_Maybe.Just(new Structures.Func(t.value0, new Structures.Func(t.value0, t.value0))))(env) + (")" + (termToSTLC(expr.value0)(t)(env) + ")")))));
                        };
                        if (t instanceof Data_Maybe.Nothing) {
                            return "((\\p: " + (typesSTLC(TypeSystem.typeInfer(env)(expr.value0)) + (". p " + (termToSTLC(Structures.T_false.value)(new Data_Maybe.Just(v.value0.value0))(env) + (")" + (termToSTLC(expr.value0)(t)(env) + ")")))));
                        };
                        throw new Error("Failed pattern match at CompileSTLC (line 141, column 35 - line 152, column 68): " + [ t.constructor.name ]);
                    };
                    return "ERRO valores no par devem ser do mesmo tipo";
                };
                return "ERRO de tipo, fst deve receber par";
            };
            if (expr instanceof Structures.T_num) {
                return "(\\f:A->A.\\x:A." + (makeNatural(expr.value0) + ")");
            };
            if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
                return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A. \\x:A. n1 f (n2 f x))" + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")));
            };
            if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
                return "((\\n1:(A->A)->A->A.\\n2:(A->A)->A->A.\\f:A->A.\\x:A. n1 (n2 f) x)" + (termToSTLC(expr.value1)(t)(env) + (" " + (termToSTLC(expr.value2)(t)(env) + ")")));
            };
            return "incompleto";
        };
    };
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
    if (expr instanceof Structures.T_if) {
        return validIfSelectorsSimple(expr.value0) && (canMakeSTLCSimple(expr.value1) && canMakeSTLCSimple(expr.value2));
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
    throw new Error("Failed pattern match at CompileSTLC (line 177, column 26 - line 198, column 50): " + [ expr.constructor.name ]);
};
var makeSTLCSimple = function (expr) {
    var $170 = canMakeSTLCSimple(expr);
    if ($170) {
        var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
        if (v instanceof Data_Maybe.Just) {
            return termToSTLC(expr)(Data_Maybe.Nothing.value)(TypeSystem.emptyEnv);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return "Erro de Tipo";
        };
        throw new Error("Failed pattern match at CompileSTLC (line 260, column 25 - line 262, column 54): " + [ v.constructor.name ]);
    };
    return "Para gerar STLC simples os seletores de if s\xf3 podem conter express\xf5es l\xf3gicas.\x0aN\xe3o \xe9 poss\xedvel representar subtra\xe7\xe3o ou compara\xe7\xf5es entre naturais em STLC. ";
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
    if (expr instanceof Structures.T_if) {
        return validIfSelector(expr.value0) && (canMakeSTLC(expr.value1) && canMakeSTLC(expr.value2));
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
    throw new Error("Failed pattern match at CompileSTLC (line 226, column 20 - line 247, column 44): " + [ expr.constructor.name ]);
};
var makeSTLC = function (expr) {
    var $214 = canMakeSTLC(expr);
    if ($214) {
        var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
        if (v instanceof Data_Maybe.Just) {
            return termToSTLC(expr)(Data_Maybe.Nothing.value)(TypeSystem.emptyEnv);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return "Erro de Tipo";
        };
        throw new Error("Failed pattern match at CompileSTLC (line 252, column 21 - line 254, column 50): " + [ v.constructor.name ]);
    };
    return "O termo seletor do if n\xe3o pode conter vari\xe1veis.\x0aN\xe3o \xe9 poss\xedvel representar subtra\xe7\xe3o ou compara\xe7\xf5es entre naturais em STLC.";
};
export {
    makeNatural,
    typesSTLC,
    makePairTypeSTLC,
    makeBooleanTypeSTLC,
    makeIfSTLC,
    makePairSTLC,
    termToSTLC,
    validIfSelectorsSimple,
    canMakeSTLCSimple,
    validIfSelector,
    canMakeSTLC,
    makeSTLC,
    makeSTLCSimple
};
