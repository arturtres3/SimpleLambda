import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Structures from "../Structures/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(Structures.eqTermType);
var eq2 = /* #__PURE__ */ Data_Eq.eq(/* #__PURE__ */ Data_Maybe.eqMaybe(Structures.eqTermType));
var show = /* #__PURE__ */ Data_Show.show(Structures["showType$prime"]);
var update = function (env) {
    return function (id) {
        return function (t) {
            return new Data_List_Types.Cons(new Data_Tuple.Tuple(id, t), env);
        };
    };
};

// Termporario para nao quebrar geracao de lambdaOmega simples
var typeInferSimple = function (expr) {
    if (expr instanceof Structures.T_false) {
        return new Data_Maybe.Just(Structures.Bool.value);
    };
    if (expr instanceof Structures.T_true) {
        return new Data_Maybe.Just(Structures.Bool.value);
    };
    if (expr instanceof Structures.T_num) {
        return new Data_Maybe.Just(Structures.Nat.value);
    };
    if (expr instanceof Structures.T_fst && expr.value0 instanceof Structures.T_pair) {
        return typeInferSimple(expr.value0.value0);
    };
    if (expr instanceof Structures.T_fst) {
        var v = typeInferSimple(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
            return new Data_Maybe.Just(v.value0.value0);
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Structures.T_snd && expr.value0 instanceof Structures.T_pair) {
        return typeInferSimple(expr.value0.value1);
    };
    if (expr instanceof Structures.T_snd) {
        var v = typeInferSimple(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
            return new Data_Maybe.Just(v.value0.value1);
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Structures.T_pair) {
        var v = typeInferSimple(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            var v1 = typeInferSimple(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(new Structures.Pair(v.value0, v1.value0));
            };
            return Data_Maybe.Nothing.value;
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Structures.T_if) {
        var v = typeInferSimple(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Bool) {
            var v1 = typeInferSimple(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                var v2 = typeInferSimple(expr.value2);
                if (v2 instanceof Data_Maybe.Just) {
                    var $53 = eq(v1.value0)(v2.value0);
                    if ($53) {
                        return new Data_Maybe.Just(v1.value0);
                    };
                    return Data_Maybe.Nothing.value;
                };
                return Data_Maybe.Nothing.value;
            };
            return Data_Maybe.Nothing.value;
        };
        return Data_Maybe.Nothing.value;
    };
    return Data_Maybe.Nothing.value;
};
var opcodeToType = function (opcode) {
    return function (t) {
        if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
            if (opcode instanceof Structures.Sub) {
                return new Data_Maybe.Just(Structures.Nat.value);
            };
            if (opcode instanceof Structures.Add) {
                return new Data_Maybe.Just(Structures.Nat.value);
            };
            if (opcode instanceof Structures.Mult) {
                return new Data_Maybe.Just(Structures.Nat.value);
            };
            if (opcode instanceof Structures.Div) {
                return new Data_Maybe.Just(Structures.Nat.value);
            };
            if (opcode instanceof Structures.Gt) {
                return new Data_Maybe.Just(Structures.Bool.value);
            };
            if (opcode instanceof Structures.Lt) {
                return new Data_Maybe.Just(Structures.Bool.value);
            };
            if (opcode instanceof Structures.Eq) {
                return new Data_Maybe.Just(Structures.Bool.value);
            };
            if (opcode instanceof Structures.Ne) {
                return new Data_Maybe.Just(Structures.Bool.value);
            };
            return Data_Maybe.Nothing.value;
        };
        if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
            if (opcode instanceof Structures.And) {
                return new Data_Maybe.Just(Structures.Bool.value);
            };
            if (opcode instanceof Structures.Or) {
                return new Data_Maybe.Just(Structures.Bool.value);
            };
            return Data_Maybe.Nothing.value;
        };
        return Data_Maybe.Nothing.value;
    };
};
var lookup = function ($copy_v) {
    return function ($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
            if (v instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return Data_Maybe.Nothing.value;
            };
            if (v instanceof Data_List_Types.Cons) {
                var $67 = v.value0.value0 === v1;
                if ($67) {
                    $tco_done = true;
                    return new Data_Maybe.Just(v.value0.value1);
                };
                $tco_var_v = v.value1;
                $copy_v1 = v1;
                return;
            };
            throw new Error("Failed pattern match at TypeSystem (line 15, column 1 - line 15, column 41): " + [ v.constructor.name, v1.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $copy_v1);
        };
        return $tco_result;
    };
};
var typeInfer = function (env) {
    return function (expr) {
        if (expr instanceof Structures.T_false) {
            return new Data_Maybe.Just(Structures.Bool.value);
        };
        if (expr instanceof Structures.T_true) {
            return new Data_Maybe.Just(Structures.Bool.value);
        };
        if (expr instanceof Structures.T_num) {
            return new Data_Maybe.Just(Structures.Nat.value);
        };
        if (expr instanceof Structures.T_var) {
            return lookup(env)(expr.value0);
        };
        if (expr instanceof Structures.T_var_system) {
            return lookup(env)(expr.value0);
        };
        if (expr instanceof Structures.T_fst && expr.value0 instanceof Structures.T_pair) {
            return typeInfer(env)(expr.value0.value0);
        };
        if (expr instanceof Structures.T_fst) {
            var v = typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                return new Data_Maybe.Just(v.value0.value0);
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_snd && expr.value0 instanceof Structures.T_pair) {
            return typeInfer(env)(expr.value0.value1);
        };
        if (expr instanceof Structures.T_snd) {
            var v = typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                return new Data_Maybe.Just(v.value0.value1);
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_pair) {
            var v = typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just) {
                var v1 = typeInfer(env)(expr.value1);
                if (v1 instanceof Data_Maybe.Just) {
                    return new Data_Maybe.Just(new Structures.Pair(v.value0, v1.value0));
                };
                return Data_Maybe.Nothing.value;
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_if) {
            var v = typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Bool) {
                var v1 = typeInfer(env)(expr.value1);
                if (v1 instanceof Data_Maybe.Just) {
                    var v2 = typeInfer(env)(expr.value2);
                    if (v2 instanceof Data_Maybe.Just) {
                        var $101 = eq(v1.value0)(v2.value0);
                        if ($101) {
                            return new Data_Maybe.Just(v1.value0);
                        };
                        return Data_Maybe.Nothing.value;
                    };
                    return Data_Maybe.Nothing.value;
                };
                return Data_Maybe.Nothing.value;
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_func) {
            var v = typeInfer(update(env)(expr.value0)(expr.value1))(expr.value2);
            if (v instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(new Structures.Func(expr.value1, v.value0));
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_func_system) {
            var v = typeInfer(update(env)(expr.value0)(expr.value1))(expr.value2);
            if (v instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(new Structures.Func(expr.value1, v.value0));
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_app) {
            var v = typeInfer(env)(expr.value0);
            if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Func) {
                var $119 = eq2(typeInfer(env)(expr.value1))(new Data_Maybe.Just(v.value0.value0));
                if ($119) {
                    return new Data_Maybe.Just(v.value0.value1);
                };
                return Data_Maybe.Nothing.value;
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_let) {
            var $125 = eq2(typeInfer(env)(expr.value2))(new Data_Maybe.Just(expr.value1));
            if ($125) {
                return typeInfer(update(env)(expr.value0)(expr.value1))(expr.value3);
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_binop) {
            var t1 = typeInfer(env)(expr.value1);
            var t2 = typeInfer(env)(expr.value2);
            var $130 = eq2(t1)(t2);
            if ($130) {
                return opcodeToType(expr.value0)(t1);
            };
            return Data_Maybe.Nothing.value;
        };
        if (expr instanceof Structures.T_unop) {
            var t1 = typeInfer(env)(expr.value1);
            if (expr.value0 instanceof Structures.Not) {
                var $135 = eq2(t1)(new Data_Maybe.Just(Structures.Bool.value));
                if ($135) {
                    return new Data_Maybe.Just(Structures.Bool.value);
                };
                return Data_Maybe.Nothing.value;
            };
            if (expr.value0 instanceof Structures.Negate) {
                var $136 = eq2(t1)(new Data_Maybe.Just(Structures.Nat.value));
                if ($136) {
                    return new Data_Maybe.Just(Structures.Nat.value);
                };
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at TypeSystem (line 102, column 29 - line 104, column 83): " + [ expr.value0.constructor.name ]);
        };
        if (expr instanceof Structures.T_natRec) {
            var $139 = eq2(typeInfer(env)(expr.value0))(new Data_Maybe.Just(Structures.Nat.value));
            if ($139) {
                var v = typeInfer(env)(expr.value2);
                if (v instanceof Data_Maybe.Just) {
                    var $141 = eq2(typeInfer(env)(expr.value1))(new Data_Maybe.Just(new Structures.Func(v.value0, v.value0)));
                    if ($141) {
                        return new Data_Maybe.Just(v.value0);
                    };
                    return Data_Maybe.Nothing.value;
                };
                return Data_Maybe.Nothing.value;
            };
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at TypeSystem (line 44, column 22 - line 112, column 41): " + [ expr.constructor.name ]);
    };
};
var emptyEnv = /* #__PURE__ */ (function () {
    return Data_List_Types.Nil.value;
})();

// _ -> Nothing
var typeCheck = function (expr) {
    var v = typeInfer(emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return show(v.value0);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Invalid Type";
    };
    throw new Error("Failed pattern match at TypeSystem (line 118, column 18 - line 120, column 30): " + [ v.constructor.name ]);
};
var a = /* #__PURE__ */ (function () {
    return new Data_List_Types.Cons(new Data_Tuple.Tuple("tst", Structures.Nat.value), new Data_List_Types.Cons(new Data_Tuple.Tuple("tst1", Structures.Bool.value), new Data_List_Types.Cons(new Data_Tuple.Tuple("mult", new Structures.Func(Structures.Nat.value, Structures.Nat.value)), Data_List_Types.Nil.value)));
})();
export {
    emptyEnv,
    lookup,
    update,
    a,
    opcodeToType,
    typeInfer,
    typeCheck,
    typeInferSimple
};
