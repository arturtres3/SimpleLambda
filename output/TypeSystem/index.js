import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Term from "../Term/index.js";
var Nat = /* #__PURE__ */ (function () {
    function Nat() {

    };
    Nat.value = new Nat();
    return Nat;
})();
var Bool = /* #__PURE__ */ (function () {
    function Bool() {

    };
    Bool.value = new Bool();
    return Bool;
})();
var Pair = /* #__PURE__ */ (function () {
    function Pair(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Pair.create = function (value0) {
        return function (value1) {
            return new Pair(value0, value1);
        };
    };
    return Pair;
})();
var showType = function (t) {
    if (t instanceof Nat) {
        return "Nat";
    };
    if (t instanceof Bool) {
        return "Bool";
    };
    if (t instanceof Pair) {
        return "(Pair " + (showType(t.value0) + (" " + (showType(t.value1) + ")")));
    };
    throw new Error("Failed pattern match at TypeSystem (line 19, column 14 - line 22, column 79): " + [ t.constructor.name ]);
};
var typeInfer = function (expr) {
    if (expr instanceof Term.T_false) {
        return new Data_Maybe.Just(Bool.value);
    };
    if (expr instanceof Term.T_true) {
        return new Data_Maybe.Just(Bool.value);
    };
    if (expr instanceof Term.T_zero) {
        return new Data_Maybe.Just(Nat.value);
    };
    if (expr instanceof Term.T_num) {
        return new Data_Maybe.Just(Nat.value);
    };
    if (expr instanceof Term.T_succ) {
        var v = typeInfer(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Nat) {
            return new Data_Maybe.Just(Nat.value);
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Term.T_fst && expr.value0 instanceof Term.T_pair) {
        return typeInfer(expr.value0.value0);
    };
    if (expr instanceof Term.T_fst) {
        var v = typeInfer(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Pair) {
            return new Data_Maybe.Just(v.value0.value0);
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Term.T_snd && expr.value0 instanceof Term.T_pair) {
        return typeInfer(expr.value0.value1);
    };
    if (expr instanceof Term.T_snd) {
        var v = typeInfer(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Pair) {
            return new Data_Maybe.Just(v.value0.value1);
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Term.T_pair) {
        var v = typeInfer(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            var v1 = typeInfer(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(new Pair(v.value0, v1.value0));
            };
            return Data_Maybe.Nothing.value;
        };
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Term.T_if) {
        var v = typeInfer(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Bool) {
            var v1 = typeInfer(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                var v2 = typeInfer(expr.value2);
                if (v2 instanceof Data_Maybe.Just) {
                    var $44 = showType(v1.value0) === showType(v2.value0);
                    if ($44) {
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
    throw new Error("Failed pattern match at TypeSystem (line 25, column 18 - line 57, column 34): " + [ expr.constructor.name ]);
};
var typeCheck = function (expr) {
    var v = typeInfer(expr);
    if (v instanceof Data_Maybe.Just) {
        return showType(v.value0);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Invalid Type";
    };
    throw new Error("Failed pattern match at TypeSystem (line 61, column 18 - line 63, column 30): " + [ v.constructor.name ]);
};
export {
    Nat,
    Bool,
    Pair,
    showType,
    typeCheck,
    typeInfer
};
