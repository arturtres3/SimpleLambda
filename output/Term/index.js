import * as Data_Int from "../Data.Int/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
var T_true = /* #__PURE__ */ (function () {
    function T_true() {

    };
    T_true.value = new T_true();
    return T_true;
})();
var T_false = /* #__PURE__ */ (function () {
    function T_false() {

    };
    T_false.value = new T_false();
    return T_false;
})();
var T_zero = /* #__PURE__ */ (function () {
    function T_zero() {

    };
    T_zero.value = new T_zero();
    return T_zero;
})();
var T_succ = /* #__PURE__ */ (function () {
    function T_succ(value0) {
        this.value0 = value0;
    };
    T_succ.create = function (value0) {
        return new T_succ(value0);
    };
    return T_succ;
})();
var T_num = /* #__PURE__ */ (function () {
    function T_num(value0) {
        this.value0 = value0;
    };
    T_num.create = function (value0) {
        return new T_num(value0);
    };
    return T_num;
})();
var T_if = /* #__PURE__ */ (function () {
    function T_if(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    T_if.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new T_if(value0, value1, value2);
            };
        };
    };
    return T_if;
})();
var T_pair = /* #__PURE__ */ (function () {
    function T_pair(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    T_pair.create = function (value0) {
        return function (value1) {
            return new T_pair(value0, value1);
        };
    };
    return T_pair;
})();
var T_fst = /* #__PURE__ */ (function () {
    function T_fst(value0) {
        this.value0 = value0;
    };
    T_fst.create = function (value0) {
        return new T_fst(value0);
    };
    return T_fst;
})();
var T_snd = /* #__PURE__ */ (function () {
    function T_snd(value0) {
        this.value0 = value0;
    };
    T_snd.create = function (value0) {
        return new T_snd(value0);
    };
    return T_snd;
})();
var step = function (expr) {
    if (expr instanceof T_false) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof T_true) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof T_zero) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof T_num) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof T_succ) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new T_succ(v.value0));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Term (line 48, column 10 - line 50, column 32): " + [ v.constructor.name ]);
    };
    if (expr instanceof T_pair) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new T_pair(v.value0, expr.value1));
        };
        if (v instanceof Data_Maybe.Nothing) {
            var v1 = step(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(new T_pair(expr.value0, v1.value0));
            };
            if (v1 instanceof Data_Maybe.Nothing) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Term (line 55, column 26 - line 57, column 44): " + [ v1.constructor.name ]);
        };
        throw new Error("Failed pattern match at Term (line 53, column 10 - line 57, column 45): " + [ v.constructor.name ]);
    };
    if (expr instanceof T_fst && expr.value0 instanceof T_pair) {
        return new Data_Maybe.Just(expr.value0.value0);
    };
    if (expr instanceof T_fst) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new T_fst(v.value0));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Term (line 61, column 10 - line 63, column 32): " + [ v.constructor.name ]);
    };
    if (expr instanceof T_snd && expr.value0 instanceof T_pair) {
        return new Data_Maybe.Just(expr.value0.value1);
    };
    if (expr instanceof T_snd) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new T_snd(v.value0));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Term (line 67, column 10 - line 69, column 32): " + [ v.constructor.name ]);
    };
    if (expr instanceof T_if && expr.value0 instanceof T_true) {
        return new Data_Maybe.Just(expr.value1);
    };
    if (expr instanceof T_if && expr.value0 instanceof T_false) {
        return new Data_Maybe.Just(expr.value2);
    };
    if (expr instanceof T_if) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new T_if(v.value0, expr.value1, expr.value2));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Term (line 74, column 10 - line 76, column 32): " + [ v.constructor.name ]);
    };
    throw new Error("Failed pattern match at Term (line 42, column 13 - line 76, column 33): " + [ expr.constructor.name ]);
};
var showTerm = function (t) {
    if (t instanceof T_true) {
        return "_true";
    };
    if (t instanceof T_false) {
        return "_false";
    };
    if (t instanceof T_zero) {
        return "_zero";
    };
    if (t instanceof T_num) {
        return "(_num " + (Data_Int.toStringAs(Data_Int.decimal)(t.value0) + ")");
    };
    if (t instanceof T_succ) {
        return "_succ " + showTerm(t.value0);
    };
    if (t instanceof T_fst) {
        return "_fst" + showTerm(t.value0);
    };
    if (t instanceof T_snd) {
        return "_snd" + showTerm(t.value0);
    };
    if (t instanceof T_pair) {
        return "(_pair " + (showTerm(t.value0) + (", " + (showTerm(t.value1) + ")")));
    };
    if (t instanceof T_if) {
        return "(_if " + (showTerm(t.value0) + (" " + (showTerm(t.value1) + (" " + (showTerm(t.value2) + ")")))));
    };
    throw new Error("Failed pattern match at Term (line 26, column 14 - line 38, column 47): " + [ t.constructor.name ]);
};
var $$eval = function ($copy_expr) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(expr) {
        var v = step(expr);
        if (v instanceof Data_Maybe.Just) {
            $copy_expr = v.value0;
            return;
        };
        if (v instanceof Data_Maybe.Nothing) {
            $tco_done = true;
            return expr;
        };
        throw new Error("Failed pattern match at Term (line 79, column 13 - line 81, column 20): " + [ v.constructor.name ]);
    };
    while (!$tco_done) {
        $tco_result = $tco_loop($copy_expr);
    };
    return $tco_result;
};
export {
    T_true,
    T_false,
    T_zero,
    T_succ,
    T_num,
    T_if,
    T_pair,
    T_fst,
    T_snd,
    $$eval as eval,
    showTerm,
    step
};
