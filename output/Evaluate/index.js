import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
var step = function (expr) {
    if (expr instanceof Structures.T_false) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Structures.T_true) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Structures.T_num) {
        return Data_Maybe.Nothing.value;
    };
    if (expr instanceof Structures.T_pair) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new Structures.T_pair(v.value0, expr.value1));
        };
        if (v instanceof Data_Maybe.Nothing) {
            var v1 = step(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                return new Data_Maybe.Just(new Structures.T_pair(expr.value0, v1.value0));
            };
            if (v1 instanceof Data_Maybe.Nothing) {
                return Data_Maybe.Nothing.value;
            };
            throw new Error("Failed pattern match at Evaluate (line 18, column 26 - line 20, column 44): " + [ v1.constructor.name ]);
        };
        throw new Error("Failed pattern match at Evaluate (line 16, column 10 - line 20, column 45): " + [ v.constructor.name ]);
    };
    if (expr instanceof Structures.T_fst && expr.value0 instanceof Structures.T_pair) {
        return new Data_Maybe.Just(expr.value0.value0);
    };
    if (expr instanceof Structures.T_fst) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new Structures.T_fst(v.value0));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Evaluate (line 24, column 10 - line 26, column 32): " + [ v.constructor.name ]);
    };
    if (expr instanceof Structures.T_snd && expr.value0 instanceof Structures.T_pair) {
        return new Data_Maybe.Just(expr.value0.value1);
    };
    if (expr instanceof Structures.T_snd) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new Structures.T_snd(v.value0));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Evaluate (line 30, column 10 - line 32, column 32): " + [ v.constructor.name ]);
    };
    if (expr instanceof Structures.T_if && expr.value0 instanceof Structures.T_true) {
        return new Data_Maybe.Just(expr.value1);
    };
    if (expr instanceof Structures.T_if && expr.value0 instanceof Structures.T_false) {
        return new Data_Maybe.Just(expr.value2);
    };
    if (expr instanceof Structures.T_if) {
        var v = step(expr.value0);
        if (v instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just(new Structures.T_if(v.value0, expr.value1, expr.value2));
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Evaluate (line 37, column 10 - line 39, column 32): " + [ v.constructor.name ]);
    };
    return Data_Maybe.Nothing.value;
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
        throw new Error("Failed pattern match at Evaluate (line 45, column 13 - line 47, column 20): " + [ v.constructor.name ]);
    };
    while (!$tco_done) {
        $tco_result = $tco_loop($copy_expr);
    };
    return $tco_result;
};
export {
    step,
    $$eval as eval
};
