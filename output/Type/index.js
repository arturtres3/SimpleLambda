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
var showType = function (t) {
    if (t instanceof Nat) {
        return "Nat";
    };
    if (t instanceof Bool) {
        return "Bool";
    };
    throw new Error("Failed pattern match at Type (line 17, column 14 - line 19, column 27): " + [ t.constructor.name ]);
};
var typeInfer = function (expr) {
    if (expr instanceof Term.T_false) {
        return new Data_Maybe.Just(Bool.value);
    };
    if (expr instanceof Term.T_true) {
        return new Data_Maybe.Just(Bool.value);
    };
    if (expr instanceof Term.T_if) {
        var v = typeInfer(expr.value0);
        if (v instanceof Data_Maybe.Just && v.value0 instanceof Bool) {
            var v1 = typeInfer(expr.value1);
            if (v1 instanceof Data_Maybe.Just) {
                var v2 = typeInfer(expr.value2);
                if (v2 instanceof Data_Maybe.Just) {
                    var $9 = showType(v1.value0) === showType(v2.value0);
                    if ($9) {
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
    throw new Error("Failed pattern match at Type (line 22, column 18 - line 31, column 34): " + [ expr.constructor.name ]);
};
export {
    Nat,
    Bool,
    typeInfer
};
