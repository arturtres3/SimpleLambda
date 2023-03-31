import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Show from "../Data.Show/index.js";
var union = /* #__PURE__ */ Data_List.union(Data_Eq.eqString);
var Not = /* #__PURE__ */ (function () {
    function Not() {

    };
    Not.value = new Not();
    return Not;
})();
var Negate = /* #__PURE__ */ (function () {
    function Negate() {

    };
    Negate.value = new Negate();
    return Negate;
})();
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
var Func = /* #__PURE__ */ (function () {
    function Func(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Func.create = function (value0) {
        return function (value1) {
            return new Func(value0, value1);
        };
    };
    return Func;
})();
var Add = /* #__PURE__ */ (function () {
    function Add() {

    };
    Add.value = new Add();
    return Add;
})();
var Sub = /* #__PURE__ */ (function () {
    function Sub() {

    };
    Sub.value = new Sub();
    return Sub;
})();
var Mult = /* #__PURE__ */ (function () {
    function Mult() {

    };
    Mult.value = new Mult();
    return Mult;
})();
var Div = /* #__PURE__ */ (function () {
    function Div() {

    };
    Div.value = new Div();
    return Div;
})();
var Lt = /* #__PURE__ */ (function () {
    function Lt() {

    };
    Lt.value = new Lt();
    return Lt;
})();
var Gt = /* #__PURE__ */ (function () {
    function Gt() {

    };
    Gt.value = new Gt();
    return Gt;
})();
var Eq = /* #__PURE__ */ (function () {
    function Eq() {

    };
    Eq.value = new Eq();
    return Eq;
})();
var Ne = /* #__PURE__ */ (function () {
    function Ne() {

    };
    Ne.value = new Ne();
    return Ne;
})();
var And = /* #__PURE__ */ (function () {
    function And() {

    };
    And.value = new And();
    return And;
})();
var Or = /* #__PURE__ */ (function () {
    function Or() {

    };
    Or.value = new Or();
    return Or;
})();
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
var T_binop = /* #__PURE__ */ (function () {
    function T_binop(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    T_binop.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new T_binop(value0, value1, value2);
            };
        };
    };
    return T_binop;
})();
var T_unop = /* #__PURE__ */ (function () {
    function T_unop(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    T_unop.create = function (value0) {
        return function (value1) {
            return new T_unop(value0, value1);
        };
    };
    return T_unop;
})();
var T_natRec = /* #__PURE__ */ (function () {
    function T_natRec(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    T_natRec.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new T_natRec(value0, value1, value2);
            };
        };
    };
    return T_natRec;
})();
var T_var = /* #__PURE__ */ (function () {
    function T_var(value0) {
        this.value0 = value0;
    };
    T_var.create = function (value0) {
        return new T_var(value0);
    };
    return T_var;
})();
var T_func = /* #__PURE__ */ (function () {
    function T_func(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    T_func.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new T_func(value0, value1, value2);
            };
        };
    };
    return T_func;
})();
var T_app = /* #__PURE__ */ (function () {
    function T_app(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    T_app.create = function (value0) {
        return function (value1) {
            return new T_app(value0, value1);
        };
    };
    return T_app;
})();
var T_let = /* #__PURE__ */ (function () {
    function T_let(value0, value1, value2, value3) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
        this.value3 = value3;
    };
    T_let.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return function (value3) {
                    return new T_let(value0, value1, value2, value3);
                };
            };
        };
    };
    return T_let;
})();
var T_func_system = /* #__PURE__ */ (function () {
    function T_func_system(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    T_func_system.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new T_func_system(value0, value1, value2);
            };
        };
    };
    return T_func_system;
})();
var T_var_system = /* #__PURE__ */ (function () {
    function T_var_system(value0) {
        this.value0 = value0;
    };
    T_var_system.create = function (value0) {
        return new T_var_system(value0);
    };
    return T_var_system;
})();
var T_error = /* #__PURE__ */ (function () {
    function T_error() {

    };
    T_error.value = new T_error();
    return T_error;
})();
var showUnop = {
    show: function (v) {
        if (v instanceof Negate) {
            return "-";
        };
        if (v instanceof Not) {
            return "~";
        };
        throw new Error("Failed pattern match at Structures (line 104, column 1 - line 106, column 17): " + [ v.constructor.name ]);
    }
};
var show = /* #__PURE__ */ Data_Show.show(showUnop);
var showType = function (t) {
    if (t instanceof Nat) {
        return "Nat";
    };
    if (t instanceof Bool) {
        return "Bool";
    };
    if (t instanceof Pair) {
        return "(" + (showType(t.value0) + (" X " + (showType(t.value1) + ")")));
    };
    if (t instanceof Func) {
        return "(" + (showType(t.value0) + (" -> " + (showType(t.value1) + ")")));
    };
    throw new Error("Failed pattern match at Structures (line 86, column 14 - line 90, column 77): " + [ t.constructor.name ]);
};
var showType$prime = {
    show: showType
};
var showBinop = {
    show: function (v) {
        if (v instanceof Add) {
            return " + ";
        };
        if (v instanceof Sub) {
            return " - ";
        };
        if (v instanceof Mult) {
            return "*";
        };
        if (v instanceof Div) {
            return "/";
        };
        if (v instanceof Lt) {
            return " < ";
        };
        if (v instanceof Gt) {
            return " > ";
        };
        if (v instanceof Eq) {
            return " == ";
        };
        if (v instanceof Ne) {
            return " != ";
        };
        if (v instanceof Or) {
            return " || ";
        };
        if (v instanceof And) {
            return " && ";
        };
        throw new Error("Failed pattern match at Structures (line 92, column 1 - line 102, column 20): " + [ v.constructor.name ]);
    }
};
var show1 = /* #__PURE__ */ Data_Show.show(showBinop);
var showTerm = function (t) {
    if (t instanceof T_true) {
        return "_true";
    };
    if (t instanceof T_false) {
        return "_false";
    };
    if (t instanceof T_num) {
        return "(_num " + (Data_Int.toStringAs(Data_Int.decimal)(t.value0) + ")");
    };
    if (t instanceof T_fst) {
        return "(_fst " + (showTerm(t.value0) + ")");
    };
    if (t instanceof T_snd) {
        return "(_snd " + (showTerm(t.value0) + ")");
    };
    if (t instanceof T_pair) {
        return "(_pair " + (showTerm(t.value0) + (", " + (showTerm(t.value1) + ")")));
    };
    if (t instanceof T_if) {
        return "(_if " + (showTerm(t.value0) + (" " + (showTerm(t.value1) + (" " + (showTerm(t.value2) + ")")))));
    };
    if (t instanceof T_var) {
        return "(_var " + (t.value0 + ")");
    };
    if (t instanceof T_app) {
        return "(_app " + (showTerm(t.value0) + (" " + (showTerm(t.value1) + ")")));
    };
    if (t instanceof T_func) {
        return "(_func " + (t.value0 + (" " + (showType(t.value1) + (" " + (showTerm(t.value2) + ")")))));
    };
    if (t instanceof T_let) {
        return "(_let " + (t.value0 + (" " + (showType(t.value1) + (" " + (showTerm(t.value2) + (" in " + (showTerm(t.value3) + ")")))))));
    };
    if (t instanceof T_binop) {
        return "(_binOp " + (show1(t.value0) + (" " + (showTerm(t.value1) + (" " + (showTerm(t.value2) + ")")))));
    };
    if (t instanceof T_unop) {
        return "(_unOp " + (show(t.value0) + (" " + (showTerm(t.value1) + ")")));
    };
    if (t instanceof T_natRec) {
        return "(_natRec " + (showTerm(t.value0) + (" " + (showTerm(t.value1) + (" " + (showTerm(t.value2) + ")")))));
    };
    if (t instanceof T_func_system) {
        return "(_func " + (t.value0 + (" " + (showType(t.value1) + (" " + (showTerm(t.value2) + ")")))));
    };
    if (t instanceof T_var_system) {
        return "(_var " + (t.value0 + ")");
    };
    if (t instanceof T_error) {
        return "ERRO";
    };
    throw new Error("Failed pattern match at Structures (line 54, column 14 - line 81, column 30): " + [ t.constructor.name ]);
};
var showTerm$prime = {
    show: showTerm
};
var makeNatural = function (v) {
    if (v === 0) {
        return "x";
    };
    return "(f " + (makeNatural(v - 1 | 0) + ")");
};
var listTermsUsed = function (expr) {
    return function (l) {
        if (expr instanceof T_true) {
            return union(new Data_List_Types.Cons("true", Data_List_Types.Nil.value))(l);
        };
        if (expr instanceof T_false) {
            return union(new Data_List_Types.Cons("false", Data_List_Types.Nil.value))(l);
        };
        if (expr instanceof T_if) {
            return union(union(union(union(new Data_List_Types.Cons("if", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l)))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_natRec) {
            return union(union(union(union(new Data_List_Types.Cons("natRec", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l)))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_pair) {
            return union(union(union(new Data_List_Types.Cons("pair", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l)))(listTermsUsed(expr.value1)(l));
        };
        if (expr instanceof T_fst) {
            return union(union(new Data_List_Types.Cons("fst", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l));
        };
        if (expr instanceof T_snd) {
            return union(union(new Data_List_Types.Cons("snd", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l));
        };
        if (expr instanceof T_func) {
            return union(listTermsUsed(expr.value2)(l))(l);
        };
        if (expr instanceof T_app) {
            return union(union(listTermsUsed(expr.value0)(l))(l))(listTermsUsed(expr.value1)(l));
        };
        if (expr instanceof T_let) {
            return union(union(listTermsUsed(expr.value2)(l))(l))(listTermsUsed(expr.value3)(l));
        };
        if (expr instanceof T_unop && expr.value0 instanceof Not) {
            return union(union(new Data_List_Types.Cons("not", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Add) {
            return union(union(union(new Data_List_Types.Cons("add", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof And) {
            return union(union(union(new Data_List_Types.Cons("and", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Or) {
            return union(union(union(new Data_List_Types.Cons("or", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Div) {
            return union(union(union(new Data_List_Types.Cons("div", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Mult) {
            return union(union(union(new Data_List_Types.Cons("mult", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Eq) {
            return union(union(union(new Data_List_Types.Cons("isZero", new Data_List_Types.Cons("pair", new Data_List_Types.Cons("fst", new Data_List_Types.Cons("snd", new Data_List_Types.Cons("succ", new Data_List_Types.Cons("sub", new Data_List_Types.Cons("and", new Data_List_Types.Cons("eq", Data_List_Types.Nil.value)))))))))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Ne) {
            return union(union(union(new Data_List_Types.Cons("isZero", new Data_List_Types.Cons("pair", new Data_List_Types.Cons("fst", new Data_List_Types.Cons("snd", new Data_List_Types.Cons("succ", new Data_List_Types.Cons("sub", new Data_List_Types.Cons("and", new Data_List_Types.Cons("not", new Data_List_Types.Cons("ne", Data_List_Types.Nil.value))))))))))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Gt) {
            return union(union(union(new Data_List_Types.Cons("isZero", new Data_List_Types.Cons("pair", new Data_List_Types.Cons("fst", new Data_List_Types.Cons("snd", new Data_List_Types.Cons("succ", new Data_List_Types.Cons("sub", new Data_List_Types.Cons("not", new Data_List_Types.Cons("gt", Data_List_Types.Nil.value)))))))))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Lt) {
            return union(union(union(new Data_List_Types.Cons("isZero", new Data_List_Types.Cons("pair", new Data_List_Types.Cons("fst", new Data_List_Types.Cons("snd", new Data_List_Types.Cons("succ", new Data_List_Types.Cons("sub", new Data_List_Types.Cons("not", new Data_List_Types.Cons("lt", Data_List_Types.Nil.value)))))))))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof T_binop && expr.value0 instanceof Sub) {
            return union(union(union(new Data_List_Types.Cons("pair", new Data_List_Types.Cons("fst", new Data_List_Types.Cons("snd", new Data_List_Types.Cons("succ", new Data_List_Types.Cons("sub", Data_List_Types.Nil.value))))))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        return l;
    };
};
var eqUnop = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Not && y instanceof Not) {
                return true;
            };
            if (x instanceof Negate && y instanceof Negate) {
                return true;
            };
            return false;
        };
    }
};
var eq2 = /* #__PURE__ */ Data_Eq.eq(eqUnop);
var eqTermType = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Nat && y instanceof Nat) {
                return true;
            };
            if (x instanceof Bool && y instanceof Bool) {
                return true;
            };
            if (x instanceof Pair && y instanceof Pair) {
                return Data_Eq.eq(eqTermType)(x.value0)(y.value0) && Data_Eq.eq(eqTermType)(x.value1)(y.value1);
            };
            if (x instanceof Func && y instanceof Func) {
                return Data_Eq.eq(eqTermType)(x.value0)(y.value0) && Data_Eq.eq(eqTermType)(x.value1)(y.value1);
            };
            return false;
        };
    }
};
var eq3 = /* #__PURE__ */ Data_Eq.eq(eqTermType);
var eqBinop = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Add && y instanceof Add) {
                return true;
            };
            if (x instanceof Sub && y instanceof Sub) {
                return true;
            };
            if (x instanceof Mult && y instanceof Mult) {
                return true;
            };
            if (x instanceof Div && y instanceof Div) {
                return true;
            };
            if (x instanceof Lt && y instanceof Lt) {
                return true;
            };
            if (x instanceof Gt && y instanceof Gt) {
                return true;
            };
            if (x instanceof Eq && y instanceof Eq) {
                return true;
            };
            if (x instanceof Ne && y instanceof Ne) {
                return true;
            };
            if (x instanceof And && y instanceof And) {
                return true;
            };
            if (x instanceof Or && y instanceof Or) {
                return true;
            };
            return false;
        };
    }
};
var eq4 = /* #__PURE__ */ Data_Eq.eq(eqBinop);
var eqTerm = {
    eq: function (x) {
        return function (y) {
            if (x instanceof T_true && y instanceof T_true) {
                return true;
            };
            if (x instanceof T_false && y instanceof T_false) {
                return true;
            };
            if (x instanceof T_num && y instanceof T_num) {
                return x.value0 === y.value0;
            };
            if (x instanceof T_if && y instanceof T_if) {
                return Data_Eq.eq(eqTerm)(x.value0)(y.value0) && Data_Eq.eq(eqTerm)(x.value1)(y.value1) && Data_Eq.eq(eqTerm)(x.value2)(y.value2);
            };
            if (x instanceof T_pair && y instanceof T_pair) {
                return Data_Eq.eq(eqTerm)(x.value0)(y.value0) && Data_Eq.eq(eqTerm)(x.value1)(y.value1);
            };
            if (x instanceof T_fst && y instanceof T_fst) {
                return Data_Eq.eq(eqTerm)(x.value0)(y.value0);
            };
            if (x instanceof T_snd && y instanceof T_snd) {
                return Data_Eq.eq(eqTerm)(x.value0)(y.value0);
            };
            if (x instanceof T_binop && y instanceof T_binop) {
                return eq4(x.value0)(y.value0) && Data_Eq.eq(eqTerm)(x.value1)(y.value1) && Data_Eq.eq(eqTerm)(x.value2)(y.value2);
            };
            if (x instanceof T_unop && y instanceof T_unop) {
                return eq2(x.value0)(y.value0) && Data_Eq.eq(eqTerm)(x.value1)(y.value1);
            };
            if (x instanceof T_natRec && y instanceof T_natRec) {
                return Data_Eq.eq(eqTerm)(x.value0)(y.value0) && Data_Eq.eq(eqTerm)(x.value1)(y.value1) && Data_Eq.eq(eqTerm)(x.value2)(y.value2);
            };
            if (x instanceof T_var && y instanceof T_var) {
                return x.value0 === y.value0;
            };
            if (x instanceof T_func && y instanceof T_func) {
                return x.value0 === y.value0 && eq3(x.value1)(y.value1) && Data_Eq.eq(eqTerm)(x.value2)(y.value2);
            };
            if (x instanceof T_app && y instanceof T_app) {
                return Data_Eq.eq(eqTerm)(x.value0)(y.value0) && Data_Eq.eq(eqTerm)(x.value1)(y.value1);
            };
            if (x instanceof T_let && y instanceof T_let) {
                return x.value0 === y.value0 && eq3(x.value1)(y.value1) && Data_Eq.eq(eqTerm)(x.value2)(y.value2) && Data_Eq.eq(eqTerm)(x.value3)(y.value3);
            };
            if (x instanceof T_func_system && y instanceof T_func_system) {
                return x.value0 === y.value0 && eq3(x.value1)(y.value1) && Data_Eq.eq(eqTerm)(x.value2)(y.value2);
            };
            if (x instanceof T_var_system && y instanceof T_var_system) {
                return x.value0 === y.value0;
            };
            if (x instanceof T_error && y instanceof T_error) {
                return true;
            };
            return false;
        };
    }
};
export {
    Nat,
    Bool,
    Pair,
    Func,
    Add,
    Sub,
    Mult,
    Div,
    Lt,
    Gt,
    Eq,
    Ne,
    And,
    Or,
    Not,
    Negate,
    T_true,
    T_false,
    T_num,
    T_if,
    T_pair,
    T_fst,
    T_snd,
    T_binop,
    T_unop,
    T_natRec,
    T_var,
    T_func,
    T_app,
    T_let,
    T_func_system,
    T_var_system,
    T_error,
    showTerm,
    showType,
    makeNatural,
    listTermsUsed,
    showTerm$prime,
    showType$prime,
    eqTermType,
    eqTerm,
    eqBinop,
    eqUnop,
    showBinop,
    showUnop
};
