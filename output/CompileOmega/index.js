import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List from "../Data.List/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var union = /* #__PURE__ */ Data_List.union(Data_Eq.eqString);
var testTypes = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(And " + (testTypes(new Data_Maybe.Just(t.value0.value0)) + (" " + (testTypes(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    return "INCOMPLETO";
};

// import Structures (Term(..), TermType(..))
// import TypeSystem (typeInfer)
var shiftIncTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func("p", new Structures.Pair(Structures.Nat.value, Structures.Nat.value), new Structures.T_pair(new Structures.T_snd(new Structures.T_var("p")), new Structures.T_binop(Structures.Add.value, new Structures.T_snd(new Structures.T_var("p")), new Structures.T_num(1))));
})();
var makeTypesOmegaDefs = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(And " + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesOmegaDefs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileOmega (line 81, column 24 - line 91, column 42): " + [ t.constructor.name ]);
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
            return "(pair" + (" [" + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value0)) + ("][" + (makeTypesOmegaDefs(TypeSystem.typeInfer(env)(expr.value1)) + ("] " + (termToOmegaDefs(expr.value0)(env) + (" " + (termToOmegaDefs(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "(fst" + ((function () {
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
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return "(not " + (termToOmegaDefs(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(sub " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(eq " + (termToOmegaDefs(expr.value1)(env) + (" " + (termToOmegaDefs(expr.value2)(env) + ")")));
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
    throw new Error("Failed pattern match at CompileOmega (line 68, column 20 - line 78, column 42): " + [ t.constructor.name ]);
};
var makeNatural = function (v) {
    if (v === 0) {
        return "x";
    };
    return "(f " + (makeNatural(v - 1 | 0) + ")");
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
            return "(\\\\C:*.\\f:C->C.\\x:C." + (makeNatural(expr.value0) + ")");
        };
        if (expr instanceof Structures.T_func) {
            return "(\\x_" + (expr.value0 + (": " + (makeTypesOmega(new Data_Maybe.Just(expr.value1)) + (". " + (termToOmega(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
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
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return "((\\c:(forall C:*,C->C->C).\\a:(forall C:*,C->C->C).\\b:(forall C:*,C->C->C). (c[(forall C:*,C->C->C)]) a b)" + (termToOmega(expr.value1)(env) + (" " + ("(\\\\C:*.\\a:C.\\b:C.b) " + "(\\\\C:*.\\a:C.\\b:C.a))")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "((\\n: " + (makeTypesOmega(new Data_Maybe.Just(Structures.Nat.value)) + (". \\m: " + (makeTypesOmega(new Data_Maybe.Just(Structures.Nat.value)) + (". " + ("m [" + (makeTypesOmega(new Data_Maybe.Just(Structures.Nat.value)) + ("] " + ("(\\n: " + (makeTypesOmega(new Data_Maybe.Just(Structures.Nat.value)) + (". " + ("(\\\\A:*. \\\\B:*. \\p: (\\A:*, \\B:*, forall C:*, (A -> B -> C) -> C) A B. p [A] (\\a: A.\\b: B. a))" + ("[" + (makeTypesOmega(new Data_Maybe.Just(Structures.Nat.value)) + ("][" + (makeTypesOmega(new Data_Maybe.Just(Structures.Nat.value)) + ("]" + ("( n [" + (makeTypesOmega(new Data_Maybe.Just(new Structures.Pair(Structures.Nat.value, Structures.Nat.value))) + ("]" + (termToOmega(shiftIncTerm)(env) + (termToOmega(new Structures.T_pair(new Structures.T_num(0), new Structures.T_num(0)))(env) + ("))" + (" n)" + (termToOmega(expr.value1)(env) + (" " + (termToOmega(expr.value2)(env) + ")"))))))))))))))))))))))))));
        };
        return "INCOMPLETO";
    };
};
var makeLOmega = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToOmega(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 348, column 19 - line 350, column 46): " + [ v.constructor.name ]);
};
var makeDef = function (str) {
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
        return "  fst     = \\\\A:*. \\\\B:*. \\p: And A B. p [A] (\\a: A.\\b: B. a);";
    };
    if (str === "snd") {
        return "  snd     = \\\\A:*. \\\\B:*. \\p: And A B. p [B] (\\a: A.\\b: B. b);";
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
        return "  sub     = \\n: Nat. \\m:Nat. m [Nat] (\\n: Nat. fst [Nat] [Nat] (n [And Nat Nat] (\\p: And Nat Nat. (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)))) (pair [Nat] [Nat] 0 0))) n;";
    };
    return "?";
};
var makeDefsUsed = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return "\x0ain\x0a\x0a";
    };
    if (v instanceof Data_List_Types.Cons) {
        return makeDef(v.value0) + ("\x0a" + makeDefsUsed(v.value1));
    };
    throw new Error("Failed pattern match at CompileOmega (line 331, column 1 - line 331, column 40): " + [ v.constructor.name ]);
};
var makeDefsBlock = function (l) {
    return "typedef\x0a" + ("  Bool          = forall C:*, C -> C -> C;\x0a" + ("  Nat           = forall C:*, (C -> C) -> C -> C;\x0a" + ("  And           = \\A:*, \\B:*, forall C:*, (A -> B -> C) -> C;\x0a" + ("  Or            = \\A:*, \\B:*, forall C:*, (A -> C) -> (B -> C) -> C;\x0a" + ("end\x0a" + ("let\x0a" + makeDefsUsed(l)))))));
};
var listTermsUsed = function (expr) {
    return function (l) {
        if (expr instanceof Structures.T_true) {
            return union(new Data_List_Types.Cons("true", Data_List_Types.Nil.value))(l);
        };
        if (expr instanceof Structures.T_false) {
            return union(new Data_List_Types.Cons("false", Data_List_Types.Nil.value))(l);
        };
        if (expr instanceof Structures.T_if) {
            return union(union(union(union(new Data_List_Types.Cons("if", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l)))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_pair) {
            return union(union(union(new Data_List_Types.Cons("pair", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l)))(listTermsUsed(expr.value1)(l));
        };
        if (expr instanceof Structures.T_fst) {
            return union(union(new Data_List_Types.Cons("fst", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l));
        };
        if (expr instanceof Structures.T_snd) {
            return union(union(new Data_List_Types.Cons("snd", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value0)(l));
        };
        if (expr instanceof Structures.T_func) {
            return union(listTermsUsed(expr.value2)(l))(l);
        };
        if (expr instanceof Structures.T_app) {
            return union(union(listTermsUsed(expr.value0)(l))(l))(listTermsUsed(expr.value1)(l));
        };
        if (expr instanceof Structures.T_let) {
            return union(union(listTermsUsed(expr.value2)(l))(l))(listTermsUsed(expr.value3)(l));
        };
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return union(union(new Data_List_Types.Cons("not", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return union(union(union(new Data_List_Types.Cons("add", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return union(union(union(new Data_List_Types.Cons("and", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return union(union(union(new Data_List_Types.Cons("or", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Div) {
            return union(union(union(new Data_List_Types.Cons("div", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return union(union(union(new Data_List_Types.Cons("mult", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return union(union(union(new Data_List_Types.Cons("eq", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return union(union(union(new Data_List_Types.Cons("ne", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return union(union(union(new Data_List_Types.Cons("gt", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return union(union(union(new Data_List_Types.Cons("lt", Data_List_Types.Nil.value))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return union(union(union(new Data_List_Types.Cons("pair", new Data_List_Types.Cons("fst", new Data_List_Types.Cons("snd", new Data_List_Types.Cons("succ", new Data_List_Types.Cons("sub", Data_List_Types.Nil.value))))))(l))(listTermsUsed(expr.value1)(l)))(listTermsUsed(expr.value2)(l));
        };
        return l;
    };
};
var makeLOmegaDefsUsed = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlock(listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToOmegaDefs(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 359, column 27 - line 361, column 46): " + [ v.constructor.name ]);
};
var defs = /* #__PURE__ */ (function () {
    return "typedef\x0a" + ("  Bool          = forall C:*, C -> C -> C;\x0a" + ("  Nat           = forall C:*, (C -> C) -> C -> C;\x0a" + ("  And           = \\A:*, \\B:*, forall C:*, (A -> B -> C) -> C;\x0a" + ("  Or            = \\A:*, \\B:*, forall C:*, (A -> C) -> (B -> C) -> C;\x0a" + ("end\x0a" + ("let\x0a" + ("  true          = \\\\C:*. \\a: C. \\b: C. a;\x0a" + ("  false         = \\\\C:*. \\a: C. \\b: C. b;\x0a" + ("  if            = \\\\D:*. \\c: Bool. \\a: D. \\b: D. c [D] a b;\x0a" + ("  and           = \\a: Bool. \\b: Bool. if [Bool] a b false;\x0a" + ("  or            = \\a: Bool. \\b: Bool. if [Bool] a true b;\x0a" + ("  not           = \\b: Bool. if [Bool] b false true;\x0a\x0a" + ("  pair          = \\\\A:*. \\\\B:*. \\a: A. \\b: B. \\\\C:*. \\f: A->B->C. f a b;\x0a" + ("  fst           = \\\\A:*. \\\\B:*. \\p: And A B. p [A] (\\a: A.\\b: B. a) ;\x0a" + ("  snd           = \\\\A:*. \\\\B:*. \\p: And A B. p [B] (\\a: A.\\b: B. b) ;\x0a\x0a" + ("  succ          = \\n: Nat. \\\\C:*. \\f: C -> C. \\x :C. f (n [C] f x);\x0a " + ("  add           = \\n: Nat. \\m: Nat. n [Nat] succ m;\x0a" + ("  double        = \\n: Nat. n [Nat] succ n;\x0a" + ("  mult          = \\n: Nat. \\m: Nat. m [Nat] (\\p: Nat. add n p) 0;\x0a\x0a" + ("  shiftInc      = \\p: Product Nat Nat. (pair [Nat] [Nat] (snd [Nat] [Nat] p) (succ (snd [Nat] [Nat] p)));\x0a" + ("  pred          = \\n: Nat. fst [Nat] [Nat] (n [And Nat Nat] shiftInc (pair [Nat] [Nat] 0 0));\x0a" + ("  sub           = \\n: Nat. \\m: Nat. m [Nat] pred n;\x0a" + ("  isZero        = \\n:Nat. n [Bool] (\\b: Bool. false) true;\x0a" + ("  eq            = \\n:Nat. \\m:Nat. and (isZero (sub n m)) (isZero (sub m n));\x0a" + "in\x0a\x0a"))))))))))))))))))))))));
})();
var makeLOmegaDefs = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return defs + termToOmegaDefs(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileOmega (line 354, column 23 - line 356, column 46): " + [ v.constructor.name ]);
};
var contains = function ($copy_v) {
    return function ($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
            if (v instanceof Data_List_Types.Nil) {
                $tco_done = true;
                return false;
            };
            if (v instanceof Data_List_Types.Cons) {
                var $192 = v.value0 === v1;
                if ($192) {
                    $tco_done = true;
                    return true;
                };
                $tco_var_v = v.value1;
                $copy_v1 = v1;
                return;
            };
            throw new Error("Failed pattern match at CompileOmega (line 20, column 1 - line 20, column 47): " + [ v.constructor.name, v1.constructor.name ]);
        };
        while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_v, $copy_v1);
        };
        return $tco_result;
    };
};
export {
    shiftIncTerm,
    contains,
    listTermsUsed,
    testTypes,
    makeTypesOmega,
    makeTypesOmegaDefs,
    makeNatural,
    termToOmega,
    termToOmegaDefs,
    makeDef,
    defs,
    makeDefsUsed,
    makeDefsBlock,
    makeLOmega,
    makeLOmegaDefs,
    makeLOmegaDefsUsed
};
