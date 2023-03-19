import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TermLibrary from "../TermLibrary/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var makeTypesLCDefs = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "Bool";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "Nat";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "(And " + (makeTypesLCDefs(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypesLCDefs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesLCDefs(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesLCDefs(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileLambdaC (line 27, column 21 - line 37, column 42): " + [ t.constructor.name ]);
};
var termToLCDefs = function (expr) {
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
            return "(\\x_" + (expr.value0 + (": " + (makeTypesLCDefs(new Data_Maybe.Just(expr.value1)) + (". " + (termToLCDefs(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToLCDefs(expr.value0)(env) + (" " + (termToLCDefs(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToLCDefs(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "(if " + (makeTypesLCDefs(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToLCDefs(expr.value0)(env) + (" " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "(pair " + (makeTypesLCDefs(TypeSystem.typeInfer(env)(expr.value0)) + (" " + (makeTypesLCDefs(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToLCDefs(expr.value0)(env) + (" " + (termToLCDefs(expr.value1)(env) + ")")))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "(fst " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesLCDefs(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesLCDefs(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToLCDefs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "(snd " + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesLCDefs(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesLCDefs(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToLCDefs(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "(add " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "(mult " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "(and " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "(or " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return "(not " + (termToLCDefs(expr.value1)(env) + ")");
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(sub " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(eq " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(ne " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(gt " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(lt " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(natRec " + (makeTypesLCDefs(TypeSystem.typeInfer(env)(expr.value2)) + (" " + (termToLCDefs(expr.value0)(env) + (" " + (termToLCDefs(expr.value1)(env) + (" " + (termToLCDefs(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeTypesLC = function (t) {
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Bool) {
        return "(||C:*.C->C->C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Nat) {
        return "(||C:*. (C -> C) -> C -> C)";
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Pair) {
        return "((\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) " + (makeTypesLC(new Data_Maybe.Just(t.value0.value0)) + (" " + (makeTypesLC(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Just && t.value0 instanceof Structures.Func) {
        return "(" + (makeTypesLC(new Data_Maybe.Just(t.value0.value0)) + ("->" + (makeTypesLC(new Data_Maybe.Just(t.value0.value1)) + ")")));
    };
    if (t instanceof Data_Maybe.Nothing) {
        return "ERRO DE TIPO";
    };
    throw new Error("Failed pattern match at CompileLambdaC (line 14, column 17 - line 24, column 42): " + [ t.constructor.name ]);
};
var termToLC = function (expr) {
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
            return "(\\x_" + (expr.value0 + (": " + (makeTypesLC(new Data_Maybe.Just(expr.value1)) + (". " + (termToLC(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_func_system) {
            return "(\\" + (expr.value0 + (": " + (makeTypesLC(new Data_Maybe.Just(expr.value1)) + (". " + (termToLC(expr.value2)(TypeSystem.update(env)(expr.value0)(expr.value1)) + ")")))));
        };
        if (expr instanceof Structures.T_var_system) {
            return expr.value0;
        };
        if (expr instanceof Structures.T_app) {
            return "(" + (termToLC(expr.value0)(env) + (" " + (termToLC(expr.value1)(env) + ")")));
        };
        if (expr instanceof Structures.T_let) {
            return termToLC(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2))(env);
        };
        if (expr instanceof Structures.T_if) {
            return "((\\D:*.\\c:(|| C:*.C->C->C).\\a:D.\\b:D.(c D) a b)" + (" " + (makeTypesLC(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToLC(expr.value0)(env) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_pair) {
            return "((\\A:*. \\B:*. \\a: A. \\b: B. \\C:*. \\f: A->B->C. f a b)" + (" " + (makeTypesLC(TypeSystem.typeInfer(env)(expr.value0)) + (" " + (makeTypesLC(TypeSystem.typeInfer(env)(expr.value1)) + (" " + (termToLC(expr.value0)(env) + (" " + (termToLC(expr.value1)(env) + ")"))))))));
        };
        if (expr instanceof Structures.T_fst) {
            return "((\\A:*. \\B:*. \\p: (\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) A B. p A (\\a: A.\\b: B. a))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesLC(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesLC(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToLC(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_snd) {
            return "((\\A:*. \\B:*. \\p: (\\A:*. \\B:*. ||C:*. (A -> B -> C) -> C) A B. p B (\\a: A.\\b: B. b))" + ((function () {
                var v = TypeSystem.typeInfer(env)(expr.value0);
                if (v instanceof Data_Maybe.Just && v.value0 instanceof Structures.Pair) {
                    return " " + (makeTypesLC(new Data_Maybe.Just(v.value0.value0)) + (" " + (makeTypesLC(new Data_Maybe.Just(v.value0.value1)) + " ")));
                };
                return "Erro de Tipo [PARES]";
            })() + (termToLC(expr.value0)(env) + ")"));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
            return "((\\n: (||C:*. (C -> C) -> C -> C). \\m: (||C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. m C f (n C f x)) " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
            return "((\\n: (||C:*. (C -> C) -> C -> C). \\m: (||C:*. (C -> C) -> C -> C). \\C:*. \\f: C -> C. \\x :C. n C (m C f) x) " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
            return "((\\c:(||C:*.C->C->C).\\a:(||C:*.C->C->C).\\b:(||C:*.C->C->C). (c (||C:*.C->C->C)) a b)" + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + (" " + "(\\C:*.\\a:C.\\b:C.b))"))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
            return "((\\c:(||C:*.C->C->C).\\a:(||C:*.C->C->C).\\b:(||C:*.C->C->C). (c (||C:*.C->C->C)) a b)" + (termToLC(expr.value1)(env) + (" " + ("(\\C:*.\\a:C.\\b:C.a) " + (termToLC(expr.value2)(env) + ")"))));
        };
        if (expr instanceof Structures.T_unop && expr.value0 instanceof Structures.Not) {
            return "((\\c:(||C:*.C->C->C).\\a:(||C:*.C->C->C).\\b:(||C:*.C->C->C). (c (||C:*.C->C->C)) a b)" + (termToLC(expr.value1)(env) + (" " + ("(\\C:*.\\a:C.\\b:C.b) " + "(\\C:*.\\a:C.\\b:C.a))")));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
            return "(" + (termToLC(TermLibrary.subTerm)(env) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
            return "(" + (termToLC(TermLibrary.eqTerm)(env) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
            return "(" + (termToLC(TermLibrary.neTerm)(env) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
            return "(" + (termToLC(TermLibrary.gtTerm)(env) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
            return "(" + (termToLC(TermLibrary.ltTerm)(env) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")))));
        };
        if (expr instanceof Structures.T_natRec) {
            return "(" + (termToLC(expr.value0)(env) + (" " + (makeTypesLC(TypeSystem.typeInfer(env)(expr.value2)) + (" " + (termToLC(expr.value1)(env) + (" " + (termToLC(expr.value2)(env) + ")")))))));
        };
        return "INCOMPLETO";
    };
};
var makeLC = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToLC(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambdaC (line 296, column 15 - line 298, column 46): " + [ v.constructor.name ]);
};
var makeDefLC = function (str) {
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
        return "  fst     = \\A:*. \\B:*. \\p: And A B. p A (\\a: A.\\b: B. a);";
    };
    if (str === "snd") {
        return "  snd     = \\A:*. \\B:*. \\p: And A B. p B (\\a: A.\\b: B. b);";
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
        return "  sub     = \\n: Nat. \\m:Nat. m Nat (\\n: Nat. fst Nat Nat (n (And Nat Nat) (\\p: And Nat Nat. (pair Nat Nat (snd Nat Nat p) (succ (snd Nat Nat p)))) (pair Nat Nat 0 0))) n;";
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
var makeDefsUsed = function (v) {
    if (v instanceof Data_List_Types.Nil) {
        return "\x0a";
    };
    if (v instanceof Data_List_Types.Cons) {
        return makeDefLC(v.value0) + ("\x0a" + makeDefsUsed(v.value1));
    };
    throw new Error("Failed pattern match at CompileLambdaC (line 280, column 1 - line 280, column 40): " + [ v.constructor.name ]);
};
var makeDefsBlock = function (l) {
    return "let\x0a" + ("  Bool          = ||C:*. C -> C -> C;\x0a" + ("  Nat           = ||C:*. (C -> C) -> C -> C;\x0a" + ("  And           = \\A:*. \\B:*. ||C:*. (A -> B -> C) -> C;\x0a" + ("  Or            = \\A:*. \\B:*. ||C:*. (A -> C) -> (B -> C) -> C;\x0a\x0a" + (makeDefsUsed(l) + "in\x0a\x0a")))));
};
var makeLCDefs = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlock(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToLCDefs(expr)(TypeSystem.emptyEnv);
    };
    if (v instanceof Data_Maybe.Nothing) {
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambdaC (line 301, column 19 - line 303, column 46): " + [ v.constructor.name ]);
};
export {
    makeTypesLC,
    makeTypesLCDefs,
    termToLC,
    termToLCDefs,
    makeDefLC,
    makeDefsUsed,
    makeDefsBlock,
    makeLC,
    makeLCDefs
};
