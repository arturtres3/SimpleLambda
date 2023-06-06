import * as Data_Eq from "../Data.Eq/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Structures from "../Structures/index.js";
import * as TermLibrary from "../TermLibrary/index.js";
import * as TypeSystem from "../TypeSystem/index.js";
var eq = /* #__PURE__ */ Data_Eq.eq(Structures.eqTerm);
var termToLambdaDefs = function (expr) {
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
        return "(\\x_" + (expr.value0 + (". " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_app) {
        return "(" + (termToLambdaDefs(expr.value0) + (" " + (termToLambdaDefs(expr.value1) + ")")));
    };
    if (expr instanceof Structures.T_let) {
        return termToLambdaDefs(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2));
    };
    if (expr instanceof Structures.T_if) {
        return "(if " + (termToLambdaDefs(expr.value0) + (" " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_pair) {
        return "(pair " + (termToLambdaDefs(expr.value0) + (" " + (termToLambdaDefs(expr.value1) + ")")));
    };
    if (expr instanceof Structures.T_fst) {
        return "(fst " + (termToLambdaDefs(expr.value0) + ")");
    };
    if (expr instanceof Structures.T_snd) {
        return "(snd " + (termToLambdaDefs(expr.value0) + ")");
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
        return "(add " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
        return "(mult " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
        return "(and " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
        return "(or " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_unop) {
        return "(not " + (termToLambdaDefs(expr.value1) + ")");
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
        return "(sub " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
        return "(eq " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
        return "(ne " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
        return "(gt " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
        return "(lt " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_natRec) {
        return "(natRec " + (termToLambdaDefs(expr.value0) + (" " + (termToLambdaDefs(expr.value1) + (" " + (termToLambdaDefs(expr.value2) + ")")))));
    };
    return "ERRO";
};
var termToLambda = function (expr) {
    if (expr instanceof Structures.T_true) {
        return "(\\a.\\b.a)";
    };
    if (expr instanceof Structures.T_false) {
        return "(\\a.\\b.b)";
    };
    if (expr instanceof Structures.T_var) {
        return "x_" + expr.value0;
    };
    if (expr instanceof Structures.T_num) {
        return "(\\f.\\x." + (Structures.makeNatural(expr.value0) + ")");
    };
    if (expr instanceof Structures.T_func) {
        return "(\\x_" + (expr.value0 + (". " + (termToLambda(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_func_system) {
        return "(\\" + (expr.value0 + (". " + (termToLambda(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_var_system) {
        return expr.value0;
    };
    if (expr instanceof Structures.T_app) {
        return "(" + (termToLambda(expr.value0) + (" " + (termToLambda(expr.value1) + ")")));
    };
    if (expr instanceof Structures.T_let) {
        return termToLambda(new Structures.T_app(new Structures.T_func(expr.value0, expr.value1, expr.value3), expr.value2));
    };
    if (expr instanceof Structures.T_if) {
        return "((\\c.\\a.\\b. c a b) " + (termToLambda(expr.value0) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_pair) {
        return "((\\a. \\b. \\c. c a b) " + (termToLambda(expr.value0) + (" " + (termToLambda(expr.value1) + ")")));
    };
    if (expr instanceof Structures.T_fst) {
        return "((\\p. p (\\a.\\b. a)) " + (termToLambda(expr.value0) + ")");
    };
    if (expr instanceof Structures.T_snd) {
        return "((\\p. p (\\a.\\b. b))     " + (termToLambda(expr.value0) + ")");
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Add) {
        return "((\\n. \\m. \\f. \\x. m f (n f x)) " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Mult) {
        return "((\\n. \\m.  \\f. \\x. n (m f) x) " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.And) {
        return "((\\c.\\a.\\b. c a b) " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + (" " + "(\\a.\\b.b))"))));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Or) {
        return "((\\c.\\a.\\b. c a b) " + (termToLambda(expr.value1) + (" " + ("(\\a.\\b.a) " + (termToLambda(expr.value2) + ")"))));
    };
    if (expr instanceof Structures.T_unop) {
        return "((\\c.\\a.\\b. c a b) " + (termToLambda(expr.value1) + (" " + ("(\\a.\\b.b) " + "(\\a.\\b.a))")));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Sub) {
        return "(" + (termToLambda(TermLibrary.subTerm) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Eq) {
        return "(" + (termToLambda(TermLibrary.eqTerm) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Ne) {
        return "(" + (termToLambda(TermLibrary.neTerm) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Gt) {
        return "(" + (termToLambda(TermLibrary.gtTerm) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_binop && expr.value0 instanceof Structures.Lt) {
        return "(" + (termToLambda(TermLibrary.ltTerm) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_natRec) {
        return "(" + (termToLambda(expr.value0) + (" " + (termToLambda(expr.value1) + (" " + (termToLambda(expr.value2) + ")")))));
    };
    if (expr instanceof Structures.T_error) {
        return "ERRO";
    };
    throw new Error("Failed pattern match at CompileLambda (line 14, column 21 - line 93, column 26): " + [ expr.constructor.name ]);
};
var makeLambda = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return termToLambda(expr);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $112 = eq(expr)(Structures.T_error.value);
        if ($112) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambda (line 214, column 19 - line 216, column 97): " + [ v.constructor.name ]);
};
var makeDefLambda = function (str) {
    if (str === "true") {
        return "  true    = \\a. \\b. a;";
    };
    if (str === "false") {
        return "  false   = \\a. \\b. b;";
    };
    if (str === "if") {
        return "  if      = \\c. \\a. \\b. c a b;";
    };
    if (str === "pair") {
        return "  pair    = \\a. \\b. \\f. f a b;";
    };
    if (str === "fst") {
        return "  fst     = \\p. p (\\a.\\b. a);";
    };
    if (str === "snd") {
        return "  snd     = \\p. p (\\a.\\b. b);";
    };
    if (str === "add") {
        return "  add     = \\n. \\m. \\f. \\x. m f (n f x);";
    };
    if (str === "mult") {
        return "  mult    = \\n. \\m. \\f. \\x. n (m f) x;";
    };
    if (str === "and") {
        return "  and     = \\a. \\b. a b (\\a. \\b. b);";
    };
    if (str === "or") {
        return "  or      = \\a. \\b. a (\\a. \\b. a) b;";
    };
    if (str === "not") {
        return "  not     = \\a. a (\\a. \\b. b) (\\a. \\b. a);";
    };
    if (str === "succ") {
        return "  succ    = \\n. \\f. \\x. f (n f x);";
    };
    if (str === "sub") {
        return "  sub     = \\n. \\m. m (\\n. fst (n (\\p. (pair (snd p) (succ (snd p)))) (pair 0 0))) n;";
    };
    if (str === "isZero") {
        return "  isZero  = \\n. n (\\b. (\\a. \\b. b)) (\\a. \\b. a);";
    };
    if (str === "eq") {
        return "  eq     = \\n. \\m. and (isZero (sub n m)) (isZero (sub m n));";
    };
    if (str === "ne") {
        return "  ne     = \\n. \\m. not (and (isZero (sub n m)) (isZero (sub m n)));";
    };
    if (str === "gt") {
        return "  gt     = \\n. \\m. not (isZero (sub n m)) ;";
    };
    if (str === "lt") {
        return "  lt     = \\n. \\m. not (isZero (sub m n)) ;";
    };
    if (str === "natRec") {
        return "  natRec  = \\n. \\step. \\init. n step init;";
    };
    return "?";
};
var makeDefsBlock = function (l) {
    if (l instanceof Data_List_Types.Cons) {
        return "let\x0a" + (Structures.makeDefsUsed(makeDefLambda)(l) + "in\x0a\x0a");
    };
    if (l instanceof Data_List_Types.Nil) {
        return "\x0a\x0a";
    };
    throw new Error("Failed pattern match at CompileLambda (line 207, column 10 - line 209, column 28): " + [ l.constructor.name ]);
};
var makeLambdaDefs = function (expr) {
    var v = TypeSystem.typeInfer(TypeSystem.emptyEnv)(expr);
    if (v instanceof Data_Maybe.Just) {
        return makeDefsBlock(Structures.listTermsUsed(expr)(Data_List_Types.Nil.value)) + termToLambdaDefs(expr);
    };
    if (v instanceof Data_Maybe.Nothing) {
        var $119 = eq(expr)(Structures.T_error.value);
        if ($119) {
            return "Sintaxe Incorreta";
        };
        return "Erro de Tipo";
    };
    throw new Error("Failed pattern match at CompileLambda (line 219, column 23 - line 221, column 97): " + [ v.constructor.name ]);
};
export {
    termToLambda,
    termToLambdaDefs,
    makeDefLambda,
    makeDefsBlock,
    makeLambda,
    makeLambdaDefs
};
