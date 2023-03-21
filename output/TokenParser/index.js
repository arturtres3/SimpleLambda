import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Control_Lazy from "../Control.Lazy/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Functor from "../Data.Functor/index.js";
import * as Data_List_NonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Data_Tuple_Nested from "../Data.Tuple.Nested/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_Expr from "../Parsing.Expr/index.js";
import * as Structures from "../Structures/index.js";
import * as Tokens from "../Tokens/index.js";
var $runtime_lazy = function (name, moduleName, init) {
    var state = 0;
    var val;
    return function (lineNumber) {
        if (state === 2) return val;
        if (state === 1) throw new ReferenceError(name + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
        state = 1;
        val = init();
        state = 2;
        return val;
    };
};
var voidLeft = /* #__PURE__ */ Data_Functor.voidLeft(Parsing.functorParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var discard = /* #__PURE__ */ Control_Bind.discard(Control_Bind.discardUnit)(Parsing.bindParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var fix = /* #__PURE__ */ Control_Lazy.fix(Parsing.lazyParserT);
var map = /* #__PURE__ */ Data_Functor.map(Parsing.functorParserT);
var whiteSpace = /* #__PURE__ */ (function () {
    return Tokens.token.whiteSpace;
})();
var reservedOp = /* #__PURE__ */ (function () {
    return Tokens.token.reservedOp;
})();

// <|> expr ENTRA EM LOOP
var table = /* #__PURE__ */ (function () {
    return [ [ new Parsing_Expr.Prefix(voidLeft(reservedOp("!"))(Structures.T_unop.create(Structures.Not.value))), new Parsing_Expr.Prefix(voidLeft(reservedOp("-"))(Structures.T_unop.create(Structures.Negate.value))) ], [ new Parsing_Expr.Infix(voidLeft(reservedOp("*"))(Structures.T_binop.create(Structures.Mult.value)), Parsing_Expr.AssocLeft.value) ], [ new Parsing_Expr.Infix(voidLeft(reservedOp("+"))(Structures.T_binop.create(Structures.Add.value)), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp("-"))(Structures.T_binop.create(Structures.Sub.value)), Parsing_Expr.AssocLeft.value) ], [ new Parsing_Expr.Infix(voidLeft(reservedOp("<"))(Structures.T_binop.create(Structures.Lt.value)), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp(">"))(Structures.T_binop.create(Structures.Gt.value)), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp("=="))(Structures.T_binop.create(Structures.Eq.value)), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp("!="))(Structures.T_binop.create(Structures.Ne.value)), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp("&&"))(Structures.T_binop.create(Structures.And.value)), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp("||"))(Structures.T_binop.create(Structures.Or.value)), Parsing_Expr.AssocLeft.value) ], [ new Parsing_Expr.Infix(voidLeft(whiteSpace)(Structures.T_app.create), Parsing_Expr.AssocLeft.value) ], [ new Parsing_Expr.Infix(voidLeft(reservedOp(","))(Structures.T_pair.create), Parsing_Expr.AssocLeft.value) ] ];
})();
var tableType = /* #__PURE__ */ (function () {
    return [ [ new Parsing_Expr.Infix(voidLeft(reservedOp("X"))(Structures.Pair.create), Parsing_Expr.AssocLeft.value), new Parsing_Expr.Infix(voidLeft(reservedOp("->"))(Structures.Func.create), Parsing_Expr.AssocRight.value) ] ];
})();
var reserved = /* #__PURE__ */ (function () {
    return Tokens.token.reserved;
})();
var parseNatRecParams = function (exp) {
    return bind(exp)(function (e1) {
        return discard(reservedOp(";"))(function () {
            return bind(exp)(function (e2) {
                return discard(reservedOp(";"))(function () {
                    return bind(exp)(function (e3) {
                        return pure(Data_Tuple_Nested.tuple3(e1)(e2)(e3));
                    });
                });
            });
        });
    });
};
var parens = /* #__PURE__ */ (function () {
    return Tokens.token.parens;
})();
var parseType = function (p) {
    return alt(parens(p))(alt(voidLeft(reserved("Nat"))(Structures.Nat.value))(voidLeft(reserved("Bool"))(Structures.Bool.value)));
};
var typeNote = /* #__PURE__ */ (function () {
    var allTypes = function (p) {
        return Parsing_Expr.buildExprParser(tableType)(parseType(p));
    };
    return fix(allTypes);
})();
var makeFunc = function (v) {
    return function (v1) {
        if (v instanceof Data_List_Types.Nil) {
            return v1;
        };
        if (v instanceof Data_List_Types.Cons) {
            return new Structures.T_func(v.value0.value0, v.value0.value1, makeFunc(v.value1)(v1));
        };
        throw new Error("Failed pattern match at TokenParser (line 56, column 1 - line 56, column 41): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var integer = /* #__PURE__ */ (function () {
    return Tokens.token.integer;
})();
var identifier = /* #__PURE__ */ (function () {
    return Tokens.token.identifier;
})();
var parseParam = /* #__PURE__ */ bind(identifier)(function (id) {
    return discard(reservedOp(":"))(function () {
        return bind(typeNote)(function (t) {
            return pure(new Data_Tuple.Tuple(id, t));
        });
    });
});
var parseManyParams = /* #__PURE__ */ bind(/* #__PURE__ */ Parsing_Combinators.sepEndBy1(parseParam)(/* #__PURE__ */ reservedOp(";")))(function (paramList) {
    return pure(Data_List_NonEmpty.toList(paramList));
});
var parseTerm = function (p) {
    return alt(parens(p))(alt(voidLeft(reserved("true"))(Structures.T_true.value))(alt(voidLeft(reserved("false"))(Structures.T_false.value))(alt(map(Structures.T_var.create)(identifier))(alt(map(Structures.T_num.create)(integer))(alt($lazy_parseFunc(124))(alt($lazy_parseIf(125))(alt($lazy_parseLet(126))(alt($lazy_parseFst(127))(alt($lazy_parseSnd(128))($lazy_parseNatRec(129)))))))))));
};
var $lazy_expr = /* #__PURE__ */ $runtime_lazy("expr", "TokenParser", function () {
    var allExprs = function (p) {
        return Parsing_Expr.buildExprParser(table)(parseTerm(p));
    };
    return fix(allExprs);
});
var $lazy_parseFst = /* #__PURE__ */ $runtime_lazy("parseFst", "TokenParser", function () {
    return discard(reserved("fst"))(function () {
        return map(Structures.T_fst.create)($lazy_expr(111));
    });
});
var $lazy_parseFunc = /* #__PURE__ */ $runtime_lazy("parseFunc", "TokenParser", function () {
    return discard(reserved("func"))(function () {
        return bind(parens(parseManyParams))(function (list) {
            return discard(reservedOp("="))(function () {
                return bind($lazy_expr(65))(function (e1) {
                    return pure(makeFunc(list)(e1));
                });
            });
        });
    });
});
var $lazy_parseIf = /* #__PURE__ */ $runtime_lazy("parseIf", "TokenParser", function () {
    return discard(reserved("if"))(function () {
        return bind($lazy_expr(101))(function (e1) {
            return discard(reserved("then"))(function () {
                return bind($lazy_expr(103))(function (e2) {
                    return discard(reserved("else"))(function () {
                        return bind($lazy_expr(105))(function (e3) {
                            return pure(new Structures.T_if(e1, e2, e3));
                        });
                    });
                });
            });
        });
    });
});
var $lazy_parseLet = /* #__PURE__ */ $runtime_lazy("parseLet", "TokenParser", function () {
    return discard(reserved("let"))(function () {
        return bind(parseParam)(function (param) {
            return discard(reservedOp("="))(function () {
                return bind($lazy_expr(74))(function (e1) {
                    return discard(reserved("in"))(function () {
                        return bind($lazy_expr(76))(function (e2) {
                            return pure(new Structures.T_let(Data_Tuple.fst(param), Data_Tuple.snd(param), e1, e2));
                        });
                    });
                });
            });
        });
    });
});
var $lazy_parseNatRec = /* #__PURE__ */ $runtime_lazy("parseNatRec", "TokenParser", function () {
    return discard(reserved("natRec"))(function () {
        return bind(parens(parseNatRecParams($lazy_expr(94))))(function (triple) {
            return pure(new Structures.T_natRec(Data_Tuple_Nested.get1(triple), Data_Tuple_Nested.get2(triple), Data_Tuple_Nested.get3(triple)));
        });
    });
});
var $lazy_parseSnd = /* #__PURE__ */ $runtime_lazy("parseSnd", "TokenParser", function () {
    return discard(reserved("snd"))(function () {
        return map(Structures.T_snd.create)($lazy_expr(116));
    });
});
var expr = /* #__PURE__ */ $lazy_expr(150);
var parseFst = /* #__PURE__ */ $lazy_parseFst(108);
var parseFunc = /* #__PURE__ */ $lazy_parseFunc(60);
var parseIf = /* #__PURE__ */ $lazy_parseIf(98);
var parseLet = /* #__PURE__ */ $lazy_parseLet(69);
var parseNatRec = /* #__PURE__ */ $lazy_parseNatRec(91);
var parseSnd = /* #__PURE__ */ $lazy_parseSnd(113);

// faz parsing do espaco em branco antes do primeiro token 
var parseExpr = /* #__PURE__ */ discard(/* #__PURE__ */ Parsing_Combinators.optional(/* #__PURE__ */ Parsing_Combinators["try"](whiteSpace)))(function () {
    return expr;
});
var getTerm = function (inputText) {
    var v = Parsing.runParser(inputText)(parseExpr);
    if (v instanceof Data_Either.Left) {
        return Structures.T_false.value;
    };
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    throw new Error("Failed pattern match at TokenParser (line 180, column 21 - line 182, column 35): " + [ v.constructor.name ]);
};
export {
    parens,
    reservedOp,
    reserved,
    identifier,
    whiteSpace,
    integer,
    parseParam,
    parseManyParams,
    makeFunc,
    parseFunc,
    parseLet,
    parseNatRecParams,
    parseNatRec,
    parseIf,
    parseFst,
    parseSnd,
    parseTerm,
    table,
    expr,
    parseExpr,
    parseType,
    tableType,
    typeNote,
    getTerm
};
