import * as Control_Alt from "../Control.Alt/index.js";
import * as Control_Applicative from "../Control.Applicative/index.js";
import * as Control_Bind from "../Control.Bind/index.js";
import * as Data_Array from "../Data.Array/index.js";
import * as Data_Either from "../Data.Either/index.js";
import * as Data_Int from "../Data.Int/index.js";
import * as Data_List_NonEmpty from "../Data.List.NonEmpty/index.js";
import * as Data_List_Types from "../Data.List.Types/index.js";
import * as Data_Maybe from "../Data.Maybe/index.js";
import * as Data_String_CodeUnits from "../Data.String.CodeUnits/index.js";
import * as Data_String_Common from "../Data.String.Common/index.js";
import * as Data_Tuple from "../Data.Tuple/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Structures from "../Structures/index.js";
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
var alt = /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT);
var bind = /* #__PURE__ */ Control_Bind.bind(Parsing.bindParserT);
var pure = /* #__PURE__ */ Control_Applicative.pure(Parsing.applicativeParserT);
var fromFoldable = /* #__PURE__ */ Data_Array.fromFoldable(Data_List_Types.foldableList);
var removeTab = function (str) {
    return Data_String_Common.replaceAll("\x09")("")(str);
};
var removeSpace = function (str) {
    return Data_String_Common.replaceAll(" ")("")(str);
};
var removeLine = function (str) {
    return Data_String_Common.replaceAll("\x0a")("")(str);
};
var removeWhiteSpace = function (str) {
    return removeLine(removeTab(removeSpace(str)));
};
var $lazy_parseType = /* #__PURE__ */ $runtime_lazy("parseType", "Parser", function () {
    return alt(Parsing_Combinators["try"](bind(Parsing_String.string("Nat"))(function () {
        return pure(Structures.Nat.value);
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("Bool"))(function () {
        return pure(Structures.Bool.value);
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String["char"]("("))(function () {
        return bind($lazy_parseType(66))(function (t1) {
            return bind(Parsing_String["char"]("X"))(function () {
                return bind($lazy_parseType(68))(function (t2) {
                    return bind(Parsing_String["char"](")"))(function () {
                        return pure(new Structures.Pair(t1, t2));
                    });
                });
            });
        });
    })))(Parsing_Combinators["try"](bind(Parsing_String["char"]("("))(function () {
        return bind($lazy_parseType(74))(function (t1) {
            return bind(Parsing_String.string("->"))(function () {
                return bind($lazy_parseType(76))(function (t2) {
                    return bind(Parsing_String["char"](")"))(function () {
                        return pure(new Structures.Func(t1, t2));
                    });
                });
            });
        });
    })))));
});
var parseType = /* #__PURE__ */ $lazy_parseType(55);
var $lazy_parseTerm = /* #__PURE__ */ $runtime_lazy("parseTerm", "Parser", function () {
    return alt(Parsing_Combinators["try"](bind(Parsing_String["char"]("("))(function () {
        return bind($lazy_parseTerm(84))(function (e) {
            return bind(Parsing_String["char"](")"))(function () {
                return pure(e);
            });
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String["char"]("("))(function () {
        return bind($lazy_parseTerm(90))(function (e1) {
            return bind(Parsing_String["char"](","))(function () {
                return bind($lazy_parseTerm(92))(function (e2) {
                    return bind(Parsing_String["char"](")"))(function () {
                        return pure(new Structures.T_pair(e1, e2));
                    });
                });
            });
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("true"))(function () {
        return pure(Structures.T_true.value);
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("false"))(function () {
        return pure(Structures.T_false.value);
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("fst"))(function () {
        return bind($lazy_parseTerm(106))(function (e) {
            return pure(new Structures.T_fst(e));
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("snd"))(function () {
        return bind($lazy_parseTerm(111))(function (e) {
            return pure(new Structures.T_snd(e));
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_Combinators.many1(Parsing_String_Basic.digit))(function (n) {
        return pure((function () {
            var v = Data_Int.fromString(Data_String_CodeUnits.fromCharArray(fromFoldable(Data_List_NonEmpty.toList(n))));
            if (v instanceof Data_Maybe.Just) {
                return new Structures.T_num(v.value0);
            };
            if (v instanceof Data_Maybe.Nothing) {
                return Structures.T_false.value;
            };
            throw new Error("Failed pattern match at Parser (line 116, column 23 - line 119, column 43): " + [ v.constructor.name ]);
        })());
    })))(Parsing_Combinators["try"](bind(Parsing_String.string("if"))(function () {
        return bind($lazy_parseTerm(124))(function (e1) {
            return bind(Parsing_String.string("then"))(function () {
                return bind($lazy_parseTerm(126))(function (e2) {
                    return bind(Parsing_String.string("else"))(function () {
                        return bind($lazy_parseTerm(128))(function (e3) {
                            return pure(new Structures.T_if(e1, e2, e3));
                        });
                    });
                });
            });
        });
    })))))))));
});
var parseTerm = /* #__PURE__ */ $lazy_parseTerm(80);

// numToNat ∷ Int → Term
// numToNat 0 = T_zero
// numToNat x = T_succ (numToNat $ x-1 )
var parseParam = /* #__PURE__ */ bind(Parsing_String_Basic.lower)(function (c1) {
    return bind(Parsing_Combinators.many(alt(Parsing_String_Basic.letter)(Parsing_String_Basic.digit)))(function (rest) {
        return bind(Parsing_String["char"](":"))(function () {
            return bind(parseType)(function (t) {
                return pure(new Data_Tuple.Tuple(Data_String_CodeUnits.fromCharArray(Data_Array.cons(c1)(fromFoldable(rest))), t));
            });
        });
    });
});
var parseManyParams = /* #__PURE__ */ bind(/* #__PURE__ */ Parsing_Combinators.sepEndBy1(parseParam)(/* #__PURE__ */ Parsing_String["char"](";")))(function (paramList) {
    return pure(Data_List_NonEmpty.toList(paramList));
});

//func(a:Nat;b:Bool)=(a,b)
//(T_func "a" Nat (T_func "b" Bool (T_pair (T_var "a") (T_var "b") )  ))
var makeFunc = function (v) {
    return function (v1) {
        if (v instanceof Data_List_Types.Nil) {
            return v1;
        };
        if (v instanceof Data_List_Types.Cons) {
            return new Structures.T_func(v.value0.value0, v.value0.value1, makeFunc(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Parser (line 49, column 1 - line 49, column 58): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var getTerm = function (inputText) {
    var v = Parsing.runParser(removeWhiteSpace(inputText))(parseTerm);
    if (v instanceof Data_Either.Left) {
        return Structures.T_false.value;
    };
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Parser (line 145, column 21 - line 147, column 35): " + [ v.constructor.name ]);
};
export {
    getTerm,
    makeFunc,
    parseManyParams,
    parseParam,
    parseTerm,
    parseType
};
