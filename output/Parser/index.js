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
import * as Parsing from "../Parsing/index.js";
import * as Parsing_Combinators from "../Parsing.Combinators/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Term from "../Term/index.js";
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
var $lazy_parseTerm = /* #__PURE__ */ $runtime_lazy("parseTerm", "Parser", function () {
    return alt(Parsing_Combinators["try"](bind(Parsing_String["char"]("("))(function () {
        return bind($lazy_parseTerm(33))(function (e) {
            return bind(Parsing_String["char"](")"))(function () {
                return pure(e);
            });
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String["char"]("("))(function () {
        return bind($lazy_parseTerm(39))(function (e1) {
            return bind(Parsing_String["char"](","))(function () {
                return bind($lazy_parseTerm(41))(function (e2) {
                    return bind(Parsing_String["char"](")"))(function () {
                        return pure(new Term.T_pair(e1, e2));
                    });
                });
            });
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("true"))(function () {
        return pure(Term.T_true.value);
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("false"))(function () {
        return pure(Term.T_false.value);
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("fst"))(function () {
        return bind($lazy_parseTerm(55))(function (e) {
            return pure(new Term.T_fst(e));
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_String.string("snd"))(function () {
        return bind($lazy_parseTerm(60))(function (e) {
            return pure(new Term.T_snd(e));
        });
    })))(alt(Parsing_Combinators["try"](bind(Parsing_Combinators.many1(Parsing_String_Basic.digit))(function (n) {
        return pure((function () {
            var v = Data_Int.fromString(Data_String_CodeUnits.fromCharArray(fromFoldable(Data_List_NonEmpty.toList(n))));
            if (v instanceof Data_Maybe.Just) {
                return new Term.T_num(v.value0);
            };
            if (v instanceof Data_Maybe.Nothing) {
                return Term.T_false.value;
            };
            throw new Error("Failed pattern match at Parser (line 65, column 23 - line 68, column 43): " + [ v.constructor.name ]);
        })());
    })))(Parsing_Combinators["try"](bind(Parsing_String.string("if"))(function () {
        return bind($lazy_parseTerm(73))(function (e1) {
            return bind(Parsing_String.string("then"))(function () {
                return bind($lazy_parseTerm(75))(function (e2) {
                    return bind(Parsing_String.string("else"))(function () {
                        return bind($lazy_parseTerm(77))(function (e3) {
                            return pure(new Term.T_if(e1, e2, e3));
                        });
                    });
                });
            });
        });
    })))))))));
});
var parseTerm = /* #__PURE__ */ $lazy_parseTerm(29);
var numToNat = function (v) {
    if (v === 0) {
        return Term.T_zero.value;
    };
    return new Term.T_succ(numToNat(v - 1 | 0));
};
var getTerm = function (inputText) {
    var v = Parsing.runParser(removeWhiteSpace(inputText))(parseTerm);
    if (v instanceof Data_Either.Left) {
        return Term.T_false.value;
    };
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Parser (line 94, column 21 - line 96, column 35): " + [ v.constructor.name ]);
};
export {
    getTerm,
    parseTerm
};
