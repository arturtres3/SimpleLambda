import * as Control_Alt from "../Control.Alt/index.js";
import * as Parsing from "../Parsing/index.js";
import * as Parsing_String from "../Parsing.String/index.js";
import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";
var languageDef = {
    commentStart: "{-",
    commentEnd: "-}",
    commentLine: "--",
    nestedComments: false,
    identStart: Parsing_String_Basic.lower,
    identLetter: /* #__PURE__ */ Control_Alt.alt(Parsing.altParserT)(Parsing_String_Basic.alphaNum)(/* #__PURE__ */ Parsing_String["char"]("_")),
    opStart: /* #__PURE__ */ Parsing_String_Basic.oneOf([ "-", ":", "+", "*", "|", "=", "~", "<", " ", "X" ]),
    opLetter: /* #__PURE__ */ Parsing_String_Basic.oneOf([ "=", "|", "&", ">" ]),
    reservedNames: [ "true", "false", "if", "then", "else", "fst", "snd", "let", "in", "func", "Nat", "Bool", "natRec" ],
    caseSensitive: true,
    reservedOpNames: [ "=", "+", "-", "*", "||", "&&", "=", "=>", "~", "<", ">", ":", "<-", "X" ]
};
var token = /* #__PURE__ */ Parsing_Token.makeTokenParser(languageDef);
export {
    languageDef,
    token
};
