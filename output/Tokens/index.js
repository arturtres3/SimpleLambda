import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";
var languageDef = {
    commentStart: "{-",
    commentEnd: "-}",
    commentLine: "--",
    nestedComments: false,
    identStart: Parsing_String_Basic.lower,
    identLetter: Parsing_String_Basic.alphaNum,
    opStart: /* #__PURE__ */ Parsing_String_Basic.oneOf([ "-", ":", "+", "*", "/", "|", "=", "~", "<", " ", "X" ]),
    opLetter: /* #__PURE__ */ Parsing_String_Basic.oneOf([ "=", "|" ]),
    reservedNames: [ "true", "false", "if", "then", "else", "fst", "snd", "let", "in", "func", "Nat", "Bool" ],
    caseSensitive: true,
    reservedOpNames: [ "=", "+", "-", "*", "/", "||", "=", "~", "<", ":", "<-", "X" ]
};
var token = /* #__PURE__ */ Parsing_Token.makeTokenParser(languageDef);
export {
    languageDef,
    token
};
