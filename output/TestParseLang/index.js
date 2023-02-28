import * as Parsing_String_Basic from "../Parsing.String.Basic/index.js";
import * as Parsing_Token from "../Parsing.Token/index.js";

//languageDef :: GenLanguageDef String 
var languageDef = {
    commentStart: "{-",
    commentEnd: "-}",
    commentLine: "--",
    nestedComments: false,
    identStart: Parsing_String_Basic.lower,
    identLetter: Parsing_String_Basic.alphaNum,
    opStart: /* #__PURE__ */ Parsing_String_Basic.oneOf([ "-", ":", "+", "*", "/", "|", "=", "~", "<" ]),
    opLetter: /* #__PURE__ */ Parsing_String_Basic.oneOf([ "=", "|" ]),
    reservedNames: [ "true", "false", "if", "then", "else", "fst", "snd", "let", "in", "func" ],
    reservedOpNames: [ "=", "+", "-", "*", "/", "||", "=", "~", "<", ":" ],
    caseSensitive: true
};
var token = /* #__PURE__ */ Parsing_Token.makeTokenParser(languageDef);
var parens = /* #__PURE__ */ (function () {
    return token.parens;
})();
var reserved = /* #__PURE__ */ (function () {
    return token.reserved;
})();
var identifier = /* #__PURE__ */ (function () {
    return token.identifier;
})();
export {
    languageDef,
    token,
    reserved,
    identifier,
    parens
};
