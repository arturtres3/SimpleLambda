import * as Compile from "../Compile/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Parser from "../Parser/index.js";
import * as Term from "../Term/index.js";
var testFunction = function (str) {
    return Compile.makeLOmega(Parser.getTerm(str));
};
var b = /* #__PURE__ */ Parser.getTerm("if true then (if true then true else true) else true");
var main = function __do() {
    Effect_Console.log(Term.showTerm(b))();
    Effect_Console.log(Compile.makeLOmega(b))();
    return Effect_Console.log(testFunction("true"))();
};
export {
    b,
    main,
    testFunction
};
