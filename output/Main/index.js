import * as Compile from "../Compile/index.js";
import * as CompileSTLC from "../CompileSTLC/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Structures from "../Structures/index.js";
import * as TokenParser from "../TokenParser/index.js";
var testFunction = function (str) {
    return Compile.makeLOmega(TokenParser.getTerm(str));
};
var compileSTLCSimple = function (str) {
    return CompileSTLC.makeSTLCSimple(TokenParser.getTerm(str));
};
var compileSTLC = function (str) {
    return CompileSTLC.makeSTLC(TokenParser.getTerm(str));
};
var b = /* #__PURE__ */ TokenParser.getTerm("if true then (if true then true else true) else true");
var main = function __do() {
    Effect_Console.log(Data_Show.show(Structures["showTerm$prime"])(b))();
    Effect_Console.log(Compile.makeLOmega(b))();
    return Effect_Console.log(testFunction("true"))();
};
export {
    testFunction,
    compileSTLC,
    compileSTLCSimple,
    b,
    main
};
