import * as CompileOmega from "../CompileOmega/index.js";
import * as CompileSTLC from "../CompileSTLC/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Structures from "../Structures/index.js";
import * as TokenParser from "../TokenParser/index.js";
var compileSTLCSimple = function (str) {
    return CompileSTLC.makeSTLCSimple(TokenParser.getTerm(str));
};
var compileSTLC = function (str) {
    return CompileSTLC.makeSTLC(TokenParser.getTerm(str));
};
var compileOmegaDefsUsed = function (str) {
    return CompileOmega.makeLOmegaDefsUsed(TokenParser.getTerm(str));
};
var compileOmegaDefs = function (str) {
    return CompileOmega.makeLOmegaDefs(TokenParser.getTerm(str));
};
var compileOmega = function (str) {
    return CompileOmega.makeLOmega(TokenParser.getTerm(str));
};
var b = /* #__PURE__ */ TokenParser.getTerm("if true then (if true then true else true) else true");
var main = function __do() {
    Effect_Console.log(Data_Show.show(Structures["showTerm$prime"])(b))();
    Effect_Console.log(CompileOmega.makeLOmega(b))();
    return Effect_Console.log(compileOmega("true"))();
};
export {
    compileOmega,
    compileSTLC,
    compileSTLCSimple,
    compileOmegaDefs,
    compileOmegaDefsUsed,
    b,
    main
};
