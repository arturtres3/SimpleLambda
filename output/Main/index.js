import * as CompileLambda2 from "../CompileLambda2/index.js";
import * as CompileLambdaC from "../CompileLambdaC/index.js";
import * as CompileOmega from "../CompileOmega/index.js";
import * as CompileSTLC from "../CompileSTLC/index.js";
import * as Data_Show from "../Data.Show/index.js";
import * as Effect_Console from "../Effect.Console/index.js";
import * as Structures from "../Structures/index.js";
import * as TokenParser from "../TokenParser/index.js";
var compileSTLCSimple = function (str) {
    return CompileSTLC.makeSTLCSimple(TokenParser.getTerm(str));
};
var compileSTLCDefs = function (str) {
    return CompileSTLC.makeSTLCDefs(TokenParser.getTerm(str));
};
var compileSTLC = function (str) {
    return CompileSTLC.makeSTLC(TokenParser.getTerm(str));
};
var compileOmegaDefs = function (str) {
    return CompileOmega.makeLOmegaDefs(TokenParser.getTerm(str));
};
var compileOmega = function (str) {
    return CompileOmega.makeLOmega(TokenParser.getTerm(str));
};
var compileLCDefs = function (str) {
    return CompileLambdaC.makeLCDefs(TokenParser.getTerm(str));
};
var compileLC = function (str) {
    return CompileLambdaC.makeLC(TokenParser.getTerm(str));
};
var compileL2Defs = function (str) {
    return CompileLambda2.makeL2Defs(TokenParser.getTerm(str));
};

// compileOmegaDefsUsed :: String -> String 
// compileOmegaDefsUsed str = makeLOmegaDefsUsed $ getTerm str
var compileL2 = function (str) {
    return CompileLambda2.makeL2(TokenParser.getTerm(str));
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
    compileSTLCDefs,
    compileSTLCSimple,
    compileOmegaDefs,
    compileL2,
    compileL2Defs,
    compileLC,
    compileLCDefs,
    b,
    main
};
