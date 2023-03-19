import * as Structures from "../Structures/index.js";
var shiftIncTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("p", new Structures.Pair(Structures.Nat.value, Structures.Nat.value), new Structures.T_pair(new Structures.T_snd(new Structures.T_var_system("p")), new Structures.T_binop(Structures.Add.value, new Structures.T_snd(new Structures.T_var_system("p")), new Structures.T_num(1))));
})();
var predTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_fst(new Structures.T_natRec(new Structures.T_var_system("n"), shiftIncTerm, new Structures.T_pair(new Structures.T_num(0), new Structures.T_num(0)))));
})();
var subTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_natRec(new Structures.T_var_system("m"), predTerm, new Structures.T_var_system("n"))));
})();
var isZeroTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_natRec(new Structures.T_var_system("n"), new Structures.T_func_system("b", Structures.Bool.value, Structures.T_false.value), Structures.T_true.value));
})();

// lt = \m n. not (isZero (sub n m));
var ltTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_unop(Structures.Not.value, new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("m")), new Structures.T_var_system("n"))))));
})();

// lte = \m n. isZero (sub m n);
var lteTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("m")), new Structures.T_var_system("n")))));
})();
var neTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_unop(Structures.Not.value, new Structures.T_binop(Structures.And.value, new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("m")), new Structures.T_var_system("n"))), new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("n")), new Structures.T_var_system("m")))))));
})();

// gte = \m n. isZero (sub n m);
var gteTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("n")), new Structures.T_var_system("m")))));
})();

// gt = \m n. not (isZero (sub m n));
var gtTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_unop(Structures.Not.value, new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("n")), new Structures.T_var_system("m"))))));
})();

// eq = \m n. and (isZero (sub m n)) (isZero (sub n m));
var eqTerm = /* #__PURE__ */ (function () {
    return new Structures.T_func_system("n", Structures.Nat.value, new Structures.T_func_system("m", Structures.Nat.value, new Structures.T_binop(Structures.And.value, new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("m")), new Structures.T_var_system("n"))), new Structures.T_app(isZeroTerm, new Structures.T_app(new Structures.T_app(subTerm, new Structures.T_var_system("n")), new Structures.T_var_system("m"))))));
})();
export {
    shiftIncTerm,
    predTerm,
    subTerm,
    isZeroTerm,
    eqTerm,
    neTerm,
    gteTerm,
    lteTerm,
    gtTerm,
    ltTerm
};
