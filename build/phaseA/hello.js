({
"requires":[{"import-type":"builtin",
"name":"arrays"},
{"import-type":"builtin",
"name":"error"},
{"import-type":"builtin",
"name":"lists"},
{"import-type":"builtin",
"name":"option"},
{"import-type":"builtin",
"name":"sets"}],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":function(R,NAMESPACE,M,$arrays15,$error16,$lists17,$option18,$sets19) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,2,0,1,2,18,19],
[M,2,6,7,2,17,18]];
var builtins1 = NAMESPACE.get("builtins");
var print2 = NAMESPACE.get("print");
var arrays4 = R.getField($arrays15,"values");
var error5 = R.getField($error16,"values");
var lists6 = R.getField($lists17,"values");
var option7 = R.getField($option18,"values");
var sets8 = R.getField($sets19,"values");
var arrays9 = R.getField($arrays15,"types");
var error10 = R.getField($error16,"types");
var lists11 = R.getField($lists17,"types");
var option12 = R.getField($option18,"types");
var sets13 = R.getField($sets19,"types");
NAMESPACE = R.addModuleToNamespace(NAMESPACE,["array-to-list-now","array-length","array-get-now","array-set-now","array-of","is-array","array-from-list","build-array","array"],["Array"],$arrays15);
NAMESPACE = R.addModuleToNamespace(NAMESPACE,[],[],$error16);
NAMESPACE = R.addModuleToNamespace(NAMESPACE,["fold4","fold3","fold2","fold","each4_n","each3_n","each2_n","each_n","each4","each3","each2","each","map4_n","map3_n","map2_n","map_n","map4","map3","map2","map","find","any","split-at","partition","filter","repeat","range-by","range","link","empty","is-link","is-empty","is-List","list"],["List"],$lists17);
NAMESPACE = R.addModuleToNamespace(NAMESPACE,["some","none","is-some","is-none","is-Option","Option"],["Option"],$option18);
NAMESPACE = R.addModuleToNamespace(NAMESPACE,["list-to-tree-set","list-to-list-set","list-to-set","empty-tree-set","empty-list-set","empty-set","list-set","tree-set","set"],["Set"],$sets19);
var some3 = NAMESPACE.get("some");
var $toplevel21 = function($$resumer32) {
var $step20 = 0;
var $ans23 = D;
var $al24 = L[0];
try {
if(R.isActivationRecord($$resumer32)) {
$step20 = $$resumer32.step;
$al24 = $$resumer32.from;
$ans23 = $$resumer32.ans;
$resumer32 = $$resumer32.args[0];
answer30 = $$resumer32.vars[0];
provides29 = $$resumer32.vars[1];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step20) {
case 0: $step20 = 1;
$al24 = L[1];
if(!(R.isFunction(some3))) {
R.ffi.throwNonFunApp($al24,some3);
}
$ans23 = some3.app(("hi!"));
break;
case 1: var anf_arg25 = $ans23;
$step20 = 2;
$al24 = L[0];
if(!(R.isFunction(print2))) {
R.ffi.throwNonFunApp($al24,print2);
}
$ans23 = print2.app(anf_arg25);
break;
case 2: var answer30 = $ans23;
var provides29 = R.makeObject({});
$step20 = 3;
$al24 = L[0];
$field26 = R.getColonFieldLoc(builtins1,"current-checker",L[0]);
if(R.isMethod($field26)) {
$ans23 = $field26.full_meth(builtins1);
} else {
if(!(R.isFunction($field26))) {
R.ffi.throwNonFunApp(L[0],$field26);
}
$ans23 = $field26.app();
}
break;
case 3: var anf_method_obj27 = $ans23;
$step20 = 4;
$al24 = L[0];
$field28 = R.getColonFieldLoc(anf_method_obj27,"results",L[0]);
if(R.isMethod($field28)) {
$ans23 = $field28.full_meth(anf_method_obj27);
} else {
if(!(R.isFunction($field28))) {
R.ffi.throwNonFunApp(L[0],$field28);
}
$ans23 = $field28.app();
}
break;
case 4: var checks31 = $ans23;
$step20 = 5;
$ans23 = R.makeObject({"answer":answer30,
"namespace":NAMESPACE,
"defined-values":{"sets":sets8,
"error":error5,
"option":option7,
"lists":lists6,
"arrays":arrays4},
"defined-types":{"sets":sets13,
"error":error10,
"option":option12,
"lists":lists11,
"arrays":arrays9},
"provide-plus-types":R.makeObject({"values":provides29,
"types":{}}),
"checks":checks31});
break;
case 5: ++R.GAS;
return $ans23;
default: throw "No case numbered " + $step20 + " in $toplevel21";
}
}
} catch($e33) {
if(R.isCont($e33) && ($step20 !== 5)) {
$e33.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al24,$toplevel21,$step20,[],[answer30,provides29]);
}
if(R.isPyretException($e33)) {
$e33.pyretStack.push($al24);
}
throw $e33;
}
};
return R.safeCall($toplevel21,function(moduleVal) {
R.modules["$file:///home/jpolitz/src/pyret$lang$nh/hello.arr14"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
