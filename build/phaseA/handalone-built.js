require = require("requirejs");
require(["runtime"], function(runtimeLib) {

  var staticModules = {
    "builtin://option": {"name":"$src/arr/base/option.arr3",
"requires":[],
"dependencies":[],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":function(R, NAMESPACE, M) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,8,42,113,8,43,114],
[M,8,20,91,8,29,100],
[M,8,36,107,8,37,108],
[M,8,4,75,11,7,178],
[M,12,53,233,12,62,242],
[M,12,24,204,12,33,213],
[M,12,40,220,12,48,228],
[M,12,4,184,15,7,287],
[M,19,6,417,19,16,427],
[M,17,42,352,17,43,353],
[M,17,20,330,17,29,339],
[M,17,36,346,17,37,347],
[M,17,4,314,20,7,435],
[M,23,13,570,23,23,580],
[M,23,6,563,23,10,567],
[M,21,53,490,21,62,499],
[M,23,6,563,23,25,582],
[M,23,11,568,23,24,581],
[M,21,24,461,21,33,470],
[M,21,40,477,21,48,485],
[M,21,4,441,24,7,590],
[M,7,2,58,15,7,287],
[M,16,2,290,24,7,590],
[M,16,4,292,16,15,303],
[M,6,0,40,31,3,755],
[M,3,0,13,31,3,755]];
var builtins1 = NAMESPACE.get("builtins");
var nothing2 = NAMESPACE.get("nothing");
return R.loadModulesNew(NAMESPACE,[],function() {
var $toplevel5 = function($$resumer85) {
var $step4 = 0;
var $ans7 = D;
var $al8 = L[25];
try {
if(R.isActivationRecord($$resumer85)) {
$step4 = $$resumer85.step;
$al8 = $$resumer85.from;
$ans7 = $$resumer85.ans;
$resumer85 = $$resumer85.args[0];
provides83 = $$resumer85.vars[0];
some42 = $$resumer85.vars[1];
is$some78 = $$resumer85.vars[2];
none76 = $$resumer85.vars[3];
is$none74 = $$resumer85.vars[4];
is$Option72 = $$resumer85.vars[5];
Option70 = $$resumer85.vars[6];
Option17 = $$resumer85.vars[7];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step4) {
case 0: var Option9 = R.namedBrander("Option");
var Option17 = R.makeBranderAnn(Option9,"Option");
var Option68 = {"$var":D};
var Option70 = {"$var":D};
var is$Option72 = {"$var":D};
var is$none74 = {"$var":D};
var none76 = {"$var":D};
var is$some78 = {"$var":D};
var some42 = {"$var":D};
var $temp_full11 = function($self12,$v13) {
var $step10 = 0;
var $ans14 = D;
var $al15 = L[3];
try {
if(R.isActivationRecord($self12)) {
$step10 = $self12.step;
$al15 = $self12.from;
$ans14 = $self12.ans;
self12 = $self12.args[0];
v13 = $self12.args[1];
ann_check_temp16 = $self12.vars[0];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[3],2,$t);
}
var self12 = $self12;
var v13 = $v13;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step10) {
case 0: $step10 = 1;
$al15 = L[1];
R._checkAnn(L[1],Option17,self12);
break;
case 1: $step10 = 2;
$al15 = L[2];
R._checkAnn(L[2],R.Any,v13);
break;
case 2: var ann_check_temp16 = v13;
$step10 = 3;
$al15 = L[0];
R._checkAnn(L[0],R.Any,ann_check_temp16);
break;
case 3: $step10 = 4;
$ans14 = ann_check_temp16;
break;
case 4: ++R.GAS;
return $ans14;
default: throw "No case numbered " + $step10 + " in $temp_full11";
}
}
} catch($e18) {
if(R.isCont($e18) && ($step10 !== 4)) {
$e18.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al15,$temp_full11,$step10,[self12,v13],[ann_check_temp16]);
}
if(R.isPyretException($e18)) {
$e18.pyretStack.push($al15);
}
throw $e18;
}
};
var anf_singleton_variant_member49 = R.makeMethod1($temp_full11);
var $temp_full20 = function($self21,$$underscore22) {
var $step19 = 0;
var $ans23 = D;
var $al24 = L[7];
try {
if(R.isActivationRecord($self21)) {
$step19 = $self21.step;
$al24 = $self21.from;
$ans23 = $self21.ans;
self21 = $self21.args[0];
$underscore22 = $self21.args[1];
ann_check_temp25 = $self21.vars[0];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[7],2,$t);
}
var self21 = $self21;
var $underscore22 = $$underscore22;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step19) {
case 0: $step19 = 1;
$al24 = L[5];
R._checkAnn(L[5],Option17,self21);
break;
case 1: $step19 = 2;
$al24 = L[6];
R._checkAnn(L[6],R.Function,$underscore22);
break;
case 2: var ann_check_temp25 = self21;
$step19 = 3;
$al24 = L[4];
R._checkAnn(L[4],Option17,ann_check_temp25);
break;
case 3: $step19 = 4;
$ans23 = ann_check_temp25;
break;
case 4: ++R.GAS;
return $ans23;
default: throw "No case numbered " + $step19 + " in $temp_full20";
}
}
} catch($e26) {
if(R.isCont($e26) && ($step19 !== 4)) {
$e26.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al24,$temp_full20,$step19,[self21,$underscore22],[ann_check_temp25]);
}
if(R.isPyretException($e26)) {
$e26.pyretStack.push($al24);
}
throw $e26;
}
};
var anf_singleton_variant_member50 = R.makeMethod1($temp_full20);
var $temp_full28 = function($self29,$v30) {
var $step27 = 0;
var $ans31 = D;
var $al32 = L[12];
try {
if(R.isActivationRecord($self29)) {
$step27 = $self29.step;
$al32 = $self29.from;
$ans31 = $self29.ans;
self29 = $self29.args[0];
v30 = $self29.args[1];
ann_check_temp33 = $self29.vars[0];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[12],2,$t);
}
var self29 = $self29;
var v30 = $v30;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step27) {
case 0: $step27 = 1;
$al32 = L[10];
R._checkAnn(L[10],Option17,self29);
break;
case 1: $step27 = 2;
$al32 = L[11];
R._checkAnn(L[11],R.Any,v30);
break;
case 2: var ann_check_temp33 = G(self29,"value",L[8]);
$step27 = 3;
$al32 = L[9];
R._checkAnn(L[9],R.Any,ann_check_temp33);
break;
case 3: $step27 = 4;
$ans31 = ann_check_temp33;
break;
case 4: ++R.GAS;
return $ans31;
default: throw "No case numbered " + $step27 + " in $temp_full28";
}
}
} catch($e34) {
if(R.isCont($e34) && ($step27 !== 4)) {
$e34.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al32,$temp_full28,$step27,[self29,v30],[ann_check_temp33]);
}
if(R.isPyretException($e34)) {
$e34.pyretStack.push($al32);
}
throw $e34;
}
};
var anf_variant_member58 = R.makeMethod1($temp_full28);
var $temp_full36 = function($self37,$f38) {
var $step35 = 0;
var $ans39 = D;
var $al40 = L[20];
try {
if(R.isActivationRecord($self37)) {
$step35 = $self37.step;
$al40 = $self37.from;
$ans39 = $self37.ans;
self37 = $self37.args[0];
f38 = $self37.args[1];
ann_check_temp44 = $self37.vars[0];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[20],2,$t);
}
var self37 = $self37;
var f38 = $f38;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step35) {
case 0: $step35 = 1;
$al40 = L[18];
R._checkAnn(L[18],Option17,self37);
break;
case 1: $step35 = 2;
$al40 = L[19];
R._checkAnn(L[19],R.Function,f38);
break;
case 2: var anf_arg41 = G(self37,"value",L[13]);
$step35 = 3;
$al40 = L[17];
if(!(R.isFunction(f38))) {
R.ffi.throwNonFunApp($al40,f38);
}
$ans39 = f38.app(anf_arg41);
break;
case 3: var anf_arg43 = $ans39;
$step35 = 4;
$al40 = L[16];
if(!(R.isFunction(some42.$var === D?U(L[14],"some"):some42.$var))) {
R.ffi.throwNonFunApp($al40,some42.$var === D?U(L[14],"some"):some42.$var);
}
$ans39 = some42.$var === D?U(L[14],"some"):some42.$var.app(anf_arg43);
break;
case 4: var ann_check_temp44 = $ans39;
$step35 = 5;
$al40 = L[15];
R._checkAnn(L[15],Option17,ann_check_temp44);
break;
case 5: $step35 = 6;
$ans39 = ann_check_temp44;
break;
case 6: ++R.GAS;
return $ans39;
default: throw "No case numbered " + $step35 + " in $temp_full36";
}
}
} catch($e45) {
if(R.isCont($e45) && ($step35 !== 6)) {
$e45.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al40,$temp_full36,$step35,[self37,f38],[ann_check_temp44]);
}
if(R.isPyretException($e45)) {
$e45.pyretStack.push($al40);
}
throw $e45;
}
};
var anf_variant_member59 = R.makeMethod1($temp_full36);
var $none_getfields53 = function(f) {
return f();
};
var $none_getfieldsref51 = function(f) {
return f();
};
var $none_mutablemask52 = [];
var $none$base46 = {"or-else":anf_singleton_variant_member49,
"and-then":anf_singleton_variant_member50,
"_match":R.makeMatch("none",0)};
var $none$brands48 = {"$brand$none":true};
$none$brands48[Option9._brand] = true;
var $some_getfields62 = function(f) {
return f(this.dict["value"]);
};
var $some_getfieldsref60 = function(f,refmask) {
return f(R.derefField(this.dict["value"],false,refmask[0]));
};
var $some_mutablemask61 = [false];
var $some$base55 = {"$fieldNames":["value"],
"or-else":anf_variant_member58,
"and-then":anf_variant_member59,
"_match":R.makeMatch("some",1)};
var $some$brands57 = {"$brand$some":true};
$some$brands57[Option9._brand] = true;
var some64 = R.makeVariantConstructor(L[23],function() {
return [];
},[],[],[false],["value65"],$some_mutablemask61,$some$base55,$some$brands57,"some",$some_getfieldsref60,$some_getfields62,$some$base55);
var anf_assign67 = R.makeObject({"Option":R.makeFunction(function($val66) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[24],1,$t);
}
return R.makeBoolean(R.hasBrand($val66,Option9._brand));
}),
"is-none":R.makeFunction(function($val54) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[21],1,$t);
}
return R.makeBoolean(R.hasBrand($val54,"$brand$none"));
}),
"none":R.makeDataValue($none$base46,$none$brands48,"none",$none_getfieldsref51,$none_getfields53,-1,$none_mutablemask52,$none$base46),
"is-some":R.makeFunction(function($val63) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[22],1,$t);
}
return R.makeBoolean(R.hasBrand($val63,"$brand$some"));
}),
"some":some64});
Option68.$var = anf_assign67;
var anf_assign69 = G(Option68.$var,"Option",L[24]);
Option70.$var = anf_assign69;
var anf_assign71 = G(Option68.$var,"Option",L[24]);
is$Option72.$var = anf_assign71;
var anf_assign73 = G(Option68.$var,"is-none",L[21]);
is$none74.$var = anf_assign73;
var anf_assign75 = G(Option68.$var,"none",L[21]);
none76.$var = anf_assign75;
var anf_assign77 = G(Option68.$var,"is-some",L[22]);
is$some78.$var = anf_assign77;
var anf_assign79 = G(Option68.$var,"some",L[22]);
some42.$var = anf_assign79;
var provides83 = R.makeObject({"Option":Option70.$var,
"is-Option":is$Option72.$var,
"none":none76.$var,
"is-none":is$none74.$var,
"some":some42.$var,
"is-some":is$some78.$var});
$step4 = 1;
$al8 = L[24];
$field80 = R.getColonFieldLoc(builtins1,"current-checker",L[24]);
if(R.isMethod($field80)) {
$ans7 = $field80.full_meth(builtins1);
} else {
if(!(R.isFunction($field80))) {
R.ffi.throwNonFunApp(L[24],$field80);
}
$ans7 = $field80.app();
}
break;
case 1: var anf_method_obj81 = $ans7;
$step4 = 2;
$al8 = L[24];
$field82 = R.getColonFieldLoc(anf_method_obj81,"results",L[24]);
if(R.isMethod($field82)) {
$ans7 = $field82.full_meth(anf_method_obj81);
} else {
if(!(R.isFunction($field82))) {
R.ffi.throwNonFunApp(L[24],$field82);
}
$ans7 = $field82.app();
}
break;
case 2: var checks84 = $ans7;
$step4 = 3;
$ans7 = R.makeObject({"answer":nothing2,
"namespace":NAMESPACE,
"defined-values":{"some":some42.$var,
"is-some":is$some78.$var,
"none":none76.$var,
"is-none":is$none74.$var,
"is-Option":is$Option72.$var,
"Option":Option70.$var},
"defined-types":{"Option":Option17},
"provide-plus-types":R.makeObject({"values":provides83,
"types":{"Option":Option17}}),
"checks":checks84});
break;
case 3: ++R.GAS;
return $ans7;
default: throw "No case numbered " + $step4 + " in $toplevel5";
}
}
} catch($e86) {
if(R.isCont($e86) && ($step4 !== 3)) {
$e86.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al8,$toplevel5,$step4,[],[provides83,some42,is$some78,none76,is$none74,is$Option72,Option70,Option17]);
}
if(R.isPyretException($e86)) {
$e86.pyretStack.push($al8);
}
throw $e86;
}
};
return R.safeCall($toplevel5,function(moduleVal) {
R.modules["$src/arr/base/option.arr3"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
});
}}
,
    "builtinjs://just-option": {
  provides: {},
  requires: [
    { "import-type": "builtin", name: "option" },
  ],
  nativeRequires: [],
  theModule: function(runtime, namespace, uri, option) {
    return {
      option: option
    }
  }
}

  };

  var depMap = {
    "builtinjs://just-option": {
      "builtin(option)": "builtin://option"
    }
  };

  var runtime = runtimeLib.makeRuntime({});

  return runtime.loadBaseModules(staticModules, depMap, ["builtin://option"],  function(/* empty */) {
    console.log("Loaded option");
    return runtime.loadBaseModules(staticModules, depMap, ["builtinjs://just-option"],  function(justOpt) {
      console.log("Loaded justopt: ", justOpt);
    });
  })

/*
  loadJSModules(thisRuntime.namespace, [require("js/ffi-helpers")], function(f) {
    thisRuntime["ffi"] = ffi;
  });
  loadModulesNew(thisRuntime.namespace,
    [require("trove/srcloc")],
    function(srclocLib) {
      thisRuntime.srcloc = getField(srclocLib, "values");
    });
  loadModulesNew(thisRuntime.namespace, [require("trove/image-lib")], function(i) {
    thisRuntime["imageLib"] = getField(i, "internal");
  });

  // NOTE(joe): set a few of these explicitly to work with s-prim-app
  thisRuntime["throwMessageException"] = ffi.throwMessageException;
  thisRuntime["throwNoBranchesMatched"] = ffi.throwNoBranchesMatched;
  thisRuntime["throwNoCasesMatched"] = ffi.throwNoCasesMatched;
  thisRuntime["throwNonBooleanCondition"] = ffi.throwNonBooleanCondition;
  thisRuntime["throwNonBooleanOp"] = ffi.throwNonBooleanOp;

  var checkList = makeCheckType(ffi.isList, "List");
  thisRuntime["checkList"] = checkList;

  thisRuntime["checkEQ"] = makeCheckType(ffi.isEqualityResult, "EqualityResult");
*/
});
