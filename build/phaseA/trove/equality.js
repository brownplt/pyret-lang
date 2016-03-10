({
"requires":[{"import-type":"builtin",
"name":"error"}],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":
function(R,NAMESPACE, M, $error9) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,8,2,85,8,9,92],
[M,9,2,95,9,46,139],
[M,9,23,116,9,29,122],
[M,9,4,97,9,46,139],
[M,10,2,142,10,45,185],
[M,10,22,162,10,28,168],
[M,10,4,144,10,45,185],
[M,7,0,62,11,3,189],
[M,15,4,263,15,32,291],
[M,16,4,296,16,32,324],
[M,17,6,331,17,37,362],
[M,17,24,349,17,37,362],
[M,17,4,329,17,49,374],
[M,17,6,331,17,19,344],
[M,16,6,298,16,22,314],
[M,15,6,265,15,22,281],
[M,13,21,212,13,35,226],
[M,13,44,235,13,58,249],
[M,13,0,191,20,3,431],
[M,24,4,504,24,29,529],
[M,25,4,534,25,29,559],
[M,26,4,564,26,31,591],
[M,26,6,566,26,21,581],
[M,25,6,536,25,19,549],
[M,24,6,506,24,19,519],
[M,22,20,453,22,34,467],
[M,22,43,476,22,57,490],
[M,22,0,433,29,3,691],
[M,33,34,793,33,67,826],
[M,33,28,787,33,68,827],
[M,33,6,765,33,24,783],
[M,34,6,834,34,11,839],
[M,35,6,854,35,21,869],
[M,32,2,733,36,5,884],
[M,32,8,739,32,22,753],
[M,31,21,714,31,35,728],
[M,31,0,693,37,3,888],
[M,40,5,927,40,6,928],
[M,40,20,942,40,57,979],
[M,39,22,912,39,29,919],
[M,39,0,890,41,3,987],
[M,3,0,13,41,3,987]];
var $type$String1 = NAMESPACE.get("$type$String");
var raise2 = NAMESPACE.get("raise");
var $type$Boolean3 = NAMESPACE.get("$type$Boolean");
var builtins4 = NAMESPACE.get("builtins");
var nothing5 = NAMESPACE.get("nothing");
var error6 = R.getField($error9,"values");
var error7 = R.getField($error9,"types");
NAMESPACE = R.addModuleToNamespace(NAMESPACE,[],[],$error9);
var $toplevel11 = function($$resumer131) {
var $step10 = 0;
var $ans13 = D;
var $al14 = L[41];
try {
if(R.isActivationRecord($$resumer131)) {
$step10 = $$resumer131.step;
$al14 = $$resumer131.from;
$ans13 = $$resumer131.ans;
$resumer131 = $$resumer131.args[0];
provides129 = $$resumer131.vars[0];
from$boolean125 = $$resumer131.vars[1];
to$boolean116 = $$resumer131.vars[2];
equal$or97 = $$resumer131.vars[3];
equal$and82 = $$resumer131.vars[4];
Unknown63 = $$resumer131.vars[5];
is$Unknown61 = $$resumer131.vars[6];
NotEqual59 = $$resumer131.vars[7];
is$NotEqual57 = $$resumer131.vars[8];
Equal55 = $$resumer131.vars[9];
is$Equal53 = $$resumer131.vars[10];
is$EqualityResult51 = $$resumer131.vars[11];
EqualityResult49 = $$resumer131.vars[12];
EqualityResult79 = $$resumer131.vars[13];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step10) {
case 0: var EqualityResult15 = R.namedBrander("EqualityResult");
var EqualityResult79 = R.makeBranderAnn(EqualityResult15,"EqualityResult");
var EqualityResult47 = {"$var":D};
var EqualityResult49 = {"$var":D};
var is$EqualityResult51 = {"$var":D};
var is$Equal53 = {"$var":D};
var Equal55 = {"$var":D};
var is$NotEqual57 = {"$var":D};
var NotEqual59 = {"$var":D};
var is$Unknown61 = {"$var":D};
var Unknown63 = {"$var":D};
var equal$and82 = {"$var":D};
var equal$or97 = {"$var":D};
var to$boolean116 = {"$var":D};
var from$boolean125 = {"$var":D};
var $Equal_getfields21 = function(f) {
return f();
};
var $Equal_getfieldsref19 = function(f) {
return f();
};
var $Equal_mutablemask20 = [];
var $Equal$base16 = {"_match":R.makeMatch("Equal",0)};
var $Equal$brands18 = {"$brand$Equal":true};
$Equal$brands18[EqualityResult15._brand] = true;
var $NotEqual_getfields28 = function(f) {
return f(this.dict["reason"],this.dict["value1"],this.dict["value2"]);
};
var $NotEqual_getfieldsref26 = function(f,refmask) {
return f(R.derefField(this.dict["reason"],false,refmask[0]),R.derefField(this.dict["value1"],false,refmask[1]),R.derefField(this.dict["value2"],false,refmask[2]));
};
var $NotEqual_mutablemask27 = [false,false,false];
var $NotEqual$base23 = {"$fieldNames":["reason","value1","value2"],
"_match":R.makeMatch("NotEqual",3)};
var $NotEqual$brands25 = {"$brand$NotEqual":true};
$NotEqual$brands25[EqualityResult15._brand] = true;
var NotEqual30 = R.makeVariantConstructor(L[3],function() {
return [$type$String1];
},["reason31"],[L[2]],[false,false,false],["reason31","value132","value233"],$NotEqual_mutablemask27,$NotEqual$base23,$NotEqual$brands25,"NotEqual",$NotEqual_getfieldsref26,$NotEqual_getfields28,$NotEqual$base23);
var $Unknown_getfields39 = function(f) {
return f(this.dict["reason"],this.dict["value1"],this.dict["value2"]);
};
var $Unknown_getfieldsref37 = function(f,refmask) {
return f(R.derefField(this.dict["reason"],false,refmask[0]),R.derefField(this.dict["value1"],false,refmask[1]),R.derefField(this.dict["value2"],false,refmask[2]));
};
var $Unknown_mutablemask38 = [false,false,false];
var $Unknown$base34 = {"$fieldNames":["reason","value1","value2"],
"_match":R.makeMatch("Unknown",3)};
var $Unknown$brands36 = {"$brand$Unknown":true};
$Unknown$brands36[EqualityResult15._brand] = true;
var Unknown41 = R.makeVariantConstructor(L[6],function() {
return [$type$String1];
},["reason42"],[L[5]],[false,false,false],["reason42","value143","value244"],$Unknown_mutablemask38,$Unknown$base34,$Unknown$brands36,"Unknown",$Unknown_getfieldsref37,$Unknown_getfields39,$Unknown$base34);
var anf_assign46 = R.makeObject({"EqualityResult":R.makeFunction(function($val45) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[7],1,$t);
}
return R.makeBoolean(R.hasBrand($val45,EqualityResult15._brand));
}),
"is-Equal":R.makeFunction(function($val22) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[0],1,$t);
}
return R.makeBoolean(R.hasBrand($val22,"$brand$Equal"));
}),
"Equal":R.makeDataValue($Equal$base16,$Equal$brands18,"Equal",$Equal_getfieldsref19,$Equal_getfields21,-1,$Equal_mutablemask20,$Equal$base16),
"is-NotEqual":R.makeFunction(function($val29) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[1],1,$t);
}
return R.makeBoolean(R.hasBrand($val29,"$brand$NotEqual"));
}),
"NotEqual":NotEqual30,
"is-Unknown":R.makeFunction(function($val40) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[4],1,$t);
}
return R.makeBoolean(R.hasBrand($val40,"$brand$Unknown"));
}),
"Unknown":Unknown41});
EqualityResult47.$var = anf_assign46;
var anf_assign48 = G(EqualityResult47.$var,"EqualityResult",L[7]);
EqualityResult49.$var = anf_assign48;
var anf_assign50 = G(EqualityResult47.$var,"EqualityResult",L[7]);
is$EqualityResult51.$var = anf_assign50;
var anf_assign52 = G(EqualityResult47.$var,"is-Equal",L[0]);
is$Equal53.$var = anf_assign52;
var anf_assign54 = G(EqualityResult47.$var,"Equal",L[0]);
Equal55.$var = anf_assign54;
var anf_assign56 = G(EqualityResult47.$var,"is-NotEqual",L[1]);
is$NotEqual57.$var = anf_assign56;
var anf_assign58 = G(EqualityResult47.$var,"NotEqual",L[1]);
NotEqual59.$var = anf_assign58;
var anf_assign60 = G(EqualityResult47.$var,"is-Unknown",L[4]);
is$Unknown61.$var = anf_assign60;
var anf_assign62 = G(EqualityResult47.$var,"Unknown",L[4]);
Unknown63.$var = anf_assign62;
var $temp_lam65 = function($er166,$er267) {
var $step64 = 0;
var $ans68 = D;
var $al69 = L[18];
try {
if(R.isActivationRecord($er166)) {
$step64 = $er166.step;
$al69 = $er166.from;
$ans68 = $er166.ans;
er166 = $er166.args[0];
er267 = $er166.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[18],2,$t);
}
var er166 = $er166;
var er267 = $er267;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step64) {
case 0: $step64 = 1;
$al69 = L[16];
R._checkAnn(L[16],EqualityResult79,er166);
break;
case 1: $step64 = 2;
$al69 = L[17];
R._checkAnn(L[17],EqualityResult79,er267);
break;
case 2: $step64 = 3;
$al69 = L[15];
if(!(R.isFunction(is$NotEqual57.$var))) {
R.ffi.throwNonFunApp($al69,is$NotEqual57.$var);
}
$ans68 = is$NotEqual57.$var.app(er166);
break;
case 3: var anf_arg70 = $ans68;
$al69 = L[8];
var anf_if78 = R.checkWrapBoolean(anf_arg70);
if(R.isPyretTrue(anf_if78)) {
$step64 = 4;
} else {
$step64 = 5;
}
break;
case 4: $step64 = 16;
$ans68 = er166;
break;
case 5: $step64 = 6;
$al69 = L[14];
if(!(R.isFunction(is$NotEqual57.$var))) {
R.ffi.throwNonFunApp($al69,is$NotEqual57.$var);
}
$ans68 = is$NotEqual57.$var.app(er267);
break;
case 6: var anf_arg71 = $ans68;
$al69 = L[9];
var anf_if77 = R.checkWrapBoolean(anf_arg71);
if(R.isPyretTrue(anf_if77)) {
$step64 = 7;
} else {
$step64 = 8;
}
break;
case 7: $step64 = 16;
$ans68 = er267;
break;
case 8: $step64 = 9;
$al69 = L[13];
if(!(R.isFunction(is$Equal53.$var))) {
R.ffi.throwNonFunApp($al69,is$Equal53.$var);
}
$ans68 = is$Equal53.$var.app(er166);
break;
case 9: var anf_arg72 = $ans68;
$al69 = L[10];
var anf_if76 = R.checkWrapBoolean(anf_arg72);
if(R.isPyretTrue(anf_if76)) {
$step64 = 10;
} else {
$step64 = 12;
}
break;
case 10: $step64 = 11;
$al69 = L[11];
if(!(R.isFunction(is$Equal53.$var))) {
R.ffi.throwNonFunApp($al69,is$Equal53.$var);
}
$ans68 = is$Equal53.$var.app(er267);
break;
case 11: var anf_arg73 = $ans68;
$step64 = 13;
$al69 = L[10];
$ans68 = R.checkWrapBoolean(anf_arg73);
break;
case 12: $step64 = 13;
$ans68 = (false);
break;
case 13: var anf_arg74 = $ans68;
$al69 = L[12];
var anf_if75 = R.checkWrapBoolean(anf_arg74);
if(R.isPyretTrue(anf_if75)) {
$step64 = 14;
} else {
$step64 = 15;
}
break;
case 14: $step64 = 16;
$ans68 = Equal55.$var;
break;
case 15: $step64 = 16;
$ans68 = er166;
break;
case 16: ++R.GAS;
return $ans68;
default: throw "No case numbered " + $step64 + " in $temp_lam65";
}
}
} catch($e80) {
if(R.isCont($e80) && ($step64 !== 16)) {
$e80.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al69,$temp_lam65,$step64,[er166,er267],[]);
}
if(R.isPyretException($e80)) {
$e80.pyretStack.push($al69);
}
throw $e80;
}
};
var anf_assign81 = R.makeFunction($temp_lam65);
equal$and82.$var = anf_assign81;
var $temp_lam84 = function($er185,$er286) {
var $step83 = 0;
var $ans87 = D;
var $al88 = L[27];
try {
if(R.isActivationRecord($er185)) {
$step83 = $er185.step;
$al88 = $er185.from;
$ans87 = $er185.ans;
er185 = $er185.args[0];
er286 = $er185.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[27],2,$t);
}
var er185 = $er185;
var er286 = $er286;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step83) {
case 0: $step83 = 1;
$al88 = L[25];
R._checkAnn(L[25],EqualityResult79,er185);
break;
case 1: $step83 = 2;
$al88 = L[26];
R._checkAnn(L[26],EqualityResult79,er286);
break;
case 2: $step83 = 3;
$al88 = L[24];
if(!(R.isFunction(is$Equal53.$var))) {
R.ffi.throwNonFunApp($al88,is$Equal53.$var);
}
$ans87 = is$Equal53.$var.app(er185);
break;
case 3: var anf_arg89 = $ans87;
$al88 = L[19];
var anf_if94 = R.checkWrapBoolean(anf_arg89);
if(R.isPyretTrue(anf_if94)) {
$step83 = 4;
} else {
$step83 = 5;
}
break;
case 4: $step83 = 12;
$ans87 = er185;
break;
case 5: $step83 = 6;
$al88 = L[23];
if(!(R.isFunction(is$Equal53.$var))) {
R.ffi.throwNonFunApp($al88,is$Equal53.$var);
}
$ans87 = is$Equal53.$var.app(er286);
break;
case 6: var anf_arg90 = $ans87;
$al88 = L[20];
var anf_if93 = R.checkWrapBoolean(anf_arg90);
if(R.isPyretTrue(anf_if93)) {
$step83 = 7;
} else {
$step83 = 8;
}
break;
case 7: $step83 = 12;
$ans87 = er286;
break;
case 8: $step83 = 9;
$al88 = L[22];
if(!(R.isFunction(is$Unknown61.$var))) {
R.ffi.throwNonFunApp($al88,is$Unknown61.$var);
}
$ans87 = is$Unknown61.$var.app(er185);
break;
case 9: var anf_arg91 = $ans87;
$al88 = L[21];
var anf_if92 = R.checkWrapBoolean(anf_arg91);
if(R.isPyretTrue(anf_if92)) {
$step83 = 10;
} else {
$step83 = 11;
}
break;
case 10: $step83 = 12;
$ans87 = er185;
break;
case 11: $step83 = 12;
$ans87 = er286;
break;
case 12: ++R.GAS;
return $ans87;
default: throw "No case numbered " + $step83 + " in $temp_lam84";
}
}
} catch($e95) {
if(R.isCont($e95) && ($step83 !== 12)) {
$e95.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al88,$temp_lam84,$step83,[er185,er286],[]);
}
if(R.isPyretException($e95)) {
$e95.pyretStack.push($al88);
}
throw $e95;
}
};
var anf_assign96 = R.makeFunction($temp_lam84);
equal$or97.$var = anf_assign96;
var $temp_lam99 = function($er100) {
var $step98 = 0;
var $ans101 = D;
var $al102 = L[36];
try {
if(R.isActivationRecord($er100)) {
$step98 = $er100.step;
$al102 = $er100.from;
$ans101 = $er100.ans;
er100 = $er100.args[0];
cases103 = $er100.vars[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[36],1,$t);
}
var er100 = $er100;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step98) {
case 0: $step98 = 1;
$al102 = L[35];
R._checkAnn(L[35],EqualityResult79,er100);
break;
case 1: var cases103 = er100;
$step98 = 2;
$al102 = L[34];
R._checkAnn(L[34],EqualityResult79,cases103);
break;
case 2: var $cases_dispatch113 = {"Unknown":3,
"Equal":5,
"NotEqual":6};
$al102 = L[33];
$step98 = $cases_dispatch113[cases103.$name] || 7;
break;
case 3: if(cases103.$arity >= 0) {
if(3 !== cases103.$arity) {
R.ffi.throwCasesArityErrorC(L[30],3,cases103.$arity);
}
} else {
R.ffi.throwCasesSingletonErrorC(L[30],true);
}
var $fn109 = cases103.$constructor.$fieldNames;
var r104 = R.derefField(cases103.dict[$fn109[0]],cases103.$mut_fields_mask[0],false);
var v1105 = R.derefField(cases103.dict[$fn109[1]],cases103.$mut_fields_mask[1],false);
var v2106 = R.derefField(cases103.dict[$fn109[2]],cases103.$mut_fields_mask[2],false);
$step98 = 4;
$al102 = L[28];
$field107 = R.getColonFieldLoc(error6,"equality-failure",L[28]);
if(R.isMethod($field107)) {
$ans101 = $field107.full_meth(error6,r104,v1105,v2106);
} else {
if(!(R.isFunction($field107))) {
R.ffi.throwNonFunApp(L[28],$field107);
}
$ans101 = $field107.app(r104,v1105,v2106);
}
break;
case 4: var anf_arg108 = $ans101;
$step98 = 8;
$al102 = L[29];
if(!(R.isFunction(raise2))) {
R.ffi.throwNonFunApp($al102,raise2);
}
$ans101 = raise2.app(anf_arg108);
break;
case 5: if(cases103.$arity !== -1) {
R.ffi.throwCasesSingletonErrorC(L[31],false);
}
$step98 = 8;
$ans101 = (true);
break;
case 6: if(cases103.$arity >= 0) {
if(3 !== cases103.$arity) {
R.ffi.throwCasesArityErrorC(L[32],3,cases103.$arity);
}
} else {
R.ffi.throwCasesSingletonErrorC(L[32],true);
}
var $fn109 = cases103.$constructor.$fieldNames;
R.derefField(cases103.dict[$fn109[0]],cases103.$mut_fields_mask[0],false);
R.derefField(cases103.dict[$fn109[1]],cases103.$mut_fields_mask[1],false);
R.derefField(cases103.dict[$fn109[2]],cases103.$mut_fields_mask[2],false);
$step98 = 8;
$ans101 = (false);
break;
case 7: $step98 = 8;
$al102 = L[33];
$ans101 = R.throwNoCasesMatched(L[33],cases103);
break;
case 8: ++R.GAS;
return $ans101;
default: throw "No case numbered " + $step98 + " in $temp_lam99";
}
}
} catch($e114) {
if(R.isCont($e114) && ($step98 !== 8)) {
$e114.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al102,$temp_lam99,$step98,[er100],[cases103]);
}
if(R.isPyretException($e114)) {
$e114.pyretStack.push($al102);
}
throw $e114;
}
};
var anf_assign115 = R.makeFunction($temp_lam99);
to$boolean116.$var = anf_assign115;
var $temp_lam118 = function($b119) {
var $step117 = 0;
var $ans120 = D;
var $al121 = L[40];
try {
if(R.isActivationRecord($b119)) {
$step117 = $b119.step;
$al121 = $b119.from;
$ans120 = $b119.ans;
b119 = $b119.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[40],1,$t);
}
var b119 = $b119;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step117) {
case 0: $step117 = 1;
$al121 = L[39];
R._checkAnn(L[39],$type$Boolean3,b119);
break;
case 1: $al121 = L[37];
var anf_if122 = R.checkWrapBoolean(b119);
if(R.isPyretTrue(anf_if122)) {
$step117 = 2;
} else {
$step117 = 3;
}
break;
case 2: $step117 = 4;
$ans120 = Equal55.$var;
break;
case 3: $step117 = 4;
$al121 = L[38];
if(!(R.isFunction(NotEqual59.$var))) {
R.ffi.throwNonFunApp($al121,NotEqual59.$var);
}
$ans120 = NotEqual59.$var.app(("false"),("value1"),("value2"));
break;
case 4: ++R.GAS;
return $ans120;
default: throw "No case numbered " + $step117 + " in $temp_lam118";
}
}
} catch($e123) {
if(R.isCont($e123) && ($step117 !== 4)) {
$e123.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al121,$temp_lam118,$step117,[b119],[]);
}
if(R.isPyretException($e123)) {
$e123.pyretStack.push($al121);
}
throw $e123;
}
};
var anf_assign124 = R.makeFunction($temp_lam118);
from$boolean125.$var = anf_assign124;
var provides129 = R.makeObject({"EqualityResult":EqualityResult49.$var,
"is-EqualityResult":is$EqualityResult51.$var,
"Equal":Equal55.$var,
"is-Equal":is$Equal53.$var,
"NotEqual":NotEqual59.$var,
"is-NotEqual":is$NotEqual57.$var,
"Unknown":Unknown63.$var,
"is-Unknown":is$Unknown61.$var,
"equal-and":equal$and82.$var,
"equal-or":equal$or97.$var,
"to-boolean":to$boolean116.$var,
"from-boolean":from$boolean125.$var});
$step10 = 1;
$al14 = L[7];
$field126 = R.getColonFieldLoc(builtins4,"current-checker",L[7]);
if(R.isMethod($field126)) {
$ans13 = $field126.full_meth(builtins4);
} else {
if(!(R.isFunction($field126))) {
R.ffi.throwNonFunApp(L[7],$field126);
}
$ans13 = $field126.app();
}
break;
case 1: var anf_method_obj127 = $ans13;
$step10 = 2;
$al14 = L[7];
$field128 = R.getColonFieldLoc(anf_method_obj127,"results",L[7]);
if(R.isMethod($field128)) {
$ans13 = $field128.full_meth(anf_method_obj127);
} else {
if(!(R.isFunction($field128))) {
R.ffi.throwNonFunApp(L[7],$field128);
}
$ans13 = $field128.app();
}
break;
case 2: var checks130 = $ans13;
$step10 = 3;
$ans13 = R.makeObject({"answer":nothing5,
"namespace":NAMESPACE,
"defined-values":{"from-boolean":from$boolean125.$var,
"to-boolean":to$boolean116.$var,
"equal-or":equal$or97.$var,
"equal-and":equal$and82.$var,
"Unknown":Unknown63.$var,
"is-Unknown":is$Unknown61.$var,
"NotEqual":NotEqual59.$var,
"is-NotEqual":is$NotEqual57.$var,
"Equal":Equal55.$var,
"is-Equal":is$Equal53.$var,
"is-EqualityResult":is$EqualityResult51.$var,
"EqualityResult":EqualityResult49.$var,
"error":error6},
"defined-types":{"EqualityResult":EqualityResult79,
"error":error7},
"provide-plus-types":R.makeObject({"values":provides129,
"types":{"EqualityResult":EqualityResult79}}),
"checks":checks130});
break;
case 3: ++R.GAS;
return $ans13;
default: throw "No case numbered " + $step10 + " in $toplevel11";
}
}
} catch($e132) {
if(R.isCont($e132) && ($step10 !== 3)) {
$e132.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al14,$toplevel11,$step10,[],[provides129,from$boolean125,to$boolean116,equal$or97,equal$and82,Unknown63,is$Unknown61,NotEqual59,is$NotEqual57,Equal55,is$Equal53,is$EqualityResult51,EqualityResult49,EqualityResult79]);
}
if(R.isPyretException($e132)) {
$e132.pyretStack.push($al14);
}
throw $e132;
}
};
return R.safeCall($toplevel11,function(moduleVal) {
R.modules["$src/arr/base/equality.arr8"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
