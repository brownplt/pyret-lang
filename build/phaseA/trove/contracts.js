({
"requires":[{"import-type":"builtin",
"name":"error-display"},
{"import-type":"builtin",
"name":"lists"}],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":
function(R,NAMESPACE,M,$ED10,$L11) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,9,39,155,9,48,164],
[M,9,2,118,9,49,165],
[M,8,0,89,10,3,169],
[M,14,25,230,14,56,261],
[M,14,4,209,14,60,265],
[M,17,6,342,17,17,353],
[M,17,32,368,17,40,376],
[M,17,6,342,17,48,384],
[M,16,4,315,18,7,392],
[M,21,6,473,21,17,484],
[M,21,32,499,21,40,507],
[M,21,6,473,21,47,514],
[M,20,4,446,22,7,522],
[M,13,2,194,14,60,265],
[M,15,2,268,18,7,392],
[M,15,24,290,15,37,303],
[M,15,4,270,15,38,304],
[M,19,2,395,22,7,522],
[M,19,28,421,19,41,434],
[M,19,4,397,19,42,435],
[M,12,0,171,23,3,526],
[M,28,7,643,28,15,651],
[M,29,9,662,29,24,677],
[M,29,26,679,29,40,693],
[M,29,61,714,29,69,722],
[M,30,10,735,30,29,754],
[M,30,47,772,30,57,782],
[M,30,39,764,30,58,783],
[M,30,31,756,30,59,784],
[M,30,61,786,30,87,812],
[M,31,8,823,31,19,834],
[M,31,8,823,31,53,868],
[M,29,42,695,29,70,723],
[M,27,4,595,32,7,877],
[M,35,7,966,35,15,974],
[M,36,9,985,36,16,992],
[M,36,18,994,36,42,1018],
[M,36,60,1036,36,70,1046],
[M,36,52,1028,36,71,1047],
[M,36,44,1020,36,72,1048],
[M,37,10,1060,37,35,1085],
[M,37,56,1106,37,64,1114],
[M,37,37,1087,37,65,1115],
[M,34,4,918,38,7,1125],
[M,26,2,549,32,7,877],
[M,26,4,551,26,37,584],
[M,33,2,880,38,7,1125],
[M,33,4,882,33,29,907],
[M,25,0,528,39,3,1129],
[M,46,11,1313,46,19,1321],
[M,47,13,1336,47,20,1343],
[M,47,22,1345,47,69,1392],
[M,48,14,1438,48,33,1457],
[M,49,12,1472,49,23,1483],
[M,49,12,1472,49,49,1509],
[M,47,71,1394,47,99,1422],
[M,45,8,1287,50,11,1522],
[M,51,9,1533,51,17,1541],
[M,52,11,1554,52,18,1561],
[M,52,20,1563,52,74,1617],
[M,53,10,1630,53,21,1641],
[M,53,10,1630,53,47,1667],
[M,44,6,1251,53,49,1669],
[M,43,4,1204,54,7,1677],
[M,57,17,1785,57,24,1792],
[M,58,8,1802,58,34,1828],
[M,58,52,1846,58,61,1855],
[M,58,44,1838,58,62,1856],
[M,58,36,1830,58,63,1857],
[M,59,8,1867,59,47,1906],
[M,60,8,1941,60,27,1960],
[M,61,9,1971,61,22,1984],
[M,64,13,2054,64,21,2062],
[M,64,41,2082,64,49,2090],
[M,64,32,2073,64,50,2091],
[M,65,15,2108,65,22,2115],
[M,65,24,2117,65,53,2146],
[M,65,55,2148,65,76,2169],
[M,63,10,2033,66,13,2185],
[M,67,11,2198,67,19,2206],
[M,67,39,2226,67,47,2234],
[M,67,30,2217,67,48,2235],
[M,62,8,1994,67,50,2237],
[M,69,9,2259,69,17,2267],
[M,69,37,2287,69,45,2295],
[M,69,28,2278,69,46,2296],
[M,59,49,1908,59,72,1931],
[M,56,4,1727,71,7,2315],
[M,74,17,2422,74,24,2429],
[M,75,8,2439,75,32,2463],
[M,75,50,2481,75,64,2495],
[M,75,42,2473,75,65,2496],
[M,75,34,2465,75,66,2497],
[M,76,8,2507,76,39,2538],
[M,76,66,2565,76,107,2606],
[M,77,9,2617,77,22,2630],
[M,80,13,2700,80,21,2708],
[M,80,41,2728,80,49,2736],
[M,80,32,2719,80,50,2737],
[M,81,15,2754,81,22,2761],
[M,81,24,2763,81,53,2792],
[M,81,55,2794,81,76,2815],
[M,79,10,2679,82,13,2831],
[M,83,11,2844,83,19,2852],
[M,83,39,2872,83,47,2880],
[M,83,30,2863,83,48,2881],
[M,78,8,2640,83,50,2883],
[M,85,9,2905,85,17,2913],
[M,85,37,2933,85,45,2941],
[M,85,28,2924,85,46,2942],
[M,76,41,2540,76,64,2563],
[M,73,4,2364,87,7,2961],
[M,90,7,3088,90,15,3096],
[M,91,9,3107,91,16,3114],
[M,92,10,3126,92,45,3161],
[M,93,49,3212,93,75,3238],
[M,93,10,3173,93,76,3239],
[M,94,10,3251,94,42,3283],
[M,95,17,3303,95,25,3311],
[M,95,8,3294,95,26,3312],
[M,96,9,3323,96,16,3330],
[M,96,18,3332,96,37,3351],
[M,97,29,3383,97,48,3402],
[M,97,53,3407,97,80,3434],
[M,97,29,3383,97,81,3435],
[M,97,8,3362,97,82,3436],
[M,89,4,3040,99,7,3452],
[M,102,7,3548,102,15,3556],
[M,103,9,3567,103,16,3574],
[M,103,18,3576,103,42,3600],
[M,104,83,3685,104,93,3695],
[M,104,49,3651,104,94,3696],
[M,104,10,3612,104,95,3697],
[M,105,10,3709,105,44,3743],
[M,105,62,3761,105,71,3770],
[M,105,54,3753,105,72,3771],
[M,105,46,3745,105,73,3772],
[M,104,57,3659,104,93,3695],
[M,101,4,3500,106,7,3782],
[M,42,2,1153,54,7,1677],
[M,42,28,1179,42,41,1192],
[M,42,4,1155,42,42,1193],
[M,55,2,1680,71,7,2315],
[M,55,31,1709,55,37,1715],
[M,55,4,1682,55,38,1716],
[M,72,2,2318,87,7,2961],
[M,72,4,2320,72,37,2353],
[M,88,2,2964,99,7,3452],
[M,88,46,3008,88,52,3014],
[M,88,46,3008,88,66,3028],
[M,88,4,2966,88,67,3029],
[M,100,2,3455,106,7,3782],
[M,100,4,3457,100,36,3489],
[M,41,0,1131,107,3,3786],
[M,3,0,17,107,3,3786]];
var _plus1 = NAMESPACE.get("_plus");
var $type$String2 = NAMESPACE.get("$type$String");
var builtins3 = NAMESPACE.get("builtins");
var nothing4 = NAMESPACE.get("nothing");
var ED5 = R.getField($ED10,"values");
var L6 = R.getField($L11,"values");
var ED7 = R.getField($ED10,"types");
var L8 = R.getField($L11,"types");
NAMESPACE = R.addModuleToNamespace(NAMESPACE,[],[],$ED10);
NAMESPACE = R.addModuleToNamespace(NAMESPACE,[],[],$L11);
var $toplevel13 = function($$resumer527) {
var $step12 = 0;
var $ans15 = D;
var $al16 = L[154];
try {
if(R.isActivationRecord($$resumer527)) {
$step12 = $$resumer527.step;
$al16 = $$resumer527.from;
$ans15 = $$resumer527.ans;
$resumer527 = $$resumer527.args[0];
provides525 = $$resumer527.vars[0];
dot$ann$not$present519 = $$resumer527.vars[1];
is$dot$ann$not$present517 = $$resumer527.vars[2];
record$fields$fail515 = $$resumer527.vars[3];
is$record$fields$fail513 = $$resumer527.vars[4];
predicate$failure511 = $$resumer527.vars[5];
is$predicate$failure509 = $$resumer527.vars[6];
type$mismatch507 = $$resumer527.vars[7];
is$type$mismatch505 = $$resumer527.vars[8];
ref$init503 = $$resumer527.vars[9];
is$ref$init501 = $$resumer527.vars[10];
is$FailureReason499 = $$resumer527.vars[11];
FailureReason497 = $$resumer527.vars[12];
missing$field200 = $$resumer527.vars[13];
is$missing$field198 = $$resumer527.vars[14];
field$failure196 = $$resumer527.vars[15];
is$field$failure194 = $$resumer527.vars[16];
is$FieldFailure192 = $$resumer527.vars[17];
FieldFailure190 = $$resumer527.vars[18];
fail$arg105 = $$resumer527.vars[19];
is$fail$arg103 = $$resumer527.vars[20];
fail101 = $$resumer527.vars[21];
is$fail99 = $$resumer527.vars[22];
ok97 = $$resumer527.vars[23];
is$ok95 = $$resumer527.vars[24];
is$ContractResult93 = $$resumer527.vars[25];
ContractResult91 = $$resumer527.vars[26];
draw$and$highlight30 = $$resumer527.vars[27];
FailureReason73 = $$resumer527.vars[28];
FieldFailure524 = $$resumer527.vars[29];
ContractResult523 = $$resumer527.vars[30];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step12) {
case 0: var ContractResult17 = R.namedBrander("ContractResult");
var ContractResult523 = R.makeBranderAnn(ContractResult17,"ContractResult");
var FieldFailure18 = R.namedBrander("FieldFailure");
var FieldFailure524 = R.makeBranderAnn(FieldFailure18,"FieldFailure");
var FailureReason19 = R.namedBrander("FailureReason");
var FailureReason73 = R.makeBranderAnn(FailureReason19,"FailureReason");
var draw$and$highlight30 = {"$var":D};
var ContractResult89 = {"$var":D};
var ContractResult91 = {"$var":D};
var is$ContractResult93 = {"$var":D};
var is$ok95 = {"$var":D};
var ok97 = {"$var":D};
var is$fail99 = {"$var":D};
var fail101 = {"$var":D};
var is$fail$arg103 = {"$var":D};
var fail$arg105 = {"$var":D};
var FieldFailure188 = {"$var":D};
var FieldFailure190 = {"$var":D};
var is$FieldFailure192 = {"$var":D};
var is$field$failure194 = {"$var":D};
var field$failure196 = {"$var":D};
var is$missing$field198 = {"$var":D};
var missing$field200 = {"$var":D};
var FailureReason495 = {"$var":D};
var FailureReason497 = {"$var":D};
var is$FailureReason499 = {"$var":D};
var is$ref$init501 = {"$var":D};
var ref$init503 = {"$var":D};
var is$type$mismatch505 = {"$var":D};
var type$mismatch507 = {"$var":D};
var is$predicate$failure509 = {"$var":D};
var predicate$failure511 = {"$var":D};
var is$record$fields$fail513 = {"$var":D};
var record$fields$fail515 = {"$var":D};
var is$dot$ann$not$present517 = {"$var":D};
var dot$ann$not$present519 = {"$var":D};
var $temp_lam21 = function($l22) {
var $step20 = 0;
var $ans23 = D;
var $al24 = L[2];
try {
if(R.isActivationRecord($l22)) {
$step20 = $l22.step;
$al24 = $l22.from;
$ans23 = $l22.ans;
l22 = $l22.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[2],1,$t);
}
var l22 = $l22;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step20) {
case 0: $step20 = 1;
$al24 = L[0];
$field25 = R.getColonFieldLoc(ED5,"loc",L[0]);
if(R.isMethod($field25)) {
$ans23 = $field25.full_meth(ED5,l22);
} else {
if(!(R.isFunction($field25))) {
R.ffi.throwNonFunApp(L[0],$field25);
}
$ans23 = $field25.app(l22);
}
break;
case 1: var anf_arg26 = $ans23;
$step20 = 2;
$al24 = L[1];
$field27 = R.getColonFieldLoc(ED5,"loc-display",L[1]);
if(R.isMethod($field27)) {
$ans23 = $field27.full_meth(ED5,l22,("error-highlight"),anf_arg26);
} else {
if(!(R.isFunction($field27))) {
R.ffi.throwNonFunApp(L[1],$field27);
}
$ans23 = $field27.app(l22,("error-highlight"),anf_arg26);
}
break;
case 2: ++R.GAS;
return $ans23;
default: throw "No case numbered " + $step20 + " in $temp_lam21";
}
}
} catch($e28) {
if(R.isCont($e28) && ($step20 !== 2)) {
$e28.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al24,$temp_lam21,$step20,[l22],[]);
}
if(R.isPyretException($e28)) {
$e28.pyretStack.push($al24);
}
throw $e28;
}
};
var anf_assign29 = R.makeFunction($temp_lam21);
draw$and$highlight30.$var = anf_assign29;
var $temp_full32 = function($self33) {
var $step31 = 0;
var $ans34 = D;
var $al35 = L[4];
try {
if(R.isActivationRecord($self33)) {
$step31 = $self33.step;
$al35 = $self33.from;
$ans34 = $self33.ans;
self33 = $self33.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[4],1,$t);
}
var self33 = $self33;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step31) {
case 0: $step31 = 1;
$al35 = L[3];
$field36 = R.getColonFieldLoc(ED5,"text",L[3]);
if(R.isMethod($field36)) {
$ans34 = $field36.full_meth(ED5,("There were no errors"));
} else {
if(!(R.isFunction($field36))) {
R.ffi.throwNonFunApp(L[3],$field36);
}
$ans34 = $field36.app(("There were no errors"));
}
break;
case 1: ++R.GAS;
return $ans34;
default: throw "No case numbered " + $step31 + " in $temp_full32";
}
}
} catch($e37) {
if(R.isCont($e37) && ($step31 !== 1)) {
$e37.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al35,$temp_full32,$step31,[self33],[]);
}
if(R.isPyretException($e37)) {
$e37.pyretStack.push($al35);
}
throw $e37;
}
};
var anf_singleton_variant_member59 = R.makeMethod0($temp_full32);
var $temp_full39 = function($self40) {
var $step38 = 0;
var $ans41 = D;
var $al42 = L[8];
try {
if(R.isActivationRecord($self40)) {
$step38 = $self40.step;
$al42 = $self40.from;
$ans41 = $self40.ans;
self40 = $self40.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[8],1,$t);
}
var self40 = $self40;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step38) {
case 0: var anf_method_obj43 = G(self40,"reason",L[5]);
var anf_arg44 = G(self40,"loc",L[6]);
$step38 = 1;
$al42 = L[7];
$field45 = R.getColonFieldLoc(anf_method_obj43,"render-reason",L[7]);
if(R.isMethod($field45)) {
$ans41 = $field45.full_meth(anf_method_obj43,anf_arg44,(false));
} else {
if(!(R.isFunction($field45))) {
R.ffi.throwNonFunApp(L[7],$field45);
}
$ans41 = $field45.app(anf_arg44,(false));
}
break;
case 1: ++R.GAS;
return $ans41;
default: throw "No case numbered " + $step38 + " in $temp_full39";
}
}
} catch($e46) {
if(R.isCont($e46) && ($step38 !== 1)) {
$e46.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al42,$temp_full39,$step38,[self40],[]);
}
if(R.isPyretException($e46)) {
$e46.pyretStack.push($al42);
}
throw $e46;
}
};
var anf_variant_member67 = R.makeMethod0($temp_full39);
var $temp_full48 = function($self49) {
var $step47 = 0;
var $ans50 = D;
var $al51 = L[12];
try {
if(R.isActivationRecord($self49)) {
$step47 = $self49.step;
$al51 = $self49.from;
$ans50 = $self49.ans;
self49 = $self49.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[12],1,$t);
}
var self49 = $self49;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step47) {
case 0: var anf_method_obj52 = G(self49,"reason",L[9]);
var anf_arg53 = G(self49,"loc",L[10]);
$step47 = 1;
$al51 = L[11];
$field54 = R.getColonFieldLoc(anf_method_obj52,"render-reason",L[11]);
if(R.isMethod($field54)) {
$ans50 = $field54.full_meth(anf_method_obj52,anf_arg53,(true));
} else {
if(!(R.isFunction($field54))) {
R.ffi.throwNonFunApp(L[11],$field54);
}
$ans50 = $field54.app(anf_arg53,(true));
}
break;
case 1: ++R.GAS;
return $ans50;
default: throw "No case numbered " + $step47 + " in $temp_full48";
}
}
} catch($e55) {
if(R.isCont($e55) && ($step47 !== 1)) {
$e55.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al51,$temp_full48,$step47,[self49],[]);
}
if(R.isPyretException($e55)) {
$e55.pyretStack.push($al51);
}
throw $e55;
}
};
var anf_variant_member79 = R.makeMethod0($temp_full48);
var $ok_getfields62 = function(f) {
return f();
};
var $ok_getfieldsref60 = function(f) {
return f();
};
var $ok_mutablemask61 = [];
var $ok$base56 = {"render-reason":anf_singleton_variant_member59,
"_match":R.makeMatch("ok",0)};
var $ok$brands58 = {"$brand$ok":true};
$ok$brands58[ContractResult17._brand] = true;
var $fail_getfields70 = function(f) {
return f(this.dict["loc"],this.dict["reason"]);
};
var $fail_getfieldsref68 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["reason"],false,refmask[1]));
};
var $fail_mutablemask69 = [false,false];
var $fail$base64 = {"$fieldNames":["loc","reason"],
"render-reason":anf_variant_member67,
"_match":R.makeMatch("fail",2)};
var $fail$brands66 = {"$brand$fail":true};
$fail$brands66[ContractResult17._brand] = true;
var fail72 = R.makeVariantConstructor(L[16],function() {
return [FailureReason73];
},["reason74"],[L[15]],[false,false],["loc75","reason74"],$fail_mutablemask69,$fail$base64,$fail$brands66,"fail",$fail_getfieldsref68,$fail_getfields70,$fail$base64);
var $fail$arg_getfields82 = function(f) {
return f(this.dict["loc"],this.dict["reason"]);
};
var $fail$arg_getfieldsref80 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["reason"],false,refmask[1]));
};
var $fail$arg_mutablemask81 = [false,false];
var $fail$arg$base76 = {"$fieldNames":["loc","reason"],
"render-reason":anf_variant_member79,
"_match":R.makeMatch("fail-arg",2)};
var $fail$arg$brands78 = {"$brand$fail$arg":true};
$fail$arg$brands78[ContractResult17._brand] = true;
var fail$arg84 = R.makeVariantConstructor(L[19],function() {
return [FailureReason73];
},["reason85"],[L[18]],[false,false],["loc86","reason85"],$fail$arg_mutablemask81,$fail$arg$base76,$fail$arg$brands78,"fail-arg",$fail$arg_getfieldsref80,$fail$arg_getfields82,$fail$arg$base76);
var anf_assign88 = R.makeObject({"ContractResult":R.makeFunction(function($val87) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[20],1,$t);
}
return R.makeBoolean(R.hasBrand($val87,ContractResult17._brand));
}),
"is-ok":R.makeFunction(function($val63) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[13],1,$t);
}
return R.makeBoolean(R.hasBrand($val63,"$brand$ok"));
}),
"ok":R.makeDataValue($ok$base56,$ok$brands58,"ok",$ok_getfieldsref60,$ok_getfields62,-1,$ok_mutablemask61,$ok$base56),
"is-fail":R.makeFunction(function($val71) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[14],1,$t);
}
return R.makeBoolean(R.hasBrand($val71,"$brand$fail"));
}),
"fail":fail72,
"is-fail-arg":R.makeFunction(function($val83) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[17],1,$t);
}
return R.makeBoolean(R.hasBrand($val83,"$brand$fail$arg"));
}),
"fail-arg":fail$arg84});
ContractResult89.$var = anf_assign88;
var anf_assign90 = G(ContractResult89.$var,"ContractResult",L[20]);
ContractResult91.$var = anf_assign90;
var anf_assign92 = G(ContractResult89.$var,"ContractResult",L[20]);
is$ContractResult93.$var = anf_assign92;
var anf_assign94 = G(ContractResult89.$var,"is-ok",L[13]);
is$ok95.$var = anf_assign94;
var anf_assign96 = G(ContractResult89.$var,"ok",L[13]);
ok97.$var = anf_assign96;
var anf_assign98 = G(ContractResult89.$var,"is-fail",L[14]);
is$fail99.$var = anf_assign98;
var anf_assign100 = G(ContractResult89.$var,"fail",L[14]);
fail101.$var = anf_assign100;
var anf_assign102 = G(ContractResult89.$var,"is-fail-arg",L[17]);
is$fail$arg103.$var = anf_assign102;
var anf_assign104 = G(ContractResult89.$var,"fail-arg",L[17]);
fail$arg105.$var = anf_assign104;
var $temp_full107 = function($self108,$loc109,$from$fail$arg110) {
var $step106 = 0;
var $ans111 = D;
var $al112 = L[33];
try {
if(R.isActivationRecord($self108)) {
$step106 = $self108.step;
$al112 = $self108.from;
$ans111 = $self108.ans;
self108 = $self108.args[0];
loc109 = $self108.args[1];
from$fail$arg110 = $self108.args[2];
anf_array_val131 = $self108.vars[0];
anf_array_val124 = $self108.vars[1];
anf_array_val123 = $self108.vars[2];
anf_array_val122 = $self108.vars[3];
anf_array_val121 = $self108.vars[4];
anf_method_obj126 = $self108.vars[5];
anf_method_obj133 = $self108.vars[6];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[33],3,$t);
}
var self108 = $self108;
var loc109 = $loc109;
var from$fail$arg110 = $from$fail$arg110;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step106) {
case 0: var anf_method_obj133 = G(ED5,"error",L[21]);
var anf_method_obj126 = G(ED5,"para-nospace",L[22]);
$step106 = 1;
$al112 = L[23];
$field113 = R.getColonFieldLoc(ED5,"text",L[23]);
if(R.isMethod($field113)) {
$ans111 = $field113.full_meth(ED5,("At "));
} else {
if(!(R.isFunction($field113))) {
R.ffi.throwNonFunApp(L[23],$field113);
}
$ans111 = $field113.app(("At "));
}
break;
case 1: var anf_array_val121 = $ans111;
var anf_arg114 = G(self108,"loc",L[24]);
$step106 = 2;
$al112 = L[32];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al112,draw$and$highlight30.$var);
}
$ans111 = draw$and$highlight30.$var.app(anf_arg114);
break;
case 2: var anf_array_val122 = $ans111;
$step106 = 3;
$al112 = L[25];
$field115 = R.getColonFieldLoc(ED5,"text",L[25]);
if(R.isMethod($field115)) {
$ans111 = $field115.full_meth(ED5,(", field "));
} else {
if(!(R.isFunction($field115))) {
R.ffi.throwNonFunApp(L[25],$field115);
}
$ans111 = $field115.app((", field "));
}
break;
case 3: var anf_array_val123 = $ans111;
var anf_arg116 = G(self108,"field",L[26]);
$step106 = 4;
$al112 = L[27];
$field117 = R.getColonFieldLoc(ED5,"text",L[27]);
if(R.isMethod($field117)) {
$ans111 = $field117.full_meth(ED5,anf_arg116);
} else {
if(!(R.isFunction($field117))) {
R.ffi.throwNonFunApp(L[27],$field117);
}
$ans111 = $field117.app(anf_arg116);
}
break;
case 4: var anf_arg118 = $ans111;
$step106 = 5;
$al112 = L[28];
$field119 = R.getColonFieldLoc(ED5,"code",L[28]);
if(R.isMethod($field119)) {
$ans111 = $field119.full_meth(ED5,anf_arg118);
} else {
if(!(R.isFunction($field119))) {
R.ffi.throwNonFunApp(L[28],$field119);
}
$ans111 = $field119.app(anf_arg118);
}
break;
case 5: var anf_array_val124 = $ans111;
$step106 = 6;
$al112 = L[29];
$field120 = R.getColonFieldLoc(ED5,"text",L[29]);
if(R.isMethod($field120)) {
$ans111 = $field120.full_meth(ED5,(" failed because"));
} else {
if(!(R.isFunction($field120))) {
R.ffi.throwNonFunApp(L[29],$field120);
}
$ans111 = $field120.app((" failed because"));
}
break;
case 6: var anf_array_val125 = $ans111;
var anf_arg127 = [anf_array_val121,anf_array_val122,anf_array_val123,anf_array_val124,anf_array_val125];
$step106 = 7;
$al112 = L[22];
$field128 = R.getColonFieldLoc(anf_method_obj126,"make",L[22]);
if(R.isMethod($field128)) {
$ans111 = $field128.full_meth(anf_method_obj126,anf_arg127);
} else {
if(!(R.isFunction($field128))) {
R.ffi.throwNonFunApp(L[22],$field128);
}
$ans111 = $field128.app(anf_arg127);
}
break;
case 7: var anf_array_val131 = $ans111;
var anf_method_obj129 = G(self108,"reason",L[30]);
$step106 = 8;
$al112 = L[31];
$field130 = R.getColonFieldLoc(anf_method_obj129,"render-reason",L[31]);
if(R.isMethod($field130)) {
$ans111 = $field130.full_meth(anf_method_obj129,loc109,from$fail$arg110);
} else {
if(!(R.isFunction($field130))) {
R.ffi.throwNonFunApp(L[31],$field130);
}
$ans111 = $field130.app(loc109,from$fail$arg110);
}
break;
case 8: var anf_array_val132 = $ans111;
var anf_arg134 = [anf_array_val131,anf_array_val132];
$step106 = 9;
$al112 = L[21];
$field135 = R.getColonFieldLoc(anf_method_obj133,"make",L[21]);
if(R.isMethod($field135)) {
$ans111 = $field135.full_meth(anf_method_obj133,anf_arg134);
} else {
if(!(R.isFunction($field135))) {
R.ffi.throwNonFunApp(L[21],$field135);
}
$ans111 = $field135.app(anf_arg134);
}
break;
case 9: ++R.GAS;
return $ans111;
default: throw "No case numbered " + $step106 + " in $temp_full107";
}
}
} catch($e136) {
if(R.isCont($e136) && ($step106 !== 9)) {
$e136.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al112,$temp_full107,$step106,[self108,loc109,from$fail$arg110],[anf_array_val131,anf_array_val124,anf_array_val123,anf_array_val122,anf_array_val121,anf_method_obj126,anf_method_obj133]);
}
if(R.isPyretException($e136)) {
$e136.pyretStack.push($al112);
}
throw $e136;
}
};
var anf_variant_member166 = R.makeMethod2($temp_full107);
var $temp_full138 = function($self139,$loc140,$from$fail$arg141) {
var $step137 = 0;
var $ans142 = D;
var $al143 = L[43];
try {
if(R.isActivationRecord($self139)) {
$step137 = $self139.step;
$al143 = $self139.from;
$ans142 = $self139.ans;
self139 = $self139.args[0];
loc140 = $self139.args[1];
from$fail$arg141 = $self139.args[2];
anf_array_val153 = $self139.vars[0];
anf_array_val152 = $self139.vars[1];
anf_array_val151 = $self139.vars[2];
anf_method_obj155 = $self139.vars[3];
anf_method_obj159 = $self139.vars[4];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[43],3,$t);
}
var self139 = $self139;
var loc140 = $loc140;
var from$fail$arg141 = $from$fail$arg141;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step137) {
case 0: var anf_method_obj159 = G(ED5,"error",L[34]);
var anf_method_obj155 = G(ED5,"para",L[35]);
$step137 = 1;
$al143 = L[36];
$field144 = R.getColonFieldLoc(ED5,"text",L[36]);
if(R.isMethod($field144)) {
$ans142 = $field144.full_meth(ED5,("Missing field"));
} else {
if(!(R.isFunction($field144))) {
R.ffi.throwNonFunApp(L[36],$field144);
}
$ans142 = $field144.app(("Missing field"));
}
break;
case 1: var anf_array_val151 = $ans142;
var anf_arg145 = G(self139,"field",L[37]);
$step137 = 2;
$al143 = L[38];
$field146 = R.getColonFieldLoc(ED5,"text",L[38]);
if(R.isMethod($field146)) {
$ans142 = $field146.full_meth(ED5,anf_arg145);
} else {
if(!(R.isFunction($field146))) {
R.ffi.throwNonFunApp(L[38],$field146);
}
$ans142 = $field146.app(anf_arg145);
}
break;
case 2: var anf_arg147 = $ans142;
$step137 = 3;
$al143 = L[39];
$field148 = R.getColonFieldLoc(ED5,"code",L[39]);
if(R.isMethod($field148)) {
$ans142 = $field148.full_meth(ED5,anf_arg147);
} else {
if(!(R.isFunction($field148))) {
R.ffi.throwNonFunApp(L[39],$field148);
}
$ans142 = $field148.app(anf_arg147);
}
break;
case 3: var anf_array_val152 = $ans142;
$step137 = 4;
$al143 = L[40];
$field149 = R.getColonFieldLoc(ED5,"text",L[40]);
if(R.isMethod($field149)) {
$ans142 = $field149.full_meth(ED5,("is required at"));
} else {
if(!(R.isFunction($field149))) {
R.ffi.throwNonFunApp(L[40],$field149);
}
$ans142 = $field149.app(("is required at"));
}
break;
case 4: var anf_array_val153 = $ans142;
var anf_arg150 = G(self139,"loc",L[41]);
$step137 = 5;
$al143 = L[42];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al143,draw$and$highlight30.$var);
}
$ans142 = draw$and$highlight30.$var.app(anf_arg150);
break;
case 5: var anf_array_val154 = $ans142;
var anf_arg156 = [anf_array_val151,anf_array_val152,anf_array_val153,anf_array_val154];
$step137 = 6;
$al143 = L[35];
$field157 = R.getColonFieldLoc(anf_method_obj155,"make",L[35]);
if(R.isMethod($field157)) {
$ans142 = $field157.full_meth(anf_method_obj155,anf_arg156);
} else {
if(!(R.isFunction($field157))) {
R.ffi.throwNonFunApp(L[35],$field157);
}
$ans142 = $field157.app(anf_arg156);
}
break;
case 6: var anf_array_val158 = $ans142;
var anf_arg160 = [anf_array_val158];
$step137 = 7;
$al143 = L[34];
$field161 = R.getColonFieldLoc(anf_method_obj159,"make",L[34]);
if(R.isMethod($field161)) {
$ans142 = $field161.full_meth(anf_method_obj159,anf_arg160);
} else {
if(!(R.isFunction($field161))) {
R.ffi.throwNonFunApp(L[34],$field161);
}
$ans142 = $field161.app(anf_arg160);
}
break;
case 7: ++R.GAS;
return $ans142;
default: throw "No case numbered " + $step137 + " in $temp_full138";
}
}
} catch($e162) {
if(R.isCont($e162) && ($step137 !== 7)) {
$e162.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al143,$temp_full138,$step137,[self139,loc140,from$fail$arg141],[anf_array_val153,anf_array_val152,anf_array_val151,anf_method_obj155,anf_method_obj159]);
}
if(R.isPyretException($e162)) {
$e162.pyretStack.push($al143);
}
throw $e162;
}
};
var anf_variant_member178 = R.makeMethod2($temp_full138);
var $field$failure_getfields169 = function(f) {
return f(this.dict["loc"],this.dict["field"],this.dict["reason"]);
};
var $field$failure_getfieldsref167 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["field"],false,refmask[1]),R.derefField(this.dict["reason"],false,refmask[2]));
};
var $field$failure_mutablemask168 = [false,false,false];
var $field$failure$base163 = {"$fieldNames":["loc","field","reason"],
"render-reason":anf_variant_member166,
"_match":R.makeMatch("field-failure",3)};
var $field$failure$brands165 = {"$brand$field$failure":true};
$field$failure$brands165[FieldFailure18._brand] = true;
var field$failure171 = R.makeVariantConstructor(L[45],function() {
return [];
},[],[],[false,false,false],["loc172","field173","reason174"],$field$failure_mutablemask168,$field$failure$base163,$field$failure$brands165,"field-failure",$field$failure_getfieldsref167,$field$failure_getfields169,$field$failure$base163);
var $missing$field_getfields181 = function(f) {
return f(this.dict["loc"],this.dict["field"]);
};
var $missing$field_getfieldsref179 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["field"],false,refmask[1]));
};
var $missing$field_mutablemask180 = [false,false];
var $missing$field$base175 = {"$fieldNames":["loc","field"],
"render-reason":anf_variant_member178,
"_match":R.makeMatch("missing-field",2)};
var $missing$field$brands177 = {"$brand$missing$field":true};
$missing$field$brands177[FieldFailure18._brand] = true;
var missing$field183 = R.makeVariantConstructor(L[47],function() {
return [];
},[],[],[false,false],["loc184","field185"],$missing$field_mutablemask180,$missing$field$base175,$missing$field$brands177,"missing-field",$missing$field_getfieldsref179,$missing$field_getfields181,$missing$field$base175);
var anf_assign187 = R.makeObject({"FieldFailure":R.makeFunction(function($val186) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[48],1,$t);
}
return R.makeBoolean(R.hasBrand($val186,FieldFailure18._brand));
}),
"is-field-failure":R.makeFunction(function($val170) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[44],1,$t);
}
return R.makeBoolean(R.hasBrand($val170,"$brand$field$failure"));
}),
"field-failure":field$failure171,
"is-missing-field":R.makeFunction(function($val182) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[46],1,$t);
}
return R.makeBoolean(R.hasBrand($val182,"$brand$missing$field"));
}),
"missing-field":missing$field183});
FieldFailure188.$var = anf_assign187;
var anf_assign189 = G(FieldFailure188.$var,"FieldFailure",L[48]);
FieldFailure190.$var = anf_assign189;
var anf_assign191 = G(FieldFailure188.$var,"FieldFailure",L[48]);
is$FieldFailure192.$var = anf_assign191;
var anf_assign193 = G(FieldFailure188.$var,"is-field-failure",L[44]);
is$field$failure194.$var = anf_assign193;
var anf_assign195 = G(FieldFailure188.$var,"field-failure",L[44]);
field$failure196.$var = anf_assign195;
var anf_assign197 = G(FieldFailure188.$var,"is-missing-field",L[46]);
is$missing$field198.$var = anf_assign197;
var anf_assign199 = G(FieldFailure188.$var,"missing-field",L[46]);
missing$field200.$var = anf_assign199;
var $temp_full202 = function($self203,$loc204,$from$fail$arg205) {
var $step201 = 0;
var $ans206 = D;
var $al207 = L[63];
try {
if(R.isActivationRecord($self203)) {
$step201 = $self203.step;
$al207 = $self203.from;
$ans206 = $self203.ans;
self203 = $self203.args[0];
loc204 = $self203.args[1];
from$fail$arg205 = $self203.args[2];
anf_array_val236 = $self203.vars[0];
anf_method_obj231 = $self203.vars[1];
anf_method_obj238 = $self203.vars[2];
anf_arg241 = $self203.vars[3];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[63],3,$t);
}
var self203 = $self203;
var loc204 = $loc204;
var from$fail$arg205 = $from$fail$arg205;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step201) {
case 0: var $temp_lam209 = function($user$loc210) {
var $step208 = 0;
var $ans211 = D;
var $al212 = L[56];
try {
if(R.isActivationRecord($user$loc210)) {
$step208 = $user$loc210.step;
$al212 = $user$loc210.from;
$ans211 = $user$loc210.ans;
user$loc210 = $user$loc210.args[0];
anf_array_val223 = $user$loc210.vars[0];
anf_array_val216 = $user$loc210.vars[1];
anf_array_val215 = $user$loc210.vars[2];
anf_method_obj218 = $user$loc210.vars[3];
anf_method_obj225 = $user$loc210.vars[4];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[56],1,$t);
}
var user$loc210 = $user$loc210;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step208) {
case 0: var anf_method_obj225 = G(ED5,"error",L[49]);
var anf_method_obj218 = G(ED5,"para",L[50]);
$step208 = 1;
$al212 = L[51];
$field213 = R.getColonFieldLoc(ED5,"text",L[51]);
if(R.isMethod($field213)) {
$ans211 = $field213.full_meth(ED5,("Failed while initializing a graph at"));
} else {
if(!(R.isFunction($field213))) {
R.ffi.throwNonFunApp(L[51],$field213);
}
$ans211 = $field213.app(("Failed while initializing a graph at"));
}
break;
case 1: var anf_array_val215 = $ans211;
$step208 = 2;
$al212 = L[55];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al212,draw$and$highlight30.$var);
}
$ans211 = draw$and$highlight30.$var.app(user$loc210);
break;
case 2: var anf_array_val216 = $ans211;
$step208 = 3;
$al212 = L[52];
$field214 = R.getColonFieldLoc(ED5,"text",L[52]);
if(R.isMethod($field214)) {
$ans211 = $field214.full_meth(ED5,("because:"));
} else {
if(!(R.isFunction($field214))) {
R.ffi.throwNonFunApp(L[52],$field214);
}
$ans211 = $field214.app(("because:"));
}
break;
case 3: var anf_array_val217 = $ans211;
var anf_arg219 = [anf_array_val215,anf_array_val216,anf_array_val217];
$step208 = 4;
$al212 = L[50];
$field220 = R.getColonFieldLoc(anf_method_obj218,"make",L[50]);
if(R.isMethod($field220)) {
$ans211 = $field220.full_meth(anf_method_obj218,anf_arg219);
} else {
if(!(R.isFunction($field220))) {
R.ffi.throwNonFunApp(L[50],$field220);
}
$ans211 = $field220.app(anf_arg219);
}
break;
case 4: var anf_array_val223 = $ans211;
var anf_method_obj221 = G(self203,"reason",L[53]);
$step208 = 5;
$al212 = L[54];
$field222 = R.getColonFieldLoc(anf_method_obj221,"render-reason",L[54]);
if(R.isMethod($field222)) {
$ans211 = $field222.full_meth(anf_method_obj221,loc204,(false));
} else {
if(!(R.isFunction($field222))) {
R.ffi.throwNonFunApp(L[54],$field222);
}
$ans211 = $field222.app(loc204,(false));
}
break;
case 5: var anf_array_val224 = $ans211;
var anf_arg226 = [anf_array_val223,anf_array_val224];
$step208 = 6;
$al212 = L[49];
$field227 = R.getColonFieldLoc(anf_method_obj225,"make",L[49]);
if(R.isMethod($field227)) {
$ans211 = $field227.full_meth(anf_method_obj225,anf_arg226);
} else {
if(!(R.isFunction($field227))) {
R.ffi.throwNonFunApp(L[49],$field227);
}
$ans211 = $field227.app(anf_arg226);
}
break;
case 6: ++R.GAS;
return $ans211;
default: throw "No case numbered " + $step208 + " in $temp_lam209";
}
}
} catch($e228) {
if(R.isCont($e228) && ($step208 !== 6)) {
$e228.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al212,$temp_lam209,$step208,[user$loc210],[anf_array_val223,anf_array_val216,anf_array_val215,anf_method_obj218,anf_method_obj225]);
}
if(R.isPyretException($e228)) {
$e228.pyretStack.push($al212);
}
throw $e228;
}
};
var anf_arg241 = R.makeFunction($temp_lam209);
var anf_method_obj238 = G(ED5,"error",L[57]);
var anf_method_obj231 = G(ED5,"para",L[58]);
$step201 = 1;
$al207 = L[59];
$field229 = R.getColonFieldLoc(ED5,"text",L[59]);
if(R.isMethod($field229)) {
$ans206 = $field229.full_meth(ED5,("Failed while initializing a graph, because:"));
} else {
if(!(R.isFunction($field229))) {
R.ffi.throwNonFunApp(L[59],$field229);
}
$ans206 = $field229.app(("Failed while initializing a graph, because:"));
}
break;
case 1: var anf_array_val230 = $ans206;
var anf_arg232 = [anf_array_val230];
$step201 = 2;
$al207 = L[58];
$field233 = R.getColonFieldLoc(anf_method_obj231,"make",L[58]);
if(R.isMethod($field233)) {
$ans206 = $field233.full_meth(anf_method_obj231,anf_arg232);
} else {
if(!(R.isFunction($field233))) {
R.ffi.throwNonFunApp(L[58],$field233);
}
$ans206 = $field233.app(anf_arg232);
}
break;
case 2: var anf_array_val236 = $ans206;
var anf_method_obj234 = G(self203,"reason",L[60]);
$step201 = 3;
$al207 = L[61];
$field235 = R.getColonFieldLoc(anf_method_obj234,"render-reason",L[61]);
if(R.isMethod($field235)) {
$ans206 = $field235.full_meth(anf_method_obj234,loc204,(false));
} else {
if(!(R.isFunction($field235))) {
R.ffi.throwNonFunApp(L[61],$field235);
}
$ans206 = $field235.app(loc204,(false));
}
break;
case 3: var anf_array_val237 = $ans206;
var anf_arg239 = [anf_array_val236,anf_array_val237];
$step201 = 4;
$al207 = L[57];
$field240 = R.getColonFieldLoc(anf_method_obj238,"make",L[57]);
if(R.isMethod($field240)) {
$ans206 = $field240.full_meth(anf_method_obj238,anf_arg239);
} else {
if(!(R.isFunction($field240))) {
R.ffi.throwNonFunApp(L[57],$field240);
}
$ans206 = $field240.app(anf_arg239);
}
break;
case 4: var anf_arg242 = $ans206;
$step201 = 5;
$al207 = L[62];
$field243 = R.getColonFieldLoc(ED5,"maybe-stack-loc",L[62]);
if(R.isMethod($field243)) {
$ans206 = $field243.full_meth(ED5,(0),(true),anf_arg241,anf_arg242);
} else {
if(!(R.isFunction($field243))) {
R.ffi.throwNonFunApp(L[62],$field243);
}
$ans206 = $field243.app((0),(true),anf_arg241,anf_arg242);
}
break;
case 5: ++R.GAS;
return $ans206;
default: throw "No case numbered " + $step201 + " in $temp_full202";
}
}
} catch($e244) {
if(R.isCont($e244) && ($step201 !== 5)) {
$e244.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al207,$temp_full202,$step201,[self203,loc204,from$fail$arg205],[anf_array_val236,anf_method_obj231,anf_method_obj238,anf_arg241]);
}
if(R.isPyretException($e244)) {
$e244.pyretStack.push($al207);
}
throw $e244;
}
};
var anf_variant_member441 = R.makeMethod2($temp_full202);
var $temp_full246 = function($self247,$loc248,$from$fail$arg249) {
var $step245 = 0;
var $ans250 = D;
var $al251 = L[87];
try {
if(R.isActivationRecord($self247)) {
$step245 = $self247.step;
$al251 = $self247.from;
$ans250 = $self247.ans;
self247 = $self247.args[0];
loc248 = $self247.args[1];
from$fail$arg249 = $self247.args[2];
anf_method_obj299 = $self247.vars[0];
anf_method_obj290 = $self247.vars[1];
anf_arg293 = $self247.vars[2];
message280 = $self247.vars[3];
anf_array_val262 = $self247.vars[4];
anf_array_val261 = $self247.vars[5];
anf_array_val260 = $self247.vars[6];
anf_array_val259 = $self247.vars[7];
anf_method_obj264 = $self247.vars[8];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[87],3,$t);
}
var self247 = $self247;
var loc248 = $loc248;
var from$fail$arg249 = $from$fail$arg249;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step245) {
case 0: var anf_method_obj264 = G(ED5,"para",L[64]);
$step245 = 1;
$al251 = L[65];
$field252 = R.getColonFieldLoc(ED5,"text",L[65]);
if(R.isMethod($field252)) {
$ans250 = $field252.full_meth(ED5,("Expected to get"));
} else {
if(!(R.isFunction($field252))) {
R.ffi.throwNonFunApp(L[65],$field252);
}
$ans250 = $field252.app(("Expected to get"));
}
break;
case 1: var anf_array_val259 = $ans250;
var anf_arg253 = G(self247,"name",L[66]);
$step245 = 2;
$al251 = L[67];
$field254 = R.getColonFieldLoc(ED5,"text",L[67]);
if(R.isMethod($field254)) {
$ans250 = $field254.full_meth(ED5,anf_arg253);
} else {
if(!(R.isFunction($field254))) {
R.ffi.throwNonFunApp(L[67],$field254);
}
$ans250 = $field254.app(anf_arg253);
}
break;
case 2: var anf_arg255 = $ans250;
$step245 = 3;
$al251 = L[68];
$field256 = R.getColonFieldLoc(ED5,"code",L[68]);
if(R.isMethod($field256)) {
$ans250 = $field256.full_meth(ED5,anf_arg255);
} else {
if(!(R.isFunction($field256))) {
R.ffi.throwNonFunApp(L[68],$field256);
}
$ans250 = $field256.app(anf_arg255);
}
break;
case 3: var anf_array_val260 = $ans250;
$step245 = 4;
$al251 = L[69];
$field257 = R.getColonFieldLoc(ED5,"text",L[69]);
if(R.isMethod($field257)) {
$ans250 = $field257.full_meth(ED5,("because of the annotation at"));
} else {
if(!(R.isFunction($field257))) {
R.ffi.throwNonFunApp(L[69],$field257);
}
$ans250 = $field257.app(("because of the annotation at"));
}
break;
case 4: var anf_array_val261 = $ans250;
$step245 = 5;
$al251 = L[86];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al251,draw$and$highlight30.$var);
}
$ans250 = draw$and$highlight30.$var.app(loc248);
break;
case 5: var anf_array_val262 = $ans250;
$step245 = 6;
$al251 = L[70];
$field258 = R.getColonFieldLoc(ED5,"text",L[70]);
if(R.isMethod($field258)) {
$ans250 = $field258.full_meth(ED5,("but got:"));
} else {
if(!(R.isFunction($field258))) {
R.ffi.throwNonFunApp(L[70],$field258);
}
$ans250 = $field258.app(("but got:"));
}
break;
case 6: var anf_array_val263 = $ans250;
var anf_arg265 = [anf_array_val259,anf_array_val260,anf_array_val261,anf_array_val262,anf_array_val263];
$step245 = 7;
$al251 = L[64];
$field266 = R.getColonFieldLoc(anf_method_obj264,"make",L[64]);
if(R.isMethod($field266)) {
$ans250 = $field266.full_meth(anf_method_obj264,anf_arg265);
} else {
if(!(R.isFunction($field266))) {
R.ffi.throwNonFunApp(L[64],$field266);
}
$ans250 = $field266.app(anf_arg265);
}
break;
case 7: var message280 = $ans250;
$al251 = L[71];
var anf_if302 = R.checkWrapBoolean(from$fail$arg249);
if(R.isPyretTrue(anf_if302)) {
$step245 = 8;
} else {
$step245 = 11;
}
break;
case 8: var $temp_lam268 = function($l269) {
var $step267 = 0;
var $ans270 = D;
var $al271 = L[78];
try {
if(R.isActivationRecord($l269)) {
$step267 = $l269.step;
$al271 = $l269.from;
$ans270 = $l269.ans;
l269 = $l269.args[0];
anf_array_val275 = $l269.vars[0];
anf_method_obj277 = $l269.vars[1];
anf_array_val281 = $l269.vars[2];
anf_method_obj283 = $l269.vars[3];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[78],1,$t);
}
var l269 = $l269;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step267) {
case 0: var anf_method_obj283 = G(ED5,"error",L[72]);
var anf_arg272 = G(self247,"val",L[73]);
$step267 = 1;
$al271 = L[74];
$field273 = R.getColonFieldLoc(ED5,"embed",L[74]);
if(R.isMethod($field273)) {
$ans270 = $field273.full_meth(ED5,anf_arg272);
} else {
if(!(R.isFunction($field273))) {
R.ffi.throwNonFunApp(L[74],$field273);
}
$ans270 = $field273.app(anf_arg272);
}
break;
case 1: var anf_array_val281 = $ans270;
var anf_method_obj277 = G(ED5,"para",L[75]);
$step267 = 2;
$al271 = L[76];
$field274 = R.getColonFieldLoc(ED5,"text",L[76]);
if(R.isMethod($field274)) {
$ans270 = $field274.full_meth(ED5,("called from around"));
} else {
if(!(R.isFunction($field274))) {
R.ffi.throwNonFunApp(L[76],$field274);
}
$ans270 = $field274.app(("called from around"));
}
break;
case 2: var anf_array_val275 = $ans270;
$step267 = 3;
$al271 = L[77];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al271,draw$and$highlight30.$var);
}
$ans270 = draw$and$highlight30.$var.app(l269);
break;
case 3: var anf_array_val276 = $ans270;
var anf_arg278 = [anf_array_val275,anf_array_val276];
$step267 = 4;
$al271 = L[75];
$field279 = R.getColonFieldLoc(anf_method_obj277,"make",L[75]);
if(R.isMethod($field279)) {
$ans270 = $field279.full_meth(anf_method_obj277,anf_arg278);
} else {
if(!(R.isFunction($field279))) {
R.ffi.throwNonFunApp(L[75],$field279);
}
$ans270 = $field279.app(anf_arg278);
}
break;
case 4: var anf_array_val282 = $ans270;
var anf_arg284 = [message280,anf_array_val281,anf_array_val282];
$step267 = 5;
$al271 = L[72];
$field285 = R.getColonFieldLoc(anf_method_obj283,"make",L[72]);
if(R.isMethod($field285)) {
$ans270 = $field285.full_meth(anf_method_obj283,anf_arg284);
} else {
if(!(R.isFunction($field285))) {
R.ffi.throwNonFunApp(L[72],$field285);
}
$ans270 = $field285.app(anf_arg284);
}
break;
case 5: ++R.GAS;
return $ans270;
default: throw "No case numbered " + $step267 + " in $temp_lam268";
}
}
} catch($e286) {
if(R.isCont($e286) && ($step267 !== 5)) {
$e286.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al271,$temp_lam268,$step267,[l269],[anf_array_val275,anf_method_obj277,anf_array_val281,anf_method_obj283]);
}
if(R.isPyretException($e286)) {
$e286.pyretStack.push($al271);
}
throw $e286;
}
};
var anf_arg293 = R.makeFunction($temp_lam268);
var anf_method_obj290 = G(ED5,"error",L[79]);
var anf_arg287 = G(self247,"val",L[80]);
$step245 = 9;
$al251 = L[81];
$field288 = R.getColonFieldLoc(ED5,"embed",L[81]);
if(R.isMethod($field288)) {
$ans250 = $field288.full_meth(ED5,anf_arg287);
} else {
if(!(R.isFunction($field288))) {
R.ffi.throwNonFunApp(L[81],$field288);
}
$ans250 = $field288.app(anf_arg287);
}
break;
case 9: var anf_array_val289 = $ans250;
var anf_arg291 = [message280,anf_array_val289];
$step245 = 10;
$al251 = L[79];
$field292 = R.getColonFieldLoc(anf_method_obj290,"make",L[79]);
if(R.isMethod($field292)) {
$ans250 = $field292.full_meth(anf_method_obj290,anf_arg291);
} else {
if(!(R.isFunction($field292))) {
R.ffi.throwNonFunApp(L[79],$field292);
}
$ans250 = $field292.app(anf_arg291);
}
break;
case 10: var anf_arg294 = $ans250;
$step245 = 13;
$al251 = L[82];
$field295 = R.getColonFieldLoc(ED5,"maybe-stack-loc",L[82]);
if(R.isMethod($field295)) {
$ans250 = $field295.full_meth(ED5,(0),(true),anf_arg293,anf_arg294);
} else {
if(!(R.isFunction($field295))) {
R.ffi.throwNonFunApp(L[82],$field295);
}
$ans250 = $field295.app((0),(true),anf_arg293,anf_arg294);
}
break;
case 11: var anf_method_obj299 = G(ED5,"error",L[83]);
var anf_arg296 = G(self247,"val",L[84]);
$step245 = 12;
$al251 = L[85];
$field297 = R.getColonFieldLoc(ED5,"embed",L[85]);
if(R.isMethod($field297)) {
$ans250 = $field297.full_meth(ED5,anf_arg296);
} else {
if(!(R.isFunction($field297))) {
R.ffi.throwNonFunApp(L[85],$field297);
}
$ans250 = $field297.app(anf_arg296);
}
break;
case 12: var anf_array_val298 = $ans250;
var anf_arg300 = [message280,anf_array_val298];
$step245 = 13;
$al251 = L[83];
$field301 = R.getColonFieldLoc(anf_method_obj299,"make",L[83]);
if(R.isMethod($field301)) {
$ans250 = $field301.full_meth(anf_method_obj299,anf_arg300);
} else {
if(!(R.isFunction($field301))) {
R.ffi.throwNonFunApp(L[83],$field301);
}
$ans250 = $field301.app(anf_arg300);
}
break;
case 13: ++R.GAS;
return $ans250;
default: throw "No case numbered " + $step245 + " in $temp_full246";
}
}
} catch($e303) {
if(R.isCont($e303) && ($step245 !== 13)) {
$e303.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al251,$temp_full246,$step245,[self247,loc248,from$fail$arg249],[anf_method_obj299,anf_method_obj290,anf_arg293,message280,anf_array_val262,anf_array_val261,anf_array_val260,anf_array_val259,anf_method_obj264]);
}
if(R.isPyretException($e303)) {
$e303.pyretStack.push($al251);
}
throw $e303;
}
};
var anf_variant_member452 = R.makeMethod2($temp_full246);
var $temp_full305 = function($self306,$loc307,$from$fail$arg308) {
var $step304 = 0;
var $ans309 = D;
var $al310 = L[111];
try {
if(R.isActivationRecord($self306)) {
$step304 = $self306.step;
$al310 = $self306.from;
$ans309 = $self306.ans;
self306 = $self306.args[0];
loc307 = $self306.args[1];
from$fail$arg308 = $self306.args[2];
anf_method_obj358 = $self306.vars[0];
anf_method_obj349 = $self306.vars[1];
anf_arg352 = $self306.vars[2];
message339 = $self306.vars[3];
anf_array_val321 = $self306.vars[4];
anf_array_val320 = $self306.vars[5];
anf_array_val319 = $self306.vars[6];
anf_array_val318 = $self306.vars[7];
anf_method_obj323 = $self306.vars[8];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[111],3,$t);
}
var self306 = $self306;
var loc307 = $loc307;
var from$fail$arg308 = $from$fail$arg308;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step304) {
case 0: var anf_method_obj323 = G(ED5,"para",L[88]);
$step304 = 1;
$al310 = L[89];
$field311 = R.getColonFieldLoc(ED5,"text",L[89]);
if(R.isMethod($field311)) {
$ans309 = $field311.full_meth(ED5,("The predicate"));
} else {
if(!(R.isFunction($field311))) {
R.ffi.throwNonFunApp(L[89],$field311);
}
$ans309 = $field311.app(("The predicate"));
}
break;
case 1: var anf_array_val318 = $ans309;
var anf_arg312 = G(self306,"pred-name",L[90]);
$step304 = 2;
$al310 = L[91];
$field313 = R.getColonFieldLoc(ED5,"text",L[91]);
if(R.isMethod($field313)) {
$ans309 = $field313.full_meth(ED5,anf_arg312);
} else {
if(!(R.isFunction($field313))) {
R.ffi.throwNonFunApp(L[91],$field313);
}
$ans309 = $field313.app(anf_arg312);
}
break;
case 2: var anf_arg314 = $ans309;
$step304 = 3;
$al310 = L[92];
$field315 = R.getColonFieldLoc(ED5,"code",L[92]);
if(R.isMethod($field315)) {
$ans309 = $field315.full_meth(ED5,anf_arg314);
} else {
if(!(R.isFunction($field315))) {
R.ffi.throwNonFunApp(L[92],$field315);
}
$ans309 = $field315.app(anf_arg314);
}
break;
case 3: var anf_array_val319 = $ans309;
$step304 = 4;
$al310 = L[93];
$field316 = R.getColonFieldLoc(ED5,"text",L[93]);
if(R.isMethod($field316)) {
$ans309 = $field316.full_meth(ED5,("in the annotation at"));
} else {
if(!(R.isFunction($field316))) {
R.ffi.throwNonFunApp(L[93],$field316);
}
$ans309 = $field316.app(("in the annotation at"));
}
break;
case 4: var anf_array_val320 = $ans309;
$step304 = 5;
$al310 = L[110];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al310,draw$and$highlight30.$var);
}
$ans309 = draw$and$highlight30.$var.app(loc307);
break;
case 5: var anf_array_val321 = $ans309;
$step304 = 6;
$al310 = L[94];
$field317 = R.getColonFieldLoc(ED5,"text",L[94]);
if(R.isMethod($field317)) {
$ans309 = $field317.full_meth(ED5,("returned false for this value:"));
} else {
if(!(R.isFunction($field317))) {
R.ffi.throwNonFunApp(L[94],$field317);
}
$ans309 = $field317.app(("returned false for this value:"));
}
break;
case 6: var anf_array_val322 = $ans309;
var anf_arg324 = [anf_array_val318,anf_array_val319,anf_array_val320,anf_array_val321,anf_array_val322];
$step304 = 7;
$al310 = L[88];
$field325 = R.getColonFieldLoc(anf_method_obj323,"make",L[88]);
if(R.isMethod($field325)) {
$ans309 = $field325.full_meth(anf_method_obj323,anf_arg324);
} else {
if(!(R.isFunction($field325))) {
R.ffi.throwNonFunApp(L[88],$field325);
}
$ans309 = $field325.app(anf_arg324);
}
break;
case 7: var message339 = $ans309;
$al310 = L[95];
var anf_if361 = R.checkWrapBoolean(from$fail$arg308);
if(R.isPyretTrue(anf_if361)) {
$step304 = 8;
} else {
$step304 = 11;
}
break;
case 8: var $temp_lam327 = function($l328) {
var $step326 = 0;
var $ans329 = D;
var $al330 = L[102];
try {
if(R.isActivationRecord($l328)) {
$step326 = $l328.step;
$al330 = $l328.from;
$ans329 = $l328.ans;
l328 = $l328.args[0];
anf_array_val334 = $l328.vars[0];
anf_method_obj336 = $l328.vars[1];
anf_array_val340 = $l328.vars[2];
anf_method_obj342 = $l328.vars[3];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[102],1,$t);
}
var l328 = $l328;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step326) {
case 0: var anf_method_obj342 = G(ED5,"error",L[96]);
var anf_arg331 = G(self306,"val",L[97]);
$step326 = 1;
$al330 = L[98];
$field332 = R.getColonFieldLoc(ED5,"embed",L[98]);
if(R.isMethod($field332)) {
$ans329 = $field332.full_meth(ED5,anf_arg331);
} else {
if(!(R.isFunction($field332))) {
R.ffi.throwNonFunApp(L[98],$field332);
}
$ans329 = $field332.app(anf_arg331);
}
break;
case 1: var anf_array_val340 = $ans329;
var anf_method_obj336 = G(ED5,"para",L[99]);
$step326 = 2;
$al330 = L[100];
$field333 = R.getColonFieldLoc(ED5,"text",L[100]);
if(R.isMethod($field333)) {
$ans329 = $field333.full_meth(ED5,("called from around"));
} else {
if(!(R.isFunction($field333))) {
R.ffi.throwNonFunApp(L[100],$field333);
}
$ans329 = $field333.app(("called from around"));
}
break;
case 2: var anf_array_val334 = $ans329;
$step326 = 3;
$al330 = L[101];
if(!(R.isFunction(draw$and$highlight30.$var))) {
R.ffi.throwNonFunApp($al330,draw$and$highlight30.$var);
}
$ans329 = draw$and$highlight30.$var.app(l328);
break;
case 3: var anf_array_val335 = $ans329;
var anf_arg337 = [anf_array_val334,anf_array_val335];
$step326 = 4;
$al330 = L[99];
$field338 = R.getColonFieldLoc(anf_method_obj336,"make",L[99]);
if(R.isMethod($field338)) {
$ans329 = $field338.full_meth(anf_method_obj336,anf_arg337);
} else {
if(!(R.isFunction($field338))) {
R.ffi.throwNonFunApp(L[99],$field338);
}
$ans329 = $field338.app(anf_arg337);
}
break;
case 4: var anf_array_val341 = $ans329;
var anf_arg343 = [message339,anf_array_val340,anf_array_val341];
$step326 = 5;
$al330 = L[96];
$field344 = R.getColonFieldLoc(anf_method_obj342,"make",L[96]);
if(R.isMethod($field344)) {
$ans329 = $field344.full_meth(anf_method_obj342,anf_arg343);
} else {
if(!(R.isFunction($field344))) {
R.ffi.throwNonFunApp(L[96],$field344);
}
$ans329 = $field344.app(anf_arg343);
}
break;
case 5: ++R.GAS;
return $ans329;
default: throw "No case numbered " + $step326 + " in $temp_lam327";
}
}
} catch($e345) {
if(R.isCont($e345) && ($step326 !== 5)) {
$e345.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al330,$temp_lam327,$step326,[l328],[anf_array_val334,anf_method_obj336,anf_array_val340,anf_method_obj342]);
}
if(R.isPyretException($e345)) {
$e345.pyretStack.push($al330);
}
throw $e345;
}
};
var anf_arg352 = R.makeFunction($temp_lam327);
var anf_method_obj349 = G(ED5,"error",L[103]);
var anf_arg346 = G(self306,"val",L[104]);
$step304 = 9;
$al310 = L[105];
$field347 = R.getColonFieldLoc(ED5,"embed",L[105]);
if(R.isMethod($field347)) {
$ans309 = $field347.full_meth(ED5,anf_arg346);
} else {
if(!(R.isFunction($field347))) {
R.ffi.throwNonFunApp(L[105],$field347);
}
$ans309 = $field347.app(anf_arg346);
}
break;
case 9: var anf_array_val348 = $ans309;
var anf_arg350 = [message339,anf_array_val348];
$step304 = 10;
$al310 = L[103];
$field351 = R.getColonFieldLoc(anf_method_obj349,"make",L[103]);
if(R.isMethod($field351)) {
$ans309 = $field351.full_meth(anf_method_obj349,anf_arg350);
} else {
if(!(R.isFunction($field351))) {
R.ffi.throwNonFunApp(L[103],$field351);
}
$ans309 = $field351.app(anf_arg350);
}
break;
case 10: var anf_arg353 = $ans309;
$step304 = 13;
$al310 = L[106];
$field354 = R.getColonFieldLoc(ED5,"maybe-stack-loc",L[106]);
if(R.isMethod($field354)) {
$ans309 = $field354.full_meth(ED5,(0),(true),anf_arg352,anf_arg353);
} else {
if(!(R.isFunction($field354))) {
R.ffi.throwNonFunApp(L[106],$field354);
}
$ans309 = $field354.app((0),(true),anf_arg352,anf_arg353);
}
break;
case 11: var anf_method_obj358 = G(ED5,"error",L[107]);
var anf_arg355 = G(self306,"val",L[108]);
$step304 = 12;
$al310 = L[109];
$field356 = R.getColonFieldLoc(ED5,"embed",L[109]);
if(R.isMethod($field356)) {
$ans309 = $field356.full_meth(ED5,anf_arg355);
} else {
if(!(R.isFunction($field356))) {
R.ffi.throwNonFunApp(L[109],$field356);
}
$ans309 = $field356.app(anf_arg355);
}
break;
case 12: var anf_array_val357 = $ans309;
var anf_arg359 = [message339,anf_array_val357];
$step304 = 13;
$al310 = L[107];
$field360 = R.getColonFieldLoc(anf_method_obj358,"make",L[107]);
if(R.isMethod($field360)) {
$ans309 = $field360.full_meth(anf_method_obj358,anf_arg359);
} else {
if(!(R.isFunction($field360))) {
R.ffi.throwNonFunApp(L[107],$field360);
}
$ans309 = $field360.app(anf_arg359);
}
break;
case 13: ++R.GAS;
return $ans309;
default: throw "No case numbered " + $step304 + " in $temp_full305";
}
}
} catch($e362) {
if(R.isCont($e362) && ($step304 !== 13)) {
$e362.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al310,$temp_full305,$step304,[self306,loc307,from$fail$arg308],[anf_method_obj358,anf_method_obj349,anf_arg352,message339,anf_array_val321,anf_array_val320,anf_array_val319,anf_array_val318,anf_method_obj323]);
}
if(R.isPyretException($e362)) {
$e362.pyretStack.push($al310);
}
throw $e362;
}
};
var anf_variant_member463 = R.makeMethod2($temp_full305);
var $temp_full364 = function($self365,$loc366,$from$fail$arg367) {
var $step363 = 0;
var $ans368 = D;
var $al369 = L[126];
try {
if(R.isActivationRecord($self365)) {
$step363 = $self365.step;
$al369 = $self365.from;
$ans368 = $self365.ans;
self365 = $self365.args[0];
loc366 = $self365.args[1];
from$fail$arg367 = $self365.args[2];
anf_array_val402 = $self365.vars[0];
anf_method_obj385 = $self365.vars[1];
anf_array_val401 = $self365.vars[2];
anf_array_val400 = $self365.vars[3];
anf_array_val376 = $self365.vars[4];
anf_array_val375 = $self365.vars[5];
anf_method_obj378 = $self365.vars[6];
anf_method_obj404 = $self365.vars[7];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[126],3,$t);
}
var self365 = $self365;
var loc366 = $loc366;
var from$fail$arg367 = $from$fail$arg367;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step363) {
case 0: var anf_method_obj404 = G(ED5,"error",L[112]);
var anf_method_obj378 = G(ED5,"para",L[113]);
$step363 = 1;
$al369 = L[114];
$field370 = R.getColonFieldLoc(ED5,"text",L[114]);
if(R.isMethod($field370)) {
$ans368 = $field370.full_meth(ED5,("The record annotation at"));
} else {
if(!(R.isFunction($field370))) {
R.ffi.throwNonFunApp(L[114],$field370);
}
$ans368 = $field370.app(("The record annotation at"));
}
break;
case 1: var anf_array_val375 = $ans368;
$step363 = 2;
$al369 = L[115];
$field371 = R.getColonFieldLoc(ED5,"text",L[115]);
if(R.isMethod($field371)) {
$ans368 = $field371.full_meth(ED5,("this annotation"));
} else {
if(!(R.isFunction($field371))) {
R.ffi.throwNonFunApp(L[115],$field371);
}
$ans368 = $field371.app(("this annotation"));
}
break;
case 2: var anf_arg372 = $ans368;
$step363 = 3;
$al369 = L[116];
$field373 = R.getColonFieldLoc(ED5,"loc-display",L[116]);
if(R.isMethod($field373)) {
$ans368 = $field373.full_meth(ED5,loc366,("error-highlight"),anf_arg372);
} else {
if(!(R.isFunction($field373))) {
R.ffi.throwNonFunApp(L[116],$field373);
}
$ans368 = $field373.app(loc366,("error-highlight"),anf_arg372);
}
break;
case 3: var anf_array_val376 = $ans368;
$step363 = 4;
$al369 = L[117];
$field374 = R.getColonFieldLoc(ED5,"text",L[117]);
if(R.isMethod($field374)) {
$ans368 = $field374.full_meth(ED5,("failed on this value:"));
} else {
if(!(R.isFunction($field374))) {
R.ffi.throwNonFunApp(L[117],$field374);
}
$ans368 = $field374.app(("failed on this value:"));
}
break;
case 4: var anf_array_val377 = $ans368;
var anf_arg379 = [anf_array_val375,anf_array_val376,anf_array_val377];
$step363 = 5;
$al369 = L[113];
$field380 = R.getColonFieldLoc(anf_method_obj378,"make",L[113]);
if(R.isMethod($field380)) {
$ans368 = $field380.full_meth(anf_method_obj378,anf_arg379);
} else {
if(!(R.isFunction($field380))) {
R.ffi.throwNonFunApp(L[113],$field380);
}
$ans368 = $field380.app(anf_arg379);
}
break;
case 5: var anf_array_val400 = $ans368;
var anf_arg381 = G(self365,"val",L[118]);
$step363 = 6;
$al369 = L[119];
$field382 = R.getColonFieldLoc(ED5,"embed",L[119]);
if(R.isMethod($field382)) {
$ans368 = $field382.full_meth(ED5,anf_arg381);
} else {
if(!(R.isFunction($field382))) {
R.ffi.throwNonFunApp(L[119],$field382);
}
$ans368 = $field382.app(anf_arg381);
}
break;
case 6: var anf_array_val401 = $ans368;
var anf_method_obj385 = G(ED5,"para",L[120]);
$step363 = 7;
$al369 = L[121];
$field383 = R.getColonFieldLoc(ED5,"text",L[121]);
if(R.isMethod($field383)) {
$ans368 = $field383.full_meth(ED5,("Because:"));
} else {
if(!(R.isFunction($field383))) {
R.ffi.throwNonFunApp(L[121],$field383);
}
$ans368 = $field383.app(("Because:"));
}
break;
case 7: var anf_array_val384 = $ans368;
var anf_arg386 = [anf_array_val384];
$step363 = 8;
$al369 = L[120];
$field387 = R.getColonFieldLoc(anf_method_obj385,"make",L[120]);
if(R.isMethod($field387)) {
$ans368 = $field387.full_meth(anf_method_obj385,anf_arg386);
} else {
if(!(R.isFunction($field387))) {
R.ffi.throwNonFunApp(L[120],$field387);
}
$ans368 = $field387.app(anf_arg386);
}
break;
case 8: var anf_array_val402 = $ans368;
var anf_method_obj395 = G(self365,"field-failures",L[122]);
var $temp_lam389 = function($recv_390) {
var $step388 = 0;
var $ans391 = D;
var $al392 = L[123];
try {
if(R.isActivationRecord($recv_390)) {
$step388 = $recv_390.step;
$al392 = $recv_390.from;
$ans391 = $recv_390.ans;
recv_390 = $recv_390.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[123],1,$t);
}
var recv_390 = $recv_390;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step388) {
case 0: $step388 = 1;
$al392 = L[123];
$field393 = R.getColonFieldLoc(recv_390,"render-reason",L[123]);
if(R.isMethod($field393)) {
$ans391 = $field393.full_meth(recv_390,loc366,(false));
} else {
if(!(R.isFunction($field393))) {
R.ffi.throwNonFunApp(L[123],$field393);
}
$ans391 = $field393.app(loc366,(false));
}
break;
case 1: ++R.GAS;
return $ans391;
default: throw "No case numbered " + $step388 + " in $temp_lam389";
}
}
} catch($e394) {
if(R.isCont($e394) && ($step388 !== 1)) {
$e394.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al392,$temp_lam389,$step388,[recv_390],[]);
}
if(R.isPyretException($e394)) {
$e394.pyretStack.push($al392);
}
throw $e394;
}
};
var anf_arg396 = R.makeFunction($temp_lam389);
$step363 = 9;
$al369 = L[124];
$field397 = R.getColonFieldLoc(anf_method_obj395,"map",L[124]);
if(R.isMethod($field397)) {
$ans368 = $field397.full_meth(anf_method_obj395,anf_arg396);
} else {
if(!(R.isFunction($field397))) {
R.ffi.throwNonFunApp(L[124],$field397);
}
$ans368 = $field397.app(anf_arg396);
}
break;
case 9: var anf_arg398 = $ans368;
$step363 = 10;
$al369 = L[125];
$field399 = R.getColonFieldLoc(ED5,"bulleted-sequence",L[125]);
if(R.isMethod($field399)) {
$ans368 = $field399.full_meth(ED5,anf_arg398);
} else {
if(!(R.isFunction($field399))) {
R.ffi.throwNonFunApp(L[125],$field399);
}
$ans368 = $field399.app(anf_arg398);
}
break;
case 10: var anf_array_val403 = $ans368;
var anf_arg405 = [anf_array_val400,anf_array_val401,anf_array_val402,anf_array_val403];
$step363 = 11;
$al369 = L[112];
$field406 = R.getColonFieldLoc(anf_method_obj404,"make",L[112]);
if(R.isMethod($field406)) {
$ans368 = $field406.full_meth(anf_method_obj404,anf_arg405);
} else {
if(!(R.isFunction($field406))) {
R.ffi.throwNonFunApp(L[112],$field406);
}
$ans368 = $field406.app(anf_arg405);
}
break;
case 11: ++R.GAS;
return $ans368;
default: throw "No case numbered " + $step363 + " in $temp_full364";
}
}
} catch($e407) {
if(R.isCont($e407) && ($step363 !== 11)) {
$e407.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al369,$temp_full364,$step363,[self365,loc366,from$fail$arg367],[anf_array_val402,anf_method_obj385,anf_array_val401,anf_array_val400,anf_array_val376,anf_array_val375,anf_method_obj378,anf_method_obj404]);
}
if(R.isPyretException($e407)) {
$e407.pyretStack.push($al369);
}
throw $e407;
}
};
var anf_variant_member474 = R.makeMethod2($temp_full364);
var $temp_full409 = function($self410,$loc411,$from$fail$arg412) {
var $step408 = 0;
var $ans413 = D;
var $al414 = L[138];
try {
if(R.isActivationRecord($self410)) {
$step408 = $self410.step;
$al414 = $self410.from;
$ans413 = $self410.ans;
self410 = $self410.args[0];
loc411 = $self410.args[1];
from$fail$arg412 = $self410.args[2];
anf_array_val428 = $self410.vars[0];
anf_array_val427 = $self410.vars[1];
anf_array_val426 = $self410.vars[2];
anf_method_obj430 = $self410.vars[3];
anf_method_obj434 = $self410.vars[4];
} else {
var $l = arguments.length;
if($l !== 3) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[138],3,$t);
}
var self410 = $self410;
var loc411 = $loc411;
var from$fail$arg412 = $from$fail$arg412;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step408) {
case 0: var anf_method_obj434 = G(ED5,"error",L[127]);
var anf_method_obj430 = G(ED5,"para",L[128]);
$step408 = 1;
$al414 = L[129];
$field415 = R.getColonFieldLoc(ED5,"text",L[129]);
if(R.isMethod($field415)) {
$ans413 = $field415.full_meth(ED5,("Couldn't find"));
} else {
if(!(R.isFunction($field415))) {
R.ffi.throwNonFunApp(L[129],$field415);
}
$ans413 = $field415.app(("Couldn't find"));
}
break;
case 1: var anf_array_val426 = $ans413;
var anf_arg416 = G(self410,"field",L[130]);
$step408 = 2;
$al414 = L[137];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al414,_plus1);
}
$ans413 = _plus1.app(("the annotation named "),anf_arg416);
break;
case 2: var anf_arg417 = $ans413;
$step408 = 3;
$al414 = L[131];
$field418 = R.getColonFieldLoc(ED5,"text",L[131]);
if(R.isMethod($field418)) {
$ans413 = $field418.full_meth(ED5,anf_arg417);
} else {
if(!(R.isFunction($field418))) {
R.ffi.throwNonFunApp(L[131],$field418);
}
$ans413 = $field418.app(anf_arg417);
}
break;
case 3: var anf_arg419 = $ans413;
$step408 = 4;
$al414 = L[132];
$field420 = R.getColonFieldLoc(ED5,"loc-display",L[132]);
if(R.isMethod($field420)) {
$ans413 = $field420.full_meth(ED5,loc411,("error-highlight"),anf_arg419);
} else {
if(!(R.isFunction($field420))) {
R.ffi.throwNonFunApp(L[132],$field420);
}
$ans413 = $field420.app(loc411,("error-highlight"),anf_arg419);
}
break;
case 4: var anf_array_val427 = $ans413;
$step408 = 5;
$al414 = L[133];
$field421 = R.getColonFieldLoc(ED5,"text",L[133]);
if(R.isMethod($field421)) {
$ans413 = $field421.full_meth(ED5,("in the annotations from"));
} else {
if(!(R.isFunction($field421))) {
R.ffi.throwNonFunApp(L[133],$field421);
}
$ans413 = $field421.app(("in the annotations from"));
}
break;
case 5: var anf_array_val428 = $ans413;
var anf_arg422 = G(self410,"name",L[134]);
$step408 = 6;
$al414 = L[135];
$field423 = R.getColonFieldLoc(ED5,"text",L[135]);
if(R.isMethod($field423)) {
$ans413 = $field423.full_meth(ED5,anf_arg422);
} else {
if(!(R.isFunction($field423))) {
R.ffi.throwNonFunApp(L[135],$field423);
}
$ans413 = $field423.app(anf_arg422);
}
break;
case 6: var anf_arg424 = $ans413;
$step408 = 7;
$al414 = L[136];
$field425 = R.getColonFieldLoc(ED5,"code",L[136]);
if(R.isMethod($field425)) {
$ans413 = $field425.full_meth(ED5,anf_arg424);
} else {
if(!(R.isFunction($field425))) {
R.ffi.throwNonFunApp(L[136],$field425);
}
$ans413 = $field425.app(anf_arg424);
}
break;
case 7: var anf_array_val429 = $ans413;
var anf_arg431 = [anf_array_val426,anf_array_val427,anf_array_val428,anf_array_val429];
$step408 = 8;
$al414 = L[128];
$field432 = R.getColonFieldLoc(anf_method_obj430,"make",L[128]);
if(R.isMethod($field432)) {
$ans413 = $field432.full_meth(anf_method_obj430,anf_arg431);
} else {
if(!(R.isFunction($field432))) {
R.ffi.throwNonFunApp(L[128],$field432);
}
$ans413 = $field432.app(anf_arg431);
}
break;
case 8: var anf_array_val433 = $ans413;
var anf_arg435 = [anf_array_val433];
$step408 = 9;
$al414 = L[127];
$field436 = R.getColonFieldLoc(anf_method_obj434,"make",L[127]);
if(R.isMethod($field436)) {
$ans413 = $field436.full_meth(anf_method_obj434,anf_arg435);
} else {
if(!(R.isFunction($field436))) {
R.ffi.throwNonFunApp(L[127],$field436);
}
$ans413 = $field436.app(anf_arg435);
}
break;
case 9: ++R.GAS;
return $ans413;
default: throw "No case numbered " + $step408 + " in $temp_full409";
}
}
} catch($e437) {
if(R.isCont($e437) && ($step408 !== 9)) {
$e437.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al414,$temp_full409,$step408,[self410,loc411,from$fail$arg412],[anf_array_val428,anf_array_val427,anf_array_val426,anf_method_obj430,anf_method_obj434]);
}
if(R.isPyretException($e437)) {
$e437.pyretStack.push($al414);
}
throw $e437;
}
};
var anf_variant_member485 = R.makeMethod2($temp_full409);
var $ref$init_getfields444 = function(f) {
return f(this.dict["loc"],this.dict["reason"]);
};
var $ref$init_getfieldsref442 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["reason"],false,refmask[1]));
};
var $ref$init_mutablemask443 = [false,false];
var $ref$init$base438 = {"$fieldNames":["loc","reason"],
"render-reason":anf_variant_member441,
"_match":R.makeMatch("ref-init",2)};
var $ref$init$brands440 = {"$brand$ref$init":true};
$ref$init$brands440[FailureReason19._brand] = true;
var ref$init446 = R.makeVariantConstructor(L[141],function() {
return [FailureReason73];
},["reason447"],[L[140]],[false,false],["loc448","reason447"],$ref$init_mutablemask443,$ref$init$base438,$ref$init$brands440,"ref-init",$ref$init_getfieldsref442,$ref$init_getfields444,$ref$init$base438);
var $type$mismatch_getfields455 = function(f) {
return f(this.dict["val"],this.dict["name"]);
};
var $type$mismatch_getfieldsref453 = function(f,refmask) {
return f(R.derefField(this.dict["val"],false,refmask[0]),R.derefField(this.dict["name"],false,refmask[1]));
};
var $type$mismatch_mutablemask454 = [false,false];
var $type$mismatch$base449 = {"$fieldNames":["val","name"],
"render-reason":anf_variant_member452,
"_match":R.makeMatch("type-mismatch",2)};
var $type$mismatch$brands451 = {"$brand$type$mismatch":true};
$type$mismatch$brands451[FailureReason19._brand] = true;
var type$mismatch457 = R.makeVariantConstructor(L[144],function() {
return [$type$String2];
},["name458"],[L[143]],[false,false],["val459","name458"],$type$mismatch_mutablemask454,$type$mismatch$base449,$type$mismatch$brands451,"type-mismatch",$type$mismatch_getfieldsref453,$type$mismatch_getfields455,$type$mismatch$base449);
var $predicate$failure_getfields466 = function(f) {
return f(this.dict["val"],this.dict["pred-name"]);
};
var $predicate$failure_getfieldsref464 = function(f,refmask) {
return f(R.derefField(this.dict["val"],false,refmask[0]),R.derefField(this.dict["pred-name"],false,refmask[1]));
};
var $predicate$failure_mutablemask465 = [false,false];
var $predicate$failure$base460 = {"$fieldNames":["val","pred-name"],
"render-reason":anf_variant_member463,
"_match":R.makeMatch("predicate-failure",2)};
var $predicate$failure$brands462 = {"$brand$predicate$failure":true};
$predicate$failure$brands462[FailureReason19._brand] = true;
var predicate$failure468 = R.makeVariantConstructor(L[146],function() {
return [];
},[],[],[false,false],["val469","pred$name470"],$predicate$failure_mutablemask465,$predicate$failure$base460,$predicate$failure$brands462,"predicate-failure",$predicate$failure_getfieldsref464,$predicate$failure_getfields466,$predicate$failure$base460);
var $record$fields$fail_getfields477 = function(f) {
return f(this.dict["val"],this.dict["field-failures"]);
};
var $record$fields$fail_getfieldsref475 = function(f,refmask) {
return f(R.derefField(this.dict["val"],false,refmask[0]),R.derefField(this.dict["field-failures"],false,refmask[1]));
};
var $record$fields$fail_mutablemask476 = [false,false];
var $record$fields$fail$base471 = {"$fieldNames":["val","field-failures"],
"render-reason":anf_variant_member474,
"_match":R.makeMatch("record-fields-fail",2)};
var $record$fields$fail$brands473 = {"$brand$record$fields$fail":true};
$record$fields$fail$brands473[FailureReason19._brand] = true;
var record$fields$fail479 = R.makeVariantConstructor(L[150],function() {
return [R.getDotAnn(L[148],"L",L8,"List")];
},["field$failures480"],[L[149]],[false,false],["val481","field$failures480"],$record$fields$fail_mutablemask476,$record$fields$fail$base471,$record$fields$fail$brands473,"record-fields-fail",$record$fields$fail_getfieldsref475,$record$fields$fail_getfields477,$record$fields$fail$base471);
var $dot$ann$not$present_getfields488 = function(f) {
return f(this.dict["name"],this.dict["field"]);
};
var $dot$ann$not$present_getfieldsref486 = function(f,refmask) {
return f(R.derefField(this.dict["name"],false,refmask[0]),R.derefField(this.dict["field"],false,refmask[1]));
};
var $dot$ann$not$present_mutablemask487 = [false,false];
var $dot$ann$not$present$base482 = {"$fieldNames":["name","field"],
"render-reason":anf_variant_member485,
"_match":R.makeMatch("dot-ann-not-present",2)};
var $dot$ann$not$present$brands484 = {"$brand$dot$ann$not$present":true};
$dot$ann$not$present$brands484[FailureReason19._brand] = true;
var dot$ann$not$present490 = R.makeVariantConstructor(L[152],function() {
return [];
},[],[],[false,false],["name491","field492"],$dot$ann$not$present_mutablemask487,$dot$ann$not$present$base482,$dot$ann$not$present$brands484,"dot-ann-not-present",$dot$ann$not$present_getfieldsref486,$dot$ann$not$present_getfields488,$dot$ann$not$present$base482);
var anf_assign494 = R.makeObject({"FailureReason":R.makeFunction(function($val493) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[153],1,$t);
}
return R.makeBoolean(R.hasBrand($val493,FailureReason19._brand));
}),
"is-ref-init":R.makeFunction(function($val445) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[139],1,$t);
}
return R.makeBoolean(R.hasBrand($val445,"$brand$ref$init"));
}),
"ref-init":ref$init446,
"is-type-mismatch":R.makeFunction(function($val456) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[142],1,$t);
}
return R.makeBoolean(R.hasBrand($val456,"$brand$type$mismatch"));
}),
"type-mismatch":type$mismatch457,
"is-predicate-failure":R.makeFunction(function($val467) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[145],1,$t);
}
return R.makeBoolean(R.hasBrand($val467,"$brand$predicate$failure"));
}),
"predicate-failure":predicate$failure468,
"is-record-fields-fail":R.makeFunction(function($val478) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[147],1,$t);
}
return R.makeBoolean(R.hasBrand($val478,"$brand$record$fields$fail"));
}),
"record-fields-fail":record$fields$fail479,
"is-dot-ann-not-present":R.makeFunction(function($val489) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[151],1,$t);
}
return R.makeBoolean(R.hasBrand($val489,"$brand$dot$ann$not$present"));
}),
"dot-ann-not-present":dot$ann$not$present490});
FailureReason495.$var = anf_assign494;
var anf_assign496 = G(FailureReason495.$var,"FailureReason",L[153]);
FailureReason497.$var = anf_assign496;
var anf_assign498 = G(FailureReason495.$var,"FailureReason",L[153]);
is$FailureReason499.$var = anf_assign498;
var anf_assign500 = G(FailureReason495.$var,"is-ref-init",L[139]);
is$ref$init501.$var = anf_assign500;
var anf_assign502 = G(FailureReason495.$var,"ref-init",L[139]);
ref$init503.$var = anf_assign502;
var anf_assign504 = G(FailureReason495.$var,"is-type-mismatch",L[142]);
is$type$mismatch505.$var = anf_assign504;
var anf_assign506 = G(FailureReason495.$var,"type-mismatch",L[142]);
type$mismatch507.$var = anf_assign506;
var anf_assign508 = G(FailureReason495.$var,"is-predicate-failure",L[145]);
is$predicate$failure509.$var = anf_assign508;
var anf_assign510 = G(FailureReason495.$var,"predicate-failure",L[145]);
predicate$failure511.$var = anf_assign510;
var anf_assign512 = G(FailureReason495.$var,"is-record-fields-fail",L[147]);
is$record$fields$fail513.$var = anf_assign512;
var anf_assign514 = G(FailureReason495.$var,"record-fields-fail",L[147]);
record$fields$fail515.$var = anf_assign514;
var anf_assign516 = G(FailureReason495.$var,"is-dot-ann-not-present",L[151]);
is$dot$ann$not$present517.$var = anf_assign516;
var anf_assign518 = G(FailureReason495.$var,"dot-ann-not-present",L[151]);
dot$ann$not$present519.$var = anf_assign518;
var provides525 = R.makeObject({"draw-and-highlight":draw$and$highlight30.$var,
"ContractResult":ContractResult91.$var,
"is-ContractResult":is$ContractResult93.$var,
"ok":ok97.$var,
"is-ok":is$ok95.$var,
"fail":fail101.$var,
"is-fail":is$fail99.$var,
"fail-arg":fail$arg105.$var,
"is-fail-arg":is$fail$arg103.$var,
"FieldFailure":FieldFailure190.$var,
"is-FieldFailure":is$FieldFailure192.$var,
"field-failure":field$failure196.$var,
"is-field-failure":is$field$failure194.$var,
"missing-field":missing$field200.$var,
"is-missing-field":is$missing$field198.$var,
"FailureReason":FailureReason497.$var,
"is-FailureReason":is$FailureReason499.$var,
"ref-init":ref$init503.$var,
"is-ref-init":is$ref$init501.$var,
"type-mismatch":type$mismatch507.$var,
"is-type-mismatch":is$type$mismatch505.$var,
"predicate-failure":predicate$failure511.$var,
"is-predicate-failure":is$predicate$failure509.$var,
"record-fields-fail":record$fields$fail515.$var,
"is-record-fields-fail":is$record$fields$fail513.$var,
"dot-ann-not-present":dot$ann$not$present519.$var,
"is-dot-ann-not-present":is$dot$ann$not$present517.$var});
$step12 = 1;
$al16 = L[20];
$field520 = R.getColonFieldLoc(builtins3,"current-checker",L[20]);
if(R.isMethod($field520)) {
$ans15 = $field520.full_meth(builtins3);
} else {
if(!(R.isFunction($field520))) {
R.ffi.throwNonFunApp(L[20],$field520);
}
$ans15 = $field520.app();
}
break;
case 1: var anf_method_obj521 = $ans15;
$step12 = 2;
$al16 = L[20];
$field522 = R.getColonFieldLoc(anf_method_obj521,"results",L[20]);
if(R.isMethod($field522)) {
$ans15 = $field522.full_meth(anf_method_obj521);
} else {
if(!(R.isFunction($field522))) {
R.ffi.throwNonFunApp(L[20],$field522);
}
$ans15 = $field522.app();
}
break;
case 2: var checks526 = $ans15;
$step12 = 3;
$ans15 = R.makeObject({"answer":nothing4,
"namespace":NAMESPACE,
"defined-values":{"dot-ann-not-present":dot$ann$not$present519.$var,
"is-dot-ann-not-present":is$dot$ann$not$present517.$var,
"record-fields-fail":record$fields$fail515.$var,
"is-record-fields-fail":is$record$fields$fail513.$var,
"predicate-failure":predicate$failure511.$var,
"is-predicate-failure":is$predicate$failure509.$var,
"type-mismatch":type$mismatch507.$var,
"is-type-mismatch":is$type$mismatch505.$var,
"ref-init":ref$init503.$var,
"is-ref-init":is$ref$init501.$var,
"is-FailureReason":is$FailureReason499.$var,
"FailureReason":FailureReason497.$var,
"missing-field":missing$field200.$var,
"is-missing-field":is$missing$field198.$var,
"field-failure":field$failure196.$var,
"is-field-failure":is$field$failure194.$var,
"is-FieldFailure":is$FieldFailure192.$var,
"FieldFailure":FieldFailure190.$var,
"fail-arg":fail$arg105.$var,
"is-fail-arg":is$fail$arg103.$var,
"fail":fail101.$var,
"is-fail":is$fail99.$var,
"ok":ok97.$var,
"is-ok":is$ok95.$var,
"is-ContractResult":is$ContractResult93.$var,
"ContractResult":ContractResult91.$var,
"draw-and-highlight":draw$and$highlight30.$var,
"ED":ED5,
"L":L6},
"defined-types":{"FailureReason":FailureReason73,
"L":L8,
"FieldFailure":FieldFailure524,
"ContractResult":ContractResult523,
"ED":ED7},
"provide-plus-types":R.makeObject({"values":provides525,
"types":{"ContractResult":ContractResult523,
"FieldFailure":FieldFailure524,
"FailureReason":FailureReason73}}),
"checks":checks526});
break;
case 3: ++R.GAS;
return $ans15;
default: throw "No case numbered " + $step12 + " in $toplevel13";
}
}
} catch($e528) {
if(R.isCont($e528) && ($step12 !== 3)) {
$e528.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al16,$toplevel13,$step12,[],[provides525,dot$ann$not$present519,is$dot$ann$not$present517,record$fields$fail515,is$record$fields$fail513,predicate$failure511,is$predicate$failure509,type$mismatch507,is$type$mismatch505,ref$init503,is$ref$init501,is$FailureReason499,FailureReason497,missing$field200,is$missing$field198,field$failure196,is$field$failure194,is$FieldFailure192,FieldFailure190,fail$arg105,is$fail$arg103,fail101,is$fail99,ok97,is$ok95,is$ContractResult93,ContractResult91,draw$and$highlight30,FailureReason73,FieldFailure524,ContractResult523]);
}
if(R.isPyretException($e528)) {
$e528.pyretStack.push($al16);
}
throw $e528;
}
};
return R.safeCall($toplevel13,function(moduleVal) {
R.modules["$src/arr/base/contracts.arr9"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
