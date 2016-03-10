({
"requires":[{"import-type":"builtin",
"name":"valueskeleton"}],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":
function(R,NAMESPACE, M, $VS15) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,82,29,2625,82,46,2642],
[M,82,17,2613,82,47,2643],
[M,82,2,2598,82,51,2647],
[M,83,22,2671,83,40,2689],
[M,83,2,2651,83,44,2693],
[M,11,20,161,11,36,177],
[M,11,6,147,11,42,183],
[M,10,4,124,12,7,191],
[M,13,15,208,13,31,224],
[M,13,4,197,13,35,228],
[M,15,6,264,15,16,274],
[M,15,6,264,15,67,325],
[M,15,29,287,15,46,304],
[M,15,50,308,15,66,324],
[M,15,29,287,15,66,324],
[M,15,6,264,15,23,281],
[M,14,4,234,16,7,333],
[M,19,35,422,19,51,438],
[M,19,54,441,19,71,458],
[M,19,35,422,19,71,458],
[M,19,10,397,19,30,417],
[M,20,10,470,20,37,497],
[M,18,6,366,21,9,516],
[M,18,12,372,18,18,378],
[M,17,4,339,22,7,524],
[M,23,4,530,23,30,556],
[M,35,9,923,35,18,932],
[M,35,20,934,35,31,945],
[M,35,20,934,35,38,952],
[M,36,29,995,36,44,1010],
[M,37,33,1045,37,50,1062],
[M,35,6,920,37,51,1063],
[M,37,24,1036,37,51,1063],
[M,36,20,986,36,45,1011],
[M,33,4,790,38,7,1071],
[M,39,15,1088,39,26,1099],
[M,39,44,1117,39,59,1132],
[M,39,78,1151,39,91,1164],
[M,39,15,1088,39,92,1165],
[M,39,69,1142,39,92,1165],
[M,39,35,1108,39,60,1133],
[M,39,4,1077,39,96,1169],
[M,41,6,1215,41,15,1224],
[M,41,6,1215,41,56,1265],
[M,41,28,1237,41,39,1248],
[M,41,43,1252,41,55,1264],
[M,41,28,1237,41,55,1264],
[M,41,6,1215,41,22,1231],
[M,40,29,1200,40,35,1206],
[M,40,4,1175,42,7,1273],
[M,46,10,1456,46,20,1466],
[M,47,18,1493,47,33,1508],
[M,47,36,1511,47,52,1527],
[M,47,18,1493,47,52,1527],
[M,45,6,1425,48,9,1537],
[M,45,12,1431,45,18,1437],
[M,43,26,1301,43,32,1307],
[M,43,4,1279,49,7,1545],
[M,51,13,1580,51,24,1591],
[M,52,8,1601,52,23,1616],
[M,52,25,1618,52,42,1635],
[M,52,44,1637,52,59,1652],
[M,53,8,1662,53,23,1677],
[M,53,25,1679,53,42,1696],
[M,53,44,1698,53,59,1713],
[M,51,6,1573,51,12,1579],
[M,51,6,1573,53,60,1714],
[M,50,4,1551,54,7,1722],
[M,56,13,1755,56,24,1766],
[M,57,8,1776,57,21,1789],
[M,57,23,1791,57,38,1806],
[M,57,40,1808,57,53,1821],
[M,58,8,1831,58,21,1844],
[M,58,23,1846,58,38,1861],
[M,58,40,1863,58,53,1876],
[M,56,6,1748,56,12,1754],
[M,56,6,1748,58,54,1877],
[M,55,4,1728,59,7,1885],
[M,62,9,2003,62,24,2018],
[M,62,28,2022,62,44,2038],
[M,62,9,2003,62,44,2038],
[M,63,11,2051,63,24,2064],
[M,63,28,2068,63,42,2082],
[M,63,11,2051,63,42,2082],
[M,66,17,2130,66,28,2141],
[M,67,12,2155,67,27,2170],
[M,67,29,2172,67,46,2189],
[M,67,48,2191,67,63,2206],
[M,68,12,2220,68,26,2234],
[M,68,28,2236,68,44,2252],
[M,68,46,2254,68,60,2268],
[M,66,10,2123,66,16,2129],
[M,66,10,2123,68,61,2269],
[M,71,11,2305,71,24,2318],
[M,71,27,2321,71,41,2335],
[M,71,11,2305,71,41,2335],
[M,72,17,2354,72,28,2365],
[M,73,12,2379,73,28,2395],
[M,73,30,2397,73,48,2415],
[M,73,50,2417,73,66,2433],
[M,74,12,2447,74,25,2460],
[M,74,27,2462,74,42,2477],
[M,74,44,2479,74,57,2492],
[M,72,10,2347,72,16,2353],
[M,72,10,2347,74,58,2493],
[M,60,33,1920,60,42,1929],
[M,60,25,1912,60,43,1930],
[M,60,4,1891,79,7,2553],
[M,80,4,2559,80,31,2586],
[M,9,2,91,23,30,556],
[M,9,4,93,9,24,113],
[M,24,2,559,80,31,2586],
[M,25,18,587,25,24,593],
[M,26,22,617,26,28,623],
[M,27,24,649,27,30,655],
[M,28,22,679,28,28,685],
[M,29,20,707,29,26,713],
[M,30,22,737,30,28,743],
[M,31,20,765,31,26,771],
[M,24,4,561,32,7,779],
[M,8,0,76,84,3,2697],
[M,3,0,21,84,3,2697]];
var _plus1 = NAMESPACE.get("_plus");
var equal$always2 = NAMESPACE.get("equal-always");
var _lessthan3 = NAMESPACE.get("_lessthan");
var tostring4 = NAMESPACE.get("tostring");
var _lessequal5 = NAMESPACE.get("_lessequal");
var _greaterthan6 = NAMESPACE.get("_greaterthan");
var _greaterequal7 = NAMESPACE.get("_greaterequal");
var $type$Number8 = NAMESPACE.get("$type$Number");
var $type$String9 = NAMESPACE.get("$type$String");
var builtins10 = NAMESPACE.get("builtins");
var nothing11 = NAMESPACE.get("nothing");
var VS12 = R.getField($VS15,"values");
var VS13 = R.getField($VS15,"types");
NAMESPACE = R.addModuleToNamespace(NAMESPACE,[],[],$VS15);
var $toplevel17 = function($$resumer272) {
var $step16 = 0;
var $ans19 = D;
var $al20 = L[121];
try {
if(R.isActivationRecord($$resumer272)) {
$step16 = $$resumer272.step;
$al20 = $$resumer272.from;
$ans19 = $$resumer272.ans;
$resumer272 = $$resumer272.args[0];
provides270 = $$resumer272.vars[0];
srcloc155 = $$resumer272.vars[1];
is$srcloc131 = $$resumer272.vars[2];
builtin264 = $$resumer272.vars[3];
is$builtin60 = $$resumer272.vars[4];
is$Srcloc261 = $$resumer272.vars[5];
Srcloc259 = $$resumer272.vars[6];
Srcloc86 = $$resumer272.vars[7];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step16) {
case 0: var Srcloc21 = R.namedBrander("Srcloc");
var Srcloc86 = R.makeBranderAnn(Srcloc21,"Srcloc");
var Srcloc257 = {"$var":D};
var Srcloc259 = {"$var":D};
var is$Srcloc261 = {"$var":D};
var is$builtin60 = {"$var":D};
var builtin264 = {"$var":D};
var is$srcloc131 = {"$var":D};
var srcloc155 = {"$var":D};
var $temp_full23 = function($self24) {
var $step22 = 0;
var $ans25 = D;
var $al26 = L[2];
try {
if(R.isActivationRecord($self24)) {
$step22 = $self24.step;
$al26 = $self24.from;
$ans25 = $self24.ans;
self24 = $self24.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[2],1,$t);
}
var self24 = $self24;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step22) {
case 0: $step22 = 1;
$al26 = L[0];
$field27 = R.getColonFieldLoc(self24,"format",L[0]);
if(R.isMethod($field27)) {
$ans25 = $field27.full_meth(self24,(true));
} else {
if(!(R.isFunction($field27))) {
R.ffi.throwNonFunApp(L[0],$field27);
}
$ans25 = $field27.app((true));
}
break;
case 1: var anf_arg28 = $ans25;
$step22 = 2;
$al26 = L[1];
$field29 = R.getColonFieldLoc(VS12,"vs-value",L[1]);
if(R.isMethod($field29)) {
$ans25 = $field29.full_meth(VS12,anf_arg28);
} else {
if(!(R.isFunction($field29))) {
R.ffi.throwNonFunApp(L[1],$field29);
}
$ans25 = $field29.app(anf_arg28);
}
break;
case 2: ++R.GAS;
return $ans25;
default: throw "No case numbered " + $step22 + " in $temp_full23";
}
}
} catch($e30) {
if(R.isCont($e30) && ($step22 !== 2)) {
$e30.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al26,$temp_full23,$step22,[self24],[]);
}
if(R.isPyretException($e30)) {
$e30.pyretStack.push($al26);
}
throw $e30;
}
};
var anf_shared216 = R.makeMethod0($temp_full23);
var $temp_full32 = function($self33,$other34) {
var $step31 = 0;
var $ans35 = D;
var $al36 = L[4];
try {
if(R.isActivationRecord($self33)) {
$step31 = $self33.step;
$al36 = $self33.from;
$ans35 = $self33.ans;
self33 = $self33.args[0];
other34 = $self33.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[4],2,$t);
}
var self33 = $self33;
var other34 = $other34;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step31) {
case 0: $step31 = 1;
$al36 = L[3];
$field37 = R.getColonFieldLoc(other34,"before",L[3]);
if(R.isMethod($field37)) {
$ans35 = $field37.full_meth(other34,self33);
} else {
if(!(R.isFunction($field37))) {
R.ffi.throwNonFunApp(L[3],$field37);
}
$ans35 = $field37.app(self33);
}
break;
case 1: ++R.GAS;
return $ans35;
default: throw "No case numbered " + $step31 + " in $temp_full32";
}
}
} catch($e38) {
if(R.isCont($e38) && ($step31 !== 1)) {
$e38.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al36,$temp_full32,$step31,[self33,other34],[]);
}
if(R.isPyretException($e38)) {
$e38.pyretStack.push($al36);
}
throw $e38;
}
};
var anf_shared217 = R.makeMethod1($temp_full32);
var $temp_full40 = function($self41,$$underscore42) {
var $step39 = 0;
var $ans43 = D;
var $al44 = L[7];
try {
if(R.isActivationRecord($self41)) {
$step39 = $self41.step;
$al44 = $self41.from;
$ans43 = $self41.ans;
self41 = $self41.args[0];
$underscore42 = $self41.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[7],2,$t);
}
var self41 = $self41;
var $underscore42 = $$underscore42;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step39) {
case 0: var anf_arg45 = G(self41,"module-name",L[5]);
$step39 = 1;
$al44 = L[6];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al44,_plus1);
}
$ans43 = _plus1.app(("<builtin "),anf_arg45);
break;
case 1: var anf_arg46 = $ans43;
$step39 = 2;
$al44 = L[6];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al44,_plus1);
}
$ans43 = _plus1.app(anf_arg46,(">"));
break;
case 2: ++R.GAS;
return $ans43;
default: throw "No case numbered " + $step39 + " in $temp_full40";
}
}
} catch($e47) {
if(R.isCont($e47) && ($step39 !== 2)) {
$e47.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al44,$temp_full40,$step39,[self41,$underscore42],[]);
}
if(R.isPyretException($e47)) {
$e47.pyretStack.push($al44);
}
throw $e47;
}
};
var anf_variant_member221 = R.makeMethod1($temp_full40);
var $temp_full49 = function($self50) {
var $step48 = 0;
var $ans51 = D;
var $al52 = L[9];
try {
if(R.isActivationRecord($self50)) {
$step48 = $self50.step;
$al52 = $self50.from;
$ans51 = $self50.ans;
self50 = $self50.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[9],1,$t);
}
var self50 = $self50;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step48) {
case 0: $step48 = 1;
$ans51 = G(self50,"module-name",L[8]);
break;
case 1: ++R.GAS;
return $ans51;
default: throw "No case numbered " + $step48 + " in $temp_full49";
}
}
} catch($e53) {
if(R.isCont($e53) && ($step48 !== 1)) {
$e53.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al52,$temp_full49,$step48,[self50],[]);
}
if(R.isPyretException($e53)) {
$e53.pyretStack.push($al52);
}
throw $e53;
}
};
var anf_variant_member222 = R.makeMethod0($temp_full49);
var $temp_full55 = function($self56,$other57) {
var $step54 = 0;
var $ans58 = D;
var $al59 = L[16];
try {
if(R.isActivationRecord($self56)) {
$step54 = $self56.step;
$al59 = $self56.from;
$ans58 = $self56.ans;
self56 = $self56.args[0];
other57 = $self56.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[16],2,$t);
}
var self56 = $self56;
var other57 = $other57;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step54) {
case 0: $step54 = 1;
$al59 = L[15];
if(!(R.isFunction(is$builtin60.$var === D?U(L[10],"is-builtin"):is$builtin60.$var))) {
R.ffi.throwNonFunApp($al59,is$builtin60.$var === D?U(L[10],"is-builtin"):is$builtin60.$var);
}
$ans58 = is$builtin60.$var === D?U(L[10],"is-builtin"):is$builtin60.$var.app(other57);
break;
case 1: var anf_arg61 = $ans58;
$al59 = L[11];
var anf_if65 = R.checkWrapBoolean(anf_arg61);
if(R.isPyretTrue(anf_if65)) {
$step54 = 2;
} else {
$step54 = 4;
}
break;
case 2: var anf_arg62 = G(other57,"module-name",L[12]);
var anf_arg63 = G(self56,"module-name",L[13]);
$step54 = 3;
$al59 = L[14];
if(!(R.isFunction(equal$always2))) {
R.ffi.throwNonFunApp($al59,equal$always2);
}
$ans58 = equal$always2.app(anf_arg62,anf_arg63);
break;
case 3: var anf_arg64 = $ans58;
$step54 = 5;
$al59 = L[11];
$ans58 = R.checkWrapBoolean(anf_arg64);
break;
case 4: $step54 = 5;
$ans58 = (false);
break;
case 5: ++R.GAS;
return $ans58;
default: throw "No case numbered " + $step54 + " in $temp_full55";
}
}
} catch($e66) {
if(R.isCont($e66) && ($step54 !== 5)) {
$e66.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al59,$temp_full55,$step54,[self56,other57],[]);
}
if(R.isPyretException($e66)) {
$e66.pyretStack.push($al59);
}
throw $e66;
}
};
var anf_variant_member223 = R.makeMethod1($temp_full55);
var $temp_full68 = function($self69,$other70) {
var $step67 = 0;
var $ans71 = D;
var $al72 = L[24];
try {
if(R.isActivationRecord($self69)) {
$step67 = $self69.step;
$al72 = $self69.from;
$ans71 = $self69.ans;
self69 = $self69.args[0];
other70 = $self69.args[1];
cases73 = $self69.vars[0];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[24],2,$t);
}
var self69 = $self69;
var other70 = $other70;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step67) {
case 0: var cases73 = other70;
$step67 = 1;
$al72 = L[23];
R._checkAnn(L[23],Srcloc86,cases73);
break;
case 1: var $cases_dispatch85 = {"builtin":2,
"srcloc":3};
$al72 = L[22];
$step67 = $cases_dispatch85[cases73.$name] || 4;
break;
case 2: if(cases73.$arity >= 0) {
if(1 !== cases73.$arity) {
R.ffi.throwCasesArityErrorC(L[20],1,cases73.$arity);
}
} else {
R.ffi.throwCasesSingletonErrorC(L[20],true);
}
var $fn76 = cases73.$constructor.$fieldNames;
R.derefField(cases73.dict[$fn76[0]],cases73.$mut_fields_mask[0],false);
var anf_arg74 = G(self69,"module-name",L[17]);
var anf_arg75 = G(other70,"module-name",L[18]);
$step67 = 5;
$al72 = L[19];
if(!(R.isFunction(_lessthan3))) {
R.ffi.throwNonFunApp($al72,_lessthan3);
}
$ans71 = _lessthan3.app(anf_arg74,anf_arg75);
break;
case 3: if(cases73.$arity >= 0) {
if(7 !== cases73.$arity) {
R.ffi.throwCasesArityErrorC(L[21],7,cases73.$arity);
}
} else {
R.ffi.throwCasesSingletonErrorC(L[21],true);
}
var $fn76 = cases73.$constructor.$fieldNames;
R.derefField(cases73.dict[$fn76[0]],cases73.$mut_fields_mask[0],false);
R.derefField(cases73.dict[$fn76[1]],cases73.$mut_fields_mask[1],false);
R.derefField(cases73.dict[$fn76[2]],cases73.$mut_fields_mask[2],false);
R.derefField(cases73.dict[$fn76[3]],cases73.$mut_fields_mask[3],false);
R.derefField(cases73.dict[$fn76[4]],cases73.$mut_fields_mask[4],false);
R.derefField(cases73.dict[$fn76[5]],cases73.$mut_fields_mask[5],false);
R.derefField(cases73.dict[$fn76[6]],cases73.$mut_fields_mask[6],false);
$step67 = 5;
$ans71 = (false);
break;
case 4: $step67 = 5;
$al72 = L[22];
$ans71 = R.throwNoCasesMatched(L[22],cases73);
break;
case 5: ++R.GAS;
return $ans71;
default: throw "No case numbered " + $step67 + " in $temp_full68";
}
}
} catch($e87) {
if(R.isCont($e87) && ($step67 !== 5)) {
$e87.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al72,$temp_full68,$step67,[self69,other70],[cases73]);
}
if(R.isPyretException($e87)) {
$e87.pyretStack.push($al72);
}
throw $e87;
}
};
var anf_variant_member224 = R.makeMethod1($temp_full68);
var $temp_full89 = function($self90) {
var $step88 = 0;
var $ans91 = D;
var $al92 = L[25];
try {
if(R.isActivationRecord($self90)) {
$step88 = $self90.step;
$al92 = $self90.from;
$ans91 = $self90.ans;
self90 = $self90.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[25],1,$t);
}
var self90 = $self90;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step88) {
case 0: $step88 = 1;
$ans91 = (true);
break;
case 1: ++R.GAS;
return $ans91;
default: throw "No case numbered " + $step88 + " in $temp_full89";
}
}
} catch($e93) {
if(R.isCont($e93) && ($step88 !== 1)) {
$e93.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al92,$temp_full89,$step88,[self90],[]);
}
if(R.isPyretException($e93)) {
$e93.pyretStack.push($al92);
}
throw $e93;
}
};
var anf_variant_member225 = R.makeMethod0($temp_full89);
var $temp_full95 = function($self96,$show$file97) {
var $step94 = 0;
var $ans98 = D;
var $al99 = L[34];
try {
if(R.isActivationRecord($self96)) {
$step94 = $self96.step;
$al99 = $self96.from;
$ans98 = $self96.ans;
self96 = $self96.args[0];
show$file97 = $self96.args[1];
anf_arg107 = $self96.vars[0];
anf_arg103 = $self96.vars[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[34],2,$t);
}
var self96 = $self96;
var show$file97 = $show$file97;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step94) {
case 0: $al99 = L[26];
var anf_if109 = R.checkWrapBoolean(show$file97);
if(R.isPyretTrue(anf_if109)) {
$step94 = 1;
} else {
$step94 = 2;
}
break;
case 1: var anf_arg100 = G(self96,"source",L[27]);
$step94 = 3;
$al99 = L[28];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al99,_plus1);
}
$ans98 = _plus1.app(anf_arg100,(": "));
break;
case 2: $step94 = 3;
$ans98 = ("");
break;
case 3: var anf_arg101 = $ans98;
$step94 = 4;
$al99 = L[31];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al99,_plus1);
}
$ans98 = _plus1.app(anf_arg101,("line "));
break;
case 4: var anf_arg103 = $ans98;
var anf_arg102 = G(self96,"start-line",L[29]);
$step94 = 5;
$al99 = L[33];
if(!(R.isFunction(tostring4))) {
R.ffi.throwNonFunApp($al99,tostring4);
}
$ans98 = tostring4.app(anf_arg102);
break;
case 5: var anf_arg104 = $ans98;
$step94 = 6;
$al99 = L[31];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al99,_plus1);
}
$ans98 = _plus1.app(anf_arg103,anf_arg104);
break;
case 6: var anf_arg105 = $ans98;
$step94 = 7;
$al99 = L[31];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al99,_plus1);
}
$ans98 = _plus1.app(anf_arg105,(", column "));
break;
case 7: var anf_arg107 = $ans98;
var anf_arg106 = G(self96,"start-column",L[30]);
$step94 = 8;
$al99 = L[32];
if(!(R.isFunction(tostring4))) {
R.ffi.throwNonFunApp($al99,tostring4);
}
$ans98 = tostring4.app(anf_arg106);
break;
case 8: var anf_arg108 = $ans98;
$step94 = 9;
$al99 = L[31];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al99,_plus1);
}
$ans98 = _plus1.app(anf_arg107,anf_arg108);
break;
case 9: ++R.GAS;
return $ans98;
default: throw "No case numbered " + $step94 + " in $temp_full95";
}
}
} catch($e110) {
if(R.isCont($e110) && ($step94 !== 9)) {
$e110.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al99,$temp_full95,$step94,[self96,show$file97],[anf_arg107,anf_arg103]);
}
if(R.isPyretException($e110)) {
$e110.pyretStack.push($al99);
}
throw $e110;
}
};
var anf_variant_member235 = R.makeMethod1($temp_full95);
var $temp_full112 = function($self113) {
var $step111 = 0;
var $ans114 = D;
var $al115 = L[41];
try {
if(R.isActivationRecord($self113)) {
$step111 = $self113.step;
$al115 = $self113.from;
$ans114 = $self113.ans;
self113 = $self113.args[0];
anf_arg122 = $self113.vars[0];
anf_arg118 = $self113.vars[1];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[41],1,$t);
}
var self113 = $self113;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step111) {
case 0: var anf_arg116 = G(self113,"source",L[35]);
$step111 = 1;
$al115 = L[38];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al115,_plus1);
}
$ans114 = _plus1.app(anf_arg116,(":"));
break;
case 1: var anf_arg118 = $ans114;
var anf_arg117 = G(self113,"start-char",L[36]);
$step111 = 2;
$al115 = L[40];
if(!(R.isFunction(tostring4))) {
R.ffi.throwNonFunApp($al115,tostring4);
}
$ans114 = tostring4.app(anf_arg117);
break;
case 2: var anf_arg119 = $ans114;
$step111 = 3;
$al115 = L[38];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al115,_plus1);
}
$ans114 = _plus1.app(anf_arg118,anf_arg119);
break;
case 3: var anf_arg120 = $ans114;
$step111 = 4;
$al115 = L[38];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al115,_plus1);
}
$ans114 = _plus1.app(anf_arg120,("-"));
break;
case 4: var anf_arg122 = $ans114;
var anf_arg121 = G(self113,"end-char",L[37]);
$step111 = 5;
$al115 = L[39];
if(!(R.isFunction(tostring4))) {
R.ffi.throwNonFunApp($al115,tostring4);
}
$ans114 = tostring4.app(anf_arg121);
break;
case 5: var anf_arg123 = $ans114;
$step111 = 6;
$al115 = L[38];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al115,_plus1);
}
$ans114 = _plus1.app(anf_arg122,anf_arg123);
break;
case 6: ++R.GAS;
return $ans114;
default: throw "No case numbered " + $step111 + " in $temp_full112";
}
}
} catch($e124) {
if(R.isCont($e124) && ($step111 !== 6)) {
$e124.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al115,$temp_full112,$step111,[self113],[anf_arg122,anf_arg118]);
}
if(R.isPyretException($e124)) {
$e124.pyretStack.push($al115);
}
throw $e124;
}
};
var anf_variant_member236 = R.makeMethod0($temp_full112);
var $temp_full126 = function($self127,$other128) {
var $step125 = 0;
var $ans129 = D;
var $al130 = L[49];
try {
if(R.isActivationRecord($self127)) {
$step125 = $self127.step;
$al130 = $self127.from;
$ans129 = $self127.ans;
self127 = $self127.args[0];
other128 = $self127.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[49],2,$t);
}
var self127 = $self127;
var other128 = $other128;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step125) {
case 0: $step125 = 1;
$al130 = L[48];
R._checkAnn(L[48],Srcloc86,other128);
break;
case 1: $step125 = 2;
$al130 = L[47];
if(!(R.isFunction(is$srcloc131.$var === D?U(L[42],"is-srcloc"):is$srcloc131.$var))) {
R.ffi.throwNonFunApp($al130,is$srcloc131.$var === D?U(L[42],"is-srcloc"):is$srcloc131.$var);
}
$ans129 = is$srcloc131.$var === D?U(L[42],"is-srcloc"):is$srcloc131.$var.app(other128);
break;
case 2: var anf_arg132 = $ans129;
$al130 = L[43];
var anf_if136 = R.checkWrapBoolean(anf_arg132);
if(R.isPyretTrue(anf_if136)) {
$step125 = 3;
} else {
$step125 = 5;
}
break;
case 3: var anf_arg133 = G(self127,"source",L[44]);
var anf_arg134 = G(other128,"source",L[45]);
$step125 = 4;
$al130 = L[46];
if(!(R.isFunction(equal$always2))) {
R.ffi.throwNonFunApp($al130,equal$always2);
}
$ans129 = equal$always2.app(anf_arg133,anf_arg134);
break;
case 4: var anf_arg135 = $ans129;
$step125 = 6;
$al130 = L[43];
$ans129 = R.checkWrapBoolean(anf_arg135);
break;
case 5: $step125 = 6;
$ans129 = (false);
break;
case 6: ++R.GAS;
return $ans129;
default: throw "No case numbered " + $step125 + " in $temp_full126";
}
}
} catch($e137) {
if(R.isCont($e137) && ($step125 !== 6)) {
$e137.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al130,$temp_full126,$step125,[self127,other128],[]);
}
if(R.isPyretException($e137)) {
$e137.pyretStack.push($al130);
}
throw $e137;
}
};
var anf_variant_member237 = R.makeMethod1($temp_full126);
var $temp_full139 = function($self140,$other141) {
var $step138 = 0;
var $ans142 = D;
var $al143 = L[57];
try {
if(R.isActivationRecord($self140)) {
$step138 = $self140.step;
$al143 = $self140.from;
$ans142 = $self140.ans;
self140 = $self140.args[0];
other141 = $self140.args[1];
cases144 = $self140.vars[0];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[57],2,$t);
}
var self140 = $self140;
var other141 = $other141;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step138) {
case 0: $step138 = 1;
$al143 = L[56];
R._checkAnn(L[56],Srcloc86,other141);
break;
case 1: var cases144 = other141;
$step138 = 2;
$al143 = L[55];
R._checkAnn(L[55],Srcloc86,cases144);
break;
case 2: var $cases_dispatch148 = {"builtin":3};
$al143 = L[54];
$step138 = $cases_dispatch148[cases144.$name] || 4;
break;
case 3: if(cases144.$arity >= 0) {
if(1 !== cases144.$arity) {
R.ffi.throwCasesArityErrorC(L[50],1,cases144.$arity);
}
} else {
R.ffi.throwCasesSingletonErrorC(L[50],true);
}
var $fn76 = cases144.$constructor.$fieldNames;
R.derefField(cases144.dict[$fn76[0]],cases144.$mut_fields_mask[0],false);
$step138 = 5;
$ans142 = (true);
break;
case 4: var anf_arg146 = G(self140,"start-char",L[51]);
var anf_arg147 = G(other141,"start-char",L[52]);
$step138 = 5;
$al143 = L[53];
if(!(R.isFunction(_lessthan3))) {
R.ffi.throwNonFunApp($al143,_lessthan3);
}
$ans142 = _lessthan3.app(anf_arg146,anf_arg147);
break;
case 5: ++R.GAS;
return $ans142;
default: throw "No case numbered " + $step138 + " in $temp_full139";
}
}
} catch($e149) {
if(R.isCont($e149) && ($step138 !== 5)) {
$e149.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al143,$temp_full139,$step138,[self140,other141],[cases144]);
}
if(R.isPyretException($e149)) {
$e149.pyretStack.push($al143);
}
throw $e149;
}
};
var anf_variant_member238 = R.makeMethod1($temp_full139);
var $temp_full151 = function($self152) {
var $step150 = 0;
var $ans153 = D;
var $al154 = L[67];
try {
if(R.isActivationRecord($self152)) {
$step150 = $self152.step;
$al154 = $self152.from;
$ans153 = $self152.ans;
self152 = $self152.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[67],1,$t);
}
var self152 = $self152;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step150) {
case 0: var anf_arg156 = G(self152,"source",L[58]);
var anf_arg157 = G(self152,"start-line",L[59]);
var anf_arg158 = G(self152,"start-column",L[60]);
var anf_arg159 = G(self152,"start-char",L[61]);
var anf_arg160 = G(self152,"start-line",L[62]);
var anf_arg161 = G(self152,"start-column",L[63]);
var anf_arg162 = G(self152,"start-char",L[64]);
$step150 = 1;
$al154 = L[66];
if(!(R.isFunction(srcloc155.$var === D?U(L[65],"srcloc"):srcloc155.$var))) {
R.ffi.throwNonFunApp($al154,srcloc155.$var === D?U(L[65],"srcloc"):srcloc155.$var);
}
$ans153 = srcloc155.$var === D?U(L[65],"srcloc"):srcloc155.$var.app(anf_arg156,anf_arg157,anf_arg158,anf_arg159,anf_arg160,anf_arg161,anf_arg162);
break;
case 1: ++R.GAS;
return $ans153;
default: throw "No case numbered " + $step150 + " in $temp_full151";
}
}
} catch($e163) {
if(R.isCont($e163) && ($step150 !== 1)) {
$e163.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al154,$temp_full151,$step150,[self152],[]);
}
if(R.isPyretException($e163)) {
$e163.pyretStack.push($al154);
}
throw $e163;
}
};
var anf_variant_member239 = R.makeMethod0($temp_full151);
var $temp_full165 = function($self166) {
var $step164 = 0;
var $ans167 = D;
var $al168 = L[77];
try {
if(R.isActivationRecord($self166)) {
$step164 = $self166.step;
$al168 = $self166.from;
$ans167 = $self166.ans;
self166 = $self166.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[77],1,$t);
}
var self166 = $self166;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step164) {
case 0: var anf_arg169 = G(self166,"source",L[68]);
var anf_arg170 = G(self166,"end-line",L[69]);
var anf_arg171 = G(self166,"end-column",L[70]);
var anf_arg172 = G(self166,"end-char",L[71]);
var anf_arg173 = G(self166,"end-line",L[72]);
var anf_arg174 = G(self166,"end-column",L[73]);
var anf_arg175 = G(self166,"end-char",L[74]);
$step164 = 1;
$al168 = L[76];
if(!(R.isFunction(srcloc155.$var === D?U(L[75],"srcloc"):srcloc155.$var))) {
R.ffi.throwNonFunApp($al168,srcloc155.$var === D?U(L[75],"srcloc"):srcloc155.$var);
}
$ans167 = srcloc155.$var === D?U(L[75],"srcloc"):srcloc155.$var.app(anf_arg169,anf_arg170,anf_arg171,anf_arg172,anf_arg173,anf_arg174,anf_arg175);
break;
case 1: ++R.GAS;
return $ans167;
default: throw "No case numbered " + $step164 + " in $temp_full165";
}
}
} catch($e176) {
if(R.isCont($e176) && ($step164 !== 1)) {
$e176.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al168,$temp_full165,$step164,[self166],[]);
}
if(R.isPyretException($e176)) {
$e176.pyretStack.push($al168);
}
throw $e176;
}
};
var anf_variant_member240 = R.makeMethod0($temp_full165);
var $temp_full178 = function($self179,$other180) {
var $step177 = 0;
var $ans181 = D;
var $al182 = L[107];
try {
if(R.isActivationRecord($self179)) {
$step177 = $self179.step;
$al182 = $self179.from;
$ans181 = $self179.ans;
self179 = $self179.args[0];
other180 = $self179.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[107],2,$t);
}
var self179 = $self179;
var other180 = $other180;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step177) {
case 0: $step177 = 1;
$al182 = L[106];
R._checkAnn(L[106],R.makePredAnn(Srcloc86,is$srcloc131.$var === D?U(L[105],"is-srcloc"):is$srcloc131.$var,"is-srcloc"),other180);
break;
case 1: var anf_arg183 = G(self179,"start-char",L[78]);
var anf_arg184 = G(other180,"start-char",L[79]);
$step177 = 2;
$al182 = L[80];
if(!(R.isFunction(_lessequal5))) {
R.ffi.throwNonFunApp($al182,_lessequal5);
}
$ans181 = _lessequal5.app(anf_arg183,anf_arg184);
break;
case 2: var anf_arg185 = $ans181;
$al182 = L[80];
var anf_if208 = R.checkWrapBoolean(anf_arg185);
if(R.isPyretTrue(anf_if208)) {
$step177 = 3;
} else {
$step177 = 7;
}
break;
case 3: var anf_arg186 = G(self179,"end-char",L[81]);
var anf_arg187 = G(other180,"end-char",L[82]);
$step177 = 4;
$al182 = L[83];
if(!(R.isFunction(_greaterequal7))) {
R.ffi.throwNonFunApp($al182,_greaterequal7);
}
$ans181 = _greaterequal7.app(anf_arg186,anf_arg187);
break;
case 4: var anf_arg188 = $ans181;
$al182 = L[83];
var anf_if196 = R.checkWrapBoolean(anf_arg188);
if(R.isPyretTrue(anf_if196)) {
$step177 = 5;
} else {
$step177 = 6;
}
break;
case 5: $step177 = 11;
$ans181 = self179;
break;
case 6: var anf_arg189 = G(self179,"source",L[84]);
var anf_arg190 = G(self179,"start-line",L[85]);
var anf_arg191 = G(self179,"start-column",L[86]);
var anf_arg192 = G(self179,"start-char",L[87]);
var anf_arg193 = G(other180,"end-line",L[88]);
var anf_arg194 = G(other180,"end-column",L[89]);
var anf_arg195 = G(other180,"end-char",L[90]);
$step177 = 11;
$al182 = L[92];
if(!(R.isFunction(srcloc155.$var === D?U(L[91],"srcloc"):srcloc155.$var))) {
R.ffi.throwNonFunApp($al182,srcloc155.$var === D?U(L[91],"srcloc"):srcloc155.$var);
}
$ans181 = srcloc155.$var === D?U(L[91],"srcloc"):srcloc155.$var.app(anf_arg189,anf_arg190,anf_arg191,anf_arg192,anf_arg193,anf_arg194,anf_arg195);
break;
case 7: var anf_arg197 = G(self179,"end-char",L[93]);
var anf_arg198 = G(other180,"end-char",L[94]);
$step177 = 8;
$al182 = L[95];
if(!(R.isFunction(_greaterthan6))) {
R.ffi.throwNonFunApp($al182,_greaterthan6);
}
$ans181 = _greaterthan6.app(anf_arg197,anf_arg198);
break;
case 8: var anf_arg199 = $ans181;
$al182 = L[95];
var anf_if207 = R.checkWrapBoolean(anf_arg199);
if(R.isPyretTrue(anf_if207)) {
$step177 = 9;
} else {
$step177 = 10;
}
break;
case 9: var anf_arg200 = G(self179,"source",L[96]);
var anf_arg201 = G(other180,"start-line",L[97]);
var anf_arg202 = G(other180,"start-column",L[98]);
var anf_arg203 = G(other180,"start-char",L[99]);
var anf_arg204 = G(self179,"end-line",L[100]);
var anf_arg205 = G(self179,"end-column",L[101]);
var anf_arg206 = G(self179,"end-char",L[102]);
$step177 = 11;
$al182 = L[104];
if(!(R.isFunction(srcloc155.$var === D?U(L[103],"srcloc"):srcloc155.$var))) {
R.ffi.throwNonFunApp($al182,srcloc155.$var === D?U(L[103],"srcloc"):srcloc155.$var);
}
$ans181 = srcloc155.$var === D?U(L[103],"srcloc"):srcloc155.$var.app(anf_arg200,anf_arg201,anf_arg202,anf_arg203,anf_arg204,anf_arg205,anf_arg206);
break;
case 10: $step177 = 11;
$ans181 = other180;
break;
case 11: ++R.GAS;
return $ans181;
default: throw "No case numbered " + $step177 + " in $temp_full178";
}
}
} catch($e209) {
if(R.isCont($e209) && ($step177 !== 11)) {
$e209.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al182,$temp_full178,$step177,[self179,other180],[]);
}
if(R.isPyretException($e209)) {
$e209.pyretStack.push($al182);
}
throw $e209;
}
};
var anf_variant_member241 = R.makeMethod1($temp_full178);
var $temp_full211 = function($self212) {
var $step210 = 0;
var $ans213 = D;
var $al214 = L[108];
try {
if(R.isActivationRecord($self212)) {
$step210 = $self212.step;
$al214 = $self212.from;
$ans213 = $self212.ans;
self212 = $self212.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[108],1,$t);
}
var self212 = $self212;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step210) {
case 0: $step210 = 1;
$ans213 = (false);
break;
case 1: ++R.GAS;
return $ans213;
default: throw "No case numbered " + $step210 + " in $temp_full211";
}
}
} catch($e215) {
if(R.isCont($e215) && ($step210 !== 1)) {
$e215.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al214,$temp_full211,$step210,[self212],[]);
}
if(R.isPyretException($e215)) {
$e215.pyretStack.push($al214);
}
throw $e215;
}
};
var anf_variant_member242 = R.makeMethod0($temp_full211);
var $builtin_getfields228 = function(f) {
return f(this.dict["module-name"]);
};
var $builtin_getfieldsref226 = function(f,refmask) {
return f(R.derefField(this.dict["module-name"],false,refmask[0]));
};
var $builtin_mutablemask227 = [false];
var $builtin$base218 = {"$fieldNames":["module-name"],
"_output":anf_shared216,
"after":anf_shared217,
"format":anf_variant_member221,
"key":anf_variant_member222,
"same-file":anf_variant_member223,
"before":anf_variant_member224,
"is-builtin":anf_variant_member225,
"_match":R.makeMatch("builtin",1)};
var $builtin$brands220 = {"$brand$builtin":true};
$builtin$brands220[Srcloc21._brand] = true;
var builtin230 = R.makeVariantConstructor(L[110],function() {
return [];
},[],[],[false],["module$name231"],$builtin_mutablemask227,$builtin$base218,$builtin$brands220,"builtin",$builtin_getfieldsref226,$builtin_getfields228,$builtin$base218);
var $srcloc_getfields245 = function(f) {
return f(this.dict["source"],this.dict["start-line"],this.dict["start-column"],this.dict["start-char"],this.dict["end-line"],this.dict["end-column"],this.dict["end-char"]);
};
var $srcloc_getfieldsref243 = function(f,refmask) {
return f(R.derefField(this.dict["source"],false,refmask[0]),R.derefField(this.dict["start-line"],false,refmask[1]),R.derefField(this.dict["start-column"],false,refmask[2]),R.derefField(this.dict["start-char"],false,refmask[3]),R.derefField(this.dict["end-line"],false,refmask[4]),R.derefField(this.dict["end-column"],false,refmask[5]),R.derefField(this.dict["end-char"],false,refmask[6]));
};
var $srcloc_mutablemask244 = [false,false,false,false,false,false,false];
var $srcloc$base232 = {"$fieldNames":["source","start-line","start-column","start-char","end-line","end-column","end-char"],
"_output":anf_shared216,
"after":anf_shared217,
"format":anf_variant_member235,
"key":anf_variant_member236,
"same-file":anf_variant_member237,
"before":anf_variant_member238,
"at-start":anf_variant_member239,
"at-end":anf_variant_member240,
"_plus":anf_variant_member241,
"is-builtin":anf_variant_member242,
"_match":R.makeMatch("srcloc",7)};
var $srcloc$brands234 = {"$brand$srcloc":true};
$srcloc$brands234[Srcloc21._brand] = true;
var srcloc247 = R.makeVariantConstructor(L[119],function() {
return [$type$String9,$type$Number8,$type$Number8,$type$Number8,$type$Number8,$type$Number8,$type$Number8];
},["source248","start$line249","start$column250","start$char251","end$line252","end$column253","end$char254"],[L[112],L[113],L[114],L[115],L[116],L[117],L[118]],[false,false,false,false,false,false,false],["source248","start$line249","start$column250","start$char251","end$line252","end$column253","end$char254"],$srcloc_mutablemask244,$srcloc$base232,$srcloc$brands234,"srcloc",$srcloc_getfieldsref243,$srcloc_getfields245,$srcloc$base232);
var anf_assign256 = R.makeObject({"Srcloc":R.makeFunction(function($val255) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[120],1,$t);
}
return R.makeBoolean(R.hasBrand($val255,Srcloc21._brand));
}),
"is-builtin":R.makeFunction(function($val229) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[109],1,$t);
}
return R.makeBoolean(R.hasBrand($val229,"$brand$builtin"));
}),
"builtin":builtin230,
"is-srcloc":R.makeFunction(function($val246) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[111],1,$t);
}
return R.makeBoolean(R.hasBrand($val246,"$brand$srcloc"));
}),
"srcloc":srcloc247});
Srcloc257.$var = anf_assign256;
var anf_assign258 = G(Srcloc257.$var,"Srcloc",L[120]);
Srcloc259.$var = anf_assign258;
var anf_assign260 = G(Srcloc257.$var,"Srcloc",L[120]);
is$Srcloc261.$var = anf_assign260;
var anf_assign262 = G(Srcloc257.$var,"is-builtin",L[109]);
is$builtin60.$var = anf_assign262;
var anf_assign263 = G(Srcloc257.$var,"builtin",L[109]);
builtin264.$var = anf_assign263;
var anf_assign265 = G(Srcloc257.$var,"is-srcloc",L[111]);
is$srcloc131.$var = anf_assign265;
var anf_assign266 = G(Srcloc257.$var,"srcloc",L[111]);
srcloc155.$var = anf_assign266;
var provides270 = R.makeObject({"Srcloc":Srcloc259.$var,
"is-Srcloc":is$Srcloc261.$var,
"builtin":builtin264.$var,
"is-builtin":is$builtin60.$var,
"srcloc":srcloc155.$var,
"is-srcloc":is$srcloc131.$var});
$step16 = 1;
$al20 = L[120];
$field267 = R.getColonFieldLoc(builtins10,"current-checker",L[120]);
if(R.isMethod($field267)) {
$ans19 = $field267.full_meth(builtins10);
} else {
if(!(R.isFunction($field267))) {
R.ffi.throwNonFunApp(L[120],$field267);
}
$ans19 = $field267.app();
}
break;
case 1: var anf_method_obj268 = $ans19;
$step16 = 2;
$al20 = L[120];
$field269 = R.getColonFieldLoc(anf_method_obj268,"results",L[120]);
if(R.isMethod($field269)) {
$ans19 = $field269.full_meth(anf_method_obj268);
} else {
if(!(R.isFunction($field269))) {
R.ffi.throwNonFunApp(L[120],$field269);
}
$ans19 = $field269.app();
}
break;
case 2: var checks271 = $ans19;
$step16 = 3;
$ans19 = R.makeObject({"answer":nothing11,
"namespace":NAMESPACE,
"defined-values":{"srcloc":srcloc155.$var,
"is-srcloc":is$srcloc131.$var,
"builtin":builtin264.$var,
"is-builtin":is$builtin60.$var,
"is-Srcloc":is$Srcloc261.$var,
"Srcloc":Srcloc259.$var,
"VS":VS12},
"defined-types":{"Srcloc":Srcloc86,
"VS":VS13},
"provide-plus-types":R.makeObject({"values":provides270,
"types":{"Srcloc":Srcloc86}}),
"checks":checks271});
break;
case 3: ++R.GAS;
return $ans19;
default: throw "No case numbered " + $step16 + " in $toplevel17";
}
}
} catch($e273) {
if(R.isCont($e273) && ($step16 !== 3)) {
$e273.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al20,$toplevel17,$step16,[],[provides270,srcloc155,is$srcloc131,builtin264,is$builtin60,is$Srcloc261,Srcloc259,Srcloc86]);
}
if(R.isPyretException($e273)) {
$e273.pyretStack.push($al20);
}
throw $e273;
}
};
return R.safeCall($toplevel17,function(moduleVal) {
R.modules["$src/arr/base/srcloc.arr14"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
