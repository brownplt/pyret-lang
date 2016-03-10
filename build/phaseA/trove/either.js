({
"requires":[],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule": function(R,NAMESPACE, M) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,7,2,60,7,16,74],
[M,7,14,72,7,15,73],
[M,7,4,62,7,16,74],
[M,8,2,77,8,17,92],
[M,8,15,90,8,16,91],
[M,8,4,79,8,17,92],
[M,6,0,40,9,3,96],
[M,3,0,13,9,3,96]];
var builtins1 = NAMESPACE.get("builtins");
var nothing2 = NAMESPACE.get("nothing");
var $toplevel5 = function($$resumer49) {
var $step4 = 0;
var $ans7 = D;
var $al8 = L[7];
try {
if(R.isActivationRecord($$resumer49)) {
$step4 = $$resumer49.step;
$al8 = $$resumer49.from;
$ans7 = $$resumer49.ans;
$resumer49 = $$resumer49.args[0];
provides47 = $$resumer49.vars[0];
right42 = $$resumer49.vars[1];
is$right40 = $$resumer49.vars[2];
left38 = $$resumer49.vars[3];
is$left36 = $$resumer49.vars[4];
is$Either34 = $$resumer49.vars[5];
Either32 = $$resumer49.vars[6];
Either46 = $$resumer49.vars[7];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step4) {
case 0: var Either9 = R.namedBrander("Either");
var Either46 = R.makeBranderAnn(Either9,"Either");
var Either30 = {"$var":D};
var Either32 = {"$var":D};
var is$Either34 = {"$var":D};
var is$left36 = {"$var":D};
var left38 = {"$var":D};
var is$right40 = {"$var":D};
var right42 = {"$var":D};
var $left_getfields15 = function(f) {
return f(this.dict["v"]);
};
var $left_getfieldsref13 = function(f,refmask) {
return f(R.derefField(this.dict["v"],false,refmask[0]));
};
var $left_mutablemask14 = [false];
var $left$base10 = {"$fieldNames":["v"],
"_match":R.makeMatch("left",1)};
var $left$brands12 = {"$brand$left":true};
$left$brands12[Either9._brand] = true;
var left17 = R.makeVariantConstructor(L[2],function() {
return [R.Any];
},["v18"],[L[1]],[false],["v18"],$left_mutablemask14,$left$base10,$left$brands12,"left",$left_getfieldsref13,$left_getfields15,$left$base10);
var $right_getfields24 = function(f) {
return f(this.dict["v"]);
};
var $right_getfieldsref22 = function(f,refmask) {
return f(R.derefField(this.dict["v"],false,refmask[0]));
};
var $right_mutablemask23 = [false];
var $right$base19 = {"$fieldNames":["v"],
"_match":R.makeMatch("right",1)};
var $right$brands21 = {"$brand$right":true};
$right$brands21[Either9._brand] = true;
var right26 = R.makeVariantConstructor(L[5],function() {
return [R.Any];
},["v27"],[L[4]],[false],["v27"],$right_mutablemask23,$right$base19,$right$brands21,"right",$right_getfieldsref22,$right_getfields24,$right$base19);
var anf_assign29 = R.makeObject({"Either":R.makeFunction(function($val28) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[6],1,$t);
}
return R.makeBoolean(R.hasBrand($val28,Either9._brand));
}),
"is-left":R.makeFunction(function($val16) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[0],1,$t);
}
return R.makeBoolean(R.hasBrand($val16,"$brand$left"));
}),
"left":left17,
"is-right":R.makeFunction(function($val25) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[3],1,$t);
}
return R.makeBoolean(R.hasBrand($val25,"$brand$right"));
}),
"right":right26});
Either30.$var = anf_assign29;
var anf_assign31 = G(Either30.$var,"Either",L[6]);
Either32.$var = anf_assign31;
var anf_assign33 = G(Either30.$var,"Either",L[6]);
is$Either34.$var = anf_assign33;
var anf_assign35 = G(Either30.$var,"is-left",L[0]);
is$left36.$var = anf_assign35;
var anf_assign37 = G(Either30.$var,"left",L[0]);
left38.$var = anf_assign37;
var anf_assign39 = G(Either30.$var,"is-right",L[3]);
is$right40.$var = anf_assign39;
var anf_assign41 = G(Either30.$var,"right",L[3]);
right42.$var = anf_assign41;
var provides47 = R.makeObject({"Either":Either32.$var,
"is-Either":is$Either34.$var,
"left":left38.$var,
"is-left":is$left36.$var,
"right":right42.$var,
"is-right":is$right40.$var});
$step4 = 1;
$al8 = L[6];
$field43 = R.getColonFieldLoc(builtins1,"current-checker",L[6]);
if(R.isMethod($field43)) {
$ans7 = $field43.full_meth(builtins1);
} else {
if(!(R.isFunction($field43))) {
R.ffi.throwNonFunApp(L[6],$field43);
}
$ans7 = $field43.app();
}
break;
case 1: var anf_method_obj44 = $ans7;
$step4 = 2;
$al8 = L[6];
$field45 = R.getColonFieldLoc(anf_method_obj44,"results",L[6]);
if(R.isMethod($field45)) {
$ans7 = $field45.full_meth(anf_method_obj44);
} else {
if(!(R.isFunction($field45))) {
R.ffi.throwNonFunApp(L[6],$field45);
}
$ans7 = $field45.app();
}
break;
case 2: var checks48 = $ans7;
$step4 = 3;
$ans7 = R.makeObject({"answer":nothing2,
"namespace":NAMESPACE,
"defined-values":{"right":right42.$var,
"is-right":is$right40.$var,
"left":left38.$var,
"is-left":is$left36.$var,
"is-Either":is$Either34.$var,
"Either":Either32.$var},
"defined-types":{"Either":Either46},
"provide-plus-types":R.makeObject({"values":provides47,
"types":{"Either":Either46}}),
"checks":checks48});
break;
case 3: ++R.GAS;
return $ans7;
default: throw "No case numbered " + $step4 + " in $toplevel5";
}
}
} catch($e50) {
if(R.isCont($e50) && ($step4 !== 3)) {
$e50.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al8,$toplevel5,$step4,[],[provides47,right42,is$right40,left38,is$left36,is$Either34,Either32,Either46]);
}
if(R.isPyretException($e50)) {
$e50.pyretStack.push($al8);
}
throw $e50;
}
};
return R.safeCall($toplevel5,function(moduleVal) {
R.modules["$src/arr/base/either.arr3"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
