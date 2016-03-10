({
"requires":[],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":
function(R,NAMESPACE,M) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,5,2,46,5,13,57],
[M,6,2,60,6,34,92],
[M,6,21,79,6,22,80],
[M,6,32,90,6,33,91],
[M,6,4,62,6,34,92],
[M,4,0,27,7,3,96],
[M,1,0,0,7,3,96]];
var builtins1 = NAMESPACE.get("builtins");
var nothing2 = NAMESPACE.get("nothing");
var $toplevel5 = function($$resumer48) {
var $step4 = 0;
var $ans7 = D;
var $al8 = L[6];
try {
if(R.isActivationRecord($$resumer48)) {
$step4 = $$resumer48.step;
$al8 = $$resumer48.from;
$ans7 = $$resumer48.ans;
$resumer48 = $$resumer48.args[0];
provides46 = $$resumer48.vars[0];
pick$some41 = $$resumer48.vars[1];
is$pick$some39 = $$resumer48.vars[2];
pick$none37 = $$resumer48.vars[3];
is$pick$none35 = $$resumer48.vars[4];
is$Pick33 = $$resumer48.vars[5];
Pick31 = $$resumer48.vars[6];
Pick45 = $$resumer48.vars[7];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step4) {
case 0: var Pick9 = R.namedBrander("Pick");
var Pick45 = R.makeBranderAnn(Pick9,"Pick");
var Pick29 = {"$var":D};
var Pick31 = {"$var":D};
var is$Pick33 = {"$var":D};
var is$pick$none35 = {"$var":D};
var pick$none37 = {"$var":D};
var is$pick$some39 = {"$var":D};
var pick$some41 = {"$var":D};
var $pick$none_getfields15 = function(f) {
return f();
};
var $pick$none_getfieldsref13 = function(f) {
return f();
};
var $pick$none_mutablemask14 = [];
var $pick$none$base10 = {"_match":R.makeMatch("pick-none",0)};
var $pick$none$brands12 = {"$brand$pick$none":true};
$pick$none$brands12[Pick9._brand] = true;
var $pick$some_getfields22 = function(f) {
return f(this.dict["elt"],this.dict["rest"]);
};
var $pick$some_getfieldsref20 = function(f,refmask) {
return f(R.derefField(this.dict["elt"],false,refmask[0]),R.derefField(this.dict["rest"],false,refmask[1]));
};
var $pick$some_mutablemask21 = [false,false];
var $pick$some$base17 = {"$fieldNames":["elt","rest"],
"_match":R.makeMatch("pick-some",2)};
var $pick$some$brands19 = {"$brand$pick$some":true};
$pick$some$brands19[Pick9._brand] = true;
var pick$some24 = R.makeVariantConstructor(L[4],function() {
return [R.Any,R.Any];
},["elt25","rest26"],[L[2],L[3]],[false,false],["elt25","rest26"],$pick$some_mutablemask21,$pick$some$base17,$pick$some$brands19,"pick-some",$pick$some_getfieldsref20,$pick$some_getfields22,$pick$some$base17);
var anf_assign28 = R.makeObject({"Pick":R.makeFunction(function($val27) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[5],1,$t);
}
return R.makeBoolean(R.hasBrand($val27,Pick9._brand));
}),
"is-pick-none":R.makeFunction(function($val16) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[0],1,$t);
}
return R.makeBoolean(R.hasBrand($val16,"$brand$pick$none"));
}),
"pick-none":R.makeDataValue($pick$none$base10,$pick$none$brands12,"pick-none",$pick$none_getfieldsref13,$pick$none_getfields15,-1,$pick$none_mutablemask14,$pick$none$base10),
"is-pick-some":R.makeFunction(function($val23) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[1],1,$t);
}
return R.makeBoolean(R.hasBrand($val23,"$brand$pick$some"));
}),
"pick-some":pick$some24});
Pick29.$var = anf_assign28;
var anf_assign30 = G(Pick29.$var,"Pick",L[5]);
Pick31.$var = anf_assign30;
var anf_assign32 = G(Pick29.$var,"Pick",L[5]);
is$Pick33.$var = anf_assign32;
var anf_assign34 = G(Pick29.$var,"is-pick-none",L[0]);
is$pick$none35.$var = anf_assign34;
var anf_assign36 = G(Pick29.$var,"pick-none",L[0]);
pick$none37.$var = anf_assign36;
var anf_assign38 = G(Pick29.$var,"is-pick-some",L[1]);
is$pick$some39.$var = anf_assign38;
var anf_assign40 = G(Pick29.$var,"pick-some",L[1]);
pick$some41.$var = anf_assign40;
var provides46 = R.makeObject({"Pick":Pick31.$var,
"is-Pick":is$Pick33.$var,
"pick-none":pick$none37.$var,
"is-pick-none":is$pick$none35.$var,
"pick-some":pick$some41.$var,
"is-pick-some":is$pick$some39.$var});
$step4 = 1;
$al8 = L[5];
$field42 = R.getColonFieldLoc(builtins1,"current-checker",L[5]);
if(R.isMethod($field42)) {
$ans7 = $field42.full_meth(builtins1);
} else {
if(!(R.isFunction($field42))) {
R.ffi.throwNonFunApp(L[5],$field42);
}
$ans7 = $field42.app();
}
break;
case 1: var anf_method_obj43 = $ans7;
$step4 = 2;
$al8 = L[5];
$field44 = R.getColonFieldLoc(anf_method_obj43,"results",L[5]);
if(R.isMethod($field44)) {
$ans7 = $field44.full_meth(anf_method_obj43);
} else {
if(!(R.isFunction($field44))) {
R.ffi.throwNonFunApp(L[5],$field44);
}
$ans7 = $field44.app();
}
break;
case 2: var checks47 = $ans7;
$step4 = 3;
$ans7 = R.makeObject({"answer":nothing2,
"namespace":NAMESPACE,
"defined-values":{"pick-some":pick$some41.$var,
"is-pick-some":is$pick$some39.$var,
"pick-none":pick$none37.$var,
"is-pick-none":is$pick$none35.$var,
"is-Pick":is$Pick33.$var,
"Pick":Pick31.$var},
"defined-types":{"Pick":Pick45},
"provide-plus-types":R.makeObject({"values":provides46,
"types":{"Pick":Pick45}}),
"checks":checks47});
break;
case 3: ++R.GAS;
return $ans7;
default: throw "No case numbered " + $step4 + " in $toplevel5";
}
}
} catch($e49) {
if(R.isCont($e49) && ($step4 !== 3)) {
$e49.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al8,$toplevel5,$step4,[],[provides46,pick$some41,is$pick$some39,pick$none37,is$pick$none35,is$Pick33,Pick31,Pick45]);
}
if(R.isPyretException($e49)) {
$e49.pyretStack.push($al8);
}
throw $e49;
}
};
return R.safeCall($toplevel5,function(moduleVal) {
R.modules["$src/arr/base/pick.arr3"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
