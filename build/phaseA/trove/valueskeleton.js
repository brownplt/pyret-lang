({
"requires":[],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":
function(R,NAMESPACE, M) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,7,2,62,7,23,83],
[M,7,16,76,7,22,82],
[M,7,4,64,7,23,83],
[M,8,2,86,8,22,106],
[M,8,4,88,8,22,106],
[M,9,2,109,9,40,147],
[M,9,26,133,9,32,139],
[M,9,4,111,9,40,147],
[M,10,2,150,10,35,183],
[M,10,22,170,10,28,176],
[M,10,4,152,10,35,183],
[M,11,2,186,11,17,201],
[M,11,4,188,11,17,201],
[M,6,0,40,12,3,205],
[M,3,0,13,12,3,205]];
var $type$String1 = NAMESPACE.get("$type$String");
var builtins2 = NAMESPACE.get("builtins");
var nothing3 = NAMESPACE.get("nothing");
var $toplevel6 = function($$resumer91) {
var $step5 = 0;
var $ans8 = D;
var $al9 = L[14];
try {
if(R.isActivationRecord($$resumer91)) {
$step5 = $$resumer91.step;
$al9 = $$resumer91.from;
$ans8 = $$resumer91.ans;
$resumer91 = $$resumer91.args[0];
provides89 = $$resumer91.vars[0];
vs$seq84 = $$resumer91.vars[1];
is$vs$seq82 = $$resumer91.vars[2];
vs$constr80 = $$resumer91.vars[3];
is$vs$constr78 = $$resumer91.vars[4];
vs$collection76 = $$resumer91.vars[5];
is$vs$collection74 = $$resumer91.vars[6];
vs$value72 = $$resumer91.vars[7];
is$vs$value70 = $$resumer91.vars[8];
vs$str68 = $$resumer91.vars[9];
is$vs$str66 = $$resumer91.vars[10];
is$ValueSkeleton64 = $$resumer91.vars[11];
ValueSkeleton62 = $$resumer91.vars[12];
ValueSkeleton88 = $$resumer91.vars[13];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step5) {
case 0: var ValueSkeleton10 = R.namedBrander("ValueSkeleton");
var ValueSkeleton88 = R.makeBranderAnn(ValueSkeleton10,"ValueSkeleton");
var ValueSkeleton60 = {"$var":D};
var ValueSkeleton62 = {"$var":D};
var is$ValueSkeleton64 = {"$var":D};
var is$vs$str66 = {"$var":D};
var vs$str68 = {"$var":D};
var is$vs$value70 = {"$var":D};
var vs$value72 = {"$var":D};
var is$vs$collection74 = {"$var":D};
var vs$collection76 = {"$var":D};
var is$vs$constr78 = {"$var":D};
var vs$constr80 = {"$var":D};
var is$vs$seq82 = {"$var":D};
var vs$seq84 = {"$var":D};
var $vs$str_getfields16 = function(f) {
return f(this.dict["s"]);
};
var $vs$str_getfieldsref14 = function(f,refmask) {
return f(R.derefField(this.dict["s"],false,refmask[0]));
};
var $vs$str_mutablemask15 = [false];
var $vs$str$base11 = {"$fieldNames":["s"],
"_match":R.makeMatch("vs-str",1)};
var $vs$str$brands13 = {"$brand$vs$str":true};
$vs$str$brands13[ValueSkeleton10._brand] = true;
var vs$str18 = R.makeVariantConstructor(L[2],function() {
return [$type$String1];
},["s19"],[L[1]],[false],["s19"],$vs$str_mutablemask15,$vs$str$base11,$vs$str$brands13,"vs-str",$vs$str_getfieldsref14,$vs$str_getfields16,$vs$str$base11);
var $vs$value_getfields25 = function(f) {
return f(this.dict["v"]);
};
var $vs$value_getfieldsref23 = function(f,refmask) {
return f(R.derefField(this.dict["v"],false,refmask[0]));
};
var $vs$value_mutablemask24 = [false];
var $vs$value$base20 = {"$fieldNames":["v"],
"_match":R.makeMatch("vs-value",1)};
var $vs$value$brands22 = {"$brand$vs$value":true};
$vs$value$brands22[ValueSkeleton10._brand] = true;
var vs$value27 = R.makeVariantConstructor(L[4],function() {
return [];
},[],[],[false],["v28"],$vs$value_mutablemask24,$vs$value$base20,$vs$value$brands22,"vs-value",$vs$value_getfieldsref23,$vs$value_getfields25,$vs$value$base20);
var $vs$collection_getfields34 = function(f) {
return f(this.dict["name"],this.dict["items"]);
};
var $vs$collection_getfieldsref32 = function(f,refmask) {
return f(R.derefField(this.dict["name"],false,refmask[0]),R.derefField(this.dict["items"],false,refmask[1]));
};
var $vs$collection_mutablemask33 = [false,false];
var $vs$collection$base29 = {"$fieldNames":["name","items"],
"_match":R.makeMatch("vs-collection",2)};
var $vs$collection$brands31 = {"$brand$vs$collection":true};
$vs$collection$brands31[ValueSkeleton10._brand] = true;
var vs$collection36 = R.makeVariantConstructor(L[7],function() {
return [$type$String1];
},["name37"],[L[6]],[false,false],["name37","items38"],$vs$collection_mutablemask33,$vs$collection$base29,$vs$collection$brands31,"vs-collection",$vs$collection_getfieldsref32,$vs$collection_getfields34,$vs$collection$base29);
var $vs$constr_getfields44 = function(f) {
return f(this.dict["name"],this.dict["args"]);
};
var $vs$constr_getfieldsref42 = function(f,refmask) {
return f(R.derefField(this.dict["name"],false,refmask[0]),R.derefField(this.dict["args"],false,refmask[1]));
};
var $vs$constr_mutablemask43 = [false,false];
var $vs$constr$base39 = {"$fieldNames":["name","args"],
"_match":R.makeMatch("vs-constr",2)};
var $vs$constr$brands41 = {"$brand$vs$constr":true};
$vs$constr$brands41[ValueSkeleton10._brand] = true;
var vs$constr46 = R.makeVariantConstructor(L[10],function() {
return [$type$String1];
},["name47"],[L[9]],[false,false],["name47","args48"],$vs$constr_mutablemask43,$vs$constr$base39,$vs$constr$brands41,"vs-constr",$vs$constr_getfieldsref42,$vs$constr_getfields44,$vs$constr$base39);
var $vs$seq_getfields54 = function(f) {
return f(this.dict["items"]);
};
var $vs$seq_getfieldsref52 = function(f,refmask) {
return f(R.derefField(this.dict["items"],false,refmask[0]));
};
var $vs$seq_mutablemask53 = [false];
var $vs$seq$base49 = {"$fieldNames":["items"],
"_match":R.makeMatch("vs-seq",1)};
var $vs$seq$brands51 = {"$brand$vs$seq":true};
$vs$seq$brands51[ValueSkeleton10._brand] = true;
var vs$seq56 = R.makeVariantConstructor(L[12],function() {
return [];
},[],[],[false],["items57"],$vs$seq_mutablemask53,$vs$seq$base49,$vs$seq$brands51,"vs-seq",$vs$seq_getfieldsref52,$vs$seq_getfields54,$vs$seq$base49);
var anf_assign59 = R.makeObject({"ValueSkeleton":R.makeFunction(function($val58) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[13],1,$t);
}
return R.makeBoolean(R.hasBrand($val58,ValueSkeleton10._brand));
}),
"is-vs-str":R.makeFunction(function($val17) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[0],1,$t);
}
return R.makeBoolean(R.hasBrand($val17,"$brand$vs$str"));
}),
"vs-str":vs$str18,
"is-vs-value":R.makeFunction(function($val26) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[3],1,$t);
}
return R.makeBoolean(R.hasBrand($val26,"$brand$vs$value"));
}),
"vs-value":vs$value27,
"is-vs-collection":R.makeFunction(function($val35) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[5],1,$t);
}
return R.makeBoolean(R.hasBrand($val35,"$brand$vs$collection"));
}),
"vs-collection":vs$collection36,
"is-vs-constr":R.makeFunction(function($val45) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[8],1,$t);
}
return R.makeBoolean(R.hasBrand($val45,"$brand$vs$constr"));
}),
"vs-constr":vs$constr46,
"is-vs-seq":R.makeFunction(function($val55) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[11],1,$t);
}
return R.makeBoolean(R.hasBrand($val55,"$brand$vs$seq"));
}),
"vs-seq":vs$seq56});
ValueSkeleton60.$var = anf_assign59;
var anf_assign61 = G(ValueSkeleton60.$var,"ValueSkeleton",L[13]);
ValueSkeleton62.$var = anf_assign61;
var anf_assign63 = G(ValueSkeleton60.$var,"ValueSkeleton",L[13]);
is$ValueSkeleton64.$var = anf_assign63;
var anf_assign65 = G(ValueSkeleton60.$var,"is-vs-str",L[0]);
is$vs$str66.$var = anf_assign65;
var anf_assign67 = G(ValueSkeleton60.$var,"vs-str",L[0]);
vs$str68.$var = anf_assign67;
var anf_assign69 = G(ValueSkeleton60.$var,"is-vs-value",L[3]);
is$vs$value70.$var = anf_assign69;
var anf_assign71 = G(ValueSkeleton60.$var,"vs-value",L[3]);
vs$value72.$var = anf_assign71;
var anf_assign73 = G(ValueSkeleton60.$var,"is-vs-collection",L[5]);
is$vs$collection74.$var = anf_assign73;
var anf_assign75 = G(ValueSkeleton60.$var,"vs-collection",L[5]);
vs$collection76.$var = anf_assign75;
var anf_assign77 = G(ValueSkeleton60.$var,"is-vs-constr",L[8]);
is$vs$constr78.$var = anf_assign77;
var anf_assign79 = G(ValueSkeleton60.$var,"vs-constr",L[8]);
vs$constr80.$var = anf_assign79;
var anf_assign81 = G(ValueSkeleton60.$var,"is-vs-seq",L[11]);
is$vs$seq82.$var = anf_assign81;
var anf_assign83 = G(ValueSkeleton60.$var,"vs-seq",L[11]);
vs$seq84.$var = anf_assign83;
var provides89 = R.makeObject({"ValueSkeleton":ValueSkeleton62.$var,
"is-ValueSkeleton":is$ValueSkeleton64.$var,
"vs-str":vs$str68.$var,
"is-vs-str":is$vs$str66.$var,
"vs-value":vs$value72.$var,
"is-vs-value":is$vs$value70.$var,
"vs-collection":vs$collection76.$var,
"is-vs-collection":is$vs$collection74.$var,
"vs-constr":vs$constr80.$var,
"is-vs-constr":is$vs$constr78.$var,
"vs-seq":vs$seq84.$var,
"is-vs-seq":is$vs$seq82.$var});
$step5 = 1;
$al9 = L[13];
$field85 = R.getColonFieldLoc(builtins2,"current-checker",L[13]);
if(R.isMethod($field85)) {
$ans8 = $field85.full_meth(builtins2);
} else {
if(!(R.isFunction($field85))) {
R.ffi.throwNonFunApp(L[13],$field85);
}
$ans8 = $field85.app();
}
break;
case 1: var anf_method_obj86 = $ans8;
$step5 = 2;
$al9 = L[13];
$field87 = R.getColonFieldLoc(anf_method_obj86,"results",L[13]);
if(R.isMethod($field87)) {
$ans8 = $field87.full_meth(anf_method_obj86);
} else {
if(!(R.isFunction($field87))) {
R.ffi.throwNonFunApp(L[13],$field87);
}
$ans8 = $field87.app();
}
break;
case 2: var checks90 = $ans8;
$step5 = 3;
$ans8 = R.makeObject({"answer":nothing3,
"namespace":NAMESPACE,
"defined-values":{"vs-seq":vs$seq84.$var,
"is-vs-seq":is$vs$seq82.$var,
"vs-constr":vs$constr80.$var,
"is-vs-constr":is$vs$constr78.$var,
"vs-collection":vs$collection76.$var,
"is-vs-collection":is$vs$collection74.$var,
"vs-value":vs$value72.$var,
"is-vs-value":is$vs$value70.$var,
"vs-str":vs$str68.$var,
"is-vs-str":is$vs$str66.$var,
"is-ValueSkeleton":is$ValueSkeleton64.$var,
"ValueSkeleton":ValueSkeleton62.$var},
"defined-types":{"ValueSkeleton":ValueSkeleton88},
"provide-plus-types":R.makeObject({"values":provides89,
"types":{"ValueSkeleton":ValueSkeleton88}}),
"checks":checks90});
break;
case 3: ++R.GAS;
return $ans8;
default: throw "No case numbered " + $step5 + " in $toplevel6";
}
}
} catch($e92) {
if(R.isCont($e92) && ($step5 !== 3)) {
$e92.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al9,$toplevel6,$step5,[],[provides89,vs$seq84,is$vs$seq82,vs$constr80,is$vs$constr78,vs$collection76,is$vs$collection74,vs$value72,is$vs$value70,vs$str68,is$vs$str66,is$ValueSkeleton64,ValueSkeleton62,ValueSkeleton88]);
}
if(R.isPyretException($e92)) {
$e92.pyretStack.push($al9);
}
throw $e92;
}
};
return R.safeCall($toplevel6,function(moduleVal) {
R.modules["$src/arr/base/valueskeleton.arr4"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
