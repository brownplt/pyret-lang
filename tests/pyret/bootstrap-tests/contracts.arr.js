define([],function() {
return function(R,NAMESPACE) {
if(R.modules["tests/pyret/bootstrap-tests/contracts.arr6"]) {
return R.modules["tests/pyret/bootstrap-tests/contracts.arr6"]
} else {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)
};
var M = "tests/pyret/bootstrap-tests/contracts.arr";
var D = R.undefined;
var builtins2 = NAMESPACE.get("builtins");
var _empty3 = NAMESPACE.get("_empty");
var triangle4 = NAMESPACE.get("triangle");
var nothing5 = NAMESPACE.get("nothing");
var $Hanf_bracket317 = function(anf_bracket28,builtins2,result_DASH_after_DASH_checks119,_empty3,triangle4) {
var anf_fun320 = G(anf_bracket28,"run-checks",[M,2,0,1,8,3,164]);
var anf_obj1325 = R.makeFunction(function() {
if(arguments.length !== 0) {
R.ffi.throwArityErrorC([M,2,0,1,8,3,164],0,arguments)
}
var greentriangle227 = {"$var":D,
"$name":"greentriangle"};
var anf_assign528 = R.makeFunction(function(n342) {
if(arguments.length !== 1) {
R.ffi.throwArityErrorC([M,4,2,45,6,5,106],1,arguments)
}
var z4546 = D;
var f5152 = function(_) {
return R.isFunction(triangle4)?triangle4.app(n342,R.makeString("solid"),R.makeString("green")):R.ffi.throwNonFunApp([M,5,4,71,5,33,100],triangle4,[n342,R.makeString("solid"),R.makeString("green")])
};
try {
if(--R.GAS > 0) {
z4546 = f5152();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f5152,
"from":[M,5,4,71,5,33,100]});
}
} catch(e4344) {
var from = [M,5,4,71,5,33,100];
if(R.isCont(e4344)) {
var ss4748 = {"from":from,
"go":function(anf_tail_app453) {
return $Hanf_tail_app2254(anf_tail_app453)
}};
e4344.stack[R.EXN_STACKHEIGHT++] = ss4748;
throw e4344;
} else {
if(R.isPyretException(e4344)) {
e4344.pyretStack.push(from);
throw e4344;
} else {
throw e4344;
}
}
}
var ret4950 = $Hanf_tail_app2254(z4546);
++R.GAS;
return ret4950
});
greentriangle227.$var = anf_assign528
var anf_fun639 = G(builtins2,"current-checker",[M,7,2,109,7,53,160]);
var z3132 = D;
var f3738 = function(_) {
return R.isFunction(anf_fun639)?anf_fun639.app():R.ffi.throwNonFunApp([M,7,2,109,7,53,160],anf_fun639,[])
};
try {
if(--R.GAS > 0) {
z3132 = f3738();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f3738,
"from":[M,7,2,109,7,53,160]});
}
} catch(e2930) {
var from = [M,7,2,109,7,53,160];
if(R.isCont(e2930)) {
var ss3334 = {"from":from,
"go":function(anf_bracket740) {
return $Hanf_bracket2641(anf_bracket740,greentriangle227,triangle4)
}};
e2930.stack[R.EXN_STACKHEIGHT++] = ss3334;
throw e2930;
} else {
if(R.isPyretException(e2930)) {
e2930.pyretStack.push(from);
throw e2930;
} else {
throw e2930;
}
}
}
var ret3536 = $Hanf_bracket2641(z3132,greentriangle227,triangle4);
++R.GAS;
return ret3536
});
var anf_obj1426 = R.makeObject({"source":R.makeString("tests/pyret/bootstrap-tests/contracts.arr"),
"start-line":R.makeNumber(2),
"start-column":R.makeNumber(0),
"start-char":R.makeNumber(1),
"end-line":R.makeNumber(8),
"end-column":R.makeNumber(3),
"end-char":R.makeNumber(164)});
var anf_arg1524 = R.makeObject({"name":R.makeString("check-block-1"),
"run":anf_obj1325,
"location":anf_obj1426});
var anf_arg1621 = R._link(anf_arg1524,_empty3);
var z1213 = D;
var f1819 = function(_) {
return R.isFunction(anf_fun320)?anf_fun320.app(R.makeString("tests/pyret/bootstrap-tests/contracts.arr"),anf_arg1621):R.ffi.throwNonFunApp([M,2,0,1,8,3,164],anf_fun320,[R.makeString("tests/pyret/bootstrap-tests/contracts.arr"),anf_arg1621])
};
try {
if(--R.GAS > 0) {
z1213 = f1819();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f1819,
"from":[M,2,0,1,8,3,164]});
}
} catch(e1011) {
var from = [M,2,0,1,8,3,164];
if(R.isCont(e1011)) {
var ss1415 = {"from":from,
"go":function(anf_begin_app_dropped2722) {
return $Hanf_begin_app_dropped3023(anf_begin_app_dropped2722,result_DASH_after_DASH_checks119,builtins2)
}};
e1011.stack[R.EXN_STACKHEIGHT++] = ss1415;
throw e1011;
} else {
if(R.isPyretException(e1011)) {
e1011.pyretStack.push(from);
throw e1011;
} else {
throw e1011;
}
}
}
var ret1617 = $Hanf_begin_app_dropped3023(z1213,result_DASH_after_DASH_checks119,builtins2);
++R.GAS;
return ret1617
};
var $Hanf_begin_app_dropped3023 = function(anf_begin_app_dropped2722,result_DASH_after_DASH_checks119,builtins2) {
var anf_obj1767 = R.makeObject({});
var anf_fun1865 = G(builtins2,"current-checker",[M,2,0,1,8,3,164]);
var z5758 = D;
var f6364 = function(_) {
return R.isFunction(anf_fun1865)?anf_fun1865.app():R.ffi.throwNonFunApp([M,2,0,1,8,3,164],anf_fun1865,[])
};
try {
if(--R.GAS > 0) {
z5758 = f6364();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f6364,
"from":[M,2,0,1,8,3,164]});
}
} catch(e5556) {
var from = [M,2,0,1,8,3,164];
if(R.isCont(e5556)) {
var ss5960 = {"from":from,
"go":function(anf_bracket1966) {
return $Hanf_bracket2968(anf_bracket1966,anf_obj1767,result_DASH_after_DASH_checks119)
}};
e5556.stack[R.EXN_STACKHEIGHT++] = ss5960;
throw e5556;
} else {
if(R.isPyretException(e5556)) {
e5556.pyretStack.push(from);
throw e5556;
} else {
throw e5556;
}
}
}
var ret6162 = $Hanf_bracket2968(z5758,anf_obj1767,result_DASH_after_DASH_checks119);
++R.GAS;
return ret6162
};
var $Hanf_bracket2968 = function(anf_bracket1966,anf_obj1767,result_DASH_after_DASH_checks119) {
var anf_fun2079 = G(anf_bracket1966,"results",[M,2,0,1,8,3,164]);
var z7172 = D;
var f7778 = function(_) {
return R.isFunction(anf_fun2079)?anf_fun2079.app():R.ffi.throwNonFunApp([M,2,0,1,8,3,164],anf_fun2079,[])
};
try {
if(--R.GAS > 0) {
z7172 = f7778();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f7778,
"from":[M,2,0,1,8,3,164]});
}
} catch(e6970) {
var from = [M,2,0,1,8,3,164];
if(R.isCont(e6970)) {
var ss7374 = {"from":from,
"go":function(anf_obj2180) {
return $Hanf_obj2881(anf_obj2180,anf_obj1767,result_DASH_after_DASH_checks119)
}};
e6970.stack[R.EXN_STACKHEIGHT++] = ss7374;
throw e6970;
} else {
if(R.isPyretException(e6970)) {
e6970.pyretStack.push(from);
throw e6970;
} else {
throw e6970;
}
}
}
var ret7576 = $Hanf_obj2881(z7172,anf_obj1767,result_DASH_after_DASH_checks119);
++R.GAS;
return ret7576
};
var $Hanf_obj2881 = function(anf_obj2180,anf_obj1767,result_DASH_after_DASH_checks119) {
return R.makeObject({"answer":result_DASH_after_DASH_checks119,
"provide":anf_obj1767,
"checks":anf_obj2180})
};
var $Hanf_bracket2641 = function(anf_bracket740,greentriangle227,triangle4) {
var anf_fun893 = G(anf_bracket740,"check-is",[M,7,2,109,7,53,160]);
var z8485 = D;
var f9091 = function(_) {
return R.isFunction(greentriangle227.$var === D?U([M,7,2,109,7,15,122],"greentriangle"):greentriangle227.$var)?greentriangle227.$var === D?U([M,7,2,109,7,15,122],"greentriangle"):greentriangle227.$var.app(R.makeNumber(50)):R.ffi.throwNonFunApp([M,7,2,109,7,19,126],greentriangle227.$var === D?U([M,7,2,109,7,15,122],"greentriangle"):greentriangle227.$var,[R.makeNumber(50)])
};
try {
if(--R.GAS > 0) {
z8485 = f9091();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f9091,
"from":[M,7,2,109,7,19,126]});
}
} catch(e8283) {
var from = [M,7,2,109,7,19,126];
if(R.isCont(e8283)) {
var ss8687 = {"from":from,
"go":function(anf_arg992) {
return $Hanf_arg2594(anf_arg992,anf_fun893,triangle4)
}};
e8283.stack[R.EXN_STACKHEIGHT++] = ss8687;
throw e8283;
} else {
if(R.isPyretException(e8283)) {
e8283.pyretStack.push(from);
throw e8283;
} else {
throw e8283;
}
}
}
var ret8889 = $Hanf_arg2594(z8485,anf_fun893,triangle4);
++R.GAS;
return ret8889
};
var $Hanf_arg2594 = function(anf_arg992,anf_fun893,triangle4) {
var z9798 = D;
var f103104 = function(_) {
return R.isFunction(triangle4)?triangle4.app(R.makeNumber(50),R.makeString("solid"),R.makeString("green")):R.ffi.throwNonFunApp([M,7,23,130,7,53,160],triangle4,[R.makeNumber(50),R.makeString("solid"),R.makeString("green")])
};
try {
if(--R.GAS > 0) {
z9798 = f103104();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f103104,
"from":[M,7,23,130,7,53,160]});
}
} catch(e9596) {
var from = [M,7,23,130,7,53,160];
if(R.isCont(e9596)) {
var ss99100 = {"from":from,
"go":function(anf_arg10105) {
return $Hanf_arg24106(anf_arg10105,anf_arg992,anf_fun893)
}};
e9596.stack[R.EXN_STACKHEIGHT++] = ss99100;
throw e9596;
} else {
if(R.isPyretException(e9596)) {
e9596.pyretStack.push(from);
throw e9596;
} else {
throw e9596;
}
}
}
var ret101102 = $Hanf_arg24106(z9798,anf_arg992,anf_fun893);
++R.GAS;
return ret101102
};
var $Hanf_arg24106 = function(anf_arg10105,anf_arg992,anf_fun893) {
var anf_arg11117 = R.makeObject({"source":R.makeString("tests/pyret/bootstrap-tests/contracts.arr"),
"start-line":R.makeNumber(7),
"start-column":R.makeNumber(2),
"start-char":R.makeNumber(109),
"end-line":R.makeNumber(7),
"end-column":R.makeNumber(53),
"end-char":R.makeNumber(160)});
var z109110 = D;
var f115116 = function(_) {
return R.isFunction(anf_fun893)?anf_fun893.app(R.makeString("greentriangle(50) is triangle(50, \"solid\", \"green\")"),anf_arg992,anf_arg10105,anf_arg11117):R.ffi.throwNonFunApp([M,7,2,109,7,53,160],anf_fun893,[R.makeString("greentriangle(50) is triangle(50, \"solid\", \"green\")"),anf_arg992,anf_arg10105,anf_arg11117])
};
try {
if(--R.GAS > 0) {
z109110 = f115116();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f115116,
"from":[M,7,2,109,7,53,160]});
}
} catch(e107108) {
var from = [M,7,2,109,7,53,160];
if(R.isCont(e107108)) {
var ss111112 = {"from":from,
"go":function(anf_tail_app12118) {
return $Hanf_tail_app23119(anf_tail_app12118)
}};
e107108.stack[R.EXN_STACKHEIGHT++] = ss111112;
throw e107108;
} else {
if(R.isPyretException(e107108)) {
e107108.pyretStack.push(from);
throw e107108;
} else {
throw e107108;
}
}
}
var ret113114 = $Hanf_tail_app23119(z109110);
++R.GAS;
return ret113114
};
var $Hanf_tail_app23119 = function(anf_tail_app12118) {
return anf_tail_app12118
};
var $Hanf_tail_app2254 = function(anf_tail_app453) {
return anf_tail_app453
};
return R.safeCall(function() {
nothing5
var result_DASH_after_DASH_checks119 = nothing5;
var anf_fun1130 = G(builtins2,"current-checker",[M,2,0,1,8,3,164]);
var z122123 = D;
var f128129 = function(_) {
return R.isFunction(anf_fun1130)?anf_fun1130.app():R.ffi.throwNonFunApp([M,2,0,1,8,3,164],anf_fun1130,[])
};
try {
if(--R.GAS > 0) {
z122123 = f128129();
} else {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont({"go":f128129,
"from":[M,2,0,1,8,3,164]});
}
} catch(e120121) {
var from = [M,2,0,1,8,3,164];
if(R.isCont(e120121)) {
var ss124125 = {"from":from,
"go":function(anf_bracket28) {
return $Hanf_bracket317(anf_bracket28,builtins2,result_DASH_after_DASH_checks119,_empty3,triangle4)
}};
e120121.stack[R.EXN_STACKHEIGHT++] = ss124125;
throw e120121;
} else {
if(R.isPyretException(e120121)) {
e120121.pyretStack.push(from);
throw e120121;
} else {
throw e120121;
}
}
}
var ret126127 = $Hanf_bracket317(z122123,builtins2,result_DASH_after_DASH_checks119,_empty3,triangle4);
++R.GAS;
return ret126127
},function(moduleVal) {
R.modules["tests/pyret/bootstrap-tests/contracts.arr6"] = moduleVal
return moduleVal
})
}
}
})