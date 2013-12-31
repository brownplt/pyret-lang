
if (require) {
  var Namespace = require('./namespace.js').Namespace;
}
  LIB = (function(RUNTIME, NAMESPACE) {
            try {
              var print408214 = NAMESPACE.get('print');
var raise408219 = NAMESPACE.get('raise');
var nothing408224 = NAMESPACE.get('nothing');
var check_DASH_brand408229 = NAMESPACE.get('check-brand');
var prim_DASH_has_DASH_field408234 = NAMESPACE.get('prim-has-field');
var prim_DASH_keys408239 = NAMESPACE.get('prim-keys');
var prim_DASH_num_DASH_keys408244 = NAMESPACE.get('prim-num-keys');
var tostring408249 = NAMESPACE.get('tostring');
var torepr408254 = NAMESPACE.get('torepr');
var brander408259 = NAMESPACE.get('brander');
var Function408264 = NAMESPACE.get('Function');
var Placeholder408269 = NAMESPACE.get('Placeholder');
var Bool408274 = NAMESPACE.get('Bool');
var Number408279 = NAMESPACE.get('Number');
var String408284 = NAMESPACE.get('String');
var Nothing408289 = NAMESPACE.get('Nothing');
var Method408294 = NAMESPACE.get('Method');
var data_DASH_equals408299 = NAMESPACE.get('data-equals');
var data_DASH_to_DASH_repr408304 = NAMESPACE.get('data-to-repr');
var equiv408309 = NAMESPACE.get('equiv');

              var RESULT;
              var EXPORT_NAMESPACE = RUNTIME.Namespace({});
              (function() {
                var identical408314 = RUNTIME.makeFunction(function (obj1359752408315,obj2359753408316) { return (function(){
 var obj1408319 = obj1359752408315;
var obj2408326 = obj2359753408316;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(has_DASH_field408333, [obj1408319,RUNTIME.makeString("eq")]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(has_DASH_field408333, [obj2408326,RUNTIME.makeString("eq")]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(obj1408319, "eq"), [obj2408326]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Identical got values that weren't created by data: "), "_plus"), [RUNTIME.applyFunc(torepr408254, [obj1408319])]), "_plus"), [RUNTIME.makeString(" and ")]), "_plus"), [RUNTIME.applyFunc(torepr408254, [obj2408326])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var mklist408462 = RUNTIME.makeFunction(function (obj359754408463) { return (function(){
 var obj408465 = obj359754408463;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.getField(obj408465, "is-empty"))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.getField(obj408465, "first"),RUNTIME.applyFunc(mklist408462, [RUNTIME.getField(obj408465, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Creates a List from something with `first` and `rest` fields, recursively"));
var keys408535 = RUNTIME.makeFunction(function (obj359755408536) { return (function(){
 var obj408465 = obj359755408536;
return (function(){
 return RUNTIME.applyFunc(mklist408462, [RUNTIME.applyFunc(prim_DASH_keys408239, [obj408465])]); 
})(); 
})(); }, RUNTIME.makeString("Returns a List of the keys of an object, as strings"));
var has_DASH_field408333 = RUNTIME.makeFunction(function (obj359756408574,name359757408575) { return (function(){
 var obj408465 = obj359756408574;
var name408584 = name359757408575;
return (function(){
 return RUNTIME.applyFunc(prim_DASH_has_DASH_field408234, [obj408465,name408584]); 
})(); 
})(); }, RUNTIME.makeString("Returns true if the object has a field with the name specified"));
var num_DASH_keys408617 = RUNTIME.makeFunction(function (obj359758408618) { return (function(){
 var obj408465 = obj359758408618;
return (function(){
 return RUNTIME.applyFunc(prim_DASH_num_DASH_keys408244, [obj408465]); 
})(); 
})(); }, RUNTIME.makeString("Returns the Number of fields in an object"));
var Eq408651 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 var b408652 = RUNTIME.applyFunc(brander408259, []);
return RUNTIME.makeObject({'extend':RUNTIME.makeFunction(function (obj359759408663) { return (function(){
 var obj408465 = obj359759408663;
return (function(){
 return obj408465.extend('eq', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(b408652, "test"), [self408671]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(b408652, "test"), [other408672]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))); 
})(); 
})(); }, RUNTIME.makeString("")),'brand':RUNTIME.makeFunction(function (obj359760408744) { return (function(){
 var obj408465 = obj359760408744;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(b408652, "brand"), [obj408465]); 
})(); 
})(); }, RUNTIME.makeString(""))}); 
})(); 
})(); }, RUNTIME.makeString(""));
var builtins408803 = RUNTIME.makeObject({'identical':identical408314,'keys':keys408535,'has-field':has_DASH_field408333,'mklist':mklist408462,'equiv':equiv408309,'data-to-repr':data_DASH_to_DASH_repr408304,'data-equals':data_DASH_equals408299,'Eq':Eq408651});
var get_DASH_help408852 = RUNTIME.makeFunction(function (lst359761408853,n359762408854) { return (function(){
 var lst408857 = lst359761408853;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359765408865) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359765408865,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359762408854]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l359763408898,cur359764408899) { return (function(){
 var l408902 = l359763408898;
var cur408909 = cur359764408899;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [l408902]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("get: n too large "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [cur408909,RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.getField(l408902, "first"); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help408897, [RUNTIME.getField(l408902, "rest"),RUNTIME.applyFunc(RUNTIME.getField(cur408909, "_minus"), [RUNTIME.makeNumber(1)])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n408864, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("get: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help408897, [lst408857,n408864]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var set_DASH_help409102 = RUNTIME.makeFunction(function (lst359766409103,n359767409104,v359768409105) { return (function(){
 var lst408857 = lst359766409103;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359771409115) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359771409115,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359767409104]);
var v409147 = v359768409105;
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l359769409154,cur359770409155) { return (function(){
 var l408902 = l359769409154;
var cur408909 = cur359770409155;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [l408902]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("set: n too large "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [cur408909,RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(link408481, [v409147,RUNTIME.getField(l408902, "rest")]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.getField(l408902, "first"),RUNTIME.applyFunc(help408897, [RUNTIME.getField(l408902, "rest"),RUNTIME.applyFunc(RUNTIME.getField(cur408909, "_minus"), [RUNTIME.makeNumber(1)])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n408864, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("set: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help408897, [lst408857,n408864]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var reverse_DASH_help409371 = RUNTIME.makeFunction(function (lst359772409372,acc359773409373) { return (function(){
 var lst408857 = lst359772409372;
var acc409382 = acc359773409373;
return (function(){
 var cases_DASH_value359484409389 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359774409390) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359774409390,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst408857]);
return RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value359484409389, "_match"), [RUNTIME.makeObject({'empty':(function() { throw new Error('Not yet implemented s_hint') })(),'link':(function() { throw new Error('Not yet implemented s_hint') })()}),(function() { throw new Error('Not yet implemented s_hint') })()]); 
})(); 
})(); }, RUNTIME.makeString(""));
var take_DASH_help409477 = RUNTIME.makeFunction(function (lst359777409478,n359778409479) { return (function(){
 var lst408857 = lst359777409478;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359781409488) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359781409488,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359778409479]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l359779409520,cur359780409521) { return (function(){
 var l408902 = l359779409520;
var cur408909 = cur359780409521;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [cur408909,RUNTIME.makeNumber(0)]))) { return (function(){
 return empty408476; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link409536, [l408902]))) { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.getField(l408902, "first"),RUNTIME.applyFunc(help408897, [RUNTIME.getField(l408902, "rest"),RUNTIME.applyFunc(RUNTIME.getField(cur408909, "_minus"), [RUNTIME.makeNumber(1)])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("take: n too large: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n408864, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("take: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help408897, [lst408857,n408864]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var drop_DASH_help409728 = RUNTIME.makeFunction(function (lst359782409729,n359783409730) { return (function(){
 var lst408857 = lst359782409729;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359786409739) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359786409739,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359783409730]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l359784409771,cur359785409772) { return (function(){
 var l408902 = l359784409771;
var cur408909 = cur359785409772;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [cur408909,RUNTIME.makeNumber(0)]))) { return (function(){
 return l408902; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link409536, [l408902]))) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(l408902, "rest"), "drop"), [RUNTIME.applyFunc(RUNTIME.getField(cur408909, "_minus"), [RUNTIME.makeNumber(1)])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("drop: n to large: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n408864, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("drop: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [n408864])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help408897, [lst408857,n408864]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var list_DASH_to_DASH_set409971 = RUNTIME.makeFunction(function (lst359787409972) { return (function(){
 var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359790409974) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359790409974,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359787409972]);
return (function(){
 return RUNTIME.applyFunc(fold410006, [RUNTIME.makeFunction(function (s359788410007,elem359789410008) { return (function(){
 var s410011 = s359788410007;
var elem410018 = elem359789410008;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(s410011, "add"), [elem410018]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(__set410048, [RUNTIME.getField(list410049, "empty")]),lst408857]); 
})(); 
})(); }, RUNTIME.makeString("Convert a list into a set."));
var List_DASH_mixins359346410086 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359347410097 = RUNTIME.makeObject({'push':RUNTIME.makeMethod(function (self408671,elt410098) { return (function(){
 return RUNTIME.applyFunc(link408481, [elt410098,self408671]); 
})(); }, RUNTIME.makeString("Adds an element to the front of the list, returning a new list")),'_plus':RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359791410121) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract359791410121,RUNTIME.makeString("(List, List => Any)")]);
var fun359792410134 = RUNTIME.applyFunc(RUNTIME.getField(contract359791410121, "_fun"), []);
return RUNTIME.makeMethod(function (arg359793410149,arg359794410150) { return (function(){
 return RUNTIME.applyFunc(fun359792410134, [RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359795410153) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359795410153,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [arg359793410149]),RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359796410179) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359796410179,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [arg359794410150])]); 
})(); }, RUNTIME.makeString("internal contract for (List, List => Any)")).extend('_doc', RUNTIME.getField(contract359791410121, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (List, List => Any)")), [RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self408671, "append"), [other408672]); 
})(); }, RUNTIME.makeString(""))]),'to-set':RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359797410273) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract359797410273,RUNTIME.makeString("(List => Any)")]);
var fun359798410286 = RUNTIME.applyFunc(RUNTIME.getField(contract359797410273, "_fun"), []);
return RUNTIME.makeMethod(function (arg359799410301) { return (function(){
 return RUNTIME.applyFunc(fun359798410286, [RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359800410303) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359800410303,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [arg359799410301])]); 
})(); }, RUNTIME.makeString("internal contract for (List => Any)")).extend('_doc', RUNTIME.getField(contract359797410273, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (List => Any)")), [RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(list_DASH_to_DASH_set409971, [self408671]); 
})(); }, RUNTIME.makeString(""))])});
var variant359348410402 = data_DASH_shared359347410097.extend('length', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeNumber(0); 
})(); }, RUNTIME.makeString(""))).extend('each', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359801410420) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract359801410420,RUNTIME.makeString("(Any, (Any -> Nothing) => Any)")]);
var fun359802410433 = RUNTIME.applyFunc(RUNTIME.getField(contract359801410420, "_fun"), []);
return RUNTIME.makeMethod(function (arg359803410448,arg359804410449) { return (function(){
 return RUNTIME.applyFunc(fun359802410433, [arg359803410448,RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359805410452) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Function408264,contract359805410452,RUNTIME.makeString("(Any -> Nothing)")]);
var fun359806410465 = contract359805410452;
return RUNTIME.makeFunction(function (arg359807410472) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359808410474) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Nothing408289,specimen359808410474,RUNTIME.makeString("Nothing")]); 
})(); }, RUNTIME.makeString("internal contract for Nothing")), [RUNTIME.applyFunc(fun359806410465, [arg359807410472])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Nothing)")).extend('_doc', RUNTIME.getField(contract359805410452, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Nothing)")), [arg359804410449])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Nothing) => Any)")).extend('_doc', RUNTIME.getField(contract359801410420, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Nothing) => Any)")), [RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return nothing408224; 
})(); }, RUNTIME.makeString(""))])).extend('map', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359809410602) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract359809410602,RUNTIME.makeString("(Any, (Any -> Any) => Any)")]);
var fun359810410615 = RUNTIME.applyFunc(RUNTIME.getField(contract359809410602, "_fun"), []);
return RUNTIME.makeMethod(function (arg359811410630,arg359812410631) { return (function(){
 return RUNTIME.applyFunc(fun359810410615, [arg359811410630,arg359812410631]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Any) => Any)")).extend('_doc', RUNTIME.getField(contract359809410602, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Any) => Any)")), [RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return empty408476; 
})(); }, RUNTIME.makeString(""))])).extend('filter', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359813410693) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract359813410693,RUNTIME.makeString("(Any, (Any -> Bool) => Any)")]);
var fun359814410706 = RUNTIME.applyFunc(RUNTIME.getField(contract359813410693, "_fun"), []);
return RUNTIME.makeMethod(function (arg359815410721,arg359816410722) { return (function(){
 return RUNTIME.applyFunc(fun359814410706, [arg359815410721,RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359817410725) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Function408264,contract359817410725,RUNTIME.makeString("(Any -> Bool)")]);
var fun359818410738 = contract359817410725;
return RUNTIME.makeFunction(function (arg359819410745) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359820410747) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359820410747,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun359818410738, [arg359819410745])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract359817410725, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [arg359816410722])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")).extend('_doc', RUNTIME.getField(contract359813410693, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")), [RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return empty408476; 
})(); }, RUNTIME.makeString(""))])).extend('find', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359821410874) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract359821410874,RUNTIME.makeString("(Any, (Any -> Bool) => Any)")]);
var fun359822410887 = RUNTIME.applyFunc(RUNTIME.getField(contract359821410874, "_fun"), []);
return RUNTIME.makeMethod(function (arg359823410902,arg359824410903) { return (function(){
 return RUNTIME.applyFunc(fun359822410887, [arg359823410902,RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359825410906) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Function408264,contract359825410906,RUNTIME.makeString("(Any -> Bool)")]);
var fun359826410919 = contract359825410906;
return RUNTIME.makeFunction(function (arg359827410926) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359828410928) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359828410928,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun359826410919, [arg359827410926])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract359825410906, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [arg359824410903])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")).extend('_doc', RUNTIME.getField(contract359821410874, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")), [RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return none411036; 
})(); }, RUNTIME.makeString(""))])).extend('partition', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return RUNTIME.makeObject({'is-true':empty408476,'is-false':empty408476}); 
})(); }, RUNTIME.makeString(""))).extend('foldr', RUNTIME.makeMethod(function (self408671,f410580,base411084) { return (function(){
 return base411084; 
})(); }, RUNTIME.makeString(""))).extend('foldl', RUNTIME.makeMethod(function (self408671,f410580,base411084) { return (function(){
 return base411084; 
})(); }, RUNTIME.makeString(""))).extend('member', RUNTIME.makeMethod(function (self408671,elt410098) { return (function(){
 return RUNTIME.makeBool(false); 
})(); }, RUNTIME.makeString(""))).extend('append', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return other408672; 
})(); }, RUNTIME.makeString(""))).extend('last', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.makeString("last: took last of empty list")]); 
})(); }, RUNTIME.makeString(""))).extend('take', RUNTIME.makeMethod(function (self408671,n408864) { return (function(){
 return RUNTIME.applyFunc(take_DASH_help409477, [self408671,n408864]); 
})(); }, RUNTIME.makeString(""))).extend('drop', RUNTIME.makeMethod(function (self408671,n408864) { return (function(){
 return RUNTIME.applyFunc(drop_DASH_help409728, [self408671,n408864]); 
})(); }, RUNTIME.makeString(""))).extend('reverse', RUNTIME.makeMethod(function (self408671) { return (function(){
 return self408671; 
})(); }, RUNTIME.makeString(""))).extend('get', RUNTIME.makeMethod(function (self408671,n408864) { return (function(){
 return RUNTIME.applyFunc(get_DASH_help408852, [self408671,n408864]); 
})(); }, RUNTIME.makeString(""))).extend('set', RUNTIME.makeMethod(function (self408671,n408864,e411256) { return (function(){
 return RUNTIME.applyFunc(set_DASH_help409102, [self408671,n408864,e411256]); 
})(); }, RUNTIME.makeString(""))).extend('_equals', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [other408672]); 
})(); }, RUNTIME.makeString(""))).extend('tostring', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("[]"); 
})(); }, RUNTIME.makeString(""))).extend('_torepr', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("[]"); 
})(); }, RUNTIME.makeString(""))).extend('sort-by', RUNTIME.makeMethod(function (self408671,cmp411336,eq411337) { return (function(){
 return self408671; 
})(); }, RUNTIME.makeString(""))).extend('sort', RUNTIME.makeMethod(function (self408671) { return (function(){
 return self408671; 
})(); }, RUNTIME.makeString(""))).extend('join-str', RUNTIME.makeMethod(function (self408671,str411370) { return (function(){
 return RUNTIME.makeString(""); 
})(); }, RUNTIME.makeString("")));
var variant359349411395 = data_DASH_shared359347410097.extend('length', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeNumber(1), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "length"), [])]); 
})(); }, RUNTIME.makeString(""))).extend('each', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 RUNTIME.applyFunc(f410580, [RUNTIME.getField(self408671, "first")]);
return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "each"), [f410580]); 
})(); }, RUNTIME.makeString(""))).extend('map', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [RUNTIME.getField(self408671, "first")]),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "map"), [f410580])]); 
})(); }, RUNTIME.makeString(""))).extend('filter', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f410580, [RUNTIME.getField(self408671, "first")]))) { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.getField(self408671, "first"),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "filter"), [f410580])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "filter"), [f410580]); 
})(); } })(); 
})(); }, RUNTIME.makeString(""))).extend('partition', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return RUNTIME.applyFunc(partition411599, [f410580,self408671]); 
})(); }, RUNTIME.makeString(""))).extend('find', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return RUNTIME.applyFunc(find411622, [f410580,self408671]); 
})(); }, RUNTIME.makeString(""))).extend('member', RUNTIME.makeMethod(function (self408671,elt410098) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [elt410098,RUNTIME.getField(self408671, "first")]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "member"), [elt410098]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))).extend('foldr', RUNTIME.makeMethod(function (self408671,f410580,base411084) { return (function(){
 return RUNTIME.applyFunc(f410580, [RUNTIME.getField(self408671, "first"),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "foldr"), [f410580,base411084])]); 
})(); }, RUNTIME.makeString(""))).extend('foldl', RUNTIME.makeMethod(function (self408671,f410580,base411084) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "foldl"), [f410580,RUNTIME.applyFunc(f410580, [RUNTIME.getField(self408671, "first"),base411084])]); 
})(); }, RUNTIME.makeString(""))).extend('append', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.getField(self408671, "first"),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "append"), [other408672])]); 
})(); }, RUNTIME.makeString(""))).extend('last', RUNTIME.makeMethod(function (self408671) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [RUNTIME.getField(self408671, "rest")]))) { return (function(){
 return RUNTIME.getField(self408671, "first"); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "last"), []); 
})(); } })(); 
})(); }, RUNTIME.makeString(""))).extend('reverse', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(reverse_DASH_help409371, [self408671,empty408476]); 
})(); }, RUNTIME.makeString(""))).extend('take', RUNTIME.makeMethod(function (self408671,n408864) { return (function(){
 return RUNTIME.applyFunc(take_DASH_help409477, [self408671,n408864]); 
})(); }, RUNTIME.makeString(""))).extend('drop', RUNTIME.makeMethod(function (self408671,n408864) { return (function(){
 return RUNTIME.applyFunc(drop_DASH_help409728, [self408671,n408864]); 
})(); }, RUNTIME.makeString(""))).extend('get', RUNTIME.makeMethod(function (self408671,n408864) { return (function(){
 return RUNTIME.applyFunc(get_DASH_help408852, [self408671,n408864]); 
})(); }, RUNTIME.makeString(""))).extend('set', RUNTIME.makeMethod(function (self408671,n408864,e411256) { return (function(){
 return RUNTIME.applyFunc(set_DASH_help409102, [self408671,n408864,e411256]); 
})(); }, RUNTIME.makeString(""))).extend('_equals', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link409536, [other408672]))) { return (function(){
 var others_DASH_equal412002 = RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getRawField(self408671, 'first'),RUNTIME.getRawField(other408672, 'first')]);
return RUNTIME.applyFunc(RUNTIME.getField(others_DASH_equal412002, "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getRawField(self408671, 'rest'),RUNTIME.getRawField(other408672, 'rest')]); 
})(); }, RUNTIME.makeString(""))]); 
})(); } else { return (function(){
 return RUNTIME.makeBool(false); 
})(); } })(); 
})(); }, RUNTIME.makeString(""))).extend('tostring', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("["), "_plus"), [RUNTIME.applyFunc(raw_DASH_fold412105, [RUNTIME.makeFunction(function (combined359829412106,elt359830412107) { return (function(){
 var combined412110 = combined359829412106;
var elt410098 = elt359830412107;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(combined412110, "_plus"), [RUNTIME.makeString(", ")]), "_plus"), [RUNTIME.applyFunc(tostring408249, [elt410098])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(tostring408249, [RUNTIME.getRawField(self408671, 'first')]),RUNTIME.getRawField(self408671, 'rest')])]), "_plus"), [RUNTIME.makeString("]")]); 
})(); }, RUNTIME.makeString(""))).extend('_torepr', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("["), "_plus"), [RUNTIME.applyFunc(raw_DASH_fold412105, [RUNTIME.makeFunction(function (combined359831412219,elt359832412220) { return (function(){
 var combined412110 = combined359831412219;
var elt410098 = elt359832412220;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(combined412110, "_plus"), [RUNTIME.makeString(", ")]), "_plus"), [RUNTIME.applyFunc(torepr408254, [elt410098])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(torepr408254, [RUNTIME.getRawField(self408671, 'first')]),RUNTIME.getRawField(self408671, 'rest')])]), "_plus"), [RUNTIME.makeString("]")]); 
})(); }, RUNTIME.makeString(""))).extend('sort-by', RUNTIME.makeMethod(function (self408671,cmp411336,eq411337) { return (function(){
 var pivot412327 = RUNTIME.getField(self408671, "first");
var less412338 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(self408671, "filter"), [RUNTIME.makeFunction(function (e359833412343) { return (function(){
 var e411256 = e359833412343;
return (function(){
 return RUNTIME.applyFunc(cmp411336, [e411256,pivot412327]); 
})(); 
})(); }, RUNTIME.makeString(""))]), "sort-by"), [cmp411336,eq411337]);
var equal412392 = RUNTIME.applyFunc(RUNTIME.getField(self408671, "filter"), [RUNTIME.makeFunction(function (e359834412397) { return (function(){
 var e411256 = e359834412397;
return (function(){
 return RUNTIME.applyFunc(eq411337, [e411256,pivot412327]); 
})(); 
})(); }, RUNTIME.makeString(""))]);
var greater412436 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(self408671, "filter"), [RUNTIME.makeFunction(function (e359835412441) { return (function(){
 var e411256 = e359835412441;
return (function(){
 return RUNTIME.applyFunc(cmp411336, [pivot412327,e411256]); 
})(); 
})(); }, RUNTIME.makeString(""))]), "sort-by"), [cmp411336,eq411337]);
return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(less412338, "append"), [equal412392]), "append"), [greater412436]); 
})(); }, RUNTIME.makeString("Takes a comparator to check for elements that are strictly greater\n        or less than one another, and an equality procedure for elements that are\n        equal, and sorts the list accordingly."))).extend('sort', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self408671, "sort-by"), [RUNTIME.makeFunction(function (e1359836412527,e2359837412528) { return (function(){
 var e1412531 = e1359836412527;
var e2412538 = e2359837412528;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(e1412531, "_lessthan"), [e2412538]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.makeFunction(function (e1359838412568,e2359839412569) { return (function(){
 var e1412531 = e1359838412568;
var e2412538 = e2359839412569;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [e1412531,e2412538]); 
})(); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))).extend('join-str', RUNTIME.makeMethod(function (self408671,str411370) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link409536, [RUNTIME.getField(self408671, "rest")]))) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(tostring408249, [RUNTIME.getField(self408671, "first")]), "_plus"), [str411370]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "rest"), "join-str"), [str411370])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(tostring408249, [RUNTIME.getField(self408671, "first")]); 
})(); } })(); 
})(); }, RUNTIME.makeString("")));
var List359485412726 = RUNTIME.applyFunc(brander408259, []);
var empty_base359488412737 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("empty"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_empty408940,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("empty")]))) { return (function(){
 var call_DASH_empty359487412796 = RUNTIME.getField(cases_DASH_funs412779, "empty");
return RUNTIME.applyFunc(call_DASH_empty359487412796, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var empty359486412850 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_empty408940 = RUNTIME.getField(empty359486412850, "test");
var empty408476 = RUNTIME.applyFunc(RUNTIME.getField(List359485412726, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(empty359486412850, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self359840412879) { return (function(){
 var self408671 = self359840412879;
return (function(){
 var mixin359350412887 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [List_DASH_mixins359346410086]))) { return RUNTIME.applyFunc(List_DASH_mixins359346410086, []); } else { return List_DASH_mixins359346410086; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359350412887, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359350412887, "extend"), [self408671.extend('push', RUNTIME.getRawField(data_DASH_shared359347410097, 'push')).extend('_plus', RUNTIME.getRawField(data_DASH_shared359347410097, '_plus')).extend('to-set', RUNTIME.getRawField(data_DASH_shared359347410097, 'to-set')).extend('length', RUNTIME.getRawField(variant359348410402, 'length')).extend('each', RUNTIME.getRawField(variant359348410402, 'each')).extend('map', RUNTIME.getRawField(variant359348410402, 'map')).extend('filter', RUNTIME.getRawField(variant359348410402, 'filter')).extend('find', RUNTIME.getRawField(variant359348410402, 'find')).extend('partition', RUNTIME.getRawField(variant359348410402, 'partition')).extend('foldr', RUNTIME.getRawField(variant359348410402, 'foldr')).extend('foldl', RUNTIME.getRawField(variant359348410402, 'foldl')).extend('member', RUNTIME.getRawField(variant359348410402, 'member')).extend('append', RUNTIME.getRawField(variant359348410402, 'append')).extend('last', RUNTIME.getRawField(variant359348410402, 'last')).extend('take', RUNTIME.getRawField(variant359348410402, 'take')).extend('drop', RUNTIME.getRawField(variant359348410402, 'drop')).extend('reverse', RUNTIME.getRawField(variant359348410402, 'reverse')).extend('get', RUNTIME.getRawField(variant359348410402, 'get')).extend('set', RUNTIME.getRawField(variant359348410402, 'set')).extend('_equals', RUNTIME.getRawField(variant359348410402, '_equals')).extend('tostring', RUNTIME.getRawField(variant359348410402, 'tostring')).extend('_torepr', RUNTIME.getRawField(variant359348410402, '_torepr')).extend('sort-by', RUNTIME.getRawField(variant359348410402, 'sort-by')).extend('sort', RUNTIME.getRawField(variant359348410402, 'sort')).extend('join-str', RUNTIME.getRawField(variant359348410402, 'join-str'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for empty")), [empty_base359488412737])])]);
var link_base359491413166 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("link"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("first"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("rest"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_link409536,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("first"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("rest"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("link")]))) { return (function(){
 var call_DASH_link359489413286 = RUNTIME.getField(cases_DASH_funs412779, "link");
return RUNTIME.applyFunc(call_DASH_link359489413286, [RUNTIME.getField(RUNTIME.getMutableField(self408671, 'first'), 'get').app(),RUNTIME.getField(RUNTIME.getMutableField(self408671, 'rest'), 'get').app()]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var link359490413354 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_link409536 = RUNTIME.getField(link359490413354, "test");
var link408481 = RUNTIME.makeFunction(function (first359492359841413375,rest359493359842413376) { return (function(){
 var first359492413379 = first359492359841413375;
var rest359493413386 = rest359493359842413376;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(List359485412726, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(link359490413354, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self359843413401) { return (function(){
 var self408671 = self359843413401;
return (function(){
 var mixin359351413409 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [List_DASH_mixins359346410086]))) { return RUNTIME.applyFunc(List_DASH_mixins359346410086, []); } else { return List_DASH_mixins359346410086; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359351413409, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359351413409, "extend"), [self408671.extend('push', RUNTIME.getRawField(data_DASH_shared359347410097, 'push')).extend('_plus', RUNTIME.getRawField(data_DASH_shared359347410097, '_plus')).extend('to-set', RUNTIME.getRawField(data_DASH_shared359347410097, 'to-set')).extend('length', RUNTIME.getRawField(variant359349411395, 'length')).extend('each', RUNTIME.getRawField(variant359349411395, 'each')).extend('map', RUNTIME.getRawField(variant359349411395, 'map')).extend('filter', RUNTIME.getRawField(variant359349411395, 'filter')).extend('partition', RUNTIME.getRawField(variant359349411395, 'partition')).extend('find', RUNTIME.getRawField(variant359349411395, 'find')).extend('member', RUNTIME.getRawField(variant359349411395, 'member')).extend('foldr', RUNTIME.getRawField(variant359349411395, 'foldr')).extend('foldl', RUNTIME.getRawField(variant359349411395, 'foldl')).extend('append', RUNTIME.getRawField(variant359349411395, 'append')).extend('last', RUNTIME.getRawField(variant359349411395, 'last')).extend('reverse', RUNTIME.getRawField(variant359349411395, 'reverse')).extend('take', RUNTIME.getRawField(variant359349411395, 'take')).extend('drop', RUNTIME.getRawField(variant359349411395, 'drop')).extend('get', RUNTIME.getRawField(variant359349411395, 'get')).extend('set', RUNTIME.getRawField(variant359349411395, 'set')).extend('_equals', RUNTIME.getRawField(variant359349411395, '_equals')).extend('tostring', RUNTIME.getRawField(variant359349411395, 'tostring')).extend('_torepr', RUNTIME.getRawField(variant359349411395, '_torepr')).extend('sort-by', RUNTIME.getRawField(variant359349411395, 'sort-by')).extend('sort', RUNTIME.getRawField(variant359349411395, 'sort')).extend('join-str', RUNTIME.getRawField(variant359349411395, 'join-str'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for link")), [link_base359491413166.extend('first', first359492413379).extend('rest', (function(){
 var maybe_DASH_placeholder359845413671 = rest359493413386;
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Placeholder408269, [maybe_DASH_placeholder359845413671]))) { return (function(){
 RUNTIME.applyFunc(RUNTIME.getField(maybe_DASH_placeholder359845413671, "guard"), [RUNTIME.makeFunction(function (specimen359844413687) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359844413687,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List"))]);
return maybe_DASH_placeholder359845413671; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359844413687) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359844413687,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [maybe_DASH_placeholder359845413671]); 
})(); } })(); 
})())])])]); 
})(); 
})(); }, RUNTIME.makeString("link: Creates an instance of link"));
var List409392 = RUNTIME.getField(List359485412726, "test");
var range413811 = RUNTIME.makeFunction(function (start359846413812,stop359847413813) { return (function(){
 var start413816 = start359846413812;
var stop413823 = stop359847413813;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(start413816, "_lessthan"), [stop413823]))) { return (function(){
 return RUNTIME.applyFunc(link408481, [start413816,RUNTIME.applyFunc(range413811, [RUNTIME.applyFunc(RUNTIME.getField(start413816, "_plus"), [RUNTIME.makeNumber(1)]),stop413823])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [start413816,stop413823]))) { return (function(){
 return empty408476; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(start413816, "_greaterthan"), [stop413823]))) { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("range: start greater than stop: ("), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(start413816, "tostring"), [])]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(stop413823, "tostring"), [])])]), "_plus"), [RUNTIME.makeString(")")])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.makeString("if: no tests matched")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Creates a list of numbers, starting with start, ending with stop-1"));
var repeat414005 = RUNTIME.makeFunction(function (n359848414006,e359849414007) { return (function(){
 var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359851414010) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359851414010,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359848414006]);
var e411256 = e359849414007;
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359850414048) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359850414048,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [(function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n408864, "_greaterthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(link408481, [e411256,RUNTIME.applyFunc(repeat414005, [RUNTIME.applyFunc(RUNTIME.getField(n408864, "_minus"), [RUNTIME.makeNumber(1)]),e411256])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [n408864,RUNTIME.makeNumber(0)]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.makeString("repeat: can't have a negative argument'")]); 
})(); } })(); 
})()]); 
})(); }, RUNTIME.makeString("Creates a list with n copies of e"));
var filter414173 = RUNTIME.makeFunction(function (f359852414174,lst359853414175) { return (function(){
 var f410580 = f359852414174;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359854414184) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359854414184,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359853414175]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [lst408857]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f410580, [RUNTIME.getField(lst408857, "first")]))) { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.getField(lst408857, "first"),RUNTIME.applyFunc(filter414173, [f410580,RUNTIME.getField(lst408857, "rest")])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(filter414173, [f410580,RUNTIME.getField(lst408857, "rest")]); 
})(); } })(); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns the subset of lst for which f(elem) is true"));
var partition411599 = RUNTIME.makeFunction(function (f359855414316,lst359856414317) { return (function(){
 var f410580 = f359855414316;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359858414326) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359858414326,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359856414317]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (inner_DASH_lst359857414358) { return (function(){
 var inner_DASH_lst414360 = inner_DASH_lst359857414358;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [inner_DASH_lst414360]))) { return (function(){
 return RUNTIME.makeObject({'is-true':RUNTIME.getField(list410049, "empty"),'is-false':RUNTIME.getField(list410049, "empty")}); 
})(); } else { return (function(){
 var split_DASH_tail414396 = RUNTIME.applyFunc(help408897, [RUNTIME.getField(inner_DASH_lst414360, "rest")]);
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f410580, [RUNTIME.getField(inner_DASH_lst414360, "first")]))) { return (function(){
 return RUNTIME.makeObject({'is-true':RUNTIME.applyFunc(link408481, [RUNTIME.getField(inner_DASH_lst414360, "first"),RUNTIME.getField(split_DASH_tail414396, "is-true")]),'is-false':RUNTIME.getField(split_DASH_tail414396, "is-false")}); 
})(); } else { return (function(){
 return RUNTIME.makeObject({'is-true':RUNTIME.getField(split_DASH_tail414396, "is-true"),'is-false':RUNTIME.applyFunc(link408481, [RUNTIME.getField(inner_DASH_lst414360, "first"),RUNTIME.getField(split_DASH_tail414396, "is-false")])}); 
})(); } })(); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [lst408857]); 
})(); 
})(); }, RUNTIME.makeString("Splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"));
var any414558 = RUNTIME.makeFunction(function (f359859414559,lst359860414560) { return (function(){
 var f410580 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359862414563) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Function408264,contract359862414563,RUNTIME.makeString("(Any -> Bool)")]);
var fun359863414576 = contract359862414563;
return RUNTIME.makeFunction(function (arg359864414583) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359865414585) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359865414585,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun359863414576, [arg359864414583])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract359862414563, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [f359859414559]);
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359866414659) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359866414659,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359860414560]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359861414691) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359861414691,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [(function(){
 return RUNTIME.applyFunc(is_DASH_some414712, [RUNTIME.applyFunc(find411622, [f410580,lst408857])]); 
})()]); 
})(); }, RUNTIME.makeString("Returns true if f(elem) returns true for any elem of lst"));
var all414749 = RUNTIME.makeFunction(function (f359871414750,lst359872414751) { return (function(){
 var f410580 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359875414754) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Function408264,contract359875414754,RUNTIME.makeString("(Any -> Bool)")]);
var fun359876414767 = contract359875414754;
return RUNTIME.makeFunction(function (arg359877414774) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359878414776) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359878414776,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun359876414767, [arg359877414774])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract359875414754, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [f359871414750]);
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359879414850) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359879414850,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359872414751]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359874414882) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359874414882,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [(function(){
 return RUNTIME.applyFunc(is_DASH_none414903, [RUNTIME.applyFunc(find411622, [RUNTIME.makeFunction(function (v359873414904) { return (function(){
 var v409147 = v359873414904;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(f410580, [v409147]), "_not"), []); 
})(); 
})(); }, RUNTIME.makeString("")),lst408857])]); 
})()]); 
})(); }, RUNTIME.makeString("Returns true if f(elem) returns true for all elems of lst"));
var find411622 = RUNTIME.makeFunction(function (f359884414975,lst359885414976) { return (function(){
 var f410580 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract359887414979) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Function408264,contract359887414979,RUNTIME.makeString("(Any -> Bool)")]);
var fun359888414992 = contract359887414979;
return RUNTIME.makeFunction(function (arg359889414999) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359890415001) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Bool408274,specimen359890415001,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun359888414992, [arg359889414999])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract359887414979, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [f359884414975]);
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359891415075) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359891415075,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359885414976]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359886415107) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Option415109,specimen359886415107,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [(function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [lst408857]))) { return (function(){
 return none411036; 
})(); } else { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f410580, [RUNTIME.getField(lst408857, "first")]))) { return (function(){
 return RUNTIME.applyFunc(some415147, [RUNTIME.getField(lst408857, "first")]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(find411622, [f410580,RUNTIME.getField(lst408857, "rest")]); 
})(); } })(); 
})(); } })(); 
})()]); 
})(); }, RUNTIME.makeString("Returns some(elem) where elem is the first elem in lst for which\n        f(elem) returns true, or none otherwise"));
var map415224 = RUNTIME.makeFunction(function (f359898415225,lst359899415226) { return (function(){
 var f410580 = f359898415225;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359900415235) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359900415235,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359899415226]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [lst408857]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [RUNTIME.getField(lst408857, "first")]),RUNTIME.applyFunc(map415224, [f410580,RUNTIME.getField(lst408857, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(elem) for each elem in lst"));
var map2415335 = RUNTIME.makeFunction(function (f359901415336,l1359902415337,l2359903415338) { return (function(){
 var f410580 = f359901415336;
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359904415349) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359904415349,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1359902415337]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359905415382) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359905415382,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2359903415338]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first")]),RUNTIME.applyFunc(map2415335, [f410580,RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"));
var map3415516 = RUNTIME.makeFunction(function (f359906415517,l1359907415518,l2359908415519,l3359909415520) { return (function(){
 var f410580 = f359906415517;
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359910415531) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359910415531,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1359907415518]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359911415563) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359911415563,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2359908415519]);
var l3415595 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359912415596) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359912415596,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3359909415520]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first")]),RUNTIME.applyFunc(map3415516, [f410580,RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3"));
var map4415764 = RUNTIME.makeFunction(function (f359913415765,l1359914415766,l2359915415767,l3359916415768,l4359917415769) { return (function(){
 var f410580 = f359913415765;
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359918415781) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359918415781,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1359914415766]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359919415813) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359919415813,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2359915415767]);
var l3415595 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359920415845) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359920415845,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3359916415768]);
var l4415877 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359921415878) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359921415878,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l4359917415769]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l4415877]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first"),RUNTIME.getField(l4415877, "first")]),RUNTIME.applyFunc(map4415764, [f410580,RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest"),RUNTIME.getField(l4415877, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4"));
var map_n416080 = RUNTIME.makeFunction(function (f359922416081,n359923416082,lst359924416083) { return (function(){
 var f410580 = f359922416081;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359925416093) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359925416093,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359923416082]);
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359926416125) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359926416125,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359924416083]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [lst408857]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(lst408857, "first")]),RUNTIME.applyFunc(map_n416080, [f410580,RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(lst408857, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"));
var map2_n416238 = RUNTIME.makeFunction(function (f359927416239,n359928416240,l1359929416241,l2359930416242) { return (function(){
 var f410580 = f359927416239;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359931416253) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359931416253,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359928416240]);
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359932416285) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359932416285,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1359929416241]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359933416317) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359933416317,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2359930416242]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first")]),RUNTIME.applyFunc(map2_n416238, [f410580,RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var map3_n416464 = RUNTIME.makeFunction(function (f359934416465,n359935416466,l1359936416467,l2359937416468,l3359938416469) { return (function(){
 var f410580 = f359934416465;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359939416481) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359939416481,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359935416466]);
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359940416513) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359940416513,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1359936416467]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359941416545) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359941416545,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2359937416468]);
var l3415595 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359942416577) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359942416577,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3359938416469]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first")]),RUNTIME.applyFunc(map3_n416464, [f410580,RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var map4_n416758 = RUNTIME.makeFunction(function (f359943416759,n359944416760,l1359945416761,l2359946416762,l3359947416763,l4359948416764) { return (function(){
 var f410580 = f359943416759;
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359949416777) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359949416777,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n359944416760]);
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359950416809) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359950416809,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1359945416761]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359951416841) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359951416841,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2359946416762]);
var l3415595 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359952416873) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359952416873,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3359947416763]);
var l4415877 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359953416905) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359953416905,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l4359948416764]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l4415877]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty408476; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link408481, [RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first"),RUNTIME.getField(l4415877, "first")]),RUNTIME.applyFunc(map4415764, [f410580,RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest"),RUNTIME.getField(l4415877, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var each417120 = RUNTIME.makeFunction(function (f359954417121,lst359955417122) { return (function(){
 var f410580 = f359954417121;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359957417131) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359957417131,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359955417122]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l359956417163) { return (function(){
 var l408902 = l359956417163;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [l408902]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [RUNTIME.getField(l408902, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.getField(l408902, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [lst408857]); 
})(); 
})(); }, RUNTIME.makeString("Calls f for each elem in lst, and returns nothing"));
var each2417259 = RUNTIME.makeFunction(function (f359958417260,lst1359959417261,lst2359960417262) { return (function(){
 var f410580 = f359958417260;
var lst1417272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359963417273) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359963417273,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1359959417261]);
var lst2417305 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359964417306) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359964417306,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2359960417262]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l1359961417338,l2359962417339) { return (function(){
 var l1415348 = l1359961417338;
var l2415381 = l2359962417339;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [lst1417272,lst2417305]); 
})(); 
})(); }, RUNTIME.makeString("Calls f on each pair of corresponding elements in l1 and l2, and returns nothing.  Stops after the shortest list"));
var each3417477 = RUNTIME.makeFunction(function (f359965417478,lst1359966417479,lst2359967417480,lst3359968417481) { return (function(){
 var f410580 = f359965417478;
var lst1417272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359972417492) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359972417492,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1359966417479]);
var lst2417305 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359973417524) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359973417524,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2359967417480]);
var lst3417556 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359974417557) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359974417557,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3359968417481]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l1359969417589,l2359970417590,l3359971417591) { return (function(){
 var l1415348 = l1359969417589;
var l2415381 = l2359970417590;
var l3415595 = l3359971417591;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [lst1417272,lst2417305,lst3417556]); 
})(); 
})(); }, RUNTIME.makeString("Calls f on each triple of corresponding elements in l1, l2 and l3, and returns nothing.  Stops after the shortest list"));
var each4417771 = RUNTIME.makeFunction(function (f359975417772,lst1359976417773,lst2359977417774,lst3359978417775,lst4359979417776) { return (function(){
 var f410580 = f359975417772;
var lst1417272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359984417788) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359984417788,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1359976417773]);
var lst2417305 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359985417820) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359985417820,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2359977417774]);
var lst3417556 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359986417852) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359986417852,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3359978417775]);
var lst4417884 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359987417885) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359987417885,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst4359979417776]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (l1359980417917,l2359981417918,l3359982417919,l4359983417920) { return (function(){
 var l1415348 = l1359980417917;
var l2415381 = l2359981417918;
var l3415595 = l3359982417919;
var l4415877 = l4359983417920;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l4415877]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first"),RUNTIME.getField(l4415877, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest"),RUNTIME.getField(l4415877, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [lst1417272,lst2417305,lst3417556,lst4417884]); 
})(); 
})(); }, RUNTIME.makeString("Calls f on each tuple of corresponding elements in l1, l2, l3 and l4, and returns nothing.  Stops after the shortest list"));
var each_n418142 = RUNTIME.makeFunction(function (f359988418143,num359989418144,lst359990418145) { return (function(){
 var f410580 = f359988418143;
var num418155 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359993418156) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen359993418156,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num359989418144]);
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen359994418188) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen359994418188,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst359990418145]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (n359991418220,l359992418221) { return (function(){
 var n408864 = n359991418220;
var l408902 = l359992418221;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [l408902]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l408902, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l408902, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [num418155,lst408857]); 
})(); 
})(); }, RUNTIME.makeString(""));
var each2_n418338 = RUNTIME.makeFunction(function (f359995418339,num359996418340,lst1359997418341,lst2359998418342) { return (function(){
 var f410580 = f359995418339;
var num418155 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360002418353) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360002418353,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num359996418340]);
var lst1417272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360003418385) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360003418385,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1359997418341]);
var lst2417305 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360004418417) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360004418417,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2359998418342]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (n359999418449,l1360000418450,l2360001418451) { return (function(){
 var n408864 = n359999418449;
var l1415348 = l1360000418450;
var l2415381 = l2360001418451;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [num418155,lst1417272,lst2417305]); 
})(); 
})(); }, RUNTIME.makeString(""));
var each3_n418610 = RUNTIME.makeFunction(function (f360005418611,num360006418612,lst1360007418613,lst2360008418614,lst3360009418615) { return (function(){
 var f410580 = f360005418611;
var num418155 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360014418627) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360014418627,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num360006418612]);
var lst1417272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360015418659) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360015418659,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1360007418613]);
var lst2417305 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360016418691) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360016418691,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2360008418614]);
var lst3417556 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360017418723) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360017418723,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3360009418615]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (n360010418755,l1360011418756,l2360012418757,l3360013418758) { return (function(){
 var n408864 = n360010418755;
var l1415348 = l1360011418756;
var l2415381 = l2360012418757;
var l3415595 = l3360013418758;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [num418155,lst1417272,lst2417305,lst3417556]); 
})(); 
})(); }, RUNTIME.makeString(""));
var each4_n418959 = RUNTIME.makeFunction(function (f360018418960,num360019418961,lst1360020418962,lst2360021418963,lst3360022418964,lst4360023418965) { return (function(){
 var f410580 = f360018418960;
var num418155 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360029418978) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360029418978,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num360019418961]);
var lst1417272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360030419010) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360030419010,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1360020418962]);
var lst2417305 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360031419042) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360031419042,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2360021418963]);
var lst3417556 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360032419074) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360032419074,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3360022418964]);
var lst4417884 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360033419106) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360033419106,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst4360023418965]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (n360024419138,l1360025419139,l2360026419140,l3360027419141,l4360028419142) { return (function(){
 var n408864 = n360024419138;
var l1415348 = l1360025419139;
var l2415381 = l2360026419140;
var l3415595 = l3360027419141;
var l4415877 = l4360028419142;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l4415877]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing408224; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f410580, [n408864,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first"),RUNTIME.getField(l4415877, "first")]);
return RUNTIME.applyFunc(help408897, [RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest"),RUNTIME.getField(l4415877, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [num418155,lst1417272,lst2417305,lst3417556,lst4417884]); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold410006 = RUNTIME.makeFunction(function (f360034419385,base360035419386,lst360036419387) { return (function(){
 var f410580 = f360034419385;
var base411084 = base360035419386;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360037419403) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360037419403,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst360036419387]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [lst408857]))) { return (function(){
 return base411084; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold410006, [f410580,RUNTIME.applyFunc(f410580, [base411084,RUNTIME.getField(lst408857, "first")]),RUNTIME.getField(lst408857, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold2419499 = RUNTIME.makeFunction(function (f360038419500,base360039419501,l1360040419502,l2360041419503) { return (function(){
 var f410580 = f360038419500;
var base411084 = base360039419501;
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360042419520) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360042419520,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1360040419502]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360043419552) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360043419552,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2360041419503]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return base411084; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold2419499, [f410580,RUNTIME.applyFunc(f410580, [base411084,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first")]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold3419682 = RUNTIME.makeFunction(function (f360044419683,base360045419684,l1360046419685,l2360047419686,l3360048419687) { return (function(){
 var f410580 = f360044419683;
var base411084 = base360045419684;
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360049419705) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360049419705,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1360046419685]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360050419737) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360050419737,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2360047419686]);
var l3415595 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360051419769) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360051419769,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3360048419687]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return base411084; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold3419682, [f410580,RUNTIME.applyFunc(f410580, [base411084,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first")]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold4419933 = RUNTIME.makeFunction(function (f360052419934,base360053419935,l1360054419936,l2360055419937,l3360056419938,l4360057419939) { return (function(){
 var f410580 = f360052419934;
var base411084 = base360053419935;
var l1415348 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360058419958) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360058419958,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1360054419936]);
var l2415381 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360059419990) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360059419990,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2360055419937]);
var l3415595 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360060420022) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360060420022,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3360056419938]);
var l4415877 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360061420054) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360061420054,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l4360057419939]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty408940, [l1415348]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l2415381]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l3415595]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty408940, [l4415877]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return base411084; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold4419933, [f410580,RUNTIME.applyFunc(f410580, [base411084,RUNTIME.getField(l1415348, "first"),RUNTIME.getField(l2415381, "first"),RUNTIME.getField(l3415595, "first"),RUNTIME.getField(l4415877, "first")]),RUNTIME.getField(l1415348, "rest"),RUNTIME.getField(l2415381, "rest"),RUNTIME.getField(l3415595, "rest"),RUNTIME.getField(l4415877, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold_n420252 = RUNTIME.makeFunction(function (f360062420253,num360063420254,base360064420255,lst360065420256) { return (function(){
 var f410580 = f360062420253;
var num418155 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360069420267) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360069420267,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num360063420254]);
var base411084 = base360064420255;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360070420305) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360070420305,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst360065420256]);
return (function(){
 var help408897 = RUNTIME.makeFunction(function (n360066420337,acc360067420338,partial_DASH_list360068420339) { return (function(){
 var n408864 = n360066420337;
var acc409382 = acc360067420338;
var partial_DASH_list420355 = partial_DASH_list360068420339;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [partial_DASH_list420355]))) { return (function(){
 return acc409382; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help408897, [RUNTIME.applyFunc(RUNTIME.getField(n408864, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(f410580, [n408864,base411084,RUNTIME.getField(partial_DASH_list420355, "first")]),RUNTIME.getField(partial_DASH_list420355, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help408897, [RUNTIME.makeNumber(0),base411084,lst408857]); 
})(); 
})(); }, RUNTIME.makeString(""));
var raw_DASH_fold412105 = RUNTIME.makeFunction(function (f360071420467,base360072420468,lst360073420469) { return (function(){
 var f410580 = f360071420467;
var base411084 = base360072420468;
var lst408857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360074420485) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360074420485,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst360073420469]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty408940, [lst408857]))) { return (function(){
 return base411084; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raw_DASH_fold412105, [f410580,RUNTIME.applyFunc(f410580, [base411084,RUNTIME.getRawField(lst408857, 'first')]),RUNTIME.getField(lst408857, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var index420581 = RUNTIME.makeFunction(function (l360075420582,n360076420583) { return (function(){
 var l408902 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360080420586) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360080420586,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l360075420582]);
var n408864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360081420618) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360081420618,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n360076420583]);
return (function(){
 var cases_DASH_value359494420650 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360077420651) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360077420651,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l408902]);
return RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value359494420650, "_match"), [RUNTIME.makeObject({'empty':(function() { throw new Error('Not yet implemented s_hint') })(),'link':(function() { throw new Error('Not yet implemented s_hint') })()}),(function() { throw new Error('Not yet implemented s_hint') })()]); 
})(); 
})(); }, RUNTIME.makeString(""));
var list410049 = RUNTIME.makeObject({'List':List409392,'is-empty':is_DASH_empty408940,'is-link':is_DASH_link409536,'empty':empty408476,'link':link408481,'range':range413811,'repeat':repeat414005,'filter':filter414173,'partition':partition411599,'any':any414558,'all':all414749,'find':find411622,'map':map415224,'map2':map2415335,'map3':map3415516,'map4':map4415764,'map_n':map_n416080,'map2_n':map2_n416238,'map3_n':map3_n416464,'map4_n':map4_n416758,'each':each417120,'each2':each2417259,'each3':each3417477,'each4':each4417771,'each_n':each_n418142,'each2_n':each2_n418338,'each3_n':each3_n418610,'each4_n':each4_n418959,'fold':fold410006,'fold2':fold2419499,'fold3':fold3419682,'fold4':fold4419933,'fold_n':fold_n420252,'index':index420581,'to-set':list_DASH_to_DASH_set409971});
var Location_DASH_mixins359352420920 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359353420931 = RUNTIME.makeObject({});
var variant359354420940 = data_DASH_shared359353420931.extend('_equals', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_location420943, [other408672]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(self408671, "file"),RUNTIME.getField(other408672, "file")]); 
})(); }, RUNTIME.makeString(""))]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(self408671, "line"),RUNTIME.getField(other408672, "line")]); 
})(); }, RUNTIME.makeString(""))]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(self408671, "column"),RUNTIME.getField(other408672, "column")]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))).extend('format', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "file"), "tostring"), []), "_plus"), [RUNTIME.makeString(": line ")]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "line"), "tostring"), []), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", column "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "column"), "tostring"), [])])])]); 
})(); }, RUNTIME.makeString(""))).extend('tostring', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self408671, "format"), []); 
})(); }, RUNTIME.makeString("")));
var Location359495421194 = RUNTIME.applyFunc(brander408259, []);
var location_base359498421205 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("file"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("line"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("column"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_location420943,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("file"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("line"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("column"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("location")]))) { return (function(){
 var call_DASH_location359496421349 = RUNTIME.getField(cases_DASH_funs412779, "location");
return RUNTIME.applyFunc(call_DASH_location359496421349, [RUNTIME.getField(self408671, "file"),RUNTIME.getField(self408671, "line"),RUNTIME.getField(self408671, "column")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var location359497421418 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_location420943 = RUNTIME.getField(location359497421418, "test");
var location421439 = RUNTIME.makeFunction(function (file359499360082421440,line359500360083421441,column359501360084421442) { return (function(){
 var file359499421446 = file359499360082421440;
var line359500421453 = line359500360083421441;
var column359501421460 = column359501360084421442;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Location359495421194, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(location359497421418, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360085421475) { return (function(){
 var self408671 = self360085421475;
return (function(){
 var mixin359355421483 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Location_DASH_mixins359352420920]))) { return RUNTIME.applyFunc(Location_DASH_mixins359352420920, []); } else { return Location_DASH_mixins359352420920; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359355421483, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359355421483, "extend"), [self408671.extend('_equals', RUNTIME.getRawField(variant359354420940, '_equals')).extend('format', RUNTIME.getRawField(variant359354420940, 'format')).extend('tostring', RUNTIME.getRawField(variant359354420940, 'tostring'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for location")), [location_base359498421205.extend('file', file359499421446).extend('line', line359500421453).extend('column', column359501421460)])])]); 
})(); 
})(); }, RUNTIME.makeString("location: Creates an instance of location"));
var Location421612 = RUNTIME.getField(Location359495421194, "test");
var Error_DASH_mixins359356421623 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359357421634 = RUNTIME.makeObject({'tostring':RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self408671, "format"), []); 
})(); }, RUNTIME.makeString("")),'format':RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "location"), "format"), []), "append"), [RUNTIME.makeString(":\n")]), "append"), [RUNTIME.applyFunc(RUNTIME.getField(self408671, "name"), [])]), "append"), [RUNTIME.makeString(": ")]), "append"), [RUNTIME.getField(self408671, "message")]); 
})(); }, RUNTIME.makeString(""))});
var variant359358421747 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Error using opaque internal value"); 
})(); }, RUNTIME.makeString("")));
var variant359359421771 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Field not found"); 
})(); }, RUNTIME.makeString("")));
var variant359360421795 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Non-string in field name"); 
})(); }, RUNTIME.makeString("")));
var variant359361421819 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("No cases matched"); 
})(); }, RUNTIME.makeString("")));
var variant359362421843 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Invalid case"); 
})(); }, RUNTIME.makeString("")));
var variant359363421867 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Eval Error"); 
})(); }, RUNTIME.makeString("")));
var variant359364421891 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Contract failure"); 
})(); }, RUNTIME.makeString("")));
var variant359365421915 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Arity mismatch"); 
})(); }, RUNTIME.makeString("")));
var variant359366421939 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Division by zero"); 
})(); }, RUNTIME.makeString("")));
var variant359367421963 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Type Error"); 
})(); }, RUNTIME.makeString("")));
var variant359368421987 = data_DASH_shared359357421634.extend('name', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.makeString("Error"); 
})(); }, RUNTIME.makeString("")));
var Error359502422011 = RUNTIME.applyFunc(brander408259, []);
var opaque_DASH_error_base359505422022 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("opaque-error"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_opaque_DASH_error422093,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("opaque-error")]))) { return (function(){
 var call_DASH_opaque_DASH_error359503422167 = RUNTIME.getField(cases_DASH_funs412779, "opaque-error");
return RUNTIME.applyFunc(call_DASH_opaque_DASH_error359503422167, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var opaque_DASH_error359504422236 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_opaque_DASH_error422093 = RUNTIME.getField(opaque_DASH_error359504422236, "test");
var opaque_DASH_error422257 = RUNTIME.makeFunction(function (message359506360086422258,location359507360087422259,trace359508360088422260) { return (function(){
 var message359506422264 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360090422265) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360090422265,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359506360086422258]);
var location359507422297 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360091422298) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360091422298,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359507360087422259]);
var trace359508422330 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360092422331) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360092422331,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359508360088422260]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(opaque_DASH_error359504422236, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360089422371) { return (function(){
 var self408671 = self360089422371;
return (function(){
 var mixin359369422379 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359369422379, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359369422379, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359358421747, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for opaque-error")), [opaque_DASH_error_base359505422022.extend('message', message359506422264).extend('location', location359507422297).extend('trace', trace359508422330)])])]); 
})(); 
})(); }, RUNTIME.makeString("opaque-error: Creates an instance of opaque-error"));
var field_DASH_not_DASH_found_base359511422508 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("field-not-found"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_field_DASH_not_DASH_found422579,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("field-not-found")]))) { return (function(){
 var call_DASH_field_DASH_not_DASH_found359509422653 = RUNTIME.getField(cases_DASH_funs412779, "field-not-found");
return RUNTIME.applyFunc(call_DASH_field_DASH_not_DASH_found359509422653, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var field_DASH_not_DASH_found359510422722 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_field_DASH_not_DASH_found422579 = RUNTIME.getField(field_DASH_not_DASH_found359510422722, "test");
var field_DASH_not_DASH_found422743 = RUNTIME.makeFunction(function (message359512360093422744,location359513360094422745,trace359514360095422746) { return (function(){
 var message359512422750 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360097422751) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360097422751,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359512360093422744]);
var location359513422783 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360098422784) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360098422784,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359513360094422745]);
var trace359514422816 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360099422817) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360099422817,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359514360095422746]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(field_DASH_not_DASH_found359510422722, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360096422857) { return (function(){
 var self408671 = self360096422857;
return (function(){
 var mixin359370422865 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359370422865, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359370422865, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359359421771, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for field-not-found")), [field_DASH_not_DASH_found_base359511422508.extend('message', message359512422750).extend('location', location359513422783).extend('trace', trace359514422816)])])]); 
})(); 
})(); }, RUNTIME.makeString("field-not-found: Creates an instance of field-not-found"));
var field_DASH_non_DASH_string_base359517422994 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("field-non-string"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_field_DASH_non_DASH_string423065,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("field-non-string")]))) { return (function(){
 var call_DASH_field_DASH_non_DASH_string359515423139 = RUNTIME.getField(cases_DASH_funs412779, "field-non-string");
return RUNTIME.applyFunc(call_DASH_field_DASH_non_DASH_string359515423139, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var field_DASH_non_DASH_string359516423208 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_field_DASH_non_DASH_string423065 = RUNTIME.getField(field_DASH_non_DASH_string359516423208, "test");
var field_DASH_non_DASH_string423229 = RUNTIME.makeFunction(function (message359518360100423230,location359519360101423231,trace359520360102423232) { return (function(){
 var message359518423236 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360104423237) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360104423237,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359518360100423230]);
var location359519423269 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360105423270) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360105423270,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359519360101423231]);
var trace359520423302 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360106423303) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360106423303,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359520360102423232]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(field_DASH_non_DASH_string359516423208, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360103423343) { return (function(){
 var self408671 = self360103423343;
return (function(){
 var mixin359371423351 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359371423351, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359371423351, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359360421795, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for field-non-string")), [field_DASH_non_DASH_string_base359517422994.extend('message', message359518423236).extend('location', location359519423269).extend('trace', trace359520423302)])])]); 
})(); 
})(); }, RUNTIME.makeString("field-non-string: Creates an instance of field-non-string"));
var cases_DASH_miss_base359523423480 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("cases-miss"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_cases_DASH_miss423551,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("cases-miss")]))) { return (function(){
 var call_DASH_cases_DASH_miss359521423625 = RUNTIME.getField(cases_DASH_funs412779, "cases-miss");
return RUNTIME.applyFunc(call_DASH_cases_DASH_miss359521423625, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var cases_DASH_miss359522423694 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_cases_DASH_miss423551 = RUNTIME.getField(cases_DASH_miss359522423694, "test");
var cases_DASH_miss423715 = RUNTIME.makeFunction(function (message359524360107423716,location359525360108423717,trace359526360109423718) { return (function(){
 var message359524423722 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360111423723) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360111423723,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359524360107423716]);
var location359525423755 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360112423756) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360112423756,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359525360108423717]);
var trace359526423788 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360113423789) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360113423789,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359526360109423718]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_miss359522423694, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360110423829) { return (function(){
 var self408671 = self360110423829;
return (function(){
 var mixin359372423837 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359372423837, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359372423837, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359361421819, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for cases-miss")), [cases_DASH_miss_base359523423480.extend('message', message359524423722).extend('location', location359525423755).extend('trace', trace359526423788)])])]); 
})(); 
})(); }, RUNTIME.makeString("cases-miss: Creates an instance of cases-miss"));
var invalid_DASH_case_base359529423966 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("invalid-case"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_invalid_DASH_case424037,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("invalid-case")]))) { return (function(){
 var call_DASH_invalid_DASH_case359527424111 = RUNTIME.getField(cases_DASH_funs412779, "invalid-case");
return RUNTIME.applyFunc(call_DASH_invalid_DASH_case359527424111, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var invalid_DASH_case359528424180 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_invalid_DASH_case424037 = RUNTIME.getField(invalid_DASH_case359528424180, "test");
var invalid_DASH_case424201 = RUNTIME.makeFunction(function (message359530360114424202,location359531360115424203,trace359532360116424204) { return (function(){
 var message359530424208 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360118424209) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360118424209,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359530360114424202]);
var location359531424241 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360119424242) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360119424242,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359531360115424203]);
var trace359532424274 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360120424275) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360120424275,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359532360116424204]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(invalid_DASH_case359528424180, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360117424315) { return (function(){
 var self408671 = self360117424315;
return (function(){
 var mixin359373424323 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359373424323, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359373424323, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359362421843, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for invalid-case")), [invalid_DASH_case_base359529423966.extend('message', message359530424208).extend('location', location359531424241).extend('trace', trace359532424274)])])]); 
})(); 
})(); }, RUNTIME.makeString("invalid-case: Creates an instance of invalid-case"));
var eval_DASH_error_base359535424452 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("eval-error"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_eval_DASH_error424523,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("eval-error")]))) { return (function(){
 var call_DASH_eval_DASH_error359533424597 = RUNTIME.getField(cases_DASH_funs412779, "eval-error");
return RUNTIME.applyFunc(call_DASH_eval_DASH_error359533424597, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var eval_DASH_error359534424666 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_eval_DASH_error424523 = RUNTIME.getField(eval_DASH_error359534424666, "test");
var eval_DASH_error424687 = RUNTIME.makeFunction(function (message359536360121424688,location359537360122424689,trace359538360123424690) { return (function(){
 var message359536424694 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360125424695) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360125424695,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359536360121424688]);
var location359537424727 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360126424728) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360126424728,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359537360122424689]);
var trace359538424760 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360127424761) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360127424761,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359538360123424690]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(eval_DASH_error359534424666, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360124424801) { return (function(){
 var self408671 = self360124424801;
return (function(){
 var mixin359374424809 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359374424809, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359374424809, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359363421867, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for eval-error")), [eval_DASH_error_base359535424452.extend('message', message359536424694).extend('location', location359537424727).extend('trace', trace359538424760)])])]); 
})(); 
})(); }, RUNTIME.makeString("eval-error: Creates an instance of eval-error"));
var user_DASH_contract_DASH_failure_base359541424938 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("user-contract-failure"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_user_DASH_contract_DASH_failure425009,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("user-contract-failure")]))) { return (function(){
 var call_DASH_user_DASH_contract_DASH_failure359539425083 = RUNTIME.getField(cases_DASH_funs412779, "user-contract-failure");
return RUNTIME.applyFunc(call_DASH_user_DASH_contract_DASH_failure359539425083, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var user_DASH_contract_DASH_failure359540425152 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_user_DASH_contract_DASH_failure425009 = RUNTIME.getField(user_DASH_contract_DASH_failure359540425152, "test");
var user_DASH_contract_DASH_failure425173 = RUNTIME.makeFunction(function (message359542360128425174,location359543360129425175,trace359544360130425176) { return (function(){
 var message359542425180 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360132425181) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360132425181,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359542360128425174]);
var location359543425213 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360133425214) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360133425214,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359543360129425175]);
var trace359544425246 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360134425247) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360134425247,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359544360130425176]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(user_DASH_contract_DASH_failure359540425152, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360131425287) { return (function(){
 var self408671 = self360131425287;
return (function(){
 var mixin359375425295 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359375425295, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359375425295, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359364421891, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for user-contract-failure")), [user_DASH_contract_DASH_failure_base359541424938.extend('message', message359542425180).extend('location', location359543425213).extend('trace', trace359544425246)])])]); 
})(); 
})(); }, RUNTIME.makeString("user-contract-failure: Creates an instance of user-contract-failure"));
var arity_DASH_error_base359547425424 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("arity-error"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_arity_DASH_error425495,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("arity-error")]))) { return (function(){
 var call_DASH_arity_DASH_error359545425569 = RUNTIME.getField(cases_DASH_funs412779, "arity-error");
return RUNTIME.applyFunc(call_DASH_arity_DASH_error359545425569, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var arity_DASH_error359546425638 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_arity_DASH_error425495 = RUNTIME.getField(arity_DASH_error359546425638, "test");
var arity_DASH_error425659 = RUNTIME.makeFunction(function (message359548360135425660,location359549360136425661,trace359550360137425662) { return (function(){
 var message359548425666 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360139425667) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360139425667,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359548360135425660]);
var location359549425699 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360140425700) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360140425700,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359549360136425661]);
var trace359550425732 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360141425733) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360141425733,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359550360137425662]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(arity_DASH_error359546425638, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360138425773) { return (function(){
 var self408671 = self360138425773;
return (function(){
 var mixin359376425781 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359376425781, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359376425781, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359365421915, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for arity-error")), [arity_DASH_error_base359547425424.extend('message', message359548425666).extend('location', location359549425699).extend('trace', trace359550425732)])])]); 
})(); 
})(); }, RUNTIME.makeString("arity-error: Creates an instance of arity-error"));
var div_DASH_0_base359553425910 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("div-0"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_div_DASH_0425981,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("div-0")]))) { return (function(){
 var call_DASH_div_DASH_0359551426055 = RUNTIME.getField(cases_DASH_funs412779, "div-0");
return RUNTIME.applyFunc(call_DASH_div_DASH_0359551426055, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var div_DASH_0359552426124 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_div_DASH_0425981 = RUNTIME.getField(div_DASH_0359552426124, "test");
var div_DASH_0426145 = RUNTIME.makeFunction(function (message359554360142426146,location359555360143426147,trace359556360144426148) { return (function(){
 var message359554426152 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360146426153) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360146426153,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359554360142426146]);
var location359555426185 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360147426186) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360147426186,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359555360143426147]);
var trace359556426218 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360148426219) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360148426219,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359556360144426148]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(div_DASH_0359552426124, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360145426259) { return (function(){
 var self408671 = self360145426259;
return (function(){
 var mixin359377426267 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359377426267, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359377426267, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359366421939, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for div-0")), [div_DASH_0_base359553425910.extend('message', message359554426152).extend('location', location359555426185).extend('trace', trace359556426218)])])]); 
})(); 
})(); }, RUNTIME.makeString("div-0: Creates an instance of div-0"));
var type_DASH_error_base359559426396 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("type-error"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_type_DASH_error426467,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("type-error")]))) { return (function(){
 var call_DASH_type_DASH_error359557426541 = RUNTIME.getField(cases_DASH_funs412779, "type-error");
return RUNTIME.applyFunc(call_DASH_type_DASH_error359557426541, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var type_DASH_error359558426610 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_type_DASH_error426467 = RUNTIME.getField(type_DASH_error359558426610, "test");
var type_DASH_error426631 = RUNTIME.makeFunction(function (message359560360149426632,location359561360150426633,trace359562360151426634) { return (function(){
 var message359560426638 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360153426639) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360153426639,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359560360149426632]);
var location359561426671 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360154426672) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360154426672,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359561360150426633]);
var trace359562426704 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360155426705) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360155426705,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359562360151426634]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(type_DASH_error359558426610, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360152426745) { return (function(){
 var self408671 = self360152426745;
return (function(){
 var mixin359378426753 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359378426753, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359378426753, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359367421963, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for type-error")), [type_DASH_error_base359559426396.extend('message', message359560426638).extend('location', location359561426671).extend('trace', trace359562426704)])])]); 
})(); 
})(); }, RUNTIME.makeString("type-error: Creates an instance of type-error"));
var lazy_DASH_error_base359565426882 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("lazy-error"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_lazy_DASH_error426953,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("lazy-error")]))) { return (function(){
 var call_DASH_lazy_DASH_error359563427027 = RUNTIME.getField(cases_DASH_funs412779, "lazy-error");
return RUNTIME.applyFunc(call_DASH_lazy_DASH_error359563427027, [RUNTIME.getField(self408671, "message"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var lazy_DASH_error359564427096 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_lazy_DASH_error426953 = RUNTIME.getField(lazy_DASH_error359564427096, "test");
var lazy_DASH_error427117 = RUNTIME.makeFunction(function (message359566360156427118,location359567360157427119,trace359568360158427120) { return (function(){
 var message359566427124 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360160427125) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360160427125,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message359566360156427118]);
var location359567427157 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360161427158) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360161427158,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359567360157427119]);
var trace359568427190 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360162427191) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360162427191,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace359568360158427120]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error359502422011, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(lazy_DASH_error359564427096, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360159427231) { return (function(){
 var self408671 = self360159427231;
return (function(){
 var mixin359379427239 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Error_DASH_mixins359356421623]))) { return RUNTIME.applyFunc(Error_DASH_mixins359356421623, []); } else { return Error_DASH_mixins359356421623; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359379427239, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359379427239, "extend"), [self408671.extend('tostring', RUNTIME.getRawField(data_DASH_shared359357421634, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared359357421634, 'format')).extend('name', RUNTIME.getRawField(variant359368421987, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for lazy-error")), [lazy_DASH_error_base359565426882.extend('message', message359566427124).extend('location', location359567427157).extend('trace', trace359568427190)])])]); 
})(); 
})(); }, RUNTIME.makeString("lazy-error: Creates an instance of lazy-error"));
var Error427368 = RUNTIME.getField(Error359502422011, "test");
var make_DASH_error427379 = RUNTIME.makeFunction(function (obj360163427380) { return (function(){
 var obj408465 = obj360163427380;
return (function(){
 var trace427388 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field408333, [obj408465,RUNTIME.makeString("trace")]))) { return (function(){
 return RUNTIME.applyFunc(map415224, [RUNTIME.makeFunction(function (l360164427397) { return (function(){
 var l408902 = l360164427397;
return (function(){
 return RUNTIME.applyFunc(location421439, [RUNTIME.getField(l408902, "path"),RUNTIME.getField(l408902, "line"),RUNTIME.getField(l408902, "column")]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(mklist408462, [RUNTIME.getField(obj408465, "trace")])]); 
})(); } else { return (function(){
 return RUNTIME.getField(list410049, "empty"); 
})(); } })();
var loc427481 = RUNTIME.applyFunc(location421439, [RUNTIME.getField(obj408465, "path"),RUNTIME.getField(obj408465, "line"),RUNTIME.getField(obj408465, "column")]);
return (function() { if (RUNTIME.isTrue(RUNTIME.getField(obj408465, "system"))) { return (function(){
 var type427511 = RUNTIME.getField(RUNTIME.getField(obj408465, "value"), "type");
var msg427526 = RUNTIME.getField(RUNTIME.getField(obj408465, "value"), "message");
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("opaque")]))) { return (function(){
 return RUNTIME.applyFunc(opaque_DASH_error422257, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("field-not-found")]))) { return (function(){
 return RUNTIME.applyFunc(field_DASH_not_DASH_found422743, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("field-non-string")]))) { return (function(){
 return RUNTIME.applyFunc(field_DASH_non_DASH_string423229, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("user-contract-failure")]))) { return (function(){
 return RUNTIME.applyFunc(user_DASH_contract_DASH_failure425173, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("eval-error")]))) { return (function(){
 return RUNTIME.applyFunc(eval_DASH_error424687, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("arity-mismatch")]))) { return (function(){
 return RUNTIME.applyFunc(arity_DASH_error425659, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("div-0")]))) { return (function(){
 return RUNTIME.applyFunc(div_DASH_0426145, [msg427526,loc427481,trace427388]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [type427511,RUNTIME.makeString("type-error")]))) { return (function(){
 return RUNTIME.applyFunc(type_DASH_error426631, [msg427526,loc427481,trace427388]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(lazy_DASH_error427117, [msg427526,loc427481,trace427388]); 
})(); } })(); 
})(); } else { return (function(){
 return RUNTIME.getField(obj408465, "value"); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var error427816 = RUNTIME.makeObject({'opaque-error':opaque_DASH_error422257,'is-opaque-error':is_DASH_opaque_DASH_error422093,'field-not-found':field_DASH_not_DASH_found422743,'is-field-not-found':is_DASH_field_DASH_not_DASH_found422579,'cases-miss':cases_DASH_miss423715,'is-cases-miss':is_DASH_cases_DASH_miss423551,'invalid-case':invalid_DASH_case424201,'is-invalid-case':is_DASH_invalid_DASH_case424037,'user-contract-failure':user_DASH_contract_DASH_failure425173,'is-user-contract-failure':is_DASH_user_DASH_contract_DASH_failure425009,'div-0':div_DASH_0426145,'is-div-0':is_DASH_div_DASH_0425981,'make-error':make_DASH_error427379,'Error':Error427368,'Location':Location421612,'location':location421439,'is-location':is_DASH_location420943});
var Set_DASH_mixins359380427910 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359381427921 = RUNTIME.makeObject({});
var variant359382427930 = data_DASH_shared359381427921.extend('member', RUNTIME.makeMethod(function (self408671,elem410018) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "elems"), "member"), [elem410018]); 
})(); }, RUNTIME.makeString("Check to see if an element is in a set."))).extend('add', RUNTIME.makeMethod(function (self408671,elem410018) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "elems"), "member"), [elem410018]))) { return (function(){
 return self408671; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(__set410048, [RUNTIME.applyFunc(link408481, [elem410018,RUNTIME.getField(self408671, "elems")])]); 
})(); } })(); 
})(); }, RUNTIME.makeString("Add an element to the set if it is not already present."))).extend('remove', RUNTIME.makeMethod(function (self408671,elem410018) { return (function(){
 return RUNTIME.applyFunc(__set410048, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "elems"), "filter"), [RUNTIME.makeFunction(function (x360165428032) { return (function(){
 var x428034 = x360165428032;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [x428034,elem410018]), "_not"), []); 
})(); 
})(); }, RUNTIME.makeString(""))])]); 
})(); }, RUNTIME.makeString("Remove an element from the set if it is present."))).extend('to-list', RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "elems"), "sort"), []); 
})(); }, RUNTIME.makeString("Convert a set into a sorted list of elements."))).extend('union', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract360166428124) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand408229, [Method408294,contract360166428124,RUNTIME.makeString("(Any, Set => Any)")]);
var fun360167428137 = RUNTIME.applyFunc(RUNTIME.getField(contract360166428124, "_fun"), []);
return RUNTIME.makeMethod(function (arg360168428152,arg360169428153) { return (function(){
 return RUNTIME.applyFunc(fun360167428137, [arg360168428152,RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360170428156) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Set428158,specimen360170428156,RUNTIME.makeString("Set")]); 
})(); }, RUNTIME.makeString("internal contract for Set")), [arg360169428153])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, Set => Any)")).extend('_doc', RUNTIME.getField(contract360166428124, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, Set => Any)")), [RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(list_DASH_to_DASH_set409971, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(self408671, "to-list"), []), "append"), [RUNTIME.applyFunc(RUNTIME.getField(other408672, "to-list"), [])])]); 
})(); }, RUNTIME.makeString("Take the union of two sets."))])).extend('_equals', RUNTIME.makeMethod(function (self408671,other408672) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(Set428158, [other408672]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self408671, "elems"), "sort"), []),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(other408672, "elems"), "sort"), [])]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString("")));
var Set359569428352 = RUNTIME.applyFunc(brander408259, []);
var __set_base359572428363 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("__set"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("elems"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH___set428410,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("elems"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("__set")]))) { return (function(){
 var call_DASH___set359570428460 = RUNTIME.getField(cases_DASH_funs412779, "__set");
return RUNTIME.applyFunc(call_DASH___set359570428460, [RUNTIME.getField(self408671, "elems")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var __set359571428519 = RUNTIME.applyFunc(brander408259, []);
var is_DASH___set428410 = RUNTIME.getField(__set359571428519, "test");
var __set410048 = RUNTIME.makeFunction(function (elems359573360171428540) { return (function(){
 var elems359573428542 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360173428543) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360173428543,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [elems359573360171428540]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Set359569428352, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(__set359571428519, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360172428583) { return (function(){
 var self408671 = self360172428583;
return (function(){
 var mixin359383428591 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Set_DASH_mixins359380427910]))) { return RUNTIME.applyFunc(Set_DASH_mixins359380427910, []); } else { return Set_DASH_mixins359380427910; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359383428591, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359383428591, "extend"), [self408671.extend('member', RUNTIME.getRawField(variant359382427930, 'member')).extend('add', RUNTIME.getRawField(variant359382427930, 'add')).extend('remove', RUNTIME.getRawField(variant359382427930, 'remove')).extend('to-list', RUNTIME.getRawField(variant359382427930, 'to-list')).extend('union', RUNTIME.getRawField(variant359382427930, 'union')).extend('_equals', RUNTIME.getRawField(variant359382427930, '_equals'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for __set")), [__set_base359572428363.extend('elems', elems359573428542)])])]); 
})(); 
})(); }, RUNTIME.makeString("__set: Creates an instance of __set"));
var Set428158 = RUNTIME.getField(Set359569428352, "test");
var sets428746 = RUNTIME.makeObject({'Set':Set428158,'set':list_DASH_to_DASH_set409971});
var Option_DASH_mixins359384428765 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359385428776 = RUNTIME.makeObject({});
var variant359386428785 = data_DASH_shared359385428776.extend('orelse', RUNTIME.makeMethod(function (self408671,v409147) { return (function(){
 return v409147; 
})(); }, RUNTIME.makeString("Return the default provided value"))).extend('andthen', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return self408671; 
})(); }, RUNTIME.makeString("")));
var variant359387428824 = data_DASH_shared359385428776.extend('orelse', RUNTIME.makeMethod(function (self408671,v409147) { return (function(){
 return RUNTIME.getField(self408671, "value"); 
})(); }, RUNTIME.makeString("Return self.value, rather than the default"))).extend('andthen', RUNTIME.makeMethod(function (self408671,f410580) { return (function(){
 return RUNTIME.applyFunc(f410580, [RUNTIME.getField(self408671, "value")]); 
})(); }, RUNTIME.makeString("")));
var Option359574428876 = RUNTIME.applyFunc(brander408259, []);
var none_base359577428887 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("none"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_none414903,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("none")]))) { return (function(){
 var call_DASH_none359576428944 = RUNTIME.getField(cases_DASH_funs412779, "none");
return RUNTIME.applyFunc(call_DASH_none359576428944, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var none359575428998 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_none414903 = RUNTIME.getField(none359575428998, "test");
var none411036 = RUNTIME.applyFunc(RUNTIME.getField(Option359574428876, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(none359575428998, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360174429027) { return (function(){
 var self408671 = self360174429027;
return (function(){
 var mixin359388429035 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Option_DASH_mixins359384428765]))) { return RUNTIME.applyFunc(Option_DASH_mixins359384428765, []); } else { return Option_DASH_mixins359384428765; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359388429035, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359388429035, "extend"), [self408671.extend('orelse', RUNTIME.getRawField(variant359386428785, 'orelse')).extend('andthen', RUNTIME.getRawField(variant359386428785, 'andthen'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for none")), [none_base359577428887])])]);
var some_base359580429130 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("some"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_some414712,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("some")]))) { return (function(){
 var call_DASH_some359578429226 = RUNTIME.getField(cases_DASH_funs412779, "some");
return RUNTIME.applyFunc(call_DASH_some359578429226, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var some359579429285 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_some414712 = RUNTIME.getField(some359579429285, "test");
var some415147 = RUNTIME.makeFunction(function (value359581360175429306) { return (function(){
 var value359581429308 = value359581360175429306;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Option359574428876, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(some359579429285, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360176429323) { return (function(){
 var self408671 = self360176429323;
return (function(){
 var mixin359389429331 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Option_DASH_mixins359384428765]))) { return RUNTIME.applyFunc(Option_DASH_mixins359384428765, []); } else { return Option_DASH_mixins359384428765; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359389429331, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359389429331, "extend"), [self408671.extend('orelse', RUNTIME.getRawField(variant359387428824, 'orelse')).extend('andthen', RUNTIME.getRawField(variant359387428824, 'andthen'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for some")), [some_base359580429130.extend('value', value359581429308)])])]); 
})(); 
})(); }, RUNTIME.makeString("some: Creates an instance of some"));
var Option415109 = RUNTIME.getField(Option359574428876, "test");
var option429454 = RUNTIME.makeObject({'Option':Option415109,'is-none':is_DASH_none414903,'is-some':is_DASH_some414712,'none':none411036,'some':some415147});
var CheckResult_DASH_mixins359390429488 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359391429499 = RUNTIME.makeObject({});
var variant359392429508 = data_DASH_shared359391429499;
var variant359393429515 = data_DASH_shared359391429499;
var variant359394429522 = data_DASH_shared359391429499;
var CheckResult359582429529 = RUNTIME.applyFunc(brander408259, []);
var success_base359585429540 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("success"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_success429599,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("success")]))) { return (function(){
 var call_DASH_success359583429661 = RUNTIME.getField(cases_DASH_funs412779, "success");
return RUNTIME.applyFunc(call_DASH_success359583429661, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "location")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var success359584429725 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_success429599 = RUNTIME.getField(success359584429725, "test");
var success429746 = RUNTIME.makeFunction(function (name359586360177429747,location359587360178429748) { return (function(){
 var name359586429751 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360180429752) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360180429752,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359586360177429747]);
var location359587429784 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360181429785) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Option415109,specimen360181429785,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [location359587360178429748]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResult359582429529, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(success359584429725, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360179429825) { return (function(){
 var self408671 = self360179429825;
return (function(){
 var mixin359395429833 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [CheckResult_DASH_mixins359390429488]))) { return RUNTIME.applyFunc(CheckResult_DASH_mixins359390429488, []); } else { return CheckResult_DASH_mixins359390429488; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359395429833, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359395429833, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for success")), [success_base359585429540.extend('name', name359586429751).extend('location', location359587429784)])])]); 
})(); 
})(); }, RUNTIME.makeString("success: Creates an instance of success"));
var failure_base359590429934 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("failure"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("reason"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_failure430005,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("reason"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("failure")]))) { return (function(){
 var call_DASH_failure359588430079 = RUNTIME.getField(cases_DASH_funs412779, "failure");
return RUNTIME.applyFunc(call_DASH_failure359588430079, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "reason"),RUNTIME.getField(self408671, "location")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var failure359589430148 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_failure430005 = RUNTIME.getField(failure359589430148, "test");
var failure430169 = RUNTIME.makeFunction(function (name359591360182430170,reason359592360183430171,location359593360184430172) { return (function(){
 var name359591430176 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360186430177) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360186430177,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359591360182430170]);
var reason359592430209 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360187430210) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360187430210,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [reason359592360183430171]);
var location359593430242 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360188430243) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Option415109,specimen360188430243,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [location359593360184430172]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResult359582429529, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(failure359589430148, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360185430283) { return (function(){
 var self408671 = self360185430283;
return (function(){
 var mixin359396430291 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [CheckResult_DASH_mixins359390429488]))) { return RUNTIME.applyFunc(CheckResult_DASH_mixins359390429488, []); } else { return CheckResult_DASH_mixins359390429488; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359396430291, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359396430291, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for failure")), [failure_base359590429934.extend('name', name359591430176).extend('reason', reason359592430209).extend('location', location359593430242)])])]); 
})(); 
})(); }, RUNTIME.makeString("failure: Creates an instance of failure"));
var err_base359596430396 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("err"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("exception"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_err430467,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("exception"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("err")]))) { return (function(){
 var call_DASH_err359594430541 = RUNTIME.getField(cases_DASH_funs412779, "err");
return RUNTIME.applyFunc(call_DASH_err359594430541, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "exception"),RUNTIME.getField(self408671, "location")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var err359595430610 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_err430467 = RUNTIME.getField(err359595430610, "test");
var err430631 = RUNTIME.makeFunction(function (name359597360189430632,exception359598360190430633,location359599360191430634) { return (function(){
 var name359597430638 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360193430639) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360193430639,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359597360189430632]);
var exception359598430671 = exception359598360190430633;
var location359599430678 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360194430679) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Option415109,specimen360194430679,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [location359599360191430634]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResult359582429529, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(err359595430610, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360192430719) { return (function(){
 var self408671 = self360192430719;
return (function(){
 var mixin359397430727 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [CheckResult_DASH_mixins359390429488]))) { return RUNTIME.applyFunc(CheckResult_DASH_mixins359390429488, []); } else { return CheckResult_DASH_mixins359390429488; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359397430727, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359397430727, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for err")), [err_base359596430396.extend('name', name359597430638).extend('exception', exception359598430671).extend('location', location359599430678)])])]); 
})(); 
})(); }, RUNTIME.makeString("err: Creates an instance of err"));
var CheckResult430832 = RUNTIME.getField(CheckResult359582429529, "test");
var current_DASH_results430843 = empty408476;
var add_DASH_result430850 = RUNTIME.makeFunction(function (result360195430851) { return (function(){
 var result430853 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360196430854) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [CheckResult430832,specimen360196430854,RUNTIME.makeString("CheckResult")]); 
})(); }, RUNTIME.makeString("internal contract for CheckResult")), [result360195430851]);
return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [result430853,RUNTIME.getField(list410049, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_is430933 = RUNTIME.makeFunction(function (name360197430934,thunk1360198430935,thunk2360199430936,loc360200430937) { return (function(){
 var name408584 = name360197430934;
var thunk1430948 = thunk1360198430935;
var thunk2430955 = thunk2360199430936;
var loc427481 = loc360200430937;
return (function(){
 return (function() { try { return (function(){
 var val1430968 = RUNTIME.applyFunc(thunk1430948, []);
return (function() { try { return (function(){
 var val2430979 = RUNTIME.applyFunc(thunk2430955, []);
return (function() { try { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [val1430968,val2430979]))) { return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(success429746, [name408584,RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Values not equal: \n"), "_plus"), [RUNTIME.applyFunc(torepr408254, [val1430968])]), "_plus"), [RUNTIME.makeString("\n\n")]), "_plus"), [RUNTIME.applyFunc(torepr408254, [val2430979])]),RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); } })(); 
})(); } catch (g359400431096) { g359400431096 = RUNTIME.unwrapException(g359400431096); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360201431097) { return (function(){
 var e411256 = e360201431097;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(err430631, [name408584,e411256,RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359400431096])]); 
})(); } })(); 
})(); } catch (g359399431170) { g359399431170 = RUNTIME.unwrapException(g359399431170); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360202431171) { return (function(){
 var e411256 = e360202431171;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(err430631, [name408584,e411256,RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359399431170])]); 
})(); } })(); 
})(); } catch (g359398431244) { g359398431244 = RUNTIME.unwrapException(g359398431244); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360203431245) { return (function(){
 var e411256 = e360203431245;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(err430631, [name408584,e411256,RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359398431244])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_raises431334 = RUNTIME.makeFunction(function (name360204431335,thunk360205431336,expected_DASH_str360206431337,loc360207431338) { return (function(){
 var name408584 = name360204431335;
var thunk431349 = thunk360205431336;
var expected_DASH_str431356 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360210431357) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360210431357,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [expected_DASH_str360206431337]);
var loc427481 = loc360207431338;
return (function(){
 var bad_DASH_err431395 = RUNTIME.makeFunction(function (actual360208431396) { return (function(){
 var actual431398 = actual360208431396;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Wrong error message.  The test expected:\n"), "_plus"), [RUNTIME.applyFunc(torepr408254, [expected_DASH_str431356])]), "_plus"), [RUNTIME.makeString("\nBut this was actually raised:\n")]), "_plus"), [RUNTIME.applyFunc(torepr408254, [actual431398])]),RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { try { return (function(){
 var val1430968 = RUNTIME.applyFunc(thunk431349, []);
return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("No exception raised.  The test expected "), "_plus"), [RUNTIME.applyFunc(torepr408254, [expected_DASH_str431356])]),RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); } catch (g359401431530) { g359401431530 = RUNTIME.unwrapException(g359401431530); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360209431531) { return (function(){
 var e411256 = e360209431531;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(String408284, [e411256]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(e411256, "contains"), [expected_DASH_str431356]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(success429746, [name408584,RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(Error427368, [e411256]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(e411256, "message"), "contains"), [expected_DASH_str431356]))) { return (function(){
 return RUNTIME.applyFunc(add_DASH_result430850, [RUNTIME.applyFunc(success429746, [name408584,RUNTIME.applyFunc(some415147, [loc427481])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(bad_DASH_err431395, [RUNTIME.getField(e411256, "message")]); 
})(); } })(); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(bad_DASH_err431395, [e411256]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359401431530])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Check that thunk raises either a string that contains expected-str,\n        or an exception with a message that contains expected-str"));
var check_DASH_true431744 = RUNTIME.makeFunction(function (name360211431745,val360212431746) { return (function(){
 var name408584 = name360211431745;
var val431755 = val360212431746;
return (function(){
 return RUNTIME.applyFunc(check_DASH_equals431762, [name408584,val431755,RUNTIME.makeBool(true)]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_false431792 = RUNTIME.makeFunction(function (name360213431793,val360214431794) { return (function(){
 var name408584 = name360213431793;
var val431755 = val360214431794;
return (function(){
 return RUNTIME.applyFunc(check_DASH_equals431762, [name408584,val431755,RUNTIME.makeBool(false)]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_equals431762 = RUNTIME.makeFunction(function (name360215431838,val1360216431839,val2360217431840) { return (function(){
 var name408584 = name360215431838;
var val1430968 = val1360216431839;
var val2430979 = val2360217431840;
return (function(){
 return (function() { try { return (function(){
 var values_equal431862 = RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [val1430968,val2430979]);
(function() { if (RUNTIME.isTrue(values_equal431862)) { return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "push"), [RUNTIME.applyFunc(success429746, [name408584,none411036])]); 
})(); } else { return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Values not equal: \n"), "_plus"), [RUNTIME.applyFunc(tostring408249, [val1430968])]), "_plus"), [RUNTIME.makeString("\n\n")]), "_plus"), [RUNTIME.applyFunc(tostring408249, [val2430979])]),none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); } })();
return values_equal431862; 
})(); } catch (g359402431997) { g359402431997 = RUNTIME.unwrapException(g359402431997); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360218431998) { return (function(){
 var e411256 = e360218431998;
return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(err430631, [name408584,e411256,none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359402431997])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_pred432104 = RUNTIME.makeFunction(function (name360219432105,val1360220432106,pred360221432107) { return (function(){
 var name408584 = name360219432105;
var val1430968 = val1360220432106;
var pred432123 = pred360221432107;
return (function(){
 return (function() { try { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(pred432123, [val1430968]))) { return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(success429746, [name408584,none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); } else { return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Value didn't satisfy predicate: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [val1430968])]), "_plus"), [RUNTIME.makeString(", ")]), "_plus"), [RUNTIME.getField(pred432123, "_doc")]),none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); } })(); 
})(); } catch (g359403432264) { g359403432264 = RUNTIME.unwrapException(g359403432264); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360222432265) { return (function(){
 var e411256 = e360222432265;
return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(err430631, [name408584,e411256,none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359403432264])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_exn432371 = RUNTIME.makeFunction(function (name360223432372,thunk360224432373,pred360225432374) { return (function(){
 var name408584 = name360223432372;
var thunk431349 = thunk360224432373;
var pred432123 = pred360225432374;
return (function(){
 return (function() { try { return (function(){
 RUNTIME.applyFunc(thunk431349, []);
return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Thunk didn't throw an exception: "), "_plus"), [RUNTIME.applyFunc(tostring408249, [thunk431349])]),none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); } catch (g359404432456) { g359404432456 = RUNTIME.unwrapException(g359404432456); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360226432457) { return (function(){
 var e411256 = e360226432457;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(pred432123, [e411256]))) { return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(success429746, [name408584,none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); } else { return (function(){
 return current_DASH_results430843 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results430843, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.applyFunc(failure430169, [name408584,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Wrong exception thrown:"), "_plus"), [RUNTIME.applyFunc(tostring408249, [e411256])]),none411036]),RUNTIME.getField(list410049, "empty")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359404432456])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var CheckResultList_DASH_mixins359405432635 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359406432646 = RUNTIME.makeObject({});
var variant359407432655 = data_DASH_shared359406432646;
var variant359408432662 = data_DASH_shared359406432646;
var CheckResultList359600432669 = RUNTIME.applyFunc(brander408259, []);
var normal_DASH_result_base359603432680 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("normal-result"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("results"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_normal_DASH_result432751,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("results"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("normal-result")]))) { return (function(){
 var call_DASH_normal_DASH_result359601432825 = RUNTIME.getField(cases_DASH_funs412779, "normal-result");
return RUNTIME.applyFunc(call_DASH_normal_DASH_result359601432825, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "results")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var normal_DASH_result359602432894 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_normal_DASH_result432751 = RUNTIME.getField(normal_DASH_result359602432894, "test");
var normal_DASH_result432915 = RUNTIME.makeFunction(function (name359604360227432916,location359605360228432917,results359606360229432918) { return (function(){
 var name359604432922 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360231432923) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360231432923,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359604360227432916]);
var location359605432955 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360232432956) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360232432956,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359605360228432917]);
var results359606432988 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360233432989) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360233432989,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [results359606360229432918]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResultList359600432669, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(normal_DASH_result359602432894, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360230433029) { return (function(){
 var self408671 = self360230433029;
return (function(){
 var mixin359409433037 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [CheckResultList_DASH_mixins359405432635]))) { return RUNTIME.applyFunc(CheckResultList_DASH_mixins359405432635, []); } else { return CheckResultList_DASH_mixins359405432635; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359409433037, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359409433037, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for normal-result")), [normal_DASH_result_base359603432680.extend('name', name359604432922).extend('location', location359605432955).extend('results', results359606432988)])])]); 
})(); 
})(); }, RUNTIME.makeString("normal-result: Creates an instance of normal-result"));
var error_DASH_result_base359609433142 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("error-result"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("results"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("err"),RUNTIME.getField(list410049, "empty")])])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_error_DASH_result433225,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("results"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("err"),RUNTIME.getField(list410049, "empty")])])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("error-result")]))) { return (function(){
 var call_DASH_error_DASH_result359607433311 = RUNTIME.getField(cases_DASH_funs412779, "error-result");
return RUNTIME.applyFunc(call_DASH_error_DASH_result359607433311, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "location"),RUNTIME.getField(self408671, "results"),RUNTIME.getField(self408671, "err")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var error_DASH_result359608433385 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_error_DASH_result433225 = RUNTIME.getField(error_DASH_result359608433385, "test");
var error_DASH_result433406 = RUNTIME.makeFunction(function (name359610360234433407,location359611360235433408,results359612360236433409,err359613360237433410) { return (function(){
 var name359610433415 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360239433416) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360239433416,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359610360234433407]);
var location359611433448 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360240433449) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Location421612,specimen360240433449,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location359611360235433408]);
var results359612433481 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360241433482) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360241433482,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [results359612360236433409]);
var err359613433514 = err359613360237433410;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResultList359600432669, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(error_DASH_result359608433385, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360238433529) { return (function(){
 var self408671 = self360238433529;
return (function(){
 var mixin359410433537 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [CheckResultList_DASH_mixins359405432635]))) { return RUNTIME.applyFunc(CheckResultList_DASH_mixins359405432635, []); } else { return CheckResultList_DASH_mixins359405432635; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359410433537, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359410433537, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for error-result")), [error_DASH_result_base359609433142.extend('name', name359610433415).extend('location', location359611433448).extend('results', results359612433481).extend('err', err359613433514)])])]); 
})(); 
})(); }, RUNTIME.makeString("error-result: Creates an instance of error-result"));
var CheckResultList433646 = RUNTIME.getField(CheckResultList359600432669, "test");
var all_DASH_results433657 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360242433658) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360242433658,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [empty408476]);
var run_DASH_checks433690 = RUNTIME.makeFunction(function (checks360243433691) { return (function(){
 var checks433693 = checks360243433691;
return (function(){
 (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.applyFunc(RUNTIME.getField(checks433693, "length"), []),RUNTIME.makeNumber(0)]), "_not"), []))) { return (function(){
 var lst_DASH_to_DASH_structural433728 = RUNTIME.makeFunction(function (lst360244433729) { return (function(){
 var lst408857 = lst360244433729;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field408333, [lst408857,RUNTIME.makeString("first")]))) { return (function(){
 return RUNTIME.makeObject({'first':RUNTIME.getField(lst408857, "first"),'rest':RUNTIME.applyFunc(lst_DASH_to_DASH_structural433728, [RUNTIME.getField(lst408857, "rest")]),'is-empty':RUNTIME.makeBool(false)}); 
})(); } else { return (function(){
 return RUNTIME.makeObject({'is-empty':RUNTIME.makeBool(true)}); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var these_DASH_checks433824 = RUNTIME.applyFunc(mklist408462, [RUNTIME.applyFunc(lst_DASH_to_DASH_structural433728, [checks433693])]);
var old_DASH_results433841 = current_DASH_results430843;
var these_DASH_check_DASH_results433848 = RUNTIME.applyFunc(map415224, [RUNTIME.makeFunction(function (chk360245433849) { return (function(){
 var chk433851 = chk360245433849;
return (function(){
 var l408902 = RUNTIME.getField(chk433851, "location");
var loc427481 = RUNTIME.applyFunc(RUNTIME.getField(error427816, "location"), [RUNTIME.getField(l408902, "file"),RUNTIME.getField(l408902, "line"),RUNTIME.getField(l408902, "column")]);
current_DASH_results430843 = empty408476;
var result430853 = (function() { try { return (function(){
 RUNTIME.applyFunc(RUNTIME.getField(chk433851, "run"), []);
return RUNTIME.applyFunc(normal_DASH_result432915, [RUNTIME.getField(chk433851, "name"),loc427481,current_DASH_results430843]); 
})(); } catch (g359411433928) { g359411433928 = RUNTIME.unwrapException(g359411433928); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e360246433929) { return (function(){
 var e411256 = e360246433929;
return (function(){
 return RUNTIME.applyFunc(error_DASH_result433406, [RUNTIME.getField(chk433851, "name"),loc427481,current_DASH_results430843,e411256]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error427816, "make-error"), [g359411433928])]); 
})(); } })();
return result430853; 
})(); 
})(); }, RUNTIME.makeString("")),these_DASH_checks433824]);
var relevant_DASH_results434025 = RUNTIME.applyFunc(RUNTIME.getField(these_DASH_check_DASH_results433848, "filter"), [RUNTIME.makeFunction(function (elt360247434030) { return (function(){
 var elt410098 = elt360247434030;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_error_DASH_result433225, [elt410098]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(elt410098, "results"), "length"), []), "_greaterthan"), [RUNTIME.makeNumber(0)]); 
})(); }, RUNTIME.makeString(""))]); 
})(); 
})(); }, RUNTIME.makeString(""))]);
current_DASH_results430843 = old_DASH_results433841;
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(relevant_DASH_results434025, "length"), []), "_greaterthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 all_DASH_results433657 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360248434135) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360248434135,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [RUNTIME.applyFunc(RUNTIME.getField(all_DASH_results433657, "push"), [relevant_DASH_results434025])]);
return nothing408224; 
})(); } else { return nothing408224; } })();
return nothing408224; 
})(); } else { return nothing408224; } })();
return nothing408224; 
})(); 
})(); }, RUNTIME.makeString(""));
var clear_DASH_results434228 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 all_DASH_results433657 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360249434229) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360249434229,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [empty408476]);
return nothing408224; 
})(); 
})(); }, RUNTIME.makeString(""));
var get_DASH_results434281 = RUNTIME.makeFunction(function (val360250434282) { return (function(){
 var val431755 = val360250434282;
return (function(){
 return RUNTIME.makeObject({'results':all_DASH_results433657,'format':RUNTIME.makeMethod(function (self408671) { return (function(){
 return RUNTIME.applyFunc(format_DASH_check_DASH_results434295, [RUNTIME.getField(self408671, "results")]); 
})(); }, RUNTIME.makeString("")),'val':val431755}); 
})(); 
})(); }, RUNTIME.makeString(""));
var format_DASH_check_DASH_results434295 = RUNTIME.makeFunction(function (results_DASH_list360251434348) { return (function(){
 var results_DASH_list434350 = results_DASH_list360251434348;
return (function(){
 return RUNTIME.applyFunc(print408214, [RUNTIME.getField(RUNTIME.applyFunc(check_DASH_results_DASH_summary434357, [results_DASH_list434350]), "message")]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_results_DASH_summary434357 = RUNTIME.makeFunction(function (results_DASH_list360252434392) { return (function(){
 var results_DASH_list434350 = results_DASH_list360252434392;
return (function(){
 var init434400 = RUNTIME.makeObject({'passed':RUNTIME.makeNumber(0),'failed':RUNTIME.makeNumber(0),'test-errors':RUNTIME.makeNumber(0),'other-errors':RUNTIME.makeNumber(0),'total':RUNTIME.makeNumber(0)});
var message434444 = RUNTIME.makeString("");
var append_DASH_str434453 = RUNTIME.makeFunction(function (appendage360253434454) { return (function(){
 var appendage434456 = appendage360253434454;
return (function(){
 return message434444 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(message434444, "_plus"), [appendage434456]), "_plus"), [RUNTIME.makeString("\n")]); 
})(); 
})(); }, RUNTIME.makeString(""));
var counts434507 = RUNTIME.applyFunc(fold410006, [RUNTIME.makeFunction(function (acc360254434508,results360255434509) { return (function(){
 var acc409382 = acc360254434508;
var results434518 = results360255434509;
return (function(){
 return RUNTIME.applyFunc(fold410006, [RUNTIME.makeFunction(function (inner_DASH_acc360256434525,check_DASH_result360257434526) { return (function(){
 var inner_DASH_acc434529 = inner_DASH_acc360256434525;
var check_DASH_result434536 = check_DASH_result360257434526;
return (function(){
 var inner_DASH_results434543 = RUNTIME.getField(check_DASH_result434536, "results");
var new_DASH_passed434554 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results434543, "filter"), [is_DASH_success429599]), "length"), []);
var new_DASH_failed434578 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results434543, "filter"), [is_DASH_failure430005]), "length"), []);
var new_DASH_errors434602 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results434543, "filter"), [is_DASH_err430467]), "length"), []);
var other_DASH_errors434626 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(link408481, [check_DASH_result434536,empty408476]), "filter"), [is_DASH_error_DASH_result433225]), "length"), []);
var new_DASH_results434656 = inner_DASH_acc434529.extend('passed', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc434529, "passed"), "_plus"), [new_DASH_passed434554])).extend('failed', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc434529, "failed"), "_plus"), [new_DASH_failed434578])).extend('test-errors', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc434529, "test-errors"), "_plus"), [new_DASH_errors434602])).extend('other-errors', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc434529, "other-errors"), "_plus"), [other_DASH_errors434626])).extend('total', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc434529, "total"), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results434543, "length"), [])]));
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [new_DASH_failed434578,RUNTIME.makeNumber(0)]), "_not"), []), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [new_DASH_errors434602,RUNTIME.makeNumber(0)]), "_not"), []); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [other_DASH_errors434626,RUNTIME.makeNumber(0)]), "_not"), []); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("\n\nIn check block at "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(check_DASH_result434536, "location"), "format"), [])])]);
return nothing408224; 
})(); } else { return nothing408224; } })();
RUNTIME.applyFunc(each417120, [RUNTIME.makeFunction(function (fail360258434900) { return (function(){
 var fail434902 = fail360258434900;
return (function(){
 var cases_DASH_value359614434909 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360259434910) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Option415109,specimen360259434910,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [RUNTIME.getField(fail434902, "location")]);
RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value359614434909, "_match"), [RUNTIME.makeObject({'none':(function() { throw new Error('Not yet implemented s_hint') })(),'some':(function() { throw new Error('Not yet implemented s_hint') })()}),(function() { throw new Error('Not yet implemented s_hint') })()]);
RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Test "), "_plus"), [RUNTIME.getField(fail434902, "name")]), "_plus"), [RUNTIME.makeString(" failed:")])]);
RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.getField(fail434902, "reason")]);
return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("")]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results434543, "filter"), [is_DASH_failure430005])]);
RUNTIME.applyFunc(each417120, [RUNTIME.makeFunction(function (fail360261435064) { return (function(){
 var fail434902 = fail360261435064;
return (function(){
 var cases_DASH_value359615435072 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360262435073) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Option415109,specimen360262435073,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [RUNTIME.getField(fail434902, "location")]);
RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value359615435072, "_match"), [RUNTIME.makeObject({'none':(function() { throw new Error('Not yet implemented s_hint') })(),'some':(function() { throw new Error('Not yet implemented s_hint') })()}),(function() { throw new Error('Not yet implemented s_hint') })()]);
RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Test "), "_plus"), [RUNTIME.getField(fail434902, "name")]), "_plus"), [RUNTIME.makeString(" raised an error:")])]);
RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(fail434902, "exception"), "tostring"), [])]);
RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("")]);
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field408333, [RUNTIME.getField(fail434902, "exception"),RUNTIME.makeString("trace")]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("Trace:")]);
RUNTIME.applyFunc(each417120, [RUNTIME.makeFunction(function (loc360264435227) { return (function(){
 var loc427481 = loc360264435227;
return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(loc427481, "format"), [])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.getField(RUNTIME.getField(fail434902, "exception"), "trace")]);
return nothing408224; 
})(); } else { return nothing408224; } })(); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results434543, "filter"), [is_DASH_err430467])]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_error_DASH_result433225, [check_DASH_result434536]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Check block "), "_plus"), [RUNTIME.getField(check_DASH_result434536, "name")]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(" "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(check_DASH_result434536, "location"), "format"), [])])]), "_plus"), [RUNTIME.makeString(" ended in an error: ")])]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Error427368, [RUNTIME.getField(check_DASH_result434536, "err")]))) { return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(check_DASH_result434536, "err"), "format"), [])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.getField(check_DASH_result434536, "err")]); 
})(); } })();
RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("")]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field408333, [RUNTIME.getField(check_DASH_result434536, "err"),RUNTIME.makeString("trace")]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("Trace:")]);
RUNTIME.applyFunc(each417120, [RUNTIME.makeFunction(function (loc360265435478) { return (function(){
 var loc427481 = loc360265435478;
return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("  "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(loc427481, "format"), [])])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.getField(RUNTIME.getField(check_DASH_result434536, "err"), "trace")]);
return nothing408224; 
})(); } else { return nothing408224; } })();
return nothing408224; 
})(); } else { return nothing408224; } })();
return new_DASH_results434656; 
})(); 
})(); }, RUNTIME.makeString("")),acc409382,results434518]); 
})(); 
})(); }, RUNTIME.makeString("")),init434400,results_DASH_list434350]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(counts434507, "other-errors"),RUNTIME.makeNumber(0)]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(counts434507, "failed"),RUNTIME.makeNumber(0)]); 
})(); }, RUNTIME.makeString(""))]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(counts434507, "test-errors"),RUNTIME.makeNumber(0)]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(counts434507, "passed"),RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("\nWARNING: Your program didn't define any tests.  Add some where: and check:\nblocks to test your code, or run with the --no-checks option to signal that you\ndon't want tests run.\n")]); 
})(); } else { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [RUNTIME.getField(counts434507, "passed"),RUNTIME.makeNumber(1)]))) { return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Looks shipshape, your "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "passed"), "tostring"), [])]), "_plus"), [RUNTIME.makeString(" test passed, mate!")])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Looks shipshape, all "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "passed"), "tostring"), [])]), "_plus"), [RUNTIME.makeString(" tests passed, mate!")])]); 
})(); } })(); 
})(); } })(); 
})(); } else { return (function(){
 RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.makeString("Avast, there be bugs!")]);
return RUNTIME.applyFunc(append_DASH_str434453, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Total: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "total"), "tostring"), [])]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", Passed: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "passed"), "tostring"), [])])]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", Failed: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "failed"), "tostring"), [])]), "_plus"), [RUNTIME.makeString(", Errors in tests: ")]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "test-errors"), "tostring"), []), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", Errors in between tests: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts434507, "other-errors"), "tostring"), [])])])])])]); 
})(); } })();
return counts434507.extend('message', message434444); 
})(); 
})(); }, RUNTIME.makeString(""));
var checkers436068 = RUNTIME.makeObject({'CheckResultList':CheckResultList433646,'CheckResult':CheckResult430832,'check-is':check_DASH_is430933,'check-raises':check_DASH_raises431334,'check-true':check_DASH_true431744,'check-false':check_DASH_false431792,'check-equals':check_DASH_equals431762,'check-pred':check_DASH_pred432104,'check-exn':check_DASH_exn432371,'run-checks':run_DASH_checks433690,'format-check-results':format_DASH_check_DASH_results434295,'check-results-summary':check_DASH_results_DASH_summary434357,'clear-results':clear_DASH_results434228,'get-results':get_DASH_results434281,'normal-result':normal_DASH_result432915,'is-normal-result':is_DASH_normal_DASH_result432751,'error-result':error_DASH_result433406,'is-error-result':is_DASH_error_DASH_result433225,'success':success429746,'is-success':is_DASH_success429599,'failure':failure430169,'is-failure':is_DASH_failure430005,'err':err430631,'is-err':is_DASH_err430467});
var interp_DASH_basic436197 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 var Value_DASH_mixins359412436198 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359413436209 = RUNTIME.makeObject({});
var variant359414436218 = data_DASH_shared359413436209;
var variant359415436225 = data_DASH_shared359413436209;
var variant359416436232 = data_DASH_shared359413436209;
var Value359616436239 = RUNTIME.applyFunc(brander408259, []);
var numV_base359619436250 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("numV"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_numV436297,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("numV")]))) { return (function(){
 var call_DASH_numV359617436347 = RUNTIME.getField(cases_DASH_funs412779, "numV");
return RUNTIME.applyFunc(call_DASH_numV359617436347, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var numV359618436406 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_numV436297 = RUNTIME.getField(numV359618436406, "test");
var numV436427 = RUNTIME.makeFunction(function (value359620360266436428) { return (function(){
 var value359620436430 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360268436431) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360268436431,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [value359620360266436428]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Value359616436239, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(numV359618436406, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360267436471) { return (function(){
 var self408671 = self360267436471;
return (function(){
 var mixin359417436479 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Value_DASH_mixins359412436198]))) { return RUNTIME.applyFunc(Value_DASH_mixins359412436198, []); } else { return Value_DASH_mixins359412436198; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359417436479, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359417436479, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for numV")), [numV_base359619436250.extend('value', value359620436430)])])]); 
})(); 
})(); }, RUNTIME.makeString("numV: Creates an instance of numV"));
var strV_base359623436576 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("strV"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_strV436623,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("strV")]))) { return (function(){
 var call_DASH_strV359621436673 = RUNTIME.getField(cases_DASH_funs412779, "strV");
return RUNTIME.applyFunc(call_DASH_strV359621436673, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var strV359622436732 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_strV436623 = RUNTIME.getField(strV359622436732, "test");
var strV436753 = RUNTIME.makeFunction(function (value359624360269436754) { return (function(){
 var value359624436756 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360271436757) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360271436757,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [value359624360269436754]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Value359616436239, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(strV359622436732, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360270436797) { return (function(){
 var self408671 = self360270436797;
return (function(){
 var mixin359418436805 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Value_DASH_mixins359412436198]))) { return RUNTIME.applyFunc(Value_DASH_mixins359412436198, []); } else { return Value_DASH_mixins359412436198; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359418436805, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359418436805, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for strV")), [strV_base359623436576.extend('value', value359624436756)])])]); 
})(); 
})(); }, RUNTIME.makeString("strV: Creates an instance of strV"));
var funV_base359627436902 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("funV"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_funV436973,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("funV")]))) { return (function(){
 var call_DASH_funV359625437047 = RUNTIME.getField(cases_DASH_funs412779, "funV");
return RUNTIME.applyFunc(call_DASH_funV359625437047, [RUNTIME.getField(self408671, "params"),RUNTIME.getField(self408671, "body"),RUNTIME.getField(self408671, "env")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var funV359626437116 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_funV436973 = RUNTIME.getField(funV359626437116, "test");
var funV437137 = RUNTIME.makeFunction(function (params359628360272437138,body359629360273437139,env359630360274437140) { return (function(){
 var params359628437144 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360276437145) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360276437145,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params359628360272437138]);
var body359629437177 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360277437178) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360277437178,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body359629360273437139]);
var env359630437211 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360278437212) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Env437214,specimen360278437212,RUNTIME.makeString("Env")]); 
})(); }, RUNTIME.makeString("internal contract for Env")), [env359630360274437140]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Value359616436239, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(funV359626437116, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360275437253) { return (function(){
 var self408671 = self360275437253;
return (function(){
 var mixin359419437261 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Value_DASH_mixins359412436198]))) { return RUNTIME.applyFunc(Value_DASH_mixins359412436198, []); } else { return Value_DASH_mixins359412436198; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359419437261, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359419437261, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for funV")), [funV_base359627436902.extend('params', params359628437144).extend('body', body359629437177).extend('env', env359630437211)])])]); 
})(); 
})(); }, RUNTIME.makeString("funV: Creates an instance of funV"));
var Value437366 = RUNTIME.getField(Value359616436239, "test");
var Env_DASH_mixins359420437377 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359421437388 = RUNTIME.makeObject({});
var variant359422437397 = data_DASH_shared359421437388;
var variant359423437404 = data_DASH_shared359421437388;
var Env359631437411 = RUNTIME.applyFunc(brander408259, []);
var mt_DASH_env_base359634437422 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("mt-env"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_mt_DASH_env437442,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("mt-env")]))) { return (function(){
 var call_DASH_mt_DASH_env359633437480 = RUNTIME.getField(cases_DASH_funs412779, "mt-env");
return RUNTIME.applyFunc(call_DASH_mt_DASH_env359633437480, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var mt_DASH_env359632437534 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_mt_DASH_env437442 = RUNTIME.getField(mt_DASH_env359632437534, "test");
var mt_DASH_env437555 = RUNTIME.applyFunc(RUNTIME.getField(Env359631437411, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mt_DASH_env359632437534, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360279437564) { return (function(){
 var self408671 = self360279437564;
return (function(){
 var mixin359424437572 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Env_DASH_mixins359420437377]))) { return RUNTIME.applyFunc(Env_DASH_mixins359420437377, []); } else { return Env_DASH_mixins359420437377; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359424437572, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359424437572, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for mt-env")), [mt_DASH_env_base359634437422])])]);
var an_DASH_env_base359637437651 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("an-env"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("val"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_an_DASH_env437722,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("val"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("an-env")]))) { return (function(){
 var call_DASH_an_DASH_env359635437796 = RUNTIME.getField(cases_DASH_funs412779, "an-env");
return RUNTIME.applyFunc(call_DASH_an_DASH_env359635437796, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "val"),RUNTIME.getField(self408671, "env")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var an_DASH_env359636437865 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_an_DASH_env437722 = RUNTIME.getField(an_DASH_env359636437865, "test");
var an_DASH_env437886 = RUNTIME.makeFunction(function (name359638360280437887,val359639360281437888,env359640360282437889) { return (function(){
 var name359638437893 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360284437894) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360284437894,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359638360280437887]);
var val359639437926 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360285437927) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Value437366,specimen360285437927,RUNTIME.makeString("Value")]); 
})(); }, RUNTIME.makeString("internal contract for Value")), [val359639360281437888]);
var env359640437959 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360286437960) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Env437214,specimen360286437960,RUNTIME.makeString("Env")]); 
})(); }, RUNTIME.makeString("internal contract for Env")), [env359640360282437889]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Env359631437411, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(an_DASH_env359636437865, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360283438000) { return (function(){
 var self408671 = self360283438000;
return (function(){
 var mixin359425438008 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Env_DASH_mixins359420437377]))) { return RUNTIME.applyFunc(Env_DASH_mixins359420437377, []); } else { return Env_DASH_mixins359420437377; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359425438008, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359425438008, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for an-env")), [an_DASH_env_base359637437651.extend('name', name359638437893).extend('val', val359639437926).extend('env', env359640437959)])])]); 
})(); 
})(); }, RUNTIME.makeString("an-env: Creates an instance of an-env"));
var Env437214 = RUNTIME.getField(Env359631437411, "test");
var Expr_DASH_mixins359426438123 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359427438134 = RUNTIME.makeObject({});
var variant359428438143 = data_DASH_shared359427438134;
var variant359429438150 = data_DASH_shared359427438134;
var variant359430438157 = data_DASH_shared359427438134;
var variant359431438164 = data_DASH_shared359427438134;
var variant359432438171 = data_DASH_shared359427438134;
var variant359433438178 = data_DASH_shared359427438134;
var variant359434438185 = data_DASH_shared359427438134;
var variant359435438192 = data_DASH_shared359427438134;
var Expr359641438199 = RUNTIME.applyFunc(brander408259, []);
var id_base359644438210 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("id"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_id438257,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("id")]))) { return (function(){
 var call_DASH_id359642438307 = RUNTIME.getField(cases_DASH_funs412779, "id");
return RUNTIME.applyFunc(call_DASH_id359642438307, [RUNTIME.getField(self408671, "name")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var id359643438366 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_id438257 = RUNTIME.getField(id359643438366, "test");
var id438387 = RUNTIME.makeFunction(function (name359645360287438388) { return (function(){
 var name359645438390 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360289438391) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360289438391,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359645360287438388]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(id359643438366, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360288438431) { return (function(){
 var self408671 = self360288438431;
return (function(){
 var mixin359436438439 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359436438439, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359436438439, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for id")), [id_base359644438210.extend('name', name359645438390)])])]); 
})(); 
})(); }, RUNTIME.makeString("id: Creates an instance of id"));
var num_base359648438536 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("num"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_num438583,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("num")]))) { return (function(){
 var call_DASH_num359646438633 = RUNTIME.getField(cases_DASH_funs412779, "num");
return RUNTIME.applyFunc(call_DASH_num359646438633, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var num359647438692 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_num438583 = RUNTIME.getField(num359647438692, "test");
var num418155 = RUNTIME.makeFunction(function (value359649360290438713) { return (function(){
 var value359649438715 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360292438716) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360292438716,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [value359649360290438713]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(num359647438692, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360291438756) { return (function(){
 var self408671 = self360291438756;
return (function(){
 var mixin359437438764 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359437438764, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359437438764, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for num")), [num_base359648438536.extend('value', value359649438715)])])]); 
})(); 
})(); }, RUNTIME.makeString("num: Creates an instance of num"));
var str_base359652438861 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("str"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_str438908,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("str")]))) { return (function(){
 var call_DASH_str359650438958 = RUNTIME.getField(cases_DASH_funs412779, "str");
return RUNTIME.applyFunc(call_DASH_str359650438958, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var str359651439017 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_str438908 = RUNTIME.getField(str359651439017, "test");
var str411370 = RUNTIME.makeFunction(function (value359653360293439038) { return (function(){
 var value359653439040 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360295439041) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360295439041,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [value359653360293439038]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str359651439017, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360294439081) { return (function(){
 var self408671 = self360294439081;
return (function(){
 var mixin359438439089 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359438439089, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359438439089, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str")), [str_base359652438861.extend('value', value359653439040)])])]); 
})(); 
})(); }, RUNTIME.makeString("str: Creates an instance of str"));
var bop_base359656439186 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("bop"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_bop439257,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("bop")]))) { return (function(){
 var call_DASH_bop359654439331 = RUNTIME.getField(cases_DASH_funs412779, "bop");
return RUNTIME.applyFunc(call_DASH_bop359654439331, [RUNTIME.getField(self408671, "op"),RUNTIME.getField(self408671, "left"),RUNTIME.getField(self408671, "right")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var bop359655439400 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_bop439257 = RUNTIME.getField(bop359655439400, "test");
var bop439421 = RUNTIME.makeFunction(function (op359657360296439422,left359658360297439423,right359659360298439424) { return (function(){
 var op359657439428 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360300439429) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Operator439431,specimen360300439429,RUNTIME.makeString("Operator")]); 
})(); }, RUNTIME.makeString("internal contract for Operator")), [op359657360296439422]);
var left359658439462 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360301439463) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360301439463,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [left359658360297439423]);
var right359659439495 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360302439496) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360302439496,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [right359659360298439424]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(bop359655439400, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360299439536) { return (function(){
 var self408671 = self360299439536;
return (function(){
 var mixin359439439544 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359439439544, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359439439544, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for bop")), [bop_base359656439186.extend('op', op359657439428).extend('left', left359658439462).extend('right', right359659439495)])])]); 
})(); 
})(); }, RUNTIME.makeString("bop: Creates an instance of bop"));
var cif_base359662439649 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("cif"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_cif439720,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("cif")]))) { return (function(){
 var call_DASH_cif359660439794 = RUNTIME.getField(cases_DASH_funs412779, "cif");
return RUNTIME.applyFunc(call_DASH_cif359660439794, [RUNTIME.getField(self408671, "cond"),RUNTIME.getField(self408671, "consq"),RUNTIME.getField(self408671, "altern")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var cif359661439863 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_cif439720 = RUNTIME.getField(cif359661439863, "test");
var cif439884 = RUNTIME.makeFunction(function (cond359663360303439885,consq359664360304439886,altern359665360305439887) { return (function(){
 var cond359663439891 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360307439892) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360307439892,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [cond359663360303439885]);
var consq359664439924 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360308439925) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360308439925,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [consq359664360304439886]);
var altern359665439957 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360309439958) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360309439958,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [altern359665360305439887]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(cif359661439863, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360306439998) { return (function(){
 var self408671 = self360306439998;
return (function(){
 var mixin359440440006 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359440440006, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359440440006, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for cif")), [cif_base359662439649.extend('cond', cond359663439891).extend('consq', consq359664439924).extend('altern', altern359665439957)])])]); 
})(); 
})(); }, RUNTIME.makeString("cif: Creates an instance of cif"));
var let_base359668440111 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("let"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_let440182,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("let")]))) { return (function(){
 var call_DASH_let359666440256 = RUNTIME.getField(cases_DASH_funs412779, "let");
return RUNTIME.applyFunc(call_DASH_let359666440256, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "expr"),RUNTIME.getField(self408671, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var let359667440325 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_let440182 = RUNTIME.getField(let359667440325, "test");
var let440346 = RUNTIME.makeFunction(function (name359669360310440347,expr359670360311440348,body359671360312440349) { return (function(){
 var name359669440353 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360314440354) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360314440354,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359669360310440347]);
var expr359670440386 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360315440387) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360315440387,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [expr359670360311440348]);
var body359671440419 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360316440420) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360316440420,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body359671360312440349]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(let359667440325, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360313440460) { return (function(){
 var self408671 = self360313440460;
return (function(){
 var mixin359441440468 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359441440468, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359441440468, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for let")), [let_base359668440111.extend('name', name359669440353).extend('expr', expr359670440386).extend('body', body359671440419)])])]); 
})(); 
})(); }, RUNTIME.makeString("let: Creates an instance of let"));
var lam_base359674440573 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("lam"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_lam440632,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("lam")]))) { return (function(){
 var call_DASH_lam359672440694 = RUNTIME.getField(cases_DASH_funs412779, "lam");
return RUNTIME.applyFunc(call_DASH_lam359672440694, [RUNTIME.getField(self408671, "params"),RUNTIME.getField(self408671, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var lam359673440758 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_lam440632 = RUNTIME.getField(lam359673440758, "test");
var lam440779 = RUNTIME.makeFunction(function (params359675360317440780,body359676360318440781) { return (function(){
 var params359675440784 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360320440785) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360320440785,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params359675360317440780]);
var body359676440817 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360321440818) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360321440818,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body359676360318440781]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(lam359673440758, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360319440858) { return (function(){
 var self408671 = self360319440858;
return (function(){
 var mixin359442440866 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359442440866, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359442440866, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for lam")), [lam_base359674440573.extend('params', params359675440784).extend('body', body359676440817)])])]); 
})(); 
})(); }, RUNTIME.makeString("lam: Creates an instance of lam"));
var app_base359679440967 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("app"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_app441026,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("app")]))) { return (function(){
 var call_DASH_app359677441088 = RUNTIME.getField(cases_DASH_funs412779, "app");
return RUNTIME.applyFunc(call_DASH_app359677441088, [RUNTIME.getField(self408671, "func"),RUNTIME.getField(self408671, "args")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var app359678441152 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_app441026 = RUNTIME.getField(app359678441152, "test");
var app441173 = RUNTIME.makeFunction(function (func359680360322441174,args359681360323441175) { return (function(){
 var func359680441178 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360325441179) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360325441179,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [func359680360322441174]);
var args359681441211 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360326441212) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360326441212,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [args359681360323441175]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359641438199, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(app359678441152, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360324441252) { return (function(){
 var self408671 = self360324441252;
return (function(){
 var mixin359443441260 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359426438123]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359426438123, []); } else { return Expr_DASH_mixins359426438123; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359443441260, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359443441260, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for app")), [app_base359679440967.extend('func', func359680441178).extend('args', args359681441211)])])]); 
})(); 
})(); }, RUNTIME.makeString("app: Creates an instance of app"));
var Expr437180 = RUNTIME.getField(Expr359641438199, "test");
var Operator_DASH_mixins359444441371 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359445441382 = RUNTIME.makeObject({});
var variant359446441391 = data_DASH_shared359445441382;
var variant359447441398 = data_DASH_shared359445441382;
var variant359448441405 = data_DASH_shared359445441382;
var variant359449441412 = data_DASH_shared359445441382;
var Operator359682441419 = RUNTIME.applyFunc(brander408259, []);
var plus_base359685441430 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("plus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_plus441450,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("plus")]))) { return (function(){
 var call_DASH_plus359684441488 = RUNTIME.getField(cases_DASH_funs412779, "plus");
return RUNTIME.applyFunc(call_DASH_plus359684441488, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var plus359683441542 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_plus441450 = RUNTIME.getField(plus359683441542, "test");
var plus441563 = RUNTIME.applyFunc(RUNTIME.getField(Operator359682441419, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(plus359683441542, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360327441572) { return (function(){
 var self408671 = self360327441572;
return (function(){
 var mixin359450441580 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359444441371]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359444441371, []); } else { return Operator_DASH_mixins359444441371; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359450441580, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359450441580, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for plus")), [plus_base359685441430])])]);
var minus_base359688441659 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("minus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_minus441679,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("minus")]))) { return (function(){
 var call_DASH_minus359687441717 = RUNTIME.getField(cases_DASH_funs412779, "minus");
return RUNTIME.applyFunc(call_DASH_minus359687441717, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var minus359686441771 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_minus441679 = RUNTIME.getField(minus359686441771, "test");
var minus441792 = RUNTIME.applyFunc(RUNTIME.getField(Operator359682441419, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(minus359686441771, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360328441801) { return (function(){
 var self408671 = self360328441801;
return (function(){
 var mixin359451441809 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359444441371]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359444441371, []); } else { return Operator_DASH_mixins359444441371; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359451441809, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359451441809, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for minus")), [minus_base359688441659])])]);
var append_base359691441888 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("append"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_append441908,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("append")]))) { return (function(){
 var call_DASH_append359690441946 = RUNTIME.getField(cases_DASH_funs412779, "append");
return RUNTIME.applyFunc(call_DASH_append359690441946, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var append359689442000 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_append441908 = RUNTIME.getField(append359689442000, "test");
var append442021 = RUNTIME.applyFunc(RUNTIME.getField(Operator359682441419, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(append359689442000, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360329442030) { return (function(){
 var self408671 = self360329442030;
return (function(){
 var mixin359452442038 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359444441371]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359444441371, []); } else { return Operator_DASH_mixins359444441371; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359452442038, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359452442038, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for append")), [append_base359691441888])])]);
var str_DASH_eq_base359694442117 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("str-eq"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_str_DASH_eq442137,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("str-eq")]))) { return (function(){
 var call_DASH_str_DASH_eq359693442175 = RUNTIME.getField(cases_DASH_funs412779, "str-eq");
return RUNTIME.applyFunc(call_DASH_str_DASH_eq359693442175, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var str_DASH_eq359692442229 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_str_DASH_eq442137 = RUNTIME.getField(str_DASH_eq359692442229, "test");
var str_DASH_eq442250 = RUNTIME.applyFunc(RUNTIME.getField(Operator359682441419, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str_DASH_eq359692442229, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360330442259) { return (function(){
 var self408671 = self360330442259;
return (function(){
 var mixin359453442267 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359444441371]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359444441371, []); } else { return Operator_DASH_mixins359444441371; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359453442267, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359453442267, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str-eq")), [str_DASH_eq_base359694442117])])]);
var Operator439431 = RUNTIME.getField(Operator359682441419, "test");
var parse442356 = RUNTIME.makeFunction(function (prog360331442357) { return (function(){
 var prog442359 = prog360331442357;
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360338442366) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360338442366,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [(function(){
 var check_DASH_params442387 = RUNTIME.makeFunction(function (params360332442388) { return (function(){
 var params442390 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360336442391) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360336442391,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params360332442388]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360335442423) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360335442423,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [(function(){
 RUNTIME.applyFunc(each417120, [RUNTIME.makeFunction(function (param360333442444) { return (function(){
 var param442446 = param360333442444;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(params442390, "filter"), [RUNTIME.makeFunction(function (x360334442457) { return (function(){
 var x428034 = x360334442457;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [x428034,param442446]); 
})(); 
})(); }, RUNTIME.makeString(""))]), "length"), []), "_greaterthan"), [RUNTIME.makeNumber(1)]))) { return (function(){
 RUNTIME.applyFunc(raise408219, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("parse: function has duplicate parameter "), "_plus"), [param442446])]);
return nothing408224; 
})(); } else { return nothing408224; } })(); 
})(); 
})(); }, RUNTIME.makeString("")),params442390]);
return params442390; 
})()]); 
})(); }, RUNTIME.makeString("Ensure that a function has no duplicate parameter names."));
var convert442592 = RUNTIME.makeFunction(function (sexpr360337442593) { return (function(){
 var sexpr442595 = sexpr360337442593;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List409392, [sexpr442595]))) { return (function(){
 var head442643 = RUNTIME.getField(sexpr442595, "first");
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("string")]))) { return (function(){
 return RUNTIME.applyFunc(str411370, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("if")]))) { return (function(){
 return RUNTIME.applyFunc(cif439884, [RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("let")]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List409392, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]))) { return (function(){
 return RUNTIME.applyFunc(let440346, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(0)]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(let440346, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("fun")]))) { return (function(){
 return RUNTIME.applyFunc(lam440779, [RUNTIME.applyFunc(check_DASH_params442387, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("+")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [plus441563,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("-")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [minus441792,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("++")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [append442021,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("==")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [str_DASH_eq442250,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 var func443231 = RUNTIME.applyFunc(convert442592, [head442643]);
var args443243 = RUNTIME.applyFunc(map415224, [convert442592,RUNTIME.getField(sexpr442595, "rest")]);
return RUNTIME.applyFunc(app441173, [func443231,args443243]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(Number408279, [sexpr442595]))) { return (function(){
 return RUNTIME.applyFunc(num418155, [sexpr442595]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(String408284, [sexpr442595]))) { return (function(){
 return RUNTIME.applyFunc(id438387, [sexpr442595]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.makeString("if: no tests matched")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Convert an s-expression into an Expr."));
return RUNTIME.applyFunc(convert442592, [prog442359]); 
})()]); 
})(); }, RUNTIME.makeString("Parse an s-expr in Paret's concrete syntax into an Expr."));
return RUNTIME.makeObject({'Value':Value437366,'numV':numV436427,'is-numV':is_DASH_numV436297,'strV':strV436753,'is-strV':is_DASH_strV436623,'funV':funV437137,'is-funV':is_DASH_funV436973,'Env':Env437214,'mt-env':mt_DASH_env437555,'is-mt-env':is_DASH_mt_DASH_env437442,'an-env':an_DASH_env437886,'is-an-env':is_DASH_an_DASH_env437722,'Expr':Expr437180,'id':id438387,'is-id':is_DASH_id438257,'num':num418155,'is-num':is_DASH_num438583,'str':str411370,'is-str':is_DASH_str438908,'bop':bop439421,'is-bop':is_DASH_bop439257,'cif':cif439884,'is-cif':is_DASH_cif439720,'let':let440346,'is-let':is_DASH_let440182,'lam':lam440779,'is-lam':is_DASH_lam440632,'app':app441173,'is-app':is_DASH_app441026,'Operator':Operator439431,'plus':plus441563,'is-plus':is_DASH_plus441450,'minus':minus441792,'is-minus':is_DASH_minus441679,'append':append442021,'is-append':is_DASH_append441908,'str-eq':str_DASH_eq442250,'is-str-eq':is_DASH_str_DASH_eq442137,'parse':parse442356}); 
})(); 
})(); }, RUNTIME.makeString(""));
var calculate_DASH_locals443572 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 var Expr_DASH_mixins359454443573 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359455443584 = RUNTIME.makeObject({});
var variant359456443593 = data_DASH_shared359455443584;
var variant359457443600 = data_DASH_shared359455443584;
var variant359458443607 = data_DASH_shared359455443584;
var variant359459443614 = data_DASH_shared359455443584;
var variant359460443621 = data_DASH_shared359455443584;
var variant359461443628 = data_DASH_shared359455443584;
var variant359462443635 = data_DASH_shared359455443584;
var variant359463443642 = data_DASH_shared359455443584;
var variant359464443649 = data_DASH_shared359455443584;
var Expr359695443656 = RUNTIME.applyFunc(brander408259, []);
var id_base359698443667 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("id"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_id438257,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("id")]))) { return (function(){
 var call_DASH_id359696443763 = RUNTIME.getField(cases_DASH_funs412779, "id");
return RUNTIME.applyFunc(call_DASH_id359696443763, [RUNTIME.getField(self408671, "name")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var id359697443822 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_id438257 = RUNTIME.getField(id359697443822, "test");
var id438387 = RUNTIME.makeFunction(function (name359699360339443843) { return (function(){
 var name359699443845 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360341443846) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360341443846,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359699360339443843]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(id359697443822, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360340443886) { return (function(){
 var self408671 = self360340443886;
return (function(){
 var mixin359465443894 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359465443894, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359465443894, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for id")), [id_base359698443667.extend('name', name359699443845)])])]); 
})(); 
})(); }, RUNTIME.makeString("id: Creates an instance of id"));
var num_base359702443991 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("num"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_num438583,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("num")]))) { return (function(){
 var call_DASH_num359700444087 = RUNTIME.getField(cases_DASH_funs412779, "num");
return RUNTIME.applyFunc(call_DASH_num359700444087, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var num359701444146 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_num438583 = RUNTIME.getField(num359701444146, "test");
var num418155 = RUNTIME.makeFunction(function (value359703360342444167) { return (function(){
 var value359703444169 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360344444170) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Number408279,specimen360344444170,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [value359703360342444167]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(num359701444146, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360343444210) { return (function(){
 var self408671 = self360343444210;
return (function(){
 var mixin359466444218 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359466444218, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359466444218, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for num")), [num_base359702443991.extend('value', value359703444169)])])]); 
})(); 
})(); }, RUNTIME.makeString("num: Creates an instance of num"));
var str_base359706444315 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("str"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_str438908,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list410049, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("str")]))) { return (function(){
 var call_DASH_str359704444411 = RUNTIME.getField(cases_DASH_funs412779, "str");
return RUNTIME.applyFunc(call_DASH_str359704444411, [RUNTIME.getField(self408671, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var str359705444470 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_str438908 = RUNTIME.getField(str359705444470, "test");
var str411370 = RUNTIME.makeFunction(function (value359707360345444491) { return (function(){
 var value359707444493 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360347444494) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360347444494,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [value359707360345444491]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str359705444470, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360346444534) { return (function(){
 var self408671 = self360346444534;
return (function(){
 var mixin359467444542 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359467444542, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359467444542, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str")), [str_base359706444315.extend('value', value359707444493)])])]); 
})(); 
})(); }, RUNTIME.makeString("str: Creates an instance of str"));
var bop_base359710444639 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("bop"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_bop439257,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("bop")]))) { return (function(){
 var call_DASH_bop359708444783 = RUNTIME.getField(cases_DASH_funs412779, "bop");
return RUNTIME.applyFunc(call_DASH_bop359708444783, [RUNTIME.getField(self408671, "op"),RUNTIME.getField(self408671, "left"),RUNTIME.getField(self408671, "right")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var bop359709444852 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_bop439257 = RUNTIME.getField(bop359709444852, "test");
var bop439421 = RUNTIME.makeFunction(function (op359711360348444873,left359712360349444874,right359713360350444875) { return (function(){
 var op359711444879 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360352444880) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Operator439431,specimen360352444880,RUNTIME.makeString("Operator")]); 
})(); }, RUNTIME.makeString("internal contract for Operator")), [op359711360348444873]);
var left359712444912 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360353444913) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360353444913,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [left359712360349444874]);
var right359713444945 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360354444946) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360354444946,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [right359713360350444875]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(bop359709444852, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360351444986) { return (function(){
 var self408671 = self360351444986;
return (function(){
 var mixin359468444994 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359468444994, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359468444994, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for bop")), [bop_base359710444639.extend('op', op359711444879).extend('left', left359712444912).extend('right', right359713444945)])])]); 
})(); 
})(); }, RUNTIME.makeString("bop: Creates an instance of bop"));
var cif_base359716445099 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("cif"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_cif439720,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("cif")]))) { return (function(){
 var call_DASH_cif359714445243 = RUNTIME.getField(cases_DASH_funs412779, "cif");
return RUNTIME.applyFunc(call_DASH_cif359714445243, [RUNTIME.getField(self408671, "cond"),RUNTIME.getField(self408671, "consq"),RUNTIME.getField(self408671, "altern")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var cif359715445312 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_cif439720 = RUNTIME.getField(cif359715445312, "test");
var cif439884 = RUNTIME.makeFunction(function (cond359717360355445333,consq359718360356445334,altern359719360357445335) { return (function(){
 var cond359717445339 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360359445340) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360359445340,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [cond359717360355445333]);
var consq359718445372 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360360445373) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360360445373,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [consq359718360356445334]);
var altern359719445405 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360361445406) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360361445406,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [altern359719360357445335]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(cif359715445312, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360358445446) { return (function(){
 var self408671 = self360358445446;
return (function(){
 var mixin359469445454 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359469445454, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359469445454, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for cif")), [cif_base359716445099.extend('cond', cond359717445339).extend('consq', consq359718445372).extend('altern', altern359719445405)])])]); 
})(); 
})(); }, RUNTIME.makeString("cif: Creates an instance of cif"));
var let_base359722445559 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("let"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_let440182,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("let")]))) { return (function(){
 var call_DASH_let359720445703 = RUNTIME.getField(cases_DASH_funs412779, "let");
return RUNTIME.applyFunc(call_DASH_let359720445703, [RUNTIME.getField(self408671, "name"),RUNTIME.getField(self408671, "expr"),RUNTIME.getField(self408671, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var let359721445772 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_let440182 = RUNTIME.getField(let359721445772, "test");
var let440346 = RUNTIME.makeFunction(function (name359723360362445793,expr359724360363445794,body359725360364445795) { return (function(){
 var name359723445799 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360366445800) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [String408284,specimen360366445800,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name359723360362445793]);
var expr359724445832 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360367445833) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360367445833,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [expr359724360363445794]);
var body359725445865 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360368445866) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360368445866,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body359725360364445795]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(let359721445772, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360365445906) { return (function(){
 var self408671 = self360365445906;
return (function(){
 var mixin359470445914 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359470445914, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359470445914, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for let")), [let_base359722445559.extend('name', name359723445799).extend('expr', expr359724445832).extend('body', body359725445865)])])]); 
})(); 
})(); }, RUNTIME.makeString("let: Creates an instance of let"));
var lam_base359728446019 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("lam"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_lam440632,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("lam")]))) { return (function(){
 var call_DASH_lam359726446139 = RUNTIME.getField(cases_DASH_funs412779, "lam");
return RUNTIME.applyFunc(call_DASH_lam359726446139, [RUNTIME.getField(self408671, "params"),RUNTIME.getField(self408671, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var lam359727446203 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_lam440632 = RUNTIME.getField(lam359727446203, "test");
var lam440779 = RUNTIME.makeFunction(function (params359729360369446224,body359730360370446225) { return (function(){
 var params359729446228 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360372446229) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360372446229,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params359729360369446224]);
var body359730446261 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360373446262) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360373446262,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body359730360370446225]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(lam359727446203, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360371446302) { return (function(){
 var self408671 = self360371446302;
return (function(){
 var mixin359471446310 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359471446310, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359471446310, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for lam")), [lam_base359728446019.extend('params', params359729446228).extend('body', body359730446261)])])]); 
})(); 
})(); }, RUNTIME.makeString("lam: Creates an instance of lam"));
var app_base359733446411 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-to-repr"), [self408671,RUNTIME.makeString("app"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_app441026,RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list410049, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list410049, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("app")]))) { return (function(){
 var call_DASH_app359731446531 = RUNTIME.getField(cases_DASH_funs412779, "app");
return RUNTIME.applyFunc(call_DASH_app359731446531, [RUNTIME.getField(self408671, "func"),RUNTIME.getField(self408671, "args")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var app359732446595 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_app441026 = RUNTIME.getField(app359732446595, "test");
var app441173 = RUNTIME.makeFunction(function (func359734360374446616,args359735360375446617) { return (function(){
 var func359734446620 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360377446621) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360377446621,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [func359734360374446616]);
var args359735446653 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360378446654) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [List409392,specimen360378446654,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [args359735360375446617]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(app359732446595, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360376446694) { return (function(){
 var self408671 = self360376446694;
return (function(){
 var mixin359472446702 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359472446702, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359472446702, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for app")), [app_base359733446411.extend('func', func359734446620).extend('args', args359735446653)])])]); 
})(); 
})(); }, RUNTIME.makeString("app: Creates an instance of app"));
var hole_base359738446803 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("hole"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_hole446823,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("hole")]))) { return (function(){
 var call_DASH_hole359737446861 = RUNTIME.getField(cases_DASH_funs412779, "hole");
return RUNTIME.applyFunc(call_DASH_hole359737446861, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var hole359736446915 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_hole446823 = RUNTIME.getField(hole359736446915, "test");
var hole446936 = RUNTIME.applyFunc(RUNTIME.getField(Expr359695443656, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(hole359736446915, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360379446945) { return (function(){
 var self408671 = self360379446945;
return (function(){
 var mixin359473446953 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Expr_DASH_mixins359454443573]))) { return RUNTIME.applyFunc(Expr_DASH_mixins359454443573, []); } else { return Expr_DASH_mixins359454443573; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359473446953, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359473446953, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for hole")), [hole_base359738446803])])]);
var Expr437180 = RUNTIME.getField(Expr359695443656, "test");
var Operator_DASH_mixins359474447042 = RUNTIME.getField(builtins408803, "Eq");
var data_DASH_shared359475447053 = RUNTIME.makeObject({});
var variant359476447062 = data_DASH_shared359475447053;
var variant359477447069 = data_DASH_shared359475447053;
var variant359478447076 = data_DASH_shared359475447053;
var variant359479447083 = data_DASH_shared359475447053;
var Operator359739447090 = RUNTIME.applyFunc(brander408259, []);
var plus_base359742447101 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("plus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_plus441450,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("plus")]))) { return (function(){
 var call_DASH_plus359741447158 = RUNTIME.getField(cases_DASH_funs412779, "plus");
return RUNTIME.applyFunc(call_DASH_plus359741447158, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var plus359740447212 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_plus441450 = RUNTIME.getField(plus359740447212, "test");
var plus441563 = RUNTIME.applyFunc(RUNTIME.getField(Operator359739447090, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(plus359740447212, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360380447241) { return (function(){
 var self408671 = self360380447241;
return (function(){
 var mixin359480447249 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359474447042]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359474447042, []); } else { return Operator_DASH_mixins359474447042; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359480447249, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359480447249, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for plus")), [plus_base359742447101])])]);
var minus_base359745447328 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("minus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_minus441679,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("minus")]))) { return (function(){
 var call_DASH_minus359744447385 = RUNTIME.getField(cases_DASH_funs412779, "minus");
return RUNTIME.applyFunc(call_DASH_minus359744447385, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var minus359743447439 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_minus441679 = RUNTIME.getField(minus359743447439, "test");
var minus441792 = RUNTIME.applyFunc(RUNTIME.getField(Operator359739447090, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(minus359743447439, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360381447468) { return (function(){
 var self408671 = self360381447468;
return (function(){
 var mixin359481447476 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359474447042]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359474447042, []); } else { return Operator_DASH_mixins359474447042; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359481447476, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359481447476, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for minus")), [minus_base359745447328])])]);
var append_base359748447555 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("append"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_append441908,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("append")]))) { return (function(){
 var call_DASH_append359747447612 = RUNTIME.getField(cases_DASH_funs412779, "append");
return RUNTIME.applyFunc(call_DASH_append359747447612, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var append359746447666 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_append441908 = RUNTIME.getField(append359746447666, "test");
var append442021 = RUNTIME.applyFunc(RUNTIME.getField(Operator359739447090, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(append359746447666, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360382447695) { return (function(){
 var self408671 = self360382447695;
return (function(){
 var mixin359482447703 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359474447042]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359474447042, []); } else { return Operator_DASH_mixins359474447042; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359482447703, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359482447703, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for append")), [append_base359748447555])])]);
var str_DASH_eq_base359751447782 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self408671) { return RUNTIME.makeString("str-eq"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self408671,other408672) { return RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "data-equals"), [self408671,other408672,is_DASH_str_DASH_eq442137,RUNTIME.getField(list410049, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self408671,cases_DASH_funs412779,else_DASH_clause412780) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "has-field"), [cases_DASH_funs412779,RUNTIME.makeString("str-eq")]))) { return (function(){
 var call_DASH_str_DASH_eq359750447839 = RUNTIME.getField(cases_DASH_funs412779, "str-eq");
return RUNTIME.applyFunc(call_DASH_str_DASH_eq359750447839, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause412780, []); } })(); }, RUNTIME.makeString(""))});
var str_DASH_eq359749447893 = RUNTIME.applyFunc(brander408259, []);
var is_DASH_str_DASH_eq442137 = RUNTIME.getField(str_DASH_eq359749447893, "test");
var str_DASH_eq442250 = RUNTIME.applyFunc(RUNTIME.getField(Operator359739447090, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str_DASH_eq359749447893, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self360383447922) { return (function(){
 var self408671 = self360383447922;
return (function(){
 var mixin359483447930 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function408264, [Operator_DASH_mixins359474447042]))) { return RUNTIME.applyFunc(Operator_DASH_mixins359474447042, []); } else { return Operator_DASH_mixins359474447042; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin359483447930, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin359483447930, "extend"), [self408671])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str-eq")), [str_DASH_eq_base359751447782])])]);
var Operator439431 = RUNTIME.getField(Operator359739447090, "test");
var parse442356 = RUNTIME.makeFunction(function (prog360384448019) { return (function(){
 var prog442359 = prog360384448019;
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen360386448027) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand408229, [Expr437180,specimen360386448027,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [(function(){
 var convert442592 = RUNTIME.makeFunction(function (sexpr360385448048) { return (function(){
 var sexpr442595 = sexpr360385448048;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List409392, [sexpr442595]))) { return (function(){
 var head442643 = RUNTIME.getField(sexpr442595, "first");
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("string")]))) { return (function(){
 return RUNTIME.applyFunc(str411370, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("if")]))) { return (function(){
 return RUNTIME.applyFunc(cif439884, [RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("let")]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List409392, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]))) { return (function(){
 return RUNTIME.applyFunc(let440346, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(0)]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(let440346, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("fun")]))) { return (function(){
 return RUNTIME.applyFunc(lam440779, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("+")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [plus441563,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("-")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [minus441792,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("++")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [append442021,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [head442643,RUNTIME.makeString("==")]))) { return (function(){
 return RUNTIME.applyFunc(bop439421, [str_DASH_eq442250,RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert442592, [RUNTIME.applyFunc(RUNTIME.getField(sexpr442595, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 var func443231 = RUNTIME.applyFunc(convert442592, [head442643]);
var args443243 = RUNTIME.applyFunc(map415224, [convert442592,RUNTIME.getField(sexpr442595, "rest")]);
return RUNTIME.applyFunc(app441173, [func443231,args443243]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(Number408279, [sexpr442595]))) { return (function(){
 return RUNTIME.applyFunc(num418155, [sexpr442595]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins408803, "equiv"), [sexpr442595,RUNTIME.makeString("@")]))) { return (function(){
 return hole446936; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(String408284, [sexpr442595]))) { return (function(){
 return RUNTIME.applyFunc(id438387, [sexpr442595]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise408219, [RUNTIME.makeString("if: no tests matched")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(convert442592, [prog442359]); 
})()]); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.makeObject({'Expr':Expr437180,'id':id438387,'is-id':is_DASH_id438257,'num':num418155,'is-num':is_DASH_num438583,'str':str411370,'is-str':is_DASH_str438908,'bop':bop439421,'is-bop':is_DASH_bop439257,'cif':cif439884,'is-cif':is_DASH_cif439720,'let':let440346,'is-let':is_DASH_let440182,'lam':lam440779,'is-lam':is_DASH_lam440632,'app':app441173,'is-app':is_DASH_app441026,'hole':hole446936,'is-hole':is_DASH_hole446823,'Operator':Operator439431,'plus':plus441563,'is-plus':is_DASH_plus441450,'minus':minus441792,'is-minus':is_DASH_minus441679,'append':append442021,'is-append':is_DASH_append441908,'str-eq':str_DASH_eq442250,'is-str-eq':is_DASH_str_DASH_eq442137,'parse':parse442356}); 
})(); 
})(); }, RUNTIME.makeString(""));
var cs173448988 = RUNTIME.makeObject({'interp-basic':RUNTIME.applyFunc(interp_DASH_basic436197, []),'calculate-locals':RUNTIME.applyFunc(calculate_DASH_locals443572, [])});
RESULT = NAMESPACE.get('nothing');
                EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("identical", identical408314)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("mklist", mklist408462)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("keys", keys408535)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("has-field", has_DASH_field408333)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("num-keys", num_DASH_keys408617)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Eq", Eq408651)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("builtins", builtins408803)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("get-help", get_DASH_help408852)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("set-help", set_DASH_help409102)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("reverse-help", reverse_DASH_help409371)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("take-help", take_DASH_help409477)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("drop-help", drop_DASH_help409728)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("list-to-set", list_DASH_to_DASH_set409971)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("List-mixins359346", List_DASH_mixins359346410086)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359347", data_DASH_shared359347410097)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359348", variant359348410402)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359349", variant359349411395)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("List359485", List359485412726)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("empty_base359488", empty_base359488412737)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("empty359486", empty359486412850)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-empty", is_DASH_empty408940)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("empty", empty408476)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("link_base359491", link_base359491413166)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("link359490", link359490413354)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-link", is_DASH_link409536)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("link", link408481)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("List", List409392)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("range", range413811)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("repeat", repeat414005)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("filter", filter414173)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("partition", partition411599)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("any", any414558)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("all", all414749)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("find", find411622)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map", map415224)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map2", map2415335)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map3", map3415516)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map4", map4415764)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map_n", map_n416080)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map2_n", map2_n416238)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map3_n", map3_n416464)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map4_n", map4_n416758)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each", each417120)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each2", each2417259)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each3", each3417477)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each4", each4417771)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each_n", each_n418142)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each2_n", each2_n418338)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each3_n", each3_n418610)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each4_n", each4_n418959)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold", fold410006)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold2", fold2419499)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold3", fold3419682)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold4", fold4419933)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold_n", fold_n420252)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("raw-fold", raw_DASH_fold412105)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("index", index420581)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("list", list410049)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Location-mixins359352", Location_DASH_mixins359352420920)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359353", data_DASH_shared359353420931)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359354", variant359354420940)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Location359495", Location359495421194)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("location_base359498", location_base359498421205)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("location359497", location359497421418)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-location", is_DASH_location420943)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("location", location421439)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Location", Location421612)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Error-mixins359356", Error_DASH_mixins359356421623)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359357", data_DASH_shared359357421634)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359358", variant359358421747)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359359", variant359359421771)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359360", variant359360421795)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359361", variant359361421819)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359362", variant359362421843)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359363", variant359363421867)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359364", variant359364421891)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359365", variant359365421915)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359366", variant359366421939)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359367", variant359367421963)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359368", variant359368421987)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Error359502", Error359502422011)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("opaque-error_base359505", opaque_DASH_error_base359505422022)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("opaque-error359504", opaque_DASH_error359504422236)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-opaque-error", is_DASH_opaque_DASH_error422093)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("opaque-error", opaque_DASH_error422257)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-not-found_base359511", field_DASH_not_DASH_found_base359511422508)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-not-found359510", field_DASH_not_DASH_found359510422722)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-field-not-found", is_DASH_field_DASH_not_DASH_found422579)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-not-found", field_DASH_not_DASH_found422743)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-non-string_base359517", field_DASH_non_DASH_string_base359517422994)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-non-string359516", field_DASH_non_DASH_string359516423208)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-field-non-string", is_DASH_field_DASH_non_DASH_string423065)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-non-string", field_DASH_non_DASH_string423229)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cases-miss_base359523", cases_DASH_miss_base359523423480)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cases-miss359522", cases_DASH_miss359522423694)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-cases-miss", is_DASH_cases_DASH_miss423551)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cases-miss", cases_DASH_miss423715)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("invalid-case_base359529", invalid_DASH_case_base359529423966)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("invalid-case359528", invalid_DASH_case359528424180)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-invalid-case", is_DASH_invalid_DASH_case424037)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("invalid-case", invalid_DASH_case424201)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("eval-error_base359535", eval_DASH_error_base359535424452)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("eval-error359534", eval_DASH_error359534424666)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-eval-error", is_DASH_eval_DASH_error424523)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("eval-error", eval_DASH_error424687)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("user-contract-failure_base359541", user_DASH_contract_DASH_failure_base359541424938)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("user-contract-failure359540", user_DASH_contract_DASH_failure359540425152)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-user-contract-failure", is_DASH_user_DASH_contract_DASH_failure425009)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("user-contract-failure", user_DASH_contract_DASH_failure425173)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("arity-error_base359547", arity_DASH_error_base359547425424)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("arity-error359546", arity_DASH_error359546425638)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-arity-error", is_DASH_arity_DASH_error425495)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("arity-error", arity_DASH_error425659)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("div-0_base359553", div_DASH_0_base359553425910)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("div-0359552", div_DASH_0359552426124)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-div-0", is_DASH_div_DASH_0425981)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("div-0", div_DASH_0426145)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("type-error_base359559", type_DASH_error_base359559426396)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("type-error359558", type_DASH_error359558426610)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-type-error", is_DASH_type_DASH_error426467)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("type-error", type_DASH_error426631)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("lazy-error_base359565", lazy_DASH_error_base359565426882)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("lazy-error359564", lazy_DASH_error359564427096)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-lazy-error", is_DASH_lazy_DASH_error426953)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("lazy-error", lazy_DASH_error427117)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Error", Error427368)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("make-error", make_DASH_error427379)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error", error427816)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Set-mixins359380", Set_DASH_mixins359380427910)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359381", data_DASH_shared359381427921)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359382", variant359382427930)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Set359569", Set359569428352)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("__set_base359572", __set_base359572428363)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("__set359571", __set359571428519)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-__set", is_DASH___set428410)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("__set", __set410048)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Set", Set428158)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("sets", sets428746)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Option-mixins359384", Option_DASH_mixins359384428765)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359385", data_DASH_shared359385428776)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359386", variant359386428785)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359387", variant359387428824)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Option359574", Option359574428876)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("none_base359577", none_base359577428887)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("none359575", none359575428998)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-none", is_DASH_none414903)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("none", none411036)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("some_base359580", some_base359580429130)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("some359579", some359579429285)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-some", is_DASH_some414712)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("some", some415147)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Option", Option415109)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("option", option429454)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResult-mixins359390", CheckResult_DASH_mixins359390429488)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359391", data_DASH_shared359391429499)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359392", variant359392429508)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359393", variant359393429515)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359394", variant359394429522)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResult359582", CheckResult359582429529)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("success_base359585", success_base359585429540)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("success359584", success359584429725)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-success", is_DASH_success429599)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("success", success429746)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("failure_base359590", failure_base359590429934)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("failure359589", failure359589430148)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-failure", is_DASH_failure430005)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("failure", failure430169)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("err_base359596", err_base359596430396)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("err359595", err359595430610)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-err", is_DASH_err430467)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("err", err430631)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResult", CheckResult430832)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("current-results", current_DASH_results430843)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("add-result", add_DASH_result430850)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-is", check_DASH_is430933)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-raises", check_DASH_raises431334)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-true", check_DASH_true431744)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-false", check_DASH_false431792)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-equals", check_DASH_equals431762)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-pred", check_DASH_pred432104)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-exn", check_DASH_exn432371)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResultList-mixins359405", CheckResultList_DASH_mixins359405432635)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared359406", data_DASH_shared359406432646)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359407", variant359407432655)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant359408", variant359408432662)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResultList359600", CheckResultList359600432669)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("normal-result_base359603", normal_DASH_result_base359603432680)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("normal-result359602", normal_DASH_result359602432894)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-normal-result", is_DASH_normal_DASH_result432751)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("normal-result", normal_DASH_result432915)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error-result_base359609", error_DASH_result_base359609433142)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error-result359608", error_DASH_result359608433385)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-error-result", is_DASH_error_DASH_result433225)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error-result", error_DASH_result433406)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResultList", CheckResultList433646)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("all-results", all_DASH_results433657)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("run-checks", run_DASH_checks433690)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("clear-results", clear_DASH_results434228)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("get-results", get_DASH_results434281)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("format-check-results", format_DASH_check_DASH_results434295)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-results-summary", check_DASH_results_DASH_summary434357)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("checkers", checkers436068)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("interp-basic", interp_DASH_basic436197)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("calculate-locals", calculate_DASH_locals443572)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cs173", cs173448988)

              })();
              return RUNTIME.makeNormalResult(RESULT, EXPORT_NAMESPACE);
            } catch(e) {
              return RUNTIME.makeFailResult(e);
            }
          })
if(typeof exports !== 'undefined') {
  exports.lib = LIB;
}
