
if (require) {
  var Namespace = require('./namespace.js').Namespace;
}
  LIB = (function(RUNTIME, NAMESPACE) {
            try {
              var print600204 = NAMESPACE.get('print');
var raise600209 = NAMESPACE.get('raise');
var nothing600214 = NAMESPACE.get('nothing');
var check_DASH_brand600219 = NAMESPACE.get('check-brand');
var prim_DASH_has_DASH_field600224 = NAMESPACE.get('prim-has-field');
var prim_DASH_keys600229 = NAMESPACE.get('prim-keys');
var torepr600234 = NAMESPACE.get('torepr');
var prim_DASH_num_DASH_keys600239 = NAMESPACE.get('prim-num-keys');
var tostring600244 = NAMESPACE.get('tostring');
var brander600249 = NAMESPACE.get('brander');
var Placeholder600254 = NAMESPACE.get('Placeholder');
var Function600259 = NAMESPACE.get('Function');
var Bool600264 = NAMESPACE.get('Bool');
var Number600269 = NAMESPACE.get('Number');
var String600274 = NAMESPACE.get('String');
var Nothing600279 = NAMESPACE.get('Nothing');
var Method600284 = NAMESPACE.get('Method');
var data_DASH_equals600289 = NAMESPACE.get('data-equals');
var data_DASH_to_DASH_repr600294 = NAMESPACE.get('data-to-repr');
var equiv600299 = NAMESPACE.get('equiv');

              var RESULT;
              var EXPORT_NAMESPACE = RUNTIME.Namespace({});
              (function() {
                var identical600304 = RUNTIME.makeFunction(function (obj1551600600305,obj2551601600306) { return (function(){
 var obj1600309 = obj1551600600305;
var obj2600316 = obj2551601600306;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(has_DASH_field600323, [obj1600309,RUNTIME.makeString("eq")]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(has_DASH_field600323, [obj2600316,RUNTIME.makeString("eq")]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(obj1600309, "eq"), [obj2600316]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Identical got values that weren't created by data: "), "_plus"), [RUNTIME.applyFunc(torepr600234, [obj1600309])]), "_plus"), [RUNTIME.makeString(" and ")]), "_plus"), [RUNTIME.applyFunc(torepr600234, [obj2600316])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var mklist600452 = RUNTIME.makeFunction(function (obj551602600453) { return (function(){
 var obj600455 = obj551602600453;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.getField(obj600455, "is-empty"))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.getField(obj600455, "first"),RUNTIME.applyFunc(mklist600452, [RUNTIME.getField(obj600455, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Creates a List from something with `first` and `rest` fields, recursively"));
var keys600525 = RUNTIME.makeFunction(function (obj551603600526) { return (function(){
 var obj600455 = obj551603600526;
return (function(){
 return RUNTIME.applyFunc(mklist600452, [RUNTIME.applyFunc(prim_DASH_keys600229, [obj600455])]); 
})(); 
})(); }, RUNTIME.makeString("Returns a List of the keys of an object, as strings"));
var has_DASH_field600323 = RUNTIME.makeFunction(function (obj551604600564,name551605600565) { return (function(){
 var obj600455 = obj551604600564;
var name600574 = name551605600565;
return (function(){
 return RUNTIME.applyFunc(prim_DASH_has_DASH_field600224, [obj600455,name600574]); 
})(); 
})(); }, RUNTIME.makeString("Returns true if the object has a field with the name specified"));
var num_DASH_keys600607 = RUNTIME.makeFunction(function (obj551606600608) { return (function(){
 var obj600455 = obj551606600608;
return (function(){
 return RUNTIME.applyFunc(prim_DASH_num_DASH_keys600239, [obj600455]); 
})(); 
})(); }, RUNTIME.makeString("Returns the Number of fields in an object"));
var Eq600641 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 var b600642 = RUNTIME.applyFunc(brander600249, []);
return RUNTIME.makeObject({'extend':RUNTIME.makeFunction(function (obj551607600653) { return (function(){
 var obj600455 = obj551607600653;
return (function(){
 return obj600455.extend('eq', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(b600642, "test"), [self600661]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(b600642, "test"), [other600662]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))); 
})(); 
})(); }, RUNTIME.makeString("")),'brand':RUNTIME.makeFunction(function (obj551608600734) { return (function(){
 var obj600455 = obj551608600734;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(b600642, "brand"), [obj600455]); 
})(); 
})(); }, RUNTIME.makeString(""))}); 
})(); 
})(); }, RUNTIME.makeString(""));
var builtins600793 = RUNTIME.makeObject({'identical':identical600304,'keys':keys600525,'has-field':has_DASH_field600323,'mklist':mklist600452,'equiv':equiv600299,'data-to-repr':data_DASH_to_DASH_repr600294,'data-equals':data_DASH_equals600289,'Eq':Eq600641});
var get_DASH_help600842 = RUNTIME.makeFunction(function (lst551609600843,n551610600844) { return (function(){
 var lst600847 = lst551609600843;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551613600855) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551613600855,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551610600844]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l551611600888,cur551612600889) { return (function(){
 var l600892 = l551611600888;
var cur600899 = cur551612600889;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [l600892]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("get: n too large "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [cur600899,RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.getField(l600892, "first"); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help600887, [RUNTIME.getField(l600892, "rest"),RUNTIME.applyFunc(RUNTIME.getField(cur600899, "_minus"), [RUNTIME.makeNumber(1)])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n600854, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("get: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help600887, [lst600847,n600854]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var set_DASH_help601092 = RUNTIME.makeFunction(function (lst551614601093,n551615601094,v551616601095) { return (function(){
 var lst600847 = lst551614601093;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551619601105) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551619601105,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551615601094]);
var v601137 = v551616601095;
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l551617601144,cur551618601145) { return (function(){
 var l600892 = l551617601144;
var cur600899 = cur551618601145;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [l600892]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("set: n too large "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [cur600899,RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(link600471, [v601137,RUNTIME.getField(l600892, "rest")]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.getField(l600892, "first"),RUNTIME.applyFunc(help600887, [RUNTIME.getField(l600892, "rest"),RUNTIME.applyFunc(RUNTIME.getField(cur600899, "_minus"), [RUNTIME.makeNumber(1)])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n600854, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("set: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help600887, [lst600847,n600854]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var reverse_DASH_help601361 = RUNTIME.makeFunction(function (lst551620601362,acc551621601363) { return (function(){
 var lst600847 = lst551620601362;
var acc601372 = acc551621601363;
return (function(){
 var cases_DASH_value551332601379 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551622601380) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551622601380,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst600847]);
return RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value551332601379, "_match"), [RUNTIME.makeObject({'empty':RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return acc601372; 
})(); 
})(); }, RUNTIME.makeString("")),'link':RUNTIME.makeFunction(function (first551623601435,rest551624601436) { return (function(){
 var first601439 = first551623601435;
var rest601446 = rest551624601436;
return (function(){
 return RUNTIME.applyFunc(reverse_DASH_help601361, [rest601446,RUNTIME.applyFunc(link600471, [first601439,acc601372])]); 
})(); 
})(); }, RUNTIME.makeString(""))}),RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(error601487, "cases-miss"), [RUNTIME.makeString("cases: no cases matched"),RUNTIME.applyFunc(RUNTIME.getField(error601487, "location"), [RUNTIME.makeString("moorings.arr"),RUNTIME.makeNumber(93),RUNTIME.makeNumber(2)]),RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString(""))]); 
})(); 
})(); }, RUNTIME.makeString(""));
var take_DASH_help601568 = RUNTIME.makeFunction(function (lst551625601569,n551626601570) { return (function(){
 var lst600847 = lst551625601569;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551629601579) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551629601579,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551626601570]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l551627601611,cur551628601612) { return (function(){
 var l600892 = l551627601611;
var cur600899 = cur551628601612;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [cur600899,RUNTIME.makeNumber(0)]))) { return (function(){
 return empty600466; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link601627, [l600892]))) { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.getField(l600892, "first"),RUNTIME.applyFunc(help600887, [RUNTIME.getField(l600892, "rest"),RUNTIME.applyFunc(RUNTIME.getField(cur600899, "_minus"), [RUNTIME.makeNumber(1)])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("take: n too large: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n600854, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("take: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help600887, [lst600847,n600854]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var drop_DASH_help601819 = RUNTIME.makeFunction(function (lst551630601820,n551631601821) { return (function(){
 var lst600847 = lst551630601820;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551634601830) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551634601830,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551631601821]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l551632601862,cur551633601863) { return (function(){
 var l600892 = l551632601862;
var cur600899 = cur551633601863;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [cur600899,RUNTIME.makeNumber(0)]))) { return (function(){
 return l600892; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link601627, [l600892]))) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(l600892, "rest"), "drop"), [RUNTIME.applyFunc(RUNTIME.getField(cur600899, "_minus"), [RUNTIME.makeNumber(1)])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("drop: n to large: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n600854, "_lessthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("drop: invalid argument: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [n600854])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help600887, [lst600847,n600854]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var list_DASH_to_DASH_set602062 = RUNTIME.makeFunction(function (lst551635602063) { return (function(){
 var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551638602065) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551638602065,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551635602063]);
return (function(){
 return RUNTIME.applyFunc(fold602097, [RUNTIME.makeFunction(function (s551636602098,elem551637602099) { return (function(){
 var s602102 = s551636602098;
var elem602109 = elem551637602099;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(s602102, "add"), [elem602109]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(__set602139, [RUNTIME.getField(list601511, "empty")]),lst600847]); 
})(); 
})(); }, RUNTIME.makeString("Convert a list into a set."));
var List_DASH_mixins551194602176 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551195602187 = RUNTIME.makeObject({'push':RUNTIME.makeMethod(function (self600661,elt602188) { return (function(){
 return RUNTIME.applyFunc(link600471, [elt602188,self600661]); 
})(); }, RUNTIME.makeString("Adds an element to the front of the list, returning a new list")),'_plus':RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551639602211) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract551639602211,RUNTIME.makeString("(List, List => Any)")]);
var fun551640602224 = RUNTIME.applyFunc(RUNTIME.getField(contract551639602211, "_fun"), []);
return RUNTIME.makeMethod(function (arg551641602239,arg551642602240) { return (function(){
 return RUNTIME.applyFunc(fun551640602224, [RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551643602243) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551643602243,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [arg551641602239]),RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551644602269) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551644602269,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [arg551642602240])]); 
})(); }, RUNTIME.makeString("internal contract for (List, List => Any)")).extend('_doc', RUNTIME.getField(contract551639602211, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (List, List => Any)")), [RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self600661, "append"), [other600662]); 
})(); }, RUNTIME.makeString(""))]),'to-set':RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551645602363) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract551645602363,RUNTIME.makeString("(List => Any)")]);
var fun551646602376 = RUNTIME.applyFunc(RUNTIME.getField(contract551645602363, "_fun"), []);
return RUNTIME.makeMethod(function (arg551647602391) { return (function(){
 return RUNTIME.applyFunc(fun551646602376, [RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551648602393) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551648602393,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [arg551647602391])]); 
})(); }, RUNTIME.makeString("internal contract for (List => Any)")).extend('_doc', RUNTIME.getField(contract551645602363, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (List => Any)")), [RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(list_DASH_to_DASH_set602062, [self600661]); 
})(); }, RUNTIME.makeString(""))])});
var variant551196602492 = data_DASH_shared551195602187.extend('length', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeNumber(0); 
})(); }, RUNTIME.makeString(""))).extend('each', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551649602510) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract551649602510,RUNTIME.makeString("(Any, (Any -> Nothing) => Any)")]);
var fun551650602523 = RUNTIME.applyFunc(RUNTIME.getField(contract551649602510, "_fun"), []);
return RUNTIME.makeMethod(function (arg551651602538,arg551652602539) { return (function(){
 return RUNTIME.applyFunc(fun551650602523, [arg551651602538,RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551653602542) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Function600259,contract551653602542,RUNTIME.makeString("(Any -> Nothing)")]);
var fun551654602555 = contract551653602542;
return RUNTIME.makeFunction(function (arg551655602562) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551656602564) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Nothing600279,specimen551656602564,RUNTIME.makeString("Nothing")]); 
})(); }, RUNTIME.makeString("internal contract for Nothing")), [RUNTIME.applyFunc(fun551654602555, [arg551655602562])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Nothing)")).extend('_doc', RUNTIME.getField(contract551653602542, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Nothing)")), [arg551652602539])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Nothing) => Any)")).extend('_doc', RUNTIME.getField(contract551649602510, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Nothing) => Any)")), [RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return nothing600214; 
})(); }, RUNTIME.makeString(""))])).extend('map', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551657602692) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract551657602692,RUNTIME.makeString("(Any, (Any -> Any) => Any)")]);
var fun551658602705 = RUNTIME.applyFunc(RUNTIME.getField(contract551657602692, "_fun"), []);
return RUNTIME.makeMethod(function (arg551659602720,arg551660602721) { return (function(){
 return RUNTIME.applyFunc(fun551658602705, [arg551659602720,arg551660602721]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Any) => Any)")).extend('_doc', RUNTIME.getField(contract551657602692, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Any) => Any)")), [RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return empty600466; 
})(); }, RUNTIME.makeString(""))])).extend('filter', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551661602783) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract551661602783,RUNTIME.makeString("(Any, (Any -> Bool) => Any)")]);
var fun551662602796 = RUNTIME.applyFunc(RUNTIME.getField(contract551661602783, "_fun"), []);
return RUNTIME.makeMethod(function (arg551663602811,arg551664602812) { return (function(){
 return RUNTIME.applyFunc(fun551662602796, [arg551663602811,RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551665602815) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Function600259,contract551665602815,RUNTIME.makeString("(Any -> Bool)")]);
var fun551666602828 = contract551665602815;
return RUNTIME.makeFunction(function (arg551667602835) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551668602837) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551668602837,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun551666602828, [arg551667602835])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract551665602815, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [arg551664602812])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")).extend('_doc', RUNTIME.getField(contract551661602783, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")), [RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return empty600466; 
})(); }, RUNTIME.makeString(""))])).extend('find', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551669602964) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract551669602964,RUNTIME.makeString("(Any, (Any -> Bool) => Any)")]);
var fun551670602977 = RUNTIME.applyFunc(RUNTIME.getField(contract551669602964, "_fun"), []);
return RUNTIME.makeMethod(function (arg551671602992,arg551672602993) { return (function(){
 return RUNTIME.applyFunc(fun551670602977, [arg551671602992,RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551673602996) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Function600259,contract551673602996,RUNTIME.makeString("(Any -> Bool)")]);
var fun551674603009 = contract551673602996;
return RUNTIME.makeFunction(function (arg551675603016) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551676603018) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551676603018,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun551674603009, [arg551675603016])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract551673602996, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [arg551672602993])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")).extend('_doc', RUNTIME.getField(contract551669602964, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, (Any -> Bool) => Any)")), [RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return none603126; 
})(); }, RUNTIME.makeString(""))])).extend('partition', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return RUNTIME.makeObject({'is-true':empty600466,'is-false':empty600466}); 
})(); }, RUNTIME.makeString(""))).extend('foldr', RUNTIME.makeMethod(function (self600661,f602670,base603174) { return (function(){
 return base603174; 
})(); }, RUNTIME.makeString(""))).extend('foldl', RUNTIME.makeMethod(function (self600661,f602670,base603174) { return (function(){
 return base603174; 
})(); }, RUNTIME.makeString(""))).extend('member', RUNTIME.makeMethod(function (self600661,elt602188) { return (function(){
 return RUNTIME.makeBool(false); 
})(); }, RUNTIME.makeString(""))).extend('append', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return other600662; 
})(); }, RUNTIME.makeString(""))).extend('last', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.makeString("last: took last of empty list")]); 
})(); }, RUNTIME.makeString(""))).extend('take', RUNTIME.makeMethod(function (self600661,n600854) { return (function(){
 return RUNTIME.applyFunc(take_DASH_help601568, [self600661,n600854]); 
})(); }, RUNTIME.makeString(""))).extend('drop', RUNTIME.makeMethod(function (self600661,n600854) { return (function(){
 return RUNTIME.applyFunc(drop_DASH_help601819, [self600661,n600854]); 
})(); }, RUNTIME.makeString(""))).extend('reverse', RUNTIME.makeMethod(function (self600661) { return (function(){
 return self600661; 
})(); }, RUNTIME.makeString(""))).extend('get', RUNTIME.makeMethod(function (self600661,n600854) { return (function(){
 return RUNTIME.applyFunc(get_DASH_help600842, [self600661,n600854]); 
})(); }, RUNTIME.makeString(""))).extend('set', RUNTIME.makeMethod(function (self600661,n600854,e603346) { return (function(){
 return RUNTIME.applyFunc(set_DASH_help601092, [self600661,n600854,e603346]); 
})(); }, RUNTIME.makeString(""))).extend('_equals', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [other600662]); 
})(); }, RUNTIME.makeString(""))).extend('tostring', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("[]"); 
})(); }, RUNTIME.makeString(""))).extend('_torepr', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("[]"); 
})(); }, RUNTIME.makeString(""))).extend('sort-by', RUNTIME.makeMethod(function (self600661,cmp603426,eq603427) { return (function(){
 return self600661; 
})(); }, RUNTIME.makeString(""))).extend('sort', RUNTIME.makeMethod(function (self600661) { return (function(){
 return self600661; 
})(); }, RUNTIME.makeString(""))).extend('join-str', RUNTIME.makeMethod(function (self600661,str603460) { return (function(){
 return RUNTIME.makeString(""); 
})(); }, RUNTIME.makeString("")));
var variant551197603485 = data_DASH_shared551195602187.extend('length', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeNumber(1), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "length"), [])]); 
})(); }, RUNTIME.makeString(""))).extend('each', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 RUNTIME.applyFunc(f602670, [RUNTIME.getField(self600661, "first")]);
return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "each"), [f602670]); 
})(); }, RUNTIME.makeString(""))).extend('map', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [RUNTIME.getField(self600661, "first")]),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "map"), [f602670])]); 
})(); }, RUNTIME.makeString(""))).extend('filter', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f602670, [RUNTIME.getField(self600661, "first")]))) { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.getField(self600661, "first"),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "filter"), [f602670])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "filter"), [f602670]); 
})(); } })(); 
})(); }, RUNTIME.makeString(""))).extend('partition', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return RUNTIME.applyFunc(partition603689, [f602670,self600661]); 
})(); }, RUNTIME.makeString(""))).extend('find', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return RUNTIME.applyFunc(find603712, [f602670,self600661]); 
})(); }, RUNTIME.makeString(""))).extend('member', RUNTIME.makeMethod(function (self600661,elt602188) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [elt602188,RUNTIME.getField(self600661, "first")]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "member"), [elt602188]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))).extend('foldr', RUNTIME.makeMethod(function (self600661,f602670,base603174) { return (function(){
 return RUNTIME.applyFunc(f602670, [RUNTIME.getField(self600661, "first"),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "foldr"), [f602670,base603174])]); 
})(); }, RUNTIME.makeString(""))).extend('foldl', RUNTIME.makeMethod(function (self600661,f602670,base603174) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "foldl"), [f602670,RUNTIME.applyFunc(f602670, [RUNTIME.getField(self600661, "first"),base603174])]); 
})(); }, RUNTIME.makeString(""))).extend('append', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.getField(self600661, "first"),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "append"), [other600662])]); 
})(); }, RUNTIME.makeString(""))).extend('last', RUNTIME.makeMethod(function (self600661) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [RUNTIME.getField(self600661, "rest")]))) { return (function(){
 return RUNTIME.getField(self600661, "first"); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "last"), []); 
})(); } })(); 
})(); }, RUNTIME.makeString(""))).extend('reverse', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(reverse_DASH_help601361, [self600661,empty600466]); 
})(); }, RUNTIME.makeString(""))).extend('take', RUNTIME.makeMethod(function (self600661,n600854) { return (function(){
 return RUNTIME.applyFunc(take_DASH_help601568, [self600661,n600854]); 
})(); }, RUNTIME.makeString(""))).extend('drop', RUNTIME.makeMethod(function (self600661,n600854) { return (function(){
 return RUNTIME.applyFunc(drop_DASH_help601819, [self600661,n600854]); 
})(); }, RUNTIME.makeString(""))).extend('get', RUNTIME.makeMethod(function (self600661,n600854) { return (function(){
 return RUNTIME.applyFunc(get_DASH_help600842, [self600661,n600854]); 
})(); }, RUNTIME.makeString(""))).extend('set', RUNTIME.makeMethod(function (self600661,n600854,e603346) { return (function(){
 return RUNTIME.applyFunc(set_DASH_help601092, [self600661,n600854,e603346]); 
})(); }, RUNTIME.makeString(""))).extend('_equals', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link601627, [other600662]))) { return (function(){
 var others_DASH_equal604092 = RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getRawField(self600661, 'first'),RUNTIME.getRawField(other600662, 'first')]);
return RUNTIME.applyFunc(RUNTIME.getField(others_DASH_equal604092, "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getRawField(self600661, 'rest'),RUNTIME.getRawField(other600662, 'rest')]); 
})(); }, RUNTIME.makeString(""))]); 
})(); } else { return (function(){
 return RUNTIME.makeBool(false); 
})(); } })(); 
})(); }, RUNTIME.makeString(""))).extend('tostring', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("["), "_plus"), [RUNTIME.applyFunc(raw_DASH_fold604195, [RUNTIME.makeFunction(function (combined551677604196,elt551678604197) { return (function(){
 var combined604200 = combined551677604196;
var elt602188 = elt551678604197;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(combined604200, "_plus"), [RUNTIME.makeString(", ")]), "_plus"), [RUNTIME.applyFunc(tostring600244, [elt602188])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(tostring600244, [RUNTIME.getRawField(self600661, 'first')]),RUNTIME.getRawField(self600661, 'rest')])]), "_plus"), [RUNTIME.makeString("]")]); 
})(); }, RUNTIME.makeString(""))).extend('_torepr', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("["), "_plus"), [RUNTIME.applyFunc(raw_DASH_fold604195, [RUNTIME.makeFunction(function (combined551679604309,elt551680604310) { return (function(){
 var combined604200 = combined551679604309;
var elt602188 = elt551680604310;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(combined604200, "_plus"), [RUNTIME.makeString(", ")]), "_plus"), [RUNTIME.applyFunc(torepr600234, [elt602188])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(torepr600234, [RUNTIME.getRawField(self600661, 'first')]),RUNTIME.getRawField(self600661, 'rest')])]), "_plus"), [RUNTIME.makeString("]")]); 
})(); }, RUNTIME.makeString(""))).extend('sort-by', RUNTIME.makeMethod(function (self600661,cmp603426,eq603427) { return (function(){
 var pivot604417 = RUNTIME.getField(self600661, "first");
var less604428 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(self600661, "filter"), [RUNTIME.makeFunction(function (e551681604433) { return (function(){
 var e603346 = e551681604433;
return (function(){
 return RUNTIME.applyFunc(cmp603426, [e603346,pivot604417]); 
})(); 
})(); }, RUNTIME.makeString(""))]), "sort-by"), [cmp603426,eq603427]);
var equal604482 = RUNTIME.applyFunc(RUNTIME.getField(self600661, "filter"), [RUNTIME.makeFunction(function (e551682604487) { return (function(){
 var e603346 = e551682604487;
return (function(){
 return RUNTIME.applyFunc(eq603427, [e603346,pivot604417]); 
})(); 
})(); }, RUNTIME.makeString(""))]);
var greater604526 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(self600661, "filter"), [RUNTIME.makeFunction(function (e551683604531) { return (function(){
 var e603346 = e551683604531;
return (function(){
 return RUNTIME.applyFunc(cmp603426, [pivot604417,e603346]); 
})(); 
})(); }, RUNTIME.makeString(""))]), "sort-by"), [cmp603426,eq603427]);
return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(less604428, "append"), [equal604482]), "append"), [greater604526]); 
})(); }, RUNTIME.makeString("Takes a comparator to check for elements that are strictly greater\n        or less than one another, and an equality procedure for elements that are\n        equal, and sorts the list accordingly."))).extend('sort', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self600661, "sort-by"), [RUNTIME.makeFunction(function (e1551684604617,e2551685604618) { return (function(){
 var e1604621 = e1551684604617;
var e2604628 = e2551685604618;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(e1604621, "_lessthan"), [e2604628]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.makeFunction(function (e1551686604658,e2551687604659) { return (function(){
 var e1604621 = e1551686604658;
var e2604628 = e2551687604659;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [e1604621,e2604628]); 
})(); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))).extend('join-str', RUNTIME.makeMethod(function (self600661,str603460) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_link601627, [RUNTIME.getField(self600661, "rest")]))) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(tostring600244, [RUNTIME.getField(self600661, "first")]), "_plus"), [str603460]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "rest"), "join-str"), [str603460])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(tostring600244, [RUNTIME.getField(self600661, "first")]); 
})(); } })(); 
})(); }, RUNTIME.makeString("")));
var List551333604816 = RUNTIME.applyFunc(brander600249, []);
var empty_base551336604827 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("empty"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_empty600930,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("empty")]))) { return (function(){
 var call_DASH_empty551335604886 = RUNTIME.getField(cases_DASH_funs604869, "empty");
return RUNTIME.applyFunc(call_DASH_empty551335604886, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var empty551334604940 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_empty600930 = RUNTIME.getField(empty551334604940, "test");
var empty600466 = RUNTIME.applyFunc(RUNTIME.getField(List551333604816, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(empty551334604940, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551688604969) { return (function(){
 var self600661 = self551688604969;
return (function(){
 var mixin551198604977 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [List_DASH_mixins551194602176]))) { return RUNTIME.applyFunc(List_DASH_mixins551194602176, []); } else { return List_DASH_mixins551194602176; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551198604977, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551198604977, "extend"), [self600661.extend('push', RUNTIME.getRawField(data_DASH_shared551195602187, 'push')).extend('_plus', RUNTIME.getRawField(data_DASH_shared551195602187, '_plus')).extend('to-set', RUNTIME.getRawField(data_DASH_shared551195602187, 'to-set')).extend('length', RUNTIME.getRawField(variant551196602492, 'length')).extend('each', RUNTIME.getRawField(variant551196602492, 'each')).extend('map', RUNTIME.getRawField(variant551196602492, 'map')).extend('filter', RUNTIME.getRawField(variant551196602492, 'filter')).extend('find', RUNTIME.getRawField(variant551196602492, 'find')).extend('partition', RUNTIME.getRawField(variant551196602492, 'partition')).extend('foldr', RUNTIME.getRawField(variant551196602492, 'foldr')).extend('foldl', RUNTIME.getRawField(variant551196602492, 'foldl')).extend('member', RUNTIME.getRawField(variant551196602492, 'member')).extend('append', RUNTIME.getRawField(variant551196602492, 'append')).extend('last', RUNTIME.getRawField(variant551196602492, 'last')).extend('take', RUNTIME.getRawField(variant551196602492, 'take')).extend('drop', RUNTIME.getRawField(variant551196602492, 'drop')).extend('reverse', RUNTIME.getRawField(variant551196602492, 'reverse')).extend('get', RUNTIME.getRawField(variant551196602492, 'get')).extend('set', RUNTIME.getRawField(variant551196602492, 'set')).extend('_equals', RUNTIME.getRawField(variant551196602492, '_equals')).extend('tostring', RUNTIME.getRawField(variant551196602492, 'tostring')).extend('_torepr', RUNTIME.getRawField(variant551196602492, '_torepr')).extend('sort-by', RUNTIME.getRawField(variant551196602492, 'sort-by')).extend('sort', RUNTIME.getRawField(variant551196602492, 'sort')).extend('join-str', RUNTIME.getRawField(variant551196602492, 'join-str'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for empty")), [empty_base551336604827])])]);
var link_base551339605256 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("link"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("first"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("rest"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_link601627,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("first"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("rest"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("link")]))) { return (function(){
 var call_DASH_link551337605376 = RUNTIME.getField(cases_DASH_funs604869, "link");
return RUNTIME.applyFunc(call_DASH_link551337605376, [RUNTIME.getMutableField(self600661, 'first'),RUNTIME.getMutableField(self600661, 'rest')]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var link551338605440 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_link601627 = RUNTIME.getField(link551338605440, "test");
var link600471 = RUNTIME.makeFunction(function (first551340551689605461,rest551341551690605462) { return (function(){
 var first551340605465 = first551340551689605461;
var rest551341605472 = rest551341551690605462;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(List551333604816, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(link551338605440, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551691605487) { return (function(){
 var self600661 = self551691605487;
return (function(){
 var mixin551199605495 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [List_DASH_mixins551194602176]))) { return RUNTIME.applyFunc(List_DASH_mixins551194602176, []); } else { return List_DASH_mixins551194602176; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551199605495, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551199605495, "extend"), [self600661.extend('push', RUNTIME.getRawField(data_DASH_shared551195602187, 'push')).extend('_plus', RUNTIME.getRawField(data_DASH_shared551195602187, '_plus')).extend('to-set', RUNTIME.getRawField(data_DASH_shared551195602187, 'to-set')).extend('length', RUNTIME.getRawField(variant551197603485, 'length')).extend('each', RUNTIME.getRawField(variant551197603485, 'each')).extend('map', RUNTIME.getRawField(variant551197603485, 'map')).extend('filter', RUNTIME.getRawField(variant551197603485, 'filter')).extend('partition', RUNTIME.getRawField(variant551197603485, 'partition')).extend('find', RUNTIME.getRawField(variant551197603485, 'find')).extend('member', RUNTIME.getRawField(variant551197603485, 'member')).extend('foldr', RUNTIME.getRawField(variant551197603485, 'foldr')).extend('foldl', RUNTIME.getRawField(variant551197603485, 'foldl')).extend('append', RUNTIME.getRawField(variant551197603485, 'append')).extend('last', RUNTIME.getRawField(variant551197603485, 'last')).extend('reverse', RUNTIME.getRawField(variant551197603485, 'reverse')).extend('take', RUNTIME.getRawField(variant551197603485, 'take')).extend('drop', RUNTIME.getRawField(variant551197603485, 'drop')).extend('get', RUNTIME.getRawField(variant551197603485, 'get')).extend('set', RUNTIME.getRawField(variant551197603485, 'set')).extend('_equals', RUNTIME.getRawField(variant551197603485, '_equals')).extend('tostring', RUNTIME.getRawField(variant551197603485, 'tostring')).extend('_torepr', RUNTIME.getRawField(variant551197603485, '_torepr')).extend('sort-by', RUNTIME.getRawField(variant551197603485, 'sort-by')).extend('sort', RUNTIME.getRawField(variant551197603485, 'sort')).extend('join-str', RUNTIME.getRawField(variant551197603485, 'join-str'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for link")), [link_base551339605256.extend('first', first551340605465).extend('rest', (function(){
 var maybe_DASH_placeholder551693605757 = rest551341605472;
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Placeholder600254, [maybe_DASH_placeholder551693605757]))) { return (function(){
 RUNTIME.applyFunc(RUNTIME.getField(maybe_DASH_placeholder551693605757, "guard"), [RUNTIME.makeFunction(function (specimen551692605773) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551692605773,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List"))]);
return maybe_DASH_placeholder551693605757; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551692605773) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551692605773,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [maybe_DASH_placeholder551693605757]); 
})(); } })(); 
})())])])]); 
})(); 
})(); }, RUNTIME.makeString("link: Creates an instance of link"));
var List601382 = RUNTIME.getField(List551333604816, "test");
var range605897 = RUNTIME.makeFunction(function (start551694605898,stop551695605899) { return (function(){
 var start605902 = start551694605898;
var stop605909 = stop551695605899;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(start605902, "_lessthan"), [stop605909]))) { return (function(){
 return RUNTIME.applyFunc(link600471, [start605902,RUNTIME.applyFunc(range605897, [RUNTIME.applyFunc(RUNTIME.getField(start605902, "_plus"), [RUNTIME.makeNumber(1)]),stop605909])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [start605902,stop605909]))) { return (function(){
 return empty600466; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(start605902, "_greaterthan"), [stop605909]))) { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("range: start greater than stop: ("), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(start605902, "tostring"), [])]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(stop605909, "tostring"), [])])]), "_plus"), [RUNTIME.makeString(")")])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.makeString("if: no tests matched")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Creates a list of numbers, starting with start, ending with stop-1"));
var repeat606091 = RUNTIME.makeFunction(function (n551696606092,e551697606093) { return (function(){
 var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551699606096) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551699606096,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551696606092]);
var e603346 = e551697606093;
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551698606134) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551698606134,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [(function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(n600854, "_greaterthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(link600471, [e603346,RUNTIME.applyFunc(repeat606091, [RUNTIME.applyFunc(RUNTIME.getField(n600854, "_minus"), [RUNTIME.makeNumber(1)]),e603346])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [n600854,RUNTIME.makeNumber(0)]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.makeString("repeat: can't have a negative argument'")]); 
})(); } })(); 
})()]); 
})(); }, RUNTIME.makeString("Creates a list with n copies of e"));
var filter606259 = RUNTIME.makeFunction(function (f551700606260,lst551701606261) { return (function(){
 var f602670 = f551700606260;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551702606270) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551702606270,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551701606261]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [lst600847]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f602670, [RUNTIME.getField(lst600847, "first")]))) { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.getField(lst600847, "first"),RUNTIME.applyFunc(filter606259, [f602670,RUNTIME.getField(lst600847, "rest")])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(filter606259, [f602670,RUNTIME.getField(lst600847, "rest")]); 
})(); } })(); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns the subset of lst for which f(elem) is true"));
var partition603689 = RUNTIME.makeFunction(function (f551703606402,lst551704606403) { return (function(){
 var f602670 = f551703606402;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551706606412) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551706606412,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551704606403]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (inner_DASH_lst551705606444) { return (function(){
 var inner_DASH_lst606446 = inner_DASH_lst551705606444;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [inner_DASH_lst606446]))) { return (function(){
 return RUNTIME.makeObject({'is-true':RUNTIME.getField(list601511, "empty"),'is-false':RUNTIME.getField(list601511, "empty")}); 
})(); } else { return (function(){
 var split_DASH_tail606482 = RUNTIME.applyFunc(help600887, [RUNTIME.getField(inner_DASH_lst606446, "rest")]);
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f602670, [RUNTIME.getField(inner_DASH_lst606446, "first")]))) { return (function(){
 return RUNTIME.makeObject({'is-true':RUNTIME.applyFunc(link600471, [RUNTIME.getField(inner_DASH_lst606446, "first"),RUNTIME.getField(split_DASH_tail606482, "is-true")]),'is-false':RUNTIME.getField(split_DASH_tail606482, "is-false")}); 
})(); } else { return (function(){
 return RUNTIME.makeObject({'is-true':RUNTIME.getField(split_DASH_tail606482, "is-true"),'is-false':RUNTIME.applyFunc(link600471, [RUNTIME.getField(inner_DASH_lst606446, "first"),RUNTIME.getField(split_DASH_tail606482, "is-false")])}); 
})(); } })(); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [lst600847]); 
})(); 
})(); }, RUNTIME.makeString("Splits the list into two lists, one for which f(elem) is true, and one for which f(elem) is false"));
var any606644 = RUNTIME.makeFunction(function (f551707606645,lst551708606646) { return (function(){
 var f602670 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551710606649) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Function600259,contract551710606649,RUNTIME.makeString("(Any -> Bool)")]);
var fun551711606662 = contract551710606649;
return RUNTIME.makeFunction(function (arg551712606669) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551713606671) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551713606671,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun551711606662, [arg551712606669])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract551710606649, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [f551707606645]);
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551714606745) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551714606745,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551708606646]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551709606777) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551709606777,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [(function(){
 return RUNTIME.applyFunc(is_DASH_some606798, [RUNTIME.applyFunc(find603712, [f602670,lst600847])]); 
})()]); 
})(); }, RUNTIME.makeString("Returns true if f(elem) returns true for any elem of lst"));
var all606835 = RUNTIME.makeFunction(function (f551719606836,lst551720606837) { return (function(){
 var f602670 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551723606840) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Function600259,contract551723606840,RUNTIME.makeString("(Any -> Bool)")]);
var fun551724606853 = contract551723606840;
return RUNTIME.makeFunction(function (arg551725606860) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551726606862) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551726606862,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun551724606853, [arg551725606860])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract551723606840, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [f551719606836]);
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551727606936) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551727606936,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551720606837]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551722606968) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551722606968,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [(function(){
 return RUNTIME.applyFunc(is_DASH_none606989, [RUNTIME.applyFunc(find603712, [RUNTIME.makeFunction(function (v551721606990) { return (function(){
 var v601137 = v551721606990;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(f602670, [v601137]), "_not"), []); 
})(); 
})(); }, RUNTIME.makeString("")),lst600847])]); 
})()]); 
})(); }, RUNTIME.makeString("Returns true if f(elem) returns true for all elems of lst"));
var find603712 = RUNTIME.makeFunction(function (f551732607061,lst551733607062) { return (function(){
 var f602670 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract551735607065) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Function600259,contract551735607065,RUNTIME.makeString("(Any -> Bool)")]);
var fun551736607078 = contract551735607065;
return RUNTIME.makeFunction(function (arg551737607085) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551738607087) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Bool600264,specimen551738607087,RUNTIME.makeString("Bool")]); 
})(); }, RUNTIME.makeString("internal contract for Bool")), [RUNTIME.applyFunc(fun551736607078, [arg551737607085])]); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")).extend('_doc', RUNTIME.getField(contract551735607065, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any -> Bool)")), [f551732607061]);
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551739607161) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551739607161,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551733607062]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551734607193) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Option607195,specimen551734607193,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [(function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [lst600847]))) { return (function(){
 return none603126; 
})(); } else { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(f602670, [RUNTIME.getField(lst600847, "first")]))) { return (function(){
 return RUNTIME.applyFunc(some607233, [RUNTIME.getField(lst600847, "first")]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(find603712, [f602670,RUNTIME.getField(lst600847, "rest")]); 
})(); } })(); 
})(); } })(); 
})()]); 
})(); }, RUNTIME.makeString("Returns some(elem) where elem is the first elem in lst for which\n        f(elem) returns true, or none otherwise"));
var map607310 = RUNTIME.makeFunction(function (f551746607311,lst551747607312) { return (function(){
 var f602670 = f551746607311;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551748607321) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551748607321,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551747607312]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [lst600847]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [RUNTIME.getField(lst600847, "first")]),RUNTIME.applyFunc(map607310, [f602670,RUNTIME.getField(lst600847, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(elem) for each elem in lst"));
var map2607421 = RUNTIME.makeFunction(function (f551749607422,l1551750607423,l2551751607424) { return (function(){
 var f602670 = f551749607422;
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551752607435) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551752607435,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551750607423]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551753607468) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551753607468,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551751607424]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first")]),RUNTIME.applyFunc(map2607421, [f602670,RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(elem1, elem2) for each elem1 in l1, elem2 in l2"));
var map3607602 = RUNTIME.makeFunction(function (f551754607603,l1551755607604,l2551756607605,l3551757607606) { return (function(){
 var f602670 = f551754607603;
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551758607617) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551758607617,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551755607604]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551759607649) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551759607649,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551756607605]);
var l3607681 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551760607682) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551760607682,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3551757607606]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first")]),RUNTIME.applyFunc(map3607602, [f602670,RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(e1, e2, e3) for each e1 in l1, e2 in l2, e3 in l3"));
var map4607850 = RUNTIME.makeFunction(function (f551761607851,l1551762607852,l2551763607853,l3551764607854,l4551765607855) { return (function(){
 var f602670 = f551761607851;
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551766607867) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551766607867,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551762607852]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551767607899) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551767607899,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551763607853]);
var l3607681 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551768607931) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551768607931,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3551764607854]);
var l4607963 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551769607964) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551769607964,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l4551765607855]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l4607963]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first"),RUNTIME.getField(l4607963, "first")]),RUNTIME.applyFunc(map4607850, [f602670,RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest"),RUNTIME.getField(l4607963, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(e1, e2, e3, e4) for each e1 in l1, e2 in l2, e3 in l3, e4 in l4"));
var map_n608166 = RUNTIME.makeFunction(function (f551770608167,n551771608168,lst551772608169) { return (function(){
 var f602670 = f551770608167;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551773608179) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551773608179,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551771608168]);
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551774608211) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551774608211,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551772608169]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [lst600847]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(lst600847, "first")]),RUNTIME.applyFunc(map_n608166, [f602670,RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(lst600847, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Returns a list made up of f(n, e1), f(n+1, e2) .. for e1, e2 ... in lst"));
var map2_n608324 = RUNTIME.makeFunction(function (f551775608325,n551776608326,l1551777608327,l2551778608328) { return (function(){
 var f602670 = f551775608325;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551779608339) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551779608339,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551776608326]);
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551780608371) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551780608371,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551777608327]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551781608403) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551781608403,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551778608328]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first")]),RUNTIME.applyFunc(map2_n608324, [f602670,RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var map3_n608550 = RUNTIME.makeFunction(function (f551782608551,n551783608552,l1551784608553,l2551785608554,l3551786608555) { return (function(){
 var f602670 = f551782608551;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551787608567) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551787608567,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551783608552]);
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551788608599) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551788608599,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551784608553]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551789608631) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551789608631,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551785608554]);
var l3607681 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551790608663) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551790608663,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3551786608555]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first")]),RUNTIME.applyFunc(map3_n608550, [f602670,RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var map4_n608844 = RUNTIME.makeFunction(function (f551791608845,n551792608846,l1551793608847,l2551794608848,l3551795608849,l4551796608850) { return (function(){
 var f602670 = f551791608845;
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551797608863) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551797608863,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551792608846]);
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551798608895) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551798608895,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551793608847]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551799608927) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551799608927,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551794608848]);
var l3607681 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551800608959) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551800608959,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3551795608849]);
var l4607963 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551801608991) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551801608991,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l4551796608850]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l4607963]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return empty600466; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(link600471, [RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first"),RUNTIME.getField(l4607963, "first")]),RUNTIME.applyFunc(map4607850, [f602670,RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest"),RUNTIME.getField(l4607963, "rest")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var each609206 = RUNTIME.makeFunction(function (f551802609207,lst551803609208) { return (function(){
 var f602670 = f551802609207;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551805609217) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551805609217,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551803609208]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l551804609249) { return (function(){
 var l600892 = l551804609249;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [l600892]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [RUNTIME.getField(l600892, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.getField(l600892, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [lst600847]); 
})(); 
})(); }, RUNTIME.makeString("Calls f for each elem in lst, and returns nothing"));
var each2609345 = RUNTIME.makeFunction(function (f551806609346,lst1551807609347,lst2551808609348) { return (function(){
 var f602670 = f551806609346;
var lst1609358 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551811609359) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551811609359,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1551807609347]);
var lst2609391 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551812609392) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551812609392,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2551808609348]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l1551809609424,l2551810609425) { return (function(){
 var l1607434 = l1551809609424;
var l2607467 = l2551810609425;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [lst1609358,lst2609391]); 
})(); 
})(); }, RUNTIME.makeString("Calls f on each pair of corresponding elements in l1 and l2, and returns nothing.  Stops after the shortest list"));
var each3609563 = RUNTIME.makeFunction(function (f551813609564,lst1551814609565,lst2551815609566,lst3551816609567) { return (function(){
 var f602670 = f551813609564;
var lst1609358 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551820609578) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551820609578,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1551814609565]);
var lst2609391 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551821609610) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551821609610,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2551815609566]);
var lst3609642 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551822609643) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551822609643,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3551816609567]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l1551817609675,l2551818609676,l3551819609677) { return (function(){
 var l1607434 = l1551817609675;
var l2607467 = l2551818609676;
var l3607681 = l3551819609677;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [lst1609358,lst2609391,lst3609642]); 
})(); 
})(); }, RUNTIME.makeString("Calls f on each triple of corresponding elements in l1, l2 and l3, and returns nothing.  Stops after the shortest list"));
var each4609857 = RUNTIME.makeFunction(function (f551823609858,lst1551824609859,lst2551825609860,lst3551826609861,lst4551827609862) { return (function(){
 var f602670 = f551823609858;
var lst1609358 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551832609874) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551832609874,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1551824609859]);
var lst2609391 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551833609906) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551833609906,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2551825609860]);
var lst3609642 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551834609938) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551834609938,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3551826609861]);
var lst4609970 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551835609971) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551835609971,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst4551827609862]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (l1551828610003,l2551829610004,l3551830610005,l4551831610006) { return (function(){
 var l1607434 = l1551828610003;
var l2607467 = l2551829610004;
var l3607681 = l3551830610005;
var l4607963 = l4551831610006;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l4607963]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first"),RUNTIME.getField(l4607963, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest"),RUNTIME.getField(l4607963, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [lst1609358,lst2609391,lst3609642,lst4609970]); 
})(); 
})(); }, RUNTIME.makeString("Calls f on each tuple of corresponding elements in l1, l2, l3 and l4, and returns nothing.  Stops after the shortest list"));
var each_n610228 = RUNTIME.makeFunction(function (f551836610229,num551837610230,lst551838610231) { return (function(){
 var f602670 = f551836610229;
var num610241 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551841610242) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551841610242,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num551837610230]);
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551842610274) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551842610274,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551838610231]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (n551839610306,l551840610307) { return (function(){
 var n600854 = n551839610306;
var l600892 = l551840610307;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [l600892]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l600892, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l600892, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [num610241,lst600847]); 
})(); 
})(); }, RUNTIME.makeString(""));
var each2_n610424 = RUNTIME.makeFunction(function (f551843610425,num551844610426,lst1551845610427,lst2551846610428) { return (function(){
 var f602670 = f551843610425;
var num610241 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551850610439) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551850610439,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num551844610426]);
var lst1609358 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551851610471) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551851610471,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1551845610427]);
var lst2609391 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551852610503) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551852610503,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2551846610428]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (n551847610535,l1551848610536,l2551849610537) { return (function(){
 var n600854 = n551847610535;
var l1607434 = l1551848610536;
var l2607467 = l2551849610537;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [num610241,lst1609358,lst2609391]); 
})(); 
})(); }, RUNTIME.makeString(""));
var each3_n610696 = RUNTIME.makeFunction(function (f551853610697,num551854610698,lst1551855610699,lst2551856610700,lst3551857610701) { return (function(){
 var f602670 = f551853610697;
var num610241 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551862610713) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551862610713,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num551854610698]);
var lst1609358 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551863610745) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551863610745,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1551855610699]);
var lst2609391 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551864610777) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551864610777,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2551856610700]);
var lst3609642 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551865610809) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551865610809,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3551857610701]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (n551858610841,l1551859610842,l2551860610843,l3551861610844) { return (function(){
 var n600854 = n551858610841;
var l1607434 = l1551859610842;
var l2607467 = l2551860610843;
var l3607681 = l3551861610844;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [num610241,lst1609358,lst2609391,lst3609642]); 
})(); 
})(); }, RUNTIME.makeString(""));
var each4_n611045 = RUNTIME.makeFunction(function (f551866611046,num551867611047,lst1551868611048,lst2551869611049,lst3551870611050,lst4551871611051) { return (function(){
 var f602670 = f551866611046;
var num610241 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551877611064) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551877611064,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num551867611047]);
var lst1609358 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551878611096) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551878611096,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst1551868611048]);
var lst2609391 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551879611128) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551879611128,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst2551869611049]);
var lst3609642 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551880611160) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551880611160,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst3551870611050]);
var lst4609970 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551881611192) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551881611192,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst4551871611051]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (n551872611224,l1551873611225,l2551874611226,l3551875611227,l4551876611228) { return (function(){
 var n600854 = n551872611224;
var l1607434 = l1551873611225;
var l2607467 = l2551874611226;
var l3607681 = l3551875611227;
var l4607963 = l4551876611228;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l4607963]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return nothing600214; 
})(); } else { return (function(){
 RUNTIME.applyFunc(f602670, [n600854,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first"),RUNTIME.getField(l4607963, "first")]);
return RUNTIME.applyFunc(help600887, [RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest"),RUNTIME.getField(l4607963, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [num610241,lst1609358,lst2609391,lst3609642,lst4609970]); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold602097 = RUNTIME.makeFunction(function (f551882611471,base551883611472,lst551884611473) { return (function(){
 var f602670 = f551882611471;
var base603174 = base551883611472;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551885611489) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551885611489,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551884611473]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [lst600847]))) { return (function(){
 return base603174; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold602097, [f602670,RUNTIME.applyFunc(f602670, [base603174,RUNTIME.getField(lst600847, "first")]),RUNTIME.getField(lst600847, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold2611585 = RUNTIME.makeFunction(function (f551886611586,base551887611587,l1551888611588,l2551889611589) { return (function(){
 var f602670 = f551886611586;
var base603174 = base551887611587;
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551890611606) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551890611606,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551888611588]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551891611638) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551891611638,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551889611589]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return base603174; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold2611585, [f602670,RUNTIME.applyFunc(f602670, [base603174,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first")]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold3611768 = RUNTIME.makeFunction(function (f551892611769,base551893611770,l1551894611771,l2551895611772,l3551896611773) { return (function(){
 var f602670 = f551892611769;
var base603174 = base551893611770;
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551897611791) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551897611791,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551894611771]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551898611823) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551898611823,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551895611772]);
var l3607681 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551899611855) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551899611855,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3551896611773]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return base603174; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold3611768, [f602670,RUNTIME.applyFunc(f602670, [base603174,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first")]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold4612019 = RUNTIME.makeFunction(function (f551900612020,base551901612021,l1551902612022,l2551903612023,l3551904612024,l4551905612025) { return (function(){
 var f602670 = f551900612020;
var base603174 = base551901612021;
var l1607434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551906612044) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551906612044,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l1551902612022]);
var l2607467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551907612076) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551907612076,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l2551903612023]);
var l3607681 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551908612108) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551908612108,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l3551904612024]);
var l4607963 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551909612140) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551909612140,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l4551905612025]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_empty600930, [l1607434]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l2607467]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l3607681]); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(is_DASH_empty600930, [l4607963]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return base603174; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(fold4612019, [f602670,RUNTIME.applyFunc(f602670, [base603174,RUNTIME.getField(l1607434, "first"),RUNTIME.getField(l2607467, "first"),RUNTIME.getField(l3607681, "first"),RUNTIME.getField(l4607963, "first")]),RUNTIME.getField(l1607434, "rest"),RUNTIME.getField(l2607467, "rest"),RUNTIME.getField(l3607681, "rest"),RUNTIME.getField(l4607963, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var fold_n612338 = RUNTIME.makeFunction(function (f551910612339,num551911612340,base551912612341,lst551913612342) { return (function(){
 var f602670 = f551910612339;
var num610241 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551917612353) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551917612353,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [num551911612340]);
var base603174 = base551912612341;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551918612391) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551918612391,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551913612342]);
return (function(){
 var help600887 = RUNTIME.makeFunction(function (n551914612423,acc551915612424,partial_DASH_list551916612425) { return (function(){
 var n600854 = n551914612423;
var acc601372 = acc551915612424;
var partial_DASH_list612441 = partial_DASH_list551916612425;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [partial_DASH_list612441]))) { return (function(){
 return acc601372; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(help600887, [RUNTIME.applyFunc(RUNTIME.getField(n600854, "_plus"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(f602670, [n600854,base603174,RUNTIME.getField(partial_DASH_list612441, "first")]),RUNTIME.getField(partial_DASH_list612441, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(help600887, [RUNTIME.makeNumber(0),base603174,lst600847]); 
})(); 
})(); }, RUNTIME.makeString(""));
var raw_DASH_fold604195 = RUNTIME.makeFunction(function (f551919612553,base551920612554,lst551921612555) { return (function(){
 var f602670 = f551919612553;
var base603174 = base551920612554;
var lst600847 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551922612571) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551922612571,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [lst551921612555]);
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_empty600930, [lst600847]))) { return (function(){
 return base603174; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raw_DASH_fold604195, [f602670,RUNTIME.applyFunc(f602670, [base603174,RUNTIME.getRawField(lst600847, 'first')]),RUNTIME.getField(lst600847, "rest")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var index612667 = RUNTIME.makeFunction(function (l551923612668,n551924612669) { return (function(){
 var l600892 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551928612672) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551928612672,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l551923612668]);
var n600854 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551929612704) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen551929612704,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [n551924612669]);
return (function(){
 var cases_DASH_value551342612736 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551925612737) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551925612737,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [l600892]);
return RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value551342612736, "_match"), [RUNTIME.makeObject({'empty':RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.makeString("index: list too short, avast!")]); 
})(); 
})(); }, RUNTIME.makeString("")),'link':RUNTIME.makeFunction(function (f551926612798,r551927612799) { return (function(){
 var f602670 = f551926612798;
var r612808 = r551927612799;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [n600854,RUNTIME.makeNumber(0)]))) { return (function(){
 return f602670; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(index612667, [r612808,RUNTIME.applyFunc(RUNTIME.getField(n600854, "_minus"), [RUNTIME.makeNumber(1)])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""))}),RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(error601487, "cases-miss"), [RUNTIME.makeString("cases: no cases matched"),RUNTIME.applyFunc(RUNTIME.getField(error601487, "location"), [RUNTIME.makeString("moorings.arr"),RUNTIME.makeNumber(613),RUNTIME.makeNumber(2)]),RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString(""))]); 
})(); 
})(); }, RUNTIME.makeString(""));
var list601511 = RUNTIME.makeObject({'List':List601382,'is-empty':is_DASH_empty600930,'is-link':is_DASH_link601627,'empty':empty600466,'link':link600471,'range':range605897,'repeat':repeat606091,'filter':filter606259,'partition':partition603689,'any':any606644,'all':all606835,'find':find603712,'map':map607310,'map2':map2607421,'map3':map3607602,'map4':map4607850,'map_n':map_n608166,'map2_n':map2_n608324,'map3_n':map3_n608550,'map4_n':map4_n608844,'each':each609206,'each2':each2609345,'each3':each3609563,'each4':each4609857,'each_n':each_n610228,'each2_n':each2_n610424,'each3_n':each3_n610696,'each4_n':each4_n611045,'fold':fold602097,'fold2':fold2611585,'fold3':fold3611768,'fold4':fold4612019,'fold_n':fold_n612338,'index':index612667,'to-set':list_DASH_to_DASH_set602062});
var Location_DASH_mixins551200613146 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551201613157 = RUNTIME.makeObject({});
var variant551202613166 = data_DASH_shared551201613157.extend('_equals', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_location613169, [other600662]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(self600661, "file"),RUNTIME.getField(other600662, "file")]); 
})(); }, RUNTIME.makeString(""))]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(self600661, "line"),RUNTIME.getField(other600662, "line")]); 
})(); }, RUNTIME.makeString(""))]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(self600661, "column"),RUNTIME.getField(other600662, "column")]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString(""))).extend('format', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "file"), "tostring"), []), "_plus"), [RUNTIME.makeString(": line ")]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "line"), "tostring"), []), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", column "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "column"), "tostring"), [])])])]); 
})(); }, RUNTIME.makeString(""))).extend('tostring', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self600661, "format"), []); 
})(); }, RUNTIME.makeString("")));
var Location551343613420 = RUNTIME.applyFunc(brander600249, []);
var location_base551346613431 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("file"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("line"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("column"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_location613169,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("file"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("line"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("column"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("location")]))) { return (function(){
 var call_DASH_location551344613575 = RUNTIME.getField(cases_DASH_funs604869, "location");
return RUNTIME.applyFunc(call_DASH_location551344613575, [RUNTIME.getField(self600661, "file"),RUNTIME.getField(self600661, "line"),RUNTIME.getField(self600661, "column")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var location551345613644 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_location613169 = RUNTIME.getField(location551345613644, "test");
var location613665 = RUNTIME.makeFunction(function (file551347551930613666,line551348551931613667,column551349551932613668) { return (function(){
 var file551347613672 = file551347551930613666;
var line551348613679 = line551348551931613667;
var column551349613686 = column551349551932613668;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Location551343613420, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(location551345613644, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551933613701) { return (function(){
 var self600661 = self551933613701;
return (function(){
 var mixin551203613709 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Location_DASH_mixins551200613146]))) { return RUNTIME.applyFunc(Location_DASH_mixins551200613146, []); } else { return Location_DASH_mixins551200613146; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551203613709, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551203613709, "extend"), [self600661.extend('_equals', RUNTIME.getRawField(variant551202613166, '_equals')).extend('format', RUNTIME.getRawField(variant551202613166, 'format')).extend('tostring', RUNTIME.getRawField(variant551202613166, 'tostring'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for location")), [location_base551346613431.extend('file', file551347613672).extend('line', line551348613679).extend('column', column551349613686)])])]); 
})(); 
})(); }, RUNTIME.makeString("location: Creates an instance of location"));
var Location613838 = RUNTIME.getField(Location551343613420, "test");
var Error_DASH_mixins551204613849 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551205613860 = RUNTIME.makeObject({'tostring':RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(self600661, "format"), []); 
})(); }, RUNTIME.makeString("")),'format':RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "location"), "format"), []), "append"), [RUNTIME.makeString(":\n")]), "append"), [RUNTIME.applyFunc(RUNTIME.getField(self600661, "name"), [])]), "append"), [RUNTIME.makeString(": ")]), "append"), [RUNTIME.getField(self600661, "message")]); 
})(); }, RUNTIME.makeString(""))});
var variant551206613973 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Error using opaque internal value"); 
})(); }, RUNTIME.makeString("")));
var variant551207613997 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Field not found"); 
})(); }, RUNTIME.makeString("")));
var variant551208614021 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Non-string in field name"); 
})(); }, RUNTIME.makeString("")));
var variant551209614045 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("No cases matched"); 
})(); }, RUNTIME.makeString("")));
var variant551210614069 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Invalid case"); 
})(); }, RUNTIME.makeString("")));
var variant551211614093 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Eval Error"); 
})(); }, RUNTIME.makeString("")));
var variant551212614117 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Contract failure"); 
})(); }, RUNTIME.makeString("")));
var variant551213614141 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Arity mismatch"); 
})(); }, RUNTIME.makeString("")));
var variant551214614165 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Division by zero"); 
})(); }, RUNTIME.makeString("")));
var variant551215614189 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Type Error"); 
})(); }, RUNTIME.makeString("")));
var variant551216614213 = data_DASH_shared551205613860.extend('name', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.makeString("Error"); 
})(); }, RUNTIME.makeString("")));
var Error551350614237 = RUNTIME.applyFunc(brander600249, []);
var opaque_DASH_error_base551353614248 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("opaque-error"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_opaque_DASH_error614319,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("opaque-error")]))) { return (function(){
 var call_DASH_opaque_DASH_error551351614393 = RUNTIME.getField(cases_DASH_funs604869, "opaque-error");
return RUNTIME.applyFunc(call_DASH_opaque_DASH_error551351614393, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var opaque_DASH_error551352614462 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_opaque_DASH_error614319 = RUNTIME.getField(opaque_DASH_error551352614462, "test");
var opaque_DASH_error614483 = RUNTIME.makeFunction(function (message551354551934614484,location551355551935614485,trace551356551936614486) { return (function(){
 var message551354614490 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551938614491) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551938614491,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551354551934614484]);
var location551355614523 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551939614524) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551939614524,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551355551935614485]);
var trace551356614556 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551940614557) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551940614557,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551356551936614486]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(opaque_DASH_error551352614462, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551937614597) { return (function(){
 var self600661 = self551937614597;
return (function(){
 var mixin551217614605 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551217614605, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551217614605, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551206613973, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for opaque-error")), [opaque_DASH_error_base551353614248.extend('message', message551354614490).extend('location', location551355614523).extend('trace', trace551356614556)])])]); 
})(); 
})(); }, RUNTIME.makeString("opaque-error: Creates an instance of opaque-error"));
var field_DASH_not_DASH_found_base551359614734 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("field-not-found"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_field_DASH_not_DASH_found614805,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("field-not-found")]))) { return (function(){
 var call_DASH_field_DASH_not_DASH_found551357614879 = RUNTIME.getField(cases_DASH_funs604869, "field-not-found");
return RUNTIME.applyFunc(call_DASH_field_DASH_not_DASH_found551357614879, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var field_DASH_not_DASH_found551358614948 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_field_DASH_not_DASH_found614805 = RUNTIME.getField(field_DASH_not_DASH_found551358614948, "test");
var field_DASH_not_DASH_found614969 = RUNTIME.makeFunction(function (message551360551941614970,location551361551942614971,trace551362551943614972) { return (function(){
 var message551360614976 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551945614977) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551945614977,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551360551941614970]);
var location551361615009 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551946615010) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551946615010,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551361551942614971]);
var trace551362615042 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551947615043) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551947615043,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551362551943614972]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(field_DASH_not_DASH_found551358614948, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551944615083) { return (function(){
 var self600661 = self551944615083;
return (function(){
 var mixin551218615091 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551218615091, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551218615091, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551207613997, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for field-not-found")), [field_DASH_not_DASH_found_base551359614734.extend('message', message551360614976).extend('location', location551361615009).extend('trace', trace551362615042)])])]); 
})(); 
})(); }, RUNTIME.makeString("field-not-found: Creates an instance of field-not-found"));
var field_DASH_non_DASH_string_base551365615220 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("field-non-string"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_field_DASH_non_DASH_string615291,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("field-non-string")]))) { return (function(){
 var call_DASH_field_DASH_non_DASH_string551363615365 = RUNTIME.getField(cases_DASH_funs604869, "field-non-string");
return RUNTIME.applyFunc(call_DASH_field_DASH_non_DASH_string551363615365, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var field_DASH_non_DASH_string551364615434 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_field_DASH_non_DASH_string615291 = RUNTIME.getField(field_DASH_non_DASH_string551364615434, "test");
var field_DASH_non_DASH_string615455 = RUNTIME.makeFunction(function (message551366551948615456,location551367551949615457,trace551368551950615458) { return (function(){
 var message551366615462 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551952615463) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551952615463,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551366551948615456]);
var location551367615495 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551953615496) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551953615496,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551367551949615457]);
var trace551368615528 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551954615529) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551954615529,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551368551950615458]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(field_DASH_non_DASH_string551364615434, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551951615569) { return (function(){
 var self600661 = self551951615569;
return (function(){
 var mixin551219615577 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551219615577, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551219615577, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551208614021, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for field-non-string")), [field_DASH_non_DASH_string_base551365615220.extend('message', message551366615462).extend('location', location551367615495).extend('trace', trace551368615528)])])]); 
})(); 
})(); }, RUNTIME.makeString("field-non-string: Creates an instance of field-non-string"));
var cases_DASH_miss_base551371615706 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("cases-miss"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_cases_DASH_miss615777,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("cases-miss")]))) { return (function(){
 var call_DASH_cases_DASH_miss551369615851 = RUNTIME.getField(cases_DASH_funs604869, "cases-miss");
return RUNTIME.applyFunc(call_DASH_cases_DASH_miss551369615851, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var cases_DASH_miss551370615920 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_cases_DASH_miss615777 = RUNTIME.getField(cases_DASH_miss551370615920, "test");
var cases_DASH_miss615941 = RUNTIME.makeFunction(function (message551372551955615942,location551373551956615943,trace551374551957615944) { return (function(){
 var message551372615948 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551959615949) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551959615949,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551372551955615942]);
var location551373615981 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551960615982) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551960615982,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551373551956615943]);
var trace551374616014 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551961616015) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551961616015,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551374551957615944]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_miss551370615920, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551958616055) { return (function(){
 var self600661 = self551958616055;
return (function(){
 var mixin551220616063 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551220616063, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551220616063, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551209614045, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for cases-miss")), [cases_DASH_miss_base551371615706.extend('message', message551372615948).extend('location', location551373615981).extend('trace', trace551374616014)])])]); 
})(); 
})(); }, RUNTIME.makeString("cases-miss: Creates an instance of cases-miss"));
var invalid_DASH_case_base551377616192 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("invalid-case"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_invalid_DASH_case616263,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("invalid-case")]))) { return (function(){
 var call_DASH_invalid_DASH_case551375616337 = RUNTIME.getField(cases_DASH_funs604869, "invalid-case");
return RUNTIME.applyFunc(call_DASH_invalid_DASH_case551375616337, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var invalid_DASH_case551376616406 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_invalid_DASH_case616263 = RUNTIME.getField(invalid_DASH_case551376616406, "test");
var invalid_DASH_case616427 = RUNTIME.makeFunction(function (message551378551962616428,location551379551963616429,trace551380551964616430) { return (function(){
 var message551378616434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551966616435) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551966616435,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551378551962616428]);
var location551379616467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551967616468) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551967616468,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551379551963616429]);
var trace551380616500 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551968616501) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551968616501,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551380551964616430]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(invalid_DASH_case551376616406, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551965616541) { return (function(){
 var self600661 = self551965616541;
return (function(){
 var mixin551221616549 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551221616549, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551221616549, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551210614069, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for invalid-case")), [invalid_DASH_case_base551377616192.extend('message', message551378616434).extend('location', location551379616467).extend('trace', trace551380616500)])])]); 
})(); 
})(); }, RUNTIME.makeString("invalid-case: Creates an instance of invalid-case"));
var eval_DASH_error_base551383616678 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("eval-error"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_eval_DASH_error616749,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("eval-error")]))) { return (function(){
 var call_DASH_eval_DASH_error551381616823 = RUNTIME.getField(cases_DASH_funs604869, "eval-error");
return RUNTIME.applyFunc(call_DASH_eval_DASH_error551381616823, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var eval_DASH_error551382616892 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_eval_DASH_error616749 = RUNTIME.getField(eval_DASH_error551382616892, "test");
var eval_DASH_error616913 = RUNTIME.makeFunction(function (message551384551969616914,location551385551970616915,trace551386551971616916) { return (function(){
 var message551384616920 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551973616921) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551973616921,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551384551969616914]);
var location551385616953 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551974616954) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551974616954,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551385551970616915]);
var trace551386616986 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551975616987) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551975616987,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551386551971616916]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(eval_DASH_error551382616892, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551972617027) { return (function(){
 var self600661 = self551972617027;
return (function(){
 var mixin551222617035 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551222617035, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551222617035, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551211614093, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for eval-error")), [eval_DASH_error_base551383616678.extend('message', message551384616920).extend('location', location551385616953).extend('trace', trace551386616986)])])]); 
})(); 
})(); }, RUNTIME.makeString("eval-error: Creates an instance of eval-error"));
var user_DASH_contract_DASH_failure_base551389617164 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("user-contract-failure"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_user_DASH_contract_DASH_failure617235,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("user-contract-failure")]))) { return (function(){
 var call_DASH_user_DASH_contract_DASH_failure551387617309 = RUNTIME.getField(cases_DASH_funs604869, "user-contract-failure");
return RUNTIME.applyFunc(call_DASH_user_DASH_contract_DASH_failure551387617309, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var user_DASH_contract_DASH_failure551388617378 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_user_DASH_contract_DASH_failure617235 = RUNTIME.getField(user_DASH_contract_DASH_failure551388617378, "test");
var user_DASH_contract_DASH_failure617399 = RUNTIME.makeFunction(function (message551390551976617400,location551391551977617401,trace551392551978617402) { return (function(){
 var message551390617406 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551980617407) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551980617407,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551390551976617400]);
var location551391617439 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551981617440) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551981617440,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551391551977617401]);
var trace551392617472 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551982617473) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551982617473,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551392551978617402]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(user_DASH_contract_DASH_failure551388617378, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551979617513) { return (function(){
 var self600661 = self551979617513;
return (function(){
 var mixin551223617521 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551223617521, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551223617521, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551212614117, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for user-contract-failure")), [user_DASH_contract_DASH_failure_base551389617164.extend('message', message551390617406).extend('location', location551391617439).extend('trace', trace551392617472)])])]); 
})(); 
})(); }, RUNTIME.makeString("user-contract-failure: Creates an instance of user-contract-failure"));
var arity_DASH_error_base551395617650 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("arity-error"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_arity_DASH_error617721,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("arity-error")]))) { return (function(){
 var call_DASH_arity_DASH_error551393617795 = RUNTIME.getField(cases_DASH_funs604869, "arity-error");
return RUNTIME.applyFunc(call_DASH_arity_DASH_error551393617795, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var arity_DASH_error551394617864 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_arity_DASH_error617721 = RUNTIME.getField(arity_DASH_error551394617864, "test");
var arity_DASH_error617885 = RUNTIME.makeFunction(function (message551396551983617886,location551397551984617887,trace551398551985617888) { return (function(){
 var message551396617892 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551987617893) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551987617893,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551396551983617886]);
var location551397617925 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551988617926) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551988617926,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551397551984617887]);
var trace551398617958 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551989617959) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551989617959,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551398551985617888]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(arity_DASH_error551394617864, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551986617999) { return (function(){
 var self600661 = self551986617999;
return (function(){
 var mixin551224618007 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551224618007, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551224618007, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551213614141, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for arity-error")), [arity_DASH_error_base551395617650.extend('message', message551396617892).extend('location', location551397617925).extend('trace', trace551398617958)])])]); 
})(); 
})(); }, RUNTIME.makeString("arity-error: Creates an instance of arity-error"));
var div_DASH_0_base551401618136 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("div-0"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_div_DASH_0618207,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("div-0")]))) { return (function(){
 var call_DASH_div_DASH_0551399618281 = RUNTIME.getField(cases_DASH_funs604869, "div-0");
return RUNTIME.applyFunc(call_DASH_div_DASH_0551399618281, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var div_DASH_0551400618350 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_div_DASH_0618207 = RUNTIME.getField(div_DASH_0551400618350, "test");
var div_DASH_0618371 = RUNTIME.makeFunction(function (message551402551990618372,location551403551991618373,trace551404551992618374) { return (function(){
 var message551402618378 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551994618379) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen551994618379,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551402551990618372]);
var location551403618411 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551995618412) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen551995618412,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551403551991618373]);
var trace551404618444 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen551996618445) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen551996618445,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551404551992618374]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(div_DASH_0551400618350, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self551993618485) { return (function(){
 var self600661 = self551993618485;
return (function(){
 var mixin551225618493 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551225618493, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551225618493, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551214614165, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for div-0")), [div_DASH_0_base551401618136.extend('message', message551402618378).extend('location', location551403618411).extend('trace', trace551404618444)])])]); 
})(); 
})(); }, RUNTIME.makeString("div-0: Creates an instance of div-0"));
var type_DASH_error_base551407618622 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("type-error"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_type_DASH_error618693,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("type-error")]))) { return (function(){
 var call_DASH_type_DASH_error551405618767 = RUNTIME.getField(cases_DASH_funs604869, "type-error");
return RUNTIME.applyFunc(call_DASH_type_DASH_error551405618767, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var type_DASH_error551406618836 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_type_DASH_error618693 = RUNTIME.getField(type_DASH_error551406618836, "test");
var type_DASH_error618857 = RUNTIME.makeFunction(function (message551408551997618858,location551409551998618859,trace551410551999618860) { return (function(){
 var message551408618864 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552001618865) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552001618865,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551408551997618858]);
var location551409618897 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552002618898) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen552002618898,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551409551998618859]);
var trace551410618930 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552003618931) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552003618931,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551410551999618860]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(type_DASH_error551406618836, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552000618971) { return (function(){
 var self600661 = self552000618971;
return (function(){
 var mixin551226618979 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551226618979, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551226618979, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551215614189, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for type-error")), [type_DASH_error_base551407618622.extend('message', message551408618864).extend('location', location551409618897).extend('trace', trace551410618930)])])]); 
})(); 
})(); }, RUNTIME.makeString("type-error: Creates an instance of type-error"));
var lazy_DASH_error_base551413619108 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("lazy-error"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_lazy_DASH_error619179,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("message"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("trace"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("lazy-error")]))) { return (function(){
 var call_DASH_lazy_DASH_error551411619253 = RUNTIME.getField(cases_DASH_funs604869, "lazy-error");
return RUNTIME.applyFunc(call_DASH_lazy_DASH_error551411619253, [RUNTIME.getField(self600661, "message"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "trace")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var lazy_DASH_error551412619322 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_lazy_DASH_error619179 = RUNTIME.getField(lazy_DASH_error551412619322, "test");
var lazy_DASH_error619343 = RUNTIME.makeFunction(function (message551414552004619344,location551415552005619345,trace551416552006619346) { return (function(){
 var message551414619350 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552008619351) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552008619351,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [message551414552004619344]);
var location551415619383 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552009619384) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen552009619384,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551415552005619345]);
var trace551416619416 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552010619417) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552010619417,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [trace551416552006619346]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Error551350614237, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(lazy_DASH_error551412619322, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552007619457) { return (function(){
 var self600661 = self552007619457;
return (function(){
 var mixin551227619465 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Error_DASH_mixins551204613849]))) { return RUNTIME.applyFunc(Error_DASH_mixins551204613849, []); } else { return Error_DASH_mixins551204613849; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551227619465, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551227619465, "extend"), [self600661.extend('tostring', RUNTIME.getRawField(data_DASH_shared551205613860, 'tostring')).extend('format', RUNTIME.getRawField(data_DASH_shared551205613860, 'format')).extend('name', RUNTIME.getRawField(variant551216614213, 'name'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for lazy-error")), [lazy_DASH_error_base551413619108.extend('message', message551414619350).extend('location', location551415619383).extend('trace', trace551416619416)])])]); 
})(); 
})(); }, RUNTIME.makeString("lazy-error: Creates an instance of lazy-error"));
var Error619594 = RUNTIME.getField(Error551350614237, "test");
var make_DASH_error619605 = RUNTIME.makeFunction(function (obj552011619606) { return (function(){
 var obj600455 = obj552011619606;
return (function(){
 var trace619614 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field600323, [obj600455,RUNTIME.makeString("trace")]))) { return (function(){
 return RUNTIME.applyFunc(map607310, [RUNTIME.makeFunction(function (l552012619623) { return (function(){
 var l600892 = l552012619623;
return (function(){
 return RUNTIME.applyFunc(location613665, [RUNTIME.getField(l600892, "path"),RUNTIME.getField(l600892, "line"),RUNTIME.getField(l600892, "column")]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(mklist600452, [RUNTIME.getField(obj600455, "trace")])]); 
})(); } else { return (function(){
 return RUNTIME.getField(list601511, "empty"); 
})(); } })();
var loc619707 = RUNTIME.applyFunc(location613665, [RUNTIME.getField(obj600455, "path"),RUNTIME.getField(obj600455, "line"),RUNTIME.getField(obj600455, "column")]);
return (function() { if (RUNTIME.isTrue(RUNTIME.getField(obj600455, "system"))) { return (function(){
 var type619737 = RUNTIME.getField(RUNTIME.getField(obj600455, "value"), "type");
var msg619752 = RUNTIME.getField(RUNTIME.getField(obj600455, "value"), "message");
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("opaque")]))) { return (function(){
 return RUNTIME.applyFunc(opaque_DASH_error614483, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("field-not-found")]))) { return (function(){
 return RUNTIME.applyFunc(field_DASH_not_DASH_found614969, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("field-non-string")]))) { return (function(){
 return RUNTIME.applyFunc(field_DASH_non_DASH_string615455, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("user-contract-failure")]))) { return (function(){
 return RUNTIME.applyFunc(user_DASH_contract_DASH_failure617399, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("eval-error")]))) { return (function(){
 return RUNTIME.applyFunc(eval_DASH_error616913, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("arity-mismatch")]))) { return (function(){
 return RUNTIME.applyFunc(arity_DASH_error617885, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("div-0")]))) { return (function(){
 return RUNTIME.applyFunc(div_DASH_0618371, [msg619752,loc619707,trace619614]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [type619737,RUNTIME.makeString("type-error")]))) { return (function(){
 return RUNTIME.applyFunc(type_DASH_error618857, [msg619752,loc619707,trace619614]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(lazy_DASH_error619343, [msg619752,loc619707,trace619614]); 
})(); } })(); 
})(); } else { return (function(){
 return RUNTIME.getField(obj600455, "value"); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var error601487 = RUNTIME.makeObject({'opaque-error':opaque_DASH_error614483,'is-opaque-error':is_DASH_opaque_DASH_error614319,'field-not-found':field_DASH_not_DASH_found614969,'is-field-not-found':is_DASH_field_DASH_not_DASH_found614805,'cases-miss':cases_DASH_miss615941,'is-cases-miss':is_DASH_cases_DASH_miss615777,'invalid-case':invalid_DASH_case616427,'is-invalid-case':is_DASH_invalid_DASH_case616263,'user-contract-failure':user_DASH_contract_DASH_failure617399,'is-user-contract-failure':is_DASH_user_DASH_contract_DASH_failure617235,'div-0':div_DASH_0618371,'is-div-0':is_DASH_div_DASH_0618207,'make-error':make_DASH_error619605,'Error':Error619594,'Location':Location613838,'location':location613665,'is-location':is_DASH_location613169});
var Set_DASH_mixins551228620135 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551229620146 = RUNTIME.makeObject({});
var variant551230620155 = data_DASH_shared551229620146.extend('member', RUNTIME.makeMethod(function (self600661,elem602109) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "elems"), "member"), [elem602109]); 
})(); }, RUNTIME.makeString("Check to see if an element is in a set."))).extend('add', RUNTIME.makeMethod(function (self600661,elem602109) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "elems"), "member"), [elem602109]))) { return (function(){
 return self600661; 
})(); } else { return (function(){
 return RUNTIME.applyFunc(__set602139, [RUNTIME.applyFunc(link600471, [elem602109,RUNTIME.getField(self600661, "elems")])]); 
})(); } })(); 
})(); }, RUNTIME.makeString("Add an element to the set if it is not already present."))).extend('remove', RUNTIME.makeMethod(function (self600661,elem602109) { return (function(){
 return RUNTIME.applyFunc(__set602139, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "elems"), "filter"), [RUNTIME.makeFunction(function (x552013620257) { return (function(){
 var x620259 = x552013620257;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [x620259,elem602109]), "_not"), []); 
})(); 
})(); }, RUNTIME.makeString(""))])]); 
})(); }, RUNTIME.makeString("Remove an element from the set if it is present."))).extend('to-list', RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "elems"), "sort"), []); 
})(); }, RUNTIME.makeString("Convert a set into a sorted list of elements."))).extend('union', RUNTIME.applyFunc(RUNTIME.makeFunction(function (contract552014620349) { return (function(){
 return (function(){
 RUNTIME.applyFunc(check_DASH_brand600219, [Method600284,contract552014620349,RUNTIME.makeString("(Any, Set => Any)")]);
var fun552015620362 = RUNTIME.applyFunc(RUNTIME.getField(contract552014620349, "_fun"), []);
return RUNTIME.makeMethod(function (arg552016620377,arg552017620378) { return (function(){
 return RUNTIME.applyFunc(fun552015620362, [arg552016620377,RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552018620381) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Set620383,specimen552018620381,RUNTIME.makeString("Set")]); 
})(); }, RUNTIME.makeString("internal contract for Set")), [arg552017620378])]); 
})(); }, RUNTIME.makeString("internal contract for (Any, Set => Any)")).extend('_doc', RUNTIME.getField(contract552014620349, "_doc")); 
})(); 
})(); }, RUNTIME.makeString("internal contract for (Any, Set => Any)")), [RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(list_DASH_to_DASH_set602062, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(self600661, "to-list"), []), "append"), [RUNTIME.applyFunc(RUNTIME.getField(other600662, "to-list"), [])])]); 
})(); }, RUNTIME.makeString("Take the union of two sets."))])).extend('_equals', RUNTIME.makeMethod(function (self600661,other600662) { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(Set620383, [other600662]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(self600661, "elems"), "sort"), []),RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(other600662, "elems"), "sort"), [])]); 
})(); }, RUNTIME.makeString(""))]); 
})(); }, RUNTIME.makeString("")));
var Set551417620577 = RUNTIME.applyFunc(brander600249, []);
var __set_base551420620588 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("__set"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("elems"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH___set620635,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("elems"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("__set")]))) { return (function(){
 var call_DASH___set551418620685 = RUNTIME.getField(cases_DASH_funs604869, "__set");
return RUNTIME.applyFunc(call_DASH___set551418620685, [RUNTIME.getField(self600661, "elems")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var __set551419620744 = RUNTIME.applyFunc(brander600249, []);
var is_DASH___set620635 = RUNTIME.getField(__set551419620744, "test");
var __set602139 = RUNTIME.makeFunction(function (elems551421552019620765) { return (function(){
 var elems551421620767 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552021620768) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552021620768,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [elems551421552019620765]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Set551417620577, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(__set551419620744, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552020620808) { return (function(){
 var self600661 = self552020620808;
return (function(){
 var mixin551231620816 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Set_DASH_mixins551228620135]))) { return RUNTIME.applyFunc(Set_DASH_mixins551228620135, []); } else { return Set_DASH_mixins551228620135; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551231620816, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551231620816, "extend"), [self600661.extend('member', RUNTIME.getRawField(variant551230620155, 'member')).extend('add', RUNTIME.getRawField(variant551230620155, 'add')).extend('remove', RUNTIME.getRawField(variant551230620155, 'remove')).extend('to-list', RUNTIME.getRawField(variant551230620155, 'to-list')).extend('union', RUNTIME.getRawField(variant551230620155, 'union')).extend('_equals', RUNTIME.getRawField(variant551230620155, '_equals'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for __set")), [__set_base551420620588.extend('elems', elems551421620767)])])]); 
})(); 
})(); }, RUNTIME.makeString("__set: Creates an instance of __set"));
var Set620383 = RUNTIME.getField(Set551417620577, "test");
var sets620971 = RUNTIME.makeObject({'Set':Set620383,'set':list_DASH_to_DASH_set602062});
var Option_DASH_mixins551232620990 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551233621001 = RUNTIME.makeObject({});
var variant551234621010 = data_DASH_shared551233621001.extend('orelse', RUNTIME.makeMethod(function (self600661,v601137) { return (function(){
 return v601137; 
})(); }, RUNTIME.makeString("Return the default provided value"))).extend('andthen', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return self600661; 
})(); }, RUNTIME.makeString("")));
var variant551235621049 = data_DASH_shared551233621001.extend('orelse', RUNTIME.makeMethod(function (self600661,v601137) { return (function(){
 return RUNTIME.getField(self600661, "value"); 
})(); }, RUNTIME.makeString("Return self.value, rather than the default"))).extend('andthen', RUNTIME.makeMethod(function (self600661,f602670) { return (function(){
 return RUNTIME.applyFunc(f602670, [RUNTIME.getField(self600661, "value")]); 
})(); }, RUNTIME.makeString("")));
var Option551422621101 = RUNTIME.applyFunc(brander600249, []);
var none_base551425621112 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("none"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_none606989,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("none")]))) { return (function(){
 var call_DASH_none551424621169 = RUNTIME.getField(cases_DASH_funs604869, "none");
return RUNTIME.applyFunc(call_DASH_none551424621169, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var none551423621223 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_none606989 = RUNTIME.getField(none551423621223, "test");
var none603126 = RUNTIME.applyFunc(RUNTIME.getField(Option551422621101, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(none551423621223, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552022621252) { return (function(){
 var self600661 = self552022621252;
return (function(){
 var mixin551236621260 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Option_DASH_mixins551232620990]))) { return RUNTIME.applyFunc(Option_DASH_mixins551232620990, []); } else { return Option_DASH_mixins551232620990; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551236621260, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551236621260, "extend"), [self600661.extend('orelse', RUNTIME.getRawField(variant551234621010, 'orelse')).extend('andthen', RUNTIME.getRawField(variant551234621010, 'andthen'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for none")), [none_base551425621112])])]);
var some_base551428621355 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("some"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_some606798,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("some")]))) { return (function(){
 var call_DASH_some551426621451 = RUNTIME.getField(cases_DASH_funs604869, "some");
return RUNTIME.applyFunc(call_DASH_some551426621451, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var some551427621510 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_some606798 = RUNTIME.getField(some551427621510, "test");
var some607233 = RUNTIME.makeFunction(function (value551429552023621531) { return (function(){
 var value551429621533 = value551429552023621531;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Option551422621101, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(some551427621510, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552024621548) { return (function(){
 var self600661 = self552024621548;
return (function(){
 var mixin551237621556 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Option_DASH_mixins551232620990]))) { return RUNTIME.applyFunc(Option_DASH_mixins551232620990, []); } else { return Option_DASH_mixins551232620990; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551237621556, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551237621556, "extend"), [self600661.extend('orelse', RUNTIME.getRawField(variant551235621049, 'orelse')).extend('andthen', RUNTIME.getRawField(variant551235621049, 'andthen'))])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for some")), [some_base551428621355.extend('value', value551429621533)])])]); 
})(); 
})(); }, RUNTIME.makeString("some: Creates an instance of some"));
var Option607195 = RUNTIME.getField(Option551422621101, "test");
var option621679 = RUNTIME.makeObject({'Option':Option607195,'is-none':is_DASH_none606989,'is-some':is_DASH_some606798,'none':none603126,'some':some607233});
var CheckResult_DASH_mixins551238621713 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551239621724 = RUNTIME.makeObject({});
var variant551240621733 = data_DASH_shared551239621724;
var variant551241621740 = data_DASH_shared551239621724;
var variant551242621747 = data_DASH_shared551239621724;
var CheckResult551430621754 = RUNTIME.applyFunc(brander600249, []);
var success_base551433621765 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("success"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_success621824,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("success")]))) { return (function(){
 var call_DASH_success551431621886 = RUNTIME.getField(cases_DASH_funs604869, "success");
return RUNTIME.applyFunc(call_DASH_success551431621886, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "location")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var success551432621950 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_success621824 = RUNTIME.getField(success551432621950, "test");
var success621971 = RUNTIME.makeFunction(function (name551434552025621972,location551435552026621973) { return (function(){
 var name551434621976 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552028621977) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552028621977,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551434552025621972]);
var location551435622009 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552029622010) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Option607195,specimen552029622010,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [location551435552026621973]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResult551430621754, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(success551432621950, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552027622050) { return (function(){
 var self600661 = self552027622050;
return (function(){
 var mixin551243622058 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [CheckResult_DASH_mixins551238621713]))) { return RUNTIME.applyFunc(CheckResult_DASH_mixins551238621713, []); } else { return CheckResult_DASH_mixins551238621713; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551243622058, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551243622058, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for success")), [success_base551433621765.extend('name', name551434621976).extend('location', location551435622009)])])]); 
})(); 
})(); }, RUNTIME.makeString("success: Creates an instance of success"));
var failure_base551438622159 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("failure"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("reason"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_failure622230,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("reason"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("failure")]))) { return (function(){
 var call_DASH_failure551436622304 = RUNTIME.getField(cases_DASH_funs604869, "failure");
return RUNTIME.applyFunc(call_DASH_failure551436622304, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "reason"),RUNTIME.getField(self600661, "location")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var failure551437622373 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_failure622230 = RUNTIME.getField(failure551437622373, "test");
var failure622394 = RUNTIME.makeFunction(function (name551439552030622395,reason551440552031622396,location551441552032622397) { return (function(){
 var name551439622401 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552034622402) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552034622402,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551439552030622395]);
var reason551440622434 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552035622435) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552035622435,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [reason551440552031622396]);
var location551441622467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552036622468) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Option607195,specimen552036622468,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [location551441552032622397]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResult551430621754, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(failure551437622373, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552033622508) { return (function(){
 var self600661 = self552033622508;
return (function(){
 var mixin551244622516 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [CheckResult_DASH_mixins551238621713]))) { return RUNTIME.applyFunc(CheckResult_DASH_mixins551238621713, []); } else { return CheckResult_DASH_mixins551238621713; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551244622516, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551244622516, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for failure")), [failure_base551438622159.extend('name', name551439622401).extend('reason', reason551440622434).extend('location', location551441622467)])])]); 
})(); 
})(); }, RUNTIME.makeString("failure: Creates an instance of failure"));
var err_base551444622621 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("err"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("exception"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_err622692,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("exception"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("err")]))) { return (function(){
 var call_DASH_err551442622766 = RUNTIME.getField(cases_DASH_funs604869, "err");
return RUNTIME.applyFunc(call_DASH_err551442622766, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "exception"),RUNTIME.getField(self600661, "location")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var err551443622835 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_err622692 = RUNTIME.getField(err551443622835, "test");
var err622856 = RUNTIME.makeFunction(function (name551445552037622857,exception551446552038622858,location551447552039622859) { return (function(){
 var name551445622863 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552041622864) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552041622864,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551445552037622857]);
var exception551446622896 = exception551446552038622858;
var location551447622903 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552042622904) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Option607195,specimen552042622904,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [location551447552039622859]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResult551430621754, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(err551443622835, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552040622944) { return (function(){
 var self600661 = self552040622944;
return (function(){
 var mixin551245622952 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [CheckResult_DASH_mixins551238621713]))) { return RUNTIME.applyFunc(CheckResult_DASH_mixins551238621713, []); } else { return CheckResult_DASH_mixins551238621713; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551245622952, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551245622952, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for err")), [err_base551444622621.extend('name', name551445622863).extend('exception', exception551446622896).extend('location', location551447622903)])])]); 
})(); 
})(); }, RUNTIME.makeString("err: Creates an instance of err"));
var CheckResult623057 = RUNTIME.getField(CheckResult551430621754, "test");
var current_DASH_results623068 = empty600466;
var add_DASH_result623075 = RUNTIME.makeFunction(function (result552043623076) { return (function(){
 var result623078 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552044623079) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [CheckResult623057,specimen552044623079,RUNTIME.makeString("CheckResult")]); 
})(); }, RUNTIME.makeString("internal contract for CheckResult")), [result552043623076]);
return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [result623078,RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_is623158 = RUNTIME.makeFunction(function (name552045623159,thunk1552046623160,thunk2552047623161,loc552048623162) { return (function(){
 var name600574 = name552045623159;
var thunk1623173 = thunk1552046623160;
var thunk2623180 = thunk2552047623161;
var loc619707 = loc552048623162;
return (function(){
 return (function() { try { return (function(){
 var val1623193 = RUNTIME.applyFunc(thunk1623173, []);
return (function() { try { return (function(){
 var val2623204 = RUNTIME.applyFunc(thunk2623180, []);
return (function() { try { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [val1623193,val2623204]))) { return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(success621971, [name600574,RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Values not equal: \n"), "_plus"), [RUNTIME.applyFunc(torepr600234, [val1623193])]), "_plus"), [RUNTIME.makeString("\n\n")]), "_plus"), [RUNTIME.applyFunc(torepr600234, [val2623204])]),RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); } })(); 
})(); } catch (g551248623321) { g551248623321 = RUNTIME.unwrapException(g551248623321); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552049623322) { return (function(){
 var e603346 = e552049623322;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(err622856, [name600574,e603346,RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551248623321])]); 
})(); } })(); 
})(); } catch (g551247623395) { g551247623395 = RUNTIME.unwrapException(g551247623395); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552050623396) { return (function(){
 var e603346 = e552050623396;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(err622856, [name600574,e603346,RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551247623395])]); 
})(); } })(); 
})(); } catch (g551246623469) { g551246623469 = RUNTIME.unwrapException(g551246623469); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552051623470) { return (function(){
 var e603346 = e552051623470;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(err622856, [name600574,e603346,RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551246623469])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_raises623559 = RUNTIME.makeFunction(function (name552052623560,thunk552053623561,expected_DASH_str552054623562,loc552055623563) { return (function(){
 var name600574 = name552052623560;
var thunk623574 = thunk552053623561;
var expected_DASH_str623581 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552058623582) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552058623582,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [expected_DASH_str552054623562]);
var loc619707 = loc552055623563;
return (function(){
 var bad_DASH_err623620 = RUNTIME.makeFunction(function (actual552056623621) { return (function(){
 var actual623623 = actual552056623621;
return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Wrong error message.  The test expected:\n"), "_plus"), [RUNTIME.applyFunc(torepr600234, [expected_DASH_str623581])]), "_plus"), [RUNTIME.makeString("\nBut this was actually raised:\n")]), "_plus"), [RUNTIME.applyFunc(torepr600234, [actual623623])]),RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); 
})(); }, RUNTIME.makeString(""));
return (function() { try { return (function(){
 var val1623193 = RUNTIME.applyFunc(thunk623574, []);
return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("No exception raised.  The test expected "), "_plus"), [RUNTIME.applyFunc(torepr600234, [expected_DASH_str623581])]),RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); } catch (g551249623755) { g551249623755 = RUNTIME.unwrapException(g551249623755); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552057623756) { return (function(){
 var e603346 = e552057623756;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(String600274, [e603346]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(e603346, "contains"), [expected_DASH_str623581]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(success621971, [name600574,RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(Error619594, [e603346]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(e603346, "message"), "contains"), [expected_DASH_str623581]))) { return (function(){
 return RUNTIME.applyFunc(add_DASH_result623075, [RUNTIME.applyFunc(success621971, [name600574,RUNTIME.applyFunc(some607233, [loc619707])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(bad_DASH_err623620, [RUNTIME.getField(e603346, "message")]); 
})(); } })(); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(bad_DASH_err623620, [e603346]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551249623755])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Check that thunk raises either a string that contains expected-str,\n        or an exception with a message that contains expected-str"));
var check_DASH_true623969 = RUNTIME.makeFunction(function (name552059623970,val552060623971) { return (function(){
 var name600574 = name552059623970;
var val623980 = val552060623971;
return (function(){
 return RUNTIME.applyFunc(check_DASH_equals623987, [name600574,val623980,RUNTIME.makeBool(true)]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_false624017 = RUNTIME.makeFunction(function (name552061624018,val552062624019) { return (function(){
 var name600574 = name552061624018;
var val623980 = val552062624019;
return (function(){
 return RUNTIME.applyFunc(check_DASH_equals623987, [name600574,val623980,RUNTIME.makeBool(false)]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_equals623987 = RUNTIME.makeFunction(function (name552063624063,val1552064624064,val2552065624065) { return (function(){
 var name600574 = name552063624063;
var val1623193 = val1552064624064;
var val2623204 = val2552065624065;
return (function(){
 return (function() { try { return (function(){
 var values_equal624087 = RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [val1623193,val2623204]);
(function() { if (RUNTIME.isTrue(values_equal624087)) { return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "push"), [RUNTIME.applyFunc(success621971, [name600574,none603126])]); 
})(); } else { return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Values not equal: \n"), "_plus"), [RUNTIME.applyFunc(torepr600234, [val1623193])]), "_plus"), [RUNTIME.makeString("\n\n")]), "_plus"), [RUNTIME.applyFunc(torepr600234, [val2623204])]),none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); } })();
return values_equal624087; 
})(); } catch (g551250624222) { g551250624222 = RUNTIME.unwrapException(g551250624222); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552066624223) { return (function(){
 var e603346 = e552066624223;
return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(err622856, [name600574,e603346,none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551250624222])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_pred624329 = RUNTIME.makeFunction(function (name552067624330,val1552068624331,pred552069624332) { return (function(){
 var name600574 = name552067624330;
var val1623193 = val1552068624331;
var pred624348 = pred552069624332;
return (function(){
 return (function() { try { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(pred624348, [val1623193]))) { return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(success621971, [name600574,none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); } else { return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Value didn't satisfy predicate: "), "_plus"), [RUNTIME.applyFunc(torepr600234, [val1623193])]), "_plus"), [RUNTIME.makeString(", ")]), "_plus"), [RUNTIME.getField(pred624348, "_doc")]),none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); } })(); 
})(); } catch (g551251624489) { g551251624489 = RUNTIME.unwrapException(g551251624489); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552070624490) { return (function(){
 var e603346 = e552070624490;
return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(err622856, [name600574,e603346,none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551251624489])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_exn624596 = RUNTIME.makeFunction(function (name552071624597,thunk552072624598,pred552073624599) { return (function(){
 var name600574 = name552071624597;
var thunk623574 = thunk552072624598;
var pred624348 = pred552073624599;
return (function(){
 return (function() { try { return (function(){
 RUNTIME.applyFunc(thunk623574, []);
return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Thunk didn't throw an exception: "), "_plus"), [RUNTIME.applyFunc(tostring600244, [thunk623574])]),none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); } catch (g551252624681) { g551252624681 = RUNTIME.unwrapException(g551252624681); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552074624682) { return (function(){
 var e603346 = e552074624682;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(pred624348, [e603346]))) { return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(success621971, [name600574,none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); } else { return (function(){
 return current_DASH_results623068 = RUNTIME.applyFunc(RUNTIME.getField(current_DASH_results623068, "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.applyFunc(failure622394, [name600574,RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Wrong exception thrown:"), "_plus"), [RUNTIME.applyFunc(tostring600244, [e603346])]),none603126]),RUNTIME.getField(list601511, "empty")])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551252624681])]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var CheckResultList_DASH_mixins551253624860 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551254624871 = RUNTIME.makeObject({});
var variant551255624880 = data_DASH_shared551254624871;
var variant551256624887 = data_DASH_shared551254624871;
var CheckResultList551448624894 = RUNTIME.applyFunc(brander600249, []);
var normal_DASH_result_base551451624905 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("normal-result"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("results"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_normal_DASH_result624976,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("results"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("normal-result")]))) { return (function(){
 var call_DASH_normal_DASH_result551449625050 = RUNTIME.getField(cases_DASH_funs604869, "normal-result");
return RUNTIME.applyFunc(call_DASH_normal_DASH_result551449625050, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "results")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var normal_DASH_result551450625119 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_normal_DASH_result624976 = RUNTIME.getField(normal_DASH_result551450625119, "test");
var normal_DASH_result625140 = RUNTIME.makeFunction(function (name551452552075625141,location551453552076625142,results551454552077625143) { return (function(){
 var name551452625147 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552079625148) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552079625148,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551452552075625141]);
var location551453625180 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552080625181) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen552080625181,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551453552076625142]);
var results551454625213 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552081625214) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552081625214,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [results551454552077625143]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResultList551448624894, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(normal_DASH_result551450625119, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552078625254) { return (function(){
 var self600661 = self552078625254;
return (function(){
 var mixin551257625262 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [CheckResultList_DASH_mixins551253624860]))) { return RUNTIME.applyFunc(CheckResultList_DASH_mixins551253624860, []); } else { return CheckResultList_DASH_mixins551253624860; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551257625262, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551257625262, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for normal-result")), [normal_DASH_result_base551451624905.extend('name', name551452625147).extend('location', location551453625180).extend('results', results551454625213)])])]); 
})(); 
})(); }, RUNTIME.makeString("normal-result: Creates an instance of normal-result"));
var error_DASH_result_base551457625367 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("error-result"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("results"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("err"),RUNTIME.getField(list601511, "empty")])])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_error_DASH_result625450,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("location"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("results"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("err"),RUNTIME.getField(list601511, "empty")])])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("error-result")]))) { return (function(){
 var call_DASH_error_DASH_result551455625536 = RUNTIME.getField(cases_DASH_funs604869, "error-result");
return RUNTIME.applyFunc(call_DASH_error_DASH_result551455625536, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "location"),RUNTIME.getField(self600661, "results"),RUNTIME.getField(self600661, "err")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var error_DASH_result551456625610 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_error_DASH_result625450 = RUNTIME.getField(error_DASH_result551456625610, "test");
var error_DASH_result625631 = RUNTIME.makeFunction(function (name551458552082625632,location551459552083625633,results551460552084625634,err551461552085625635) { return (function(){
 var name551458625640 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552087625641) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552087625641,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551458552082625632]);
var location551459625673 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552088625674) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Location613838,specimen552088625674,RUNTIME.makeString("Location")]); 
})(); }, RUNTIME.makeString("internal contract for Location")), [location551459552083625633]);
var results551460625706 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552089625707) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552089625707,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [results551460552084625634]);
var err551461625739 = err551461552085625635;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(CheckResultList551448624894, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(error_DASH_result551456625610, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552086625754) { return (function(){
 var self600661 = self552086625754;
return (function(){
 var mixin551258625762 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [CheckResultList_DASH_mixins551253624860]))) { return RUNTIME.applyFunc(CheckResultList_DASH_mixins551253624860, []); } else { return CheckResultList_DASH_mixins551253624860; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551258625762, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551258625762, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for error-result")), [error_DASH_result_base551457625367.extend('name', name551458625640).extend('location', location551459625673).extend('results', results551460625706).extend('err', err551461625739)])])]); 
})(); 
})(); }, RUNTIME.makeString("error-result: Creates an instance of error-result"));
var CheckResultList625871 = RUNTIME.getField(CheckResultList551448624894, "test");
var all_DASH_results625882 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552090625883) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552090625883,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [empty600466]);
var run_DASH_checks625915 = RUNTIME.makeFunction(function (checks552091625916) { return (function(){
 var checks625918 = checks552091625916;
return (function(){
 (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.applyFunc(RUNTIME.getField(checks625918, "length"), []),RUNTIME.makeNumber(0)]), "_not"), []))) { return (function(){
 var lst_DASH_to_DASH_structural625953 = RUNTIME.makeFunction(function (lst552092625954) { return (function(){
 var lst600847 = lst552092625954;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field600323, [lst600847,RUNTIME.makeString("first")]))) { return (function(){
 return RUNTIME.makeObject({'first':RUNTIME.getField(lst600847, "first"),'rest':RUNTIME.applyFunc(lst_DASH_to_DASH_structural625953, [RUNTIME.getField(lst600847, "rest")]),'is-empty':RUNTIME.makeBool(false)}); 
})(); } else { return (function(){
 return RUNTIME.makeObject({'is-empty':RUNTIME.makeBool(true)}); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
var these_DASH_checks626049 = RUNTIME.applyFunc(mklist600452, [RUNTIME.applyFunc(lst_DASH_to_DASH_structural625953, [checks625918])]);
var old_DASH_results626066 = current_DASH_results623068;
var these_DASH_check_DASH_results626073 = RUNTIME.applyFunc(map607310, [RUNTIME.makeFunction(function (chk552093626074) { return (function(){
 var chk626076 = chk552093626074;
return (function(){
 var l600892 = RUNTIME.getField(chk626076, "location");
var loc619707 = RUNTIME.applyFunc(RUNTIME.getField(error601487, "location"), [RUNTIME.getField(l600892, "file"),RUNTIME.getField(l600892, "line"),RUNTIME.getField(l600892, "column")]);
current_DASH_results623068 = empty600466;
var result623078 = (function() { try { return (function(){
 RUNTIME.applyFunc(RUNTIME.getField(chk626076, "run"), []);
return RUNTIME.applyFunc(normal_DASH_result625140, [RUNTIME.getField(chk626076, "name"),loc619707,current_DASH_results623068]); 
})(); } catch (g551259626153) { g551259626153 = RUNTIME.unwrapException(g551259626153); return (function(){
 return RUNTIME.applyFunc(RUNTIME.makeFunction(function (e552094626154) { return (function(){
 var e603346 = e552094626154;
return (function(){
 return RUNTIME.applyFunc(error_DASH_result625631, [RUNTIME.getField(chk626076, "name"),loc619707,current_DASH_results623068,e603346]); 
})(); 
})(); }, RUNTIME.makeString("")), [RUNTIME.applyFunc(RUNTIME.getField(error601487, "make-error"), [g551259626153])]); 
})(); } })();
return result623078; 
})(); 
})(); }, RUNTIME.makeString("")),these_DASH_checks626049]);
var relevant_DASH_results626250 = RUNTIME.applyFunc(RUNTIME.getField(these_DASH_check_DASH_results626073, "filter"), [RUNTIME.makeFunction(function (elt552095626255) { return (function(){
 var elt602188 = elt552095626255;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(is_DASH_error_DASH_result625450, [elt602188]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(elt602188, "results"), "length"), []), "_greaterthan"), [RUNTIME.makeNumber(0)]); 
})(); }, RUNTIME.makeString(""))]); 
})(); 
})(); }, RUNTIME.makeString(""))]);
current_DASH_results623068 = old_DASH_results626066;
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(relevant_DASH_results626250, "length"), []), "_greaterthan"), [RUNTIME.makeNumber(0)]))) { return (function(){
 all_DASH_results625882 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552096626360) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552096626360,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [RUNTIME.applyFunc(RUNTIME.getField(all_DASH_results625882, "push"), [relevant_DASH_results626250])]);
return nothing600214; 
})(); } else { return nothing600214; } })();
return nothing600214; 
})(); } else { return nothing600214; } })();
return nothing600214; 
})(); 
})(); }, RUNTIME.makeString(""));
var clear_DASH_results626453 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 all_DASH_results625882 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552097626454) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552097626454,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [empty600466]);
return nothing600214; 
})(); 
})(); }, RUNTIME.makeString(""));
var get_DASH_results626506 = RUNTIME.makeFunction(function (val552098626507) { return (function(){
 var val623980 = val552098626507;
return (function(){
 return RUNTIME.makeObject({'results':all_DASH_results625882,'format':RUNTIME.makeMethod(function (self600661) { return (function(){
 return RUNTIME.applyFunc(format_DASH_check_DASH_results626520, [RUNTIME.getField(self600661, "results")]); 
})(); }, RUNTIME.makeString("")),'val':val623980}); 
})(); 
})(); }, RUNTIME.makeString(""));
var format_DASH_check_DASH_results626520 = RUNTIME.makeFunction(function (results_DASH_list552099626573) { return (function(){
 var results_DASH_list626575 = results_DASH_list552099626573;
return (function(){
 return RUNTIME.applyFunc(print600204, [RUNTIME.getField(RUNTIME.applyFunc(check_DASH_results_DASH_summary626582, [results_DASH_list626575]), "message")]); 
})(); 
})(); }, RUNTIME.makeString(""));
var check_DASH_results_DASH_summary626582 = RUNTIME.makeFunction(function (results_DASH_list552100626617) { return (function(){
 var results_DASH_list626575 = results_DASH_list552100626617;
return (function(){
 var init626625 = RUNTIME.makeObject({'passed':RUNTIME.makeNumber(0),'failed':RUNTIME.makeNumber(0),'test-errors':RUNTIME.makeNumber(0),'other-errors':RUNTIME.makeNumber(0),'total':RUNTIME.makeNumber(0)});
var message626669 = RUNTIME.makeString("");
var append_DASH_str626678 = RUNTIME.makeFunction(function (appendage552101626679) { return (function(){
 var appendage626681 = appendage552101626679;
return (function(){
 return message626669 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(message626669, "_plus"), [appendage626681]), "_plus"), [RUNTIME.makeString("\n")]); 
})(); 
})(); }, RUNTIME.makeString(""));
var counts626732 = RUNTIME.applyFunc(fold602097, [RUNTIME.makeFunction(function (acc552102626733,results552103626734) { return (function(){
 var acc601372 = acc552102626733;
var results626743 = results552103626734;
return (function(){
 return RUNTIME.applyFunc(fold602097, [RUNTIME.makeFunction(function (inner_DASH_acc552104626750,check_DASH_result552105626751) { return (function(){
 var inner_DASH_acc626754 = inner_DASH_acc552104626750;
var check_DASH_result626761 = check_DASH_result552105626751;
return (function(){
 var inner_DASH_results626768 = RUNTIME.getField(check_DASH_result626761, "results");
var new_DASH_passed626779 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results626768, "filter"), [is_DASH_success621824]), "length"), []);
var new_DASH_failed626803 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results626768, "filter"), [is_DASH_failure622230]), "length"), []);
var new_DASH_errors626827 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results626768, "filter"), [is_DASH_err622692]), "length"), []);
var other_DASH_errors626851 = RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(link600471, [check_DASH_result626761,empty600466]), "filter"), [is_DASH_error_DASH_result625450]), "length"), []);
var new_DASH_results626881 = inner_DASH_acc626754.extend('passed', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc626754, "passed"), "_plus"), [new_DASH_passed626779])).extend('failed', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc626754, "failed"), "_plus"), [new_DASH_failed626803])).extend('test-errors', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc626754, "test-errors"), "_plus"), [new_DASH_errors626827])).extend('other-errors', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc626754, "other-errors"), "_plus"), [other_DASH_errors626851])).extend('total', RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(inner_DASH_acc626754, "total"), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results626768, "length"), [])]));
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [new_DASH_failed626803,RUNTIME.makeNumber(0)]), "_not"), []), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [new_DASH_errors626827,RUNTIME.makeNumber(0)]), "_not"), []); 
})(); }, RUNTIME.makeString(""))]), "_or"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [other_DASH_errors626851,RUNTIME.makeNumber(0)]), "_not"), []); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("\n\nIn check block at "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(check_DASH_result626761, "location"), "format"), [])])]);
return nothing600214; 
})(); } else { return nothing600214; } })();
RUNTIME.applyFunc(each609206, [RUNTIME.makeFunction(function (fail552106627125) { return (function(){
 var fail627127 = fail552106627125;
return (function(){
 var cases_DASH_value551462627134 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552107627135) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Option607195,specimen552107627135,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [RUNTIME.getField(fail627127, "location")]);
RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value551462627134, "_match"), [RUNTIME.makeObject({'none':RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return nothing600214; 
})(); 
})(); }, RUNTIME.makeString("")),'some':RUNTIME.makeFunction(function (loc552108627193) { return (function(){
 var loc619707 = loc552108627193;
return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("In test at "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(loc619707, "format"), [])])]); 
})(); 
})(); }, RUNTIME.makeString(""))}),RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(error601487, "cases-miss"), [RUNTIME.makeString("cases: no cases matched"),RUNTIME.applyFunc(RUNTIME.getField(error601487, "location"), [RUNTIME.makeString("moorings.arr"),RUNTIME.makeNumber(1057),RUNTIME.makeNumber(8)]),RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString(""))]);
RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Test "), "_plus"), [RUNTIME.getField(fail627127, "name")]), "_plus"), [RUNTIME.makeString(" failed:")])]);
RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.getField(fail627127, "reason")]);
return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("")]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results626768, "filter"), [is_DASH_failure622230])]);
RUNTIME.applyFunc(each609206, [RUNTIME.makeFunction(function (fail552109627390) { return (function(){
 var fail627127 = fail552109627390;
return (function(){
 var cases_DASH_value551463627398 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552110627399) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Option607195,specimen552110627399,RUNTIME.makeString("Option")]); 
})(); }, RUNTIME.makeString("internal contract for Option")), [RUNTIME.getField(fail627127, "location")]);
RUNTIME.applyFunc(RUNTIME.getField(cases_DASH_value551463627398, "_match"), [RUNTIME.makeObject({'none':RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return nothing600214; 
})(); 
})(); }, RUNTIME.makeString("")),'some':RUNTIME.makeFunction(function (loc552111627457) { return (function(){
 var loc619707 = loc552111627457;
return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("In test at "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(loc619707, "format"), [])])]); 
})(); 
})(); }, RUNTIME.makeString(""))}),RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(error601487, "cases-miss"), [RUNTIME.makeString("cases: no cases matched"),RUNTIME.applyFunc(RUNTIME.getField(error601487, "location"), [RUNTIME.makeString("moorings.arr"),RUNTIME.makeNumber(1067),RUNTIME.makeNumber(8)]),RUNTIME.getField(list601511, "empty")])]); 
})(); 
})(); }, RUNTIME.makeString(""))]);
RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Test "), "_plus"), [RUNTIME.getField(fail627127, "name")]), "_plus"), [RUNTIME.makeString(" raised an error:")])]);
RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(fail627127, "exception"), "tostring"), [])]);
RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("")]);
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field600323, [RUNTIME.getField(fail627127, "exception"),RUNTIME.makeString("trace")]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("Trace:")]);
RUNTIME.applyFunc(each609206, [RUNTIME.makeFunction(function (loc552112627654) { return (function(){
 var loc619707 = loc552112627654;
return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(loc619707, "format"), [])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.getField(RUNTIME.getField(fail627127, "exception"), "trace")]);
return nothing600214; 
})(); } else { return nothing600214; } })(); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.applyFunc(RUNTIME.getField(inner_DASH_results626768, "filter"), [is_DASH_err622692])]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(is_DASH_error_DASH_result625450, [check_DASH_result626761]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Check block "), "_plus"), [RUNTIME.getField(check_DASH_result626761, "name")]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(" "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(check_DASH_result626761, "location"), "format"), [])])]), "_plus"), [RUNTIME.makeString(" ended in an error: ")])]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Error619594, [RUNTIME.getField(check_DASH_result626761, "err")]))) { return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(check_DASH_result626761, "err"), "format"), [])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.getField(check_DASH_result626761, "err")]); 
})(); } })();
RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("")]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(has_DASH_field600323, [RUNTIME.getField(check_DASH_result626761, "err"),RUNTIME.makeString("trace")]))) { return (function(){
 RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("Trace:")]);
RUNTIME.applyFunc(each609206, [RUNTIME.makeFunction(function (loc552113627905) { return (function(){
 var loc619707 = loc552113627905;
return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("  "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(loc619707, "format"), [])])]); 
})(); 
})(); }, RUNTIME.makeString("")),RUNTIME.getField(RUNTIME.getField(check_DASH_result626761, "err"), "trace")]);
return nothing600214; 
})(); } else { return nothing600214; } })();
return nothing600214; 
})(); } else { return nothing600214; } })();
return new_DASH_results626881; 
})(); 
})(); }, RUNTIME.makeString("")),acc601372,results626743]); 
})(); 
})(); }, RUNTIME.makeString("")),init626625,results_DASH_list626575]);
(function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(counts626732, "other-errors"),RUNTIME.makeNumber(0)]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(counts626732, "failed"),RUNTIME.makeNumber(0)]); 
})(); }, RUNTIME.makeString(""))]), "_and"), [RUNTIME.makeFunction(function () { return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(counts626732, "test-errors"),RUNTIME.makeNumber(0)]); 
})(); }, RUNTIME.makeString(""))]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(counts626732, "passed"),RUNTIME.makeNumber(0)]))) { return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("\nWARNING: Your program didn't define any tests.  Add some where: and check:\nblocks to test your code, or run with the --no-checks option to signal that you\ndon't want tests run.\n")]); 
})(); } else { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [RUNTIME.getField(counts626732, "passed"),RUNTIME.makeNumber(1)]))) { return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Looks shipshape, your "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "passed"), "tostring"), [])]), "_plus"), [RUNTIME.makeString(" test passed, mate!")])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Looks shipshape, all "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "passed"), "tostring"), [])]), "_plus"), [RUNTIME.makeString(" tests passed, mate!")])]); 
})(); } })(); 
})(); } })(); 
})(); } else { return (function(){
 RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.makeString("Avast, there be bugs!")]);
return RUNTIME.applyFunc(append_DASH_str626678, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("Total: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "total"), "tostring"), [])]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", Passed: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "passed"), "tostring"), [])])]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", Failed: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "failed"), "tostring"), [])]), "_plus"), [RUNTIME.makeString(", Errors in tests: ")]), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "test-errors"), "tostring"), []), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString(", Errors in between tests: "), "_plus"), [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.getField(counts626732, "other-errors"), "tostring"), [])])])])])]); 
})(); } })();
return counts626732.extend('message', message626669); 
})(); 
})(); }, RUNTIME.makeString(""));
var checkers628495 = RUNTIME.makeObject({'CheckResultList':CheckResultList625871,'CheckResult':CheckResult623057,'check-is':check_DASH_is623158,'check-raises':check_DASH_raises623559,'check-true':check_DASH_true623969,'check-false':check_DASH_false624017,'check-equals':check_DASH_equals623987,'check-pred':check_DASH_pred624329,'check-exn':check_DASH_exn624596,'run-checks':run_DASH_checks625915,'format-check-results':format_DASH_check_DASH_results626520,'check-results-summary':check_DASH_results_DASH_summary626582,'clear-results':clear_DASH_results626453,'get-results':get_DASH_results626506,'normal-result':normal_DASH_result625140,'is-normal-result':is_DASH_normal_DASH_result624976,'error-result':error_DASH_result625631,'is-error-result':is_DASH_error_DASH_result625450,'success':success621971,'is-success':is_DASH_success621824,'failure':failure622394,'is-failure':is_DASH_failure622230,'err':err622856,'is-err':is_DASH_err622692});
var interp_DASH_basic628624 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 var Value_DASH_mixins551260628625 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551261628636 = RUNTIME.makeObject({});
var variant551262628645 = data_DASH_shared551261628636;
var variant551263628652 = data_DASH_shared551261628636;
var variant551264628659 = data_DASH_shared551261628636;
var Value551464628666 = RUNTIME.applyFunc(brander600249, []);
var numV_base551467628677 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("numV"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_numV628724,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("numV")]))) { return (function(){
 var call_DASH_numV551465628774 = RUNTIME.getField(cases_DASH_funs604869, "numV");
return RUNTIME.applyFunc(call_DASH_numV551465628774, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var numV551466628833 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_numV628724 = RUNTIME.getField(numV551466628833, "test");
var numV628854 = RUNTIME.makeFunction(function (value551468552114628855) { return (function(){
 var value551468628857 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552116628858) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen552116628858,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [value551468552114628855]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Value551464628666, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(numV551466628833, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552115628898) { return (function(){
 var self600661 = self552115628898;
return (function(){
 var mixin551265628906 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Value_DASH_mixins551260628625]))) { return RUNTIME.applyFunc(Value_DASH_mixins551260628625, []); } else { return Value_DASH_mixins551260628625; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551265628906, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551265628906, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for numV")), [numV_base551467628677.extend('value', value551468628857)])])]); 
})(); 
})(); }, RUNTIME.makeString("numV: Creates an instance of numV"));
var strV_base551471629003 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("strV"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_strV629050,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("strV")]))) { return (function(){
 var call_DASH_strV551469629100 = RUNTIME.getField(cases_DASH_funs604869, "strV");
return RUNTIME.applyFunc(call_DASH_strV551469629100, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var strV551470629159 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_strV629050 = RUNTIME.getField(strV551470629159, "test");
var strV629180 = RUNTIME.makeFunction(function (value551472552117629181) { return (function(){
 var value551472629183 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552119629184) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552119629184,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [value551472552117629181]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Value551464628666, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(strV551470629159, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552118629224) { return (function(){
 var self600661 = self552118629224;
return (function(){
 var mixin551266629232 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Value_DASH_mixins551260628625]))) { return RUNTIME.applyFunc(Value_DASH_mixins551260628625, []); } else { return Value_DASH_mixins551260628625; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551266629232, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551266629232, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for strV")), [strV_base551471629003.extend('value', value551472629183)])])]); 
})(); 
})(); }, RUNTIME.makeString("strV: Creates an instance of strV"));
var funV_base551475629329 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("funV"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_funV629400,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("funV")]))) { return (function(){
 var call_DASH_funV551473629474 = RUNTIME.getField(cases_DASH_funs604869, "funV");
return RUNTIME.applyFunc(call_DASH_funV551473629474, [RUNTIME.getField(self600661, "params"),RUNTIME.getField(self600661, "body"),RUNTIME.getField(self600661, "env")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var funV551474629543 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_funV629400 = RUNTIME.getField(funV551474629543, "test");
var funV629564 = RUNTIME.makeFunction(function (params551476552120629565,body551477552121629566,env551478552122629567) { return (function(){
 var params551476629571 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552124629572) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552124629572,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params551476552120629565]);
var body551477629604 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552125629605) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552125629605,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body551477552121629566]);
var env551478629638 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552126629639) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Env629641,specimen552126629639,RUNTIME.makeString("Env")]); 
})(); }, RUNTIME.makeString("internal contract for Env")), [env551478552122629567]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Value551464628666, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(funV551474629543, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552123629680) { return (function(){
 var self600661 = self552123629680;
return (function(){
 var mixin551267629688 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Value_DASH_mixins551260628625]))) { return RUNTIME.applyFunc(Value_DASH_mixins551260628625, []); } else { return Value_DASH_mixins551260628625; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551267629688, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551267629688, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for funV")), [funV_base551475629329.extend('params', params551476629571).extend('body', body551477629604).extend('env', env551478629638)])])]); 
})(); 
})(); }, RUNTIME.makeString("funV: Creates an instance of funV"));
var Value629793 = RUNTIME.getField(Value551464628666, "test");
var Env_DASH_mixins551268629804 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551269629815 = RUNTIME.makeObject({});
var variant551270629824 = data_DASH_shared551269629815;
var variant551271629831 = data_DASH_shared551269629815;
var Env551479629838 = RUNTIME.applyFunc(brander600249, []);
var mt_DASH_env_base551482629849 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("mt-env"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_mt_DASH_env629869,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("mt-env")]))) { return (function(){
 var call_DASH_mt_DASH_env551481629907 = RUNTIME.getField(cases_DASH_funs604869, "mt-env");
return RUNTIME.applyFunc(call_DASH_mt_DASH_env551481629907, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var mt_DASH_env551480629961 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_mt_DASH_env629869 = RUNTIME.getField(mt_DASH_env551480629961, "test");
var mt_DASH_env629982 = RUNTIME.applyFunc(RUNTIME.getField(Env551479629838, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mt_DASH_env551480629961, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552127629991) { return (function(){
 var self600661 = self552127629991;
return (function(){
 var mixin551272629999 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Env_DASH_mixins551268629804]))) { return RUNTIME.applyFunc(Env_DASH_mixins551268629804, []); } else { return Env_DASH_mixins551268629804; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551272629999, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551272629999, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for mt-env")), [mt_DASH_env_base551482629849])])]);
var an_DASH_env_base551485630078 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("an-env"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("val"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_an_DASH_env630149,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("val"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("env"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("an-env")]))) { return (function(){
 var call_DASH_an_DASH_env551483630223 = RUNTIME.getField(cases_DASH_funs604869, "an-env");
return RUNTIME.applyFunc(call_DASH_an_DASH_env551483630223, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "val"),RUNTIME.getField(self600661, "env")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var an_DASH_env551484630292 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_an_DASH_env630149 = RUNTIME.getField(an_DASH_env551484630292, "test");
var an_DASH_env630313 = RUNTIME.makeFunction(function (name551486552128630314,val551487552129630315,env551488552130630316) { return (function(){
 var name551486630320 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552132630321) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552132630321,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551486552128630314]);
var val551487630353 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552133630354) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Value629793,specimen552133630354,RUNTIME.makeString("Value")]); 
})(); }, RUNTIME.makeString("internal contract for Value")), [val551487552129630315]);
var env551488630386 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552134630387) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Env629641,specimen552134630387,RUNTIME.makeString("Env")]); 
})(); }, RUNTIME.makeString("internal contract for Env")), [env551488552130630316]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Env551479629838, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(an_DASH_env551484630292, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552131630427) { return (function(){
 var self600661 = self552131630427;
return (function(){
 var mixin551273630435 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Env_DASH_mixins551268629804]))) { return RUNTIME.applyFunc(Env_DASH_mixins551268629804, []); } else { return Env_DASH_mixins551268629804; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551273630435, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551273630435, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for an-env")), [an_DASH_env_base551485630078.extend('name', name551486630320).extend('val', val551487630353).extend('env', env551488630386)])])]); 
})(); 
})(); }, RUNTIME.makeString("an-env: Creates an instance of an-env"));
var Env629641 = RUNTIME.getField(Env551479629838, "test");
var Expr_DASH_mixins551274630550 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551275630561 = RUNTIME.makeObject({});
var variant551276630570 = data_DASH_shared551275630561;
var variant551277630577 = data_DASH_shared551275630561;
var variant551278630584 = data_DASH_shared551275630561;
var variant551279630591 = data_DASH_shared551275630561;
var variant551280630598 = data_DASH_shared551275630561;
var variant551281630605 = data_DASH_shared551275630561;
var variant551282630612 = data_DASH_shared551275630561;
var variant551283630619 = data_DASH_shared551275630561;
var Expr551489630626 = RUNTIME.applyFunc(brander600249, []);
var id_base551492630637 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("id"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_id630684,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("id")]))) { return (function(){
 var call_DASH_id551490630734 = RUNTIME.getField(cases_DASH_funs604869, "id");
return RUNTIME.applyFunc(call_DASH_id551490630734, [RUNTIME.getField(self600661, "name")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var id551491630793 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_id630684 = RUNTIME.getField(id551491630793, "test");
var id630814 = RUNTIME.makeFunction(function (name551493552135630815) { return (function(){
 var name551493630817 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552137630818) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552137630818,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551493552135630815]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(id551491630793, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552136630858) { return (function(){
 var self600661 = self552136630858;
return (function(){
 var mixin551284630866 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551284630866, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551284630866, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for id")), [id_base551492630637.extend('name', name551493630817)])])]); 
})(); 
})(); }, RUNTIME.makeString("id: Creates an instance of id"));
var num_base551496630963 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("num"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_num631010,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("num")]))) { return (function(){
 var call_DASH_num551494631060 = RUNTIME.getField(cases_DASH_funs604869, "num");
return RUNTIME.applyFunc(call_DASH_num551494631060, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var num551495631119 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_num631010 = RUNTIME.getField(num551495631119, "test");
var num610241 = RUNTIME.makeFunction(function (value551497552138631140) { return (function(){
 var value551497631142 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552140631143) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen552140631143,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [value551497552138631140]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(num551495631119, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552139631183) { return (function(){
 var self600661 = self552139631183;
return (function(){
 var mixin551285631191 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551285631191, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551285631191, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for num")), [num_base551496630963.extend('value', value551497631142)])])]); 
})(); 
})(); }, RUNTIME.makeString("num: Creates an instance of num"));
var str_base551500631288 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("str"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_str631335,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("str")]))) { return (function(){
 var call_DASH_str551498631385 = RUNTIME.getField(cases_DASH_funs604869, "str");
return RUNTIME.applyFunc(call_DASH_str551498631385, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var str551499631444 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_str631335 = RUNTIME.getField(str551499631444, "test");
var str603460 = RUNTIME.makeFunction(function (value551501552141631465) { return (function(){
 var value551501631467 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552143631468) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552143631468,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [value551501552141631465]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str551499631444, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552142631508) { return (function(){
 var self600661 = self552142631508;
return (function(){
 var mixin551286631516 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551286631516, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551286631516, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str")), [str_base551500631288.extend('value', value551501631467)])])]); 
})(); 
})(); }, RUNTIME.makeString("str: Creates an instance of str"));
var bop_base551504631613 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("bop"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_bop631684,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("bop")]))) { return (function(){
 var call_DASH_bop551502631758 = RUNTIME.getField(cases_DASH_funs604869, "bop");
return RUNTIME.applyFunc(call_DASH_bop551502631758, [RUNTIME.getField(self600661, "op"),RUNTIME.getField(self600661, "left"),RUNTIME.getField(self600661, "right")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var bop551503631827 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_bop631684 = RUNTIME.getField(bop551503631827, "test");
var bop631848 = RUNTIME.makeFunction(function (op551505552144631849,left551506552145631850,right551507552146631851) { return (function(){
 var op551505631855 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552148631856) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Operator631858,specimen552148631856,RUNTIME.makeString("Operator")]); 
})(); }, RUNTIME.makeString("internal contract for Operator")), [op551505552144631849]);
var left551506631889 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552149631890) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552149631890,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [left551506552145631850]);
var right551507631922 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552150631923) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552150631923,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [right551507552146631851]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(bop551503631827, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552147631963) { return (function(){
 var self600661 = self552147631963;
return (function(){
 var mixin551287631971 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551287631971, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551287631971, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for bop")), [bop_base551504631613.extend('op', op551505631855).extend('left', left551506631889).extend('right', right551507631922)])])]); 
})(); 
})(); }, RUNTIME.makeString("bop: Creates an instance of bop"));
var cif_base551510632076 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("cif"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_cif632147,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("cif")]))) { return (function(){
 var call_DASH_cif551508632221 = RUNTIME.getField(cases_DASH_funs604869, "cif");
return RUNTIME.applyFunc(call_DASH_cif551508632221, [RUNTIME.getField(self600661, "cond"),RUNTIME.getField(self600661, "consq"),RUNTIME.getField(self600661, "altern")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var cif551509632290 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_cif632147 = RUNTIME.getField(cif551509632290, "test");
var cif632311 = RUNTIME.makeFunction(function (cond551511552151632312,consq551512552152632313,altern551513552153632314) { return (function(){
 var cond551511632318 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552155632319) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552155632319,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [cond551511552151632312]);
var consq551512632351 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552156632352) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552156632352,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [consq551512552152632313]);
var altern551513632384 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552157632385) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552157632385,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [altern551513552153632314]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(cif551509632290, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552154632425) { return (function(){
 var self600661 = self552154632425;
return (function(){
 var mixin551288632433 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551288632433, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551288632433, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for cif")), [cif_base551510632076.extend('cond', cond551511632318).extend('consq', consq551512632351).extend('altern', altern551513632384)])])]); 
})(); 
})(); }, RUNTIME.makeString("cif: Creates an instance of cif"));
var let_base551516632538 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("let"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_let632609,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("let")]))) { return (function(){
 var call_DASH_let551514632683 = RUNTIME.getField(cases_DASH_funs604869, "let");
return RUNTIME.applyFunc(call_DASH_let551514632683, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "expr"),RUNTIME.getField(self600661, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var let551515632752 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_let632609 = RUNTIME.getField(let551515632752, "test");
var let632773 = RUNTIME.makeFunction(function (name551517552158632774,expr551518552159632775,body551519552160632776) { return (function(){
 var name551517632780 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552162632781) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552162632781,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551517552158632774]);
var expr551518632813 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552163632814) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552163632814,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [expr551518552159632775]);
var body551519632846 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552164632847) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552164632847,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body551519552160632776]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(let551515632752, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552161632887) { return (function(){
 var self600661 = self552161632887;
return (function(){
 var mixin551289632895 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551289632895, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551289632895, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for let")), [let_base551516632538.extend('name', name551517632780).extend('expr', expr551518632813).extend('body', body551519632846)])])]); 
})(); 
})(); }, RUNTIME.makeString("let: Creates an instance of let"));
var lam_base551522633000 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("lam"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_lam633059,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("lam")]))) { return (function(){
 var call_DASH_lam551520633121 = RUNTIME.getField(cases_DASH_funs604869, "lam");
return RUNTIME.applyFunc(call_DASH_lam551520633121, [RUNTIME.getField(self600661, "params"),RUNTIME.getField(self600661, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var lam551521633185 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_lam633059 = RUNTIME.getField(lam551521633185, "test");
var lam633206 = RUNTIME.makeFunction(function (params551523552165633207,body551524552166633208) { return (function(){
 var params551523633211 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552168633212) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552168633212,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params551523552165633207]);
var body551524633244 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552169633245) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552169633245,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body551524552166633208]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(lam551521633185, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552167633285) { return (function(){
 var self600661 = self552167633285;
return (function(){
 var mixin551290633293 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551290633293, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551290633293, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for lam")), [lam_base551522633000.extend('params', params551523633211).extend('body', body551524633244)])])]); 
})(); 
})(); }, RUNTIME.makeString("lam: Creates an instance of lam"));
var app_base551527633394 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("app"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_app633453,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("app")]))) { return (function(){
 var call_DASH_app551525633515 = RUNTIME.getField(cases_DASH_funs604869, "app");
return RUNTIME.applyFunc(call_DASH_app551525633515, [RUNTIME.getField(self600661, "func"),RUNTIME.getField(self600661, "args")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var app551526633579 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_app633453 = RUNTIME.getField(app551526633579, "test");
var app633600 = RUNTIME.makeFunction(function (func551528552170633601,args551529552171633602) { return (function(){
 var func551528633605 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552173633606) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552173633606,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [func551528552170633601]);
var args551529633638 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552174633639) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552174633639,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [args551529552171633602]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551489630626, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(app551526633579, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552172633679) { return (function(){
 var self600661 = self552172633679;
return (function(){
 var mixin551291633687 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551274630550]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551274630550, []); } else { return Expr_DASH_mixins551274630550; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551291633687, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551291633687, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for app")), [app_base551527633394.extend('func', func551528633605).extend('args', args551529633638)])])]); 
})(); 
})(); }, RUNTIME.makeString("app: Creates an instance of app"));
var Expr629607 = RUNTIME.getField(Expr551489630626, "test");
var Operator_DASH_mixins551292633798 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551293633809 = RUNTIME.makeObject({});
var variant551294633818 = data_DASH_shared551293633809;
var variant551295633825 = data_DASH_shared551293633809;
var variant551296633832 = data_DASH_shared551293633809;
var variant551297633839 = data_DASH_shared551293633809;
var Operator551530633846 = RUNTIME.applyFunc(brander600249, []);
var plus_base551533633857 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("plus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_plus633877,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("plus")]))) { return (function(){
 var call_DASH_plus551532633915 = RUNTIME.getField(cases_DASH_funs604869, "plus");
return RUNTIME.applyFunc(call_DASH_plus551532633915, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var plus551531633969 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_plus633877 = RUNTIME.getField(plus551531633969, "test");
var plus633990 = RUNTIME.applyFunc(RUNTIME.getField(Operator551530633846, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(plus551531633969, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552175633999) { return (function(){
 var self600661 = self552175633999;
return (function(){
 var mixin551298634007 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551292633798]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551292633798, []); } else { return Operator_DASH_mixins551292633798; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551298634007, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551298634007, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for plus")), [plus_base551533633857])])]);
var minus_base551536634086 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("minus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_minus634106,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("minus")]))) { return (function(){
 var call_DASH_minus551535634144 = RUNTIME.getField(cases_DASH_funs604869, "minus");
return RUNTIME.applyFunc(call_DASH_minus551535634144, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var minus551534634198 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_minus634106 = RUNTIME.getField(minus551534634198, "test");
var minus634219 = RUNTIME.applyFunc(RUNTIME.getField(Operator551530633846, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(minus551534634198, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552176634228) { return (function(){
 var self600661 = self552176634228;
return (function(){
 var mixin551299634236 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551292633798]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551292633798, []); } else { return Operator_DASH_mixins551292633798; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551299634236, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551299634236, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for minus")), [minus_base551536634086])])]);
var append_base551539634315 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("append"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_append634335,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("append")]))) { return (function(){
 var call_DASH_append551538634373 = RUNTIME.getField(cases_DASH_funs604869, "append");
return RUNTIME.applyFunc(call_DASH_append551538634373, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var append551537634427 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_append634335 = RUNTIME.getField(append551537634427, "test");
var append634448 = RUNTIME.applyFunc(RUNTIME.getField(Operator551530633846, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(append551537634427, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552177634457) { return (function(){
 var self600661 = self552177634457;
return (function(){
 var mixin551300634465 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551292633798]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551292633798, []); } else { return Operator_DASH_mixins551292633798; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551300634465, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551300634465, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for append")), [append_base551539634315])])]);
var str_DASH_eq_base551542634544 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("str-eq"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_str_DASH_eq634564,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("str-eq")]))) { return (function(){
 var call_DASH_str_DASH_eq551541634602 = RUNTIME.getField(cases_DASH_funs604869, "str-eq");
return RUNTIME.applyFunc(call_DASH_str_DASH_eq551541634602, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var str_DASH_eq551540634656 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_str_DASH_eq634564 = RUNTIME.getField(str_DASH_eq551540634656, "test");
var str_DASH_eq634677 = RUNTIME.applyFunc(RUNTIME.getField(Operator551530633846, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str_DASH_eq551540634656, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552178634686) { return (function(){
 var self600661 = self552178634686;
return (function(){
 var mixin551301634694 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551292633798]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551292633798, []); } else { return Operator_DASH_mixins551292633798; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551301634694, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551301634694, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str-eq")), [str_DASH_eq_base551542634544])])]);
var Operator631858 = RUNTIME.getField(Operator551530633846, "test");
var parse634783 = RUNTIME.makeFunction(function (prog552179634784) { return (function(){
 var prog634786 = prog552179634784;
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552186634793) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552186634793,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [(function(){
 var check_DASH_params634814 = RUNTIME.makeFunction(function (params552180634815) { return (function(){
 var params634817 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552184634818) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552184634818,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params552180634815]);
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552183634850) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552183634850,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [(function(){
 RUNTIME.applyFunc(each609206, [RUNTIME.makeFunction(function (param552181634871) { return (function(){
 var param634873 = param552181634871;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(params634817, "filter"), [RUNTIME.makeFunction(function (x552182634884) { return (function(){
 var x620259 = x552182634884;
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [x620259,param634873]); 
})(); 
})(); }, RUNTIME.makeString(""))]), "length"), []), "_greaterthan"), [RUNTIME.makeNumber(1)]))) { return (function(){
 RUNTIME.applyFunc(raise600209, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.makeString("parse: function has duplicate parameter "), "_plus"), [param634873])]);
return nothing600214; 
})(); } else { return nothing600214; } })(); 
})(); 
})(); }, RUNTIME.makeString("")),params634817]);
return params634817; 
})()]); 
})(); }, RUNTIME.makeString("Ensure that a function has no duplicate parameter names."));
var convert635019 = RUNTIME.makeFunction(function (sexpr552185635020) { return (function(){
 var sexpr635022 = sexpr552185635020;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List601382, [sexpr635022]))) { return (function(){
 var head635070 = RUNTIME.getField(sexpr635022, "first");
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("string")]))) { return (function(){
 return RUNTIME.applyFunc(str603460, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("if")]))) { return (function(){
 return RUNTIME.applyFunc(cif632311, [RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("let")]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List601382, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]))) { return (function(){
 return RUNTIME.applyFunc(let632773, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(0)]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(let632773, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("fun")]))) { return (function(){
 return RUNTIME.applyFunc(lam633206, [RUNTIME.applyFunc(check_DASH_params634814, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("+")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [plus633990,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("-")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [minus634219,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("++")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [append634448,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("==")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [str_DASH_eq634677,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 var func635658 = RUNTIME.applyFunc(convert635019, [head635070]);
var args635670 = RUNTIME.applyFunc(map607310, [convert635019,RUNTIME.getField(sexpr635022, "rest")]);
return RUNTIME.applyFunc(app633600, [func635658,args635670]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(Number600269, [sexpr635022]))) { return (function(){
 return RUNTIME.applyFunc(num610241, [sexpr635022]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(String600274, [sexpr635022]))) { return (function(){
 return RUNTIME.applyFunc(id630814, [sexpr635022]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.makeString("if: no tests matched")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString("Convert an s-expression into an Expr."));
return RUNTIME.applyFunc(convert635019, [prog634786]); 
})()]); 
})(); }, RUNTIME.makeString("Parse an s-expr in Paret's concrete syntax into an Expr."));
return RUNTIME.makeObject({'Value':Value629793,'numV':numV628854,'is-numV':is_DASH_numV628724,'strV':strV629180,'is-strV':is_DASH_strV629050,'funV':funV629564,'is-funV':is_DASH_funV629400,'Env':Env629641,'mt-env':mt_DASH_env629982,'is-mt-env':is_DASH_mt_DASH_env629869,'an-env':an_DASH_env630313,'is-an-env':is_DASH_an_DASH_env630149,'Expr':Expr629607,'id':id630814,'is-id':is_DASH_id630684,'num':num610241,'is-num':is_DASH_num631010,'str':str603460,'is-str':is_DASH_str631335,'bop':bop631848,'is-bop':is_DASH_bop631684,'cif':cif632311,'is-cif':is_DASH_cif632147,'let':let632773,'is-let':is_DASH_let632609,'lam':lam633206,'is-lam':is_DASH_lam633059,'app':app633600,'is-app':is_DASH_app633453,'Operator':Operator631858,'plus':plus633990,'is-plus':is_DASH_plus633877,'minus':minus634219,'is-minus':is_DASH_minus634106,'append':append634448,'is-append':is_DASH_append634335,'str-eq':str_DASH_eq634677,'is-str-eq':is_DASH_str_DASH_eq634564,'parse':parse634783}); 
})(); 
})(); }, RUNTIME.makeString(""));
var calculate_DASH_locals635999 = RUNTIME.makeFunction(function () { return (function(){
 return (function(){
 var Expr_DASH_mixins551302636000 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551303636011 = RUNTIME.makeObject({});
var variant551304636020 = data_DASH_shared551303636011;
var variant551305636027 = data_DASH_shared551303636011;
var variant551306636034 = data_DASH_shared551303636011;
var variant551307636041 = data_DASH_shared551303636011;
var variant551308636048 = data_DASH_shared551303636011;
var variant551309636055 = data_DASH_shared551303636011;
var variant551310636062 = data_DASH_shared551303636011;
var variant551311636069 = data_DASH_shared551303636011;
var variant551312636076 = data_DASH_shared551303636011;
var Expr551543636083 = RUNTIME.applyFunc(brander600249, []);
var id_base551546636094 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("id"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_id630684,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("id")]))) { return (function(){
 var call_DASH_id551544636190 = RUNTIME.getField(cases_DASH_funs604869, "id");
return RUNTIME.applyFunc(call_DASH_id551544636190, [RUNTIME.getField(self600661, "name")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var id551545636249 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_id630684 = RUNTIME.getField(id551545636249, "test");
var id630814 = RUNTIME.makeFunction(function (name551547552187636270) { return (function(){
 var name551547636272 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552189636273) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552189636273,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551547552187636270]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(id551545636249, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552188636313) { return (function(){
 var self600661 = self552188636313;
return (function(){
 var mixin551313636321 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551313636321, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551313636321, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for id")), [id_base551546636094.extend('name', name551547636272)])])]); 
})(); 
})(); }, RUNTIME.makeString("id: Creates an instance of id"));
var num_base551550636418 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("num"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_num631010,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("num")]))) { return (function(){
 var call_DASH_num551548636514 = RUNTIME.getField(cases_DASH_funs604869, "num");
return RUNTIME.applyFunc(call_DASH_num551548636514, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var num551549636573 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_num631010 = RUNTIME.getField(num551549636573, "test");
var num610241 = RUNTIME.makeFunction(function (value551551552190636594) { return (function(){
 var value551551636596 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552192636597) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Number600269,specimen552192636597,RUNTIME.makeString("Number")]); 
})(); }, RUNTIME.makeString("internal contract for Number")), [value551551552190636594]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(num551549636573, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552191636637) { return (function(){
 var self600661 = self552191636637;
return (function(){
 var mixin551314636645 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551314636645, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551314636645, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for num")), [num_base551550636418.extend('value', value551551636596)])])]); 
})(); 
})(); }, RUNTIME.makeString("num: Creates an instance of num"));
var str_base551554636742 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("str"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_str631335,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("value"),RUNTIME.getField(list601511, "empty")])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("str")]))) { return (function(){
 var call_DASH_str551552636838 = RUNTIME.getField(cases_DASH_funs604869, "str");
return RUNTIME.applyFunc(call_DASH_str551552636838, [RUNTIME.getField(self600661, "value")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var str551553636897 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_str631335 = RUNTIME.getField(str551553636897, "test");
var str603460 = RUNTIME.makeFunction(function (value551555552193636918) { return (function(){
 var value551555636920 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552195636921) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552195636921,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [value551555552193636918]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str551553636897, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552194636961) { return (function(){
 var self600661 = self552194636961;
return (function(){
 var mixin551315636969 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551315636969, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551315636969, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str")), [str_base551554636742.extend('value', value551555636920)])])]); 
})(); 
})(); }, RUNTIME.makeString("str: Creates an instance of str"));
var bop_base551558637066 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("bop"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_bop631684,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("op"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("left"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("right"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("bop")]))) { return (function(){
 var call_DASH_bop551556637210 = RUNTIME.getField(cases_DASH_funs604869, "bop");
return RUNTIME.applyFunc(call_DASH_bop551556637210, [RUNTIME.getField(self600661, "op"),RUNTIME.getField(self600661, "left"),RUNTIME.getField(self600661, "right")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var bop551557637279 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_bop631684 = RUNTIME.getField(bop551557637279, "test");
var bop631848 = RUNTIME.makeFunction(function (op551559552196637300,left551560552197637301,right551561552198637302) { return (function(){
 var op551559637306 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552200637307) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Operator631858,specimen552200637307,RUNTIME.makeString("Operator")]); 
})(); }, RUNTIME.makeString("internal contract for Operator")), [op551559552196637300]);
var left551560637339 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552201637340) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552201637340,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [left551560552197637301]);
var right551561637372 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552202637373) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552202637373,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [right551561552198637302]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(bop551557637279, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552199637413) { return (function(){
 var self600661 = self552199637413;
return (function(){
 var mixin551316637421 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551316637421, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551316637421, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for bop")), [bop_base551558637066.extend('op', op551559637306).extend('left', left551560637339).extend('right', right551561637372)])])]); 
})(); 
})(); }, RUNTIME.makeString("bop: Creates an instance of bop"));
var cif_base551564637526 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("cif"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_cif632147,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("cond"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("consq"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("altern"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("cif")]))) { return (function(){
 var call_DASH_cif551562637670 = RUNTIME.getField(cases_DASH_funs604869, "cif");
return RUNTIME.applyFunc(call_DASH_cif551562637670, [RUNTIME.getField(self600661, "cond"),RUNTIME.getField(self600661, "consq"),RUNTIME.getField(self600661, "altern")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var cif551563637739 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_cif632147 = RUNTIME.getField(cif551563637739, "test");
var cif632311 = RUNTIME.makeFunction(function (cond551565552203637760,consq551566552204637761,altern551567552205637762) { return (function(){
 var cond551565637766 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552207637767) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552207637767,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [cond551565552203637760]);
var consq551566637799 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552208637800) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552208637800,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [consq551566552204637761]);
var altern551567637832 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552209637833) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552209637833,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [altern551567552205637762]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(cif551563637739, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552206637873) { return (function(){
 var self600661 = self552206637873;
return (function(){
 var mixin551317637881 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551317637881, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551317637881, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for cif")), [cif_base551564637526.extend('cond', cond551565637766).extend('consq', consq551566637799).extend('altern', altern551567637832)])])]); 
})(); 
})(); }, RUNTIME.makeString("cif: Creates an instance of cif"));
var let_base551570637986 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("let"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_let632609,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("name"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("expr"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("let")]))) { return (function(){
 var call_DASH_let551568638130 = RUNTIME.getField(cases_DASH_funs604869, "let");
return RUNTIME.applyFunc(call_DASH_let551568638130, [RUNTIME.getField(self600661, "name"),RUNTIME.getField(self600661, "expr"),RUNTIME.getField(self600661, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var let551569638199 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_let632609 = RUNTIME.getField(let551569638199, "test");
var let632773 = RUNTIME.makeFunction(function (name551571552210638220,expr551572552211638221,body551573552212638222) { return (function(){
 var name551571638226 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552214638227) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [String600274,specimen552214638227,RUNTIME.makeString("String")]); 
})(); }, RUNTIME.makeString("internal contract for String")), [name551571552210638220]);
var expr551572638259 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552215638260) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552215638260,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [expr551572552211638221]);
var body551573638292 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552216638293) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552216638293,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body551573552212638222]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(let551569638199, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552213638333) { return (function(){
 var self600661 = self552213638333;
return (function(){
 var mixin551318638341 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551318638341, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551318638341, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for let")), [let_base551570637986.extend('name', name551571638226).extend('expr', expr551572638259).extend('body', body551573638292)])])]); 
})(); 
})(); }, RUNTIME.makeString("let: Creates an instance of let"));
var lam_base551576638446 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("lam"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_lam633059,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("params"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("body"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("lam")]))) { return (function(){
 var call_DASH_lam551574638566 = RUNTIME.getField(cases_DASH_funs604869, "lam");
return RUNTIME.applyFunc(call_DASH_lam551574638566, [RUNTIME.getField(self600661, "params"),RUNTIME.getField(self600661, "body")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var lam551575638630 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_lam633059 = RUNTIME.getField(lam551575638630, "test");
var lam633206 = RUNTIME.makeFunction(function (params551577552217638651,body551578552218638652) { return (function(){
 var params551577638655 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552220638656) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552220638656,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [params551577552217638651]);
var body551578638688 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552221638689) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552221638689,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [body551578552218638652]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(lam551575638630, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552219638729) { return (function(){
 var self600661 = self552219638729;
return (function(){
 var mixin551319638737 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551319638737, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551319638737, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for lam")), [lam_base551576638446.extend('params', params551577638655).extend('body', body551578638688)])])]); 
})(); 
})(); }, RUNTIME.makeString("lam: Creates an instance of lam"));
var app_base551581638838 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-to-repr"), [self600661,RUNTIME.makeString("app"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_app633453,RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("func"),RUNTIME.applyFunc(RUNTIME.getField(list601511, "link"), [RUNTIME.makeString("args"),RUNTIME.getField(list601511, "empty")])])]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("app")]))) { return (function(){
 var call_DASH_app551579638958 = RUNTIME.getField(cases_DASH_funs604869, "app");
return RUNTIME.applyFunc(call_DASH_app551579638958, [RUNTIME.getField(self600661, "func"),RUNTIME.getField(self600661, "args")]); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var app551580639022 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_app633453 = RUNTIME.getField(app551580639022, "test");
var app633600 = RUNTIME.makeFunction(function (func551582552222639043,args551583552223639044) { return (function(){
 var func551582639047 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552225639048) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552225639048,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [func551582552222639043]);
var args551583639080 = RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552226639081) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [List601382,specimen552226639081,RUNTIME.makeString("List")]); 
})(); }, RUNTIME.makeString("internal contract for List")), [args551583552223639044]);
return (function(){
 return RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(app551580639022, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552224639121) { return (function(){
 var self600661 = self552224639121;
return (function(){
 var mixin551320639129 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551320639129, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551320639129, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for app")), [app_base551581638838.extend('func', func551582639047).extend('args', args551583639080)])])]); 
})(); 
})(); }, RUNTIME.makeString("app: Creates an instance of app"));
var hole_base551586639230 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("hole"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_hole639250,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("hole")]))) { return (function(){
 var call_DASH_hole551585639288 = RUNTIME.getField(cases_DASH_funs604869, "hole");
return RUNTIME.applyFunc(call_DASH_hole551585639288, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var hole551584639342 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_hole639250 = RUNTIME.getField(hole551584639342, "test");
var hole639363 = RUNTIME.applyFunc(RUNTIME.getField(Expr551543636083, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(hole551584639342, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552227639372) { return (function(){
 var self600661 = self552227639372;
return (function(){
 var mixin551321639380 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Expr_DASH_mixins551302636000]))) { return RUNTIME.applyFunc(Expr_DASH_mixins551302636000, []); } else { return Expr_DASH_mixins551302636000; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551321639380, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551321639380, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for hole")), [hole_base551586639230])])]);
var Expr629607 = RUNTIME.getField(Expr551543636083, "test");
var Operator_DASH_mixins551322639469 = RUNTIME.getField(builtins600793, "Eq");
var data_DASH_shared551323639480 = RUNTIME.makeObject({});
var variant551324639489 = data_DASH_shared551323639480;
var variant551325639496 = data_DASH_shared551323639480;
var variant551326639503 = data_DASH_shared551323639480;
var variant551327639510 = data_DASH_shared551323639480;
var Operator551587639517 = RUNTIME.applyFunc(brander600249, []);
var plus_base551590639528 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("plus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_plus633877,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("plus")]))) { return (function(){
 var call_DASH_plus551589639585 = RUNTIME.getField(cases_DASH_funs604869, "plus");
return RUNTIME.applyFunc(call_DASH_plus551589639585, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var plus551588639639 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_plus633877 = RUNTIME.getField(plus551588639639, "test");
var plus633990 = RUNTIME.applyFunc(RUNTIME.getField(Operator551587639517, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(plus551588639639, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552228639668) { return (function(){
 var self600661 = self552228639668;
return (function(){
 var mixin551328639676 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551322639469]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551322639469, []); } else { return Operator_DASH_mixins551322639469; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551328639676, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551328639676, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for plus")), [plus_base551590639528])])]);
var minus_base551593639755 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("minus"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_minus634106,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("minus")]))) { return (function(){
 var call_DASH_minus551592639812 = RUNTIME.getField(cases_DASH_funs604869, "minus");
return RUNTIME.applyFunc(call_DASH_minus551592639812, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var minus551591639866 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_minus634106 = RUNTIME.getField(minus551591639866, "test");
var minus634219 = RUNTIME.applyFunc(RUNTIME.getField(Operator551587639517, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(minus551591639866, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552229639895) { return (function(){
 var self600661 = self552229639895;
return (function(){
 var mixin551329639903 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551322639469]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551322639469, []); } else { return Operator_DASH_mixins551322639469; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551329639903, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551329639903, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for minus")), [minus_base551593639755])])]);
var append_base551596639982 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("append"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_append634335,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("append")]))) { return (function(){
 var call_DASH_append551595640039 = RUNTIME.getField(cases_DASH_funs604869, "append");
return RUNTIME.applyFunc(call_DASH_append551595640039, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var append551594640093 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_append634335 = RUNTIME.getField(append551594640093, "test");
var append634448 = RUNTIME.applyFunc(RUNTIME.getField(Operator551587639517, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(append551594640093, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552230640122) { return (function(){
 var self600661 = self552230640122;
return (function(){
 var mixin551330640130 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551322639469]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551322639469, []); } else { return Operator_DASH_mixins551322639469; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551330640130, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551330640130, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for append")), [append_base551596639982])])]);
var str_DASH_eq_base551599640209 = RUNTIME.makeObject({'_torepr':RUNTIME.makeMethod(function (self600661) { return RUNTIME.makeString("str-eq"); }, RUNTIME.makeString("")),'_equals':RUNTIME.makeMethod(function (self600661,other600662) { return RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "data-equals"), [self600661,other600662,is_DASH_str_DASH_eq634564,RUNTIME.getField(list601511, "empty")]); }, RUNTIME.makeString("")),'_match':RUNTIME.makeMethod(function (self600661,cases_DASH_funs604869,else_DASH_clause604870) { return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "has-field"), [cases_DASH_funs604869,RUNTIME.makeString("str-eq")]))) { return (function(){
 var call_DASH_str_DASH_eq551598640266 = RUNTIME.getField(cases_DASH_funs604869, "str-eq");
return RUNTIME.applyFunc(call_DASH_str_DASH_eq551598640266, []); 
})(); } else { return RUNTIME.applyFunc(else_DASH_clause604870, []); } })(); }, RUNTIME.makeString(""))});
var str_DASH_eq551597640320 = RUNTIME.applyFunc(brander600249, []);
var is_DASH_str_DASH_eq634564 = RUNTIME.getField(str_DASH_eq551597640320, "test");
var str_DASH_eq634677 = RUNTIME.applyFunc(RUNTIME.getField(Operator551587639517, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(str_DASH_eq551597640320, "brand"), [RUNTIME.applyFunc(RUNTIME.makeFunction(function (self552231640349) { return (function(){
 var self600661 = self552231640349;
return (function(){
 var mixin551331640357 = (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(Function600259, [Operator_DASH_mixins551322639469]))) { return RUNTIME.applyFunc(Operator_DASH_mixins551322639469, []); } else { return Operator_DASH_mixins551322639469; } })();
return RUNTIME.applyFunc(RUNTIME.getField(mixin551331640357, "brand"), [RUNTIME.applyFunc(RUNTIME.getField(mixin551331640357, "extend"), [self600661])]); 
})(); 
})(); }, RUNTIME.makeString("Constructor for str-eq")), [str_DASH_eq_base551599640209])])]);
var Operator631858 = RUNTIME.getField(Operator551587639517, "test");
var parse634783 = RUNTIME.makeFunction(function (prog552232640446) { return (function(){
 var prog634786 = prog552232640446;
return RUNTIME.applyFunc(RUNTIME.makeFunction(function (specimen552234640454) { return (function(){
 return RUNTIME.applyFunc(check_DASH_brand600219, [Expr629607,specimen552234640454,RUNTIME.makeString("Expr")]); 
})(); }, RUNTIME.makeString("internal contract for Expr")), [(function(){
 var convert635019 = RUNTIME.makeFunction(function (sexpr552233640475) { return (function(){
 var sexpr635022 = sexpr552233640475;
return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List601382, [sexpr635022]))) { return (function(){
 var head635070 = RUNTIME.getField(sexpr635022, "first");
return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("string")]))) { return (function(){
 return RUNTIME.applyFunc(str603460, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("if")]))) { return (function(){
 return RUNTIME.applyFunc(cif632311, [RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("let")]))) { return (function(){
 return (function() { if (RUNTIME.isTrue(RUNTIME.applyFunc(List601382, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]))) { return (function(){
 return RUNTIME.applyFunc(let632773, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(0)]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]), "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(let632773, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(3)])])]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("fun")]))) { return (function(){
 return RUNTIME.applyFunc(lam633206, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("+")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [plus633990,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("-")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [minus634219,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("++")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [append634448,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [head635070,RUNTIME.makeString("==")]))) { return (function(){
 return RUNTIME.applyFunc(bop631848, [str_DASH_eq634677,RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(1)])]),RUNTIME.applyFunc(convert635019, [RUNTIME.applyFunc(RUNTIME.getField(sexpr635022, "get"), [RUNTIME.makeNumber(2)])])]); 
})(); } else { return (function(){
 var func635658 = RUNTIME.applyFunc(convert635019, [head635070]);
var args635670 = RUNTIME.applyFunc(map607310, [convert635019,RUNTIME.getField(sexpr635022, "rest")]);
return RUNTIME.applyFunc(app633600, [func635658,args635670]); 
})(); } })(); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(Number600269, [sexpr635022]))) { return (function(){
 return RUNTIME.applyFunc(num610241, [sexpr635022]); 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(RUNTIME.getField(builtins600793, "equiv"), [sexpr635022,RUNTIME.makeString("@")]))) { return (function(){
 return hole639363; 
})(); } else if (RUNTIME.isTrue(RUNTIME.applyFunc(String600274, [sexpr635022]))) { return (function(){
 return RUNTIME.applyFunc(id630814, [sexpr635022]); 
})(); } else { return (function(){
 return RUNTIME.applyFunc(raise600209, [RUNTIME.makeString("if: no tests matched")]); 
})(); } })(); 
})(); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.applyFunc(convert635019, [prog634786]); 
})()]); 
})(); }, RUNTIME.makeString(""));
return RUNTIME.makeObject({'Expr':Expr629607,'id':id630814,'is-id':is_DASH_id630684,'num':num610241,'is-num':is_DASH_num631010,'str':str603460,'is-str':is_DASH_str631335,'bop':bop631848,'is-bop':is_DASH_bop631684,'cif':cif632311,'is-cif':is_DASH_cif632147,'let':let632773,'is-let':is_DASH_let632609,'lam':lam633206,'is-lam':is_DASH_lam633059,'app':app633600,'is-app':is_DASH_app633453,'hole':hole639363,'is-hole':is_DASH_hole639250,'Operator':Operator631858,'plus':plus633990,'is-plus':is_DASH_plus633877,'minus':minus634219,'is-minus':is_DASH_minus634106,'append':append634448,'is-append':is_DASH_append634335,'str-eq':str_DASH_eq634677,'is-str-eq':is_DASH_str_DASH_eq634564,'parse':parse634783}); 
})(); 
})(); }, RUNTIME.makeString(""));
var cs173641415 = RUNTIME.makeObject({'interp-basic':RUNTIME.applyFunc(interp_DASH_basic628624, []),'calculate-locals':RUNTIME.applyFunc(calculate_DASH_locals635999, [])});
RESULT = NAMESPACE.get('nothing');
                EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("identical", identical600304)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("mklist", mklist600452)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("keys", keys600525)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("has-field", has_DASH_field600323)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("num-keys", num_DASH_keys600607)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Eq", Eq600641)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("builtins", builtins600793)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("get-help", get_DASH_help600842)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("set-help", set_DASH_help601092)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("reverse-help", reverse_DASH_help601361)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("take-help", take_DASH_help601568)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("drop-help", drop_DASH_help601819)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("list-to-set", list_DASH_to_DASH_set602062)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("List-mixins551194", List_DASH_mixins551194602176)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551195", data_DASH_shared551195602187)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551196", variant551196602492)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551197", variant551197603485)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("List551333", List551333604816)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("empty_base551336", empty_base551336604827)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("empty551334", empty551334604940)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-empty", is_DASH_empty600930)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("empty", empty600466)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("link_base551339", link_base551339605256)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("link551338", link551338605440)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-link", is_DASH_link601627)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("link", link600471)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("List", List601382)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("range", range605897)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("repeat", repeat606091)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("filter", filter606259)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("partition", partition603689)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("any", any606644)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("all", all606835)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("find", find603712)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map", map607310)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map2", map2607421)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map3", map3607602)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map4", map4607850)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map_n", map_n608166)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map2_n", map2_n608324)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map3_n", map3_n608550)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("map4_n", map4_n608844)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each", each609206)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each2", each2609345)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each3", each3609563)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each4", each4609857)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each_n", each_n610228)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each2_n", each2_n610424)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each3_n", each3_n610696)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("each4_n", each4_n611045)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold", fold602097)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold2", fold2611585)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold3", fold3611768)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold4", fold4612019)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("fold_n", fold_n612338)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("raw-fold", raw_DASH_fold604195)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("index", index612667)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("list", list601511)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Location-mixins551200", Location_DASH_mixins551200613146)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551201", data_DASH_shared551201613157)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551202", variant551202613166)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Location551343", Location551343613420)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("location_base551346", location_base551346613431)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("location551345", location551345613644)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-location", is_DASH_location613169)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("location", location613665)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Location", Location613838)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Error-mixins551204", Error_DASH_mixins551204613849)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551205", data_DASH_shared551205613860)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551206", variant551206613973)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551207", variant551207613997)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551208", variant551208614021)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551209", variant551209614045)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551210", variant551210614069)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551211", variant551211614093)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551212", variant551212614117)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551213", variant551213614141)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551214", variant551214614165)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551215", variant551215614189)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551216", variant551216614213)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Error551350", Error551350614237)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("opaque-error_base551353", opaque_DASH_error_base551353614248)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("opaque-error551352", opaque_DASH_error551352614462)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-opaque-error", is_DASH_opaque_DASH_error614319)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("opaque-error", opaque_DASH_error614483)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-not-found_base551359", field_DASH_not_DASH_found_base551359614734)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-not-found551358", field_DASH_not_DASH_found551358614948)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-field-not-found", is_DASH_field_DASH_not_DASH_found614805)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-not-found", field_DASH_not_DASH_found614969)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-non-string_base551365", field_DASH_non_DASH_string_base551365615220)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-non-string551364", field_DASH_non_DASH_string551364615434)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-field-non-string", is_DASH_field_DASH_non_DASH_string615291)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("field-non-string", field_DASH_non_DASH_string615455)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cases-miss_base551371", cases_DASH_miss_base551371615706)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cases-miss551370", cases_DASH_miss551370615920)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-cases-miss", is_DASH_cases_DASH_miss615777)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cases-miss", cases_DASH_miss615941)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("invalid-case_base551377", invalid_DASH_case_base551377616192)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("invalid-case551376", invalid_DASH_case551376616406)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-invalid-case", is_DASH_invalid_DASH_case616263)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("invalid-case", invalid_DASH_case616427)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("eval-error_base551383", eval_DASH_error_base551383616678)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("eval-error551382", eval_DASH_error551382616892)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-eval-error", is_DASH_eval_DASH_error616749)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("eval-error", eval_DASH_error616913)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("user-contract-failure_base551389", user_DASH_contract_DASH_failure_base551389617164)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("user-contract-failure551388", user_DASH_contract_DASH_failure551388617378)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-user-contract-failure", is_DASH_user_DASH_contract_DASH_failure617235)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("user-contract-failure", user_DASH_contract_DASH_failure617399)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("arity-error_base551395", arity_DASH_error_base551395617650)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("arity-error551394", arity_DASH_error551394617864)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-arity-error", is_DASH_arity_DASH_error617721)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("arity-error", arity_DASH_error617885)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("div-0_base551401", div_DASH_0_base551401618136)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("div-0551400", div_DASH_0551400618350)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-div-0", is_DASH_div_DASH_0618207)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("div-0", div_DASH_0618371)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("type-error_base551407", type_DASH_error_base551407618622)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("type-error551406", type_DASH_error551406618836)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-type-error", is_DASH_type_DASH_error618693)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("type-error", type_DASH_error618857)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("lazy-error_base551413", lazy_DASH_error_base551413619108)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("lazy-error551412", lazy_DASH_error551412619322)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-lazy-error", is_DASH_lazy_DASH_error619179)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("lazy-error", lazy_DASH_error619343)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Error", Error619594)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("make-error", make_DASH_error619605)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error", error601487)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Set-mixins551228", Set_DASH_mixins551228620135)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551229", data_DASH_shared551229620146)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551230", variant551230620155)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Set551417", Set551417620577)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("__set_base551420", __set_base551420620588)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("__set551419", __set551419620744)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-__set", is_DASH___set620635)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("__set", __set602139)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Set", Set620383)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("sets", sets620971)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Option-mixins551232", Option_DASH_mixins551232620990)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551233", data_DASH_shared551233621001)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551234", variant551234621010)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551235", variant551235621049)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Option551422", Option551422621101)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("none_base551425", none_base551425621112)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("none551423", none551423621223)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-none", is_DASH_none606989)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("none", none603126)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("some_base551428", some_base551428621355)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("some551427", some551427621510)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-some", is_DASH_some606798)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("some", some607233)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("Option", Option607195)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("option", option621679)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResult-mixins551238", CheckResult_DASH_mixins551238621713)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551239", data_DASH_shared551239621724)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551240", variant551240621733)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551241", variant551241621740)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551242", variant551242621747)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResult551430", CheckResult551430621754)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("success_base551433", success_base551433621765)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("success551432", success551432621950)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-success", is_DASH_success621824)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("success", success621971)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("failure_base551438", failure_base551438622159)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("failure551437", failure551437622373)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-failure", is_DASH_failure622230)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("failure", failure622394)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("err_base551444", err_base551444622621)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("err551443", err551443622835)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-err", is_DASH_err622692)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("err", err622856)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResult", CheckResult623057)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("current-results", current_DASH_results623068)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("add-result", add_DASH_result623075)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-is", check_DASH_is623158)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-raises", check_DASH_raises623559)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-true", check_DASH_true623969)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-false", check_DASH_false624017)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-equals", check_DASH_equals623987)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-pred", check_DASH_pred624329)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-exn", check_DASH_exn624596)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResultList-mixins551253", CheckResultList_DASH_mixins551253624860)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("data-shared551254", data_DASH_shared551254624871)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551255", variant551255624880)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("variant551256", variant551256624887)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResultList551448", CheckResultList551448624894)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("normal-result_base551451", normal_DASH_result_base551451624905)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("normal-result551450", normal_DASH_result551450625119)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-normal-result", is_DASH_normal_DASH_result624976)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("normal-result", normal_DASH_result625140)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error-result_base551457", error_DASH_result_base551457625367)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error-result551456", error_DASH_result551456625610)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("is-error-result", is_DASH_error_DASH_result625450)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("error-result", error_DASH_result625631)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("CheckResultList", CheckResultList625871)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("all-results", all_DASH_results625882)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("run-checks", run_DASH_checks625915)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("clear-results", clear_DASH_results626453)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("get-results", get_DASH_results626506)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("format-check-results", format_DASH_check_DASH_results626520)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("check-results-summary", check_DASH_results_DASH_summary626582)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("checkers", checkers628495)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("interp-basic", interp_DASH_basic628624)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("calculate-locals", calculate_DASH_locals635999)
EXPORT_NAMESPACE = EXPORT_NAMESPACE.set("cs173", cs173641415)

              })();
              return RUNTIME.makeNormalResult(RESULT, EXPORT_NAMESPACE);
            } catch(e) {
              return RUNTIME.makeFailResult(e);
            }
          })
if(typeof exports !== 'undefined') {
  exports.lib = LIB;
}
