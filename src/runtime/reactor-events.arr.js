var _runtime = require("./runtime.js");
var _nothing = undefined;
var $time$tick12 = {"names":false};
var $mouse13 = {"names":["x","y","kind"]};
var $keypress14 = {"names":["key"]};
var $constructorTMP15 = {"$brand":$time$tick12,
"$tag":0};
var Event1 = {"time-tick":$constructorTMP15,
"mouse":function mouse(x16,y17,kind18) {
var $constructorTMP19 = {"$brand":$mouse13,
"$tag":1,
"kind":kind18,
"x":x16,
"y":y17};
return $constructorTMP19;
},
"keypress":function keypress(key21) {
var $constructorTMP22 = {"$brand":$keypress14,
"$tag":2,
"key":key21};
return $constructorTMP22;
},
"is-time-tick":function time$tick(val) {
return val.$brand === $time$tick12;
},
"is-mouse":function mouse(val) {
return val.$brand === $mouse13;
},
"is-keypress":function keypress(val) {
return val.$brand === $keypress14;
}};
var is$Event8 = Event1["Event"];
var is$time$tick7 = Event1["is-time-tick"];
var time$tick6 = Event1["time-tick"];
var is$mouse5 = Event1["is-mouse"];
var mouse4 = Event1["mouse"];
var is$keypress3 = Event1["is-keypress"];
var keypress2 = Event1["keypress"];
return module["exports"] = {"time-tick":time$tick6,
"is-time-tick":is$time$tick7,
"is-Event":is$Event8,
"is-keypress":is$keypress3,
"mouse":mouse4,
"keypress":keypress2,
"is-mouse":is$mouse5,
"$checks":_runtime["$checkResults"](),
"$traces":_runtime["$getTraces"](),
"$locations":[{"name":"time-tick",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",7,2,61,7,13,72]},
{"name":"is-time-tick",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",7,2,61,7,13,72]},
{"name":"is-Event",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",6,0,47,10,3,156]},
{"name":"is-keypress",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",9,2,127,9,27,152]},
{"name":"mouse",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",8,2,75,8,51,124]},
{"name":"keypress",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",9,2,127,9,27,152]},
{"name":"is-mouse",
"srcloc":["file:///home/michael/projects/pyret-lang/src/runtime-arr/reactor-events.arr",8,2,75,8,51,124]}]};
