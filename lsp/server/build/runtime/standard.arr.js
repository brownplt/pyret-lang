var _runtime = require(".\/runtime.js");
var G3 = require(".\/global.arr.js");
_runtime["addModule"]("builtin:\/\/global", G3);
var L4 = require(".\/lists.arr.js");
_runtime["addModule"]("builtin:\/\/lists", L4);
var S5 = require(".\/sets.arr.js");
_runtime["addModule"]("builtin:\/\/sets", S5);
var $G7 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $G7);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/standard.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/standard.arr");
var nothing1 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var $answer2 = _runtime["trace-value"](["dummy location"], nothing1);
return module["exports"] = {
    "$answer": $answer2,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/standard.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/standard.arr"),
    "$locations": []
};