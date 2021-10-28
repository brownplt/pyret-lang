var _runtime = require(".\/runtime.js");
var PT3 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", PT3);
var RA4 = require(".\/raw-array.arr.js");
_runtime["addModule"]("builtin:\/\/raw-array", RA4);
var lists5 = require(".\/lists.arr.js");
_runtime["addModule"]("builtin:\/\/lists", lists5);
var option6 = require(".\/option.arr.js");
_runtime["addModule"]("builtin:\/\/option", option6);
var sets7 = require(".\/sets.arr.js");
_runtime["addModule"]("builtin:\/\/sets", sets7);
var G8 = require(".\/global.arr.js");
_runtime["addModule"]("builtin:\/\/global", G8);
var S9 = require(".\/string.arr.js");
_runtime["addModule"]("builtin:\/\/string", S9);
var N10 = require(".\/number.arr.js");
_runtime["addModule"]("builtin:\/\/number", N10);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/cpo.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/cpo.arr");
var nothing1 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var $answer2 = _runtime["trace-value"](["dummy location"], nothing1);
return module["exports"] = {
    "$answer": $answer2,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/cpo.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-preludes\/cpo.arr"),
    "$locations": []
};