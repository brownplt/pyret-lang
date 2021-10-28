var _runtime = require(".\/runtime.js");
var C3 = require(".\/color.arr.js");
_runtime["addModule"]("builtin:\/\/color", C3);
var $G5 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $G5);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/image-structs.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/image-structs.arr");
var nothing1 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var $answer2 = _runtime["trace-value"](["dummy location"], nothing1);
return module["exports"] = {
    "$answer": $answer2,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/image-structs.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/image-structs.arr"),
    "$locations": []
};