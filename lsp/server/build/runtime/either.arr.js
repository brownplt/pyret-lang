var _runtime = require(".\/runtime.js");
var $underscore_import19 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $underscore_import19);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr");
var nothing17 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_Either1 = { "$methods": {} };
var variantBase_left2 = _runtime["$createVariant"](sharedBase_Either1, { "$methods": {} }, {
    "$data": sharedBase_Either1,
    "$name": "left",
    "$fieldNames": ["v"]
});
var variantBase_right3 = _runtime["$createVariant"](sharedBase_Either1, { "$methods": {} }, {
    "$data": sharedBase_Either1,
    "$name": "right",
    "$fieldNames": ["v"]
});
var Either11 = {
    "left": function left5(v4) {
        return _runtime["$makeDataValue"](variantBase_left2, { "v": v4 });
    },
    "right": function right7(v6) {
        return _runtime["$makeDataValue"](variantBase_right3, { "v": v6 });
    },
    "is-left": function is$left8(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_left2["$variant"];
    },
    "is-right": function is$right9(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_right3["$variant"];
    },
    "is-Either": function is$Either10(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_Either1;
    }
};
var is$Either12 = Either11["is-Either"];
var is$left13 = Either11["is-left"];
var left14 = Either11["left"];
var is$right15 = Either11["is-right"];
var right16 = Either11["right"];
var $answer18 = _runtime["trace-value"](["dummy location"], nothing17);
return module["exports"] = {
    "is-Either": is$Either12,
    "is-left": is$left13,
    "left": left14,
    "is-right": is$right15,
    "right": right16,
    "$answer": $answer18,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr"),
    "$locations": [
        {
            "name": "is-Either",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr",
                11,
                0,
                188,
                14,
                3,
                245
            ]
        },
        {
            "name": "is-left",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr",
                12,
                2,
                209,
                12,
                16,
                223
            ]
        },
        {
            "name": "left",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr",
                12,
                2,
                209,
                12,
                16,
                223
            ]
        },
        {
            "name": "is-right",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr",
                13,
                2,
                226,
                13,
                17,
                241
            ]
        },
        {
            "name": "right",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/either.arr",
                13,
                2,
                226,
                13,
                17,
                241
            ]
        }
    ]
};