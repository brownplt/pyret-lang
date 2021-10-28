var _runtime = require(".\/runtime.js");
var $underscore_import18 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $underscore_import18);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr");
var nothing16 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_Pick1 = { "$methods": {} };
var variantBase_pick$none2 = _runtime["$createVariant"](sharedBase_Pick1, { "$methods": {} }, {
    "$data": sharedBase_Pick1,
    "$name": "pick-none",
    "$fieldNames": null
});
var variantBase_pick$some3 = _runtime["$createVariant"](sharedBase_Pick1, { "$methods": {} }, {
    "$data": sharedBase_Pick1,
    "$name": "pick-some",
    "$fieldNames": [
        "elt",
        "rest"
    ]
});
var Pick10 = {
    "pick-none": variantBase_pick$none2,
    "pick-some": function pick$some6(elt4, rest5) {
        return _runtime["$makeDataValue"](variantBase_pick$some3, {
            "elt": elt4,
            "rest": rest5
        });
    },
    "is-pick-none": function is$pick$none7(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_pick$none2["$variant"];
    },
    "is-pick-some": function is$pick$some8(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_pick$some3["$variant"];
    },
    "is-Pick": function is$Pick9(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_Pick1;
    }
};
var is$Pick11 = Pick10["is-Pick"];
var is$pick$none12 = Pick10["is-pick-none"];
var pick$none13 = Pick10["pick-none"];
var is$pick$some14 = Pick10["is-pick-some"];
var pick$some15 = Pick10["pick-some"];
var $answer17 = _runtime["trace-value"](["dummy location"], nothing16);
return module["exports"] = {
    "is-Pick": is$Pick11,
    "is-pick-none": is$pick$none12,
    "pick-none": pick$none13,
    "is-pick-some": is$pick$some14,
    "pick-some": pick$some15,
    "$answer": $answer17,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr"),
    "$locations": [
        {
            "name": "is-Pick",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr",
                6,
                0,
                56,
                9,
                3,
                125
            ]
        },
        {
            "name": "is-pick-none",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr",
                7,
                2,
                75,
                7,
                13,
                86
            ]
        },
        {
            "name": "pick-none",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr",
                7,
                2,
                75,
                7,
                13,
                86
            ]
        },
        {
            "name": "is-pick-some",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr",
                8,
                2,
                89,
                8,
                34,
                121
            ]
        },
        {
            "name": "pick-some",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/pick.arr",
                8,
                2,
                89,
                8,
                34,
                121
            ]
        }
    ]
};