var _runtime = require(".\/runtime.js");
var $underscore_import25 = require(".\/runtime-global.arr.js");
_runtime["addModule"]("builtin:\/\/runtime-global", $underscore_import25);
var $G27 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $G27);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr");
var nothing23 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_Event1 = { "$methods": {} };
var variantBase_time$tick2 = _runtime["$createVariant"](sharedBase_Event1, { "$methods": {} }, {
    "$data": sharedBase_Event1,
    "$name": "time-tick",
    "$fieldNames": null
});
var variantBase_mouse3 = _runtime["$createVariant"](sharedBase_Event1, { "$methods": {} }, {
    "$data": sharedBase_Event1,
    "$name": "mouse",
    "$fieldNames": [
        "x",
        "y",
        "kind"
    ]
});
var variantBase_keypress4 = _runtime["$createVariant"](sharedBase_Event1, { "$methods": {} }, {
    "$data": sharedBase_Event1,
    "$name": "keypress",
    "$fieldNames": ["key"]
});
var Event15 = {
    "time-tick": variantBase_time$tick2,
    "mouse": function mouse8(x5, y6, kind7) {
        return _runtime["$makeDataValue"](variantBase_mouse3, {
            "x": x5,
            "y": y6,
            "kind": kind7
        });
    },
    "keypress": function keypress10(key9) {
        return _runtime["$makeDataValue"](variantBase_keypress4, { "key": key9 });
    },
    "is-time-tick": function is$time$tick11(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_time$tick2["$variant"];
    },
    "is-mouse": function is$mouse12(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_mouse3["$variant"];
    },
    "is-keypress": function is$keypress13(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_keypress4["$variant"];
    },
    "is-Event": function is$Event14(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_Event1;
    }
};
var is$Event16 = Event15["is-Event"];
var is$time$tick17 = Event15["is-time-tick"];
var time$tick18 = Event15["time-tick"];
var is$mouse19 = Event15["is-mouse"];
var mouse20 = Event15["mouse"];
var is$keypress21 = Event15["is-keypress"];
var keypress22 = Event15["keypress"];
var $answer24 = _runtime["trace-value"](["dummy location"], nothing23);
return module["exports"] = {
    "is-Event": is$Event16,
    "is-time-tick": is$time$tick17,
    "time-tick": time$tick18,
    "is-mouse": is$mouse19,
    "mouse": mouse20,
    "is-keypress": is$keypress21,
    "keypress": keypress22,
    "$answer": $answer24,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr"),
    "$locations": [
        {
            "name": "is-Event",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                6,
                0,
                55,
                10,
                3,
                164
            ]
        },
        {
            "name": "is-time-tick",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                7,
                2,
                69,
                7,
                13,
                80
            ]
        },
        {
            "name": "time-tick",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                7,
                2,
                69,
                7,
                13,
                80
            ]
        },
        {
            "name": "is-mouse",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                8,
                2,
                83,
                8,
                51,
                132
            ]
        },
        {
            "name": "mouse",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                8,
                2,
                83,
                8,
                51,
                132
            ]
        },
        {
            "name": "is-keypress",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                9,
                2,
                135,
                9,
                27,
                160
            ]
        },
        {
            "name": "keypress",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/reactor-events.arr",
                9,
                2,
                135,
                9,
                27,
                160
            ]
        }
    ]
};