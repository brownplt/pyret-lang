var _runtime = require(".\/runtime.js");
var $underscore_import31 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $underscore_import31);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr");
var nothing29 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_Option17 = _runtime["$setupMethodGetters"]({
    "$methods": {
        "or-else": function getWrapper_or$else7() {
            var self1 = this;
            return _runtime["$installMethod"](self1, "or-else", function lam_or$else6(v5) {
                var cases2 = self1;
                var $ans3;
                switch (cases2["$name"]) {
                case "some":
                    var value4 = cases2[cases2["$fieldNames"][0]];
                    $ans3 = value4;
                    break;
                case "none":
                    $ans3 = v5;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                        16,
                        4,
                        296,
                        19,
                        7,
                        370
                    ], cases2);
                    $ans3 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                        16,
                        4,
                        296,
                        19,
                        7,
                        370
                    ], cases2);
                }
                return $ans3;
            });
        },
        "and-then": function getWrapper_and$then16() {
            var self8 = this;
            return _runtime["$installMethod"](self8, "and-then", function lam_and$then15(f12) {
                var cases9 = self8;
                var $ans10;
                switch (cases9["$name"]) {
                case "some":
                    var value13 = cases9[cases9["$fieldNames"][0]];
                    $ans10 = (some11 !== undefined ? some11 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                        24,
                        23,
                        495,
                        24,
                        27,
                        499
                    ], "Uninitialized letrec identifier"))(f12(value13));
                    break;
                case "none":
                    $ans10 = none14 !== undefined ? none14 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                        25,
                        16,
                        526,
                        25,
                        20,
                        530
                    ], "Uninitialized letrec identifier");
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                        23,
                        4,
                        452,
                        26,
                        7,
                        538
                    ], cases9);
                    $ans10 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                        23,
                        4,
                        452,
                        26,
                        7,
                        538
                    ], cases9);
                }
                return $ans10;
            });
        }
    }
});
var variantBase_some18 = _runtime["$createVariant"](sharedBase_Option17, { "$methods": {} }, {
    "$data": sharedBase_Option17,
    "$name": "some",
    "$fieldNames": ["value"]
});
var variantBase_none19 = _runtime["$createVariant"](sharedBase_Option17, { "$methods": {} }, {
    "$data": sharedBase_Option17,
    "$name": "none",
    "$fieldNames": null
});
var Option25 = {
    "some": function some21(value20) {
        return _runtime["$makeDataValue"](variantBase_some18, { "value": value20 });
    },
    "none": variantBase_none19,
    "is-some": function is$some22(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_some18["$variant"];
    },
    "is-none": function is$none23(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_none19["$variant"];
    },
    "is-Option": function is$Option24(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_Option17;
    }
};
var is$Option26 = Option25["is-Option"];
var is$some27 = Option25["is-some"];
var some11 = Option25["some"];
var is$none28 = Option25["is-none"];
var none14 = Option25["none"];
var $answer30 = _runtime["trace-value"](["dummy location"], nothing29);
return module["exports"] = {
    "is-Option": is$Option26,
    "is-some": is$some27,
    "some": some11,
    "is-none": is$none28,
    "none": none14,
    "$answer": $answer30,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr"),
    "$locations": [
        {
            "name": "is-Option",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                11,
                0,
                188,
                35,
                3,
                711
            ]
        },
        {
            "name": "is-some",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                12,
                2,
                206,
                12,
                20,
                224
            ]
        },
        {
            "name": "some",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                12,
                2,
                206,
                12,
                20,
                224
            ]
        },
        {
            "name": "is-none",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                13,
                2,
                227,
                13,
                8,
                233
            ]
        },
        {
            "name": "none",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/option.arr",
                13,
                2,
                227,
                13,
                8,
                233
            ]
        }
    ]
};