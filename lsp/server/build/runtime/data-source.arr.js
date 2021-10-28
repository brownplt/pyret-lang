var _runtime = require(".\/runtime.js");
var G183 = require(".\/runtime-global.arr.js");
_runtime["addModule"]("builtin:\/\/runtime-global", G183);
var O184 = require(".\/option.arr.js");
_runtime["addModule"]("builtin:\/\/option", O184);
var S185 = require(".\/string.arr.js");
_runtime["addModule"]("builtin:\/\/string", S185);
var N186 = require(".\/number.arr.js");
_runtime["addModule"]("builtin:\/\/number", N186);
var $G188 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $G188);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr");
var none47 = _runtime["getModuleValue"]("builtin:\/\/option", "none");
var some48 = _runtime["getModuleValue"]("builtin:\/\/option", "some");
var num$to$string59 = _runtime["getModuleValue"]("builtin:\/\/number", "num-to-string");
var torepr61 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "js-to-string");
var string$to$number74 = _runtime["getModuleValue"]("builtin:\/\/string", "string-to-number");
var raise78 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "raise");
var string$tolower98 = _runtime["getModuleValue"]("builtin:\/\/string", "string-to-lower");
var nothing181 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_CellContent1 = { "$methods": {} };
var variantBase_c$empty2 = _runtime["$createVariant"](sharedBase_CellContent1, { "$methods": {} }, {
    "$data": sharedBase_CellContent1,
    "$name": "c-empty",
    "$fieldNames": null
});
var variantBase_c$str3 = _runtime["$createVariant"](sharedBase_CellContent1, { "$methods": {} }, {
    "$data": sharedBase_CellContent1,
    "$name": "c-str",
    "$fieldNames": ["s"]
});
var variantBase_c$num4 = _runtime["$createVariant"](sharedBase_CellContent1, { "$methods": {} }, {
    "$data": sharedBase_CellContent1,
    "$name": "c-num",
    "$fieldNames": ["n"]
});
var variantBase_c$bool5 = _runtime["$createVariant"](sharedBase_CellContent1, { "$methods": {} }, {
    "$data": sharedBase_CellContent1,
    "$name": "c-bool",
    "$fieldNames": ["b"]
});
var variantBase_c$custom6 = _runtime["$createVariant"](sharedBase_CellContent1, { "$methods": {} }, {
    "$data": sharedBase_CellContent1,
    "$name": "c-custom",
    "$fieldNames": ["datum"]
});
var CellContent21 = {
    "c-empty": variantBase_c$empty2,
    "c-str": function c$str8(s7) {
        return _runtime["$makeDataValue"](variantBase_c$str3, { "s": s7 });
    },
    "c-num": function c$num10(n9) {
        return _runtime["$makeDataValue"](variantBase_c$num4, { "n": n9 });
    },
    "c-bool": function c$bool12(b11) {
        return _runtime["$makeDataValue"](variantBase_c$bool5, { "b": b11 });
    },
    "c-custom": function c$custom14(datum13) {
        return _runtime["$makeDataValue"](variantBase_c$custom6, { "datum": datum13 });
    },
    "is-c-empty": function is$c$empty15(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_c$empty2["$variant"];
    },
    "is-c-str": function is$c$str16(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_c$str3["$variant"];
    },
    "is-c-num": function is$c$num17(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_c$num4["$variant"];
    },
    "is-c-bool": function is$c$bool18(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_c$bool5["$variant"];
    },
    "is-c-custom": function is$c$custom19(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_c$custom6["$variant"];
    },
    "is-CellContent": function is$CellContent20(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_CellContent1;
    }
};
var is$CellContent22 = CellContent21["is-CellContent"];
var is$c$empty23 = CellContent21["is-c-empty"];
var c$empty24 = CellContent21["c-empty"];
var is$c$str25 = CellContent21["is-c-str"];
var c$str26 = CellContent21["c-str"];
var is$c$num27 = CellContent21["is-c-num"];
var c$num28 = CellContent21["c-num"];
var is$c$bool29 = CellContent21["is-c-bool"];
var c$bool30 = CellContent21["c-bool"];
var is$c$custom31 = CellContent21["is-c-custom"];
var c$custom32 = CellContent21["c-custom"];
var sharedBase_DataSourceLoaderOption33 = { "$methods": {} };
var variantBase_sanitize$col34 = _runtime["$createVariant"](sharedBase_DataSourceLoaderOption33, { "$methods": {} }, {
    "$data": sharedBase_DataSourceLoaderOption33,
    "$name": "sanitize-col",
    "$fieldNames": [
        "col",
        "sanitizer"
    ]
});
var DataSourceLoaderOption40 = {
    "sanitize-col": function sanitize$col37(col35, sanitizer36) {
        return _runtime["$makeDataValue"](variantBase_sanitize$col34, {
            "col": col35,
            "sanitizer": sanitizer36
        });
    },
    "is-sanitize-col": function is$sanitize$col38(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_sanitize$col34["$variant"];
    },
    "is-DataSourceLoaderOption": function is$DataSourceLoaderOption39(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_DataSourceLoaderOption33;
    }
};
var is$DataSourceLoaderOption41 = DataSourceLoaderOption40["is-DataSourceLoaderOption"];
var is$sanitize$col42 = DataSourceLoaderOption40["is-sanitize-col"];
var sanitize$col43 = DataSourceLoaderOption40["sanitize-col"];
var option$sanitizer54 = function lam_option$sanitizer53(val$sanitizer49) {
    return function lam_52(x44, col50, row51) {
        var cases45 = x44;
        var $ans46;
        switch (cases45["$name"]) {
        case "c-empty":
            $ans46 = none47;
            break;
        default:
            $ans46 = some48(val$sanitizer49(x44, col50, row51));
        }
        return $ans46;
    };
};
var string$sanitizer67 = function lam_string$sanitizer66(x55, col64, row65) {
    var cases56 = x55;
    var $ans57;
    switch (cases56["$name"]) {
    case "c-empty":
        $ans57 = "";
        break;
    case "c-str":
        var s58 = cases56[cases56["$fieldNames"][0]];
        $ans57 = s58;
        break;
    case "c-num":
        var n60 = cases56[cases56["$fieldNames"][0]];
        $ans57 = num$to$string59(n60);
        break;
    case "c-bool":
        var b62 = cases56[cases56["$fieldNames"][0]];
        $ans57 = torepr61(b62);
        break;
    case "c-custom":
        var datum63 = cases56[cases56["$fieldNames"][0]];
        $ans57 = torepr61(datum63);
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            64,
            2,
            1437,
            70,
            5,
            1607
        ], cases56);
        $ans57 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            64,
            2,
            1437,
            70,
            5,
            1607
        ], cases56);
    }
    return $ans57;
};
var num$sanitizer85 = function lam_num$sanitizer84(x71, col68, row69) {
    var loc70 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col68), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row69), _runtime["$errCallbacks"]);
    var cases72 = x71;
    var $ans73;
    switch (cases72["$name"]) {
    case "c-str":
        var s75 = cases72[cases72["$fieldNames"][0]];
        var cases76 = string$to$number74(s75);
        var $ans77;
        switch (cases76["$name"]) {
        case "none":
            $ans77 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the string \"", s75, _runtime["$errCallbacks"]), "\" at ", _runtime["$errCallbacks"]), loc70, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
            break;
        case "some":
            var n79 = cases76[cases76["$fieldNames"][0]];
            $ans77 = n79;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                77,
                6,
                1815,
                81,
                9,
                1989
            ], cases76);
            $ans77 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                77,
                6,
                1815,
                81,
                9,
                1989
            ], cases76);
        }
        $ans73 = $ans77;
        break;
    case "c-num":
        var n80 = cases72[cases72["$fieldNames"][0]];
        $ans73 = n80;
        break;
    case "c-bool":
        var b82 = cases72[cases72["$fieldNames"][0]];
        if (b82) {
            $ans81 = 1;
        } else {
            $ans81 = 0;
        }
        $ans73 = $ans81;
        break;
    case "c-custom":
        var datum83 = cases72[cases72["$fieldNames"][0]];
        $ans73 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the datum ", torepr61(datum83), _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc70, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
        break;
    case "c-empty":
        $ans73 = raise78(_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the empty cell at ", loc70, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            75,
            2,
            1769,
            88,
            5,
            2267
        ], cases72);
        $ans73 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            75,
            2,
            1769,
            88,
            5,
            2267
        ], cases72);
    }
    return $ans73;
};
var bool$sanitizer102 = function lam_bool$sanitizer101(x89, col86, row87) {
    var loc88 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col86), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row87), _runtime["$errCallbacks"]);
    var cases90 = x89;
    var $ans91;
    switch (cases90["$name"]) {
    case "c-bool":
        var b92 = cases90[cases90["$fieldNames"][0]];
        $ans91 = b92;
        break;
    case "c-num":
        var n93 = cases90[cases90["$fieldNames"][0]];
        if (_runtime["equal-always"](n93, 0)) {
            $ans95 = false;
        } else {
            if (_runtime["equal-always"](n93, 1)) {
                $ans94 = true;
            } else {
                $ans94 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the number ", num$to$string59(n93), _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc88, _runtime["$errCallbacks"]), " as a boolean", _runtime["$errCallbacks"]));
            }
            $ans95 = $ans94;
        }
        $ans91 = $ans95;
        break;
    case "c-str":
        var s96 = cases90[cases90["$fieldNames"][0]];
        if (_runtime["equal-always"](string$tolower98(s96), "true")) {
            $ans99 = true;
        } else {
            if (_runtime["equal-always"](string$tolower98(s96), "false")) {
                $ans97 = false;
            } else {
                $ans97 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the string \"", s96, _runtime["$errCallbacks"]), "\" at ", _runtime["$errCallbacks"]), loc88, _runtime["$errCallbacks"]), " as a boolean", _runtime["$errCallbacks"]));
            }
            $ans99 = $ans97;
        }
        $ans91 = $ans99;
        break;
    case "c-custom":
        var datum100 = cases90[cases90["$fieldNames"][0]];
        $ans91 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the datum ", torepr61(datum100), _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc88, _runtime["$errCallbacks"]), " as a boolean", _runtime["$errCallbacks"]));
        break;
    case "c-empty":
        $ans91 = raise78(_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the empty cell at ", loc88, _runtime["$errCallbacks"]), " as a boolean", _runtime["$errCallbacks"]));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            93,
            2,
            2431,
            113,
            5,
            3164
        ], cases90);
        $ans91 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            93,
            2,
            2431,
            113,
            5,
            3164
        ], cases90);
    }
    return $ans91;
};
var strict$num$sanitizer117 = function lam_strict$num$sanitizer116(x106, col103, row104) {
    var loc105 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col103), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row104), _runtime["$errCallbacks"]);
    var cases107 = x106;
    var $ans108;
    switch (cases107["$name"]) {
    case "c-str":
        var s109 = cases107[cases107["$fieldNames"][0]];
        var cases110 = string$to$number74(s109);
        var $ans111;
        switch (cases110["$name"]) {
        case "none":
            $ans111 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the string \"", s109, _runtime["$errCallbacks"]), "\" at ", _runtime["$errCallbacks"]), loc105, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
            break;
        case "some":
            var n112 = cases110[cases110["$fieldNames"][0]];
            $ans111 = n112;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                120,
                6,
                3379,
                124,
                9,
                3553
            ], cases110);
            $ans111 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                120,
                6,
                3379,
                124,
                9,
                3553
            ], cases110);
        }
        $ans108 = $ans111;
        break;
    case "c-num":
        var n113 = cases107[cases107["$fieldNames"][0]];
        $ans108 = n113;
        break;
    case "c-bool":
        var b114 = cases107[cases107["$fieldNames"][0]];
        $ans108 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the boolean ", torepr61(b114), _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc105, _runtime["$errCallbacks"]), " as a number in strict mode.", _runtime["$errCallbacks"]));
        break;
    case "c-custom":
        var datum115 = cases107[cases107["$fieldNames"][0]];
        $ans108 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the datum ", torepr61(datum115), _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc105, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
        break;
    case "c-empty":
        $ans108 = raise78(_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the empty cell at ", loc105, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            118,
            2,
            3333,
            132,
            5,
            3919
        ], cases107);
        $ans108 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
            118,
            2,
            3333,
            132,
            5,
            3919
        ], cases107);
    }
    return $ans108;
};
var strings$only133 = function lam_strings$only132(x121, col118, row119) {
    var loc120 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col118), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row119), _runtime["$errCallbacks"]);
    var cases122 = x121;
    var $ans123;
    switch (cases122["$name"]) {
    case "c-str":
        var s124 = cases122[cases122["$fieldNames"][0]];
        $ans123 = s124;
        break;
    default:
        var cases125 = x121;
        var $ans126;
        switch (cases125["$name"]) {
        case "c-num":
            var n127 = cases125[cases125["$fieldNames"][0]];
            $ans126 = _runtime["_plus"]("the number ", num$to$string59(n127), _runtime["$errCallbacks"]);
            break;
        case "c-bool":
            var b128 = cases125[cases125["$fieldNames"][0]];
            $ans126 = _runtime["_plus"]("the boolean ", torepr61(b128), _runtime["$errCallbacks"]);
            break;
        case "c-custom":
            var datum129 = cases125[cases125["$fieldNames"][0]];
            $ans126 = _runtime["_plus"]("the datum ", torepr61(datum129), _runtime["$errCallbacks"]);
            break;
        case "c-empty":
            $ans126 = "the empty cell";
            break;
        case "c-str":
            var s130 = cases125[cases125["$fieldNames"][0]];
            $ans126 = raise78("unreachable");
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                140,
                15,
                4151,
                146,
                9,
                4426
            ], cases125);
            $ans126 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                140,
                15,
                4151,
                146,
                9,
                4426
            ], cases125);
        }
        var as$str131 = $ans126;
        $ans123 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize ", as$str131, _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc120, _runtime["$errCallbacks"]), " as a string", _runtime["$errCallbacks"]));
    }
    return $ans123;
};
var numbers$only149 = function lam_numbers$only148(x137, col134, row135) {
    var loc136 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col134), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row135), _runtime["$errCallbacks"]);
    var cases138 = x137;
    var $ans139;
    switch (cases138["$name"]) {
    case "c-num":
        var n140 = cases138[cases138["$fieldNames"][0]];
        $ans139 = n140;
        break;
    default:
        var cases141 = x137;
        var $ans142;
        switch (cases141["$name"]) {
        case "c-str":
            var s143 = cases141[cases141["$fieldNames"][0]];
            $ans142 = _runtime["_plus"]("the string ", torepr61(s143), _runtime["$errCallbacks"]);
            break;
        case "c-bool":
            var b144 = cases141[cases141["$fieldNames"][0]];
            $ans142 = _runtime["_plus"]("the boolean ", torepr61(b144), _runtime["$errCallbacks"]);
            break;
        case "c-custom":
            var datum145 = cases141[cases141["$fieldNames"][0]];
            $ans142 = _runtime["_plus"]("the datum ", torepr61(datum145), _runtime["$errCallbacks"]);
            break;
        case "c-empty":
            $ans142 = "an empty cell";
            break;
        case "c-num":
            var n146 = cases141[cases141["$fieldNames"][0]];
            $ans142 = raise78("unreachable");
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                157,
                15,
                4747,
                163,
                9,
                5014
            ], cases141);
            $ans142 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                157,
                15,
                4747,
                163,
                9,
                5014
            ], cases141);
        }
        var as$str147 = $ans142;
        $ans139 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize ", as$str147, _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc136, _runtime["$errCallbacks"]), " as a number", _runtime["$errCallbacks"]));
    }
    return $ans139;
};
var booleans$only165 = function lam_booleans$only164(x153, col150, row151) {
    var loc152 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col150), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row151), _runtime["$errCallbacks"]);
    var cases154 = x153;
    var $ans155;
    switch (cases154["$name"]) {
    case "c-bool":
        var b156 = cases154[cases154["$fieldNames"][0]];
        $ans155 = b156;
        break;
    default:
        var cases157 = x153;
        var $ans158;
        switch (cases157["$name"]) {
        case "c-num":
            var n159 = cases157[cases157["$fieldNames"][0]];
            $ans158 = _runtime["_plus"]("the number ", num$to$string59(n159), _runtime["$errCallbacks"]);
            break;
        case "c-str":
            var s160 = cases157[cases157["$fieldNames"][0]];
            $ans158 = _runtime["_plus"]("the string ", torepr61(s160), _runtime["$errCallbacks"]);
            break;
        case "c-custom":
            var datum161 = cases157[cases157["$fieldNames"][0]];
            $ans158 = _runtime["_plus"]("the datum ", torepr61(datum161), _runtime["$errCallbacks"]);
            break;
        case "c-empty":
            $ans158 = "an empty cell";
            break;
        case "c-bool":
            var b162 = cases157[cases157["$fieldNames"][0]];
            $ans158 = raise78("unreachable");
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                174,
                15,
                5338,
                180,
                9,
                5611
            ], cases157);
            $ans158 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                174,
                15,
                5338,
                180,
                9,
                5611
            ], cases157);
        }
        var as$str163 = $ans158;
        $ans155 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize ", as$str163, _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc152, _runtime["$errCallbacks"]), " as a boolean", _runtime["$errCallbacks"]));
    }
    return $ans155;
};
var empty$only180 = function lam_empty$only179(x169, col166, row167) {
    var loc168 = _runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("column ", num$to$string59(col166), _runtime["$errCallbacks"]), ", row ", _runtime["$errCallbacks"]), num$to$string59(row167), _runtime["$errCallbacks"]);
    var cases170 = x169;
    var $ans171;
    switch (cases170["$name"]) {
    case "c-empty":
        $ans171 = none47;
        break;
    default:
        var cases172 = x169;
        var $ans173;
        switch (cases172["$name"]) {
        case "c-num":
            var n174 = cases172[cases172["$fieldNames"][0]];
            $ans173 = _runtime["_plus"]("number ", num$to$string59(n174), _runtime["$errCallbacks"]);
            break;
        case "c-str":
            var s175 = cases172[cases172["$fieldNames"][0]];
            $ans173 = _runtime["_plus"]("string ", torepr61(s175), _runtime["$errCallbacks"]);
            break;
        case "c-bool":
            var b176 = cases172[cases172["$fieldNames"][0]];
            $ans173 = _runtime["_plus"]("boolean ", torepr61(b176), _runtime["$errCallbacks"]);
            break;
        case "c-custom":
            var datum177 = cases172[cases172["$fieldNames"][0]];
            $ans173 = _runtime["_plus"]("datum ", torepr61(datum177), _runtime["$errCallbacks"]);
            break;
        case "c-empty":
            $ans173 = raise78("unreachable");
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                191,
                15,
                5936,
                197,
                9,
                6204
            ], cases172);
            $ans173 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                191,
                15,
                5936,
                197,
                9,
                6204
            ], cases172);
        }
        var as$str178 = $ans173;
        $ans171 = raise78(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("Cannot sanitize the ", as$str178, _runtime["$errCallbacks"]), " at ", _runtime["$errCallbacks"]), loc168, _runtime["$errCallbacks"]), " as an empty cell", _runtime["$errCallbacks"]));
    }
    return $ans171;
};
var $answer182 = _runtime["trace-value"](["dummy location"], nothing181);
return module["exports"] = {
    "is-c-empty": is$c$empty23,
    "num-sanitizer": num$sanitizer85,
    "c-empty": c$empty24,
    "is-c-str": is$c$str25,
    "bool-sanitizer": bool$sanitizer102,
    "c-str": c$str26,
    "empty-only": empty$only180,
    "string-sanitizer": string$sanitizer67,
    "booleans-only": booleans$only165,
    "numbers-only": numbers$only149,
    "is-DataSourceLoaderOption": is$DataSourceLoaderOption41,
    "option-sanitizer": option$sanitizer54,
    "is-CellContent": is$CellContent22,
    "c-bool": c$bool30,
    "is-c-bool": is$c$bool29,
    "strings-only": strings$only133,
    "is-c-num": is$c$num27,
    "c-custom": c$custom32,
    "sanitize-col": sanitize$col43,
    "c-num": c$num28,
    "is-c-custom": is$c$custom31,
    "strict-num-sanitizer": strict$num$sanitizer117,
    "is-sanitize-col": is$sanitize$col42,
    "$answer": $answer182,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr"),
    "$locations": [
        {
            "name": "is-c-empty",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                30,
                2,
                389,
                30,
                11,
                398
            ]
        },
        {
            "name": "num-sanitizer",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                73,
                0,
                1613,
                89,
                3,
                2271
            ]
        },
        {
            "name": "c-empty",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                30,
                2,
                389,
                30,
                11,
                398
            ]
        },
        {
            "name": "is-c-str",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                31,
                2,
                401,
                31,
                22,
                421
            ]
        },
        {
            "name": "bool-sanitizer",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                91,
                0,
                2273,
                114,
                3,
                3168
            ]
        },
        {
            "name": "c-str",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                31,
                2,
                401,
                31,
                22,
                421
            ]
        },
        {
            "name": "empty-only",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                186,
                0,
                5707,
                201,
                3,
                6306
            ]
        },
        {
            "name": "string-sanitizer",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                63,
                0,
                1349,
                71,
                3,
                1611
            ]
        },
        {
            "name": "booleans-only",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                169,
                0,
                5109,
                184,
                3,
                5705
            ]
        },
        {
            "name": "numbers-only",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                152,
                0,
                4521,
                167,
                3,
                5107
            ]
        },
        {
            "name": "is-DataSourceLoaderOption",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                46,
                0,
                796,
                48,
                3,
                898
            ]
        },
        {
            "name": "option-sanitizer",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                54,
                0,
                1034,
                61,
                3,
                1347
            ]
        },
        {
            "name": "is-CellContent",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                29,
                0,
                366,
                35,
                3,
                498
            ]
        },
        {
            "name": "c-bool",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                33,
                2,
                447,
                33,
                24,
                469
            ]
        },
        {
            "name": "is-c-bool",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                33,
                2,
                447,
                33,
                24,
                469
            ]
        },
        {
            "name": "strings-only",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                135,
                0,
                3925,
                150,
                3,
                4519
            ]
        },
        {
            "name": "is-c-num",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                32,
                2,
                424,
                32,
                22,
                444
            ]
        },
        {
            "name": "c-custom",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                34,
                2,
                472,
                34,
                24,
                494
            ]
        },
        {
            "name": "sanitize-col",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                47,
                2,
                832,
                47,
                64,
                894
            ]
        },
        {
            "name": "c-num",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                32,
                2,
                424,
                32,
                22,
                444
            ]
        },
        {
            "name": "is-c-custom",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                34,
                2,
                472,
                34,
                24,
                494
            ]
        },
        {
            "name": "strict-num-sanitizer",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                116,
                0,
                3170,
                133,
                3,
                3923
            ]
        },
        {
            "name": "is-sanitize-col",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/data-source.arr",
                47,
                2,
                832,
                47,
                64,
                894
            ]
        }
    ]
};