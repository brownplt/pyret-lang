var $underscore_import18 = require("././global.arr.js");
var _global = require("././global.arr.js");
var _runtime = require("././runtime.js");
var _nothing = undefined;
var nothing17 = $underscore_import18.nothing;
var $left9 = { "names": ["v"] };
var $right10 = { "names": ["v"] };
var Either1 = {
    "left": function left(v11) {
        var $constructorTMP12 = {
            "$brand": $left9,
            "$tag": 0,
            "v": v11
        };
        return $constructorTMP12;
    },
    "right": function right(v14) {
        var $constructorTMP15 = {
            "$brand": $right10,
            "$tag": 1,
            "v": v14
        };
        return $constructorTMP15;
    },
    "is-left": function left(val) {
        return val.$brand === $left9;
    },
    "is-right": function right(val) {
        return val.$brand === $right10;
    }
};
var is$Either6 = Either1["Either"];
var is$left5 = Either1["is-left"];
var left4 = Either1["left"];
var is$right3 = Either1["is-right"];
var right2 = Either1["right"];
return module["exports"] = {
    "right": right2,
    "is-right": is$right3,
    "is-left": is$left5,
    "is-Either": is$Either6,
    "left": left4,
    "$answer": _global["trace-value"]("srcloc", nothing17),
    "$checks": _runtime["$checkResults"]()
};
