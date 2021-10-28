var _runtime = require(".\/runtime.js");
var $underscore_import870 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $underscore_import870);
var G227 = require(".\/runtime-global.arr.js");
_runtime["addModule"]("builtin:\/\/runtime-global", G227);
var O871 = require(".\/option.arr.js");
_runtime["addModule"]("builtin:\/\/option", O871);
var E872 = require(".\/either.arr.js");
_runtime["addModule"]("builtin:\/\/either", E872);
var equality190 = require(".\/equality.arr.js");
_runtime["addModule"]("builtin:\/\/equality", equality190);
var RA202 = require(".\/raw-array.arr.js");
_runtime["addModule"]("builtin:\/\/raw-array", RA202);
var N873 = require(".\/number.arr.js");
_runtime["addModule"]("builtin:\/\/number", N873);
var LP208 = require(".\/list-perf.arr.js");
_runtime["addModule"]("builtin:\/\/list-perf", LP208);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr");
var raise58 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "raise");
var none168 = _runtime["getModuleValue"]("builtin:\/\/option", "none");
var _lessthan187 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "_lessthan");
var typecast207 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "typecast");
var nothing278 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var num$is$integer281 = _runtime["getModuleValue"]("builtin:\/\/number", "num-is-integer");
var some349 = _runtime["getModuleValue"]("builtin:\/\/option", "some");
var tostring353 = _runtime["getModuleValue"]("builtin:\/\/number", "num-to-string");
var num$max463 = _runtime["getModuleValue"]("builtin:\/\/number", "num-max");
var num$ceiling464 = _runtime["getModuleValue"]("builtin:\/\/number", "num-ceiling");
var equal$always3733 = _runtime["getModuleValue"]("builtin:\/\/equality", "equal-always3");
var identical3753 = _runtime["getModuleValue"]("builtin:\/\/equality", "identical3");
var sharedBase_List166 = _runtime["$setupMethodGetters"]({
    "$methods": {
        "length": function getWrapper_length4() {
            var self1 = this;
            return _runtime["$installMethod"](self1, "length", function lam_length3() {
                return (length2 !== undefined ? length2 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    119,
                    4,
                    4502,
                    119,
                    10,
                    4508
                ], "Uninitialized letrec identifier"))(self1);
            });
        },
        "member": function getWrapper_member12() {
            var self5 = this;
            return _runtime["$installMethod"](self5, "member", function lam_member11(elt8) {
                var cases6 = self5;
                var $ans7;
                switch (cases6["$name"]) {
                case "empty":
                    $ans7 = false;
                    break;
                case "link":
                    var first9 = cases6[cases6["$fieldNames"][0]];
                    var rest10 = cases6[cases6["$fieldNames"][1]];
                    $ans7 = _runtime["equal-always"](elt8, first9) || rest10["member"](elt8);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        124,
                        4,
                        4652,
                        127,
                        7,
                        4765
                    ], cases6);
                    $ans7 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        124,
                        4,
                        4652,
                        127,
                        7,
                        4765
                    ], cases6);
                }
                return $ans7;
            });
        },
        "foldr": function getWrapper_foldr21() {
            var self13 = this;
            return _runtime["$installMethod"](self13, "foldr", function lam_foldr20(f15, base19) {
                return (foldr14 !== undefined ? foldr14 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    133,
                    4,
                    4987,
                    133,
                    9,
                    4992
                ], "Uninitialized letrec identifier"))(function lam_18(acc17, e16) {
                    return f15(e16, acc17);
                }, base19, self13);
            });
        },
        "foldl": function getWrapper_foldl29() {
            var self22 = this;
            return _runtime["$installMethod"](self22, "foldl", function lam_foldl28(f24, base27) {
                return (fold23 !== undefined ? fold23 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    139,
                    4,
                    5253,
                    139,
                    8,
                    5257
                ], "Uninitialized letrec identifier"))(function lam_18(acc26, e25) {
                    return f24(e25, acc26);
                }, base27, self22);
            });
        },
        "all": function getWrapper_all37() {
            var self30 = this;
            return _runtime["$installMethod"](self30, "all", function lam_all36(f33) {
                var cases31 = self30;
                var $ans32;
                switch (cases31["$name"]) {
                case "empty":
                    $ans32 = true;
                    break;
                case "link":
                    var first34 = cases31[cases31["$fieldNames"][0]];
                    var rest35 = cases31[cases31["$fieldNames"][1]];
                    $ans32 = f33(first34) && rest35["all"](f33);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        144,
                        4,
                        5452,
                        147,
                        7,
                        5553
                    ], cases31);
                    $ans32 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        144,
                        4,
                        5452,
                        147,
                        7,
                        5553
                    ], cases31);
                }
                return $ans32;
            });
        },
        "any": function getWrapper_any45() {
            var self38 = this;
            return _runtime["$installMethod"](self38, "any", function lam_any44(f41) {
                var cases39 = self38;
                var $ans40;
                switch (cases39["$name"]) {
                case "empty":
                    $ans40 = false;
                    break;
                case "link":
                    var first42 = cases39[cases39["$fieldNames"][0]];
                    var rest43 = cases39[cases39["$fieldNames"][1]];
                    $ans40 = f41(first42) || rest43["any"](f41);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        152,
                        4,
                        5706,
                        156,
                        7,
                        5815
                    ], cases39);
                    $ans40 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        152,
                        4,
                        5706,
                        156,
                        7,
                        5815
                    ], cases39);
                }
                return $ans40;
            });
        },
        "append": function getWrapper_append54() {
            var self46 = this;
            return _runtime["$installMethod"](self46, "append", function lam_append53(other49) {
                var cases47 = self46;
                var $ans48;
                switch (cases47["$name"]) {
                case "empty":
                    $ans48 = other49;
                    break;
                case "link":
                    var first51 = cases47[cases47["$fieldNames"][0]];
                    var rest52 = cases47[cases47["$fieldNames"][1]];
                    $ans48 = (link50 !== undefined ? link50 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        164,
                        8,
                        6046,
                        164,
                        12,
                        6050
                    ], "Uninitialized letrec identifier"))(first51, rest52["append"](other49));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        161,
                        4,
                        5968,
                        165,
                        7,
                        6085
                    ], cases47);
                    $ans48 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        161,
                        4,
                        5968,
                        165,
                        7,
                        6085
                    ], cases47);
                }
                return $ans48;
            });
        },
        "head": function getWrapper_head62() {
            var self55 = this;
            return _runtime["$installMethod"](self55, "head", function lam_head61() {
                var cases56 = self55;
                var $ans57;
                switch (cases56["$name"]) {
                case "empty":
                    $ans57 = raise58("head: empty list");
                    break;
                case "link":
                    var head59 = cases56[cases56["$fieldNames"][0]];
                    var $underscore60 = cases56[cases56["$fieldNames"][1]];
                    $ans57 = head59;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        169,
                        4,
                        6124,
                        172,
                        7,
                        6222
                    ], cases56);
                    $ans57 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        169,
                        4,
                        6124,
                        172,
                        7,
                        6222
                    ], cases56);
                }
                return $ans57;
            });
        },
        "tail": function getWrapper_tail69() {
            var self63 = this;
            return _runtime["$installMethod"](self63, "tail", function lam_tail68() {
                var cases64 = self63;
                var $ans65;
                switch (cases64["$name"]) {
                case "empty":
                    $ans65 = raise58("tail: empty list");
                    break;
                case "link":
                    var $underscore67 = cases64[cases64["$fieldNames"][0]];
                    var tail66 = cases64[cases64["$fieldNames"][1]];
                    $ans65 = tail66;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        176,
                        4,
                        6267,
                        179,
                        7,
                        6365
                    ], cases64);
                    $ans65 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        176,
                        4,
                        6267,
                        179,
                        7,
                        6365
                    ], cases64);
                }
                return $ans65;
            });
        },
        "last": function getWrapper_last78() {
            var self70 = this;
            return _runtime["$installMethod"](self70, "last", function lam_last77() {
                var cases71 = self70;
                var $ans72;
                switch (cases71["$name"]) {
                case "empty":
                    $ans72 = raise58("last: took last of empty list");
                    break;
                case "link":
                    var first76 = cases71[cases71["$fieldNames"][0]];
                    var rest73 = cases71[cases71["$fieldNames"][1]];
                    if ((is$empty75 !== undefined ? is$empty75 : _runtime["$messageThrow"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                            190,
                            11,
                            6618,
                            190,
                            19,
                            6626
                        ], "Uninitialized letrec identifier"))(rest73)) {
                        $ans74 = self70["first"];
                    } else {
                        $ans74 = rest73["last"]();
                    }
                    $ans72 = $ans74;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        184,
                        4,
                        6494,
                        195,
                        7,
                        6710
                    ], cases71);
                    $ans72 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        184,
                        4,
                        6494,
                        195,
                        7,
                        6710
                    ], cases71);
                }
                return $ans72;
            });
        },
        "sort-by": function getWrapper_sort$by98() {
            var self79 = this;
            return _runtime["$installMethod"](self79, "sort-by", function lam_sort$by97(cmp92, eq90) {
                var cases80 = self79;
                var $ans81;
                switch (cases80["$name"]) {
                case "empty":
                    $ans81 = self79;
                    break;
                case "link":
                    var first82 = cases80[cases80["$fieldNames"][0]];
                    var $underscore96 = cases80[cases80["$fieldNames"][1]];
                    var pivot83 = first82;
                    var are$lt85 = empty84 !== undefined ? empty84 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        213,
                        23,
                        7487,
                        213,
                        28,
                        7492
                    ], "Uninitialized letrec identifier");
                    var are$eq86 = empty84 !== undefined ? empty84 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        214,
                        23,
                        7516,
                        214,
                        28,
                        7521
                    ], "Uninitialized letrec identifier");
                    var are$gt87 = empty84 !== undefined ? empty84 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        215,
                        23,
                        7545,
                        215,
                        28,
                        7550
                    ], "Uninitialized letrec identifier");
                    self79["each"](function lam_18(e88) {
                        if (cmp92(e88, pivot83)) {
                            $ans91 = are$lt85 = (link50 !== undefined ? link50 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                                223,
                                46,
                                7919,
                                223,
                                50,
                                7923
                            ], "Uninitialized letrec identifier"))(e88, are$lt85);
                        } else {
                            if (eq90(e88, pivot83)) {
                                $ans89 = are$eq86 = (link50 !== undefined ? link50 : _runtime["$messageThrow"]([
                                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                                    224,
                                    46,
                                    7981,
                                    224,
                                    50,
                                    7985
                                ], "Uninitialized letrec identifier"))(e88, are$eq86);
                            } else {
                                $ans89 = are$gt87 = (link50 !== undefined ? link50 : _runtime["$messageThrow"]([
                                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                                    225,
                                    46,
                                    8043,
                                    225,
                                    50,
                                    8047
                                ], "Uninitialized letrec identifier"))(e88, are$gt87);
                            }
                            $ans91 = $ans89;
                        }
                        return $ans91;
                    });
                    var less93 = are$lt85["sort-by"](cmp92, eq90);
                    var equal94 = are$eq86;
                    var greater95 = are$gt87["sort-by"](cmp92, eq90);
                    less93["append"](equal94["append"](greater95));
                    $ans81 = less93["append"](equal94["append"](greater95));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        202,
                        4,
                        7072,
                        234,
                        7,
                        8304
                    ], cases80);
                    $ans81 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        202,
                        4,
                        7072,
                        234,
                        7,
                        8304
                    ], cases80);
                }
                return $ans81;
            });
        },
        "_plus": function getWrapper__plus102() {
            var self99 = this;
            return _runtime["$installMethod"](self99, "_plus", function lam__plus101(other100) {
                return self99["append"](other100);
            });
        },
        "map": function getWrapper_map107() {
            var self103 = this;
            return _runtime["$installMethod"](self103, "map", function lam_map106(f105) {
                return (map104 !== undefined ? map104 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    243,
                    4,
                    8573,
                    243,
                    7,
                    8576
                ], "Uninitialized letrec identifier"))(f105, self103);
            });
        },
        "filter": function getWrapper_filter112() {
            var self108 = this;
            return _runtime["$installMethod"](self108, "filter", function lam_filter111(f110) {
                return (filter109 !== undefined ? filter109 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    248,
                    4,
                    8784,
                    248,
                    10,
                    8790
                ], "Uninitialized letrec identifier"))(f110, self108);
            });
        },
        "each": function getWrapper_each117() {
            var self113 = this;
            return _runtime["$installMethod"](self113, "each", function lam_each116(f115) {
                return (each114 !== undefined ? each114 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    253,
                    4,
                    8974,
                    253,
                    8,
                    8978
                ], "Uninitialized letrec identifier"))(f115, self113);
            });
        },
        "reverse": function getWrapper_reverse121() {
            var self118 = this;
            return _runtime["$installMethod"](self118, "reverse", function lam_reverse120() {
                return (reverse119 !== undefined ? reverse119 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    258,
                    4,
                    9136,
                    258,
                    11,
                    9143
                ], "Uninitialized letrec identifier"))(self118);
            });
        },
        "push": function getWrapper_push125() {
            var self122 = this;
            return _runtime["$installMethod"](self122, "push", function lam_push124(elt123) {
                return (link50 !== undefined ? link50 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    263,
                    4,
                    9289,
                    263,
                    8,
                    9293
                ], "Uninitialized letrec identifier"))(elt123, self122);
            });
        },
        "split-at": function getWrapper_split$at130() {
            var self126 = this;
            return _runtime["$installMethod"](self126, "split-at", function lam_split$at129(n128) {
                return (split$at127 !== undefined ? split$at127 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    267,
                    4,
                    9524,
                    267,
                    12,
                    9532
                ], "Uninitialized letrec identifier"))(n128, self126);
            });
        },
        "take": function getWrapper_take134() {
            var self131 = this;
            return _runtime["$installMethod"](self131, "take", function lam_take133(n132) {
                return (split$at127 !== undefined ? split$at127 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    271,
                    4,
                    9662,
                    271,
                    12,
                    9670
                ], "Uninitialized letrec identifier"))(n132, self131)["prefix"];
            });
        },
        "drop": function getWrapper_drop138() {
            var self135 = this;
            return _runtime["$installMethod"](self135, "drop", function lam_drop137(n136) {
                return (split$at127 !== undefined ? split$at127 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    275,
                    4,
                    9815,
                    275,
                    12,
                    9823
                ], "Uninitialized letrec identifier"))(n136, self135)["suffix"];
            });
        },
        "get": function getWrapper_get143() {
            var self139 = this;
            return _runtime["$installMethod"](self139, "get", function lam_get142(n141) {
                return (get140 !== undefined ? get140 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    280,
                    4,
                    9990,
                    280,
                    7,
                    9993
                ], "Uninitialized letrec identifier"))(self139, n141);
            });
        },
        "set": function getWrapper_set149() {
            var self144 = this;
            return _runtime["$installMethod"](self144, "set", function lam_set148(n146, e147) {
                return (function$set145 !== undefined ? function$set145 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    284,
                    4,
                    10192,
                    284,
                    16,
                    10204
                ], "Uninitialized letrec identifier"))(self144, n146, e147);
            });
        },
        "remove": function getWrapper_remove154() {
            var self150 = this;
            return _runtime["$installMethod"](self150, "remove", function lam_remove153(e152) {
                return (remove151 !== undefined ? remove151 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    288,
                    4,
                    10370,
                    288,
                    10,
                    10376
                ], "Uninitialized letrec identifier"))(self150, e152);
            });
        },
        "join-str": function getWrapper_join$str159() {
            var self155 = this;
            return _runtime["$installMethod"](self155, "join-str", function lam_join$str158(sep157) {
                return (join$str156 !== undefined ? join$str156 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    293,
                    4,
                    10603,
                    293,
                    12,
                    10611
                ], "Uninitialized letrec identifier"))(self155, sep157);
            });
        },
        "join-str-last": function getWrapper_join$str$last165() {
            var self160 = this;
            return _runtime["$installMethod"](self160, "join-str-last", function lam_join$str$last164(sep162, last$sep163) {
                return (join$str$last161 !== undefined ? join$str$last161 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    298,
                    4,
                    10922,
                    298,
                    17,
                    10935
                ], "Uninitialized letrec identifier"))(self160, sep162, last$sep163);
            });
        }
    }
});
var variantBase_empty179 = _runtime["$createVariant"](sharedBase_List166, _runtime["$setupMethodGetters"]({
    "$methods": {
        "find": function getWrapper_find171() {
            var self167 = this;
            return _runtime["$installMethod"](self167, "find", function lam_find170(f169) {
                return none168;
            });
        },
        "partition": function getWrapper_partition175() {
            var self172 = this;
            return _runtime["$installMethod"](self172, "partition", function lam_partition174(f173) {
                return {
                    "is-true": empty84 !== undefined ? empty84 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        84,
                        17,
                        2912,
                        84,
                        22,
                        2917
                    ], "Uninitialized letrec identifier"),
                    "is-false": empty84 !== undefined ? empty84 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                        84,
                        34,
                        2929,
                        84,
                        39,
                        2934
                    ], "Uninitialized letrec identifier"),
                    "$methods": {}
                };
            });
        },
        "sort": function getWrapper_sort178() {
            var self176 = this;
            return _runtime["$installMethod"](self176, "sort", function lam_sort177() {
                return self176;
            });
        }
    }
}), {
    "$data": sharedBase_List166,
    "$name": "empty",
    "$fieldNames": null
});
var variantBase_link191 = _runtime["$createVariant"](sharedBase_List166, _runtime["$setupMethodGetters"]({
    "$methods": {
        "partition": function getWrapper_partition175() {
            var self180 = this;
            return _runtime["$installMethod"](self180, "partition", function lam_partition174(f182) {
                return (partition181 !== undefined ? partition181 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    98,
                    6,
                    3573,
                    98,
                    15,
                    3582
                ], "Uninitialized letrec identifier"))(f182, self180);
            });
        },
        "find": function getWrapper_find171() {
            var self183 = this;
            return _runtime["$installMethod"](self183, "find", function lam_find170(f185) {
                return (find184 !== undefined ? find184 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    103,
                    6,
                    3799,
                    103,
                    10,
                    3803
                ], "Uninitialized letrec identifier"))(f185, self183);
            });
        },
        "sort": function getWrapper_sort178() {
            var self186 = this;
            return _runtime["$installMethod"](self186, "sort", function lam_sort177() {
                return self186["sort-by"](function lam_18(e1188, e2189) {
                    return _lessthan187(e1188, e2189);
                }, equality190["within"](_runtime["_makeNumberFromString"]("~0", _runtime["$errCallbacks"])));
            });
        }
    }
}), {
    "$data": sharedBase_List166,
    "$name": "link",
    "$fieldNames": [
        "first",
        "rest"
    ]
});
var List198 = {
    "empty": variantBase_empty179,
    "link": function link194(first192, rest193) {
        return _runtime["$makeDataValue"](variantBase_link191, {
            "first": first192,
            "rest": rest193
        });
    },
    "is-empty": function is$empty195(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_empty179["$variant"];
    },
    "is-link": function is$link196(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_link191["$variant"];
    },
    "is-List": function is$List197(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_List166;
    }
};
var is$List199 = List198["is-List"];
var is$empty75 = List198["is-empty"];
var empty84 = List198["empty"];
var is$link200 = List198["is-link"];
var link50 = List198["link"];
var to$raw$array206 = function lam_to$raw$array205(lst201) {
    return lst201["foldl"](function lam_18(elem204, acc203) {
        return RA202["raw-array-push"](acc203, elem204);
    }, RA202["raw-array"]["make"]([]));
};
var raw$array$to$list211 = function lam_raw$array$to$list210(array209) {
    return typecast207(LP208["perf-array-to-list"](typecast207(array209)));
};
var foldl$complicated215 = function lam_foldl$complicated225(is$first224, flist212, f217, x218, l219, base221) {
    var cases213 = flist212;
    var $ans214;
    switch (cases213["$name"]) {
    case "link":
        var head220 = cases213[cases213["$fieldNames"][0]];
        var tail216 = cases213[cases213["$fieldNames"][1]];
        if (is$first224) {
            $ans223 = foldl$complicated215(false, tail216, f217, x218, l219, f217(head220, base221));
        } else {
            if (_runtime["equal-always"](tail216, empty84)) {
                $ans222 = foldl$complicated215(false, tail216, f217, x218, l219, l219(head220, base221));
            } else {
                $ans222 = foldl$complicated215(false, tail216, f217, x218, l219, x218(head220, base221));
            }
            $ans223 = $ans222;
        }
        $ans214 = $ans223;
        break;
    case "empty":
        $ans214 = base221;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            320,
            2,
            11585,
            330,
            5,
            11914
        ], cases213);
        $ans214 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            320,
            2,
            11585,
            330,
            5,
            11914
        ], cases213);
    }
    return $ans214;
};
var join$str156 = function lam_join$str158(l234, sep231) {
    var f229 = function lam_18(elem228, acc226) {
        return _runtime["_plus"](acc226, G227["js-to-string"](elem228), _runtime["$errCallbacks"]);
    };
    var x233 = function lam_18(elem232, acc230) {
        return _runtime["_plus"](_runtime["_plus"](acc230, sep231, _runtime["$errCallbacks"]), G227["js-to-string"](elem232), _runtime["$errCallbacks"]);
    };
    return foldl$complicated215(true, l234, f229, x233, x233, "");
};
var $underscore244 = undefined;
var join$str$last161 = function lam_join$str$last164(jlist256, sep249, last$sep253) {
    var f247 = function lam_18(elem246, acc245) {
        return _runtime["_plus"](acc245, G227["js-to-string"](elem246), _runtime["$errCallbacks"]);
    };
    var x251 = function lam_18(elem250, acc248) {
        return _runtime["_plus"](_runtime["_plus"](acc248, sep249, _runtime["$errCallbacks"]), G227["js-to-string"](elem250), _runtime["$errCallbacks"]);
    };
    var l255 = function lam_18(elem254, acc252) {
        return _runtime["_plus"](_runtime["_plus"](acc252, last$sep253, _runtime["$errCallbacks"]), G227["js-to-string"](elem254), _runtime["$errCallbacks"]);
    };
    return foldl$complicated215(true, jlist256, f247, x251, l255, "");
};
var $underscore268 = undefined;
var remove151 = function lam_remove153(lst269, elt273) {
    var cases270 = lst269;
    var $ans271;
    switch (cases270["$name"]) {
    case "empty":
        $ans271 = empty84;
        break;
    case "link":
        var first272 = cases270[cases270["$fieldNames"][0]];
        var rest275 = cases270[cases270["$fieldNames"][1]];
        if (_runtime["equal-always"](elt273, lst269["first"])) {
            $ans274 = remove151(rest275, elt273);
        } else {
            $ans274 = link50(first272, remove151(lst269["rest"], elt273));
        }
        $ans271 = $ans274;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            378,
            2,
            13704,
            386,
            5,
            13892
        ], cases270);
        $ans271 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            378,
            2,
            13704,
            386,
            5,
            13892
        ], cases270);
    }
    return $ans271;
};
var filter109 = function lam_filter111(f276, lst277) {
    return typecast207(LP208["perf-filter"](f276, typecast207(lst277)));
};
var split$at127 = function lam_split$at129(n280, lst294) {
    if (_runtime["_lessthan"](n280, 0, _runtime["$errCallbacks"]) || G227["not"](num$is$integer281(n280))) {
        raise58("Invalid index");
        nothing278;
        $ans279 = nothing278;
    } else {
        nothing278;
        $ans279 = nothing278;
    }
    $ans279;
    var prefix282 = empty84;
    var suffix283 = empty84;
    var help287 = function lam_help293(ind288, l284) {
        if (_runtime["equal-always"](ind288, 0)) {
            $ans292 = suffix283 = l284;
        } else {
            var cases285 = l284;
            var $ans286;
            switch (cases285["$name"]) {
            case "empty":
                $ans286 = raise58("Index too large");
                break;
            case "link":
                var fst290 = cases285[cases285["$fieldNames"][0]];
                var rst289 = cases285[cases285["$fieldNames"][1]];
                help287(_runtime["_minus"](ind288, 1, _runtime["$errCallbacks"]), rst289);
                prefix282 = function lam_18(arg_291) {
                    return link50(arg_291, prefix282);
                }(fst290);
                $ans286 = prefix282 = function lam_18(arg_291) {
                    return link50(arg_291, prefix282);
                }(fst290);
                break;
            default:
                _runtime["throwNoCaseesMatched"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    407,
                    6,
                    14694,
                    412,
                    9,
                    14867
                ], cases285);
                $ans286 = _runtime["throwNoCaseesMatched"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                    407,
                    6,
                    14694,
                    412,
                    9,
                    14867
                ], cases285);
            }
            $ans292 = $ans286;
        }
        return $ans292;
    };
    help287(n280, lst294);
    ({
        "prefix": prefix282,
        "suffix": suffix283,
        "$methods": {}
    });
    ({
        "prefix": prefix282,
        "suffix": suffix283,
        "$methods": {}
    });
    return {
        "prefix": prefix282,
        "suffix": suffix283,
        "$methods": {}
    };
};
var fold23 = function lam_fold298(f295, base296, lst297) {
    return LP208["perf-foldl"](f295, base296, typecast207(lst297));
};
var reverse119 = function lam_reverse120(lst301) {
    return fold23(function lam_18(acc300, elt299) {
        return link50(elt299, acc300);
    }, empty84, lst301);
};
var $underscore307 = undefined;
var each114 = function lam_each116(f308, lst311) {
    fold23(function lam_18($underscore310, elt309) {
        return f308(elt309);
    }, nothing278, lst311);
    nothing278;
    return nothing278;
};
var map104 = function lam_map106(f312, lst313) {
    return typecast207(LP208["perf-map"](f312, typecast207(lst313)));
};
var slice328 = function lam_slice327(lst326, inclusive$lower320, exclusive$upper321) {
    var help322 = function lam_help293(acc317, inner$lst314, index319) {
        var cases315 = inner$lst314;
        var $ans316;
        switch (cases315["$name"]) {
        case "empty":
            $ans316 = acc317;
            break;
        case "link":
            var first325 = cases315[cases315["$fieldNames"][0]];
            var rest323 = cases315[cases315["$fieldNames"][1]];
            if (_runtime["_greaterequal"](index319, inclusive$lower320, _runtime["$errCallbacks"]) && _runtime["_lessthan"](index319, exclusive$upper321, _runtime["$errCallbacks"])) {
                $ans324 = link50(inner$lst314["first"], help322(acc317, rest323, _runtime["_plus"](index319, 1, _runtime["$errCallbacks"])));
            } else {
                if (_runtime["_lessthan"](index319, inclusive$lower320, _runtime["$errCallbacks"]) && _runtime["_lessthan"](index319, exclusive$upper321, _runtime["$errCallbacks"])) {
                    $ans318 = help322(acc317, rest323, _runtime["_plus"](index319, 1, _runtime["$errCallbacks"]));
                } else {
                    $ans318 = acc317;
                }
                $ans324 = $ans318;
            }
            $ans316 = $ans324;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                452,
                4,
                16545,
                463,
                7,
                16921
            ], cases315);
            $ans316 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                452,
                4,
                16545,
                463,
                7,
                16921
            ], cases315);
        }
        return $ans316;
    };
    return help322(empty84, lst326, 0);
};
var partition181 = function lam_partition174(f338, lst341) {
    var is$true329 = empty84;
    var is$false330 = empty84;
    var help334 = function lam_help293(inner$lst331) {
        var cases332 = inner$lst331;
        var $ans333;
        switch (cases332["$name"]) {
        case "empty":
            $ans333 = nothing278;
            break;
        case "link":
            var first340 = cases332[cases332["$fieldNames"][0]];
            var rest335 = cases332[cases332["$fieldNames"][1]];
            help334(rest335);
            if (f338(inner$lst331["first"])) {
                $ans337 = is$true329 = function lam_18(arg_339) {
                    return link50(arg_339, is$true329);
                }(inner$lst331["first"]);
            } else {
                $ans337 = is$false330 = function lam_18(arg_336) {
                    return link50(arg_336, is$false330);
                }(inner$lst331["first"]);
            }
            $ans337;
            nothing278;
            $ans333 = nothing278;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                473,
                4,
                17251,
                484,
                7,
                17558
            ], cases332);
            $ans333 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                473,
                4,
                17251,
                484,
                7,
                17558
            ], cases332);
        }
        return $ans333;
    };
    help334(lst341);
    ({
        "is-true": is$true329,
        "is-false": is$false330,
        "$methods": {}
    });
    return {
        "is-true": is$true329,
        "is-false": is$false330,
        "$methods": {}
    };
};
var find184 = function lam_find170(f345, lst342) {
    var cases343 = lst342;
    var $ans344;
    switch (cases343["$name"]) {
    case "empty":
        $ans344 = none168;
        break;
    case "link":
        var first348 = cases343[cases343["$fieldNames"][0]];
        var rest346 = cases343[cases343["$fieldNames"][1]];
        if (f345(first348)) {
            $ans347 = some349(first348);
        } else {
            $ans347 = find184(f345, rest346);
        }
        $ans344 = $ans347;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            494,
            2,
            17817,
            502,
            5,
            17969
        ], cases343);
        $ans344 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            494,
            2,
            17817,
            502,
            5,
            17969
        ], cases343);
    }
    return $ans344;
};
var get140 = function lam_get142(lst360, n354) {
    var help355 = function lam_help293(l350, cur357) {
        var cases351 = l350;
        var $ans352;
        switch (cases351["$name"]) {
        case "empty":
            $ans352 = raise58(_runtime["_plus"]("get: n too large ", tostring353(n354), _runtime["$errCallbacks"]));
            break;
        case "link":
            var first359 = cases351[cases351["$fieldNames"][0]];
            var rest356 = cases351[cases351["$fieldNames"][1]];
            if (_runtime["equal-always"](cur357, 0)) {
                $ans358 = first359;
            } else {
                $ans358 = help355(rest356, _runtime["_minus"](cur357, 1, _runtime["$errCallbacks"]));
            }
            $ans352 = $ans358;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                508,
                4,
                18158,
                516,
                7,
                18350
            ], cases351);
            $ans352 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                508,
                4,
                18158,
                516,
                7,
                18350
            ], cases351);
        }
        return $ans352;
    };
    if (_runtime["_lessthan"](n354, 0, _runtime["$errCallbacks"])) {
        $ans361 = raise58(_runtime["_plus"]("get: invalid argument: ", tostring353(n354), _runtime["$errCallbacks"]));
    } else {
        $ans361 = help355(lst360, n354);
    }
    return $ans361;
};
var function$set145 = function lam_function$set376(lst374, n365, v372) {
    var help368 = function lam_help293(l362, cur370) {
        var cases363 = l362;
        var $ans364;
        switch (cases363["$name"]) {
        case "empty":
            $ans364 = raise58(_runtime["_plus"]("set: n too large ", tostring353(n365), _runtime["$errCallbacks"]));
            break;
        case "link":
            var first366 = cases363[cases363["$fieldNames"][0]];
            var rest369 = cases363[cases363["$fieldNames"][1]];
            if (_runtime["equal-always"](cur370, 0)) {
                $ans371 = function lam_18(arg_373) {
                    return link50(arg_373, rest369);
                }(v372);
            } else {
                $ans371 = function lam_18(arg_367) {
                    return link50(arg_367, help368(rest369, _runtime["_minus"](cur370, 1, _runtime["$errCallbacks"])));
                }(first366);
            }
            $ans364 = $ans371;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                527,
                4,
                18730,
                535,
                7,
                18961
            ], cases363);
            $ans364 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                527,
                4,
                18730,
                535,
                7,
                18961
            ], cases363);
        }
        return $ans364;
    };
    if (_runtime["_lessthan"](n365, 0, _runtime["$errCallbacks"])) {
        $ans375 = raise58(_runtime["_plus"]("set: invalid argument: ", tostring353(n365), _runtime["$errCallbacks"]));
    } else {
        $ans375 = help368(lst374, n365);
    }
    return $ans375;
};
var list392 = {
    "make": raw$array$to$list211,
    "make0": function lam_18() {
        return empty84;
    },
    "make1": function lam_18(a377) {
        return link50(a377, empty84);
    },
    "make2": function lam_18(a378, b379) {
        return link50(a378, link50(b379, empty84));
    },
    "make3": function lam_18(a380, b381, c382) {
        return link50(a380, link50(b381, link50(c382, empty84)));
    },
    "make4": function lam_18(a383, b384, c385, d386) {
        return link50(a383, link50(b384, link50(c385, link50(d386, empty84))));
    },
    "make5": function lam_18(a387, b388, c389, d390, e391) {
        return link50(a387, link50(b388, link50(c389, link50(d390, link50(e391, empty84)))));
    },
    "$methods": {}
};
var length2 = function lam_length3(lst393) {
    return LP208["perf-length"](typecast207(lst393));
};
var same$length397 = function lam_same$length396(lst1394, lst2395) {
    return LP208["perf-same-length"](typecast207(lst1394), typecast207(lst2395));
};
var $underscore405 = undefined;
var longer$than410 = function lam_longer$than413(lst406, len409) {
    var cases407 = lst406;
    var $ans408;
    switch (cases407["$name"]) {
    case "empty":
        $ans408 = _runtime["_lessthan"](len409, 0, _runtime["$errCallbacks"]);
        break;
    case "link":
        var $underscore412 = cases407[cases407["$fieldNames"][0]];
        var rest411 = cases407[cases407["$fieldNames"][1]];
        $ans408 = _runtime["_lessthan"](len409, 1, _runtime["$errCallbacks"]) || longer$than410(rest411, _runtime["_minus"](len409, 1, _runtime["$errCallbacks"]));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            575,
            2,
            20621,
            578,
            5,
            20729
        ], cases407);
        $ans408 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            575,
            2,
            20621,
            578,
            5,
            20729
        ], cases407);
    }
    return $ans408;
};
var $underscore421 = undefined;
var shorter$than426 = function lam_shorter$than429(lst422, len425) {
    var cases423 = lst422;
    var $ans424;
    switch (cases423["$name"]) {
    case "empty":
        $ans424 = _runtime["_greaterthan"](len425, 0, _runtime["$errCallbacks"]);
        break;
    case "link":
        var $underscore428 = cases423[cases423["$fieldNames"][0]];
        var rest427 = cases423[cases423["$fieldNames"][1]];
        $ans424 = _runtime["_greaterthan"](len425, 1, _runtime["$errCallbacks"]) && shorter$than426(rest427, _runtime["_minus"](len425, 1, _runtime["$errCallbacks"]));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            587,
            2,
            21065,
            590,
            5,
            21175
        ], cases423);
        $ans424 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            587,
            2,
            21065,
            590,
            5,
            21175
        ], cases423);
    }
    return $ans424;
};
var $underscore437 = undefined;
var push440 = function lam_push124(l439, elt438) {
    return link50(elt438, l439);
};
var last450 = function lam_last77(lst441) {
    var helper444 = function lam_helper449(l448) {
        var cases442 = lst441;
        var $ans443;
        switch (cases442["$name"]) {
        case "empty":
            $ans443 = raise58("last: took last of empty list");
            break;
        case "link":
            var first447 = cases442[cases442["$fieldNames"][0]];
            var rest445 = cases442[cases442["$fieldNames"][1]];
            if (is$empty75(rest445)) {
                $ans446 = first447;
            } else {
                $ans446 = helper444(rest445);
            }
            $ans443 = $ans446;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                604,
                4,
                21538,
                612,
                7,
                21739
            ], cases442);
            $ans443 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                604,
                4,
                21538,
                612,
                7,
                21739
            ], cases442);
        }
        return $ans443;
    };
    return helper444(lst441);
};
var sort$by454 = function lam_sort$by97(lst451, cmp452, eq453) {
    return lst451["sort-by"](cmp452, eq453);
};
var sort456 = function lam_sort177(lst455) {
    return lst455["sort"]();
};
var range462 = function lam_range461(start458, stop459) {
    if (_runtime["_greaterthan"](start458, stop459, _runtime["$errCallbacks"])) {
        $ans460 = raise58(_runtime["_plus"](_runtime["_plus"](_runtime["_plus"](_runtime["_plus"]("range: start greater than stop: (", tostring353(start458), _runtime["$errCallbacks"]), ", ", _runtime["$errCallbacks"]), tostring353(stop459), _runtime["$errCallbacks"]), ")", _runtime["$errCallbacks"]));
    } else {
        $ans460 = raw$array$to$list211(RA202["raw-array-build"](function lam_18(i457) {
            return _runtime["_plus"](i457, start458, _runtime["$errCallbacks"]);
        }, _runtime["_minus"](stop459, start458, _runtime["$errCallbacks"])));
    }
    return $ans460;
};
var range$by473 = function lam_range$by472(start466, stop465, delta467) {
    if (_runtime["equal-always"](delta467, 0)) {
        if (_runtime["equal-always"](start466, stop465)) {
            $ans471 = empty84;
        } else {
            $ans471 = raise58("range-by: an interval of 0 would produce an infinite list");
        }
        $ans470 = $ans471;
    } else {
        var len468 = num$max463(num$ceiling464(_runtime["_divide"](_runtime["_minus"](stop465, start466, _runtime["$errCallbacks"]), delta467, _runtime["$errCallbacks"])), 0);
        $ans470 = raw$array$to$list211(RA202["raw-array-build"](function lam_18(i469) {
            return _runtime["_plus"](start466, _runtime["_times"](i469, delta467, _runtime["$errCallbacks"]), _runtime["$errCallbacks"]);
        }, len468));
    }
    return $ans470;
};
var $underscore487 = undefined;
var repeat492 = function lam_repeat491(n489, e488) {
    if (_runtime["_lessthan"](n489, 0, _runtime["$errCallbacks"])) {
        $ans490 = raise58("repeat: can't have a negative argument'");
    } else {
        $ans490 = raw$array$to$list211(RA202["raw-array-of"](e488, n489));
    }
    return $ans490;
};
var append498 = function lam_append53(front493, back496) {
    var cases494 = front493;
    var $ans495;
    switch (cases494["$name"]) {
    case "empty":
        $ans495 = back496;
        break;
    case "link":
        var f497 = cases494[cases494["$fieldNames"][0]];
        var r499 = cases494[cases494["$fieldNames"][1]];
        $ans495 = link50(f497, append498(r499, back496));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            665,
            2,
            23339,
            668,
            5,
            23428
        ], cases494);
        $ans495 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            665,
            2,
            23339,
            668,
            5,
            23428
        ], cases494);
    }
    return $ans495;
};
var take502 = function lam_take133(n500, lst501) {
    return split$at127(n500, lst501)["prefix"];
};
var drop505 = function lam_drop137(n503, lst504) {
    return split$at127(n503, lst504)["suffix"];
};
var any511 = function lam_any44(f509, lst506) {
    var cases507 = lst506;
    var $ans508;
    switch (cases507["$name"]) {
    case "empty":
        $ans508 = false;
        break;
    case "link":
        var first510 = cases507[cases507["$fieldNames"][0]];
        var rest512 = cases507[cases507["$fieldNames"][1]];
        $ans508 = f509(first510) || any511(f509, rest512);
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            683,
            2,
            23886,
            686,
            5,
            23981
        ], cases507);
        $ans508 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            683,
            2,
            23886,
            686,
            5,
            23981
        ], cases507);
    }
    return $ans508;
};
var all518 = function lam_all36(f516, lst513) {
    var cases514 = lst513;
    var $ans515;
    switch (cases514["$name"]) {
    case "empty":
        $ans515 = true;
        break;
    case "link":
        var first517 = cases514[cases514["$fieldNames"][0]];
        var rest519 = cases514[cases514["$fieldNames"][1]];
        $ans515 = f516(first517) && all518(f516, rest519);
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            691,
            2,
            24116,
            694,
            5,
            24211
        ], cases514);
        $ans515 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            691,
            2,
            24116,
            694,
            5,
            24211
        ], cases514);
    }
    return $ans515;
};
var all2528 = function lam_all2527(f520, lst1525, lst2526) {
    var help523 = function lam_help293(l1521, l2522) {
        if (is$empty75(l1521) || is$empty75(l2522)) {
            $ans524 = true;
        } else {
            $ans524 = f520(l1521["head"](), l2522["head"]()) && help523(l1521["tail"](), l2522["tail"]());
        }
        return $ans524;
    };
    return help523(lst1525, lst2526);
};
var map2533 = function lam_map2535(f529, l1530, l2531) {
    if (is$empty75(l1530) || is$empty75(l2531)) {
        $ans534 = empty84;
    } else {
        $ans534 = function lam_18(arg_532) {
            return link50(arg_532, map2533(f529, l1530["tail"](), l2531["tail"]()));
        }(f529(l1530["head"](), l2531["head"]()));
    }
    return $ans534;
};
var map3541 = function lam_map3543(f536, l1537, l2538, l3539) {
    if (is$empty75(l1537) || is$empty75(l2538) || is$empty75(l3539)) {
        $ans542 = empty84;
    } else {
        $ans542 = function lam_18(arg_540) {
            return link50(arg_540, map3541(f536, l1537["tail"](), l2538["tail"](), l3539["tail"]()));
        }(f536(l1537["head"](), l2538["head"](), l3539["head"]()));
    }
    return $ans542;
};
var map4550 = function lam_map4552(f544, l1545, l2546, l3547, l4548) {
    if (is$empty75(l1545) || is$empty75(l2546) || is$empty75(l3547) || is$empty75(l4548)) {
        $ans551 = empty84;
    } else {
        $ans551 = function lam_18(arg_549) {
            return link50(arg_549, map4550(f544, l1545["tail"](), l2546["tail"](), l3547["tail"](), l4548["tail"]()));
        }(f544(l1545["head"](), l2546["head"](), l3547["head"](), l4548["head"]()));
    }
    return $ans551;
};
var map_n557 = function lam_map_n559(f553, n554, lst555) {
    if (is$empty75(lst555)) {
        $ans558 = empty84;
    } else {
        $ans558 = function lam_18(arg_556) {
            return link50(arg_556, map_n557(f553, _runtime["_plus"](n554, 1, _runtime["$errCallbacks"]), lst555["tail"]()));
        }(f553(n554, lst555["head"]()));
    }
    return $ans558;
};
var map2_n565 = function lam_map2_n567(f560, n561, l1562, l2563) {
    if (is$empty75(l1562) || is$empty75(l2563)) {
        $ans566 = empty84;
    } else {
        $ans566 = function lam_18(arg_564) {
            return link50(arg_564, map2_n565(f560, _runtime["_plus"](n561, 1, _runtime["$errCallbacks"]), l1562["tail"](), l2563["tail"]()));
        }(f560(n561, l1562["head"](), l2563["head"]()));
    }
    return $ans566;
};
var map3_n574 = function lam_map3_n576(f568, n569, l1570, l2571, l3572) {
    if (is$empty75(l1570) || is$empty75(l2571) || is$empty75(l3572)) {
        $ans575 = empty84;
    } else {
        $ans575 = function lam_18(arg_573) {
            return link50(arg_573, map3_n574(f568, _runtime["_plus"](n569, 1, _runtime["$errCallbacks"]), l1570["tail"](), l2571["tail"](), l3572["tail"]()));
        }(f568(n569, l1570["head"](), l2571["head"](), l3572["head"]()));
    }
    return $ans575;
};
var map4_n584 = function lam_map4_n586(f577, n578, l1579, l2580, l3581, l4582) {
    if (is$empty75(l1579) || is$empty75(l2580) || is$empty75(l3581) || is$empty75(l4582)) {
        $ans585 = empty84;
    } else {
        $ans585 = function lam_18(arg_583) {
            return link50(arg_583, map4_n584(f577, _runtime["_plus"](n578, 1, _runtime["$errCallbacks"]), l1579["tail"](), l2580["tail"](), l3581["tail"](), l4582["tail"]()));
        }(f577(n578, l1579["head"](), l2580["head"](), l3581["head"](), l4582["head"]()));
    }
    return $ans585;
};
var each2595 = function lam_each2594(f587, lst1592, lst2593) {
    var help590 = function lam_help293(l1588, l2589) {
        if (is$empty75(l1588) || is$empty75(l2589)) {
            $ans591 = nothing278;
        } else {
            f587(l1588["head"](), l2589["head"]());
            help590(l1588["tail"](), l2589["tail"]());
            $ans591 = help590(l1588["tail"](), l2589["tail"]());
        }
        return $ans591;
    };
    return help590(lst1592, lst2593);
};
var each3606 = function lam_each3605(f596, lst1602, lst2603, lst3604) {
    var help600 = function lam_help293(l1597, l2598, l3599) {
        if (is$empty75(l1597) || is$empty75(l2598) || is$empty75(l3599)) {
            $ans601 = nothing278;
        } else {
            f596(l1597["head"](), l2598["head"](), l3599["head"]());
            help600(l1597["tail"](), l2598["tail"](), l3599["tail"]());
            $ans601 = help600(l1597["tail"](), l2598["tail"](), l3599["tail"]());
        }
        return $ans601;
    };
    return help600(lst1602, lst2603, lst3604);
};
var each4619 = function lam_each4618(f607, lst1614, lst2615, lst3616, lst4617) {
    var help612 = function lam_help293(l1608, l2609, l3610, l4611) {
        if (is$empty75(l1608) || is$empty75(l2609) || is$empty75(l3610) || is$empty75(l4611)) {
            $ans613 = nothing278;
        } else {
            f607(l1608["head"](), l2609["head"](), l3610["head"](), l4611["head"]());
            help612(l1608["tail"](), l2609["tail"](), l3610["tail"](), l4611["tail"]());
            $ans613 = help612(l1608["tail"](), l2609["tail"](), l3610["tail"](), l4611["tail"]());
        }
        return $ans613;
    };
    return help612(lst1614, lst2615, lst3616, lst4617);
};
var each_n628 = function lam_each_n627(f620, num625, lst626) {
    var help623 = function lam_help293(n621, l622) {
        if (is$empty75(l622)) {
            $ans624 = nothing278;
        } else {
            f620(n621, l622["head"]());
            help623(_runtime["_plus"](n621, 1, _runtime["$errCallbacks"]), l622["tail"]());
            $ans624 = help623(_runtime["_plus"](n621, 1, _runtime["$errCallbacks"]), l622["tail"]());
        }
        return $ans624;
    };
    return help623(num625, lst626);
};
var each2_n639 = function lam_each2_n638(f629, num635, lst1636, lst2637) {
    var help633 = function lam_help293(n630, l1631, l2632) {
        if (is$empty75(l1631) || is$empty75(l2632)) {
            $ans634 = nothing278;
        } else {
            f629(n630, l1631["head"](), l2632["head"]());
            help633(_runtime["_plus"](n630, 1, _runtime["$errCallbacks"]), l1631["tail"](), l2632["tail"]());
            $ans634 = help633(_runtime["_plus"](n630, 1, _runtime["$errCallbacks"]), l1631["tail"](), l2632["tail"]());
        }
        return $ans634;
    };
    return help633(num635, lst1636, lst2637);
};
var each3_n652 = function lam_each3_n651(f640, num647, lst1648, lst2649, lst3650) {
    var help645 = function lam_help293(n641, l1642, l2643, l3644) {
        if (is$empty75(l1642) || is$empty75(l2643) || is$empty75(l3644)) {
            $ans646 = nothing278;
        } else {
            f640(n641, l1642["head"](), l2643["head"](), l3644["head"]());
            help645(_runtime["_plus"](n641, 1, _runtime["$errCallbacks"]), l1642["tail"](), l2643["tail"](), l3644["tail"]());
            $ans646 = help645(_runtime["_plus"](n641, 1, _runtime["$errCallbacks"]), l1642["tail"](), l2643["tail"](), l3644["tail"]());
        }
        return $ans646;
    };
    return help645(num647, lst1648, lst2649, lst3650);
};
var each4_n667 = function lam_each4_n666(f653, num661, lst1662, lst2663, lst3664, lst4665) {
    var help659 = function lam_help293(n654, l1655, l2656, l3657, l4658) {
        if (is$empty75(l1655) || is$empty75(l2656) || is$empty75(l3657) || is$empty75(l4658)) {
            $ans660 = nothing278;
        } else {
            f653(n654, l1655["head"](), l2656["head"](), l3657["head"](), l4658["head"]());
            help659(_runtime["_plus"](n654, 1, _runtime["$errCallbacks"]), l1655["tail"](), l2656["tail"](), l3657["tail"](), l4658["tail"]());
            $ans660 = help659(_runtime["_plus"](n654, 1, _runtime["$errCallbacks"]), l1655["tail"](), l2656["tail"](), l3657["tail"](), l4658["tail"]());
        }
        return $ans660;
    };
    return help659(num661, lst1662, lst2663, lst3664, lst4665);
};
var fold$while676 = function lam_fold$while680(f672, base671, lst668) {
    var cases669 = lst668;
    var $ans670;
    switch (cases669["$name"]) {
    case "empty":
        $ans670 = base671;
        break;
    case "link":
        var elt673 = cases669[cases669["$fieldNames"][0]];
        var r678 = cases669[cases669["$fieldNames"][1]];
        var cases674 = f672(base671, elt673);
        var $ans675;
        switch (cases674["$name"]) {
        case "left":
            var v677 = cases674[cases674["$fieldNames"][0]];
            $ans675 = fold$while676(f672, v677, r678);
            break;
        case "right":
            var v679 = cases674[cases674["$fieldNames"][0]];
            $ans675 = v679;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                869,
                6,
                31177,
                872,
                9,
                31279
            ], cases674);
            $ans675 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                869,
                6,
                31177,
                872,
                9,
                31279
            ], cases674);
        }
        $ans670 = $ans675;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            866,
            2,
            31112,
            873,
            5,
            31285
        ], cases669);
        $ans670 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            866,
            2,
            31112,
            873,
            5,
            31285
        ], cases669);
    }
    return $ans670;
};
var foldr14 = function lam_foldr20(f681, base682, lst683) {
    return LP208["perf-foldr"](f681, base682, typecast207(lst683));
};
var fold2684 = function lam_fold2690(f685, base686, l1687, l2688) {
    if (is$empty75(l1687) || is$empty75(l2688)) {
        $ans689 = base686;
    } else {
        $ans689 = fold2684(f685, f685(base686, l1687["head"](), l2688["head"]()), l1687["tail"](), l2688["tail"]());
    }
    return $ans689;
};
var fold3691 = function lam_fold3698(f692, base693, l1694, l2695, l3696) {
    if (is$empty75(l1694) || is$empty75(l2695) || is$empty75(l3696)) {
        $ans697 = base693;
    } else {
        $ans697 = fold3691(f692, f692(base693, l1694["head"](), l2695["head"](), l3696["head"]()), l1694["tail"](), l2695["tail"](), l3696["tail"]());
    }
    return $ans697;
};
var fold4699 = function lam_fold4707(f700, base701, l1702, l2703, l3704, l4705) {
    if (is$empty75(l1702) || is$empty75(l2703) || is$empty75(l3704) || is$empty75(l4705)) {
        $ans706 = base701;
    } else {
        $ans706 = fold4699(f700, f700(base701, l1702["head"](), l2703["head"](), l3704["head"](), l4705["head"]()), l1702["tail"](), l2703["tail"](), l3704["tail"](), l4705["tail"]());
    }
    return $ans706;
};
var fold_n718 = function lam_fold_n717(f710, num714, base715, lst716) {
    var help708 = function lam_help293(n709, acc711, partial$list712) {
        if (is$empty75(partial$list712)) {
            $ans713 = acc711;
        } else {
            $ans713 = help708(_runtime["_plus"](n709, 1, _runtime["$errCallbacks"]), f710(n709, acc711, partial$list712["head"]()), partial$list712["tail"]());
        }
        return $ans713;
    };
    return help708(num714, base715, lst716);
};
var member$with728 = function lam_member$with730(lst719, elt722, eq723) {
    var cases720 = lst719;
    var $ans721;
    switch (cases720["$name"]) {
    case "empty":
        $ans721 = equality190["NotEqual"]("list", elt722, lst719);
        break;
    case "link":
        var first724 = cases720[cases720["$fieldNames"][0]];
        var rest729 = cases720[cases720["$fieldNames"][1]];
        var first$elt$equal725 = eq723(first724, elt722);
        var cases726 = first$elt$equal725;
        var $ans727;
        switch (cases726["$name"]) {
        case "Equal":
            $ans727 = equality190["Equal"];
            break;
        default:
            $ans727 = equality190["equal-or"](first$elt$equal725, member$with728(rest729, elt722, eq723));
        }
        $ans721 = $ans727;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            934,
            2,
            33992,
            942,
            5,
            34301
        ], cases720);
        $ans721 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            934,
            2,
            33992,
            942,
            5,
            34301
        ], cases720);
    }
    return $ans721;
};
var member3737 = function lam_member3736(lst731, elt732) {
    return member$with728(lst731, elt732, function lam_18(l734, r735) {
        return equal$always3733(l734, r735);
    });
};
var member740 = function lam_member11(lst738, elt739) {
    return equality190["to-boolean"](member3737(lst738, elt739));
};
var member$now3746 = function lam_member$now3745(lst741, elt742) {
    return member$with728(lst741, elt742, function lam_18(l743, r744) {
        return equality190["equal-now3"](l743, r744);
    });
};
var member$now750 = function lam_member$now749(lst747, elt748) {
    return equality190["to-boolean"](member$now3746(lst747, elt748));
};
var member$identical3757 = function lam_member$identical3756(lst751, elt752) {
    return member$with728(lst751, elt752, function lam_18(l754, r755) {
        return identical3753(l754, r755);
    });
};
var member$identical761 = function lam_member$identical760(lst758, elt759) {
    return equality190["to-boolean"](member$identical3757(lst758, elt759));
};
var shuffle771 = function lam_shuffle770(lst767) {
    if (is$empty75(lst767)) {
        $ans769 = empty84;
    } else {
        var elts768 = fold_n718(function lam_loop766(i764, arr763, e765) {
            var ix762 = raise58("TODO(alex): Implement random generator somewhere");
            RA202["raw-array-set"](arr763, i764, RA202["raw-array-get"](arr763, ix762));
            RA202["raw-array-set"](arr763, ix762, e765);
            arr763;
            return arr763;
        }, 1, RA202["raw-array-of"](lst767["head"](), lst767["length"]()), lst767["tail"]());
        $ans769 = raw$array$to$list211(elts768);
    }
    return $ans769;
};
var filter$map775 = function lam_filter$map774(f772, lst773) {
    return typecast207(LP208["perf-filter-map"](f772, typecast207(lst773)));
};
var filter$values782 = function lam_filter$values785(lst776) {
    var cases777 = lst776;
    var $ans778;
    switch (cases777["$name"]) {
    case "empty":
        $ans778 = empty84;
        break;
    case "link":
        var first779 = cases777[cases777["$fieldNames"][0]];
        var rest783 = cases777[cases777["$fieldNames"][1]];
        var cases780 = first779;
        var $ans781;
        switch (cases780["$name"]) {
        case "none":
            $ans781 = filter$values782(rest783);
            break;
        case "some":
            var v784 = cases780[cases780["$fieldNames"][0]];
            $ans781 = link50(v784, filter$values782(rest783));
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                992,
                6,
                35786,
                995,
                9,
                35904
            ], cases780);
            $ans781 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                992,
                6,
                35786,
                995,
                9,
                35904
            ], cases780);
        }
        $ans778 = $ans781;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            989,
            2,
            35715,
            996,
            5,
            35910
        ], cases777);
        $ans778 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            989,
            2,
            35715,
            996,
            5,
            35910
        ], cases777);
    }
    return $ans778;
};
var distinct793 = function lam_distinct800(l786) {
    var cases787 = l786;
    var $ans788;
    switch (cases787["$name"]) {
    case "empty":
        $ans788 = empty84;
        break;
    case "link":
        var first790 = cases787[cases787["$fieldNames"][0]];
        var rest789 = cases787[cases787["$fieldNames"][1]];
        var cases791 = member3737(rest789, first790);
        var $ans792;
        switch (cases791["$name"]) {
        case "NotEqual":
            var $underscore794 = cases791[cases791["$fieldNames"][0]];
            var $underscore795 = cases791[cases791["$fieldNames"][1]];
            var $underscore796 = cases791[cases791["$fieldNames"][2]];
            $ans792 = link50(first790, distinct793(rest789));
            break;
        case "Unknown":
            var $underscore797 = cases791[cases791["$fieldNames"][0]];
            var $underscore798 = cases791[cases791["$fieldNames"][1]];
            var $underscore799 = cases791[cases791["$fieldNames"][2]];
            $ans792 = link50(first790, distinct793(rest789));
            break;
        case "Equal":
            $ans792 = distinct793(rest789);
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1004,
                6,
                36138,
                1008,
                9,
                36342
            ], cases791);
            $ans792 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1004,
                6,
                36138,
                1008,
                9,
                36342
            ], cases791);
        }
        $ans788 = $ans792;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            1001,
            2,
            36068,
            1009,
            5,
            36348
        ], cases787);
        $ans788 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            1001,
            2,
            36068,
            1009,
            5,
            36348
        ], cases787);
    }
    return $ans788;
};
var take$while812 = function lam_take$while811(pred806, lst810) {
    var tail801 = empty84;
    var help808 = function lam_help293(l802) {
        var cases803 = l802;
        var $ans804;
        switch (cases803["$name"]) {
        case "empty":
            $ans804 = empty84;
            break;
        case "link":
            var first807 = cases803[cases803["$fieldNames"][0]];
            var rest809 = cases803[cases803["$fieldNames"][1]];
            if (pred806(first807)) {
                $ans805 = link50(first807, help808(rest809));
            } else {
                tail801 = l802;
                empty84;
                $ans805 = empty84;
            }
            $ans804 = $ans805;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1016,
                4,
                36576,
                1025,
                7,
                36776
            ], cases803);
            $ans804 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1016,
                4,
                36576,
                1025,
                7,
                36776
            ], cases803);
        }
        return $ans804;
    };
    return _runtime["PTuple"]([
        help808(lst810),
        tail801
    ]);
};
var $underscore829 = undefined;
var max847 = function lam_max846(lst830) {
    var cases831 = lst830;
    var $ans832;
    switch (cases831["$name"]) {
    case "empty":
        $ans832 = raise58("list max: empty list");
        break;
    case "link":
        var first833 = cases831[cases831["$fieldNames"][0]];
        var rest834 = cases831[cases831["$fieldNames"][1]];
        $ans832 = _runtime["PTuple"]([
            first833,
            rest834
        ]);
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            1041,
            26,
            37546,
            1044,
            5,
            37656
        ], cases831);
        $ans832 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            1041,
            26,
            37546,
            1044,
            5,
            37656
        ], cases831);
    }
    var tup835 = $ans832;
    var max$v836 = tup835[0];
    var lst837 = tup835[1];
    var helper842 = function lam_helper449(inner838, inner$max841) {
        var cases839 = inner838;
        var $ans840;
        switch (cases839["$name"]) {
        case "empty":
            $ans840 = inner$max841;
            break;
        case "link":
            var first845 = cases839[cases839["$fieldNames"][0]];
            var rest843 = cases839[cases839["$fieldNames"][1]];
            if (_runtime["_greaterthan"](first845, inner$max841, _runtime["$errCallbacks"])) {
                $ans844 = helper842(rest843, first845);
            } else {
                $ans844 = helper842(rest843, inner$max841);
            }
            $ans840 = $ans844;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1047,
                4,
                37730,
                1055,
                7,
                37932
            ], cases839);
            $ans840 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1047,
                4,
                37730,
                1055,
                7,
                37932
            ], cases839);
        }
        return $ans840;
    };
    return helper842(lst837, max$v836);
};
var min865 = function lam_min864(lst848) {
    var cases849 = lst848;
    var $ans850;
    switch (cases849["$name"]) {
    case "empty":
        $ans850 = raise58("list max: empty list");
        break;
    case "link":
        var first851 = cases849[cases849["$fieldNames"][0]];
        var rest852 = cases849[cases849["$fieldNames"][1]];
        $ans850 = _runtime["PTuple"]([
            first851,
            rest852
        ]);
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            1062,
            26,
            38032,
            1065,
            5,
            38142
        ], cases849);
        $ans850 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
            1062,
            26,
            38032,
            1065,
            5,
            38142
        ], cases849);
    }
    var tup853 = $ans850;
    var min$v854 = tup853[0];
    var lst855 = tup853[1];
    var helper860 = function lam_helper449(inner856, inner$min859) {
        var cases857 = inner856;
        var $ans858;
        switch (cases857["$name"]) {
        case "empty":
            $ans858 = inner$min859;
            break;
        case "link":
            var first863 = cases857[cases857["$fieldNames"][0]];
            var rest861 = cases857[cases857["$fieldNames"][1]];
            if (_runtime["_lessthan"](first863, inner$min859, _runtime["$errCallbacks"])) {
                $ans862 = helper860(rest861, first863);
            } else {
                $ans862 = helper860(rest861, inner$min859);
            }
            $ans858 = $ans862;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1068,
                4,
                38216,
                1076,
                7,
                38418
            ], cases857);
            $ans858 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1068,
                4,
                38216,
                1076,
                7,
                38418
            ], cases857);
        }
        return $ans858;
    };
    return helper860(lst855, min$v854);
};
_runtime["trace-value"]([
    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
    1083,
    0,
    38553,
    1088,
    2,
    38638
], LP208["setup"]({
    "is-link": is$link200,
    "is-empty": is$empty75,
    "empty": empty84,
    "link": link50,
    "$methods": {}
}));
var member$always3866 = member3737;
var member$always867 = member740;
var foldl868 = fold23;
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:343:0-343:6", "join-str", function $check$blockjoin$str243() {
    _runtime["$checkTest"](function $LHS() {
        return join$str156(raw$array$to$list211(RA202["raw-array"]["make"]([
            1,
            2,
            3
        ])), "+");
    }, function $RHS() {
        return "1+2+3";
    }, function $TEST($lhs235, $rhs236) {
        if ($lhs235["exception"]) {
            return {
                "success": false,
                "lhs": $lhs235,
                "rhs": $rhs236
            };
        }
        if ($rhs236["exception"]) {
            return {
                "success": false,
                "lhs": $lhs235,
                "rhs": $rhs236
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs235["value"], $rhs236["value"]),
            "lhs": $lhs235,
            "rhs": $rhs236
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:346:2-346:70");
    _runtime["$checkTest"](function $LHS() {
        return join$str156(raw$array$to$list211(RA202["raw-array"]["make"]([])), "+");
    }, function $RHS() {
        return "";
    }, function $TEST($lhs237, $rhs238) {
        if ($lhs237["exception"]) {
            return {
                "success": false,
                "lhs": $lhs237,
                "rhs": $rhs238
            };
        }
        if ($rhs238["exception"]) {
            return {
                "success": false,
                "lhs": $lhs237,
                "rhs": $rhs238
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs237["value"], $rhs238["value"]),
            "lhs": $lhs237,
            "rhs": $rhs238
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:347:2-347:58");
    _runtime["$checkTest"](function $LHS() {
        return join$str156(raw$array$to$list211(RA202["raw-array"]["make"]([1])), "+");
    }, function $RHS() {
        return "1";
    }, function $TEST($lhs239, $rhs240) {
        if ($lhs239["exception"]) {
            return {
                "success": false,
                "lhs": $lhs239,
                "rhs": $rhs240
            };
        }
        if ($rhs240["exception"]) {
            return {
                "success": false,
                "lhs": $lhs239,
                "rhs": $rhs240
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs239["value"], $rhs240["value"]),
            "lhs": $lhs239,
            "rhs": $rhs240
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:348:2-348:60");
    _runtime["$checkTest"](function $LHS() {
        return join$str156(raw$array$to$list211(RA202["raw-array"]["make"]([
            1,
            2
        ])), "+");
    }, function $RHS() {
        return "1+2";
    }, function $TEST($lhs241, $rhs242) {
        if ($lhs241["exception"]) {
            return {
                "success": false,
                "lhs": $lhs241,
                "rhs": $rhs242
            };
        }
        if ($rhs242["exception"]) {
            return {
                "success": false,
                "lhs": $lhs241,
                "rhs": $rhs242
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs241["value"], $rhs242["value"]),
            "lhs": $lhs241,
            "rhs": $rhs242
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:349:2-349:65");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:366:0-366:6", "join-str-last", function $check$blockjoin$str$last267() {
    _runtime["$checkTest"](function $LHS() {
        return join$str$last161(raw$array$to$list211(RA202["raw-array"]["make"]([
            1,
            2,
            3
        ])), "+", "-");
    }, function $RHS() {
        return "1+2-3";
    }, function $TEST($lhs257, $rhs258) {
        if ($lhs257["exception"]) {
            return {
                "success": false,
                "lhs": $lhs257,
                "rhs": $rhs258
            };
        }
        if ($rhs258["exception"]) {
            return {
                "success": false,
                "lhs": $lhs257,
                "rhs": $rhs258
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs257["value"], $rhs258["value"]),
            "lhs": $lhs257,
            "rhs": $rhs258
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:369:2-369:80");
    _runtime["$checkTest"](function $LHS() {
        return join$str$last161(raw$array$to$list211(RA202["raw-array"]["make"]([])), "+", "-");
    }, function $RHS() {
        return "";
    }, function $TEST($lhs259, $rhs260) {
        if ($lhs259["exception"]) {
            return {
                "success": false,
                "lhs": $lhs259,
                "rhs": $rhs260
            };
        }
        if ($rhs260["exception"]) {
            return {
                "success": false,
                "lhs": $lhs259,
                "rhs": $rhs260
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs259["value"], $rhs260["value"]),
            "lhs": $lhs259,
            "rhs": $rhs260
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:370:2-370:68");
    _runtime["$checkTest"](function $LHS() {
        return join$str$last161(raw$array$to$list211(RA202["raw-array"]["make"]([1])), "+", "-");
    }, function $RHS() {
        return "1";
    }, function $TEST($lhs261, $rhs262) {
        if ($lhs261["exception"]) {
            return {
                "success": false,
                "lhs": $lhs261,
                "rhs": $rhs262
            };
        }
        if ($rhs262["exception"]) {
            return {
                "success": false,
                "lhs": $lhs261,
                "rhs": $rhs262
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs261["value"], $rhs262["value"]),
            "lhs": $lhs261,
            "rhs": $rhs262
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:371:2-371:70");
    _runtime["$checkTest"](function $LHS() {
        return join$str$last161(raw$array$to$list211(RA202["raw-array"]["make"]([
            1,
            2
        ])), "+", "-");
    }, function $RHS() {
        return "1-2";
    }, function $TEST($lhs263, $rhs264) {
        if ($lhs263["exception"]) {
            return {
                "success": false,
                "lhs": $lhs263,
                "rhs": $rhs264
            };
        }
        if ($rhs264["exception"]) {
            return {
                "success": false,
                "lhs": $lhs263,
                "rhs": $rhs264
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs263["value"], $rhs264["value"]),
            "lhs": $lhs263,
            "rhs": $rhs264
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:372:2-372:75");
    _runtime["$checkTest"](function $LHS() {
        return join$str$last161(raw$array$to$list211(RA202["raw-array"]["make"]([
            1,
            2,
            3,
            4
        ])), "+", "-");
    }, function $RHS() {
        return "1+2+3-4";
    }, function $TEST($lhs265, $rhs266) {
        if ($lhs265["exception"]) {
            return {
                "success": false,
                "lhs": $lhs265,
                "rhs": $rhs266
            };
        }
        if ($rhs266["exception"]) {
            return {
                "success": false,
                "lhs": $lhs265,
                "rhs": $rhs266
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs265["value"], $rhs266["value"]),
            "lhs": $lhs265,
            "rhs": $rhs266
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:373:2-373:85");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:431:0-431:6", "reverse", function $check$blockreverse306() {
    _runtime["$checkTest"](function $LHS() {
        return reverse119(raw$array$to$list211(RA202["raw-array"]["make"]([])));
    }, function $RHS() {
        return raw$array$to$list211(RA202["raw-array"]["make"]([]));
    }, function $TEST($lhs302, $rhs303) {
        if ($lhs302["exception"]) {
            return {
                "success": false,
                "lhs": $lhs302,
                "rhs": $rhs303
            };
        }
        if ($rhs303["exception"]) {
            return {
                "success": false,
                "lhs": $lhs302,
                "rhs": $rhs303
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs302["value"], $rhs303["value"]),
            "lhs": $lhs302,
            "rhs": $rhs303
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:432:2-432:85");
    _runtime["$checkTest"](function $LHS() {
        return reverse119(raw$array$to$list211(RA202["raw-array"]["make"]([
            1,
            3
        ])));
    }, function $RHS() {
        return raw$array$to$list211(RA202["raw-array"]["make"]([
            3,
            1
        ]));
    }, function $TEST($lhs304, $rhs305) {
        if ($lhs304["exception"]) {
            return {
                "success": false,
                "lhs": $lhs304,
                "rhs": $rhs305
            };
        }
        if ($rhs305["exception"]) {
            return {
                "success": false,
                "lhs": $lhs304,
                "rhs": $rhs305
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs304["value"], $rhs305["value"]),
            "lhs": $lhs304,
            "rhs": $rhs305
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:433:2-433:93");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:567:0-567:6", "same-length", function $check$blocksame$length404() {
    _runtime["$checkTest"](function $LHS() {
        return same$length397(list392["make"]([
            1,
            2
        ]), list392["make"]([
            true,
            false
        ]));
    }, function $RHS() {
        return true;
    }, function $TEST($lhs398, $rhs399) {
        if ($lhs398["exception"]) {
            return {
                "success": false,
                "lhs": $lhs398,
                "rhs": $rhs399
            };
        }
        if ($rhs399["exception"]) {
            return {
                "success": false,
                "lhs": $lhs398,
                "rhs": $rhs399
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs398["value"], $rhs399["value"]),
            "lhs": $lhs398,
            "rhs": $rhs399
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:568:2-568:56");
    _runtime["$checkTest"](function $LHS() {
        return same$length397(list392["make"]([
            1,
            2,
            3
        ]), list392["make"]([
            true,
            false
        ]));
    }, function $RHS() {
        return false;
    }, function $TEST($lhs400, $rhs401) {
        if ($lhs400["exception"]) {
            return {
                "success": false,
                "lhs": $lhs400,
                "rhs": $rhs401
            };
        }
        if ($rhs401["exception"]) {
            return {
                "success": false,
                "lhs": $lhs400,
                "rhs": $rhs401
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs400["value"], $rhs401["value"]),
            "lhs": $lhs400,
            "rhs": $rhs401
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:569:2-569:60");
    _runtime["$checkTest"](function $LHS() {
        return same$length397(list392["make"]([]), list392["make"]([
            true,
            false
        ]));
    }, function $RHS() {
        return false;
    }, function $TEST($lhs402, $rhs403) {
        if ($lhs402["exception"]) {
            return {
                "success": false,
                "lhs": $lhs402,
                "rhs": $rhs403
            };
        }
        if ($rhs403["exception"]) {
            return {
                "success": false,
                "lhs": $lhs402,
                "rhs": $rhs403
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs402["value"], $rhs403["value"]),
            "lhs": $lhs402,
            "rhs": $rhs403
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:570:2-570:53");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:579:0-579:6", "longer-than", function $check$blocklonger$than420() {
    _runtime["$checkTest"](function $LHS() {
        return longer$than410(list392["make"]([
            1,
            2,
            3
        ]), 2);
    }, function $RHS() {
        return true;
    }, function $TEST($lhs414, $rhs415) {
        if ($lhs414["exception"]) {
            return {
                "success": false,
                "lhs": $lhs414,
                "rhs": $rhs415
            };
        }
        if ($rhs415["exception"]) {
            return {
                "success": false,
                "lhs": $lhs414,
                "rhs": $rhs415
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs414["value"], $rhs415["value"]),
            "lhs": $lhs414,
            "rhs": $rhs415
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:580:2-580:41");
    _runtime["$checkTest"](function $LHS() {
        return longer$than410(list392["make"]([
            1,
            2,
            3
        ]), 4);
    }, function $RHS() {
        return false;
    }, function $TEST($lhs416, $rhs417) {
        if ($lhs416["exception"]) {
            return {
                "success": false,
                "lhs": $lhs416,
                "rhs": $rhs417
            };
        }
        if ($rhs417["exception"]) {
            return {
                "success": false,
                "lhs": $lhs416,
                "rhs": $rhs417
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs416["value"], $rhs417["value"]),
            "lhs": $lhs416,
            "rhs": $rhs417
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:581:2-581:42");
    _runtime["$checkTest"](function $LHS() {
        return longer$than410(list392["make"]([]), 0);
    }, function $RHS() {
        return false;
    }, function $TEST($lhs418, $rhs419) {
        if ($lhs418["exception"]) {
            return {
                "success": false,
                "lhs": $lhs418,
                "rhs": $rhs419
            };
        }
        if ($rhs419["exception"]) {
            return {
                "success": false,
                "lhs": $lhs418,
                "rhs": $rhs419
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs418["value"], $rhs419["value"]),
            "lhs": $lhs418,
            "rhs": $rhs419
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:582:2-582:34");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:591:0-591:6", "shorter-than", function $check$blockshorter$than436() {
    _runtime["$checkTest"](function $LHS() {
        return shorter$than426(list392["make"]([
            1,
            2,
            3
        ]), 2);
    }, function $RHS() {
        return false;
    }, function $TEST($lhs430, $rhs431) {
        if ($lhs430["exception"]) {
            return {
                "success": false,
                "lhs": $lhs430,
                "rhs": $rhs431
            };
        }
        if ($rhs431["exception"]) {
            return {
                "success": false,
                "lhs": $lhs430,
                "rhs": $rhs431
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs430["value"], $rhs431["value"]),
            "lhs": $lhs430,
            "rhs": $rhs431
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:592:2-592:43");
    _runtime["$checkTest"](function $LHS() {
        return shorter$than426(list392["make"]([
            1,
            2,
            3
        ]), 4);
    }, function $RHS() {
        return true;
    }, function $TEST($lhs432, $rhs433) {
        if ($lhs432["exception"]) {
            return {
                "success": false,
                "lhs": $lhs432,
                "rhs": $rhs433
            };
        }
        if ($rhs433["exception"]) {
            return {
                "success": false,
                "lhs": $lhs432,
                "rhs": $rhs433
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs432["value"], $rhs433["value"]),
            "lhs": $lhs432,
            "rhs": $rhs433
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:593:2-593:42");
    _runtime["$checkTest"](function $LHS() {
        return shorter$than426(list392["make"]([]), 0);
    }, function $RHS() {
        return false;
    }, function $TEST($lhs434, $rhs435) {
        if ($lhs434["exception"]) {
            return {
                "success": false,
                "lhs": $lhs434,
                "rhs": $rhs435
            };
        }
        if ($rhs435["exception"]) {
            return {
                "success": false,
                "lhs": $lhs434,
                "rhs": $rhs435
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs434["value"], $rhs435["value"]),
            "lhs": $lhs434,
            "rhs": $rhs435
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:594:2-594:35");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:648:0-648:6", "range-by", function $check$blockrange$by486() {
    _runtime["$checkTest"](function $LHS() {
        return range$by473(1, 10, 4);
    }, function $RHS() {
        return list392["make"]([
            1,
            5,
            9
        ]);
    }, function $TEST($lhs474, $rhs475) {
        if ($lhs474["exception"]) {
            return {
                "success": false,
                "lhs": $lhs474,
                "rhs": $rhs475
            };
        }
        if ($rhs475["exception"]) {
            return {
                "success": false,
                "lhs": $lhs474,
                "rhs": $rhs475
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs474["value"], $rhs475["value"]),
            "lhs": $lhs474,
            "rhs": $rhs475
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:649:2-649:39");
    _runtime["$checkTest"](function $LHS() {
        return range$by473(10, 1, -4);
    }, function $RHS() {
        return list392["make"]([
            10,
            6,
            2
        ]);
    }, function $TEST($lhs476, $rhs477) {
        if ($lhs476["exception"]) {
            return {
                "success": false,
                "lhs": $lhs476,
                "rhs": $rhs477
            };
        }
        if ($rhs477["exception"]) {
            return {
                "success": false,
                "lhs": $lhs476,
                "rhs": $rhs477
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs476["value"], $rhs477["value"]),
            "lhs": $lhs476,
            "rhs": $rhs477
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:650:2-650:41");
    _runtime["$checkTest"](function $LHS() {
        return range$by473(3, 20, 9);
    }, function $RHS() {
        return list392["make"]([
            3,
            12
        ]);
    }, function $TEST($lhs478, $rhs479) {
        if ($lhs478["exception"]) {
            return {
                "success": false,
                "lhs": $lhs478,
                "rhs": $rhs479
            };
        }
        if ($rhs479["exception"]) {
            return {
                "success": false,
                "lhs": $lhs478,
                "rhs": $rhs479
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs478["value"], $rhs479["value"]),
            "lhs": $lhs478,
            "rhs": $rhs479
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:651:2-651:37");
    _runtime["$checkTest"](function $LHS() {
        return range$by473(20, 3, 9);
    }, function $RHS() {
        return empty84;
    }, function $TEST($lhs480, $rhs481) {
        if ($lhs480["exception"]) {
            return {
                "success": false,
                "lhs": $lhs480,
                "rhs": $rhs481
            };
        }
        if ($rhs481["exception"]) {
            return {
                "success": false,
                "lhs": $lhs480,
                "rhs": $rhs481
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs480["value"], $rhs481["value"]),
            "lhs": $lhs480,
            "rhs": $rhs481
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:652:2-652:29");
    _runtime["$checkTest"](function $LHS() {
        return range$by473(20, 3, -9);
    }, function $RHS() {
        return list392["make"]([
            20,
            11
        ]);
    }, function $TEST($lhs482, $rhs483) {
        if ($lhs482["exception"]) {
            return {
                "success": false,
                "lhs": $lhs482,
                "rhs": $rhs483
            };
        }
        if ($rhs483["exception"]) {
            return {
                "success": false,
                "lhs": $lhs482,
                "rhs": $rhs483
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs482["value"], $rhs483["value"]),
            "lhs": $lhs482,
            "rhs": $rhs483
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:653:2-653:39");
    _runtime["$checkTest"](function $LHS() {
        return range$by473(2, 3, 0);
    }, function $RHS() {
        return "interval of 0";
    }, function $TEST($lhs484, $rhs485) {
        return {
            "success": $lhs484["exception"] && _runtime["$torepr"](_runtime["$raiseExtract"]($lhs484["exception_val"]))["includes"]($rhs485["value"]),
            "lhs": $lhs484,
            "rhs": {
                "value": undefined,
                "exception": true,
                "exception_value": $rhs485["value"]
            }
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:654:2-654:42");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:1028:0-1028:6", "take-while", function $check$blocktake$while828() {
    _runtime["$checkTest"](function $LHS() {
        return take$while812(function lam_18(x813) {
            return _runtime["_greaterthan"](x813, 0, _runtime["$errCallbacks"]);
        }, list392["make"]([
            5,
            3,
            1,
            0,
            1,
            2,
            3
        ]));
    }, function $RHS() {
        return _runtime["PTuple"]([
            list392["make"]([
                5,
                3,
                1
            ]),
            list392["make"]([
                0,
                1,
                2,
                3
            ])
        ]);
    }, function $TEST($lhs814, $rhs815) {
        if ($lhs814["exception"]) {
            return {
                "success": false,
                "lhs": $lhs814,
                "rhs": $rhs815
            };
        }
        if ($rhs815["exception"]) {
            return {
                "success": false,
                "lhs": $lhs814,
                "rhs": $rhs815
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs814["value"], $rhs815["value"]),
            "lhs": $lhs814,
            "rhs": $rhs815
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:1033:2-1033:113");
    _runtime["$checkTest"](function $LHS() {
        return take$while812(function lam_18(x816) {
            return _runtime["_greaterthan"](x816, 0, _runtime["$errCallbacks"]);
        }, empty84);
    }, function $RHS() {
        return _runtime["PTuple"]([
            empty84,
            empty84
        ]);
    }, function $TEST($lhs817, $rhs818) {
        if ($lhs817["exception"]) {
            return {
                "success": false,
                "lhs": $lhs817,
                "rhs": $rhs818
            };
        }
        if ($rhs818["exception"]) {
            return {
                "success": false,
                "lhs": $lhs817,
                "rhs": $rhs818
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs817["value"], $rhs818["value"]),
            "lhs": $lhs817,
            "rhs": $rhs818
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:1034:2-1034:68");
    _runtime["$checkTest"](function $LHS() {
        return take$while812(function lam_18(x819) {
            return _runtime["_greaterthan"](x819, 0, _runtime["$errCallbacks"]);
        }, list392["make"]([
            0,
            1,
            2,
            3
        ]));
    }, function $RHS() {
        return _runtime["PTuple"]([
            empty84,
            list392["make"]([
                0,
                1,
                2,
                3
            ])
        ]);
    }, function $TEST($lhs820, $rhs821) {
        if ($lhs820["exception"]) {
            return {
                "success": false,
                "lhs": $lhs820,
                "rhs": $rhs821
            };
        }
        if ($rhs821["exception"]) {
            return {
                "success": false,
                "lhs": $lhs820,
                "rhs": $rhs821
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs820["value"], $rhs821["value"]),
            "lhs": $lhs820,
            "rhs": $rhs821
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:1035:2-1035:94");
    _runtime["$checkTest"](function $LHS() {
        return take$while812(function lam_18(x822) {
            return _runtime["_greaterthan"](x822, 0, _runtime["$errCallbacks"]);
        }, list392["make"]([
            5,
            4,
            3,
            2,
            1
        ]));
    }, function $RHS() {
        return _runtime["PTuple"]([
            list392["make"]([
                5,
                4,
                3,
                2,
                1
            ]),
            empty84
        ]);
    }, function $TEST($lhs823, $rhs824) {
        if ($lhs823["exception"]) {
            return {
                "success": false,
                "lhs": $lhs823,
                "rhs": $rhs824
            };
        }
        if ($rhs824["exception"]) {
            return {
                "success": false,
                "lhs": $lhs823,
                "rhs": $rhs824
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs823["value"], $rhs824["value"]),
            "lhs": $lhs823,
            "rhs": $rhs824
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:1036:2-1036:100");
    _runtime["$checkTest"](function $LHS() {
        return take$while812(function lam_18(x825) {
            return _runtime["equal-always"](x825, true);
        }, list392["make"]([
            true,
            true,
            false,
            true
        ]));
    }, function $RHS() {
        return _runtime["PTuple"]([
            list392["make"]([
                true,
                true
            ]),
            list392["make"]([
                false,
                true
            ])
        ]);
    }, function $TEST($lhs826, $rhs827) {
        if ($lhs826["exception"]) {
            return {
                "success": false,
                "lhs": $lhs826,
                "rhs": $rhs827
            };
        }
        if ($rhs827["exception"]) {
            return {
                "success": false,
                "lhs": $lhs826,
                "rhs": $rhs827
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs826["value"], $rhs827["value"]),
            "lhs": $lhs826,
            "rhs": $rhs827
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr:1037:2-1037:126");
    undefined;
});
var $answer869 = _runtime["trace-value"](["dummy location"], nothing278);
module["exports"] = {
    "each2_n": each2_n639,
    "fold": fold23,
    "all": all518,
    "each3_n": each3_n652,
    "each": each114,
    "is-List": is$List199,
    "member-always": member$always867,
    "member-identical": member$identical761,
    "each4_n": each4_n667,
    "reverse": reverse119,
    "to-raw-array": to$raw$array206,
    "member-now": member$now750,
    "remove": remove151,
    "max": max847,
    "map2_n": map2_n565,
    "longer-than": longer$than410,
    "fold-while": fold$while676,
    "shorter-than": shorter$than426,
    "map3_n": map3_n574,
    "length": length2,
    "sort-by": sort$by454,
    "split-at": split$at127,
    "map4_n": map4_n584,
    "take": take502,
    "filter-map": filter$map775,
    "range-by": range$by473,
    "foldl-complicated": foldl$complicated215,
    "partition": partition181,
    "is-empty": is$empty75,
    "foldl": foldl868,
    "map_n": map_n557,
    "take-while": take$while812,
    "any": any511,
    "function-set": function$set145,
    "empty": empty84,
    "same-length": same$length397,
    "join-str": join$str156,
    "drop": drop505,
    "each_n": each_n628,
    "member-now3": member$now3746,
    "fold_n": fold_n718,
    "fold2": fold2684,
    "foldr": foldr14,
    "each2": each2595,
    "all2": all2528,
    "member-identical3": member$identical3757,
    "member-always3": member$always3866,
    "fold3": fold3691,
    "slice": slice328,
    "min": min865,
    "each3": each3606,
    "fold4": fold4699,
    "each4": each4619,
    "join-str-last": join$str$last161,
    "map2": map2533,
    "last": last450,
    "get": get140,
    "raw-array-to-list": raw$array$to$list211,
    "filter-values": filter$values782,
    "map3": map3541,
    "map4": map4550,
    "filter": filter109,
    "member-with": member$with728,
    "member3": member3737,
    "find": find184,
    "shuffle": shuffle771,
    "member": member740,
    "push": push440,
    "append": append498,
    "link": link50,
    "repeat": repeat492,
    "map": map104,
    "distinct": distinct793,
    "range": range462,
    "is-link": is$link200,
    "list": list392,
    "sort": sort456,
    "$answer": $answer869,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr"),
    "$locations": [
        {
            "name": "each2_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                823,
                0,
                29082,
                834,
                3,
                29542
            ]
        },
        {
            "name": "fold",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                421,
                0,
                15083,
                426,
                3,
                15411
            ]
        },
        {
            "name": "all",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                689,
                0,
                23987,
                695,
                3,
                24215
            ]
        },
        {
            "name": "each3_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                836,
                0,
                29544,
                847,
                3,
                30102
            ]
        },
        {
            "name": "each",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                436,
                0,
                15790,
                440,
                3,
                15975
            ]
        },
        {
            "name": "is-List",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                72,
                0,
                2274,
                300,
                3,
                10967
            ]
        },
        {
            "name": "member-always",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1091,
                0,
                38665,
                1091,
                22,
                38687
            ]
        },
        {
            "name": "member-identical",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                965,
                0,
                34932,
                967,
                3,
                35048
            ]
        },
        {
            "name": "each4_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                849,
                0,
                30104,
                860,
                3,
                30760
            ]
        },
        {
            "name": "reverse",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                428,
                0,
                15413,
                434,
                3,
                15788
            ]
        },
        {
            "name": "to-raw-array",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                302,
                0,
                10969,
                304,
                3,
                11104
            ]
        },
        {
            "name": "member-now",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                957,
                0,
                34684,
                959,
                3,
                34788
            ]
        },
        {
            "name": "remove",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                376,
                0,
                13559,
                387,
                3,
                13896
            ]
        },
        {
            "name": "max",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1040,
                0,
                37480,
                1059,
                3,
                37964
            ]
        },
        {
            "name": "map2_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                744,
                0,
                25972,
                751,
                3,
                26318
            ]
        },
        {
            "name": "longer-than",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                573,
                0,
                20420,
                583,
                3,
                20860
            ]
        },
        {
            "name": "fold-while",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                862,
                0,
                30762,
                874,
                3,
                31289
            ]
        },
        {
            "name": "shorter-than",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                585,
                0,
                20862,
                595,
                3,
                21309
            ]
        },
        {
            "name": "map3_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                753,
                0,
                26320,
                760,
                3,
                26739
            ]
        },
        {
            "name": "length",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                559,
                0,
                19843,
                562,
                3,
                19985
            ]
        },
        {
            "name": "sort-by",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                618,
                0,
                21766,
                620,
                3,
                21886
            ]
        },
        {
            "name": "split-at",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                397,
                0,
                14281,
                417,
                3,
                14937
            ]
        },
        {
            "name": "map4_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                762,
                0,
                26741,
                769,
                3,
                27233
            ]
        },
        {
            "name": "take",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                671,
                0,
                23434,
                674,
                3,
                23590
            ]
        },
        {
            "name": "filter-map",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                984,
                0,
                35530,
                986,
                3,
                35654
            ]
        },
        {
            "name": "range-by",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                637,
                0,
                22324,
                655,
                3,
                23063
            ]
        },
        {
            "name": "foldl-complicated",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                313,
                0,
                11432,
                331,
                3,
                11918
            ]
        },
        {
            "name": "partition",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                468,
                0,
                16955,
                488,
                3,
                17623
            ]
        },
        {
            "name": "is-empty",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                73,
                2,
                2290,
                91,
                8,
                3143
            ]
        },
        {
            "name": "foldl",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1092,
                0,
                38688,
                1092,
                12,
                38700
            ]
        },
        {
            "name": "map_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                735,
                0,
                25702,
                742,
                3,
                25970
            ]
        },
        {
            "name": "take-while",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1012,
                0,
                36354,
                1038,
                3,
                37478
            ]
        },
        {
            "name": "any",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                681,
                0,
                23758,
                687,
                3,
                23985
            ]
        },
        {
            "name": "function-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                523,
                0,
                18448,
                541,
                3,
                19058
            ]
        },
        {
            "name": "empty",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                73,
                2,
                2290,
                91,
                8,
                3143
            ]
        },
        {
            "name": "same-length",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                564,
                0,
                19987,
                571,
                3,
                20418
            ]
        },
        {
            "name": "join-str",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                333,
                0,
                11920,
                350,
                3,
                12610
            ]
        },
        {
            "name": "drop",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                676,
                0,
                23592,
                679,
                3,
                23756
            ]
        },
        {
            "name": "each_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                810,
                0,
                28750,
                821,
                3,
                29080
            ]
        },
        {
            "name": "member-now3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                953,
                0,
                34540,
                955,
                3,
                34682
            ]
        },
        {
            "name": "fold_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                913,
                0,
                33060,
                924,
                3,
                33557
            ]
        },
        {
            "name": "fold2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                883,
                0,
                31623,
                891,
                3,
                32042
            ]
        },
        {
            "name": "foldr",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                876,
                0,
                31291,
                881,
                3,
                31621
            ]
        },
        {
            "name": "each2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                771,
                0,
                27235,
                782,
                3,
                27651
            ]
        },
        {
            "name": "all2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                697,
                0,
                24217,
                706,
                3,
                24617
            ]
        },
        {
            "name": "member-identical3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                961,
                0,
                34790,
                963,
                3,
                34930
            ]
        },
        {
            "name": "member-always3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1090,
                0,
                38640,
                1090,
                24,
                38664
            ]
        },
        {
            "name": "fold3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                893,
                0,
                32044,
                901,
                3,
                32521
            ]
        },
        {
            "name": "slice",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                450,
                0,
                16362,
                466,
                3,
                16953
            ]
        },
        {
            "name": "min",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1061,
                0,
                37966,
                1080,
                3,
                38450
            ]
        },
        {
            "name": "each3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                784,
                0,
                27653,
                795,
                3,
                28157
            ]
        },
        {
            "name": "fold4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                903,
                0,
                32523,
                911,
                3,
                33058
            ]
        },
        {
            "name": "each4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                797,
                0,
                28159,
                808,
                3,
                28748
            ]
        },
        {
            "name": "join-str-last",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                352,
                0,
                12612,
                374,
                3,
                13557
            ]
        },
        {
            "name": "map2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                708,
                0,
                24619,
                715,
                3,
                24913
            ]
        },
        {
            "name": "last",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                601,
                0,
                21379,
                616,
                3,
                21764
            ]
        },
        {
            "name": "get",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                505,
                0,
                17975,
                521,
                3,
                18446
            ]
        },
        {
            "name": "raw-array-to-list",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                306,
                0,
                11106,
                309,
                3,
                11286
            ]
        },
        {
            "name": "filter-values",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                988,
                0,
                35656,
                997,
                3,
                35914
            ]
        },
        {
            "name": "map3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                717,
                0,
                24915,
                724,
                3,
                25270
            ]
        },
        {
            "name": "map4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                726,
                0,
                25272,
                733,
                3,
                25700
            ]
        },
        {
            "name": "filter",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                389,
                0,
                13898,
                395,
                3,
                14279
            ]
        },
        {
            "name": "member-with",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                932,
                0,
                33893,
                943,
                3,
                34305
            ]
        },
        {
            "name": "member3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                945,
                0,
                34307,
                947,
                3,
                34440
            ]
        },
        {
            "name": "find",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                490,
                0,
                17625,
                503,
                3,
                17973
            ]
        },
        {
            "name": "shuffle",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                969,
                0,
                35050,
                982,
                3,
                35528
            ]
        },
        {
            "name": "member",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                949,
                0,
                34442,
                951,
                3,
                34538
            ]
        },
        {
            "name": "push",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                597,
                0,
                21311,
                599,
                3,
                21377
            ]
        },
        {
            "name": "append",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                664,
                0,
                23276,
                669,
                3,
                23432
            ]
        },
        {
            "name": "link",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                92,
                2,
                3146,
                110,
                8,
                4083
            ]
        },
        {
            "name": "repeat",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                657,
                0,
                23065,
                662,
                3,
                23274
            ]
        },
        {
            "name": "map",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                444,
                0,
                16121,
                448,
                3,
                16360
            ]
        },
        {
            "name": "distinct",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                999,
                0,
                35916,
                1010,
                3,
                36352
            ]
        },
        {
            "name": "range",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                626,
                0,
                21946,
                635,
                3,
                22322
            ]
        },
        {
            "name": "is-link",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                92,
                2,
                3146,
                110,
                8,
                4083
            ]
        },
        {
            "name": "list",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                549,
                0,
                19309,
                557,
                1,
                19841
            ]
        },
        {
            "name": "sort",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                622,
                0,
                21888,
                624,
                3,
                21944
            ]
        }
    ]
};
return module["exports"] = {
    "each2_n": each2_n639,
    "fold": fold23,
    "all": all518,
    "each3_n": each3_n652,
    "each": each114,
    "is-List": is$List199,
    "member-always": member$always867,
    "member-identical": member$identical761,
    "each4_n": each4_n667,
    "reverse": reverse119,
    "to-raw-array": to$raw$array206,
    "member-now": member$now750,
    "remove": remove151,
    "max": max847,
    "map2_n": map2_n565,
    "longer-than": longer$than410,
    "fold-while": fold$while676,
    "shorter-than": shorter$than426,
    "map3_n": map3_n574,
    "length": length2,
    "sort-by": sort$by454,
    "split-at": split$at127,
    "map4_n": map4_n584,
    "take": take502,
    "filter-map": filter$map775,
    "range-by": range$by473,
    "foldl-complicated": foldl$complicated215,
    "partition": partition181,
    "is-empty": is$empty75,
    "foldl": foldl868,
    "map_n": map_n557,
    "take-while": take$while812,
    "any": any511,
    "function-set": function$set145,
    "empty": empty84,
    "same-length": same$length397,
    "join-str": join$str156,
    "drop": drop505,
    "each_n": each_n628,
    "member-now3": member$now3746,
    "fold_n": fold_n718,
    "fold2": fold2684,
    "foldr": foldr14,
    "each2": each2595,
    "all2": all2528,
    "member-identical3": member$identical3757,
    "member-always3": member$always3866,
    "fold3": fold3691,
    "slice": slice328,
    "min": min865,
    "each3": each3606,
    "fold4": fold4699,
    "each4": each4619,
    "join-str-last": join$str$last161,
    "map2": map2533,
    "last": last450,
    "get": get140,
    "raw-array-to-list": raw$array$to$list211,
    "filter-values": filter$values782,
    "map3": map3541,
    "map4": map4550,
    "filter": filter109,
    "member-with": member$with728,
    "member3": member3737,
    "find": find184,
    "shuffle": shuffle771,
    "member": member740,
    "push": push440,
    "append": append498,
    "link": link50,
    "repeat": repeat492,
    "map": map104,
    "distinct": distinct793,
    "range": range462,
    "is-link": is$link200,
    "list": list392,
    "sort": sort456,
    "$answer": $answer869,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr"),
    "$locations": [
        {
            "name": "each2_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                823,
                0,
                29082,
                834,
                3,
                29542
            ]
        },
        {
            "name": "fold",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                421,
                0,
                15083,
                426,
                3,
                15411
            ]
        },
        {
            "name": "all",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                689,
                0,
                23987,
                695,
                3,
                24215
            ]
        },
        {
            "name": "each3_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                836,
                0,
                29544,
                847,
                3,
                30102
            ]
        },
        {
            "name": "each",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                436,
                0,
                15790,
                440,
                3,
                15975
            ]
        },
        {
            "name": "is-List",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                72,
                0,
                2274,
                300,
                3,
                10967
            ]
        },
        {
            "name": "member-always",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1091,
                0,
                38665,
                1091,
                22,
                38687
            ]
        },
        {
            "name": "member-identical",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                965,
                0,
                34932,
                967,
                3,
                35048
            ]
        },
        {
            "name": "each4_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                849,
                0,
                30104,
                860,
                3,
                30760
            ]
        },
        {
            "name": "reverse",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                428,
                0,
                15413,
                434,
                3,
                15788
            ]
        },
        {
            "name": "to-raw-array",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                302,
                0,
                10969,
                304,
                3,
                11104
            ]
        },
        {
            "name": "member-now",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                957,
                0,
                34684,
                959,
                3,
                34788
            ]
        },
        {
            "name": "remove",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                376,
                0,
                13559,
                387,
                3,
                13896
            ]
        },
        {
            "name": "max",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1040,
                0,
                37480,
                1059,
                3,
                37964
            ]
        },
        {
            "name": "map2_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                744,
                0,
                25972,
                751,
                3,
                26318
            ]
        },
        {
            "name": "longer-than",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                573,
                0,
                20420,
                583,
                3,
                20860
            ]
        },
        {
            "name": "fold-while",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                862,
                0,
                30762,
                874,
                3,
                31289
            ]
        },
        {
            "name": "shorter-than",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                585,
                0,
                20862,
                595,
                3,
                21309
            ]
        },
        {
            "name": "map3_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                753,
                0,
                26320,
                760,
                3,
                26739
            ]
        },
        {
            "name": "length",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                559,
                0,
                19843,
                562,
                3,
                19985
            ]
        },
        {
            "name": "sort-by",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                618,
                0,
                21766,
                620,
                3,
                21886
            ]
        },
        {
            "name": "split-at",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                397,
                0,
                14281,
                417,
                3,
                14937
            ]
        },
        {
            "name": "map4_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                762,
                0,
                26741,
                769,
                3,
                27233
            ]
        },
        {
            "name": "take",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                671,
                0,
                23434,
                674,
                3,
                23590
            ]
        },
        {
            "name": "filter-map",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                984,
                0,
                35530,
                986,
                3,
                35654
            ]
        },
        {
            "name": "range-by",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                637,
                0,
                22324,
                655,
                3,
                23063
            ]
        },
        {
            "name": "foldl-complicated",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                313,
                0,
                11432,
                331,
                3,
                11918
            ]
        },
        {
            "name": "partition",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                468,
                0,
                16955,
                488,
                3,
                17623
            ]
        },
        {
            "name": "is-empty",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                73,
                2,
                2290,
                91,
                8,
                3143
            ]
        },
        {
            "name": "foldl",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1092,
                0,
                38688,
                1092,
                12,
                38700
            ]
        },
        {
            "name": "map_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                735,
                0,
                25702,
                742,
                3,
                25970
            ]
        },
        {
            "name": "take-while",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1012,
                0,
                36354,
                1038,
                3,
                37478
            ]
        },
        {
            "name": "any",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                681,
                0,
                23758,
                687,
                3,
                23985
            ]
        },
        {
            "name": "function-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                523,
                0,
                18448,
                541,
                3,
                19058
            ]
        },
        {
            "name": "empty",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                73,
                2,
                2290,
                91,
                8,
                3143
            ]
        },
        {
            "name": "same-length",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                564,
                0,
                19987,
                571,
                3,
                20418
            ]
        },
        {
            "name": "join-str",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                333,
                0,
                11920,
                350,
                3,
                12610
            ]
        },
        {
            "name": "drop",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                676,
                0,
                23592,
                679,
                3,
                23756
            ]
        },
        {
            "name": "each_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                810,
                0,
                28750,
                821,
                3,
                29080
            ]
        },
        {
            "name": "member-now3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                953,
                0,
                34540,
                955,
                3,
                34682
            ]
        },
        {
            "name": "fold_n",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                913,
                0,
                33060,
                924,
                3,
                33557
            ]
        },
        {
            "name": "fold2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                883,
                0,
                31623,
                891,
                3,
                32042
            ]
        },
        {
            "name": "foldr",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                876,
                0,
                31291,
                881,
                3,
                31621
            ]
        },
        {
            "name": "each2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                771,
                0,
                27235,
                782,
                3,
                27651
            ]
        },
        {
            "name": "all2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                697,
                0,
                24217,
                706,
                3,
                24617
            ]
        },
        {
            "name": "member-identical3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                961,
                0,
                34790,
                963,
                3,
                34930
            ]
        },
        {
            "name": "member-always3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1090,
                0,
                38640,
                1090,
                24,
                38664
            ]
        },
        {
            "name": "fold3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                893,
                0,
                32044,
                901,
                3,
                32521
            ]
        },
        {
            "name": "slice",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                450,
                0,
                16362,
                466,
                3,
                16953
            ]
        },
        {
            "name": "min",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                1061,
                0,
                37966,
                1080,
                3,
                38450
            ]
        },
        {
            "name": "each3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                784,
                0,
                27653,
                795,
                3,
                28157
            ]
        },
        {
            "name": "fold4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                903,
                0,
                32523,
                911,
                3,
                33058
            ]
        },
        {
            "name": "each4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                797,
                0,
                28159,
                808,
                3,
                28748
            ]
        },
        {
            "name": "join-str-last",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                352,
                0,
                12612,
                374,
                3,
                13557
            ]
        },
        {
            "name": "map2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                708,
                0,
                24619,
                715,
                3,
                24913
            ]
        },
        {
            "name": "last",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                601,
                0,
                21379,
                616,
                3,
                21764
            ]
        },
        {
            "name": "get",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                505,
                0,
                17975,
                521,
                3,
                18446
            ]
        },
        {
            "name": "raw-array-to-list",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                306,
                0,
                11106,
                309,
                3,
                11286
            ]
        },
        {
            "name": "filter-values",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                988,
                0,
                35656,
                997,
                3,
                35914
            ]
        },
        {
            "name": "map3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                717,
                0,
                24915,
                724,
                3,
                25270
            ]
        },
        {
            "name": "map4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                726,
                0,
                25272,
                733,
                3,
                25700
            ]
        },
        {
            "name": "filter",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                389,
                0,
                13898,
                395,
                3,
                14279
            ]
        },
        {
            "name": "member-with",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                932,
                0,
                33893,
                943,
                3,
                34305
            ]
        },
        {
            "name": "member3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                945,
                0,
                34307,
                947,
                3,
                34440
            ]
        },
        {
            "name": "find",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                490,
                0,
                17625,
                503,
                3,
                17973
            ]
        },
        {
            "name": "shuffle",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                969,
                0,
                35050,
                982,
                3,
                35528
            ]
        },
        {
            "name": "member",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                949,
                0,
                34442,
                951,
                3,
                34538
            ]
        },
        {
            "name": "push",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                597,
                0,
                21311,
                599,
                3,
                21377
            ]
        },
        {
            "name": "append",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                664,
                0,
                23276,
                669,
                3,
                23432
            ]
        },
        {
            "name": "link",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                92,
                2,
                3146,
                110,
                8,
                4083
            ]
        },
        {
            "name": "repeat",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                657,
                0,
                23065,
                662,
                3,
                23274
            ]
        },
        {
            "name": "map",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                444,
                0,
                16121,
                448,
                3,
                16360
            ]
        },
        {
            "name": "distinct",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                999,
                0,
                35916,
                1010,
                3,
                36352
            ]
        },
        {
            "name": "range",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                626,
                0,
                21946,
                635,
                3,
                22322
            ]
        },
        {
            "name": "is-link",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                92,
                2,
                3146,
                110,
                8,
                4083
            ]
        },
        {
            "name": "list",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                549,
                0,
                19309,
                557,
                1,
                19841
            ]
        },
        {
            "name": "sort",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-2\/lists.arr",
                622,
                0,
                21888,
                624,
                3,
                21944
            ]
        }
    ]
};