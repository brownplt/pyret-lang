var _runtime = require(".\/runtime.js");
var $underscore_import803 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $underscore_import803);
var G804 = require(".\/runtime-global.arr.js");
_runtime["addModule"]("builtin:\/\/runtime-global", G804);
var $included$1805 = require(".\/pick.arr.js");
_runtime["addModule"]("builtin:\/\/pick", $included$1805);
var $included$2806 = require(".\/lists.arr.js");
_runtime["addModule"]("builtin:\/\/lists", $included$2806);
var equality194 = require(".\/equality.arr.js");
_runtime["addModule"]("builtin:\/\/equality", equality194);
var RA807 = require(".\/raw-array.arr.js");
_runtime["addModule"]("builtin:\/\/raw-array", RA807);
var N808 = require(".\/number.arr.js");
_runtime["addModule"]("builtin:\/\/number", N808);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr");
var _lessthan16 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "_lessthan");
var link52 = _runtime["getModuleValue"]("builtin:\/\/lists", "link");
var empty57 = _runtime["getModuleValue"]("builtin:\/\/lists", "empty");
var not192 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "not");
var num$max226 = _runtime["getModuleValue"]("builtin:\/\/number", "num-max");
var raise300 = _runtime["getModuleValue"]("builtin:\/\/runtime-global", "raise");
var num$abs318 = _runtime["getModuleValue"]("builtin:\/\/number", "num-abs");
var list403 = _runtime["getModuleValue"]("builtin:\/\/lists", "list");
var pick$none425 = _runtime["getModuleValue"]("builtin:\/\/pick", "pick-none");
var pick$some429 = _runtime["getModuleValue"]("builtin:\/\/pick", "pick-some");
var fold449 = _runtime["getModuleValue"]("builtin:\/\/lists", "fold");
var is$empty537 = _runtime["getModuleValue"]("builtin:\/\/lists", "is-empty");
var member$with562 = _runtime["getModuleValue"]("builtin:\/\/lists", "member-with");
var num$floor596 = _runtime["getModuleValue"]("builtin:\/\/number", "num-floor");
var num$ceiling608 = _runtime["getModuleValue"]("builtin:\/\/number", "num-ceiling");
var raw$array$fold717 = _runtime["getModuleValue"]("builtin:\/\/raw-array", "raw-array-fold");
var nothing801 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_AVLTree197 = _runtime["$setupMethodGetters"]({
    "$methods": {
        "height": function getWrapper_height9() {
            var self1 = this;
            return _runtime["$installMethod"](self1, "height", function lam_height8() {
                var cases2 = self1;
                var $ans3;
                switch (cases2["$name"]) {
                case "leaf":
                    $ans3 = 0;
                    break;
                case "branch":
                    var $underscore4 = cases2[cases2["$fieldNames"][0]];
                    var $underscore5 = cases2[cases2["$fieldNames"][1]];
                    var $underscore6 = cases2[cases2["$fieldNames"][2]];
                    var $underscore7 = cases2[cases2["$fieldNames"][3]];
                    $ans3 = self1["h"];
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        64,
                        4,
                        1379,
                        67,
                        7,
                        1462
                    ], cases2);
                    $ans3 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        64,
                        4,
                        1379,
                        67,
                        7,
                        1462
                    ], cases2);
                }
                return $ans3;
            });
        },
        "contains": function getWrapper_contains22() {
            var self10 = this;
            return _runtime["$installMethod"](self10, "contains", function lam_contains21(val14) {
                var cases11 = self10;
                var $ans12;
                switch (cases11["$name"]) {
                case "leaf":
                    $ans12 = false;
                    break;
                case "branch":
                    var value17 = cases11[cases11["$fieldNames"][0]];
                    var $underscore20 = cases11[cases11["$fieldNames"][1]];
                    var left18 = cases11[cases11["$fieldNames"][2]];
                    var right13 = cases11[cases11["$fieldNames"][3]];
                    if (_runtime["equal-always"](val14, value17)) {
                        $ans19 = true;
                    } else {
                        if (_lessthan16(val14, value17)) {
                            $ans15 = left18["contains"](val14);
                        } else {
                            $ans15 = right13["contains"](val14);
                        }
                        $ans19 = $ans15;
                    }
                    $ans12 = $ans19;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        71,
                        4,
                        1594,
                        81,
                        7,
                        1851
                    ], cases11);
                    $ans12 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        71,
                        4,
                        1594,
                        81,
                        7,
                        1851
                    ], cases11);
                }
                return $ans12;
            });
        },
        "insert": function getWrapper_insert37() {
            var self23 = this;
            return _runtime["$installMethod"](self23, "insert", function lam_insert36(val27) {
                var cases24 = self23;
                var $ans25;
                switch (cases24["$name"]) {
                case "leaf":
                    $ans25 = (mkbranch26 !== undefined ? mkbranch26 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        86,
                        16,
                        2012,
                        86,
                        24,
                        2020
                    ], "Uninitialized letrec identifier"))(val27, leaf28 !== undefined ? leaf28 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        86,
                        30,
                        2026,
                        86,
                        34,
                        2030
                    ], "Uninitialized letrec identifier"), leaf28 !== undefined ? leaf28 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        86,
                        36,
                        2032,
                        86,
                        40,
                        2036
                    ], "Uninitialized letrec identifier"));
                    break;
                case "branch":
                    var value30 = cases24[cases24["$fieldNames"][0]];
                    var $underscore35 = cases24[cases24["$fieldNames"][1]];
                    var left31 = cases24[cases24["$fieldNames"][2]];
                    var right32 = cases24[cases24["$fieldNames"][3]];
                    if (_runtime["equal-always"](val27, value30)) {
                        $ans34 = (mkbranch26 !== undefined ? mkbranch26 : _runtime["$messageThrow"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            89,
                            10,
                            2114,
                            89,
                            18,
                            2122
                        ], "Uninitialized letrec identifier"))(val27, left31, right32);
                    } else {
                        if (_lessthan16(val27, value30)) {
                            $ans33 = (rebalance29 !== undefined ? rebalance29 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                91,
                                10,
                                2190,
                                91,
                                19,
                                2199
                            ], "Uninitialized letrec identifier"))((mkbranch26 !== undefined ? mkbranch26 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                91,
                                20,
                                2200,
                                91,
                                28,
                                2208
                            ], "Uninitialized letrec identifier"))(value30, left31["insert"](val27), right32));
                        } else {
                            $ans33 = (rebalance29 !== undefined ? rebalance29 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                93,
                                10,
                                2266,
                                93,
                                19,
                                2275
                            ], "Uninitialized letrec identifier"))((mkbranch26 !== undefined ? mkbranch26 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                93,
                                20,
                                2276,
                                93,
                                28,
                                2284
                            ], "Uninitialized letrec identifier"))(value30, left31, right32["insert"](val27)));
                        }
                        $ans34 = $ans33;
                    }
                    $ans25 = $ans34;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        85,
                        4,
                        1975,
                        95,
                        7,
                        2337
                    ], cases24);
                    $ans25 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        85,
                        4,
                        1975,
                        95,
                        7,
                        2337
                    ], cases24);
                }
                return $ans25;
            });
        },
        "remove": function getWrapper_remove50() {
            var self38 = this;
            return _runtime["$installMethod"](self38, "remove", function lam_remove49(val44) {
                var cases39 = self38;
                var $ans40;
                switch (cases39["$name"]) {
                case "leaf":
                    $ans40 = leaf28 !== undefined ? leaf28 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        100,
                        16,
                        2495,
                        100,
                        20,
                        2499
                    ], "Uninitialized letrec identifier");
                    break;
                case "branch":
                    var value41 = cases39[cases39["$fieldNames"][0]];
                    var $underscore48 = cases39[cases39["$fieldNames"][1]];
                    var left42 = cases39[cases39["$fieldNames"][2]];
                    var right43 = cases39[cases39["$fieldNames"][3]];
                    if (_runtime["equal-always"](val44, value41)) {
                        $ans46 = (remove$root47 !== undefined ? remove$root47 : _runtime["$messageThrow"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            103,
                            10,
                            2576,
                            103,
                            21,
                            2587
                        ], "Uninitialized letrec identifier"))(self38);
                    } else {
                        if (_lessthan16(val44, value41)) {
                            $ans45 = (rebalance29 !== undefined ? rebalance29 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                105,
                                10,
                                2643,
                                105,
                                19,
                                2652
                            ], "Uninitialized letrec identifier"))((mkbranch26 !== undefined ? mkbranch26 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                105,
                                20,
                                2653,
                                105,
                                28,
                                2661
                            ], "Uninitialized letrec identifier"))(value41, left42["remove"](val44), right43));
                        } else {
                            $ans45 = (rebalance29 !== undefined ? rebalance29 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                107,
                                10,
                                2719,
                                107,
                                19,
                                2728
                            ], "Uninitialized letrec identifier"))((mkbranch26 !== undefined ? mkbranch26 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                107,
                                20,
                                2729,
                                107,
                                28,
                                2737
                            ], "Uninitialized letrec identifier"))(value41, left42, right43["remove"](val44)));
                        }
                        $ans46 = $ans45;
                    }
                    $ans40 = $ans46;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        99,
                        4,
                        2458,
                        109,
                        7,
                        2790
                    ], cases39);
                    $ans40 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        99,
                        4,
                        2458,
                        109,
                        7,
                        2790
                    ], cases39);
                }
                return $ans40;
            });
        },
        "preorder": function getWrapper_preorder59() {
            var self51 = this;
            return _runtime["$installMethod"](self51, "preorder", function lam_preorder58() {
                var knil56 = function lam_knil55(l54, x53) {
                    return link52(x53, l54);
                };
                return self51["fold-revpostorder"](knil56, empty57);
            });
        },
        "inorder": function getWrapper_inorder65() {
            var self60 = this;
            return _runtime["$installMethod"](self60, "inorder", function lam_inorder64() {
                var knil63 = function lam_knil55(l62, x61) {
                    return link52(x61, l62);
                };
                return self60["fold-revinorder"](knil63, empty57);
            });
        },
        "postorder": function getWrapper_postorder71() {
            var self66 = this;
            return _runtime["$installMethod"](self66, "postorder", function lam_postorder70() {
                var knil69 = function lam_knil55(l68, x67) {
                    return link52(x67, l68);
                };
                return self66["fold-revpreorder"](knil69, empty57);
            });
        },
        "revpreorder": function getWrapper_revpreorder77() {
            var self72 = this;
            return _runtime["$installMethod"](self72, "revpreorder", function lam_revpreorder76() {
                var knil75 = function lam_knil55(l74, x73) {
                    return link52(x73, l74);
                };
                return self72["fold-preorder"](knil75, empty57);
            });
        },
        "revinorder": function getWrapper_revinorder83() {
            var self78 = this;
            return _runtime["$installMethod"](self78, "revinorder", function lam_revinorder82() {
                var knil81 = function lam_knil55(l80, x79) {
                    return link52(x79, l80);
                };
                return self78["fold-inorder"](knil81, empty57);
            });
        },
        "revpostorder": function getWrapper_revpostorder89() {
            var self84 = this;
            return _runtime["$installMethod"](self84, "revpostorder", function lam_revpostorder88() {
                var knil87 = function lam_knil55(l86, x85) {
                    return link52(x85, l86);
                };
                return self84["fold-postorder"](knil87, empty57);
            });
        },
        "fold-preorder": function getWrapper_fold$preorder100() {
            var self90 = this;
            return _runtime["$installMethod"](self90, "fold-preorder", function lam_fold$preorder99(f95, base93) {
                var cases91 = self90;
                var $ans92;
                switch (cases91["$name"]) {
                case "leaf":
                    $ans92 = base93;
                    break;
                case "branch":
                    var value97 = cases91[cases91["$fieldNames"][0]];
                    var $underscore98 = cases91[cases91["$fieldNames"][1]];
                    var left96 = cases91[cases91["$fieldNames"][2]];
                    var right94 = cases91[cases91["$fieldNames"][3]];
                    $ans92 = right94["fold-preorder"](f95, left96["fold-preorder"](f95, f95(base93, value97)));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        146,
                        4,
                        4446,
                        150,
                        7,
                        4606
                    ], cases91);
                    $ans92 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        146,
                        4,
                        4446,
                        150,
                        7,
                        4606
                    ], cases91);
                }
                return $ans92;
            });
        },
        "fold-inorder": function getWrapper_fold$inorder111() {
            var self101 = this;
            return _runtime["$installMethod"](self101, "fold-inorder", function lam_fold$inorder110(f106, base104) {
                var cases102 = self101;
                var $ans103;
                switch (cases102["$name"]) {
                case "leaf":
                    $ans103 = base104;
                    break;
                case "branch":
                    var value108 = cases102[cases102["$fieldNames"][0]];
                    var $underscore109 = cases102[cases102["$fieldNames"][1]];
                    var left107 = cases102[cases102["$fieldNames"][2]];
                    var right105 = cases102[cases102["$fieldNames"][3]];
                    $ans103 = right105["fold-inorder"](f106, f106(left107["fold-inorder"](f106, base104), value108));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        155,
                        4,
                        4831,
                        159,
                        7,
                        4989
                    ], cases102);
                    $ans103 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        155,
                        4,
                        4831,
                        159,
                        7,
                        4989
                    ], cases102);
                }
                return $ans103;
            });
        },
        "fold-postorder": function getWrapper_fold$postorder122() {
            var self112 = this;
            return _runtime["$installMethod"](self112, "fold-postorder", function lam_fold$postorder121(f116, base115) {
                var cases113 = self112;
                var $ans114;
                switch (cases113["$name"]) {
                case "leaf":
                    $ans114 = base115;
                    break;
                case "branch":
                    var value119 = cases113[cases113["$fieldNames"][0]];
                    var $underscore120 = cases113[cases113["$fieldNames"][1]];
                    var left118 = cases113[cases113["$fieldNames"][2]];
                    var right117 = cases113[cases113["$fieldNames"][3]];
                    $ans114 = f116(right117["fold-postorder"](f116, left118["fold-postorder"](f116, base115)), value119);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        164,
                        4,
                        5217,
                        168,
                        7,
                        5379
                    ], cases113);
                    $ans114 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        164,
                        4,
                        5217,
                        168,
                        7,
                        5379
                    ], cases113);
                }
                return $ans114;
            });
        },
        "fold-revpreorder": function getWrapper_fold$revpreorder133() {
            var self123 = this;
            return _runtime["$installMethod"](self123, "fold-revpreorder", function lam_fold$revpreorder132(f128, base126) {
                var cases124 = self123;
                var $ans125;
                switch (cases124["$name"]) {
                case "leaf":
                    $ans125 = base126;
                    break;
                case "branch":
                    var value130 = cases124[cases124["$fieldNames"][0]];
                    var $underscore131 = cases124[cases124["$fieldNames"][1]];
                    var left127 = cases124[cases124["$fieldNames"][2]];
                    var right129 = cases124[cases124["$fieldNames"][3]];
                    $ans125 = left127["fold-revpreorder"](f128, right129["fold-revpreorder"](f128, f128(base126, value130)));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        173,
                        4,
                        5622,
                        177,
                        7,
                        5788
                    ], cases124);
                    $ans125 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        173,
                        4,
                        5622,
                        177,
                        7,
                        5788
                    ], cases124);
                }
                return $ans125;
            });
        },
        "fold-revinorder": function getWrapper_fold$revinorder144() {
            var self134 = this;
            return _runtime["$installMethod"](self134, "fold-revinorder", function lam_fold$revinorder143(f139, base137) {
                var cases135 = self134;
                var $ans136;
                switch (cases135["$name"]) {
                case "leaf":
                    $ans136 = base137;
                    break;
                case "branch":
                    var value141 = cases135[cases135["$fieldNames"][0]];
                    var $underscore142 = cases135[cases135["$fieldNames"][1]];
                    var left138 = cases135[cases135["$fieldNames"][2]];
                    var right140 = cases135[cases135["$fieldNames"][3]];
                    $ans136 = left138["fold-revinorder"](f139, f139(right140["fold-revinorder"](f139, base137), value141));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        182,
                        4,
                        6029,
                        186,
                        7,
                        6193
                    ], cases135);
                    $ans136 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        182,
                        4,
                        6029,
                        186,
                        7,
                        6193
                    ], cases135);
                }
                return $ans136;
            });
        },
        "fold-revpostorder": function getWrapper_fold$revpostorder155() {
            var self145 = this;
            return _runtime["$installMethod"](self145, "fold-revpostorder", function lam_fold$revpostorder154(f149, base148) {
                var cases146 = self145;
                var $ans147;
                switch (cases146["$name"]) {
                case "leaf":
                    $ans147 = base148;
                    break;
                case "branch":
                    var value152 = cases146[cases146["$fieldNames"][0]];
                    var $underscore153 = cases146[cases146["$fieldNames"][1]];
                    var left150 = cases146[cases146["$fieldNames"][2]];
                    var right151 = cases146[cases146["$fieldNames"][3]];
                    $ans147 = f149(left150["fold-revpostorder"](f149, right151["fold-revpostorder"](f149, base148)), value152);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        191,
                        4,
                        6438,
                        195,
                        7,
                        6606
                    ], cases146);
                    $ans147 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        191,
                        4,
                        6438,
                        195,
                        7,
                        6606
                    ], cases146);
                }
                return $ans147;
            });
        },
        "count": function getWrapper_count164() {
            var self156 = this;
            return _runtime["$installMethod"](self156, "count", function lam_count163() {
                var cases157 = self156;
                var $ans158;
                switch (cases157["$name"]) {
                case "leaf":
                    $ans158 = 0;
                    break;
                case "branch":
                    var value161 = cases157[cases157["$fieldNames"][0]];
                    var $underscore162 = cases157[cases157["$fieldNames"][1]];
                    var left159 = cases157[cases157["$fieldNames"][2]];
                    var right160 = cases157[cases157["$fieldNames"][3]];
                    $ans158 = _runtime["_plus"](_runtime["_plus"](1, left159["count"](), _runtime["$errCallbacks"]), right160["count"](), _runtime["$errCallbacks"]);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        198,
                        4,
                        6640,
                        202,
                        7,
                        6768
                    ], cases157);
                    $ans158 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        198,
                        4,
                        6640,
                        202,
                        7,
                        6768
                    ], cases157);
                }
                return $ans158;
            });
        },
        "all": function getWrapper_all174() {
            var self165 = this;
            return _runtime["$installMethod"](self165, "all", function lam_all173(f168) {
                var cases166 = self165;
                var $ans167;
                switch (cases166["$name"]) {
                case "leaf":
                    $ans167 = true;
                    break;
                case "branch":
                    var value169 = cases166[cases166["$fieldNames"][0]];
                    var $underscore172 = cases166[cases166["$fieldNames"][1]];
                    var left171 = cases166[cases166["$fieldNames"][2]];
                    var right170 = cases166[cases166["$fieldNames"][3]];
                    $ans167 = f168(value169) && right170["all"](f168) && left171["all"](f168);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        205,
                        4,
                        6832,
                        209,
                        7,
                        6972
                    ], cases166);
                    $ans167 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        205,
                        4,
                        6832,
                        209,
                        7,
                        6972
                    ], cases166);
                }
                return $ans167;
            });
        },
        "any": function getWrapper_any184() {
            var self175 = this;
            return _runtime["$installMethod"](self175, "any", function lam_any183(f178) {
                var cases176 = self175;
                var $ans177;
                switch (cases176["$name"]) {
                case "leaf":
                    $ans177 = false;
                    break;
                case "branch":
                    var value179 = cases176[cases176["$fieldNames"][0]];
                    var $underscore182 = cases176[cases176["$fieldNames"][1]];
                    var left181 = cases176[cases176["$fieldNames"][2]];
                    var right180 = cases176[cases176["$fieldNames"][3]];
                    $ans177 = f178(value179) || right180["all"](f178) || left181["all"](f178);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        212,
                        4,
                        7036,
                        216,
                        7,
                        7175
                    ], cases176);
                    $ans177 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        212,
                        4,
                        7036,
                        216,
                        7,
                        7175
                    ], cases176);
                }
                return $ans177;
            });
        },
        "to-list": function getWrapper_to$list187() {
            var self185 = this;
            return _runtime["$installMethod"](self185, "to-list", function lam_to$list186() {
                return self185["inorder"]();
            });
        },
        "_equals": function getWrapper__equals196() {
            var self188 = this;
            return _runtime["$installMethod"](self188, "_equals", function lam__equals195(other190, eq189) {
                if (not192((is$AVLTree193 !== undefined ? is$AVLTree193 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        223,
                        11,
                        7357,
                        223,
                        21,
                        7367
                    ], "Uninitialized letrec identifier"))(other190))) {
                    $ans191 = equality194["NotEqual"]("Non-AVLTree", self188, other190);
                } else {
                    $ans191 = eq189(self188["inorder"](), other190["inorder"]());
                }
                return $ans191;
            });
        }
    }
});
var variantBase_leaf198 = _runtime["$createVariant"](sharedBase_AVLTree197, { "$methods": {} }, {
    "$data": sharedBase_AVLTree197,
    "$name": "leaf",
    "$fieldNames": null
});
var variantBase_branch199 = _runtime["$createVariant"](sharedBase_AVLTree197, { "$methods": {} }, {
    "$data": sharedBase_AVLTree197,
    "$name": "branch",
    "$fieldNames": [
        "value",
        "h",
        "left",
        "right"
    ]
});
var AVLTree208 = {
    "leaf": variantBase_leaf198,
    "branch": function branch204(value200, h201, left202, right203) {
        return _runtime["$makeDataValue"](variantBase_branch199, {
            "value": value200,
            "h": h201,
            "left": left202,
            "right": right203
        });
    },
    "is-leaf": function is$leaf205(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_leaf198["$variant"];
    },
    "is-branch": function is$branch206(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_branch199["$variant"];
    },
    "is-AVLTree": function is$AVLTree207(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_AVLTree197;
    }
};
var is$AVLTree193 = AVLTree208["is-AVLTree"];
var is$leaf209 = AVLTree208["is-leaf"];
var leaf28 = AVLTree208["leaf"];
var is$branch210 = AVLTree208["is-branch"];
var branch211 = AVLTree208["branch"];
var tree$fold216 = function lam_tree$fold215(f213, base214, tree212) {
    return tree212["fold-preorder"](f213, base214);
};
var tree$all220 = function lam_tree$all219(f218, tree217) {
    return tree217["all"](f218);
};
var tree$any224 = function lam_tree$any223(f222, tree221) {
    return tree221["any"](f222);
};
var mkbranch26 = function lam_mkbranch229(val225, left227, right228) {
    return branch211(val225, _runtime["_plus"](num$max226(left227["height"](), right228["height"]()), 1, _runtime["$errCallbacks"]), left227, right228);
};
var rebalance29 = function lam_rebalance321(tree293) {
    var left$left243 = function lam_left$left242(t231) {
        var tup232 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            249,
            32,
            8076,
            249,
            40,
            8084
        ], "Uninitialized letrec identifier"))(t231);
        var value233 = tup232[0];
        var $underscore234 = tup232[1];
        var left235 = tup232[2];
        var right236 = tup232[3];
        var tup237 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            250,
            63,
            8151,
            250,
            71,
            8159
        ], "Uninitialized letrec identifier"))(left235);
        var left$value238 = tup237[0];
        var $underscore239 = tup237[1];
        var left$left$subtree240 = tup237[2];
        var left$right$subtree241 = tup237[3];
        return mkbranch26(left$value238, left$left$subtree240, mkbranch26(value233, left$right$subtree241, right236));
    };
    var right$right256 = function lam_right$right255(t244) {
        var tup245 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            255,
            32,
            8343,
            255,
            40,
            8351
        ], "Uninitialized letrec identifier"))(t244);
        var value246 = tup245[0];
        var $underscore247 = tup245[1];
        var left248 = tup245[2];
        var right249 = tup245[3];
        var tup250 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            256,
            66,
            8421,
            256,
            74,
            8429
        ], "Uninitialized letrec identifier"))(right249);
        var right$value251 = tup250[0];
        var $underscore252 = tup250[1];
        var right$left$subtree253 = tup250[2];
        var right$right$subtree254 = tup250[3];
        return mkbranch26(right$value251, mkbranch26(value246, left248, right$left$subtree253), right$right$subtree254);
    };
    var left$right274 = function lam_left$right273(t257) {
        var tup258 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            260,
            32,
            8614,
            260,
            40,
            8622
        ], "Uninitialized letrec identifier"))(t257);
        var value259 = tup258[0];
        var $underscore260 = tup258[1];
        var left261 = tup258[2];
        var right262 = tup258[3];
        var tup263 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            261,
            63,
            8689,
            261,
            71,
            8697
        ], "Uninitialized letrec identifier"))(left261);
        var left$value264 = tup263[0];
        var $underscore265 = tup263[1];
        var left$left$subtree266 = tup263[2];
        var left$right$subtree267 = tup263[3];
        var tup268 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            262,
            81,
            8785,
            262,
            89,
            8793
        ], "Uninitialized letrec identifier"))(left$right$subtree267);
        var left$right$value269 = tup268[0];
        var $underscore270 = tup268[1];
        var left$right$left$subtree271 = tup268[2];
        var left$right$right$subtree272 = tup268[3];
        return mkbranch26(left$right$value269, mkbranch26(left$value264, left$left$subtree266, left$right$left$subtree271), mkbranch26(value259, left$right$right$subtree272, right262));
    };
    var right$left292 = function lam_right$left291(t275) {
        var tup276 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            268,
            32,
            9060,
            268,
            40,
            9068
        ], "Uninitialized letrec identifier"))(t275);
        var value277 = tup276[0];
        var $underscore278 = tup276[1];
        var left279 = tup276[2];
        var right280 = tup276[3];
        var tup281 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            269,
            66,
            9138,
            269,
            74,
            9146
        ], "Uninitialized letrec identifier"))(right280);
        var right$value282 = tup281[0];
        var $underscore283 = tup281[1];
        var right$left$subtree284 = tup281[2];
        var right$right$subtree285 = tup281[3];
        var tup286 = (tree$get230 !== undefined ? tree$get230 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            270,
            81,
            9235,
            270,
            89,
            9243
        ], "Uninitialized letrec identifier"))(right$left$subtree284);
        var right$left$value287 = tup286[0];
        var $underscore288 = tup286[1];
        var right$left$left$subtree289 = tup286[2];
        var right$left$right$subtree290 = tup286[3];
        return mkbranch26(right$left$value287, mkbranch26(value277, left279, right$left$left$subtree289), mkbranch26(right$value282, right$left$right$subtree290, right$right$subtree285));
    };
    var cases294 = tree293;
    var $ans295;
    switch (cases294["$name"]) {
    case "leaf":
        $ans295 = leaf28;
        break;
    case "branch":
        var value319 = cases294[cases294["$fieldNames"][0]];
        var height320 = cases294[cases294["$fieldNames"][1]];
        var left296 = cases294[cases294["$fieldNames"][2]];
        var right298 = cases294[cases294["$fieldNames"][3]];
        var lh297 = left296["height"]();
        var rh299 = right298["height"]();
        if (_runtime["_lessequal"](num$abs318(_runtime["_minus"](lh297, rh299, _runtime["$errCallbacks"])), 1, _runtime["$errCallbacks"])) {
            $ans317 = tree293;
        } else {
            if (_runtime["equal-always"](_runtime["_minus"](lh297, rh299, _runtime["$errCallbacks"]), 2)) {
                var tup311 = (tree$get$left310 !== undefined ? tree$get$left310 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                    284,
                    68,
                    9712,
                    284,
                    81,
                    9725
                ], "Uninitialized letrec identifier"))(tree293);
                var $underscore312 = tup311[0];
                var left$height313 = tup311[1];
                var left$left$subtree314 = tup311[2];
                var left$right$subtree315 = tup311[3];
                if (_runtime["_greaterequal"](left$left$subtree314["height"](), left$right$subtree315["height"](), _runtime["$errCallbacks"])) {
                    $ans316 = left$left243(tree293);
                } else {
                    $ans316 = left$right274(tree293);
                }
                $ans309 = $ans316;
            } else {
                if (_runtime["equal-always"](_runtime["_minus"](rh299, lh297, _runtime["$errCallbacks"]), 2)) {
                    var tup303 = (tree$get$right302 !== undefined ? tree$get$right302 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        291,
                        71,
                        9982,
                        291,
                        85,
                        9996
                    ], "Uninitialized letrec identifier"))(tree293);
                    var $underscore304 = tup303[0];
                    var right$height305 = tup303[1];
                    var right$left$subtree306 = tup303[2];
                    var right$right$subtree307 = tup303[3];
                    if (_runtime["_greaterequal"](right$right$subtree307["height"](), right$left$subtree306["height"](), _runtime["$errCallbacks"])) {
                        $ans308 = right$right256(tree293);
                    } else {
                        $ans308 = right$left292(tree293);
                    }
                    $ans301 = $ans308;
                } else {
                    $ans301 = raise300("AVL tree invariant has been broken!");
                }
                $ans309 = $ans301;
            }
            $ans317 = $ans309;
        }
        $ans295 = $ans317;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            276,
            2,
            9434,
            300,
            5,
            10236
        ], cases294);
        $ans295 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            276,
            2,
            9434,
            300,
            5,
            10236
        ], cases294);
    }
    return $ans295;
};
var tree$get230 = function lam_tree$get329(tree322) {
    var cases323 = tree322;
    var $ans324;
    switch (cases323["$name"]) {
    case "leaf":
        $ans324 = raise300("Parent was a leaf");
        break;
    case "branch":
        var v325 = cases323[cases323["$fieldNames"][0]];
        var h326 = cases323[cases323["$fieldNames"][1]];
        var l327 = cases323[cases323["$fieldNames"][2]];
        var r328 = cases323[cases323["$fieldNames"][3]];
        $ans324 = _runtime["PTuple"]([
            v325,
            h326,
            l327,
            r328
        ]);
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            304,
            2,
            10322,
            307,
            5,
            10431
        ], cases323);
        $ans324 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            304,
            2,
            10322,
            307,
            5,
            10431
        ], cases323);
    }
    return $ans324;
};
var tree$get$left310 = function lam_tree$get$left343(tree330) {
    var cases331 = tree330;
    var $ans332;
    switch (cases331["$name"]) {
    case "leaf":
        $ans332 = raise300("Parent was a leaf");
        break;
    case "branch":
        var $underscore340 = cases331[cases331["$fieldNames"][0]];
        var $underscore341 = cases331[cases331["$fieldNames"][1]];
        var left333 = cases331[cases331["$fieldNames"][2]];
        var $underscore342 = cases331[cases331["$fieldNames"][3]];
        var cases334 = left333;
        var $ans335;
        switch (cases334["$name"]) {
        case "leaf":
            $ans335 = raise300("Left subtree was a leaf");
            break;
        case "branch":
            var v336 = cases334[cases334["$fieldNames"][0]];
            var h337 = cases334[cases334["$fieldNames"][1]];
            var l338 = cases334[cases334["$fieldNames"][2]];
            var r339 = cases334[cases334["$fieldNames"][3]];
            $ans335 = _runtime["PTuple"]([
                v336,
                h337,
                l338,
                r339
            ]);
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                315,
                6,
                10622,
                318,
                9,
                10750
            ], cases334);
            $ans335 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                315,
                6,
                10622,
                318,
                9,
                10750
            ], cases334);
        }
        $ans332 = $ans335;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            312,
            2,
            10523,
            319,
            5,
            10756
        ], cases331);
        $ans332 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            312,
            2,
            10523,
            319,
            5,
            10756
        ], cases331);
    }
    return $ans332;
};
var tree$get$right302 = function lam_tree$get$right357(tree344) {
    var cases345 = tree344;
    var $ans346;
    switch (cases345["$name"]) {
    case "leaf":
        $ans346 = raise300("Parent was a leaf");
        break;
    case "branch":
        var $underscore354 = cases345[cases345["$fieldNames"][0]];
        var $underscore355 = cases345[cases345["$fieldNames"][1]];
        var $underscore356 = cases345[cases345["$fieldNames"][2]];
        var right347 = cases345[cases345["$fieldNames"][3]];
        var cases348 = right347;
        var $ans349;
        switch (cases348["$name"]) {
        case "leaf":
            $ans349 = raise300("Right subtree was a leaf");
            break;
        case "branch":
            var v350 = cases348[cases348["$fieldNames"][0]];
            var h351 = cases348[cases348["$fieldNames"][1]];
            var l352 = cases348[cases348["$fieldNames"][2]];
            var r353 = cases348[cases348["$fieldNames"][3]];
            $ans349 = _runtime["PTuple"]([
                v350,
                h351,
                l352,
                r353
            ]);
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                326,
                6,
                10948,
                329,
                9,
                11078
            ], cases348);
            $ans349 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                326,
                6,
                10948,
                329,
                9,
                11078
            ], cases348);
        }
        $ans346 = $ans349;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            323,
            2,
            10848,
            330,
            5,
            11084
        ], cases345);
        $ans346 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            323,
            2,
            10848,
            330,
            5,
            11084
        ], cases345);
    }
    return $ans346;
};
var remove$root47 = function lam_remove$root369(tree358) {
    var cases359 = tree358;
    var $ans360;
    switch (cases359["$name"]) {
    case "leaf":
        $ans360 = raise300("Trying to remove the root of a leaf");
        break;
    case "branch":
        var $underscore367 = cases359[cases359["$fieldNames"][0]];
        var $underscore368 = cases359[cases359["$fieldNames"][1]];
        var left364 = cases359[cases359["$fieldNames"][2]];
        var right363 = cases359[cases359["$fieldNames"][3]];
        if (is$leaf209(left364)) {
            if (is$leaf209(right363)) {
                $ans366 = leaf28;
            } else {
                $ans366 = right363;
            }
            $ans365 = $ans366;
        } else {
            if (is$leaf209(right363)) {
                $ans362 = left364;
            } else {
                $ans362 = (swap$next$lowest361 !== undefined ? swap$next$lowest361 : _runtime["$messageThrow"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                    347,
                    10,
                    11447,
                    347,
                    26,
                    11463
                ], "Uninitialized letrec identifier"))(tree358);
            }
            $ans365 = $ans362;
        }
        $ans360 = $ans365;
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            334,
            2,
            11146,
            350,
            5,
            11497
        ], cases359);
        $ans360 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            334,
            2,
            11146,
            350,
            5,
            11497
        ], cases359);
    }
    return $ans360;
};
var swap$next$lowest361 = function lam_swap$next$lowest401(tree390) {
    var greatest373 = function lam_greatest379(t370) {
        var cases371 = t370;
        var $ans372;
        switch (cases371["$name"]) {
        case "leaf":
            $ans372 = raise300("Went too far in traversal step");
            break;
        case "branch":
            var $underscore376 = cases371[cases371["$fieldNames"][0]];
            var $underscore377 = cases371[cases371["$fieldNames"][1]];
            var $underscore378 = cases371[cases371["$fieldNames"][2]];
            var right374 = cases371[cases371["$fieldNames"][3]];
            if (is$leaf209(right374)) {
                $ans375 = t370;
            } else {
                $ans375 = greatest373(right374);
            }
            $ans372 = $ans375;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                355,
                4,
                11613,
                358,
                7,
                11775
            ], cases371);
            $ans372 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                355,
                4,
                11613,
                358,
                7,
                11775
            ], cases371);
        }
        return $ans372;
    };
    var remove$greatest$and$rebalance385 = function lam_remove$greatest$and$rebalance389(t380) {
        var cases381 = t380;
        var $ans382;
        switch (cases381["$name"]) {
        case "leaf":
            $ans382 = raise300("Went too far in removal step");
            break;
        case "branch":
            var val383 = cases381[cases381["$fieldNames"][0]];
            var $underscore388 = cases381[cases381["$fieldNames"][1]];
            var left384 = cases381[cases381["$fieldNames"][2]];
            var right386 = cases381[cases381["$fieldNames"][3]];
            if (is$leaf209(right386)) {
                $ans387 = left384;
            } else {
                $ans387 = rebalance29(mkbranch26(val383, left384, remove$greatest$and$rebalance385(right386)));
            }
            $ans382 = $ans387;
            break;
        default:
            _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                361,
                4,
                11854,
                369,
                7,
                12119
            ], cases381);
            $ans382 = _runtime["throwNoCaseesMatched"]([
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                361,
                4,
                11854,
                369,
                7,
                12119
            ], cases381);
        }
        return $ans382;
    };
    var tup391 = tree$get230(tree390);
    var $underscore392 = tup391[0];
    var $underscore393 = tup391[1];
    var left394 = tup391[2];
    var right395 = tup391[3];
    var tup396 = tree$get230(greatest373(left394));
    var new$value397 = tup396[0];
    var $underscore398 = tup396[1];
    var $underscore399 = tup396[2];
    var $underscore400 = tup396[3];
    return rebalance29(mkbranch26(new$value397, remove$greatest$and$rebalance385(left394), right395));
};
var $underscore417 = undefined;
var sharedBase_Set570 = _runtime["$setupMethodGetters"]({
    "$methods": {
        "pick": function getWrapper_pick445() {
            var self418 = this;
            return _runtime["$installMethod"](self418, "pick", function lam_pick444() {
                var cases419 = self418;
                var $ans420;
                switch (cases419["$name"]) {
                case "list-set":
                    var elems421 = cases419[cases419["$fieldNames"][0]];
                    var lst422 = elems421;
                    var cases423 = lst422;
                    var $ans424;
                    switch (cases423["$name"]) {
                    case "empty":
                        $ans424 = pick$none425;
                        break;
                    case "link":
                        var f430 = cases423[cases423["$fieldNames"][0]];
                        var r426 = cases423[cases423["$fieldNames"][1]];
                        var cases427 = r426;
                        var $ans428;
                        switch (cases427["$name"]) {
                        case "empty":
                            $ans428 = pick$some429(f430, (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                414,
                                38,
                                13417,
                                414,
                                46,
                                13425
                            ], "Uninitialized letrec identifier"))(empty57));
                            break;
                        case "link":
                            var f2433 = cases427[cases427["$fieldNames"][0]];
                            var r2434 = cases427[cases427["$fieldNames"][1]];
                            var get$first432 = raise300("sets TODO: random");
                            if (_runtime["equal-always"](get$first432, 0)) {
                                $ans435 = pick$some429(f430, (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                    420,
                                    31,
                                    13671,
                                    420,
                                    39,
                                    13679
                                ], "Uninitialized letrec identifier"))(r426));
                            } else {
                                $ans435 = pick$some429(f2433, (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                    422,
                                    32,
                                    13738,
                                    422,
                                    40,
                                    13746
                                ], "Uninitialized letrec identifier"))(link52(f430, r2434)));
                            }
                            $ans428 = $ans435;
                            break;
                        default:
                            _runtime["throwNoCaseesMatched"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                413,
                                12,
                                13364,
                                424,
                                15,
                                13796
                            ], cases427);
                            $ans428 = _runtime["throwNoCaseesMatched"]([
                                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                                413,
                                12,
                                13364,
                                424,
                                15,
                                13796
                            ], cases427);
                        }
                        $ans424 = $ans428;
                        break;
                    default:
                        _runtime["throwNoCaseesMatched"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            410,
                            8,
                            13278,
                            425,
                            11,
                            13808
                        ], cases423);
                        $ans424 = _runtime["throwNoCaseesMatched"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            410,
                            8,
                            13278,
                            425,
                            11,
                            13808
                        ], cases423);
                    }
                    $ans420 = $ans424;
                    break;
                case "tree-set":
                    var elems436 = cases419[cases419["$fieldNames"][0]];
                    var cases437 = elems436;
                    var $ans438;
                    switch (cases437["$name"]) {
                    case "leaf":
                        $ans438 = pick$none425;
                        break;
                    case "branch":
                        var v439 = cases437[cases437["$fieldNames"][0]];
                        var $underscore441 = cases437[cases437["$fieldNames"][1]];
                        var $underscore442 = cases437[cases437["$fieldNames"][2]];
                        var $underscore443 = cases437[cases437["$fieldNames"][3]];
                        $ans438 = pick$some429(v439, (tree$set440 !== undefined ? tree$set440 : _runtime["$messageThrow"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            430,
                            25,
                            13955,
                            430,
                            33,
                            13963
                        ], "Uninitialized letrec identifier"))(elems436["remove"](v439)));
                        break;
                    default:
                        _runtime["throwNoCaseesMatched"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            427,
                            8,
                            13844,
                            431,
                            11,
                            13993
                        ], cases437);
                        $ans438 = _runtime["throwNoCaseesMatched"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            427,
                            8,
                            13844,
                            431,
                            11,
                            13993
                        ], cases437);
                    }
                    $ans420 = $ans438;
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        407,
                        4,
                        13206,
                        432,
                        7,
                        14001
                    ], cases419);
                    $ans420 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        407,
                        4,
                        13206,
                        432,
                        7,
                        14001
                    ], cases419);
                }
                return $ans420;
            });
        },
        "fold": function getWrapper_fold455() {
            var self446 = this;
            return _runtime["$installMethod"](self446, "fold", function lam_fold454(f450, base451) {
                var cases447 = self446;
                var $ans448;
                switch (cases447["$name"]) {
                case "list-set":
                    var elems452 = cases447[cases447["$fieldNames"][0]];
                    $ans448 = fold449(f450, base451, elems452);
                    break;
                case "tree-set":
                    var elems453 = cases447[cases447["$fieldNames"][0]];
                    $ans448 = tree$fold216(f450, base451, elems453);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        439,
                        4,
                        14196,
                        442,
                        7,
                        14321
                    ], cases447);
                    $ans448 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        439,
                        4,
                        14196,
                        442,
                        7,
                        14321
                    ], cases447);
                }
                return $ans448;
            });
        },
        "member": function getWrapper_member463() {
            var self456 = this;
            return _runtime["$installMethod"](self456, "member", function lam_member462(elem460) {
                var cases457 = self456;
                var $ans458;
                switch (cases457["$name"]) {
                case "list-set":
                    var elems459 = cases457[cases457["$fieldNames"][0]];
                    $ans458 = elems459["member"](elem460);
                    break;
                case "tree-set":
                    var elems461 = cases457[cases457["$fieldNames"][0]];
                    $ans458 = elems461["contains"](elem460);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        448,
                        4,
                        14431,
                        451,
                        7,
                        14549
                    ], cases457);
                    $ans458 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        448,
                        4,
                        14431,
                        451,
                        7,
                        14549
                    ], cases457);
                }
                return $ans458;
            });
        },
        "add": function getWrapper_add472() {
            var self464 = this;
            return _runtime["$installMethod"](self464, "add", function lam_add471(elem467) {
                var cases465 = self464;
                var $ans466;
                switch (cases465["$name"]) {
                case "list-set":
                    var elems468 = cases465[cases465["$fieldNames"][0]];
                    if (elems468["member"](elem467)) {
                        $ans469 = self464;
                    } else {
                        $ans469 = (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                            461,
                            10,
                            14786,
                            461,
                            18,
                            14794
                        ], "Uninitialized letrec identifier"))(link52(elem467, elems468));
                    }
                    $ans466 = $ans469;
                    break;
                case "tree-set":
                    var elems470 = cases465[cases465["$fieldNames"][0]];
                    $ans466 = (tree$set440 !== undefined ? tree$set440 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        463,
                        27,
                        14853,
                        463,
                        35,
                        14861
                    ], "Uninitialized letrec identifier"))(elems470["insert"](elem467));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        456,
                        4,
                        14670,
                        464,
                        7,
                        14889
                    ], cases465);
                    $ans466 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        456,
                        4,
                        14670,
                        464,
                        7,
                        14889
                    ], cases465);
                }
                return $ans466;
            });
        },
        "remove": function getWrapper_remove50() {
            var self473 = this;
            return _runtime["$installMethod"](self473, "remove", function lam_remove49(elem477) {
                var cases474 = self473;
                var $ans475;
                switch (cases474["$name"]) {
                case "list-set":
                    var elems476 = cases474[cases474["$fieldNames"][0]];
                    $ans475 = (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        470,
                        27,
                        15050,
                        470,
                        35,
                        15058
                    ], "Uninitialized letrec identifier"))(elems476["remove"](elem477));
                    break;
                case "tree-set":
                    var elems478 = cases474[cases474["$fieldNames"][0]];
                    $ans475 = (tree$set440 !== undefined ? tree$set440 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        471,
                        27,
                        15106,
                        471,
                        35,
                        15114
                    ], "Uninitialized letrec identifier"))(elems478["remove"](elem477));
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        469,
                        4,
                        15006,
                        472,
                        7,
                        15142
                    ], cases474);
                    $ans475 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        469,
                        4,
                        15006,
                        472,
                        7,
                        15142
                    ], cases474);
                }
                return $ans475;
            });
        },
        "to-list": function getWrapper_to$list187() {
            var self479 = this;
            return _runtime["$installMethod"](self479, "to-list", function lam_to$list186() {
                var cases480 = self479;
                var $ans481;
                switch (cases480["$name"]) {
                case "list-set":
                    var elems482 = cases480[cases480["$fieldNames"][0]];
                    $ans481 = elems482;
                    break;
                case "tree-set":
                    var elems483 = cases480[cases480["$fieldNames"][0]];
                    $ans481 = elems483["inorder"]();
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        477,
                        4,
                        15240,
                        480,
                        7,
                        15340
                    ], cases480);
                    $ans481 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        477,
                        4,
                        15240,
                        480,
                        7,
                        15340
                    ], cases480);
                }
                return $ans481;
            });
        },
        "union": function getWrapper_union495() {
            var self484 = this;
            return _runtime["$installMethod"](self484, "union", function lam_union494(other487) {
                var cases485 = self484;
                var $ans486;
                switch (cases485["$name"]) {
                case "list-set":
                    var elems491 = cases485[cases485["$fieldNames"][0]];
                    $ans486 = other487["fold"](function lam_490(u488, elem489) {
                        return u488["add"](elem489);
                    }, (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        489,
                        13,
                        15587,
                        489,
                        21,
                        15595
                    ], "Uninitialized letrec identifier"))(elems491));
                    break;
                case "tree-set":
                    var $underscore493 = cases485[cases485["$fieldNames"][0]];
                    $ans486 = (tree$set$union492 !== undefined ? tree$set$union492 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        490,
                        23,
                        15627,
                        490,
                        37,
                        15641
                    ], "Uninitialized letrec identifier"))(self484, other487);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        485,
                        4,
                        15460,
                        491,
                        7,
                        15662
                    ], cases485);
                    $ans486 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        485,
                        4,
                        15460,
                        491,
                        7,
                        15662
                    ], cases485);
                }
                return $ans486;
            });
        },
        "intersect": function getWrapper_intersect509() {
            var self496 = this;
            return _runtime["$installMethod"](self496, "intersect", function lam_intersect508(other502) {
                var cases497 = self496;
                var $ans498;
                switch (cases497["$name"]) {
                case "list-set":
                    var elems504 = cases497[cases497["$fieldNames"][0]];
                    var new$elems505 = fold449(function lam_loop503(elems499, elem500) {
                        if (other502["member"](elem500)) {
                            $ans501 = elems499;
                        } else {
                            $ans501 = elems499["remove"](elem500);
                        }
                        return $ans501;
                    }, elems504, elems504);
                    $ans498 = (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        506,
                        8,
                        16042,
                        506,
                        16,
                        16050
                    ], "Uninitialized letrec identifier"))(new$elems505);
                    break;
                case "tree-set":
                    var $underscore507 = cases497[cases497["$fieldNames"][0]];
                    $ans498 = (tree$set$intersect506 !== undefined ? tree$set$intersect506 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        507,
                        23,
                        16085,
                        507,
                        41,
                        16103
                    ], "Uninitialized letrec identifier"))(self496, other502);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        497,
                        4,
                        15794,
                        508,
                        7,
                        16124
                    ], cases497);
                    $ans498 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        497,
                        4,
                        15794,
                        508,
                        7,
                        16124
                    ], cases497);
                }
                return $ans498;
            });
        },
        "overlaps": function getWrapper_overlaps513() {
            var self510 = this;
            return _runtime["$installMethod"](self510, "overlaps", function lam_overlaps512(other511) {
                return self510["any"](other511["member"]);
            });
        },
        "difference": function getWrapper_difference526() {
            var self514 = this;
            return _runtime["$installMethod"](self514, "difference", function lam_difference525(other519) {
                var cases515 = self514;
                var $ans516;
                switch (cases515["$name"]) {
                case "list-set":
                    var elems521 = cases515[cases515["$fieldNames"][0]];
                    var new$elems522 = fold449(function lam_loop503(elems517, elem520) {
                        if (other519["member"](elem520)) {
                            $ans518 = elems517["remove"](elem520);
                        } else {
                            $ans518 = elems517;
                        }
                        return $ans518;
                    }, elems521, elems521);
                    $ans516 = (list$set431 !== undefined ? list$set431 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        527,
                        6,
                        16692,
                        527,
                        14,
                        16700
                    ], "Uninitialized letrec identifier"))(new$elems522);
                    break;
                case "tree-set":
                    var $underscore524 = cases515[cases515["$fieldNames"][0]];
                    $ans516 = (tree$set$difference523 !== undefined ? tree$set$difference523 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        528,
                        23,
                        16735,
                        528,
                        42,
                        16754
                    ], "Uninitialized letrec identifier"))(self514, other519);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        518,
                        4,
                        16446,
                        529,
                        7,
                        16775
                    ], cases515);
                    $ans516 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        518,
                        4,
                        16446,
                        529,
                        7,
                        16775
                    ], cases515);
                }
                return $ans516;
            });
        },
        "size": function getWrapper_size533() {
            var self527 = this;
            return _runtime["$installMethod"](self527, "size", function lam_size532() {
                var cases528 = self527;
                var $ans529;
                switch (cases528["$name"]) {
                case "list-set":
                    var elems530 = cases528[cases528["$fieldNames"][0]];
                    $ans529 = elems530["length"]();
                    break;
                case "tree-set":
                    var elems531 = cases528[cases528["$fieldNames"][0]];
                    $ans529 = elems531["count"]();
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        534,
                        4,
                        16830,
                        537,
                        7,
                        16937
                    ], cases528);
                    $ans529 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        534,
                        4,
                        16830,
                        537,
                        7,
                        16937
                    ], cases528);
                }
                return $ans529;
            });
        },
        "is-empty": function getWrapper_is$empty541() {
            var self534 = this;
            return _runtime["$installMethod"](self534, "is-empty", function lam_is$empty540() {
                var cases535 = self534;
                var $ans536;
                switch (cases535["$name"]) {
                case "list-set":
                    var elems538 = cases535[cases535["$fieldNames"][0]];
                    $ans536 = is$empty537(elems538);
                    break;
                case "tree-set":
                    var elems539 = cases535[cases535["$fieldNames"][0]];
                    $ans536 = is$leaf209(elems539);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        541,
                        4,
                        16975,
                        544,
                        7,
                        17084
                    ], cases535);
                    $ans536 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        541,
                        4,
                        16975,
                        544,
                        7,
                        17084
                    ], cases535);
                }
                return $ans536;
            });
        },
        "all": function getWrapper_all174() {
            var self542 = this;
            return _runtime["$installMethod"](self542, "all", function lam_all173(f546) {
                var cases543 = self542;
                var $ans544;
                switch (cases543["$name"]) {
                case "list-set":
                    var elems545 = cases543[cases543["$fieldNames"][0]];
                    $ans544 = elems545["all"](f546);
                    break;
                case "tree-set":
                    var elems547 = cases543[cases543["$fieldNames"][0]];
                    $ans544 = elems547["all"](f546);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        548,
                        4,
                        17131,
                        551,
                        7,
                        17235
                    ], cases543);
                    $ans544 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        548,
                        4,
                        17131,
                        551,
                        7,
                        17235
                    ], cases543);
                }
                return $ans544;
            });
        },
        "any": function getWrapper_any184() {
            var self548 = this;
            return _runtime["$installMethod"](self548, "any", function lam_any183(f552) {
                var cases549 = self548;
                var $ans550;
                switch (cases549["$name"]) {
                case "list-set":
                    var elems551 = cases549[cases549["$fieldNames"][0]];
                    $ans550 = elems551["any"](f552);
                    break;
                case "tree-set":
                    var elems553 = cases549[cases549["$fieldNames"][0]];
                    $ans550 = elems553["any"](f552);
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        555,
                        4,
                        17282,
                        558,
                        7,
                        17386
                    ], cases549);
                    $ans550 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        555,
                        4,
                        17282,
                        558,
                        7,
                        17386
                    ], cases549);
                }
                return $ans550;
            });
        },
        "symmetric-difference": function getWrapper_symmetric$difference557() {
            var self554 = this;
            return _runtime["$installMethod"](self554, "symmetric-difference", function lam_symmetric$difference556(other555) {
                return self554["union"](other555)["difference"](self554["intersect"](other555));
            });
        },
        "_equals": function getWrapper__equals196() {
            var self558 = this;
            return _runtime["$installMethod"](self558, "_equals", function lam__equals195(other560, eq564) {
                if (not192((is$Set569 !== undefined ? is$Set569 : _runtime["$messageThrow"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        567,
                        11,
                        17652,
                        567,
                        17,
                        17658
                    ], "Uninitialized letrec identifier"))(other560))) {
                    $ans568 = equality194["NotEqual"]("Non-Set", self558, other560);
                } else {
                    var self$list559 = self558["to-list"]();
                    var other$list561 = other560["to-list"]();
                    if (not192(_runtime["equal-always"](other$list561["length"](), self$list559["length"]()))) {
                        $ans567 = equality194["NotEqual"]("set size", self558, other560);
                    } else {
                        $ans567 = fold449(function lam_loop503(result566, elt563) {
                            var result$for$elt565 = member$with562(other$list561, elt563, eq564);
                            return equality194["equal-and"](result566, result$for$elt565);
                        }, equality194["Equal"], self$list559);
                    }
                    $ans568 = $ans567;
                }
                return $ans568;
            });
        }
    }
});
var variantBase_list$set571 = _runtime["$createVariant"](sharedBase_Set570, { "$methods": {} }, {
    "$data": sharedBase_Set570,
    "$name": "list-set",
    "$fieldNames": ["elems"]
});
var variantBase_tree$set572 = _runtime["$createVariant"](sharedBase_Set570, { "$methods": {} }, {
    "$data": sharedBase_Set570,
    "$name": "tree-set",
    "$fieldNames": ["elems"]
});
var Set580 = {
    "list-set": function list$set574(elems573) {
        return _runtime["$makeDataValue"](variantBase_list$set571, { "elems": elems573 });
    },
    "tree-set": function tree$set576(elems575) {
        return _runtime["$makeDataValue"](variantBase_tree$set572, { "elems": elems575 });
    },
    "is-list-set": function is$list$set577(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_list$set571["$variant"];
    },
    "is-tree-set": function is$tree$set578(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_tree$set572["$variant"];
    },
    "is-Set": function is$Set579(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_Set570;
    }
};
var is$Set569 = Set580["is-Set"];
var is$list$set581 = Set580["is-list-set"];
var list$set431 = Set580["list-set"];
var is$tree$set582 = Set580["is-tree-set"];
var tree$set440 = Set580["tree-set"];
var set$to$sorted$elems589 = function lam_set$to$sorted$elems588(s583) {
    var cases584 = s583;
    var $ans585;
    switch (cases584["$name"]) {
    case "list-set":
        var elems586 = cases584[cases584["$fieldNames"][0]];
        $ans585 = elems586["sort"]();
        break;
    case "tree-set":
        var elems587 = cases584[cases584["$fieldNames"][0]];
        $ans585 = elems587["inorder"]();
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            585,
            2,
            18210,
            588,
            5,
            18308
        ], cases584);
        $ans585 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
            585,
            2,
            18210,
            588,
            5,
            18308
        ], cases584);
    }
    return $ans585;
};
var elems$to$balanced$avl620 = function lam_elems$to$balanced$avl619(elems590) {
    var len591 = elems590["length"]();
    var helperv2595 = function lam_helperv2615(l597, head592) {
        if (_runtime["_lessequal"](l597, 0, _runtime["$errCallbacks"])) {
            $ans614 = _runtime["PTuple"]([
                head592,
                leaf28
            ]);
        } else {
            var cases593 = head592;
            var $ans594;
            switch (cases593["$name"]) {
            case "link":
                var first612 = cases593[cases593["$fieldNames"][0]];
                var rest613 = cases593[cases593["$fieldNames"][1]];
                var tup598 = helperv2595(num$floor596(_runtime["_divide"](l597, 2, _runtime["$errCallbacks"])), head592);
                var left$head$out599 = tup598[0];
                var left600 = tup598[1];
                var cases601 = left$head$out599;
                var $ans602;
                switch (cases601["$name"]) {
                case "link":
                    var newf604 = cases601[cases601["$fieldNames"][0]];
                    var inner$rest603 = cases601[cases601["$fieldNames"][1]];
                    $ans602 = _runtime["PTuple"]([
                        inner$rest603,
                        newf604
                    ]);
                    break;
                case "empty":
                    $ans602 = raise300("unreachable");
                    break;
                default:
                    _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        629,
                        36,
                        19791,
                        632,
                        13,
                        19936
                    ], cases601);
                    $ans602 = _runtime["throwNoCaseesMatched"]([
                        "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                        629,
                        36,
                        19791,
                        632,
                        13,
                        19936
                    ], cases601);
                }
                var tup605 = $ans602;
                var right$head$in606 = tup605[0];
                var item607 = tup605[1];
                var tup609 = helperv2595(num$ceiling608(_runtime["_minus"](_runtime["_divide"](l597, 2, _runtime["$errCallbacks"]), 1, _runtime["$errCallbacks"])), right$head$in606);
                var right$head$out610 = tup609[0];
                var right611 = tup609[1];
                $ans594 = _runtime["PTuple"]([
                    right$head$out610,
                    branch211(item607, _runtime["_plus"](left600["height"](), 1, _runtime["$errCallbacks"]), left600, right611)
                ]);
                break;
            case "empty":
                $ans594 = _runtime["PTuple"]([
                    head592,
                    leaf28
                ]);
                break;
            default:
                _runtime["throwNoCaseesMatched"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                    626,
                    6,
                    19621,
                    637,
                    9,
                    20144
                ], cases593);
                $ans594 = _runtime["throwNoCaseesMatched"]([
                    "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                    626,
                    6,
                    19621,
                    637,
                    9,
                    20144
                ], cases593);
            }
            $ans614 = $ans594;
        }
        return $ans614;
    };
    var tup616 = helperv2595(len591, elems590);
    var $underscore617 = tup616[0];
    var result618 = tup616[1];
    return result618;
};
var $underscore634 = undefined;
var merge$no$dups636 = function lam_merge$no$dups642(l1637, l2635) {
    if (is$empty537(l1637)) {
        $ans641 = l2635;
    } else {
        if (is$empty537(l2635)) {
            $ans640 = l1637;
        } else {
            if (_runtime["_lessthan"](l1637["first"], l2635["first"], _runtime["$errCallbacks"])) {
                $ans639 = link52(l1637["first"], merge$no$dups636(l1637["rest"], l2635));
            } else {
                if (_runtime["equal-always"](l1637["first"], l2635["first"])) {
                    $ans638 = merge$no$dups636(l1637["rest"], l2635);
                } else {
                    $ans638 = link52(l2635["first"], merge$no$dups636(l1637, l2635["rest"]));
                }
                $ans639 = $ans638;
            }
            $ans640 = $ans639;
        }
        $ans641 = $ans640;
    }
    return $ans641;
};
var $underscore646 = undefined;
var tree$set$union492 = function lam_tree$set$union652(s1647, s2649) {
    var s1$elems648 = set$to$sorted$elems589(s1647);
    var s2$elems650 = set$to$sorted$elems589(s2649);
    var new$elems651 = merge$no$dups636(s1$elems648, s2$elems650);
    return tree$set440(elems$to$balanced$avl620(new$elems651));
};
var merge$only$dups653 = function lam_merge$only$dups659(l1654, l2655) {
    if (is$empty537(l1654) || is$empty537(l2655)) {
        $ans658 = empty57;
    } else {
        if (_runtime["_lessthan"](l1654["first"], l2655["first"], _runtime["$errCallbacks"])) {
            $ans657 = merge$only$dups653(l1654["rest"], l2655);
        } else {
            if (_runtime["equal-always"](l1654["first"], l2655["first"])) {
                $ans656 = link52(l1654["first"], merge$only$dups653(l1654["rest"], l2655["rest"]));
            } else {
                $ans656 = merge$only$dups653(l1654, l2655["rest"]);
            }
            $ans657 = $ans656;
        }
        $ans658 = $ans657;
    }
    return $ans658;
};
var $underscore663 = undefined;
var tree$set$intersect506 = function lam_tree$set$intersect669(s1664, s2666) {
    var s1$elems665 = set$to$sorted$elems589(s1664);
    var s2$elems667 = set$to$sorted$elems589(s2666);
    var new$elems668 = merge$only$dups653(s1$elems665, s2$elems667);
    return tree$set440(elems$to$balanced$avl620(new$elems668));
};
var merge$drop$l2670 = function lam_merge$drop$l2676(l1671, l2672) {
    if (is$empty537(l1671) || is$empty537(l2672)) {
        $ans675 = l1671;
    } else {
        if (_runtime["equal-always"](l1671["first"], l2672["first"])) {
            $ans674 = merge$drop$l2670(l1671["rest"], l2672["rest"]);
        } else {
            if (_runtime["_lessthan"](l1671["first"], l2672["first"], _runtime["$errCallbacks"])) {
                $ans673 = link52(l1671["first"], merge$drop$l2670(l1671["rest"], l2672));
            } else {
                $ans673 = merge$drop$l2670(l1671, l2672["rest"]);
            }
            $ans674 = $ans673;
        }
        $ans675 = $ans674;
    }
    return $ans675;
};
var $underscore680 = undefined;
var tree$set$difference523 = function lam_tree$set$difference686(s1681, s2683) {
    var s1$elems682 = set$to$sorted$elems589(s1681);
    var s2$elems684 = set$to$sorted$elems589(s2683);
    var new$elems685 = merge$drop$l2670(s1$elems682, s2$elems684);
    return tree$set440(elems$to$balanced$avl620(new$elems685));
};
var set$all690 = function lam_set$all689(f688, s687) {
    return s687["all"](f688);
};
var set$any694 = function lam_set$any693(f692, s691) {
    return s691["any"](f692);
};
var set$fold699 = function lam_set$fold698(f696, base697, s695) {
    return s695["fold"](f696, base697);
};
var list$to$set705 = function lam_list$to$set704(lst703, base$set702) {
    return fold449(function lam_loop503(s700, elem701) {
        return s700["add"](elem701);
    }, base$set702, lst703);
};
var list$to$list$set708 = function lam_list$to$list$set707(lst706) {
    return list$to$set705(lst706, list$set431(empty57));
};
var list$to$tree$set711 = function lam_list$to$tree$set710(lst709) {
    return list$to$set705(lst709, tree$set440(leaf28));
};
var list$to$tree716 = function lam_list$to$tree715(lst714) {
    return fold449(function lam_loop503(tree712, elt713) {
        return tree712["insert"](elt713);
    }, leaf28, lst714);
};
var arr$to$list$set722 = function lam_arr$to$list$set721(arr720) {
    return raw$array$fold717(function lam_490(acc718, elem719) {
        return acc718["add"](elem719);
    }, list$set431(empty57), arr720);
};
var arr$to$tree$set728 = function lam_arr$to$tree$set727(arr725) {
    var tree726 = raw$array$fold717(function lam_490(acc723, elem724) {
        return acc723["insert"](elem724);
    }, leaf28, arr725);
    return tree$set440(tree726);
};
var empty$list$set729 = list$set431(empty57);
var empty$tree$set730 = tree$set440(leaf28);
var makeSet2735 = function lam_makeSet2734(a731, b732) {
    if (_runtime["equal-always"](a731, b732)) {
        $ans733 = link52(a731, empty57);
    } else {
        $ans733 = link52(a731, link52(b732, empty57));
    }
    return $ans733;
};
var makeSet3742 = function lam_makeSet3741(a736, b737, c738) {
    if (_runtime["equal-always"](a736, b737)) {
        $ans740 = makeSet2735(b737, c738);
    } else {
        if (_runtime["equal-always"](a736, c738)) {
            $ans739 = makeSet2735(a736, b737);
        } else {
            $ans739 = link52(a736, makeSet2735(b737, c738));
        }
        $ans740 = $ans739;
    }
    return $ans740;
};
var makeSet4751 = function lam_makeSet4750(a743, b744, c745, d746) {
    if (_runtime["equal-always"](a743, b744)) {
        $ans749 = makeSet3742(b744, c745, d746);
    } else {
        if (_runtime["equal-always"](a743, c745)) {
            $ans748 = makeSet3742(a743, b744, d746);
        } else {
            if (_runtime["equal-always"](a743, d746)) {
                $ans747 = makeSet3742(a743, b744, c745);
            } else {
                $ans747 = link52(a743, makeSet3742(b744, c745, d746));
            }
            $ans748 = $ans747;
        }
        $ans749 = $ans748;
    }
    return $ans749;
};
var makeSet5762 = function lam_makeSet5761(a752, b753, c754, d755, e756) {
    if (_runtime["equal-always"](a752, b753)) {
        $ans760 = makeSet4751(b753, c754, d755, e756);
    } else {
        if (_runtime["equal-always"](a752, c754)) {
            $ans759 = makeSet4751(a752, b753, d755, e756);
        } else {
            if (_runtime["equal-always"](a752, d755)) {
                $ans758 = makeSet4751(a752, b753, c754, e756);
            } else {
                if (_runtime["equal-always"](a752, e756)) {
                    $ans757 = makeSet4751(a752, b753, c754, d755);
                } else {
                    $ans757 = link52(a752, makeSet4751(b753, c754, d755, e756));
                }
                $ans758 = $ans757;
            }
            $ans759 = $ans758;
        }
        $ans760 = $ans759;
    }
    return $ans760;
};
var list$set778 = {
    "make": arr$to$list$set722,
    "make0": function lam_490() {
        return empty$list$set729;
    },
    "make1": function lam_490(a763) {
        return list$set431(link52(a763, empty57));
    },
    "make2": function lam_490(a764, b765) {
        return list$set431(makeSet2735(a764, b765));
    },
    "make3": function lam_490(a766, b767, c768) {
        return list$set431(makeSet3742(a766, b767, c768));
    },
    "make4": function lam_490(a769, b770, c771, d772) {
        return list$set431(makeSet4751(a769, b770, c771, d772));
    },
    "make5": function lam_490(a773, b774, c775, d776, e777) {
        return list$set431(makeSet5762(a773, b774, c775, d776, e777));
    },
    "$methods": {}
};
var tree$set794 = {
    "make": arr$to$tree$set728,
    "make0": function lam_490() {
        return empty$tree$set730;
    },
    "make1": function lam_490(a779) {
        return empty$tree$set730["add"](a779);
    },
    "make2": function lam_490(a780, b781) {
        return empty$tree$set730["add"](a780)["add"](b781);
    },
    "make3": function lam_490(a782, b783, c784) {
        return empty$tree$set730["add"](a782)["add"](b783)["add"](c784);
    },
    "make4": function lam_490(a785, b786, c787, d788) {
        return empty$tree$set730["add"](a785)["add"](b786)["add"](c787)["add"](d788);
    },
    "make5": function lam_490(a789, b790, c791, d792, e793) {
        return empty$tree$set730["add"](a789)["add"](b790)["add"](c791)["add"](d792)["add"](e793);
    },
    "$methods": {}
};
var empty$set795 = empty$list$set729;
var set796 = list$set778;
var list$to$set797 = list$to$list$set708;
var fold798 = set$fold699;
var all799 = set$all690;
var any800 = set$any694;
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:380:0-390:3", "$check$block416", function $check$block416() {
    var tree1402 = branch211(4, 666, branch211(2, 666, branch211(1, 666, leaf28, leaf28), branch211(3, 666, leaf28, leaf28)), branch211(6, 666, branch211(5, 666, leaf28, leaf28), leaf28));
    _runtime["$checkTest"](function $LHS() {
        return tree1402["inorder"]();
    }, function $RHS() {
        return list403["make"]([
            1,
            2,
            3,
            4,
            5,
            6
        ]);
    }, function $TEST($lhs404, $rhs405) {
        if ($lhs404["exception"]) {
            return {
                "success": false,
                "lhs": $lhs404,
                "rhs": $rhs405
            };
        }
        if ($rhs405["exception"]) {
            return {
                "success": false,
                "lhs": $lhs404,
                "rhs": $rhs405
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs404["value"], $rhs405["value"]),
            "lhs": $lhs404,
            "rhs": $rhs405
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:384:2-384:47");
    _runtime["$checkTest"](function $LHS() {
        return tree1402["preorder"]();
    }, function $RHS() {
        return list403["make"]([
            4,
            2,
            1,
            3,
            6,
            5
        ]);
    }, function $TEST($lhs406, $rhs407) {
        if ($lhs406["exception"]) {
            return {
                "success": false,
                "lhs": $lhs406,
                "rhs": $rhs407
            };
        }
        if ($rhs407["exception"]) {
            return {
                "success": false,
                "lhs": $lhs406,
                "rhs": $rhs407
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs406["value"], $rhs407["value"]),
            "lhs": $lhs406,
            "rhs": $rhs407
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:385:2-385:47");
    _runtime["$checkTest"](function $LHS() {
        return tree1402["postorder"]();
    }, function $RHS() {
        return list403["make"]([
            1,
            3,
            2,
            5,
            6,
            4
        ]);
    }, function $TEST($lhs408, $rhs409) {
        if ($lhs408["exception"]) {
            return {
                "success": false,
                "lhs": $lhs408,
                "rhs": $rhs409
            };
        }
        if ($rhs409["exception"]) {
            return {
                "success": false,
                "lhs": $lhs408,
                "rhs": $rhs409
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs408["value"], $rhs409["value"]),
            "lhs": $lhs408,
            "rhs": $rhs409
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:386:2-386:47");
    _runtime["$checkTest"](function $LHS() {
        return tree1402["revinorder"]();
    }, function $RHS() {
        return list403["make"]([
            6,
            5,
            4,
            3,
            2,
            1
        ]);
    }, function $TEST($lhs410, $rhs411) {
        if ($lhs410["exception"]) {
            return {
                "success": false,
                "lhs": $lhs410,
                "rhs": $rhs411
            };
        }
        if ($rhs411["exception"]) {
            return {
                "success": false,
                "lhs": $lhs410,
                "rhs": $rhs411
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs410["value"], $rhs411["value"]),
            "lhs": $lhs410,
            "rhs": $rhs411
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:387:2-387:50");
    _runtime["$checkTest"](function $LHS() {
        return tree1402["revpreorder"]();
    }, function $RHS() {
        return list403["make"]([
            5,
            6,
            3,
            1,
            2,
            4
        ]);
    }, function $TEST($lhs412, $rhs413) {
        if ($lhs412["exception"]) {
            return {
                "success": false,
                "lhs": $lhs412,
                "rhs": $rhs413
            };
        }
        if ($rhs413["exception"]) {
            return {
                "success": false,
                "lhs": $lhs412,
                "rhs": $rhs413
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs412["value"], $rhs413["value"]),
            "lhs": $lhs412,
            "rhs": $rhs413
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:388:2-388:50");
    _runtime["$checkTest"](function $LHS() {
        return tree1402["revpostorder"]();
    }, function $RHS() {
        return list403["make"]([
            4,
            6,
            5,
            2,
            3,
            1
        ]);
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
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:389:2-389:50");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:644:0-644:6", "elems-to-balanced-avl", function $check$blockelems$to$balanced$avl633() {
    _runtime["$checkTest"](function $LHS() {
        return elems$to$balanced$avl620(list403["make"]([1]));
    }, function $RHS() {
        return branch211(1, 1, leaf28, leaf28);
    }, function $TEST($lhs621, $rhs622) {
        if ($lhs621["exception"]) {
            return {
                "success": false,
                "lhs": $lhs621,
                "rhs": $rhs622
            };
        }
        if ($rhs622["exception"]) {
            return {
                "success": false,
                "lhs": $lhs621,
                "rhs": $rhs622
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs621["value"], $rhs622["value"]),
            "lhs": $lhs621,
            "rhs": $rhs622
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:645:2-646:26");
    _runtime["$checkTest"](function $LHS() {
        return elems$to$balanced$avl620(list403["make"]([
            1,
            2
        ]));
    }, function $RHS() {
        return branch211(2, 2, branch211(1, 1, leaf28, leaf28), leaf28);
    }, function $TEST($lhs623, $rhs624) {
        if ($lhs623["exception"]) {
            return {
                "success": false,
                "lhs": $lhs623,
                "rhs": $rhs624
            };
        }
        if ($rhs624["exception"]) {
            return {
                "success": false,
                "lhs": $lhs623,
                "rhs": $rhs624
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs623["value"], $rhs624["value"]),
            "lhs": $lhs623,
            "rhs": $rhs624
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:647:2-648:46");
    _runtime["$checkTest"](function $LHS() {
        return elems$to$balanced$avl620(list403["make"]([
            1,
            2,
            3
        ]));
    }, function $RHS() {
        return branch211(2, 2, branch211(1, 1, leaf28, leaf28), branch211(3, 1, leaf28, leaf28));
    }, function $TEST($lhs625, $rhs626) {
        if ($lhs625["exception"]) {
            return {
                "success": false,
                "lhs": $lhs625,
                "rhs": $rhs626
            };
        }
        if ($rhs626["exception"]) {
            return {
                "success": false,
                "lhs": $lhs625,
                "rhs": $rhs626
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs625["value"], $rhs626["value"]),
            "lhs": $lhs625,
            "rhs": $rhs626
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:649:2-650:66");
    _runtime["$checkTest"](function $LHS() {
        return elems$to$balanced$avl620(empty57);
    }, function $RHS() {
        return leaf28;
    }, function $TEST($lhs627, $rhs628) {
        if ($lhs627["exception"]) {
            return {
                "success": false,
                "lhs": $lhs627,
                "rhs": $rhs628
            };
        }
        if ($rhs628["exception"]) {
            return {
                "success": false,
                "lhs": $lhs627,
                "rhs": $rhs628
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs627["value"], $rhs628["value"]),
            "lhs": $lhs627,
            "rhs": $rhs628
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:652:2-652:38");
    _runtime["$checkTest"](function $LHS() {
        return elems$to$balanced$avl620(list403["make"]([
            1,
            2,
            3,
            4,
            5
        ]));
    }, function $RHS() {
        return branch211(3, 3, branch211(2, 2, branch211(1, 1, leaf28, leaf28), leaf28), branch211(5, 2, branch211(4, 1, leaf28, leaf28), leaf28));
    }, function $TEST($lhs629, $rhs630) {
        if ($lhs629["exception"]) {
            return {
                "success": false,
                "lhs": $lhs629,
                "rhs": $rhs630
            };
        }
        if ($rhs630["exception"]) {
            return {
                "success": false,
                "lhs": $lhs629,
                "rhs": $rhs630
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs629["value"], $rhs630["value"]),
            "lhs": $lhs629,
            "rhs": $rhs630
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:653:2-655:49");
    _runtime["$checkTest"](function $LHS() {
        return elems$to$balanced$avl620(list403["make"]([
            1,
            2,
            3,
            4,
            5,
            6
        ]));
    }, function $RHS() {
        return branch211(4, 3, branch211(2, 2, branch211(1, 1, leaf28, leaf28), branch211(3, 1, leaf28, leaf28)), branch211(6, 2, branch211(5, 1, leaf28, leaf28), leaf28));
    }, function $TEST($lhs631, $rhs632) {
        if ($lhs631["exception"]) {
            return {
                "success": false,
                "lhs": $lhs631,
                "rhs": $rhs632
            };
        }
        if ($rhs632["exception"]) {
            return {
                "success": false,
                "lhs": $lhs631,
                "rhs": $rhs632
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs631["value"], $rhs632["value"]),
            "lhs": $lhs631,
            "rhs": $rhs632
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:656:2-658:49");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:669:0-669:6", "merge-no-dups", function $check$blockmerge$no$dups645() {
    _runtime["$checkTest"](function $LHS() {
        return merge$no$dups636(list403["make"]([
            1,
            3,
            5,
            6
        ]), list403["make"]([
            1,
            2,
            4,
            5
        ]));
    }, function $RHS() {
        return list403["make"]([
            1,
            2,
            3,
            4,
            5,
            6
        ]);
    }, function $TEST($lhs643, $rhs644) {
        if ($lhs643["exception"]) {
            return {
                "success": false,
                "lhs": $lhs643,
                "rhs": $rhs644
            };
        }
        if ($rhs644["exception"]) {
            return {
                "success": false,
                "lhs": $lhs643,
                "rhs": $rhs644
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs643["value"], $rhs644["value"]),
            "lhs": $lhs643,
            "rhs": $rhs644
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:670:2-670:83");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:687:0-687:6", "merge-only-dups", function $check$blockmerge$only$dups662() {
    _runtime["$checkTest"](function $LHS() {
        return merge$only$dups653(list403["make"]([
            1,
            3,
            5,
            6
        ]), list403["make"]([
            1,
            2,
            4,
            5
        ]));
    }, function $RHS() {
        return list403["make"]([
            1,
            5
        ]);
    }, function $TEST($lhs660, $rhs661) {
        if ($lhs660["exception"]) {
            return {
                "success": false,
                "lhs": $lhs660,
                "rhs": $rhs661
            };
        }
        if ($rhs661["exception"]) {
            return {
                "success": false,
                "lhs": $lhs660,
                "rhs": $rhs661
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs660["value"], $rhs661["value"]),
            "lhs": $lhs660,
            "rhs": $rhs661
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:688:2-688:73");
    undefined;
});
_runtime["$checkBlock"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:705:0-705:6", "merge-drop-l2", function $check$blockmerge$drop$l2679() {
    _runtime["$checkTest"](function $LHS() {
        return merge$drop$l2670(list403["make"]([
            1,
            3,
            5,
            6
        ]), list403["make"]([
            1,
            2,
            4,
            5
        ]));
    }, function $RHS() {
        return list403["make"]([
            3,
            6
        ]);
    }, function $TEST($lhs677, $rhs678) {
        if ($lhs677["exception"]) {
            return {
                "success": false,
                "lhs": $lhs677,
                "rhs": $rhs678
            };
        }
        if ($rhs678["exception"]) {
            return {
                "success": false,
                "lhs": $lhs677,
                "rhs": $rhs678
            };
        }
        return {
            "success": _runtime["equal-always"]($lhs677["value"], $rhs678["value"]),
            "lhs": $lhs677,
            "rhs": $rhs678
        };
    }, "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr:706:2-706:71");
    undefined;
});
var $answer802 = _runtime["trace-value"](["dummy location"], nothing801);
return module["exports"] = {
    "merge-only-dups": merge$only$dups653,
    "mkbranch": mkbranch26,
    "makeSet4": makeSet4751,
    "fold": fold798,
    "list-to-tree": list$to$tree716,
    "all": all799,
    "set-any": set$any694,
    "is-leaf": is$leaf209,
    "makeSet5": makeSet5762,
    "set": set796,
    "branch": branch211,
    "empty-set": empty$set795,
    "tree-set-intersect": tree$set$intersect506,
    "is-branch": is$branch210,
    "tree-get": tree$get230,
    "rebalance": rebalance29,
    "remove-root": remove$root47,
    "merge-no-dups": merge$no$dups636,
    "any": any800,
    "set-fold": set$fold699,
    "tree-get-left": tree$get$left310,
    "merge-drop-l2": merge$drop$l2670,
    "tree-fold": tree$fold216,
    "swap-next-lowest": swap$next$lowest361,
    "is-AVLTree": is$AVLTree193,
    "tree-all": tree$all220,
    "tree-set": tree$set794,
    "empty-tree-set": empty$tree$set730,
    "list-set": list$set778,
    "empty-list-set": empty$list$set729,
    "tree-set-union": tree$set$union492,
    "tree-get-right": tree$get$right302,
    "set-all": set$all690,
    "list-to-tree-set": list$to$tree$set711,
    "is-tree-set": is$tree$set582,
    "list-to-list-set": list$to$list$set708,
    "is-list-set": is$list$set581,
    "tree-set-difference": tree$set$difference523,
    "arr-to-list-set": arr$to$list$set722,
    "arr-to-tree-set": arr$to$tree$set728,
    "elems-to-balanced-avl": elems$to$balanced$avl620,
    "set-to-sorted-elems": set$to$sorted$elems589,
    "tree-any": tree$any224,
    "makeSet2": makeSet2735,
    "leaf": leaf28,
    "is-Set": is$Set569,
    "list-to-set": list$to$set797,
    "makeSet3": makeSet3742,
    "$answer": $answer802,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr"),
    "$locations": [
        {
            "name": "merge-only-dups",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                680,
                0,
                21600,
                689,
                3,
                22049
            ]
        },
        {
            "name": "mkbranch",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                243,
                0,
                7786,
                245,
                3,
                7942
            ]
        },
        {
            "name": "makeSet4",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                786,
                0,
                24435,
                792,
                3,
                24623
            ]
        },
        {
            "name": "fold",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                825,
                0,
                26030,
                825,
                22,
                26052
            ]
        },
        {
            "name": "list-to-tree",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                745,
                0,
                23645,
                749,
                3,
                23768
            ]
        },
        {
            "name": "all",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                826,
                0,
                26053,
                826,
                20,
                26073
            ]
        },
        {
            "name": "set-any",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                720,
                0,
                23031,
                722,
                3,
                23088
            ]
        },
        {
            "name": "is-leaf",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                58,
                2,
                1207,
                58,
                8,
                1213
            ]
        },
        {
            "name": "makeSet5",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                793,
                0,
                24624,
                800,
                3,
                24866
            ]
        },
        {
            "name": "set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                823,
                0,
                25970,
                823,
                21,
                25991
            ]
        },
        {
            "name": "branch",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                59,
                2,
                1216,
                59,
                76,
                1290
            ]
        },
        {
            "name": "empty-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                822,
                0,
                25943,
                822,
                26,
                25969
            ]
        },
        {
            "name": "tree-set-intersect",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                691,
                0,
                22051,
                696,
                3,
                22288
            ]
        },
        {
            "name": "is-branch",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                59,
                2,
                1216,
                59,
                76,
                1290
            ]
        },
        {
            "name": "tree-get",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                303,
                0,
                10242,
                308,
                3,
                10435
            ]
        },
        {
            "name": "rebalance",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                247,
                0,
                7944,
                301,
                3,
                10240
            ]
        },
        {
            "name": "remove-root",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                333,
                0,
                11090,
                351,
                3,
                11501
            ]
        },
        {
            "name": "merge-no-dups",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                661,
                0,
                20885,
                671,
                3,
                21365
            ]
        },
        {
            "name": "any",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                827,
                0,
                26074,
                827,
                20,
                26094
            ]
        },
        {
            "name": "set-fold",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                724,
                0,
                23090,
                726,
                3,
                23178
            ]
        },
        {
            "name": "tree-get-left",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                311,
                0,
                10438,
                320,
                3,
                10760
            ]
        },
        {
            "name": "merge-drop-l2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                698,
                0,
                22290,
                707,
                3,
                22732
            ]
        },
        {
            "name": "tree-fold",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                231,
                0,
                7500,
                233,
                3,
                7608
            ]
        },
        {
            "name": "swap-next-lowest",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                353,
                0,
                11503,
                377,
                3,
                12312
            ]
        },
        {
            "name": "is-AVLTree",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                57,
                0,
                1188,
                229,
                3,
                7498
            ]
        },
        {
            "name": "tree-all",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                235,
                0,
                7610,
                237,
                3,
                7696
            ]
        },
        {
            "name": "tree-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                812,
                0,
                25387,
                820,
                1,
                25941
            ]
        },
        {
            "name": "empty-tree-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                773,
                0,
                24171,
                773,
                31,
                24202
            ]
        },
        {
            "name": "list-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                802,
                0,
                24868,
                810,
                1,
                25385
            ]
        },
        {
            "name": "empty-list-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                772,
                0,
                24138,
                772,
                32,
                24170
            ]
        },
        {
            "name": "tree-set-union",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                673,
                0,
                21367,
                678,
                3,
                21598
            ]
        },
        {
            "name": "tree-get-right",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                322,
                0,
                10762,
                331,
                3,
                11088
            ]
        },
        {
            "name": "set-all",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                716,
                0,
                22972,
                718,
                3,
                23029
            ]
        },
        {
            "name": "list-to-tree-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                740,
                0,
                23507,
                743,
                3,
                23643
            ]
        },
        {
            "name": "is-tree-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                399,
                2,
                12957,
                399,
                33,
                12988
            ]
        },
        {
            "name": "list-to-list-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                735,
                0,
                23368,
                738,
                3,
                23505
            ]
        },
        {
            "name": "is-list-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                394,
                2,
                12797,
                394,
                30,
                12825
            ]
        },
        {
            "name": "tree-set-difference",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                709,
                0,
                22734,
                714,
                3,
                22970
            ]
        },
        {
            "name": "arr-to-list-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                751,
                0,
                23770,
                759,
                3,
                23942
            ]
        },
        {
            "name": "arr-to-tree-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                761,
                0,
                23944,
                770,
                3,
                24136
            ]
        },
        {
            "name": "elems-to-balanced-avl",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                591,
                0,
                18314,
                659,
                3,
                20883
            ]
        },
        {
            "name": "set-to-sorted-elems",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                584,
                0,
                18156,
                589,
                3,
                18312
            ]
        },
        {
            "name": "tree-any",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                239,
                0,
                7698,
                241,
                3,
                7784
            ]
        },
        {
            "name": "makeSet2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                775,
                0,
                24204,
                779,
                3,
                24293
            ]
        },
        {
            "name": "leaf",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                58,
                2,
                1207,
                58,
                8,
                1213
            ]
        },
        {
            "name": "is-Set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                393,
                0,
                12782,
                582,
                3,
                18154
            ]
        },
        {
            "name": "list-to-set",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                824,
                0,
                25992,
                824,
                37,
                26029
            ]
        },
        {
            "name": "makeSet3",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/sets.arr",
                780,
                0,
                24294,
                785,
                3,
                24434
            ]
        }
    ]
};