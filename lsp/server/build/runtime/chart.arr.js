var _runtime = require(".\/runtime.js");
var G37 = require(".\/runtime-global.arr.js");
_runtime["addModule"]("builtin:\/\/runtime-global", G37);
var RA1 = require(".\/raw-array.arr.js");
_runtime["addModule"]("builtin:\/\/raw-array", RA1);
var CL123 = require(".\/chart-lib.arr.js");
_runtime["addModule"]("builtin:\/\/chart-lib", CL123);
var IM128 = require(".\/image.arr.js");
_runtime["addModule"]("builtin:\/\/image", IM128);
var L6 = require(".\/lists.arr.js");
_runtime["addModule"]("builtin:\/\/lists", L6);
var O41 = require(".\/option.arr.js");
_runtime["addModule"]("builtin:\/\/option", O41);
var $G130 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $G130);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr");
var nothing107 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var posn5 = function lam_4(x2, y3) {
    return RA1["raw-array"]["make"]([
        x2,
        y3
    ]);
};
var map212 = function lam_map211(xs9, ys10) {
    return L6["map2"](function lam_4(x7, y8) {
        return RA1["raw-array"]["make"]([
            x7,
            y8
        ]);
    }, xs9, ys10);
};
var raw$array$from$list15 = function lam_raw$array$from$list14(l13) {
    return L6["to-raw-array"](l13);
};
var to$table221 = function lam_to$table220(xs18, ys19) {
    return L6["to-raw-array"](L6["map2"](function lam_4(x16, y17) {
        return RA1["raw-array"]["make"]([
            x16,
            y17
        ]);
    }, xs18, ys19));
};
var default$bounding$box22 = {
    "x-min": 0,
    "x-max": 0,
    "y-min": 0,
    "y-max": 0,
    "is-valid": false,
    "$methods": {}
};
var compute$min25 = function lam_compute$min24(ps23) {
    return RA1["raw-array-min"](ps23);
};
var compute$max28 = function lam_compute$max27(ps26) {
    return RA1["raw-array-max"](ps26);
};
var get$bounding$box34 = function lam_get$bounding$box33(ps29) {
    if (_runtime["equal-always"](L6["length"](ps29), 0)) {
        $ans32 = _runtime["$extend"](default$bounding$box22, {
            "is-valid": false,
            "$methods": {}
        });
    } else {
        var x$arr30 = L6["get"](ps29, 0);
        var y$arr31 = L6["get"](ps29, 1);
        $ans32 = _runtime["$extend"](default$bounding$box22, {
            "x-min": compute$min25(x$arr30),
            "x-max": compute$max28(x$arr30),
            "y-min": compute$min25(y$arr31),
            "y-max": compute$max28(y$arr31),
            "is-valid": true,
            "$methods": {}
        });
    }
    return $ans32;
};
var default$bar$chart$series35 = { "$methods": {} };
var default$chart$window$object40 = _runtime["$setupMethodGetters"]({
    "title": "",
    "width": 800,
    "height": 600,
    "$methods": {
        "render": function getWrapper_render39() {
            var self36 = this;
            return _runtime["$installMethod"](self36, "render", function lam_render38() {
                return G37["raise"]("unimplemented");
            });
        }
    }
});
var default$bar$chart$window$object42 = _runtime["$extend"](default$chart$window$object40, {
    "x-axis": "",
    "y-axis": "",
    "y-min": O41["none"],
    "y-max": O41["none"],
    "$methods": {}
});
var sharedBase_DataSeries43 = { "$methods": {} };
var variantBase_bar$chart$series45 = _runtime["$createVariant"](sharedBase_DataSeries43, {
    "is-single": true,
    "constr": function lam_4() {
        return bar$chart$series44 !== undefined ? bar$chart$series44 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
            147,
            17,
            3614,
            147,
            33,
            3630
        ], "Uninitialized letrec identifier");
    },
    "$methods": {}
}, {
    "$data": sharedBase_DataSeries43,
    "$name": "bar-chart-series",
    "$fieldNames": ["obj"]
});
var DataSeries50 = {
    "bar-chart-series": function bar$chart$series47(obj46) {
        return _runtime["$makeDataValue"](variantBase_bar$chart$series45, { "obj": obj46 });
    },
    "is-bar-chart-series": function is$bar$chart$series48(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_bar$chart$series45["$variant"];
    },
    "is-DataSeries": function is$DataSeries49(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_DataSeries43;
    }
};
var is$DataSeries51 = DataSeries50["is-DataSeries"];
var is$bar$chart$series52 = DataSeries50["is-bar-chart-series"];
var bar$chart$series44 = DataSeries50["bar-chart-series"];
var check$chart$window56 = function lam_check$chart$window55(p54) {
    if (_runtime["_lessequal"](p54["width"], 0, _runtime["$errCallbacks"]) || _runtime["_lessequal"](p54["height"], 0, _runtime["$errCallbacks"])) {
        $ans53 = G37["raise"]("render: width and height must be positive");
    } else {
        $ans53 = G37["nothing"];
    }
    return $ans53;
};
var sharedBase_ChartWindow77 = _runtime["$setupMethodGetters"]({
    "$methods": {
        "display": function getWrapper_display60() {
            var self57 = this;
            return _runtime["$installMethod"](self57, "display", function lam_display59() {
                var $underscore58 = check$chart$window56(self57["obj"]);
                return _runtime["$extend"](self57["obj"], {
                    "interact": true,
                    "$methods": {}
                })["render"]();
            });
        },
        "get-image": function getWrapper_get$image64() {
            var self61 = this;
            return _runtime["$installMethod"](self61, "get-image", function lam_get$image63() {
                var $underscore62 = check$chart$window56(self61["obj"]);
                return _runtime["$extend"](self61["obj"], {
                    "interact": false,
                    "$methods": {}
                })["render"]();
            });
        },
        "title": function getWrapper_title68() {
            var self65 = this;
            return _runtime["$installMethod"](self65, "title", function lam_title67(title66) {
                return self65["constr"]()(_runtime["$extend"](self65["obj"], {
                    "title": title66,
                    "$methods": {}
                }));
            });
        },
        "width": function getWrapper_width72() {
            var self69 = this;
            return _runtime["$installMethod"](self69, "width", function lam_width71(width70) {
                return self69["constr"]()(_runtime["$extend"](self69["obj"], {
                    "width": width70,
                    "$methods": {}
                }));
            });
        },
        "height": function getWrapper_height76() {
            var self73 = this;
            return _runtime["$installMethod"](self73, "height", function lam_height75(height74) {
                return self73["constr"]()(_runtime["$extend"](self73["obj"], {
                    "height": height74,
                    "$methods": {}
                }));
            });
        }
    }
});
var variantBase_bar$chart$window95 = _runtime["$createVariant"](sharedBase_ChartWindow77, _runtime["$setupMethodGetters"]({
    "constr": function lam_4() {
        return bar$chart$window78 !== undefined ? bar$chart$window78 : _runtime["$messageThrow"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
            161,
            17,
            3996,
            161,
            33,
            4012
        ], "Uninitialized letrec identifier");
    },
    "$methods": {
        "x-axis": function getWrapper_x$axis82() {
            var self79 = this;
            return _runtime["$installMethod"](self79, "x-axis", function lam_x$axis81(x$axis80) {
                return self79["constr"]()(_runtime["$extend"](self79["obj"], {
                    "x-axis": x$axis80,
                    "$methods": {}
                }));
            });
        },
        "y-axis": function getWrapper_y$axis86() {
            var self83 = this;
            return _runtime["$installMethod"](self83, "y-axis", function lam_y$axis85(y$axis84) {
                return self83["constr"]()(_runtime["$extend"](self83["obj"], {
                    "y-axis": y$axis84,
                    "$methods": {}
                }));
            });
        },
        "y-min": function getWrapper_y$min90() {
            var self87 = this;
            return _runtime["$installMethod"](self87, "y-min", function lam_y$min89(y$min88) {
                return self87["constr"]()(_runtime["$extend"](self87["obj"], {
                    "y-min": O41["some"](y$min88),
                    "$methods": {}
                }));
            });
        },
        "y-max": function getWrapper_y$max94() {
            var self91 = this;
            return _runtime["$installMethod"](self91, "y-max", function lam_y$max93(y$max92) {
                return self91["constr"]()(_runtime["$extend"](self91["obj"], {
                    "y-max": O41["some"](y$max92),
                    "$methods": {}
                }));
            });
        }
    }
}), {
    "$data": sharedBase_ChartWindow77,
    "$name": "bar-chart-window",
    "$fieldNames": ["obj"]
});
var ChartWindow100 = {
    "bar-chart-window": function bar$chart$window97(obj96) {
        return _runtime["$makeDataValue"](variantBase_bar$chart$window95, { "obj": obj96 });
    },
    "is-bar-chart-window": function is$bar$chart$window98(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_bar$chart$window95["$variant"];
    },
    "is-ChartWindow": function is$ChartWindow99(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_ChartWindow77;
    }
};
var is$ChartWindow101 = ChartWindow100["is-ChartWindow"];
var is$bar$chart$window102 = ChartWindow100["is-bar-chart-window"];
var bar$chart$window78 = ChartWindow100["bar-chart-window"];
var bar$chart$from$list110 = function lam_bar$chart$from$list109(labels103, values105) {
    var label$length104 = L6["length"](labels103);
    var value$length106 = L6["length"](values105);
    if (!_runtime["equal-always"](label$length104, value$length106)) {
        G37["raise"]("bar-chart: labels and values should have the same length");
        nothing107;
        $ans108 = nothing107;
    } else {
        nothing107;
        $ans108 = nothing107;
    }
    $ans108;
    bar$chart$series44(_runtime["$extend"](default$bar$chart$series35, {
        "tab": to$table221(labels103, values105),
        "legends": RA1["raw-array"]["make"]([""]),
        "has-legend": false,
        "$methods": {}
    }));
    return bar$chart$series44(_runtime["$extend"](default$bar$chart$series35, {
        "tab": to$table221(labels103, values105),
        "legends": RA1["raw-array"]["make"]([""]),
        "has-legend": false,
        "$methods": {}
    }));
};
var render$chart126 = function lam_render$chart125(s111) {
    var cases112 = s111;
    var $ans113;
    switch (cases112["$name"]) {
    case "bar-chart-series":
        var obj124 = cases112[cases112["$fieldNames"][0]];
        $ans113 = bar$chart$window78(_runtime["$extend"](default$bar$chart$window$object42, _runtime["$setupMethodGetters"]({
            "$methods": {
                "render": function getWrapper_render39() {
                    var self114 = this;
                    return _runtime["$installMethod"](self114, "render", function lam_render38() {
                        var cases115 = self114["y-min"];
                        var $ans116;
                        switch (cases115["$name"]) {
                        case "some":
                            var y$min120 = cases115[cases115["$fieldNames"][0]];
                            var cases117 = self114["y-max"];
                            var $ans118;
                            switch (cases117["$name"]) {
                            case "some":
                                var y$max121 = cases117[cases117["$fieldNames"][0]];
                                if (_runtime["_greaterequal"](y$min120, y$max121, _runtime["$errCallbacks"])) {
                                    $ans119 = G37["raise"]("render: y-min must be strictly less than y-max");
                                } else {
                                    $ans119 = G37["nothing"];
                                }
                                $ans118 = $ans119;
                                break;
                            default:
                                $ans118 = G37["nothing"];
                            }
                            $ans116 = $ans118;
                            break;
                        default:
                            $ans116 = G37["nothing"];
                        }
                        var $underscore122 = $ans116;
                        return CL123["bar-chart"](obj124["tab"]);
                    });
                }
            }
        })));
        break;
    default:
        _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
            217,
            2,
            6041,
            237,
            5,
            6721
        ], cases112);
        $ans113 = _runtime["throwNoCaseesMatched"]([
            "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
            217,
            2,
            6041,
            237,
            5,
            6721
        ], cases112);
    }
    return $ans113;
};
var $answer127 = _runtime["trace-value"](["dummy location"], nothing107);
return module["exports"] = {
    "bar-chart-series": bar$chart$series44,
    "default-chart-window-object": default$chart$window$object40,
    "is-bar-chart-series": is$bar$chart$series52,
    "check-chart-window": check$chart$window56,
    "is-DataSeries": is$DataSeries51,
    "render-chart": render$chart126,
    "default-bar-chart-window-object": default$bar$chart$window$object42,
    "raw-array-from-list": raw$array$from$list15,
    "is-ChartWindow": is$ChartWindow101,
    "default-bar-chart-series": default$bar$chart$series35,
    "default-bounding-box": default$bounding$box22,
    "compute-max": compute$max28,
    "to-table2": to$table221,
    "map2": map212,
    "bar-chart-from-list": bar$chart$from$list110,
    "bar-chart-window": bar$chart$window78,
    "get-bounding-box": get$bounding$box34,
    "posn": posn5,
    "is-bar-chart-window": is$bar$chart$window102,
    "compute-min": compute$min25,
    "$answer": $answer127,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr"),
    "$locations": [
        {
            "name": "bar-chart-series",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                145,
                2,
                3528,
                147,
                35,
                3632
            ]
        },
        {
            "name": "default-chart-window-object",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                115,
                0,
                2806,
                120,
                1,
                2955
            ]
        },
        {
            "name": "is-bar-chart-series",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                145,
                2,
                3528,
                147,
                35,
                3632
            ]
        },
        {
            "name": "check-chart-window",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                151,
                0,
                3716,
                157,
                3,
                3903
            ]
        },
        {
            "name": "is-DataSeries",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                144,
                0,
                3509,
                149,
                3,
                3714
            ]
        },
        {
            "name": "render-chart",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                215,
                0,
                5969,
                240,
                3,
                6784
            ]
        },
        {
            "name": "default-bar-chart-window-object",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                133,
                0,
                3174,
                138,
                1,
                3325
            ]
        },
        {
            "name": "raw-array-from-list",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                35,
                0,
                952,
                37,
                3,
                1035
            ]
        },
        {
            "name": "is-ChartWindow",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                159,
                0,
                3905,
                185,
                3,
                4959
            ]
        },
        {
            "name": "default-bar-chart-series",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                104,
                0,
                2647,
                104,
                29,
                2676
            ]
        },
        {
            "name": "default-bounding-box",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                62,
                0,
                1714,
                68,
                1,
                1822
            ]
        },
        {
            "name": "compute-max",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                74,
                0,
                1903,
                76,
                3,
                1980
            ]
        },
        {
            "name": "to-table2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                39,
                0,
                1037,
                41,
                3,
                1173
            ]
        },
        {
            "name": "map2",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                31,
                0,
                850,
                33,
                3,
                950
            ]
        },
        {
            "name": "bar-chart-from-list",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                191,
                0,
                5136,
                209,
                3,
                5796
            ]
        },
        {
            "name": "bar-chart-window",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                160,
                2,
                3925,
                165,
                92,
                4378
            ]
        },
        {
            "name": "get-bounding-box",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                78,
                0,
                1982,
                92,
                3,
                2360
            ]
        },
        {
            "name": "posn",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                29,
                0,
                791,
                29,
                57,
                848
            ]
        },
        {
            "name": "is-bar-chart-window",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                160,
                2,
                3925,
                165,
                92,
                4378
            ]
        },
        {
            "name": "compute-min",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr\/chart.arr",
                70,
                0,
                1824,
                72,
                3,
                1901
            ]
        }
    ]
};