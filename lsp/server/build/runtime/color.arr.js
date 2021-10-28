var _runtime = require(".\/runtime.js");
var $underscore_import160 = require(".\/primitive-types.arr.js");
_runtime["addModule"]("builtin:\/\/primitive-types", $underscore_import160);
_runtime["$clearTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr");
_runtime["$clearChecks"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr");
var nothing158 = _runtime["getModuleValue"]("builtin:\/\/primitive-types", "nothing");
var sharedBase_Color1 = { "$methods": {} };
var variantBase_color2 = _runtime["$createVariant"](sharedBase_Color1, { "$methods": {} }, {
    "$data": sharedBase_Color1,
    "$name": "color",
    "$fieldNames": [
        "red",
        "green",
        "blue",
        "alpha"
    ]
});
var Color10 = {
    "color": function color7(red3, green4, blue5, alpha6) {
        return _runtime["$makeDataValue"](variantBase_color2, {
            "red": red3,
            "green": green4,
            "blue": blue5,
            "alpha": alpha6
        });
    },
    "is-color": function is$color8(val) {
        return typeof val === "object" && val !== null && val["$variant"] === variantBase_color2["$variant"];
    },
    "is-Color": function is$Color9(val) {
        return typeof val === "object" && val !== null && val["$data"] === sharedBase_Color1;
    }
};
var is$Color11 = Color10["is-Color"];
var is$color12 = Color10["is-color"];
var color13 = Color10["color"];
var orange14 = color13(255, 165, 0, 1);
var red15 = color13(255, 0, 0, 1);
var orange$red16 = color13(255, 69, 0, 1);
var tomato17 = color13(255, 99, 71, 1);
var dark$red18 = color13(139, 0, 0, 1);
var fire$brick19 = color13(178, 34, 34, 1);
var crimson20 = color13(220, 20, 60, 1);
var deep$pink21 = color13(255, 20, 147, 1);
var maroon22 = color13(176, 48, 96, 1);
var indian$red23 = color13(205, 92, 92, 1);
var medium$violet$red24 = color13(199, 21, 133, 1);
var violet$red25 = color13(208, 32, 144, 1);
var light$coral26 = color13(240, 128, 128, 1);
var hot$pink27 = color13(255, 105, 180, 1);
var pale$violet$red28 = color13(219, 112, 147, 1);
var light$pink29 = color13(255, 182, 193, 1);
var rosy$brown30 = color13(188, 143, 143, 1);
var pink31 = color13(255, 192, 203, 1);
var orchid32 = color13(218, 112, 214, 1);
var lavender$blush33 = color13(255, 240, 245, 1);
var snow34 = color13(255, 250, 250, 1);
var chocolate35 = color13(210, 105, 30, 1);
var saddle$brown36 = color13(139, 69, 19, 1);
var brown37 = color13(132, 60, 36, 1);
var dark$orange38 = color13(255, 140, 0, 1);
var coral39 = color13(255, 127, 80, 1);
var sienna40 = color13(160, 82, 45, 1);
var salmon41 = color13(250, 128, 114, 1);
var peru42 = color13(205, 133, 63, 1);
var dark$goldenrod43 = color13(184, 134, 11, 1);
var goldenrod44 = color13(218, 165, 32, 1);
var sandy$brown45 = color13(244, 164, 96, 1);
var light$salmon46 = color13(255, 160, 122, 1);
var dark$salmon47 = color13(233, 150, 122, 1);
var gold48 = color13(255, 215, 0, 1);
var yellow49 = color13(255, 255, 0, 1);
var olive50 = color13(128, 128, 0, 1);
var burlywood51 = color13(222, 184, 135, 1);
var tan52 = color13(210, 180, 140, 1);
var navajo$white53 = color13(255, 222, 173, 1);
var peach$puff54 = color13(255, 218, 185, 1);
var khaki55 = color13(240, 230, 140, 1);
var dark$khaki56 = color13(189, 183, 107, 1);
var moccasin57 = color13(255, 228, 181, 1);
var wheat58 = color13(245, 222, 179, 1);
var bisque59 = color13(255, 228, 196, 1);
var pale$goldenrod60 = color13(238, 232, 170, 1);
var blanched$almond61 = color13(255, 235, 205, 1);
var medium$goldenrod62 = color13(234, 234, 173, 1);
var papaya$whip63 = color13(255, 239, 213, 1);
var misty$rose64 = color13(255, 228, 225, 1);
var lemon$chiffon65 = color13(255, 250, 205, 1);
var antique$white66 = color13(250, 235, 215, 1);
var cornsilk67 = color13(255, 248, 220, 1);
var light$goldenrod$yellow68 = color13(250, 250, 210, 1);
var old$lace69 = color13(253, 245, 230, 1);
var linen70 = color13(250, 240, 230, 1);
var light$yellow71 = color13(255, 255, 224, 1);
var seashell72 = color13(255, 245, 238, 1);
var beige73 = color13(245, 245, 220, 1);
var floral$white74 = color13(255, 250, 240, 1);
var ivory75 = color13(255, 255, 240, 1);
var green76 = color13(0, 255, 0, 1);
var lawn$green77 = color13(124, 252, 0, 1);
var chartreuse78 = color13(127, 255, 0, 1);
var green$yellow79 = color13(173, 255, 47, 1);
var yellow$green80 = color13(154, 205, 50, 1);
var medium$forest$green81 = color13(107, 142, 35, 1);
var olive$drab82 = color13(107, 142, 35, 1);
var dark$olive$green83 = color13(85, 107, 47, 1);
var dark$sea$green84 = color13(143, 188, 139, 1);
var lime85 = color13(0, 255, 0, 1);
var dark$green86 = color13(0, 100, 0, 1);
var lime$green87 = color13(50, 205, 50, 1);
var forest$green88 = color13(34, 139, 34, 1);
var spring$green89 = color13(0, 255, 127, 1);
var medium$spring$green90 = color13(0, 250, 154, 1);
var sea$green91 = color13(46, 139, 87, 1);
var medium$sea$green92 = color13(60, 179, 113, 1);
var aquamarine93 = color13(112, 216, 144, 1);
var light$green94 = color13(144, 238, 144, 1);
var pale$green95 = color13(152, 251, 152, 1);
var medium$aquamarine96 = color13(102, 205, 170, 1);
var turquoise97 = color13(64, 224, 208, 1);
var light$sea$green98 = color13(32, 178, 170, 1);
var medium$turquoise99 = color13(72, 209, 204, 1);
var honeydew100 = color13(240, 255, 240, 1);
var mint$cream101 = color13(245, 255, 250, 1);
var royal$blue102 = color13(65, 105, 225, 1);
var dodger$blue103 = color13(30, 144, 255, 1);
var deep$sky$blue104 = color13(0, 191, 255, 1);
var cornflower$blue105 = color13(100, 149, 237, 1);
var steel$blue106 = color13(70, 130, 180, 1);
var light$sky$blue107 = color13(135, 206, 250, 1);
var dark$turquoise108 = color13(0, 206, 209, 1);
var cyan109 = color13(0, 255, 255, 1);
var aqua110 = color13(0, 255, 255, 1);
var dark$cyan111 = color13(0, 139, 139, 1);
var teal112 = color13(0, 128, 128, 1);
var sky$blue113 = color13(135, 206, 235, 1);
var cadet$blue114 = color13(95, 158, 160, 1);
var dark$slate$gray115 = color13(47, 79, 79, 1);
var light$slate$gray116 = color13(119, 136, 153, 1);
var slate$gray117 = color13(112, 128, 144, 1);
var light$steel$blue118 = color13(176, 196, 222, 1);
var light$blue119 = color13(173, 216, 230, 1);
var powder$blue120 = color13(176, 224, 230, 1);
var pale$turquoise121 = color13(175, 238, 238, 1);
var light$cyan122 = color13(224, 255, 255, 1);
var alice$blue123 = color13(240, 248, 255, 1);
var azure124 = color13(240, 255, 255, 1);
var medium$blue125 = color13(0, 0, 205, 1);
var dark$blue126 = color13(0, 0, 139, 1);
var midnight$blue127 = color13(25, 25, 112, 1);
var navy128 = color13(36, 36, 140, 1);
var blue129 = color13(0, 0, 255, 1);
var indigo130 = color13(75, 0, 130, 1);
var blue$violet131 = color13(138, 43, 226, 1);
var medium$slate$blue132 = color13(123, 104, 238, 1);
var slate$blue133 = color13(106, 90, 205, 1);
var purple134 = color13(160, 32, 240, 1);
var dark$slate$blue135 = color13(72, 61, 139, 1);
var dark$violet136 = color13(148, 0, 211, 1);
var dark$orchid137 = color13(153, 50, 204, 1);
var medium$purple138 = color13(147, 112, 219, 1);
var medium$orchid139 = color13(186, 85, 211, 1);
var magenta140 = color13(255, 0, 255, 1);
var fuchsia141 = color13(255, 0, 255, 1);
var dark$magenta142 = color13(139, 0, 139, 1);
var violet143 = color13(238, 130, 238, 1);
var plum144 = color13(221, 160, 221, 1);
var lavender145 = color13(230, 230, 250, 1);
var thistle146 = color13(216, 191, 216, 1);
var ghost$white147 = color13(248, 248, 255, 1);
var white148 = color13(255, 255, 255, 1);
var white$smoke149 = color13(245, 245, 245, 1);
var gainsboro150 = color13(220, 220, 220, 1);
var light$gray151 = color13(211, 211, 211, 1);
var silver152 = color13(192, 192, 192, 1);
var gray153 = color13(190, 190, 190, 1);
var dark$gray154 = color13(169, 169, 169, 1);
var dim$gray155 = color13(105, 105, 105, 1);
var black156 = color13(0, 0, 0, 1);
var transparent157 = color13(0, 0, 0, 0);
var $answer159 = _runtime["trace-value"](["dummy location"], nothing158);
return module["exports"] = {
    "gold": gold48,
    "is-Color": is$Color11,
    "is-color": is$color12,
    "misty-rose": misty$rose64,
    "pale-green": pale$green95,
    "royal-blue": royal$blue102,
    "olive-drab": olive$drab82,
    "turquoise": turquoise97,
    "seashell": seashell72,
    "peach-puff": peach$puff54,
    "tan": tan52,
    "lawn-green": lawn$green77,
    "khaki": khaki55,
    "orchid": orchid32,
    "snow": snow34,
    "violet-red": violet$red25,
    "gray": gray153,
    "green": green76,
    "cyan": cyan109,
    "spring-green": spring$green89,
    "color": color13,
    "cornflower-blue": cornflower$blue105,
    "beige": beige73,
    "sea-green": sea$green91,
    "dark-orange": dark$orange38,
    "dark-salmon": dark$salmon47,
    "dark-slate-blue": dark$slate$blue135,
    "light-salmon": light$salmon46,
    "pale-violet-red": pale$violet$red28,
    "medium-slate-blue": medium$slate$blue132,
    "sky-blue": sky$blue113,
    "medium-goldenrod": medium$goldenrod62,
    "deep-sky-blue": deep$sky$blue104,
    "lavender": lavender145,
    "dark-goldenrod": dark$goldenrod43,
    "wheat": wheat58,
    "alice-blue": alice$blue123,
    "dim-gray": dim$gray155,
    "white": white148,
    "lemon-chiffon": lemon$chiffon65,
    "magenta": magenta140,
    "ivory": ivory75,
    "medium-turquoise": medium$turquoise99,
    "tomato": tomato17,
    "yellow-green": yellow$green80,
    "dark-turquoise": dark$turquoise108,
    "fire-brick": fire$brick19,
    "medium-forest-green": medium$forest$green81,
    "light-yellow": light$yellow71,
    "dark-khaki": dark$khaki56,
    "white-smoke": white$smoke149,
    "lime-green": lime$green87,
    "ghost-white": ghost$white147,
    "rosy-brown": rosy$brown30,
    "dark-green": dark$green86,
    "dodger-blue": dodger$blue103,
    "light-green": light$green94,
    "silver": silver152,
    "medium-sea-green": medium$sea$green92,
    "light-pink": light$pink29,
    "light-sea-green": light$sea$green98,
    "slate-blue": slate$blue133,
    "dark-slate-gray": dark$slate$gray115,
    "orange": orange14,
    "dark-sea-green": dark$sea$green84,
    "salmon": salmon41,
    "powder-blue": powder$blue120,
    "light-slate-gray": light$slate$gray116,
    "thistle": thistle146,
    "azure": azure124,
    "floral-white": floral$white74,
    "maroon": maroon22,
    "saddle-brown": saddle$brown36,
    "coral": coral39,
    "dark-blue": dark$blue126,
    "red": red15,
    "light-blue": light$blue119,
    "cadet-blue": cadet$blue114,
    "orange-red": orange$red16,
    "blue-violet": blue$violet131,
    "dark-magenta": dark$magenta142,
    "transparent": transparent157,
    "indigo": indigo130,
    "sienna": sienna40,
    "medium-blue": medium$blue125,
    "light-steel-blue": light$steel$blue118,
    "forest-green": forest$green88,
    "yellow": yellow49,
    "medium-purple": medium$purple138,
    "plum": plum144,
    "blanched-almond": blanched$almond61,
    "bisque": bisque59,
    "lime": lime85,
    "medium-aquamarine": medium$aquamarine96,
    "brown": brown37,
    "chartreuse": chartreuse78,
    "pink": pink31,
    "dark-violet": dark$violet136,
    "hot-pink": hot$pink27,
    "navy": navy128,
    "midnight-blue": midnight$blue127,
    "deep-pink": deep$pink21,
    "slate-gray": slate$gray117,
    "fuchsia": fuchsia141,
    "mint-cream": mint$cream101,
    "peru": peru42,
    "burlywood": burlywood51,
    "moccasin": moccasin57,
    "blue": blue129,
    "dark-olive-green": dark$olive$green83,
    "light-gray": light$gray151,
    "light-coral": light$coral26,
    "light-goldenrod-yellow": light$goldenrod$yellow68,
    "lavender-blush": lavender$blush33,
    "light-cyan": light$cyan122,
    "dark-red": dark$red18,
    "dark-gray": dark$gray154,
    "dark-orchid": dark$orchid137,
    "linen": linen70,
    "dark-cyan": dark$cyan111,
    "antique-white": antique$white66,
    "pale-goldenrod": pale$goldenrod60,
    "papaya-whip": papaya$whip63,
    "olive": olive50,
    "medium-orchid": medium$orchid139,
    "medium-spring-green": medium$spring$green90,
    "honeydew": honeydew100,
    "medium-violet-red": medium$violet$red24,
    "indian-red": indian$red23,
    "chocolate": chocolate35,
    "teal": teal112,
    "aqua": aqua110,
    "steel-blue": steel$blue106,
    "purple": purple134,
    "light-sky-blue": light$sky$blue107,
    "old-lace": old$lace69,
    "cornsilk": cornsilk67,
    "goldenrod": goldenrod44,
    "pale-turquoise": pale$turquoise121,
    "sandy-brown": sandy$brown45,
    "gainsboro": gainsboro150,
    "green-yellow": green$yellow79,
    "aquamarine": aquamarine93,
    "violet": violet143,
    "crimson": crimson20,
    "navajo-white": navajo$white53,
    "black": black156,
    "$answer": $answer159,
    "$checks": _runtime["$checkResults"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr"),
    "$traces": _runtime["$getTraces"]("file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr"),
    "$locations": [
        {
            "name": "gold",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                48,
                0,
                1350,
                48,
                28,
                1378
            ]
        },
        {
            "name": "is-Color",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                6,
                0,
                56,
                12,
                3,
                171
            ]
        },
        {
            "name": "is-color",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                7,
                2,
                70,
                11,
                22,
                167
            ]
        },
        {
            "name": "misty-rose",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                64,
                0,
                1915,
                64,
                36,
                1951
            ]
        },
        {
            "name": "pale-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                95,
                0,
                3061,
                95,
                36,
                3097
            ]
        },
        {
            "name": "royal-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                102,
                0,
                3332,
                102,
                35,
                3367
            ]
        },
        {
            "name": "olive-drab",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                82,
                0,
                2579,
                82,
                35,
                2614
            ]
        },
        {
            "name": "turquoise",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                97,
                0,
                3142,
                97,
                34,
                3176
            ]
        },
        {
            "name": "seashell",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                72,
                0,
                2222,
                72,
                34,
                2256
            ]
        },
        {
            "name": "peach-puff",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                54,
                0,
                1545,
                54,
                36,
                1581
            ]
        },
        {
            "name": "tan",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                52,
                0,
                1476,
                52,
                29,
                1505
            ]
        },
        {
            "name": "lawn-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                77,
                0,
                2388,
                77,
                34,
                2422
            ]
        },
        {
            "name": "khaki",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                55,
                0,
                1582,
                55,
                31,
                1613
            ]
        },
        {
            "name": "orchid",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                32,
                0,
                793,
                32,
                32,
                825
            ]
        },
        {
            "name": "snow",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                34,
                0,
                867,
                34,
                30,
                897
            ]
        },
        {
            "name": "violet-red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                25,
                0,
                537,
                25,
                35,
                572
            ]
        },
        {
            "name": "gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                153,
                0,
                5156,
                153,
                30,
                5186
            ]
        },
        {
            "name": "green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                76,
                0,
                2360,
                76,
                27,
                2387
            ]
        },
        {
            "name": "cyan",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                109,
                0,
                3601,
                109,
                28,
                3629
            ]
        },
        {
            "name": "spring-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                89,
                0,
                2829,
                89,
                36,
                2865
            ]
        },
        {
            "name": "color",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                7,
                2,
                70,
                11,
                22,
                167
            ]
        },
        {
            "name": "cornflower-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                105,
                0,
                3443,
                105,
                41,
                3484
            ]
        },
        {
            "name": "beige",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                73,
                0,
                2257,
                73,
                31,
                2288
            ]
        },
        {
            "name": "sea-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                91,
                0,
                2910,
                91,
                33,
                2943
            ]
        },
        {
            "name": "dark-orange",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                38,
                0,
                1000,
                38,
                35,
                1035
            ]
        },
        {
            "name": "dark-salmon",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                47,
                0,
                1312,
                47,
                37,
                1349
            ]
        },
        {
            "name": "dark-slate-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                135,
                0,
                4516,
                135,
                39,
                4555
            ]
        },
        {
            "name": "light-salmon",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                46,
                0,
                1273,
                46,
                38,
                1311
            ]
        },
        {
            "name": "pale-violet-red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                28,
                0,
                646,
                28,
                41,
                687
            ]
        },
        {
            "name": "medium-slate-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                132,
                0,
                4404,
                132,
                43,
                4447
            ]
        },
        {
            "name": "sky-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                113,
                0,
                3722,
                113,
                34,
                3756
            ]
        },
        {
            "name": "medium-goldenrod",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                62,
                0,
                1834,
                62,
                42,
                1876
            ]
        },
        {
            "name": "deep-sky-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                104,
                0,
                3405,
                104,
                37,
                3442
            ]
        },
        {
            "name": "lavender",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                145,
                0,
                4873,
                145,
                34,
                4907
            ]
        },
        {
            "name": "dark-goldenrod",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                43,
                0,
                1161,
                43,
                39,
                1200
            ]
        },
        {
            "name": "wheat",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                58,
                0,
                1686,
                58,
                31,
                1717
            ]
        },
        {
            "name": "alice-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                123,
                0,
                4108,
                123,
                36,
                4144
            ]
        },
        {
            "name": "dim-gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                155,
                0,
                5223,
                155,
                34,
                5257
            ]
        },
        {
            "name": "white",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                148,
                0,
                4980,
                148,
                31,
                5011
            ]
        },
        {
            "name": "lemon-chiffon",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                65,
                0,
                1952,
                65,
                39,
                1991
            ]
        },
        {
            "name": "magenta",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                140,
                0,
                4708,
                140,
                31,
                4739
            ]
        },
        {
            "name": "ivory",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                75,
                0,
                2328,
                75,
                31,
                2359
            ]
        },
        {
            "name": "medium-turquoise",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                99,
                0,
                3218,
                99,
                41,
                3259
            ]
        },
        {
            "name": "tomato",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                17,
                0,
                264,
                17,
                30,
                294
            ]
        },
        {
            "name": "yellow-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                80,
                0,
                2496,
                80,
                37,
                2533
            ]
        },
        {
            "name": "dark-turquoise",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                108,
                0,
                3562,
                108,
                38,
                3600
            ]
        },
        {
            "name": "fire-brick",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                19,
                0,
                326,
                19,
                34,
                360
            ]
        },
        {
            "name": "medium-forest-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                81,
                0,
                2534,
                81,
                44,
                2578
            ]
        },
        {
            "name": "light-yellow",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                71,
                0,
                2183,
                71,
                38,
                2221
            ]
        },
        {
            "name": "dark-khaki",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                56,
                0,
                1614,
                56,
                36,
                1650
            ]
        },
        {
            "name": "white-smoke",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                149,
                0,
                5012,
                149,
                37,
                5049
            ]
        },
        {
            "name": "lime-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                87,
                0,
                2757,
                87,
                34,
                2791
            ]
        },
        {
            "name": "ghost-white",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                147,
                0,
                4942,
                147,
                37,
                4979
            ]
        },
        {
            "name": "rosy-brown",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                30,
                0,
                725,
                30,
                36,
                761
            ]
        },
        {
            "name": "dark-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                86,
                0,
                2724,
                86,
                32,
                2756
            ]
        },
        {
            "name": "dodger-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                103,
                0,
                3368,
                103,
                36,
                3404
            ]
        },
        {
            "name": "light-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                94,
                0,
                3023,
                94,
                37,
                3060
            ]
        },
        {
            "name": "silver",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                152,
                0,
                5123,
                152,
                32,
                5155
            ]
        },
        {
            "name": "medium-sea-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                92,
                0,
                2944,
                92,
                41,
                2985
            ]
        },
        {
            "name": "light-pink",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                29,
                0,
                688,
                29,
                36,
                724
            ]
        },
        {
            "name": "light-sea-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                98,
                0,
                3177,
                98,
                40,
                3217
            ]
        },
        {
            "name": "slate-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                133,
                0,
                4448,
                133,
                35,
                4483
            ]
        },
        {
            "name": "dark-slate-gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                115,
                0,
                3793,
                115,
                38,
                3831
            ]
        },
        {
            "name": "orange",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                14,
                0,
                173,
                14,
                30,
                203
            ]
        },
        {
            "name": "dark-sea-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                84,
                0,
                2656,
                84,
                40,
                2696
            ]
        },
        {
            "name": "salmon",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                41,
                0,
                1098,
                41,
                32,
                1130
            ]
        },
        {
            "name": "powder-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                120,
                0,
                3992,
                120,
                37,
                4029
            ]
        },
        {
            "name": "light-slate-gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                116,
                0,
                3832,
                116,
                42,
                3874
            ]
        },
        {
            "name": "thistle",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                146,
                0,
                4908,
                146,
                33,
                4941
            ]
        },
        {
            "name": "azure",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                124,
                0,
                4145,
                124,
                31,
                4176
            ]
        },
        {
            "name": "floral-white",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                74,
                0,
                2289,
                74,
                38,
                2327
            ]
        },
        {
            "name": "maroon",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                22,
                0,
                428,
                22,
                30,
                458
            ]
        },
        {
            "name": "saddle-brown",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                36,
                0,
                933,
                36,
                36,
                969
            ]
        },
        {
            "name": "coral",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                39,
                0,
                1036,
                39,
                30,
                1066
            ]
        },
        {
            "name": "dark-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                126,
                0,
                4211,
                126,
                31,
                4242
            ]
        },
        {
            "name": "red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                15,
                0,
                204,
                15,
                25,
                229
            ]
        },
        {
            "name": "light-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                119,
                0,
                3955,
                119,
                36,
                3991
            ]
        },
        {
            "name": "cadet-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                114,
                0,
                3757,
                114,
                35,
                3792
            ]
        },
        {
            "name": "orange-red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                16,
                0,
                230,
                16,
                33,
                263
            ]
        },
        {
            "name": "blue-violet",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                131,
                0,
                4367,
                131,
                36,
                4403
            ]
        },
        {
            "name": "dark-magenta",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                142,
                0,
                4772,
                142,
                36,
                4808
            ]
        },
        {
            "name": "transparent",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                157,
                0,
                5284,
                157,
                31,
                5315
            ]
        },
        {
            "name": "indigo",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                130,
                0,
                4337,
                130,
                29,
                4366
            ]
        },
        {
            "name": "sienna",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                40,
                0,
                1067,
                40,
                30,
                1097
            ]
        },
        {
            "name": "medium-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                125,
                0,
                4177,
                125,
                33,
                4210
            ]
        },
        {
            "name": "light-steel-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                118,
                0,
                3912,
                118,
                42,
                3954
            ]
        },
        {
            "name": "forest-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                88,
                0,
                2792,
                88,
                36,
                2828
            ]
        },
        {
            "name": "yellow",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                49,
                0,
                1379,
                49,
                30,
                1409
            ]
        },
        {
            "name": "medium-purple",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                138,
                0,
                4629,
                138,
                39,
                4668
            ]
        },
        {
            "name": "plum",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                144,
                0,
                4842,
                144,
                30,
                4872
            ]
        },
        {
            "name": "blanched-almond",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                61,
                0,
                1792,
                61,
                41,
                1833
            ]
        },
        {
            "name": "bisque",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                59,
                0,
                1718,
                59,
                32,
                1750
            ]
        },
        {
            "name": "lime",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                85,
                0,
                2697,
                85,
                26,
                2723
            ]
        },
        {
            "name": "medium-aquamarine",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                96,
                0,
                3098,
                96,
                43,
                3141
            ]
        },
        {
            "name": "brown",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                37,
                0,
                970,
                37,
                29,
                999
            ]
        },
        {
            "name": "chartreuse",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                78,
                0,
                2423,
                78,
                34,
                2457
            ]
        },
        {
            "name": "pink",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                31,
                0,
                762,
                31,
                30,
                792
            ]
        },
        {
            "name": "dark-violet",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                136,
                0,
                4556,
                136,
                35,
                4591
            ]
        },
        {
            "name": "hot-pink",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                27,
                0,
                611,
                27,
                34,
                645
            ]
        },
        {
            "name": "navy",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                128,
                0,
                4281,
                128,
                28,
                4309
            ]
        },
        {
            "name": "midnight-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                127,
                0,
                4243,
                127,
                37,
                4280
            ]
        },
        {
            "name": "deep-pink",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                21,
                0,
                393,
                21,
                34,
                427
            ]
        },
        {
            "name": "slate-gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                117,
                0,
                3875,
                117,
                36,
                3911
            ]
        },
        {
            "name": "fuchsia",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                141,
                0,
                4740,
                141,
                31,
                4771
            ]
        },
        {
            "name": "mint-cream",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                101,
                0,
                3295,
                101,
                36,
                3331
            ]
        },
        {
            "name": "peru",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                42,
                0,
                1131,
                42,
                29,
                1160
            ]
        },
        {
            "name": "burlywood",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                51,
                0,
                1440,
                51,
                35,
                1475
            ]
        },
        {
            "name": "moccasin",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                57,
                0,
                1651,
                57,
                34,
                1685
            ]
        },
        {
            "name": "blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                129,
                0,
                4310,
                129,
                26,
                4336
            ]
        },
        {
            "name": "dark-olive-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                83,
                0,
                2615,
                83,
                40,
                2655
            ]
        },
        {
            "name": "light-gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                151,
                0,
                5086,
                151,
                36,
                5122
            ]
        },
        {
            "name": "light-coral",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                26,
                0,
                573,
                26,
                37,
                610
            ]
        },
        {
            "name": "light-goldenrod-yellow",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                68,
                0,
                2067,
                68,
                48,
                2115
            ]
        },
        {
            "name": "lavender-blush",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                33,
                0,
                826,
                33,
                40,
                866
            ]
        },
        {
            "name": "light-cyan",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                122,
                0,
                4071,
                122,
                36,
                4107
            ]
        },
        {
            "name": "dark-red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                18,
                0,
                295,
                18,
                30,
                325
            ]
        },
        {
            "name": "dark-gray",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                154,
                0,
                5187,
                154,
                35,
                5222
            ]
        },
        {
            "name": "dark-orchid",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                137,
                0,
                4592,
                137,
                36,
                4628
            ]
        },
        {
            "name": "linen",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                70,
                0,
                2151,
                70,
                31,
                2182
            ]
        },
        {
            "name": "dark-cyan",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                111,
                0,
                3659,
                111,
                33,
                3692
            ]
        },
        {
            "name": "antique-white",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                66,
                0,
                1992,
                66,
                39,
                2031
            ]
        },
        {
            "name": "pale-goldenrod",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                60,
                0,
                1751,
                60,
                40,
                1791
            ]
        },
        {
            "name": "papaya-whip",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                63,
                0,
                1877,
                63,
                37,
                1914
            ]
        },
        {
            "name": "olive",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                50,
                0,
                1410,
                50,
                29,
                1439
            ]
        },
        {
            "name": "medium-orchid",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                139,
                0,
                4669,
                139,
                38,
                4707
            ]
        },
        {
            "name": "medium-spring-green",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                90,
                0,
                2866,
                90,
                43,
                2909
            ]
        },
        {
            "name": "honeydew",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                100,
                0,
                3260,
                100,
                34,
                3294
            ]
        },
        {
            "name": "medium-violet-red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                24,
                0,
                494,
                24,
                42,
                536
            ]
        },
        {
            "name": "indian-red",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                23,
                0,
                459,
                23,
                34,
                493
            ]
        },
        {
            "name": "chocolate",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                35,
                0,
                898,
                35,
                34,
                932
            ]
        },
        {
            "name": "teal",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                112,
                0,
                3693,
                112,
                28,
                3721
            ]
        },
        {
            "name": "aqua",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                110,
                0,
                3630,
                110,
                28,
                3658
            ]
        },
        {
            "name": "steel-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                106,
                0,
                3485,
                106,
                35,
                3520
            ]
        },
        {
            "name": "purple",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                134,
                0,
                4484,
                134,
                31,
                4515
            ]
        },
        {
            "name": "light-sky-blue",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                107,
                0,
                3521,
                107,
                40,
                3561
            ]
        },
        {
            "name": "old-lace",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                69,
                0,
                2116,
                69,
                34,
                2150
            ]
        },
        {
            "name": "cornsilk",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                67,
                0,
                2032,
                67,
                34,
                2066
            ]
        },
        {
            "name": "goldenrod",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                44,
                0,
                1201,
                44,
                34,
                1235
            ]
        },
        {
            "name": "pale-turquoise",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                121,
                0,
                4030,
                121,
                40,
                4070
            ]
        },
        {
            "name": "sandy-brown",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                45,
                0,
                1236,
                45,
                36,
                1272
            ]
        },
        {
            "name": "gainsboro",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                150,
                0,
                5050,
                150,
                35,
                5085
            ]
        },
        {
            "name": "green-yellow",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                79,
                0,
                2458,
                79,
                37,
                2495
            ]
        },
        {
            "name": "aquamarine",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                93,
                0,
                2986,
                93,
                36,
                3022
            ]
        },
        {
            "name": "violet",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                143,
                0,
                4809,
                143,
                32,
                4841
            ]
        },
        {
            "name": "crimson",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                20,
                0,
                361,
                20,
                31,
                392
            ]
        },
        {
            "name": "navajo-white",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                53,
                0,
                1506,
                53,
                38,
                1544
            ]
        },
        {
            "name": "black",
            "srcloc": [
                "file:\/\/\/Users\/elijah\/Desktop\/Pyret\/pyret-lang\/src\/runtime-arr-stage-1\/color.arr",
                156,
                0,
                5258,
                156,
                25,
                5283
            ]
        }
    ]
};