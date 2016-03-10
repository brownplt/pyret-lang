({
"requires":[{"import-type":"builtin",
"name":"error-display"}],
"provides":{"values":{},
"aliases":{},
"datatypes":{}},
"theModule":
function(R,NAMESPACE, M, $ED15) {
var G = R.getFieldLoc;
var U = function(loc,name) {
R.ffi.throwUninitializedIdMkLoc(loc,name)};
var D = R.undefined;
var L = [[M,9,39,162,9,48,171],
[M,9,2,125,9,49,172],
[M,8,0,96,10,3,176],
[M,12,36,241,12,43,248],
[M,12,45,250,12,58,263],
[M,12,25,230,12,63,268],
[M,12,16,221,12,64,269],
[M,12,2,207,12,65,270],
[M,11,0,177,13,3,274],
[M,19,7,375,19,15,383],
[M,19,18,386,19,25,393],
[M,19,35,403,19,47,415],
[M,19,27,395,19,48,416],
[M,18,4,347,20,7,426],
[M,23,7,496,23,15,504],
[M,24,9,515,24,16,522],
[M,25,10,534,25,67,591],
[M,26,29,622,26,37,630],
[M,26,40,633,26,60,653],
[M,27,17,673,27,25,681],
[M,27,8,664,27,26,682],
[M,26,10,603,26,38,631],
[M,22,4,468,28,7,691],
[M,31,7,781,31,15,789],
[M,32,9,800,32,16,807],
[M,33,10,819,33,47,856],
[M,33,65,874,33,80,889],
[M,33,57,866,33,81,890],
[M,33,49,858,33,82,891],
[M,34,10,903,34,34,927],
[M,34,55,948,34,63,956],
[M,34,36,929,34,64,957],
[M,30,4,753,35,7,967],
[M,38,7,1045,38,15,1053],
[M,39,9,1064,39,16,1071],
[M,39,18,1073,39,44,1099],
[M,39,54,1109,39,66,1121],
[M,39,46,1101,39,67,1122],
[M,40,9,1134,40,16,1141],
[M,40,18,1143,40,48,1173],
[M,41,25,1201,41,39,1215],
[M,41,8,1184,41,40,1216],
[M,37,4,1017,42,7,1225],
[M,45,7,1311,45,15,1319],
[M,46,9,1330,46,16,1337],
[M,47,10,1349,47,26,1365],
[M,47,44,1383,47,54,1393],
[M,47,36,1375,47,55,1394],
[M,47,28,1367,47,56,1395],
[M,47,58,1397,47,106,1445],
[M,48,29,1476,48,37,1484],
[M,49,9,1497,49,16,1504],
[M,49,18,1506,49,44,1532],
[M,50,17,1552,50,25,1560],
[M,50,8,1543,50,26,1561],
[M,48,10,1457,48,38,1485],
[M,44,4,1283,51,7,1570],
[M,54,7,1662,54,15,1670],
[M,55,9,1681,55,16,1688],
[M,56,10,1700,56,43,1733],
[M,56,61,1751,56,71,1761],
[M,56,53,1743,56,72,1762],
[M,56,45,1735,56,73,1763],
[M,57,10,1775,57,64,1829],
[M,58,29,1860,58,37,1868],
[M,59,9,1881,59,16,1888],
[M,59,18,1890,59,48,1920],
[M,60,17,1940,60,29,1952],
[M,60,8,1931,60,30,1953],
[M,58,10,1841,58,38,1869],
[M,53,4,1634,61,7,1962],
[M,64,7,2037,64,15,2045],
[M,65,9,2056,65,16,2063],
[M,66,10,2075,66,70,2135],
[M,67,29,2166,67,37,2174],
[M,68,9,2187,68,16,2194],
[M,68,18,2196,68,48,2226],
[M,69,17,2246,69,29,2258],
[M,69,8,2237,69,30,2259],
[M,67,10,2147,67,38,2175],
[M,63,4,2009,70,7,2268],
[M,73,7,2350,73,15,2358],
[M,74,9,2369,74,16,2376],
[M,75,10,2388,75,29,2407],
[M,75,39,2417,75,54,2432],
[M,75,31,2409,75,55,2433],
[M,75,57,2435,75,70,2448],
[M,75,80,2458,75,96,2474],
[M,75,72,2450,75,97,2475],
[M,76,10,2487,76,40,2517],
[M,76,50,2527,76,58,2535],
[M,76,42,2519,76,59,2536],
[M,76,61,2538,76,85,2562],
[M,77,29,2593,77,37,2601],
[M,77,40,2604,77,60,2624],
[M,78,17,2644,78,27,2654],
[M,78,8,2635,78,28,2655],
[M,77,10,2574,77,38,2602],
[M,72,4,2322,79,7,2664],
[M,82,7,2749,82,15,2757],
[M,83,9,2768,83,16,2775],
[M,84,10,2787,84,29,2806],
[M,84,39,2816,84,54,2831],
[M,84,31,2808,84,55,2832],
[M,84,57,2834,84,70,2847],
[M,84,80,2857,84,96,2873],
[M,84,72,2849,84,97,2874],
[M,85,10,2886,85,28,2904],
[M,85,38,2914,85,51,2927],
[M,85,30,2906,85,52,2928],
[M,85,54,2930,85,80,2956],
[M,86,18,2976,86,26,2984],
[M,86,10,2968,86,27,2985],
[M,86,29,2987,86,53,3011],
[M,87,29,3042,87,37,3050],
[M,87,40,3053,87,60,3073],
[M,88,17,3093,88,27,3103],
[M,88,8,3084,88,28,3104],
[M,87,10,3023,87,38,3051],
[M,81,4,2721,89,7,3113],
[M,94,11,3254,94,19,3262],
[M,95,13,3277,95,20,3284],
[M,96,14,3300,96,42,3328],
[M,96,53,3339,96,61,3347],
[M,96,44,3330,96,62,3348],
[M,96,64,3350,96,112,3398],
[M,97,21,3422,97,29,3430],
[M,97,12,3413,97,30,3431],
[M,98,13,3446,98,20,3453],
[M,98,22,3455,98,35,3468],
[M,98,37,3470,98,60,3493],
[M,93,8,3233,99,11,3507],
[M,100,9,3518,100,17,3526],
[M,101,11,3539,101,26,3554],
[M,101,28,3556,101,48,3576],
[M,101,59,3587,101,67,3595],
[M,101,50,3578,101,68,3596],
[M,101,70,3598,101,91,3619],
[M,101,102,3630,101,110,3638],
[M,101,93,3621,101,111,3639],
[M,92,6,3197,101,114,3642],
[M,91,4,3170,102,7,3650],
[M,105,7,3731,105,15,3739],
[M,106,9,3750,106,24,3765],
[M,106,26,3767,106,63,3804],
[M,106,74,3815,106,82,3823],
[M,106,65,3806,106,83,3824],
[M,107,10,3836,107,26,3852],
[M,107,37,3863,107,46,3872],
[M,107,28,3854,107,47,3873],
[M,108,10,3885,108,30,3905],
[M,108,41,3916,108,49,3924],
[M,108,32,3907,108,50,3925],
[M,104,4,3703,109,7,3935],
[M,112,7,4041,112,15,4049],
[M,113,9,4060,113,16,4067],
[M,114,10,4079,114,35,4104],
[M,114,53,4122,114,64,4133],
[M,114,45,4114,114,65,4134],
[M,114,37,4106,114,66,4135],
[M,114,68,4137,114,96,4165],
[M,115,9,4177,115,16,4184],
[M,115,27,4195,115,36,4204],
[M,115,18,4186,115,37,4205],
[M,116,9,4217,116,16,4224],
[M,116,27,4235,116,36,4244],
[M,116,18,4226,116,37,4245],
[M,117,16,4264,117,27,4275],
[M,117,8,4256,117,43,4291],
[M,118,9,4302,118,20,4313],
[M,119,10,4325,119,33,4348],
[M,120,10,4360,120,36,4386],
[M,121,11,4399,121,18,4406],
[M,122,12,4420,122,53,4461],
[M,122,71,4479,122,86,4494],
[M,122,63,4471,122,87,4495],
[M,122,55,4463,122,88,4496],
[M,122,90,4498,122,107,4515],
[M,117,16,4264,117,42,4290],
[M,111,4,4013,123,7,4526],
[M,126,7,4621,126,15,4629],
[M,127,9,4640,127,16,4647],
[M,128,10,4659,128,35,4684],
[M,128,53,4702,128,64,4713],
[M,128,45,4694,128,65,4714],
[M,128,37,4686,128,66,4715],
[M,128,68,4717,128,96,4745],
[M,129,9,4757,129,16,4764],
[M,129,27,4775,129,36,4784],
[M,129,18,4766,129,37,4785],
[M,130,9,4797,130,16,4804],
[M,130,27,4815,130,36,4824],
[M,130,18,4806,130,37,4825],
[M,131,8,4836,131,26,4854],
[M,132,9,4865,132,20,4876],
[M,133,10,4888,133,55,4933],
[M,134,11,4946,134,18,4953],
[M,135,12,4967,135,51,5006],
[M,135,69,5024,135,84,5039],
[M,135,61,5016,135,85,5040],
[M,135,53,5008,135,86,5041],
[M,135,88,5043,135,105,5060],
[M,125,4,4593,136,7,5071],
[M,139,7,5171,139,15,5179],
[M,140,11,5192,140,24,5205],
[M,140,27,5208,140,44,5225],
[M,140,11,5192,140,44,5225],
[M,141,11,5238,141,18,5245],
[M,142,12,5259,142,42,5289],
[M,142,63,5310,142,78,5325],
[M,143,12,5340,143,35,5363],
[M,143,46,5374,143,59,5387],
[M,143,37,5365,143,60,5388],
[M,144,15,5405,144,28,5418],
[M,144,15,5405,144,33,5423],
[M,144,35,5425,144,55,5445],
[M,144,62,5452,144,83,5473],
[M,145,12,5491,145,47,5526],
[M,145,58,5537,145,75,5554],
[M,145,49,5528,145,76,5555],
[M,146,15,5572,146,32,5589],
[M,146,15,5572,146,37,5594],
[M,146,39,5596,146,55,5612],
[M,146,62,5619,146,79,5636],
[M,142,44,5291,142,79,5326],
[M,148,11,5667,148,18,5674],
[M,149,12,5688,149,42,5718],
[M,149,63,5739,149,78,5754],
[M,150,12,5769,150,30,5787],
[M,150,41,5798,150,54,5811],
[M,150,32,5789,150,55,5812],
[M,151,15,5829,151,28,5842],
[M,151,15,5829,151,33,5847],
[M,151,35,5849,151,55,5869],
[M,151,62,5876,151,83,5897],
[M,152,12,5915,152,52,5955],
[M,152,63,5966,152,80,5983],
[M,152,54,5957,152,81,5984],
[M,153,15,6001,153,32,6018],
[M,153,15,6001,153,37,6023],
[M,153,39,6025,153,55,6041],
[M,153,62,6048,153,79,6065],
[M,149,44,5720,149,79,5755],
[M,138,4,5143,155,7,6091],
[M,158,9,6205,158,33,6229],
[M,159,9,6240,159,17,6248],
[M,160,11,6261,160,18,6268],
[M,161,12,6282,161,42,6312],
[M,161,63,6333,161,78,6348],
[M,162,12,6363,162,76,6427],
[M,161,44,6314,161,79,6349],
[M,164,9,6451,164,17,6459],
[M,165,11,6472,165,18,6479],
[M,166,12,6493,166,42,6523],
[M,166,63,6544,166,78,6559],
[M,167,12,6574,167,89,6651],
[M,166,44,6525,166,79,6560],
[M,157,4,6175,169,7,6671],
[M,172,17,6770,172,26,6779],
[M,172,17,6770,172,35,6788],
[M,173,20,6809,173,33,6822],
[M,174,19,6868,174,32,6881],
[M,175,23,6941,175,42,6960],
[M,175,23,6941,175,47,6965],
[M,178,13,7072,178,25,7084],
[M,178,13,7072,178,38,7097],
[M,179,13,7112,179,21,7120],
[M,180,15,7137,180,22,7144],
[M,180,24,7146,180,50,7172],
[M,180,61,7183,180,80,7202],
[M,180,52,7174,180,81,7203],
[M,180,83,7205,180,111,7233],
[M,182,15,7297,182,22,7304],
[M,182,24,7306,182,54,7336],
[M,182,56,7338,182,74,7356],
[M,182,76,7358,182,92,7374],
[M,183,31,7408,183,40,7417],
[M,183,14,7391,183,41,7418],
[M,182,32,7314,182,53,7335],
[M,181,14,7250,181,44,7280],
[M,180,91,7213,180,110,7232],
[M,185,13,7449,185,21,7457],
[M,186,15,7474,186,22,7481],
[M,186,24,7483,186,50,7509],
[M,186,61,7520,186,80,7539],
[M,186,52,7511,186,81,7540],
[M,187,16,7558,187,70,7612],
[M,188,33,7648,188,45,7660],
[M,189,15,7678,189,22,7685],
[M,189,24,7687,189,39,7702],
[M,191,15,7766,191,22,7773],
[M,191,24,7775,191,54,7805],
[M,191,56,7807,191,74,7825],
[M,191,76,7827,191,92,7843],
[M,192,31,7877,192,40,7886],
[M,192,14,7860,192,41,7887],
[M,191,32,7783,191,53,7804],
[M,190,14,7719,190,44,7749],
[M,188,14,7629,188,46,7661],
[M,187,24,7566,187,69,7611],
[M,177,8,7042,194,11,7914],
[M,195,9,7925,195,17,7933],
[M,196,11,7946,196,18,7953],
[M,196,20,7955,196,46,7981],
[M,196,57,7992,196,76,8011],
[M,196,48,7983,196,77,8012],
[M,196,79,8014,196,107,8042],
[M,197,29,8074,197,41,8086],
[M,198,11,8100,198,18,8107],
[M,198,20,8109,198,50,8139],
[M,198,52,8141,198,70,8159],
[M,198,72,8161,198,88,8177],
[M,199,27,8207,199,36,8216],
[M,176,6,7006,199,39,8219],
[M,199,10,8190,199,37,8217],
[M,198,28,8117,198,49,8138],
[M,197,10,8055,197,42,8087],
[M,196,87,8022,196,106,8041],
[M,171,4,6732,200,7,8227],
[M,203,7,8305,203,15,8313],
[M,204,9,8324,204,16,8331],
[M,205,10,8343,205,73,8406],
[M,206,29,8437,206,37,8445],
[M,206,40,8448,206,60,8468],
[M,207,17,8488,207,33,8504],
[M,207,8,8479,207,34,8505],
[M,206,10,8418,206,38,8446],
[M,202,4,8277,208,7,8514],
[M,211,7,8642,211,15,8650],
[M,211,17,8652,211,40,8675],
[M,211,25,8660,211,39,8674],
[M,210,4,8614,212,7,8684],
[M,215,7,8765,215,15,8773],
[M,216,9,8784,216,16,8791],
[M,216,18,8793,216,37,8812],
[M,216,55,8830,216,64,8839],
[M,216,47,8822,216,65,8840],
[M,216,39,8814,216,66,8841],
[M,216,68,8843,216,90,8865],
[M,217,29,8896,217,37,8904],
[M,217,40,8907,217,73,8940],
[M,217,10,8877,217,38,8905],
[M,214,4,8737,218,7,8950],
[M,221,7,9044,221,15,9052],
[M,222,9,9063,222,16,9070],
[M,223,13,9085,223,23,9095],
[M,223,13,9085,223,32,9104],
[M,223,13,9085,223,37,9109],
[M,223,39,9111,223,86,9158],
[M,224,39,9198,224,87,9246],
[M,226,22,9285,226,32,9295],
[M,226,37,9300,226,44,9307],
[M,226,22,9285,226,45,9308],
[M,226,8,9271,226,52,9315],
[M,220,4,9016,227,7,9324],
[M,232,11,9525,232,19,9533],
[M,233,13,9548,233,20,9555],
[M,233,22,9557,233,52,9587],
[M,233,71,9606,233,81,9616],
[M,233,62,9597,233,82,9617],
[M,233,54,9589,233,83,9618],
[M,234,14,9634,234,52,9672],
[M,235,14,9713,235,33,9732],
[M,235,43,9742,235,54,9753],
[M,235,35,9734,235,55,9754],
[M,234,54,9674,234,77,9697],
[M,231,8,9504,236,11,9768],
[M,237,9,9779,237,17,9787],
[M,238,11,9800,238,18,9807],
[M,238,20,9809,238,50,9839],
[M,238,69,9858,238,79,9868],
[M,238,60,9849,238,80,9869],
[M,238,52,9841,238,81,9870],
[M,239,12,9884,239,31,9903],
[M,239,41,9913,239,52,9924],
[M,239,33,9905,239,53,9925],
[M,230,6,9468,239,56,9928],
[M,229,4,9441,240,7,9936],
[M,243,15,10038,243,26,10049],
[M,244,15,10065,244,26,10076],
[M,246,10,10098,246,49,10137],
[M,246,32,10120,246,49,10137],
[M,246,8,10096,263,13,11033],
[M,249,13,10247,249,21,10255],
[M,250,15,10272,250,22,10279],
[M,250,24,10281,250,40,10297],
[M,251,15,10315,251,22,10322],
[M,251,24,10324,251,40,10340],
[M,252,15,10358,252,22,10365],
[M,252,24,10367,252,40,10383],
[M,253,15,10401,253,22,10408],
[M,253,24,10410,253,54,10440],
[M,254,22,10464,254,39,10481],
[M,254,14,10456,254,40,10482],
[M,254,42,10484,254,87,10529],
[M,248,10,10207,256,13,10546],
[M,257,13,10560,257,64,10611],
[M,257,41,10588,257,64,10611],
[M,258,12,10625,258,98,10711],
[M,259,10,10722,260,110,10865],
[M,260,12,10767,260,110,10865],
[M,261,10,10876,262,110,11019],
[M,262,12,10921,262,110,11019],
[M,257,10,10557,263,13,11033],
[M,261,18,10884,261,41,10907],
[M,259,18,10730,259,41,10753],
[M,257,13,10560,257,36,10583],
[M,265,11,11066,265,19,11074],
[M,266,13,11089,266,20,11096],
[M,266,22,11098,266,79,11155],
[M,267,13,11171,267,20,11178],
[M,267,31,11189,267,42,11200],
[M,267,22,11180,267,43,11201],
[M,268,13,11217,268,20,11224],
[M,268,31,11235,268,42,11246],
[M,268,22,11226,268,43,11247],
[M,246,10,10098,246,27,10115],
[M,242,4,10002,270,7,11266],
[M,274,7,11321,274,15,11329],
[M,274,17,11331,274,51,11365],
[M,273,4,11293,275,7,11374],
[M,278,34,11449,278,44,11459],
[M,278,25,11440,278,45,11460],
[M,278,4,11419,278,49,11464],
[M,17,2,298,20,7,426],
[M,17,33,329,17,39,335],
[M,17,4,300,17,40,336],
[M,21,2,429,28,7,691],
[M,21,4,431,21,30,457],
[M,29,2,694,35,7,967],
[M,29,43,735,29,49,741],
[M,29,4,696,29,50,742],
[M,36,2,970,42,7,1225],
[M,36,4,972,36,38,1006],
[M,43,2,1228,51,7,1570],
[M,43,39,1265,43,45,1271],
[M,43,4,1230,43,46,1272],
[M,52,2,1573,61,7,1962],
[M,52,45,1616,52,51,1622],
[M,52,4,1575,52,52,1623],
[M,62,2,1965,70,7,2268],
[M,62,4,1967,62,35,1998],
[M,71,2,2271,79,7,2664],
[M,71,4,2273,71,42,2311],
[M,80,2,2667,89,7,3113],
[M,80,4,2669,80,45,2710],
[M,90,2,3116,102,7,3650],
[M,90,38,3152,90,44,3158],
[M,90,4,3118,90,45,3159],
[M,103,2,3653,109,7,3935],
[M,103,4,3655,103,41,3692],
[M,110,2,3938,123,7,4526],
[M,110,4,3940,110,66,4002],
[M,124,2,4529,136,7,5071],
[M,124,4,4531,124,55,4582],
[M,137,2,5074,155,7,6091],
[M,137,4,5076,137,60,5132],
[M,156,2,6094,169,7,6671],
[M,156,64,6156,156,71,6163],
[M,156,4,6096,156,72,6164],
[M,170,2,6674,200,7,8227],
[M,170,4,6676,170,49,6721],
[M,201,2,8230,208,7,8514],
[M,201,4,8232,201,38,8266],
[M,209,2,8517,212,7,8684],
[M,209,29,8544,209,35,8550],
[M,209,48,8563,209,54,8569],
[M,209,72,8587,209,78,8593],
[M,209,4,8519,209,88,8603],
[M,213,2,8687,218,7,8950],
[M,213,34,8719,213,40,8725],
[M,213,4,8689,213,41,8726],
[M,219,2,8953,227,7,9324],
[M,219,4,8955,219,30,8981],
[M,228,2,9327,240,7,9936],
[M,228,39,9364,228,45,9370],
[M,228,63,9388,228,69,9394],
[M,228,81,9406,228,87,9412],
[M,228,4,9329,228,88,9413],
[M,241,2,9939,270,7,11266],
[M,241,31,9968,241,37,9974],
[M,241,4,9941,241,54,9991],
[M,272,2,11270,275,7,11374],
[M,277,2,11378,278,49,11464],
[M,277,4,11380,277,32,11408],
[M,16,0,277,279,3,11468],
[M,361,12,15317,361,36,15341],
[M,361,4,15309,361,37,15342],
[M,360,2,15284,362,5,15348],
[M,285,9,11597,285,17,11605],
[M,286,11,11618,286,18,11625],
[M,286,20,11627,286,63,11670],
[M,287,11,11684,287,26,11699],
[M,288,12,11713,288,49,11750],
[M,288,61,11762,288,88,11789],
[M,288,51,11752,288,108,11809],
[M,289,12,11823,289,62,11873],
[M,290,12,11887,290,58,11933],
[M,290,68,11943,290,80,11955],
[M,290,60,11935,290,81,11956],
[M,291,12,11970,291,34,11992],
[M,291,44,12002,291,56,12014],
[M,291,36,11994,291,57,12015],
[M,291,59,12017,291,89,12047],
[M,292,20,12069,292,33,12082],
[M,292,12,12061,292,34,12083],
[M,292,36,12085,292,63,12112],
[M,293,11,12126,293,18,12133],
[M,293,30,12145,293,97,12212],
[M,293,20,12135,293,106,12221],
[M,295,9,12247,295,17,12255],
[M,296,11,12268,296,18,12275],
[M,296,20,12277,296,67,12324],
[M,297,11,12338,297,26,12353],
[M,298,12,12367,298,49,12404],
[M,298,61,12416,298,88,12443],
[M,298,51,12406,298,108,12463],
[M,299,12,12477,299,59,12524],
[M,300,12,12538,300,97,12623],
[M,301,11,12637,301,18,12644],
[M,302,22,12668,302,86,12732],
[M,302,12,12658,302,95,12741],
[M,303,12,12755,303,97,12840],
[M,304,7,12850,304,15,12858],
[M,305,9,12869,305,16,12876],
[M,305,18,12878,305,72,12932],
[M,305,93,12953,305,101,12961],
[M,306,9,12974,306,15,12980],
[M,307,11,12993,307,18,13000],
[M,307,20,13002,307,73,13055],
[M,308,11,13069,308,22,13080],
[M,305,74,12934,305,102,12962],
[M,283,4,11551,309,7,13107],
[M,312,7,13171,312,15,13179],
[M,312,18,13182,312,25,13189],
[M,313,10,13201,313,74,13265],
[M,314,10,13277,314,112,13379],
[M,311,4,13143,315,7,13389],
[M,318,7,13469,318,15,13477],
[M,318,18,13480,318,33,13495],
[M,319,10,13507,319,87,13584],
[M,320,29,13615,320,37,13623],
[M,321,10,13636,321,62,13688],
[M,320,10,13596,320,38,13624],
[M,317,4,13441,322,7,13698],
[M,325,7,13771,325,15,13779],
[M,325,18,13782,325,33,13797],
[M,326,10,13809,326,37,13836],
[M,327,29,13867,327,37,13875],
[M,328,10,13888,328,52,13930],
[M,327,10,13848,327,38,13876],
[M,324,4,13743,329,7,13940],
[M,332,7,14011,332,15,14019],
[M,332,18,14022,332,33,14037],
[M,333,10,14049,333,72,14111],
[M,334,29,14142,334,37,14150],
[M,335,10,14163,335,100,14253],
[M,334,10,14123,334,38,14151],
[M,331,4,13983,336,7,14263],
[M,339,26,14355,339,34,14363],
[M,339,26,14355,339,47,14376],
[M,339,6,14335,339,47,14376],
[M,338,4,14295,340,7,14384],
[M,343,75,14528,343,83,14536],
[M,343,75,14528,343,96,14549],
[M,343,6,14459,343,96,14549],
[M,342,4,14419,344,7,14557],
[M,347,75,14707,347,83,14715],
[M,347,75,14707,347,96,14728],
[M,347,6,14638,347,96,14728],
[M,346,4,14598,348,7,14736],
[M,350,62,14832,350,70,14840],
[M,350,62,14832,350,83,14853],
[M,350,38,14808,350,83,14853],
[M,350,4,14774,350,87,14857],
[M,352,60,14949,352,68,14957],
[M,352,60,14949,352,81,14970],
[M,352,38,14927,352,81,14970],
[M,352,4,14893,352,85,14974],
[M,354,63,15072,354,71,15080],
[M,354,63,15072,354,84,15093],
[M,354,38,15047,354,84,15093],
[M,354,4,15013,354,88,15097],
[M,356,67,15203,356,75,15211],
[M,356,67,15203,356,88,15224],
[M,356,38,15174,356,88,15224],
[M,356,4,15140,356,92,15228],
[M,282,2,11489,309,7,13107],
[M,282,46,11533,282,52,11539],
[M,282,4,11491,282,53,11540],
[M,310,2,13110,315,7,13389],
[M,310,4,13112,310,24,13132],
[M,316,2,13392,322,7,13698],
[M,316,4,13394,316,40,13430],
[M,323,2,13701,329,7,13940],
[M,323,4,13703,323,33,13732],
[M,330,2,13943,336,7,14263],
[M,330,4,13945,330,31,13972],
[M,337,2,14266,340,7,14384],
[M,337,4,14268,337,20,14284],
[M,341,2,14387,344,7,14557],
[M,341,4,14389,341,23,14408],
[M,345,2,14560,348,7,14736],
[M,345,4,14562,345,29,14587],
[M,349,2,14739,350,87,14857],
[M,349,4,14741,349,26,14763],
[M,351,2,14860,352,85,14974],
[M,351,4,14862,351,24,14882],
[M,353,2,14977,354,88,15097],
[M,353,4,14979,353,27,15002],
[M,355,2,15100,356,92,15228],
[M,355,4,15102,355,31,15129],
[M,357,2,15231,357,20,15249],
[M,357,4,15233,357,20,15249],
[M,358,2,15252,358,22,15272],
[M,358,4,15254,358,22,15272],
[M,281,0,11470,363,3,15352],
[M,1,0,0,363,3,15352]];
var _plus1 = NAMESPACE.get("_plus");
var _lessthan2 = NAMESPACE.get("_lessthan");
var equal$always3 = NAMESPACE.get("equal-always");
var tostring4 = NAMESPACE.get("tostring");
var is$number5 = NAMESPACE.get("is-number");
var num$is$roughnum6 = NAMESPACE.get("num-is-roughnum");
var $type$String7 = NAMESPACE.get("$type$String");
var $type$Number8 = NAMESPACE.get("$type$Number");
var $type$Boolean9 = NAMESPACE.get("$type$Boolean");
var builtins10 = NAMESPACE.get("builtins");
var nothing11 = NAMESPACE.get("nothing");
var ED12 = R.getField($ED15,"values");
var ED13 = R.getField($ED15,"types");
NAMESPACE = R.addModuleToNamespace(NAMESPACE,[],[],$ED15);
var $toplevel17 = function($$resumer1862) {
var $step16 = 0;
var $ans19 = D;
var $al20 = L[617];
try {
if(R.isActivationRecord($$resumer1862)) {
$step16 = $$resumer1862.step;
$al20 = $$resumer1862.from;
$ans19 = $$resumer1862.ans;
$resumer1862 = $$resumer1862.args[0];
provides1860 = $$resumer1862.vars[0];
missing$comma1854 = $$resumer1862.vars[1];
is$missing$comma1852 = $$resumer1862.vars[2];
missing$end1850 = $$resumer1862.vars[3];
is$missing$end1848 = $$resumer1862.vars[4];
app$args$missing$comma1846 = $$resumer1862.vars[5];
is$app$args$missing$comma1844 = $$resumer1862.vars[6];
args$missing$comma1842 = $$resumer1862.vars[7];
is$args$missing$comma1840 = $$resumer1862.vars[8];
fun$missing$end1838 = $$resumer1862.vars[9];
is$fun$missing$end1836 = $$resumer1862.vars[10];
fun$missing$colon1834 = $$resumer1862.vars[11];
is$fun$missing$colon1832 = $$resumer1862.vars[12];
bad$check$block$stmt1830 = $$resumer1862.vars[13];
is$bad$check$block$stmt1828 = $$resumer1862.vars[14];
bad$block$stmt1826 = $$resumer1862.vars[15];
is$bad$block$stmt1824 = $$resumer1862.vars[16];
empty$block1822 = $$resumer1862.vars[17];
is$empty$block1820 = $$resumer1862.vars[18];
parse$error$bad$number1818 = $$resumer1862.vars[19];
is$parse$error$bad$number1816 = $$resumer1862.vars[20];
parse$error$bad$operator1814 = $$resumer1862.vars[21];
is$parse$error$bad$operator1812 = $$resumer1862.vars[22];
parse$error$unterminated$string1810 = $$resumer1862.vars[23];
is$parse$error$unterminated$string1808 = $$resumer1862.vars[24];
parse$error$eof1806 = $$resumer1862.vars[25];
is$parse$error$eof1804 = $$resumer1862.vars[26];
parse$error$next$token1802 = $$resumer1862.vars[27];
is$parse$error$next$token1800 = $$resumer1862.vars[28];
is$ParseError1798 = $$resumer1862.vars[29];
ParseError1796 = $$resumer1862.vars[30];
user$exception1383 = $$resumer1862.vars[31];
is$user$exception1381 = $$resumer1862.vars[32];
user$break1379 = $$resumer1862.vars[33];
is$user$break1377 = $$resumer1862.vars[34];
equality$failure1375 = $$resumer1862.vars[35];
is$equality$failure1373 = $$resumer1862.vars[36];
invalid$array$index1371 = $$resumer1862.vars[37];
is$invalid$array$index1369 = $$resumer1862.vars[38];
module$load$failure1367 = $$resumer1862.vars[39];
is$module$load$failure1365 = $$resumer1862.vars[40];
uninitialized$id1363 = $$resumer1862.vars[41];
is$uninitialized$id1361 = $$resumer1862.vars[42];
bad$app1359 = $$resumer1862.vars[43];
is$bad$app1357 = $$resumer1862.vars[44];
non$function$app1355 = $$resumer1862.vars[45];
is$non$function$app1353 = $$resumer1862.vars[46];
arity$mismatch1351 = $$resumer1862.vars[47];
is$arity$mismatch1349 = $$resumer1862.vars[48];
cases$singleton$mismatch1347 = $$resumer1862.vars[49];
is$cases$singleton$mismatch1345 = $$resumer1862.vars[50];
cases$arity$mismatch1343 = $$resumer1862.vars[51];
is$cases$arity$mismatch1341 = $$resumer1862.vars[52];
numeric$binop$error1339 = $$resumer1862.vars[53];
is$numeric$binop$error1337 = $$resumer1862.vars[54];
num$string$binop$error1335 = $$resumer1862.vars[55];
is$num$string$binop$error1333 = $$resumer1862.vars[56];
outside$numeric$range1331 = $$resumer1862.vars[57];
is$outside$numeric$range1329 = $$resumer1862.vars[58];
generic$type$mismatch1327 = $$resumer1862.vars[59];
is$generic$type$mismatch1325 = $$resumer1862.vars[60];
non$boolean$op1323 = $$resumer1862.vars[61];
is$non$boolean$op1321 = $$resumer1862.vars[62];
non$boolean$condition1319 = $$resumer1862.vars[63];
is$non$boolean$condition1317 = $$resumer1862.vars[64];
extend$non$object1315 = $$resumer1862.vars[65];
is$extend$non$object1313 = $$resumer1862.vars[66];
lookup$non$object1311 = $$resumer1862.vars[67];
is$lookup$non$object1309 = $$resumer1862.vars[68];
field$not$found1307 = $$resumer1862.vars[69];
is$field$not$found1305 = $$resumer1862.vars[70];
internal$error1303 = $$resumer1862.vars[71];
is$internal$error1301 = $$resumer1862.vars[72];
no$branches$matched1299 = $$resumer1862.vars[73];
is$no$branches$matched1297 = $$resumer1862.vars[74];
no$cases$matched1295 = $$resumer1862.vars[75];
is$no$cases$matched1293 = $$resumer1862.vars[76];
message$exception1291 = $$resumer1862.vars[77];
is$message$exception1289 = $$resumer1862.vars[78];
is$RuntimeError1287 = $$resumer1862.vars[79];
RuntimeError1285 = $$resumer1862.vars[80];
vert$list$values56 = $$resumer1862.vars[81];
draw$and$highlight33 = $$resumer1862.vars[82];
ParseError1859 = $$resumer1862.vars[83];
RuntimeError1858 = $$resumer1862.vars[84];
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step16) {
case 0: var RuntimeError21 = R.namedBrander("RuntimeError");
var RuntimeError1858 = R.makeBranderAnn(RuntimeError21,"RuntimeError");
var ParseError22 = R.namedBrander("ParseError");
var ParseError1859 = R.makeBranderAnn(ParseError22,"ParseError");
var draw$and$highlight33 = {"$var":D};
var vert$list$values56 = {"$var":D};
var RuntimeError1283 = {"$var":D};
var RuntimeError1285 = {"$var":D};
var is$RuntimeError1287 = {"$var":D};
var is$message$exception1289 = {"$var":D};
var message$exception1291 = {"$var":D};
var is$no$cases$matched1293 = {"$var":D};
var no$cases$matched1295 = {"$var":D};
var is$no$branches$matched1297 = {"$var":D};
var no$branches$matched1299 = {"$var":D};
var is$internal$error1301 = {"$var":D};
var internal$error1303 = {"$var":D};
var is$field$not$found1305 = {"$var":D};
var field$not$found1307 = {"$var":D};
var is$lookup$non$object1309 = {"$var":D};
var lookup$non$object1311 = {"$var":D};
var is$extend$non$object1313 = {"$var":D};
var extend$non$object1315 = {"$var":D};
var is$non$boolean$condition1317 = {"$var":D};
var non$boolean$condition1319 = {"$var":D};
var is$non$boolean$op1321 = {"$var":D};
var non$boolean$op1323 = {"$var":D};
var is$generic$type$mismatch1325 = {"$var":D};
var generic$type$mismatch1327 = {"$var":D};
var is$outside$numeric$range1329 = {"$var":D};
var outside$numeric$range1331 = {"$var":D};
var is$num$string$binop$error1333 = {"$var":D};
var num$string$binop$error1335 = {"$var":D};
var is$numeric$binop$error1337 = {"$var":D};
var numeric$binop$error1339 = {"$var":D};
var is$cases$arity$mismatch1341 = {"$var":D};
var cases$arity$mismatch1343 = {"$var":D};
var is$cases$singleton$mismatch1345 = {"$var":D};
var cases$singleton$mismatch1347 = {"$var":D};
var is$arity$mismatch1349 = {"$var":D};
var arity$mismatch1351 = {"$var":D};
var is$non$function$app1353 = {"$var":D};
var non$function$app1355 = {"$var":D};
var is$bad$app1357 = {"$var":D};
var bad$app1359 = {"$var":D};
var is$uninitialized$id1361 = {"$var":D};
var uninitialized$id1363 = {"$var":D};
var is$module$load$failure1365 = {"$var":D};
var module$load$failure1367 = {"$var":D};
var is$invalid$array$index1369 = {"$var":D};
var invalid$array$index1371 = {"$var":D};
var is$equality$failure1373 = {"$var":D};
var equality$failure1375 = {"$var":D};
var is$user$break1377 = {"$var":D};
var user$break1379 = {"$var":D};
var is$user$exception1381 = {"$var":D};
var user$exception1383 = {"$var":D};
var ParseError1794 = {"$var":D};
var ParseError1796 = {"$var":D};
var is$ParseError1798 = {"$var":D};
var is$parse$error$next$token1800 = {"$var":D};
var parse$error$next$token1802 = {"$var":D};
var is$parse$error$eof1804 = {"$var":D};
var parse$error$eof1806 = {"$var":D};
var is$parse$error$unterminated$string1808 = {"$var":D};
var parse$error$unterminated$string1810 = {"$var":D};
var is$parse$error$bad$operator1812 = {"$var":D};
var parse$error$bad$operator1814 = {"$var":D};
var is$parse$error$bad$number1816 = {"$var":D};
var parse$error$bad$number1818 = {"$var":D};
var is$empty$block1820 = {"$var":D};
var empty$block1822 = {"$var":D};
var is$bad$block$stmt1824 = {"$var":D};
var bad$block$stmt1826 = {"$var":D};
var is$bad$check$block$stmt1828 = {"$var":D};
var bad$check$block$stmt1830 = {"$var":D};
var is$fun$missing$colon1832 = {"$var":D};
var fun$missing$colon1834 = {"$var":D};
var is$fun$missing$end1836 = {"$var":D};
var fun$missing$end1838 = {"$var":D};
var is$args$missing$comma1840 = {"$var":D};
var args$missing$comma1842 = {"$var":D};
var is$app$args$missing$comma1844 = {"$var":D};
var app$args$missing$comma1846 = {"$var":D};
var is$missing$end1848 = {"$var":D};
var missing$end1850 = {"$var":D};
var is$missing$comma1852 = {"$var":D};
var missing$comma1854 = {"$var":D};
var $temp_lam24 = function($l25) {
var $step23 = 0;
var $ans26 = D;
var $al27 = L[2];
try {
if(R.isActivationRecord($l25)) {
$step23 = $l25.step;
$al27 = $l25.from;
$ans26 = $l25.ans;
l25 = $l25.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[2],1,$t);
}
var l25 = $l25;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step23) {
case 0: $step23 = 1;
$al27 = L[0];
$field28 = R.getColonFieldLoc(ED12,"loc",L[0]);
if(R.isMethod($field28)) {
$ans26 = $field28.full_meth(ED12,l25);
} else {
if(!(R.isFunction($field28))) {
R.ffi.throwNonFunApp(L[0],$field28);
}
$ans26 = $field28.app(l25);
}
break;
case 1: var anf_arg29 = $ans26;
$step23 = 2;
$al27 = L[1];
$field30 = R.getColonFieldLoc(ED12,"loc-display",L[1]);
if(R.isMethod($field30)) {
$ans26 = $field30.full_meth(ED12,l25,("error-highlight"),anf_arg29);
} else {
if(!(R.isFunction($field30))) {
R.ffi.throwNonFunApp(L[1],$field30);
}
$ans26 = $field30.app(l25,("error-highlight"),anf_arg29);
}
break;
case 2: ++R.GAS;
return $ans26;
default: throw "No case numbered " + $step23 + " in $temp_lam24";
}
}
} catch($e31) {
if(R.isCont($e31) && ($step23 !== 2)) {
$e31.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al27,$temp_lam24,$step23,[l25],[]);
}
if(R.isPyretException($e31)) {
$e31.pyretStack.push($al27);
}
throw $e31;
}
};
var anf_assign32 = R.makeFunction($temp_lam24);
draw$and$highlight33.$var = anf_assign32;
var $temp_lam35 = function($vals36) {
var $step34 = 0;
var $ans37 = D;
var $al38 = L[8];
try {
if(R.isActivationRecord($vals36)) {
$step34 = $vals36.step;
$al38 = $vals36.from;
$ans37 = $vals36.ans;
vals36 = $vals36.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[8],1,$t);
}
var vals36 = $vals36;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step34) {
case 0: var $temp_lam40 = function($val41) {
var $step39 = 0;
var $ans42 = D;
var $al43 = L[5];
try {
if(R.isActivationRecord($val41)) {
$step39 = $val41.step;
$al43 = $val41.from;
$ans42 = $val41.ans;
val41 = $val41.args[0];
anf_method_obj46 = $val41.vars[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[5],1,$t);
}
var val41 = $val41;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step39) {
case 0: var anf_method_obj46 = G(ED12,"para",L[3]);
$step39 = 1;
$al43 = L[4];
$field44 = R.getColonFieldLoc(ED12,"embed",L[4]);
if(R.isMethod($field44)) {
$ans42 = $field44.full_meth(ED12,val41);
} else {
if(!(R.isFunction($field44))) {
R.ffi.throwNonFunApp(L[4],$field44);
}
$ans42 = $field44.app(val41);
}
break;
case 1: var anf_array_val45 = $ans42;
var anf_arg47 = [anf_array_val45];
$step39 = 2;
$al43 = L[3];
$field48 = R.getColonFieldLoc(anf_method_obj46,"make",L[3]);
if(R.isMethod($field48)) {
$ans42 = $field48.full_meth(anf_method_obj46,anf_arg47);
} else {
if(!(R.isFunction($field48))) {
R.ffi.throwNonFunApp(L[3],$field48);
}
$ans42 = $field48.app(anf_arg47);
}
break;
case 2: ++R.GAS;
return $ans42;
default: throw "No case numbered " + $step39 + " in $temp_lam40";
}
}
} catch($e49) {
if(R.isCont($e49) && ($step39 !== 2)) {
$e49.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al43,$temp_lam40,$step39,[val41],[anf_method_obj46]);
}
if(R.isPyretException($e49)) {
$e49.pyretStack.push($al43);
}
throw $e49;
}
};
var anf_arg50 = R.makeFunction($temp_lam40);
$step34 = 1;
$al38 = L[6];
$field51 = R.getColonFieldLoc(vals36,"map",L[6]);
if(R.isMethod($field51)) {
$ans37 = $field51.full_meth(vals36,anf_arg50);
} else {
if(!(R.isFunction($field51))) {
R.ffi.throwNonFunApp(L[6],$field51);
}
$ans37 = $field51.app(anf_arg50);
}
break;
case 1: var anf_arg52 = $ans37;
$step34 = 2;
$al38 = L[7];
$field53 = R.getColonFieldLoc(ED12,"v-sequence",L[7]);
if(R.isMethod($field53)) {
$ans37 = $field53.full_meth(ED12,anf_arg52);
} else {
if(!(R.isFunction($field53))) {
R.ffi.throwNonFunApp(L[7],$field53);
}
$ans37 = $field53.app(anf_arg52);
}
break;
case 2: ++R.GAS;
return $ans37;
default: throw "No case numbered " + $step34 + " in $temp_lam35";
}
}
} catch($e54) {
if(R.isCont($e54) && ($step34 !== 2)) {
$e54.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al38,$temp_lam35,$step34,[vals36],[]);
}
if(R.isPyretException($e54)) {
$e54.pyretStack.push($al38);
}
throw $e54;
}
};
var anf_assign55 = R.makeFunction($temp_lam35);
vert$list$values56.$var = anf_assign55;
var $temp_full58 = function($self59) {
var $step57 = 0;
var $ans60 = D;
var $al61 = L[13];
try {
if(R.isActivationRecord($self59)) {
$step57 = $self59.step;
$al61 = $self59.from;
$ans60 = $self59.ans;
self59 = $self59.args[0];
anf_method_obj65 = $self59.vars[0];
anf_method_obj69 = $self59.vars[1];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[13],1,$t);
}
var self59 = $self59;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step57) {
case 0: var anf_method_obj69 = G(ED12,"error",L[9]);
var anf_method_obj65 = G(ED12,"para",L[10]);
var anf_arg62 = G(self59,"message",L[11]);
$step57 = 1;
$al61 = L[12];
$field63 = R.getColonFieldLoc(ED12,"text",L[12]);
if(R.isMethod($field63)) {
$ans60 = $field63.full_meth(ED12,anf_arg62);
} else {
if(!(R.isFunction($field63))) {
R.ffi.throwNonFunApp(L[12],$field63);
}
$ans60 = $field63.app(anf_arg62);
}
break;
case 1: var anf_array_val64 = $ans60;
var anf_arg66 = [anf_array_val64];
$step57 = 2;
$al61 = L[10];
$field67 = R.getColonFieldLoc(anf_method_obj65,"make",L[10]);
if(R.isMethod($field67)) {
$ans60 = $field67.full_meth(anf_method_obj65,anf_arg66);
} else {
if(!(R.isFunction($field67))) {
R.ffi.throwNonFunApp(L[10],$field67);
}
$ans60 = $field67.app(anf_arg66);
}
break;
case 2: var anf_array_val68 = $ans60;
var anf_arg70 = [anf_array_val68];
$step57 = 3;
$al61 = L[9];
$field71 = R.getColonFieldLoc(anf_method_obj69,"make",L[9]);
if(R.isMethod($field71)) {
$ans60 = $field71.full_meth(anf_method_obj69,anf_arg70);
} else {
if(!(R.isFunction($field71))) {
R.ffi.throwNonFunApp(L[9],$field71);
}
$ans60 = $field71.app(anf_arg70);
}
break;
case 3: ++R.GAS;
return $ans60;
default: throw "No case numbered " + $step57 + " in $temp_full58";
}
}
} catch($e72) {
if(R.isCont($e72) && ($step57 !== 3)) {
$e72.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al61,$temp_full58,$step57,[self59],[anf_method_obj65,anf_method_obj69]);
}
if(R.isPyretException($e72)) {
$e72.pyretStack.push($al61);
}
throw $e72;
}
};
var anf_variant_member1007 = R.makeMethod0($temp_full58);
var $temp_full74 = function($self75) {
var $step73 = 0;
var $ans76 = D;
var $al77 = L[22];
try {
if(R.isActivationRecord($self75)) {
$step73 = $self75.step;
$al77 = $self75.from;
$ans76 = $self75.ans;
self75 = $self75.args[0];
anf_array_val89 = $self75.vars[0];
anf_array_val82 = $self75.vars[1];
anf_array_val81 = $self75.vars[2];
anf_method_obj84 = $self75.vars[3];
anf_method_obj91 = $self75.vars[4];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[22],1,$t);
}
var self75 = $self75;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step73) {
case 0: var anf_method_obj91 = G(ED12,"error",L[14]);
var anf_method_obj84 = G(ED12,"para",L[15]);
$step73 = 1;
$al77 = L[16];
$field78 = R.getColonFieldLoc(ED12,"text",L[16]);
if(R.isMethod($field78)) {
$ans76 = $field78.full_meth(ED12,("No branches matched in the cases expression at"));
} else {
if(!(R.isFunction($field78))) {
R.ffi.throwNonFunApp(L[16],$field78);
}
$ans76 = $field78.app(("No branches matched in the cases expression at"));
}
break;
case 1: var anf_array_val81 = $ans76;
var anf_arg79 = G(self75,"loc",L[17]);
$step73 = 2;
$al77 = L[21];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al77,draw$and$highlight33.$var);
}
$ans76 = draw$and$highlight33.$var.app(anf_arg79);
break;
case 2: var anf_array_val82 = $ans76;
$step73 = 3;
$al77 = L[18];
$field80 = R.getColonFieldLoc(ED12,"text",L[18]);
if(R.isMethod($field80)) {
$ans76 = $field80.full_meth(ED12,("for value"));
} else {
if(!(R.isFunction($field80))) {
R.ffi.throwNonFunApp(L[18],$field80);
}
$ans76 = $field80.app(("for value"));
}
break;
case 3: var anf_array_val83 = $ans76;
var anf_arg85 = [anf_array_val81,anf_array_val82,anf_array_val83];
$step73 = 4;
$al77 = L[15];
$field86 = R.getColonFieldLoc(anf_method_obj84,"make",L[15]);
if(R.isMethod($field86)) {
$ans76 = $field86.full_meth(anf_method_obj84,anf_arg85);
} else {
if(!(R.isFunction($field86))) {
R.ffi.throwNonFunApp(L[15],$field86);
}
$ans76 = $field86.app(anf_arg85);
}
break;
case 4: var anf_array_val89 = $ans76;
var anf_arg87 = G(self75,"val",L[19]);
$step73 = 5;
$al77 = L[20];
$field88 = R.getColonFieldLoc(ED12,"embed",L[20]);
if(R.isMethod($field88)) {
$ans76 = $field88.full_meth(ED12,anf_arg87);
} else {
if(!(R.isFunction($field88))) {
R.ffi.throwNonFunApp(L[20],$field88);
}
$ans76 = $field88.app(anf_arg87);
}
break;
case 5: var anf_array_val90 = $ans76;
var anf_arg92 = [anf_array_val89,anf_array_val90];
$step73 = 6;
$al77 = L[14];
$field93 = R.getColonFieldLoc(anf_method_obj91,"make",L[14]);
if(R.isMethod($field93)) {
$ans76 = $field93.full_meth(anf_method_obj91,anf_arg92);
} else {
if(!(R.isFunction($field93))) {
R.ffi.throwNonFunApp(L[14],$field93);
}
$ans76 = $field93.app(anf_arg92);
}
break;
case 6: ++R.GAS;
return $ans76;
default: throw "No case numbered " + $step73 + " in $temp_full74";
}
}
} catch($e94) {
if(R.isCont($e94) && ($step73 !== 6)) {
$e94.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al77,$temp_full74,$step73,[self75],[anf_array_val89,anf_array_val82,anf_array_val81,anf_method_obj84,anf_method_obj91]);
}
if(R.isPyretException($e94)) {
$e94.pyretStack.push($al77);
}
throw $e94;
}
};
var anf_variant_member1017 = R.makeMethod0($temp_full74);
var $temp_full96 = function($self97) {
var $step95 = 0;
var $ans98 = D;
var $al99 = L[32];
try {
if(R.isActivationRecord($self97)) {
$step95 = $self97.step;
$al99 = $self97.from;
$ans98 = $self97.ans;
self97 = $self97.args[0];
anf_array_val109 = $self97.vars[0];
anf_array_val108 = $self97.vars[1];
anf_array_val107 = $self97.vars[2];
anf_method_obj111 = $self97.vars[3];
anf_method_obj115 = $self97.vars[4];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[32],1,$t);
}
var self97 = $self97;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step95) {
case 0: var anf_method_obj115 = G(ED12,"error",L[23]);
var anf_method_obj111 = G(ED12,"para",L[24]);
$step95 = 1;
$al99 = L[25];
$field100 = R.getColonFieldLoc(ED12,"text",L[25]);
if(R.isMethod($field100)) {
$ans98 = $field100.full_meth(ED12,("No branches matched in the"));
} else {
if(!(R.isFunction($field100))) {
R.ffi.throwNonFunApp(L[25],$field100);
}
$ans98 = $field100.app(("No branches matched in the"));
}
break;
case 1: var anf_array_val107 = $ans98;
var anf_arg101 = G(self97,"expression",L[26]);
$step95 = 2;
$al99 = L[27];
$field102 = R.getColonFieldLoc(ED12,"text",L[27]);
if(R.isMethod($field102)) {
$ans98 = $field102.full_meth(ED12,anf_arg101);
} else {
if(!(R.isFunction($field102))) {
R.ffi.throwNonFunApp(L[27],$field102);
}
$ans98 = $field102.app(anf_arg101);
}
break;
case 2: var anf_arg103 = $ans98;
$step95 = 3;
$al99 = L[28];
$field104 = R.getColonFieldLoc(ED12,"code",L[28]);
if(R.isMethod($field104)) {
$ans98 = $field104.full_meth(ED12,anf_arg103);
} else {
if(!(R.isFunction($field104))) {
R.ffi.throwNonFunApp(L[28],$field104);
}
$ans98 = $field104.app(anf_arg103);
}
break;
case 3: var anf_array_val108 = $ans98;
$step95 = 4;
$al99 = L[29];
$field105 = R.getColonFieldLoc(ED12,"text",L[29]);
if(R.isMethod($field105)) {
$ans98 = $field105.full_meth(ED12,("expression at"));
} else {
if(!(R.isFunction($field105))) {
R.ffi.throwNonFunApp(L[29],$field105);
}
$ans98 = $field105.app(("expression at"));
}
break;
case 4: var anf_array_val109 = $ans98;
var anf_arg106 = G(self97,"loc",L[30]);
$step95 = 5;
$al99 = L[31];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al99,draw$and$highlight33.$var);
}
$ans98 = draw$and$highlight33.$var.app(anf_arg106);
break;
case 5: var anf_array_val110 = $ans98;
var anf_arg112 = [anf_array_val107,anf_array_val108,anf_array_val109,anf_array_val110];
$step95 = 6;
$al99 = L[24];
$field113 = R.getColonFieldLoc(anf_method_obj111,"make",L[24]);
if(R.isMethod($field113)) {
$ans98 = $field113.full_meth(anf_method_obj111,anf_arg112);
} else {
if(!(R.isFunction($field113))) {
R.ffi.throwNonFunApp(L[24],$field113);
}
$ans98 = $field113.app(anf_arg112);
}
break;
case 6: var anf_array_val114 = $ans98;
var anf_arg116 = [anf_array_val114];
$step95 = 7;
$al99 = L[23];
$field117 = R.getColonFieldLoc(anf_method_obj115,"make",L[23]);
if(R.isMethod($field117)) {
$ans98 = $field117.full_meth(anf_method_obj115,anf_arg116);
} else {
if(!(R.isFunction($field117))) {
R.ffi.throwNonFunApp(L[23],$field117);
}
$ans98 = $field117.app(anf_arg116);
}
break;
case 7: ++R.GAS;
return $ans98;
default: throw "No case numbered " + $step95 + " in $temp_full96";
}
}
} catch($e118) {
if(R.isCont($e118) && ($step95 !== 7)) {
$e118.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al99,$temp_full96,$step95,[self97],[anf_array_val109,anf_array_val108,anf_array_val107,anf_method_obj111,anf_method_obj115]);
}
if(R.isPyretException($e118)) {
$e118.pyretStack.push($al99);
}
throw $e118;
}
};
var anf_variant_member1028 = R.makeMethod0($temp_full96);
var $temp_full120 = function($self121) {
var $step119 = 0;
var $ans122 = D;
var $al123 = L[42];
try {
if(R.isActivationRecord($self121)) {
$step119 = $self121.step;
$al123 = $self121.from;
$ans122 = $self121.ans;
self121 = $self121.args[0];
anf_array_val139 = $self121.vars[0];
anf_method_obj134 = $self121.vars[1];
anf_array_val138 = $self121.vars[2];
anf_array_val127 = $self121.vars[3];
anf_method_obj129 = $self121.vars[4];
anf_method_obj141 = $self121.vars[5];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[42],1,$t);
}
var self121 = $self121;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step119) {
case 0: var anf_method_obj141 = G(ED12,"error",L[33]);
var anf_method_obj129 = G(ED12,"para",L[34]);
$step119 = 1;
$al123 = L[35];
$field124 = R.getColonFieldLoc(ED12,"text",L[35]);
if(R.isMethod($field124)) {
$ans122 = $field124.full_meth(ED12,("Internal error:"));
} else {
if(!(R.isFunction($field124))) {
R.ffi.throwNonFunApp(L[35],$field124);
}
$ans122 = $field124.app(("Internal error:"));
}
break;
case 1: var anf_array_val127 = $ans122;
var anf_arg125 = G(self121,"message",L[36]);
$step119 = 2;
$al123 = L[37];
$field126 = R.getColonFieldLoc(ED12,"text",L[37]);
if(R.isMethod($field126)) {
$ans122 = $field126.full_meth(ED12,anf_arg125);
} else {
if(!(R.isFunction($field126))) {
R.ffi.throwNonFunApp(L[37],$field126);
}
$ans122 = $field126.app(anf_arg125);
}
break;
case 2: var anf_array_val128 = $ans122;
var anf_arg130 = [anf_array_val127,anf_array_val128];
$step119 = 3;
$al123 = L[34];
$field131 = R.getColonFieldLoc(anf_method_obj129,"make",L[34]);
if(R.isMethod($field131)) {
$ans122 = $field131.full_meth(anf_method_obj129,anf_arg130);
} else {
if(!(R.isFunction($field131))) {
R.ffi.throwNonFunApp(L[34],$field131);
}
$ans122 = $field131.app(anf_arg130);
}
break;
case 3: var anf_array_val138 = $ans122;
var anf_method_obj134 = G(ED12,"para",L[38]);
$step119 = 4;
$al123 = L[39];
$field132 = R.getColonFieldLoc(ED12,"text",L[39]);
if(R.isMethod($field132)) {
$ans122 = $field132.full_meth(ED12,("Relevant arguments:"));
} else {
if(!(R.isFunction($field132))) {
R.ffi.throwNonFunApp(L[39],$field132);
}
$ans122 = $field132.app(("Relevant arguments:"));
}
break;
case 4: var anf_array_val133 = $ans122;
var anf_arg135 = [anf_array_val133];
$step119 = 5;
$al123 = L[38];
$field136 = R.getColonFieldLoc(anf_method_obj134,"make",L[38]);
if(R.isMethod($field136)) {
$ans122 = $field136.full_meth(anf_method_obj134,anf_arg135);
} else {
if(!(R.isFunction($field136))) {
R.ffi.throwNonFunApp(L[38],$field136);
}
$ans122 = $field136.app(anf_arg135);
}
break;
case 5: var anf_array_val139 = $ans122;
var anf_arg137 = G(self121,"info-args",L[40]);
$step119 = 6;
$al123 = L[41];
if(!(R.isFunction(vert$list$values56.$var))) {
R.ffi.throwNonFunApp($al123,vert$list$values56.$var);
}
$ans122 = vert$list$values56.$var.app(anf_arg137);
break;
case 6: var anf_array_val140 = $ans122;
var anf_arg142 = [anf_array_val138,anf_array_val139,anf_array_val140];
$step119 = 7;
$al123 = L[33];
$field143 = R.getColonFieldLoc(anf_method_obj141,"make",L[33]);
if(R.isMethod($field143)) {
$ans122 = $field143.full_meth(anf_method_obj141,anf_arg142);
} else {
if(!(R.isFunction($field143))) {
R.ffi.throwNonFunApp(L[33],$field143);
}
$ans122 = $field143.app(anf_arg142);
}
break;
case 7: ++R.GAS;
return $ans122;
default: throw "No case numbered " + $step119 + " in $temp_full120";
}
}
} catch($e144) {
if(R.isCont($e144) && ($step119 !== 7)) {
$e144.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al123,$temp_full120,$step119,[self121],[anf_array_val139,anf_method_obj134,anf_array_val138,anf_array_val127,anf_method_obj129,anf_method_obj141]);
}
if(R.isPyretException($e144)) {
$e144.pyretStack.push($al123);
}
throw $e144;
}
};
var anf_variant_member1039 = R.makeMethod0($temp_full120);
var $temp_full146 = function($self147) {
var $step145 = 0;
var $ans148 = D;
var $al149 = L[56];
try {
if(R.isActivationRecord($self147)) {
$step145 = $self147.step;
$al149 = $self147.from;
$ans148 = $self147.ans;
self147 = $self147.args[0];
anf_array_val172 = $self147.vars[0];
anf_method_obj166 = $self147.vars[1];
anf_array_val171 = $self147.vars[2];
anf_array_val159 = $self147.vars[3];
anf_array_val158 = $self147.vars[4];
anf_array_val157 = $self147.vars[5];
anf_method_obj161 = $self147.vars[6];
anf_method_obj174 = $self147.vars[7];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[56],1,$t);
}
var self147 = $self147;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step145) {
case 0: var anf_method_obj174 = G(ED12,"error",L[43]);
var anf_method_obj161 = G(ED12,"para",L[44]);
$step145 = 1;
$al149 = L[45];
$field150 = R.getColonFieldLoc(ED12,"text",L[45]);
if(R.isMethod($field150)) {
$ans148 = $field150.full_meth(ED12,("Field"));
} else {
if(!(R.isFunction($field150))) {
R.ffi.throwNonFunApp(L[45],$field150);
}
$ans148 = $field150.app(("Field"));
}
break;
case 1: var anf_array_val157 = $ans148;
var anf_arg151 = G(self147,"field",L[46]);
$step145 = 2;
$al149 = L[47];
$field152 = R.getColonFieldLoc(ED12,"text",L[47]);
if(R.isMethod($field152)) {
$ans148 = $field152.full_meth(ED12,anf_arg151);
} else {
if(!(R.isFunction($field152))) {
R.ffi.throwNonFunApp(L[47],$field152);
}
$ans148 = $field152.app(anf_arg151);
}
break;
case 2: var anf_arg153 = $ans148;
$step145 = 3;
$al149 = L[48];
$field154 = R.getColonFieldLoc(ED12,"code",L[48]);
if(R.isMethod($field154)) {
$ans148 = $field154.full_meth(ED12,anf_arg153);
} else {
if(!(R.isFunction($field154))) {
R.ffi.throwNonFunApp(L[48],$field154);
}
$ans148 = $field154.app(anf_arg153);
}
break;
case 3: var anf_array_val158 = $ans148;
$step145 = 4;
$al149 = L[49];
$field155 = R.getColonFieldLoc(ED12,"text",L[49]);
if(R.isMethod($field155)) {
$ans148 = $field155.full_meth(ED12,("not found in the lookup expression at"));
} else {
if(!(R.isFunction($field155))) {
R.ffi.throwNonFunApp(L[49],$field155);
}
$ans148 = $field155.app(("not found in the lookup expression at"));
}
break;
case 4: var anf_array_val159 = $ans148;
var anf_arg156 = G(self147,"loc",L[50]);
$step145 = 5;
$al149 = L[55];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al149,draw$and$highlight33.$var);
}
$ans148 = draw$and$highlight33.$var.app(anf_arg156);
break;
case 5: var anf_array_val160 = $ans148;
var anf_arg162 = [anf_array_val157,anf_array_val158,anf_array_val159,anf_array_val160];
$step145 = 6;
$al149 = L[44];
$field163 = R.getColonFieldLoc(anf_method_obj161,"make",L[44]);
if(R.isMethod($field163)) {
$ans148 = $field163.full_meth(anf_method_obj161,anf_arg162);
} else {
if(!(R.isFunction($field163))) {
R.ffi.throwNonFunApp(L[44],$field163);
}
$ans148 = $field163.app(anf_arg162);
}
break;
case 6: var anf_array_val171 = $ans148;
var anf_method_obj166 = G(ED12,"para",L[51]);
$step145 = 7;
$al149 = L[52];
$field164 = R.getColonFieldLoc(ED12,"text",L[52]);
if(R.isMethod($field164)) {
$ans148 = $field164.full_meth(ED12,("The object was:"));
} else {
if(!(R.isFunction($field164))) {
R.ffi.throwNonFunApp(L[52],$field164);
}
$ans148 = $field164.app(("The object was:"));
}
break;
case 7: var anf_array_val165 = $ans148;
var anf_arg167 = [anf_array_val165];
$step145 = 8;
$al149 = L[51];
$field168 = R.getColonFieldLoc(anf_method_obj166,"make",L[51]);
if(R.isMethod($field168)) {
$ans148 = $field168.full_meth(anf_method_obj166,anf_arg167);
} else {
if(!(R.isFunction($field168))) {
R.ffi.throwNonFunApp(L[51],$field168);
}
$ans148 = $field168.app(anf_arg167);
}
break;
case 8: var anf_array_val172 = $ans148;
var anf_arg169 = G(self147,"obj",L[53]);
$step145 = 9;
$al149 = L[54];
$field170 = R.getColonFieldLoc(ED12,"embed",L[54]);
if(R.isMethod($field170)) {
$ans148 = $field170.full_meth(ED12,anf_arg169);
} else {
if(!(R.isFunction($field170))) {
R.ffi.throwNonFunApp(L[54],$field170);
}
$ans148 = $field170.app(anf_arg169);
}
break;
case 9: var anf_array_val173 = $ans148;
var anf_arg175 = [anf_array_val171,anf_array_val172,anf_array_val173];
$step145 = 10;
$al149 = L[43];
$field176 = R.getColonFieldLoc(anf_method_obj174,"make",L[43]);
if(R.isMethod($field176)) {
$ans148 = $field176.full_meth(anf_method_obj174,anf_arg175);
} else {
if(!(R.isFunction($field176))) {
R.ffi.throwNonFunApp(L[43],$field176);
}
$ans148 = $field176.app(anf_arg175);
}
break;
case 10: ++R.GAS;
return $ans148;
default: throw "No case numbered " + $step145 + " in $temp_full146";
}
}
} catch($e177) {
if(R.isCont($e177) && ($step145 !== 10)) {
$e177.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al149,$temp_full146,$step145,[self147],[anf_array_val172,anf_method_obj166,anf_array_val171,anf_array_val159,anf_array_val158,anf_array_val157,anf_method_obj161,anf_method_obj174]);
}
if(R.isPyretException($e177)) {
$e177.pyretStack.push($al149);
}
throw $e177;
}
};
var anf_variant_member1050 = R.makeMethod0($temp_full146);
var $temp_full179 = function($self180) {
var $step178 = 0;
var $ans181 = D;
var $al182 = L[70];
try {
if(R.isActivationRecord($self180)) {
$step178 = $self180.step;
$al182 = $self180.from;
$ans181 = $self180.ans;
self180 = $self180.args[0];
anf_array_val205 = $self180.vars[0];
anf_method_obj199 = $self180.vars[1];
anf_array_val204 = $self180.vars[2];
anf_array_val192 = $self180.vars[3];
anf_array_val191 = $self180.vars[4];
anf_array_val190 = $self180.vars[5];
anf_method_obj194 = $self180.vars[6];
anf_method_obj207 = $self180.vars[7];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[70],1,$t);
}
var self180 = $self180;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step178) {
case 0: var anf_method_obj207 = G(ED12,"error",L[57]);
var anf_method_obj194 = G(ED12,"para",L[58]);
$step178 = 1;
$al182 = L[59];
$field183 = R.getColonFieldLoc(ED12,"text",L[59]);
if(R.isMethod($field183)) {
$ans181 = $field183.full_meth(ED12,("Tried to look up field"));
} else {
if(!(R.isFunction($field183))) {
R.ffi.throwNonFunApp(L[59],$field183);
}
$ans181 = $field183.app(("Tried to look up field"));
}
break;
case 1: var anf_array_val190 = $ans181;
var anf_arg184 = G(self180,"field",L[60]);
$step178 = 2;
$al182 = L[61];
$field185 = R.getColonFieldLoc(ED12,"text",L[61]);
if(R.isMethod($field185)) {
$ans181 = $field185.full_meth(ED12,anf_arg184);
} else {
if(!(R.isFunction($field185))) {
R.ffi.throwNonFunApp(L[61],$field185);
}
$ans181 = $field185.app(anf_arg184);
}
break;
case 2: var anf_arg186 = $ans181;
$step178 = 3;
$al182 = L[62];
$field187 = R.getColonFieldLoc(ED12,"code",L[62]);
if(R.isMethod($field187)) {
$ans181 = $field187.full_meth(ED12,anf_arg186);
} else {
if(!(R.isFunction($field187))) {
R.ffi.throwNonFunApp(L[62],$field187);
}
$ans181 = $field187.app(anf_arg186);
}
break;
case 3: var anf_array_val191 = $ans181;
$step178 = 4;
$al182 = L[63];
$field188 = R.getColonFieldLoc(ED12,"text",L[63]);
if(R.isMethod($field188)) {
$ans181 = $field188.full_meth(ED12,("on a non-object in the lookup expression at"));
} else {
if(!(R.isFunction($field188))) {
R.ffi.throwNonFunApp(L[63],$field188);
}
$ans181 = $field188.app(("on a non-object in the lookup expression at"));
}
break;
case 4: var anf_array_val192 = $ans181;
var anf_arg189 = G(self180,"loc",L[64]);
$step178 = 5;
$al182 = L[69];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al182,draw$and$highlight33.$var);
}
$ans181 = draw$and$highlight33.$var.app(anf_arg189);
break;
case 5: var anf_array_val193 = $ans181;
var anf_arg195 = [anf_array_val190,anf_array_val191,anf_array_val192,anf_array_val193];
$step178 = 6;
$al182 = L[58];
$field196 = R.getColonFieldLoc(anf_method_obj194,"make",L[58]);
if(R.isMethod($field196)) {
$ans181 = $field196.full_meth(anf_method_obj194,anf_arg195);
} else {
if(!(R.isFunction($field196))) {
R.ffi.throwNonFunApp(L[58],$field196);
}
$ans181 = $field196.app(anf_arg195);
}
break;
case 6: var anf_array_val204 = $ans181;
var anf_method_obj199 = G(ED12,"para",L[65]);
$step178 = 7;
$al182 = L[66];
$field197 = R.getColonFieldLoc(ED12,"text",L[66]);
if(R.isMethod($field197)) {
$ans181 = $field197.full_meth(ED12,("The non-object was:"));
} else {
if(!(R.isFunction($field197))) {
R.ffi.throwNonFunApp(L[66],$field197);
}
$ans181 = $field197.app(("The non-object was:"));
}
break;
case 7: var anf_array_val198 = $ans181;
var anf_arg200 = [anf_array_val198];
$step178 = 8;
$al182 = L[65];
$field201 = R.getColonFieldLoc(anf_method_obj199,"make",L[65]);
if(R.isMethod($field201)) {
$ans181 = $field201.full_meth(anf_method_obj199,anf_arg200);
} else {
if(!(R.isFunction($field201))) {
R.ffi.throwNonFunApp(L[65],$field201);
}
$ans181 = $field201.app(anf_arg200);
}
break;
case 8: var anf_array_val205 = $ans181;
var anf_arg202 = G(self180,"non-obj",L[67]);
$step178 = 9;
$al182 = L[68];
$field203 = R.getColonFieldLoc(ED12,"embed",L[68]);
if(R.isMethod($field203)) {
$ans181 = $field203.full_meth(ED12,anf_arg202);
} else {
if(!(R.isFunction($field203))) {
R.ffi.throwNonFunApp(L[68],$field203);
}
$ans181 = $field203.app(anf_arg202);
}
break;
case 9: var anf_array_val206 = $ans181;
var anf_arg208 = [anf_array_val204,anf_array_val205,anf_array_val206];
$step178 = 10;
$al182 = L[57];
$field209 = R.getColonFieldLoc(anf_method_obj207,"make",L[57]);
if(R.isMethod($field209)) {
$ans181 = $field209.full_meth(anf_method_obj207,anf_arg208);
} else {
if(!(R.isFunction($field209))) {
R.ffi.throwNonFunApp(L[57],$field209);
}
$ans181 = $field209.app(anf_arg208);
}
break;
case 10: ++R.GAS;
return $ans181;
default: throw "No case numbered " + $step178 + " in $temp_full179";
}
}
} catch($e210) {
if(R.isCont($e210) && ($step178 !== 10)) {
$e210.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al182,$temp_full179,$step178,[self180],[anf_array_val205,anf_method_obj199,anf_array_val204,anf_array_val192,anf_array_val191,anf_array_val190,anf_method_obj194,anf_method_obj207]);
}
if(R.isPyretException($e210)) {
$e210.pyretStack.push($al182);
}
throw $e210;
}
};
var anf_variant_member1062 = R.makeMethod0($temp_full179);
var $temp_full212 = function($self213) {
var $step211 = 0;
var $ans214 = D;
var $al215 = L[80];
try {
if(R.isActivationRecord($self213)) {
$step211 = $self213.step;
$al215 = $self213.from;
$ans214 = $self213.ans;
self213 = $self213.args[0];
anf_array_val231 = $self213.vars[0];
anf_method_obj225 = $self213.vars[1];
anf_array_val230 = $self213.vars[2];
anf_array_val218 = $self213.vars[3];
anf_method_obj220 = $self213.vars[4];
anf_method_obj233 = $self213.vars[5];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[80],1,$t);
}
var self213 = $self213;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step211) {
case 0: var anf_method_obj233 = G(ED12,"error",L[71]);
var anf_method_obj220 = G(ED12,"para",L[72]);
$step211 = 1;
$al215 = L[73];
$field216 = R.getColonFieldLoc(ED12,"text",L[73]);
if(R.isMethod($field216)) {
$ans214 = $field216.full_meth(ED12,("Tried to extend a non-object in the expression at"));
} else {
if(!(R.isFunction($field216))) {
R.ffi.throwNonFunApp(L[73],$field216);
}
$ans214 = $field216.app(("Tried to extend a non-object in the expression at"));
}
break;
case 1: var anf_array_val218 = $ans214;
var anf_arg217 = G(self213,"loc",L[74]);
$step211 = 2;
$al215 = L[79];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al215,draw$and$highlight33.$var);
}
$ans214 = draw$and$highlight33.$var.app(anf_arg217);
break;
case 2: var anf_array_val219 = $ans214;
var anf_arg221 = [anf_array_val218,anf_array_val219];
$step211 = 3;
$al215 = L[72];
$field222 = R.getColonFieldLoc(anf_method_obj220,"make",L[72]);
if(R.isMethod($field222)) {
$ans214 = $field222.full_meth(anf_method_obj220,anf_arg221);
} else {
if(!(R.isFunction($field222))) {
R.ffi.throwNonFunApp(L[72],$field222);
}
$ans214 = $field222.app(anf_arg221);
}
break;
case 3: var anf_array_val230 = $ans214;
var anf_method_obj225 = G(ED12,"para",L[75]);
$step211 = 4;
$al215 = L[76];
$field223 = R.getColonFieldLoc(ED12,"text",L[76]);
if(R.isMethod($field223)) {
$ans214 = $field223.full_meth(ED12,("The non-object was:"));
} else {
if(!(R.isFunction($field223))) {
R.ffi.throwNonFunApp(L[76],$field223);
}
$ans214 = $field223.app(("The non-object was:"));
}
break;
case 4: var anf_array_val224 = $ans214;
var anf_arg226 = [anf_array_val224];
$step211 = 5;
$al215 = L[75];
$field227 = R.getColonFieldLoc(anf_method_obj225,"make",L[75]);
if(R.isMethod($field227)) {
$ans214 = $field227.full_meth(anf_method_obj225,anf_arg226);
} else {
if(!(R.isFunction($field227))) {
R.ffi.throwNonFunApp(L[75],$field227);
}
$ans214 = $field227.app(anf_arg226);
}
break;
case 5: var anf_array_val231 = $ans214;
var anf_arg228 = G(self213,"non-obj",L[77]);
$step211 = 6;
$al215 = L[78];
$field229 = R.getColonFieldLoc(ED12,"embed",L[78]);
if(R.isMethod($field229)) {
$ans214 = $field229.full_meth(ED12,anf_arg228);
} else {
if(!(R.isFunction($field229))) {
R.ffi.throwNonFunApp(L[78],$field229);
}
$ans214 = $field229.app(anf_arg228);
}
break;
case 6: var anf_array_val232 = $ans214;
var anf_arg234 = [anf_array_val230,anf_array_val231,anf_array_val232];
$step211 = 7;
$al215 = L[71];
$field235 = R.getColonFieldLoc(anf_method_obj233,"make",L[71]);
if(R.isMethod($field235)) {
$ans214 = $field235.full_meth(anf_method_obj233,anf_arg234);
} else {
if(!(R.isFunction($field235))) {
R.ffi.throwNonFunApp(L[71],$field235);
}
$ans214 = $field235.app(anf_arg234);
}
break;
case 7: ++R.GAS;
return $ans214;
default: throw "No case numbered " + $step211 + " in $temp_full212";
}
}
} catch($e236) {
if(R.isCont($e236) && ($step211 !== 7)) {
$e236.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al215,$temp_full212,$step211,[self213],[anf_array_val231,anf_method_obj225,anf_array_val230,anf_array_val218,anf_method_obj220,anf_method_obj233]);
}
if(R.isPyretException($e236)) {
$e236.pyretStack.push($al215);
}
throw $e236;
}
};
var anf_variant_member1074 = R.makeMethod0($temp_full212);
var $temp_full238 = function($self239) {
var $step237 = 0;
var $ans240 = D;
var $al241 = L[98];
try {
if(R.isActivationRecord($self239)) {
$step237 = $self239.step;
$al241 = $self239.from;
$ans240 = $self239.ans;
self239 = $self239.args[0];
anf_array_val270 = $self239.vars[0];
anf_array_val263 = $self239.vars[1];
anf_array_val262 = $self239.vars[2];
anf_array_val261 = $self239.vars[3];
anf_array_val260 = $self239.vars[4];
anf_array_val259 = $self239.vars[5];
anf_array_val258 = $self239.vars[6];
anf_array_val257 = $self239.vars[7];
anf_array_val256 = $self239.vars[8];
anf_method_obj265 = $self239.vars[9];
anf_method_obj272 = $self239.vars[10];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[98],1,$t);
}
var self239 = $self239;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step237) {
case 0: var anf_method_obj272 = G(ED12,"error",L[81]);
var anf_method_obj265 = G(ED12,"para",L[82]);
$step237 = 1;
$al241 = L[83];
$field242 = R.getColonFieldLoc(ED12,"text",L[83]);
if(R.isMethod($field242)) {
$ans240 = $field242.full_meth(ED12,("Expected"));
} else {
if(!(R.isFunction($field242))) {
R.ffi.throwNonFunApp(L[83],$field242);
}
$ans240 = $field242.app(("Expected"));
}
break;
case 1: var anf_array_val256 = $ans240;
$step237 = 2;
$al241 = L[84];
$field243 = R.getColonFieldLoc(ED12,"text",L[84]);
if(R.isMethod($field243)) {
$ans240 = $field243.full_meth(ED12,("true"));
} else {
if(!(R.isFunction($field243))) {
R.ffi.throwNonFunApp(L[84],$field243);
}
$ans240 = $field243.app(("true"));
}
break;
case 2: var anf_arg244 = $ans240;
$step237 = 3;
$al241 = L[85];
$field245 = R.getColonFieldLoc(ED12,"code",L[85]);
if(R.isMethod($field245)) {
$ans240 = $field245.full_meth(ED12,anf_arg244);
} else {
if(!(R.isFunction($field245))) {
R.ffi.throwNonFunApp(L[85],$field245);
}
$ans240 = $field245.app(anf_arg244);
}
break;
case 3: var anf_array_val257 = $ans240;
$step237 = 4;
$al241 = L[86];
$field246 = R.getColonFieldLoc(ED12,"text",L[86]);
if(R.isMethod($field246)) {
$ans240 = $field246.full_meth(ED12,("or"));
} else {
if(!(R.isFunction($field246))) {
R.ffi.throwNonFunApp(L[86],$field246);
}
$ans240 = $field246.app(("or"));
}
break;
case 4: var anf_array_val258 = $ans240;
$step237 = 5;
$al241 = L[87];
$field247 = R.getColonFieldLoc(ED12,"text",L[87]);
if(R.isMethod($field247)) {
$ans240 = $field247.full_meth(ED12,("false"));
} else {
if(!(R.isFunction($field247))) {
R.ffi.throwNonFunApp(L[87],$field247);
}
$ans240 = $field247.app(("false"));
}
break;
case 5: var anf_arg248 = $ans240;
$step237 = 6;
$al241 = L[88];
$field249 = R.getColonFieldLoc(ED12,"code",L[88]);
if(R.isMethod($field249)) {
$ans240 = $field249.full_meth(ED12,anf_arg248);
} else {
if(!(R.isFunction($field249))) {
R.ffi.throwNonFunApp(L[88],$field249);
}
$ans240 = $field249.app(anf_arg248);
}
break;
case 6: var anf_array_val259 = $ans240;
$step237 = 7;
$al241 = L[89];
$field250 = R.getColonFieldLoc(ED12,"text",L[89]);
if(R.isMethod($field250)) {
$ans240 = $field250.full_meth(ED12,("for the test in the"));
} else {
if(!(R.isFunction($field250))) {
R.ffi.throwNonFunApp(L[89],$field250);
}
$ans240 = $field250.app(("for the test in the"));
}
break;
case 7: var anf_array_val260 = $ans240;
var anf_arg251 = G(self239,"typ",L[90]);
$step237 = 8;
$al241 = L[91];
$field252 = R.getColonFieldLoc(ED12,"text",L[91]);
if(R.isMethod($field252)) {
$ans240 = $field252.full_meth(ED12,anf_arg251);
} else {
if(!(R.isFunction($field252))) {
R.ffi.throwNonFunApp(L[91],$field252);
}
$ans240 = $field252.app(anf_arg251);
}
break;
case 8: var anf_array_val261 = $ans240;
$step237 = 9;
$al241 = L[92];
$field253 = R.getColonFieldLoc(ED12,"text",L[92]);
if(R.isMethod($field253)) {
$ans240 = $field253.full_meth(ED12,("expression at"));
} else {
if(!(R.isFunction($field253))) {
R.ffi.throwNonFunApp(L[92],$field253);
}
$ans240 = $field253.app(("expression at"));
}
break;
case 9: var anf_array_val262 = $ans240;
var anf_arg254 = G(self239,"loc",L[93]);
$step237 = 10;
$al241 = L[97];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al241,draw$and$highlight33.$var);
}
$ans240 = draw$and$highlight33.$var.app(anf_arg254);
break;
case 10: var anf_array_val263 = $ans240;
$step237 = 11;
$al241 = L[94];
$field255 = R.getColonFieldLoc(ED12,"text",L[94]);
if(R.isMethod($field255)) {
$ans240 = $field255.full_meth(ED12,(" but got:"));
} else {
if(!(R.isFunction($field255))) {
R.ffi.throwNonFunApp(L[94],$field255);
}
$ans240 = $field255.app((" but got:"));
}
break;
case 11: var anf_array_val264 = $ans240;
var anf_arg266 = [anf_array_val256,anf_array_val257,anf_array_val258,anf_array_val259,anf_array_val260,anf_array_val261,anf_array_val262,anf_array_val263,anf_array_val264];
$step237 = 12;
$al241 = L[82];
$field267 = R.getColonFieldLoc(anf_method_obj265,"make",L[82]);
if(R.isMethod($field267)) {
$ans240 = $field267.full_meth(anf_method_obj265,anf_arg266);
} else {
if(!(R.isFunction($field267))) {
R.ffi.throwNonFunApp(L[82],$field267);
}
$ans240 = $field267.app(anf_arg266);
}
break;
case 12: var anf_array_val270 = $ans240;
var anf_arg268 = G(self239,"value",L[95]);
$step237 = 13;
$al241 = L[96];
$field269 = R.getColonFieldLoc(ED12,"embed",L[96]);
if(R.isMethod($field269)) {
$ans240 = $field269.full_meth(ED12,anf_arg268);
} else {
if(!(R.isFunction($field269))) {
R.ffi.throwNonFunApp(L[96],$field269);
}
$ans240 = $field269.app(anf_arg268);
}
break;
case 13: var anf_array_val271 = $ans240;
var anf_arg273 = [anf_array_val270,anf_array_val271];
$step237 = 14;
$al241 = L[81];
$field274 = R.getColonFieldLoc(anf_method_obj272,"make",L[81]);
if(R.isMethod($field274)) {
$ans240 = $field274.full_meth(anf_method_obj272,anf_arg273);
} else {
if(!(R.isFunction($field274))) {
R.ffi.throwNonFunApp(L[81],$field274);
}
$ans240 = $field274.app(anf_arg273);
}
break;
case 14: ++R.GAS;
return $ans240;
default: throw "No case numbered " + $step237 + " in $temp_full238";
}
}
} catch($e275) {
if(R.isCont($e275) && ($step237 !== 14)) {
$e275.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al241,$temp_full238,$step237,[self239],[anf_array_val270,anf_array_val263,anf_array_val262,anf_array_val261,anf_array_val260,anf_array_val259,anf_array_val258,anf_array_val257,anf_array_val256,anf_method_obj265,anf_method_obj272]);
}
if(R.isPyretException($e275)) {
$e275.pyretStack.push($al241);
}
throw $e275;
}
};
var anf_variant_member1085 = R.makeMethod0($temp_full238);
var $temp_full277 = function($self278) {
var $step276 = 0;
var $ans279 = D;
var $al280 = L[119];
try {
if(R.isActivationRecord($self278)) {
$step276 = $self278.step;
$al280 = $self278.from;
$ans279 = $self278.ans;
self278 = $self278.args[0];
anf_array_val314 = $self278.vars[0];
anf_array_val307 = $self278.vars[1];
anf_array_val306 = $self278.vars[2];
anf_array_val305 = $self278.vars[3];
anf_array_val304 = $self278.vars[4];
anf_array_val303 = $self278.vars[5];
anf_array_val302 = $self278.vars[6];
anf_array_val301 = $self278.vars[7];
anf_array_val300 = $self278.vars[8];
anf_array_val299 = $self278.vars[9];
anf_array_val298 = $self278.vars[10];
anf_method_obj309 = $self278.vars[11];
anf_method_obj316 = $self278.vars[12];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[119],1,$t);
}
var self278 = $self278;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step276) {
case 0: var anf_method_obj316 = G(ED12,"error",L[99]);
var anf_method_obj309 = G(ED12,"para",L[100]);
$step276 = 1;
$al280 = L[101];
$field281 = R.getColonFieldLoc(ED12,"text",L[101]);
if(R.isMethod($field281)) {
$ans279 = $field281.full_meth(ED12,("Expected"));
} else {
if(!(R.isFunction($field281))) {
R.ffi.throwNonFunApp(L[101],$field281);
}
$ans279 = $field281.app(("Expected"));
}
break;
case 1: var anf_array_val298 = $ans279;
$step276 = 2;
$al280 = L[102];
$field282 = R.getColonFieldLoc(ED12,"text",L[102]);
if(R.isMethod($field282)) {
$ans279 = $field282.full_meth(ED12,("true"));
} else {
if(!(R.isFunction($field282))) {
R.ffi.throwNonFunApp(L[102],$field282);
}
$ans279 = $field282.app(("true"));
}
break;
case 2: var anf_arg283 = $ans279;
$step276 = 3;
$al280 = L[103];
$field284 = R.getColonFieldLoc(ED12,"code",L[103]);
if(R.isMethod($field284)) {
$ans279 = $field284.full_meth(ED12,anf_arg283);
} else {
if(!(R.isFunction($field284))) {
R.ffi.throwNonFunApp(L[103],$field284);
}
$ans279 = $field284.app(anf_arg283);
}
break;
case 3: var anf_array_val299 = $ans279;
$step276 = 4;
$al280 = L[104];
$field285 = R.getColonFieldLoc(ED12,"text",L[104]);
if(R.isMethod($field285)) {
$ans279 = $field285.full_meth(ED12,("or"));
} else {
if(!(R.isFunction($field285))) {
R.ffi.throwNonFunApp(L[104],$field285);
}
$ans279 = $field285.app(("or"));
}
break;
case 4: var anf_array_val300 = $ans279;
$step276 = 5;
$al280 = L[105];
$field286 = R.getColonFieldLoc(ED12,"text",L[105]);
if(R.isMethod($field286)) {
$ans279 = $field286.full_meth(ED12,("false"));
} else {
if(!(R.isFunction($field286))) {
R.ffi.throwNonFunApp(L[105],$field286);
}
$ans279 = $field286.app(("false"));
}
break;
case 5: var anf_arg287 = $ans279;
$step276 = 6;
$al280 = L[106];
$field288 = R.getColonFieldLoc(ED12,"code",L[106]);
if(R.isMethod($field288)) {
$ans279 = $field288.full_meth(ED12,anf_arg287);
} else {
if(!(R.isFunction($field288))) {
R.ffi.throwNonFunApp(L[106],$field288);
}
$ans279 = $field288.app(anf_arg287);
}
break;
case 6: var anf_array_val301 = $ans279;
$step276 = 7;
$al280 = L[107];
$field289 = R.getColonFieldLoc(ED12,"text",L[107]);
if(R.isMethod($field289)) {
$ans279 = $field289.full_meth(ED12,("for the"));
} else {
if(!(R.isFunction($field289))) {
R.ffi.throwNonFunApp(L[107],$field289);
}
$ans279 = $field289.app(("for the"));
}
break;
case 7: var anf_array_val302 = $ans279;
var anf_arg290 = G(self278,"position",L[108]);
$step276 = 8;
$al280 = L[109];
$field291 = R.getColonFieldLoc(ED12,"text",L[109]);
if(R.isMethod($field291)) {
$ans279 = $field291.full_meth(ED12,anf_arg290);
} else {
if(!(R.isFunction($field291))) {
R.ffi.throwNonFunApp(L[109],$field291);
}
$ans279 = $field291.app(anf_arg290);
}
break;
case 8: var anf_array_val303 = $ans279;
$step276 = 9;
$al280 = L[110];
$field292 = R.getColonFieldLoc(ED12,"text",L[110]);
if(R.isMethod($field292)) {
$ans279 = $field292.full_meth(ED12,("argument in the"));
} else {
if(!(R.isFunction($field292))) {
R.ffi.throwNonFunApp(L[110],$field292);
}
$ans279 = $field292.app(("argument in the"));
}
break;
case 9: var anf_array_val304 = $ans279;
var anf_arg293 = G(self278,"typ",L[111]);
$step276 = 10;
$al280 = L[112];
$field294 = R.getColonFieldLoc(ED12,"text",L[112]);
if(R.isMethod($field294)) {
$ans279 = $field294.full_meth(ED12,anf_arg293);
} else {
if(!(R.isFunction($field294))) {
R.ffi.throwNonFunApp(L[112],$field294);
}
$ans279 = $field294.app(anf_arg293);
}
break;
case 10: var anf_array_val305 = $ans279;
$step276 = 11;
$al280 = L[113];
$field295 = R.getColonFieldLoc(ED12,"text",L[113]);
if(R.isMethod($field295)) {
$ans279 = $field295.full_meth(ED12,("expression at"));
} else {
if(!(R.isFunction($field295))) {
R.ffi.throwNonFunApp(L[113],$field295);
}
$ans279 = $field295.app(("expression at"));
}
break;
case 11: var anf_array_val306 = $ans279;
var anf_arg296 = G(self278,"loc",L[114]);
$step276 = 12;
$al280 = L[118];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al280,draw$and$highlight33.$var);
}
$ans279 = draw$and$highlight33.$var.app(anf_arg296);
break;
case 12: var anf_array_val307 = $ans279;
$step276 = 13;
$al280 = L[115];
$field297 = R.getColonFieldLoc(ED12,"text",L[115]);
if(R.isMethod($field297)) {
$ans279 = $field297.full_meth(ED12,(" but got:"));
} else {
if(!(R.isFunction($field297))) {
R.ffi.throwNonFunApp(L[115],$field297);
}
$ans279 = $field297.app((" but got:"));
}
break;
case 13: var anf_array_val308 = $ans279;
var anf_arg310 = [anf_array_val298,anf_array_val299,anf_array_val300,anf_array_val301,anf_array_val302,anf_array_val303,anf_array_val304,anf_array_val305,anf_array_val306,anf_array_val307,anf_array_val308];
$step276 = 14;
$al280 = L[100];
$field311 = R.getColonFieldLoc(anf_method_obj309,"make",L[100]);
if(R.isMethod($field311)) {
$ans279 = $field311.full_meth(anf_method_obj309,anf_arg310);
} else {
if(!(R.isFunction($field311))) {
R.ffi.throwNonFunApp(L[100],$field311);
}
$ans279 = $field311.app(anf_arg310);
}
break;
case 14: var anf_array_val314 = $ans279;
var anf_arg312 = G(self278,"value",L[116]);
$step276 = 15;
$al280 = L[117];
$field313 = R.getColonFieldLoc(ED12,"embed",L[117]);
if(R.isMethod($field313)) {
$ans279 = $field313.full_meth(ED12,anf_arg312);
} else {
if(!(R.isFunction($field313))) {
R.ffi.throwNonFunApp(L[117],$field313);
}
$ans279 = $field313.app(anf_arg312);
}
break;
case 15: var anf_array_val315 = $ans279;
var anf_arg317 = [anf_array_val314,anf_array_val315];
$step276 = 16;
$al280 = L[99];
$field318 = R.getColonFieldLoc(anf_method_obj316,"make",L[99]);
if(R.isMethod($field318)) {
$ans279 = $field318.full_meth(anf_method_obj316,anf_arg317);
} else {
if(!(R.isFunction($field318))) {
R.ffi.throwNonFunApp(L[99],$field318);
}
$ans279 = $field318.app(anf_arg317);
}
break;
case 16: ++R.GAS;
return $ans279;
default: throw "No case numbered " + $step276 + " in $temp_full277";
}
}
} catch($e319) {
if(R.isCont($e319) && ($step276 !== 16)) {
$e319.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al280,$temp_full277,$step276,[self278],[anf_array_val314,anf_array_val307,anf_array_val306,anf_array_val305,anf_array_val304,anf_array_val303,anf_array_val302,anf_array_val301,anf_array_val300,anf_array_val299,anf_array_val298,anf_method_obj309,anf_method_obj316]);
}
if(R.isPyretException($e319)) {
$e319.pyretStack.push($al280);
}
throw $e319;
}
};
var anf_variant_member1097 = R.makeMethod0($temp_full277);
var $temp_full321 = function($self322) {
var $step320 = 0;
var $ans323 = D;
var $al324 = L[141];
try {
if(R.isActivationRecord($self322)) {
$step320 = $self322.step;
$al324 = $self322.from;
$ans323 = $self322.ans;
self322 = $self322.args[0];
anf_array_val363 = $self322.vars[0];
anf_array_val362 = $self322.vars[1];
anf_array_val361 = $self322.vars[2];
anf_method_obj365 = $self322.vars[3];
anf_method_obj369 = $self322.vars[4];
anf_arg372 = $self322.vars[5];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[141],1,$t);
}
var self322 = $self322;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step320) {
case 0: var $temp_lam326 = function($loc327) {
var $step325 = 0;
var $ans328 = D;
var $al329 = L[131];
try {
if(R.isActivationRecord($loc327)) {
$step325 = $loc327.step;
$al329 = $loc327.from;
$ans328 = $loc327.ans;
loc327 = $loc327.args[0];
anf_array_val343 = $loc327.vars[0];
anf_method_obj345 = $loc327.vars[1];
anf_array_val349 = $loc327.vars[2];
anf_array_val348 = $loc327.vars[3];
anf_array_val335 = $loc327.vars[4];
anf_array_val334 = $loc327.vars[5];
anf_method_obj337 = $loc327.vars[6];
anf_method_obj351 = $loc327.vars[7];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[131],1,$t);
}
var loc327 = $loc327;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step325) {
case 0: var anf_method_obj351 = G(ED12,"error",L[120]);
var anf_method_obj337 = G(ED12,"para",L[121]);
$step325 = 1;
$al329 = L[122];
$field330 = R.getColonFieldLoc(ED12,"text",L[122]);
if(R.isMethod($field330)) {
$ans328 = $field330.full_meth(ED12,("Expected to get a"));
} else {
if(!(R.isFunction($field330))) {
R.ffi.throwNonFunApp(L[122],$field330);
}
$ans328 = $field330.app(("Expected to get a"));
}
break;
case 1: var anf_array_val334 = $ans328;
var anf_arg331 = G(self322,"typ",L[123]);
$step325 = 2;
$al329 = L[124];
$field332 = R.getColonFieldLoc(ED12,"embed",L[124]);
if(R.isMethod($field332)) {
$ans328 = $field332.full_meth(ED12,anf_arg331);
} else {
if(!(R.isFunction($field332))) {
R.ffi.throwNonFunApp(L[124],$field332);
}
$ans328 = $field332.app(anf_arg331);
}
break;
case 2: var anf_array_val335 = $ans328;
$step325 = 3;
$al329 = L[125];
$field333 = R.getColonFieldLoc(ED12,"text",L[125]);
if(R.isMethod($field333)) {
$ans328 = $field333.full_meth(ED12,("as an argument, but got this instead:"));
} else {
if(!(R.isFunction($field333))) {
R.ffi.throwNonFunApp(L[125],$field333);
}
$ans328 = $field333.app(("as an argument, but got this instead:"));
}
break;
case 3: var anf_array_val336 = $ans328;
var anf_arg338 = [anf_array_val334,anf_array_val335,anf_array_val336];
$step325 = 4;
$al329 = L[121];
$field339 = R.getColonFieldLoc(anf_method_obj337,"make",L[121]);
if(R.isMethod($field339)) {
$ans328 = $field339.full_meth(anf_method_obj337,anf_arg338);
} else {
if(!(R.isFunction($field339))) {
R.ffi.throwNonFunApp(L[121],$field339);
}
$ans328 = $field339.app(anf_arg338);
}
break;
case 4: var anf_array_val348 = $ans328;
var anf_arg340 = G(self322,"val",L[126]);
$step325 = 5;
$al329 = L[127];
$field341 = R.getColonFieldLoc(ED12,"embed",L[127]);
if(R.isMethod($field341)) {
$ans328 = $field341.full_meth(ED12,anf_arg340);
} else {
if(!(R.isFunction($field341))) {
R.ffi.throwNonFunApp(L[127],$field341);
}
$ans328 = $field341.app(anf_arg340);
}
break;
case 5: var anf_array_val349 = $ans328;
var anf_method_obj345 = G(ED12,"para",L[128]);
$step325 = 6;
$al329 = L[129];
$field342 = R.getColonFieldLoc(ED12,"text",L[129]);
if(R.isMethod($field342)) {
$ans328 = $field342.full_meth(ED12,("at"));
} else {
if(!(R.isFunction($field342))) {
R.ffi.throwNonFunApp(L[129],$field342);
}
$ans328 = $field342.app(("at"));
}
break;
case 6: var anf_array_val343 = $ans328;
$step325 = 7;
$al329 = L[130];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al329,draw$and$highlight33.$var);
}
$ans328 = draw$and$highlight33.$var.app(loc327);
break;
case 7: var anf_array_val344 = $ans328;
var anf_arg346 = [anf_array_val343,anf_array_val344];
$step325 = 8;
$al329 = L[128];
$field347 = R.getColonFieldLoc(anf_method_obj345,"make",L[128]);
if(R.isMethod($field347)) {
$ans328 = $field347.full_meth(anf_method_obj345,anf_arg346);
} else {
if(!(R.isFunction($field347))) {
R.ffi.throwNonFunApp(L[128],$field347);
}
$ans328 = $field347.app(anf_arg346);
}
break;
case 8: var anf_array_val350 = $ans328;
var anf_arg352 = [anf_array_val348,anf_array_val349,anf_array_val350];
$step325 = 9;
$al329 = L[120];
$field353 = R.getColonFieldLoc(anf_method_obj351,"make",L[120]);
if(R.isMethod($field353)) {
$ans328 = $field353.full_meth(anf_method_obj351,anf_arg352);
} else {
if(!(R.isFunction($field353))) {
R.ffi.throwNonFunApp(L[120],$field353);
}
$ans328 = $field353.app(anf_arg352);
}
break;
case 9: ++R.GAS;
return $ans328;
default: throw "No case numbered " + $step325 + " in $temp_lam326";
}
}
} catch($e354) {
if(R.isCont($e354) && ($step325 !== 9)) {
$e354.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al329,$temp_lam326,$step325,[loc327],[anf_array_val343,anf_method_obj345,anf_array_val349,anf_array_val348,anf_array_val335,anf_array_val334,anf_method_obj337,anf_method_obj351]);
}
if(R.isPyretException($e354)) {
$e354.pyretStack.push($al329);
}
throw $e354;
}
};
var anf_arg372 = R.makeFunction($temp_lam326);
var anf_method_obj369 = G(ED12,"error",L[132]);
var anf_method_obj365 = G(ED12,"para-nospace",L[133]);
$step320 = 1;
$al324 = L[134];
$field355 = R.getColonFieldLoc(ED12,"text",L[134]);
if(R.isMethod($field355)) {
$ans323 = $field355.full_meth(ED12,("Expected "));
} else {
if(!(R.isFunction($field355))) {
R.ffi.throwNonFunApp(L[134],$field355);
}
$ans323 = $field355.app(("Expected "));
}
break;
case 1: var anf_array_val361 = $ans323;
var anf_arg356 = G(self322,"typ",L[135]);
$step320 = 2;
$al324 = L[136];
$field357 = R.getColonFieldLoc(ED12,"embed",L[136]);
if(R.isMethod($field357)) {
$ans323 = $field357.full_meth(ED12,anf_arg356);
} else {
if(!(R.isFunction($field357))) {
R.ffi.throwNonFunApp(L[136],$field357);
}
$ans323 = $field357.app(anf_arg356);
}
break;
case 2: var anf_array_val362 = $ans323;
$step320 = 3;
$al324 = L[137];
$field358 = R.getColonFieldLoc(ED12,"text",L[137]);
if(R.isMethod($field358)) {
$ans323 = $field358.full_meth(ED12,(", but got "));
} else {
if(!(R.isFunction($field358))) {
R.ffi.throwNonFunApp(L[137],$field358);
}
$ans323 = $field358.app((", but got "));
}
break;
case 3: var anf_array_val363 = $ans323;
var anf_arg359 = G(self322,"val",L[138]);
$step320 = 4;
$al324 = L[139];
$field360 = R.getColonFieldLoc(ED12,"embed",L[139]);
if(R.isMethod($field360)) {
$ans323 = $field360.full_meth(ED12,anf_arg359);
} else {
if(!(R.isFunction($field360))) {
R.ffi.throwNonFunApp(L[139],$field360);
}
$ans323 = $field360.app(anf_arg359);
}
break;
case 4: var anf_array_val364 = $ans323;
var anf_arg366 = [anf_array_val361,anf_array_val362,anf_array_val363,anf_array_val364];
$step320 = 5;
$al324 = L[133];
$field367 = R.getColonFieldLoc(anf_method_obj365,"make",L[133]);
if(R.isMethod($field367)) {
$ans323 = $field367.full_meth(anf_method_obj365,anf_arg366);
} else {
if(!(R.isFunction($field367))) {
R.ffi.throwNonFunApp(L[133],$field367);
}
$ans323 = $field367.app(anf_arg366);
}
break;
case 5: var anf_array_val368 = $ans323;
var anf_arg370 = [anf_array_val368];
$step320 = 6;
$al324 = L[132];
$field371 = R.getColonFieldLoc(anf_method_obj369,"make",L[132]);
if(R.isMethod($field371)) {
$ans323 = $field371.full_meth(anf_method_obj369,anf_arg370);
} else {
if(!(R.isFunction($field371))) {
R.ffi.throwNonFunApp(L[132],$field371);
}
$ans323 = $field371.app(anf_arg370);
}
break;
case 6: var anf_arg373 = $ans323;
$step320 = 7;
$al324 = L[140];
$field374 = R.getColonFieldLoc(ED12,"maybe-stack-loc",L[140]);
if(R.isMethod($field374)) {
$ans323 = $field374.full_meth(ED12,(0),(true),anf_arg372,anf_arg373);
} else {
if(!(R.isFunction($field374))) {
R.ffi.throwNonFunApp(L[140],$field374);
}
$ans323 = $field374.app((0),(true),anf_arg372,anf_arg373);
}
break;
case 7: ++R.GAS;
return $ans323;
default: throw "No case numbered " + $step320 + " in $temp_full321";
}
}
} catch($e375) {
if(R.isCont($e375) && ($step320 !== 7)) {
$e375.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al324,$temp_full321,$step320,[self322],[anf_array_val363,anf_array_val362,anf_array_val361,anf_method_obj365,anf_method_obj369,anf_arg372]);
}
if(R.isPyretException($e375)) {
$e375.pyretStack.push($al324);
}
throw $e375;
}
};
var anf_variant_member1110 = R.makeMethod0($temp_full321);
var $temp_full377 = function($self378) {
var $step376 = 0;
var $ans379 = D;
var $al380 = L[153];
try {
if(R.isActivationRecord($self378)) {
$step376 = $self378.step;
$al380 = $self378.from;
$ans379 = $self378.ans;
self378 = $self378.args[0];
anf_array_val394 = $self378.vars[0];
anf_array_val393 = $self378.vars[1];
anf_array_val392 = $self378.vars[2];
anf_array_val391 = $self378.vars[3];
anf_array_val390 = $self378.vars[4];
anf_method_obj396 = $self378.vars[5];
anf_method_obj400 = $self378.vars[6];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[153],1,$t);
}
var self378 = $self378;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step376) {
case 0: var anf_method_obj400 = G(ED12,"error",L[142]);
var anf_method_obj396 = G(ED12,"para-nospace",L[143]);
$step376 = 1;
$al380 = L[144];
$field381 = R.getColonFieldLoc(ED12,"text",L[144]);
if(R.isMethod($field381)) {
$ans379 = $field381.full_meth(ED12,("expected a number between "));
} else {
if(!(R.isFunction($field381))) {
R.ffi.throwNonFunApp(L[144],$field381);
}
$ans379 = $field381.app(("expected a number between "));
}
break;
case 1: var anf_array_val390 = $ans379;
var anf_arg382 = G(self378,"low",L[145]);
$step376 = 2;
$al380 = L[146];
$field383 = R.getColonFieldLoc(ED12,"embed",L[146]);
if(R.isMethod($field383)) {
$ans379 = $field383.full_meth(ED12,anf_arg382);
} else {
if(!(R.isFunction($field383))) {
R.ffi.throwNonFunApp(L[146],$field383);
}
$ans379 = $field383.app(anf_arg382);
}
break;
case 2: var anf_array_val391 = $ans379;
$step376 = 3;
$al380 = L[147];
$field384 = R.getColonFieldLoc(ED12,"text",L[147]);
if(R.isMethod($field384)) {
$ans379 = $field384.full_meth(ED12,(" and "));
} else {
if(!(R.isFunction($field384))) {
R.ffi.throwNonFunApp(L[147],$field384);
}
$ans379 = $field384.app((" and "));
}
break;
case 3: var anf_array_val392 = $ans379;
var anf_arg385 = G(self378,"high",L[148]);
$step376 = 4;
$al380 = L[149];
$field386 = R.getColonFieldLoc(ED12,"embed",L[149]);
if(R.isMethod($field386)) {
$ans379 = $field386.full_meth(ED12,anf_arg385);
} else {
if(!(R.isFunction($field386))) {
R.ffi.throwNonFunApp(L[149],$field386);
}
$ans379 = $field386.app(anf_arg385);
}
break;
case 4: var anf_array_val393 = $ans379;
$step376 = 5;
$al380 = L[150];
$field387 = R.getColonFieldLoc(ED12,"text",L[150]);
if(R.isMethod($field387)) {
$ans379 = $field387.full_meth(ED12,(", but got"));
} else {
if(!(R.isFunction($field387))) {
R.ffi.throwNonFunApp(L[150],$field387);
}
$ans379 = $field387.app((", but got"));
}
break;
case 5: var anf_array_val394 = $ans379;
var anf_arg388 = G(self378,"val",L[151]);
$step376 = 6;
$al380 = L[152];
$field389 = R.getColonFieldLoc(ED12,"embed",L[152]);
if(R.isMethod($field389)) {
$ans379 = $field389.full_meth(ED12,anf_arg388);
} else {
if(!(R.isFunction($field389))) {
R.ffi.throwNonFunApp(L[152],$field389);
}
$ans379 = $field389.app(anf_arg388);
}
break;
case 6: var anf_array_val395 = $ans379;
var anf_arg397 = [anf_array_val390,anf_array_val391,anf_array_val392,anf_array_val393,anf_array_val394,anf_array_val395];
$step376 = 7;
$al380 = L[143];
$field398 = R.getColonFieldLoc(anf_method_obj396,"make",L[143]);
if(R.isMethod($field398)) {
$ans379 = $field398.full_meth(anf_method_obj396,anf_arg397);
} else {
if(!(R.isFunction($field398))) {
R.ffi.throwNonFunApp(L[143],$field398);
}
$ans379 = $field398.app(anf_arg397);
}
break;
case 7: var anf_array_val399 = $ans379;
var anf_arg401 = [anf_array_val399];
$step376 = 8;
$al380 = L[142];
$field402 = R.getColonFieldLoc(anf_method_obj400,"make",L[142]);
if(R.isMethod($field402)) {
$ans379 = $field402.full_meth(anf_method_obj400,anf_arg401);
} else {
if(!(R.isFunction($field402))) {
R.ffi.throwNonFunApp(L[142],$field402);
}
$ans379 = $field402.app(anf_arg401);
}
break;
case 8: ++R.GAS;
return $ans379;
default: throw "No case numbered " + $step376 + " in $temp_full377";
}
}
} catch($e403) {
if(R.isCont($e403) && ($step376 !== 8)) {
$e403.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al380,$temp_full377,$step376,[self378],[anf_array_val394,anf_array_val393,anf_array_val392,anf_array_val391,anf_array_val390,anf_method_obj396,anf_method_obj400]);
}
if(R.isPyretException($e403)) {
$e403.pyretStack.push($al380);
}
throw $e403;
}
};
var anf_variant_member1121 = R.makeMethod0($temp_full377);
var $temp_full405 = function($self406) {
var $step404 = 0;
var $ans407 = D;
var $al408 = L[179];
try {
if(R.isActivationRecord($self406)) {
$step404 = $self406.step;
$al408 = $self406.from;
$ans407 = $self406.ans;
self406 = $self406.args[0];
anf_array_val445 = $self406.vars[0];
anf_array_val444 = $self406.vars[1];
anf_method_obj447 = $self406.vars[2];
anf_array_val451 = $self406.vars[3];
anf_array_val450 = $self406.vars[4];
anf_method_obj453 = $self406.vars[5];
anf_array_val459 = $self406.vars[6];
anf_array_val458 = $self406.vars[7];
anf_method_obj430 = $self406.vars[8];
anf_array_val457 = $self406.vars[9];
anf_method_obj424 = $self406.vars[10];
anf_array_val456 = $self406.vars[11];
anf_array_val416 = $self406.vars[12];
anf_array_val415 = $self406.vars[13];
anf_method_obj418 = $self406.vars[14];
anf_method_obj461 = $self406.vars[15];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[179],1,$t);
}
var self406 = $self406;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step404) {
case 0: var anf_method_obj461 = G(ED12,"error",L[154]);
var anf_method_obj418 = G(ED12,"para",L[155]);
$step404 = 1;
$al408 = L[156];
$field409 = R.getColonFieldLoc(ED12,"text",L[156]);
if(R.isMethod($field409)) {
$ans407 = $field409.full_meth(ED12,("Invalid use of"));
} else {
if(!(R.isFunction($field409))) {
R.ffi.throwNonFunApp(L[156],$field409);
}
$ans407 = $field409.app(("Invalid use of"));
}
break;
case 1: var anf_array_val415 = $ans407;
var anf_arg410 = G(self406,"opname",L[157]);
$step404 = 2;
$al408 = L[158];
$field411 = R.getColonFieldLoc(ED12,"text",L[158]);
if(R.isMethod($field411)) {
$ans407 = $field411.full_meth(ED12,anf_arg410);
} else {
if(!(R.isFunction($field411))) {
R.ffi.throwNonFunApp(L[158],$field411);
}
$ans407 = $field411.app(anf_arg410);
}
break;
case 2: var anf_arg412 = $ans407;
$step404 = 3;
$al408 = L[159];
$field413 = R.getColonFieldLoc(ED12,"code",L[159]);
if(R.isMethod($field413)) {
$ans407 = $field413.full_meth(ED12,anf_arg412);
} else {
if(!(R.isFunction($field413))) {
R.ffi.throwNonFunApp(L[159],$field413);
}
$ans407 = $field413.app(anf_arg412);
}
break;
case 3: var anf_array_val416 = $ans407;
$step404 = 4;
$al408 = L[160];
$field414 = R.getColonFieldLoc(ED12,"text",L[160]);
if(R.isMethod($field414)) {
$ans407 = $field414.full_meth(ED12,("for these values:"));
} else {
if(!(R.isFunction($field414))) {
R.ffi.throwNonFunApp(L[160],$field414);
}
$ans407 = $field414.app(("for these values:"));
}
break;
case 4: var anf_array_val417 = $ans407;
var anf_arg419 = [anf_array_val415,anf_array_val416,anf_array_val417];
$step404 = 5;
$al408 = L[155];
$field420 = R.getColonFieldLoc(anf_method_obj418,"make",L[155]);
if(R.isMethod($field420)) {
$ans407 = $field420.full_meth(anf_method_obj418,anf_arg419);
} else {
if(!(R.isFunction($field420))) {
R.ffi.throwNonFunApp(L[155],$field420);
}
$ans407 = $field420.app(anf_arg419);
}
break;
case 5: var anf_array_val456 = $ans407;
var anf_method_obj424 = G(ED12,"para",L[161]);
var anf_arg421 = G(self406,"val1",L[162]);
$step404 = 6;
$al408 = L[163];
$field422 = R.getColonFieldLoc(ED12,"embed",L[163]);
if(R.isMethod($field422)) {
$ans407 = $field422.full_meth(ED12,anf_arg421);
} else {
if(!(R.isFunction($field422))) {
R.ffi.throwNonFunApp(L[163],$field422);
}
$ans407 = $field422.app(anf_arg421);
}
break;
case 6: var anf_array_val423 = $ans407;
var anf_arg425 = [anf_array_val423];
$step404 = 7;
$al408 = L[161];
$field426 = R.getColonFieldLoc(anf_method_obj424,"make",L[161]);
if(R.isMethod($field426)) {
$ans407 = $field426.full_meth(anf_method_obj424,anf_arg425);
} else {
if(!(R.isFunction($field426))) {
R.ffi.throwNonFunApp(L[161],$field426);
}
$ans407 = $field426.app(anf_arg425);
}
break;
case 7: var anf_array_val457 = $ans407;
var anf_method_obj430 = G(ED12,"para",L[164]);
var anf_arg427 = G(self406,"val2",L[165]);
$step404 = 8;
$al408 = L[166];
$field428 = R.getColonFieldLoc(ED12,"embed",L[166]);
if(R.isMethod($field428)) {
$ans407 = $field428.full_meth(ED12,anf_arg427);
} else {
if(!(R.isFunction($field428))) {
R.ffi.throwNonFunApp(L[166],$field428);
}
$ans407 = $field428.app(anf_arg427);
}
break;
case 8: var anf_array_val429 = $ans407;
var anf_arg431 = [anf_array_val429];
$step404 = 9;
$al408 = L[164];
$field432 = R.getColonFieldLoc(anf_method_obj430,"make",L[164]);
if(R.isMethod($field432)) {
$ans407 = $field432.full_meth(anf_method_obj430,anf_arg431);
} else {
if(!(R.isFunction($field432))) {
R.ffi.throwNonFunApp(L[164],$field432);
}
$ans407 = $field432.app(anf_arg431);
}
break;
case 9: var anf_array_val458 = $ans407;
var anf_arg433 = G(self406,"opdesc",L[167]);
$step404 = 10;
$al408 = L[178];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al408,_plus1);
}
$ans407 = _plus1.app(anf_arg433,(" requires:"));
break;
case 10: var anf_arg434 = $ans407;
$step404 = 11;
$al408 = L[168];
$field435 = R.getColonFieldLoc(ED12,"text",L[168]);
if(R.isMethod($field435)) {
$ans407 = $field435.full_meth(ED12,anf_arg434);
} else {
if(!(R.isFunction($field435))) {
R.ffi.throwNonFunApp(L[168],$field435);
}
$ans407 = $field435.app(anf_arg434);
}
break;
case 11: var anf_array_val459 = $ans407;
var anf_method_obj453 = G(ED12,"bulleted",L[169]);
$step404 = 12;
$al408 = L[170];
$field436 = R.getColonFieldLoc(ED12,"text",L[170]);
if(R.isMethod($field436)) {
$ans407 = $field436.full_meth(ED12,("Two numbers,"));
} else {
if(!(R.isFunction($field436))) {
R.ffi.throwNonFunApp(L[170],$field436);
}
$ans407 = $field436.app(("Two numbers,"));
}
break;
case 12: var anf_array_val450 = $ans407;
$step404 = 13;
$al408 = L[171];
$field437 = R.getColonFieldLoc(ED12,"text",L[171]);
if(R.isMethod($field437)) {
$ans407 = $field437.full_meth(ED12,("Two strings, or"));
} else {
if(!(R.isFunction($field437))) {
R.ffi.throwNonFunApp(L[171],$field437);
}
$ans407 = $field437.app(("Two strings, or"));
}
break;
case 13: var anf_array_val451 = $ans407;
var anf_method_obj447 = G(ED12,"para",L[172]);
$step404 = 14;
$al408 = L[173];
$field438 = R.getColonFieldLoc(ED12,"text",L[173]);
if(R.isMethod($field438)) {
$ans407 = $field438.full_meth(ED12,("A left-hand operand that has a"));
} else {
if(!(R.isFunction($field438))) {
R.ffi.throwNonFunApp(L[173],$field438);
}
$ans407 = $field438.app(("A left-hand operand that has a"));
}
break;
case 14: var anf_array_val444 = $ans407;
var anf_arg439 = G(self406,"methodname",L[174]);
$step404 = 15;
$al408 = L[175];
$field440 = R.getColonFieldLoc(ED12,"text",L[175]);
if(R.isMethod($field440)) {
$ans407 = $field440.full_meth(ED12,anf_arg439);
} else {
if(!(R.isFunction($field440))) {
R.ffi.throwNonFunApp(L[175],$field440);
}
$ans407 = $field440.app(anf_arg439);
}
break;
case 15: var anf_arg441 = $ans407;
$step404 = 16;
$al408 = L[176];
$field442 = R.getColonFieldLoc(ED12,"code",L[176]);
if(R.isMethod($field442)) {
$ans407 = $field442.full_meth(ED12,anf_arg441);
} else {
if(!(R.isFunction($field442))) {
R.ffi.throwNonFunApp(L[176],$field442);
}
$ans407 = $field442.app(anf_arg441);
}
break;
case 16: var anf_array_val445 = $ans407;
$step404 = 17;
$al408 = L[177];
$field443 = R.getColonFieldLoc(ED12,"text",L[177]);
if(R.isMethod($field443)) {
$ans407 = $field443.full_meth(ED12,("method"));
} else {
if(!(R.isFunction($field443))) {
R.ffi.throwNonFunApp(L[177],$field443);
}
$ans407 = $field443.app(("method"));
}
break;
case 17: var anf_array_val446 = $ans407;
var anf_arg448 = [anf_array_val444,anf_array_val445,anf_array_val446];
$step404 = 18;
$al408 = L[172];
$field449 = R.getColonFieldLoc(anf_method_obj447,"make",L[172]);
if(R.isMethod($field449)) {
$ans407 = $field449.full_meth(anf_method_obj447,anf_arg448);
} else {
if(!(R.isFunction($field449))) {
R.ffi.throwNonFunApp(L[172],$field449);
}
$ans407 = $field449.app(anf_arg448);
}
break;
case 18: var anf_array_val452 = $ans407;
var anf_arg454 = [anf_array_val450,anf_array_val451,anf_array_val452];
$step404 = 19;
$al408 = L[169];
$field455 = R.getColonFieldLoc(anf_method_obj453,"make",L[169]);
if(R.isMethod($field455)) {
$ans407 = $field455.full_meth(anf_method_obj453,anf_arg454);
} else {
if(!(R.isFunction($field455))) {
R.ffi.throwNonFunApp(L[169],$field455);
}
$ans407 = $field455.app(anf_arg454);
}
break;
case 19: var anf_array_val460 = $ans407;
var anf_arg462 = [anf_array_val456,anf_array_val457,anf_array_val458,anf_array_val459,anf_array_val460];
$step404 = 20;
$al408 = L[154];
$field463 = R.getColonFieldLoc(anf_method_obj461,"make",L[154]);
if(R.isMethod($field463)) {
$ans407 = $field463.full_meth(anf_method_obj461,anf_arg462);
} else {
if(!(R.isFunction($field463))) {
R.ffi.throwNonFunApp(L[154],$field463);
}
$ans407 = $field463.app(anf_arg462);
}
break;
case 20: ++R.GAS;
return $ans407;
default: throw "No case numbered " + $step404 + " in $temp_full405";
}
}
} catch($e464) {
if(R.isCont($e464) && ($step404 !== 20)) {
$e464.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al408,$temp_full405,$step404,[self406],[anf_array_val445,anf_array_val444,anf_method_obj447,anf_array_val451,anf_array_val450,anf_method_obj453,anf_array_val459,anf_array_val458,anf_method_obj430,anf_array_val457,anf_method_obj424,anf_array_val456,anf_array_val416,anf_array_val415,anf_method_obj418,anf_method_obj461]);
}
if(R.isPyretException($e464)) {
$e464.pyretStack.push($al408);
}
throw $e464;
}
};
var anf_variant_member1133 = R.makeMethod0($temp_full405);
var $temp_full466 = function($self467) {
var $step465 = 0;
var $ans468 = D;
var $al469 = L[202];
try {
if(R.isActivationRecord($self467)) {
$step465 = $self467.step;
$al469 = $self467.from;
$ans468 = $self467.ans;
self467 = $self467.args[0];
anf_array_val503 = $self467.vars[0];
anf_array_val502 = $self467.vars[1];
anf_method_obj505 = $self467.vars[2];
anf_array_val508 = $self467.vars[3];
anf_method_obj510 = $self467.vars[4];
anf_array_val516 = $self467.vars[5];
anf_array_val515 = $self467.vars[6];
anf_method_obj491 = $self467.vars[7];
anf_array_val514 = $self467.vars[8];
anf_method_obj485 = $self467.vars[9];
anf_array_val513 = $self467.vars[10];
anf_array_val477 = $self467.vars[11];
anf_array_val476 = $self467.vars[12];
anf_method_obj479 = $self467.vars[13];
anf_method_obj518 = $self467.vars[14];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[202],1,$t);
}
var self467 = $self467;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step465) {
case 0: var anf_method_obj518 = G(ED12,"error",L[180]);
var anf_method_obj479 = G(ED12,"para",L[181]);
$step465 = 1;
$al469 = L[182];
$field470 = R.getColonFieldLoc(ED12,"text",L[182]);
if(R.isMethod($field470)) {
$ans468 = $field470.full_meth(ED12,("Invalid use of"));
} else {
if(!(R.isFunction($field470))) {
R.ffi.throwNonFunApp(L[182],$field470);
}
$ans468 = $field470.app(("Invalid use of"));
}
break;
case 1: var anf_array_val476 = $ans468;
var anf_arg471 = G(self467,"opname",L[183]);
$step465 = 2;
$al469 = L[184];
$field472 = R.getColonFieldLoc(ED12,"text",L[184]);
if(R.isMethod($field472)) {
$ans468 = $field472.full_meth(ED12,anf_arg471);
} else {
if(!(R.isFunction($field472))) {
R.ffi.throwNonFunApp(L[184],$field472);
}
$ans468 = $field472.app(anf_arg471);
}
break;
case 2: var anf_arg473 = $ans468;
$step465 = 3;
$al469 = L[185];
$field474 = R.getColonFieldLoc(ED12,"code",L[185]);
if(R.isMethod($field474)) {
$ans468 = $field474.full_meth(ED12,anf_arg473);
} else {
if(!(R.isFunction($field474))) {
R.ffi.throwNonFunApp(L[185],$field474);
}
$ans468 = $field474.app(anf_arg473);
}
break;
case 3: var anf_array_val477 = $ans468;
$step465 = 4;
$al469 = L[186];
$field475 = R.getColonFieldLoc(ED12,"text",L[186]);
if(R.isMethod($field475)) {
$ans468 = $field475.full_meth(ED12,("for these values:"));
} else {
if(!(R.isFunction($field475))) {
R.ffi.throwNonFunApp(L[186],$field475);
}
$ans468 = $field475.app(("for these values:"));
}
break;
case 4: var anf_array_val478 = $ans468;
var anf_arg480 = [anf_array_val476,anf_array_val477,anf_array_val478];
$step465 = 5;
$al469 = L[181];
$field481 = R.getColonFieldLoc(anf_method_obj479,"make",L[181]);
if(R.isMethod($field481)) {
$ans468 = $field481.full_meth(anf_method_obj479,anf_arg480);
} else {
if(!(R.isFunction($field481))) {
R.ffi.throwNonFunApp(L[181],$field481);
}
$ans468 = $field481.app(anf_arg480);
}
break;
case 5: var anf_array_val513 = $ans468;
var anf_method_obj485 = G(ED12,"para",L[187]);
var anf_arg482 = G(self467,"val1",L[188]);
$step465 = 6;
$al469 = L[189];
$field483 = R.getColonFieldLoc(ED12,"embed",L[189]);
if(R.isMethod($field483)) {
$ans468 = $field483.full_meth(ED12,anf_arg482);
} else {
if(!(R.isFunction($field483))) {
R.ffi.throwNonFunApp(L[189],$field483);
}
$ans468 = $field483.app(anf_arg482);
}
break;
case 6: var anf_array_val484 = $ans468;
var anf_arg486 = [anf_array_val484];
$step465 = 7;
$al469 = L[187];
$field487 = R.getColonFieldLoc(anf_method_obj485,"make",L[187]);
if(R.isMethod($field487)) {
$ans468 = $field487.full_meth(anf_method_obj485,anf_arg486);
} else {
if(!(R.isFunction($field487))) {
R.ffi.throwNonFunApp(L[187],$field487);
}
$ans468 = $field487.app(anf_arg486);
}
break;
case 7: var anf_array_val514 = $ans468;
var anf_method_obj491 = G(ED12,"para",L[190]);
var anf_arg488 = G(self467,"val2",L[191]);
$step465 = 8;
$al469 = L[192];
$field489 = R.getColonFieldLoc(ED12,"embed",L[192]);
if(R.isMethod($field489)) {
$ans468 = $field489.full_meth(ED12,anf_arg488);
} else {
if(!(R.isFunction($field489))) {
R.ffi.throwNonFunApp(L[192],$field489);
}
$ans468 = $field489.app(anf_arg488);
}
break;
case 8: var anf_array_val490 = $ans468;
var anf_arg492 = [anf_array_val490];
$step465 = 9;
$al469 = L[190];
$field493 = R.getColonFieldLoc(anf_method_obj491,"make",L[190]);
if(R.isMethod($field493)) {
$ans468 = $field493.full_meth(anf_method_obj491,anf_arg492);
} else {
if(!(R.isFunction($field493))) {
R.ffi.throwNonFunApp(L[190],$field493);
}
$ans468 = $field493.app(anf_arg492);
}
break;
case 9: var anf_array_val515 = $ans468;
$step465 = 10;
$al469 = L[193];
$field494 = R.getColonFieldLoc(ED12,"text",L[193]);
if(R.isMethod($field494)) {
$ans468 = $field494.full_meth(ED12,("Either:"));
} else {
if(!(R.isFunction($field494))) {
R.ffi.throwNonFunApp(L[193],$field494);
}
$ans468 = $field494.app(("Either:"));
}
break;
case 10: var anf_array_val516 = $ans468;
var anf_method_obj510 = G(ED12,"bulleted",L[194]);
$step465 = 11;
$al469 = L[195];
$field495 = R.getColonFieldLoc(ED12,"text",L[195]);
if(R.isMethod($field495)) {
$ans468 = $field495.full_meth(ED12,("Both arguments must be numbers, or"));
} else {
if(!(R.isFunction($field495))) {
R.ffi.throwNonFunApp(L[195],$field495);
}
$ans468 = $field495.app(("Both arguments must be numbers, or"));
}
break;
case 11: var anf_array_val508 = $ans468;
var anf_method_obj505 = G(ED12,"para",L[196]);
$step465 = 12;
$al469 = L[197];
$field496 = R.getColonFieldLoc(ED12,"text",L[197]);
if(R.isMethod($field496)) {
$ans468 = $field496.full_meth(ED12,("The left operand must have a"));
} else {
if(!(R.isFunction($field496))) {
R.ffi.throwNonFunApp(L[197],$field496);
}
$ans468 = $field496.app(("The left operand must have a"));
}
break;
case 12: var anf_array_val502 = $ans468;
var anf_arg497 = G(self467,"methodname",L[198]);
$step465 = 13;
$al469 = L[199];
$field498 = R.getColonFieldLoc(ED12,"text",L[199]);
if(R.isMethod($field498)) {
$ans468 = $field498.full_meth(ED12,anf_arg497);
} else {
if(!(R.isFunction($field498))) {
R.ffi.throwNonFunApp(L[199],$field498);
}
$ans468 = $field498.app(anf_arg497);
}
break;
case 13: var anf_arg499 = $ans468;
$step465 = 14;
$al469 = L[200];
$field500 = R.getColonFieldLoc(ED12,"code",L[200]);
if(R.isMethod($field500)) {
$ans468 = $field500.full_meth(ED12,anf_arg499);
} else {
if(!(R.isFunction($field500))) {
R.ffi.throwNonFunApp(L[200],$field500);
}
$ans468 = $field500.app(anf_arg499);
}
break;
case 14: var anf_array_val503 = $ans468;
$step465 = 15;
$al469 = L[201];
$field501 = R.getColonFieldLoc(ED12,"text",L[201]);
if(R.isMethod($field501)) {
$ans468 = $field501.full_meth(ED12,("method"));
} else {
if(!(R.isFunction($field501))) {
R.ffi.throwNonFunApp(L[201],$field501);
}
$ans468 = $field501.app(("method"));
}
break;
case 15: var anf_array_val504 = $ans468;
var anf_arg506 = [anf_array_val502,anf_array_val503,anf_array_val504];
$step465 = 16;
$al469 = L[196];
$field507 = R.getColonFieldLoc(anf_method_obj505,"make",L[196]);
if(R.isMethod($field507)) {
$ans468 = $field507.full_meth(anf_method_obj505,anf_arg506);
} else {
if(!(R.isFunction($field507))) {
R.ffi.throwNonFunApp(L[196],$field507);
}
$ans468 = $field507.app(anf_arg506);
}
break;
case 16: var anf_array_val509 = $ans468;
var anf_arg511 = [anf_array_val508,anf_array_val509];
$step465 = 17;
$al469 = L[194];
$field512 = R.getColonFieldLoc(anf_method_obj510,"make",L[194]);
if(R.isMethod($field512)) {
$ans468 = $field512.full_meth(anf_method_obj510,anf_arg511);
} else {
if(!(R.isFunction($field512))) {
R.ffi.throwNonFunApp(L[194],$field512);
}
$ans468 = $field512.app(anf_arg511);
}
break;
case 17: var anf_array_val517 = $ans468;
var anf_arg519 = [anf_array_val513,anf_array_val514,anf_array_val515,anf_array_val516,anf_array_val517];
$step465 = 18;
$al469 = L[180];
$field520 = R.getColonFieldLoc(anf_method_obj518,"make",L[180]);
if(R.isMethod($field520)) {
$ans468 = $field520.full_meth(anf_method_obj518,anf_arg519);
} else {
if(!(R.isFunction($field520))) {
R.ffi.throwNonFunApp(L[180],$field520);
}
$ans468 = $field520.app(anf_arg519);
}
break;
case 18: ++R.GAS;
return $ans468;
default: throw "No case numbered " + $step465 + " in $temp_full466";
}
}
} catch($e521) {
if(R.isCont($e521) && ($step465 !== 18)) {
$e521.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al469,$temp_full466,$step465,[self467],[anf_array_val503,anf_array_val502,anf_method_obj505,anf_array_val508,anf_method_obj510,anf_array_val516,anf_array_val515,anf_method_obj491,anf_array_val514,anf_method_obj485,anf_array_val513,anf_array_val477,anf_array_val476,anf_method_obj479,anf_method_obj518]);
}
if(R.isPyretException($e521)) {
$e521.pyretStack.push($al469);
}
throw $e521;
}
};
var anf_variant_member1147 = R.makeMethod0($temp_full466);
var $temp_full523 = function($self524) {
var $step522 = 0;
var $ans525 = D;
var $al526 = L[243];
try {
if(R.isActivationRecord($self524)) {
$step522 = $self524.step;
$al526 = $self524.from;
$ans525 = $self524.ans;
self524 = $self524.args[0];
anf_array_val581 = $self524.vars[0];
anf_array_val580 = $self524.vars[1];
anf_array_val579 = $self524.vars[2];
anf_array_val578 = $self524.vars[3];
anf_array_val577 = $self524.vars[4];
anf_array_val576 = $self524.vars[5];
anf_array_val575 = $self524.vars[6];
anf_method_obj583 = $self524.vars[7];
anf_array_val552 = $self524.vars[8];
anf_array_val551 = $self524.vars[9];
anf_array_val550 = $self524.vars[10];
anf_array_val549 = $self524.vars[11];
anf_array_val548 = $self524.vars[12];
anf_array_val547 = $self524.vars[13];
anf_array_val546 = $self524.vars[14];
anf_method_obj554 = $self524.vars[15];
anf_method_obj589 = $self524.vars[16];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[243],1,$t);
}
var self524 = $self524;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step522) {
case 0: var anf_method_obj589 = G(ED12,"error",L[203]);
var anf_arg527 = G(self524,"num-args",L[204]);
var anf_arg528 = G(self524,"actual-arity",L[205]);
$step522 = 1;
$al526 = L[206];
if(!(R.isFunction(_lessthan2))) {
R.ffi.throwNonFunApp($al526,_lessthan2);
}
$ans525 = _lessthan2.app(anf_arg527,anf_arg528);
break;
case 1: var anf_arg529 = $ans525;
$al526 = L[206];
var anf_if592 = R.checkWrapBoolean(anf_arg529);
if(R.isPyretTrue(anf_if592)) {
$step522 = 2;
} else {
$step522 = 17;
}
break;
case 2: var anf_method_obj554 = G(ED12,"para",L[207]);
$step522 = 3;
$al526 = L[208];
$field530 = R.getColonFieldLoc(ED12,"text",L[208]);
if(R.isMethod($field530)) {
$ans525 = $field530.full_meth(ED12,("The cases branch at"));
} else {
if(!(R.isFunction($field530))) {
R.ffi.throwNonFunApp(L[208],$field530);
}
$ans525 = $field530.app(("The cases branch at"));
}
break;
case 3: var anf_array_val546 = $ans525;
var anf_arg531 = G(self524,"branch-loc",L[209]);
$step522 = 4;
$al526 = L[224];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al526,draw$and$highlight33.$var);
}
$ans525 = draw$and$highlight33.$var.app(anf_arg531);
break;
case 4: var anf_array_val547 = $ans525;
$step522 = 5;
$al526 = L[210];
$field532 = R.getColonFieldLoc(ED12,"text",L[210]);
if(R.isMethod($field532)) {
$ans525 = $field532.full_meth(ED12,("expects only"));
} else {
if(!(R.isFunction($field532))) {
R.ffi.throwNonFunApp(L[210],$field532);
}
$ans525 = $field532.app(("expects only"));
}
break;
case 5: var anf_array_val548 = $ans525;
var anf_arg533 = G(self524,"num-args",L[211]);
$step522 = 6;
$al526 = L[212];
$field534 = R.getColonFieldLoc(ED12,"embed",L[212]);
if(R.isMethod($field534)) {
$ans525 = $field534.full_meth(ED12,anf_arg533);
} else {
if(!(R.isFunction($field534))) {
R.ffi.throwNonFunApp(L[212],$field534);
}
$ans525 = $field534.app(anf_arg533);
}
break;
case 6: var anf_array_val549 = $ans525;
var anf_arg535 = G(self524,"num-args",L[213]);
$step522 = 7;
$al526 = L[214];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al526,equal$always3);
}
$ans525 = equal$always3.app(anf_arg535,(1));
break;
case 7: var anf_arg536 = $ans525;
$al526 = L[214];
var anf_if558 = R.checkWrapBoolean(anf_arg536);
if(R.isPyretTrue(anf_if558)) {
$step522 = 8;
} else {
$step522 = 9;
}
break;
case 8: $step522 = 10;
$al526 = L[215];
$field537 = R.getColonFieldLoc(ED12,"text",L[215]);
if(R.isMethod($field537)) {
$ans525 = $field537.full_meth(ED12,("argument,"));
} else {
if(!(R.isFunction($field537))) {
R.ffi.throwNonFunApp(L[215],$field537);
}
$ans525 = $field537.app(("argument,"));
}
break;
case 9: $step522 = 10;
$al526 = L[216];
$field538 = R.getColonFieldLoc(ED12,"text",L[216]);
if(R.isMethod($field538)) {
$ans525 = $field538.full_meth(ED12,("arguments,"));
} else {
if(!(R.isFunction($field538))) {
R.ffi.throwNonFunApp(L[216],$field538);
}
$ans525 = $field538.app(("arguments,"));
}
break;
case 10: var anf_array_val550 = $ans525;
$step522 = 11;
$al526 = L[217];
$field539 = R.getColonFieldLoc(ED12,"text",L[217]);
if(R.isMethod($field539)) {
$ans525 = $field539.full_meth(ED12,("but the actual value has"));
} else {
if(!(R.isFunction($field539))) {
R.ffi.throwNonFunApp(L[217],$field539);
}
$ans525 = $field539.app(("but the actual value has"));
}
break;
case 11: var anf_array_val551 = $ans525;
var anf_arg540 = G(self524,"actual-arity",L[218]);
$step522 = 12;
$al526 = L[219];
$field541 = R.getColonFieldLoc(ED12,"embed",L[219]);
if(R.isMethod($field541)) {
$ans525 = $field541.full_meth(ED12,anf_arg540);
} else {
if(!(R.isFunction($field541))) {
R.ffi.throwNonFunApp(L[219],$field541);
}
$ans525 = $field541.app(anf_arg540);
}
break;
case 12: var anf_array_val552 = $ans525;
var anf_arg542 = G(self524,"actual-arity",L[220]);
$step522 = 13;
$al526 = L[221];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al526,equal$always3);
}
$ans525 = equal$always3.app(anf_arg542,(1));
break;
case 13: var anf_arg543 = $ans525;
$al526 = L[221];
var anf_if557 = R.checkWrapBoolean(anf_arg543);
if(R.isPyretTrue(anf_if557)) {
$step522 = 14;
} else {
$step522 = 15;
}
break;
case 14: $step522 = 16;
$al526 = L[222];
$field544 = R.getColonFieldLoc(ED12,"text",L[222]);
if(R.isMethod($field544)) {
$ans525 = $field544.full_meth(ED12,("field"));
} else {
if(!(R.isFunction($field544))) {
R.ffi.throwNonFunApp(L[222],$field544);
}
$ans525 = $field544.app(("field"));
}
break;
case 15: $step522 = 16;
$al526 = L[223];
$field545 = R.getColonFieldLoc(ED12,"text",L[223]);
if(R.isMethod($field545)) {
$ans525 = $field545.full_meth(ED12,("fields"));
} else {
if(!(R.isFunction($field545))) {
R.ffi.throwNonFunApp(L[223],$field545);
}
$ans525 = $field545.app(("fields"));
}
break;
case 16: var anf_array_val553 = $ans525;
var anf_arg555 = [anf_array_val546,anf_array_val547,anf_array_val548,anf_array_val549,anf_array_val550,anf_array_val551,anf_array_val552,anf_array_val553];
$step522 = 32;
$al526 = L[207];
$field556 = R.getColonFieldLoc(anf_method_obj554,"make",L[207]);
if(R.isMethod($field556)) {
$ans525 = $field556.full_meth(anf_method_obj554,anf_arg555);
} else {
if(!(R.isFunction($field556))) {
R.ffi.throwNonFunApp(L[207],$field556);
}
$ans525 = $field556.app(anf_arg555);
}
break;
case 17: var anf_method_obj583 = G(ED12,"para",L[225]);
$step522 = 18;
$al526 = L[226];
$field559 = R.getColonFieldLoc(ED12,"text",L[226]);
if(R.isMethod($field559)) {
$ans525 = $field559.full_meth(ED12,("The cases branch at"));
} else {
if(!(R.isFunction($field559))) {
R.ffi.throwNonFunApp(L[226],$field559);
}
$ans525 = $field559.app(("The cases branch at"));
}
break;
case 18: var anf_array_val575 = $ans525;
var anf_arg560 = G(self524,"branch-loc",L[227]);
$step522 = 19;
$al526 = L[242];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al526,draw$and$highlight33.$var);
}
$ans525 = draw$and$highlight33.$var.app(anf_arg560);
break;
case 19: var anf_array_val576 = $ans525;
$step522 = 20;
$al526 = L[228];
$field561 = R.getColonFieldLoc(ED12,"text",L[228]);
if(R.isMethod($field561)) {
$ans525 = $field561.full_meth(ED12,("expects"));
} else {
if(!(R.isFunction($field561))) {
R.ffi.throwNonFunApp(L[228],$field561);
}
$ans525 = $field561.app(("expects"));
}
break;
case 20: var anf_array_val577 = $ans525;
var anf_arg562 = G(self524,"num-args",L[229]);
$step522 = 21;
$al526 = L[230];
$field563 = R.getColonFieldLoc(ED12,"embed",L[230]);
if(R.isMethod($field563)) {
$ans525 = $field563.full_meth(ED12,anf_arg562);
} else {
if(!(R.isFunction($field563))) {
R.ffi.throwNonFunApp(L[230],$field563);
}
$ans525 = $field563.app(anf_arg562);
}
break;
case 21: var anf_array_val578 = $ans525;
var anf_arg564 = G(self524,"num-args",L[231]);
$step522 = 22;
$al526 = L[232];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al526,equal$always3);
}
$ans525 = equal$always3.app(anf_arg564,(1));
break;
case 22: var anf_arg565 = $ans525;
$al526 = L[232];
var anf_if587 = R.checkWrapBoolean(anf_arg565);
if(R.isPyretTrue(anf_if587)) {
$step522 = 23;
} else {
$step522 = 24;
}
break;
case 23: $step522 = 25;
$al526 = L[233];
$field566 = R.getColonFieldLoc(ED12,"text",L[233]);
if(R.isMethod($field566)) {
$ans525 = $field566.full_meth(ED12,("argument,"));
} else {
if(!(R.isFunction($field566))) {
R.ffi.throwNonFunApp(L[233],$field566);
}
$ans525 = $field566.app(("argument,"));
}
break;
case 24: $step522 = 25;
$al526 = L[234];
$field567 = R.getColonFieldLoc(ED12,"text",L[234]);
if(R.isMethod($field567)) {
$ans525 = $field567.full_meth(ED12,("arguments,"));
} else {
if(!(R.isFunction($field567))) {
R.ffi.throwNonFunApp(L[234],$field567);
}
$ans525 = $field567.app(("arguments,"));
}
break;
case 25: var anf_array_val579 = $ans525;
$step522 = 26;
$al526 = L[235];
$field568 = R.getColonFieldLoc(ED12,"text",L[235]);
if(R.isMethod($field568)) {
$ans525 = $field568.full_meth(ED12,("but the actual value has only"));
} else {
if(!(R.isFunction($field568))) {
R.ffi.throwNonFunApp(L[235],$field568);
}
$ans525 = $field568.app(("but the actual value has only"));
}
break;
case 26: var anf_array_val580 = $ans525;
var anf_arg569 = G(self524,"actual-arity",L[236]);
$step522 = 27;
$al526 = L[237];
$field570 = R.getColonFieldLoc(ED12,"embed",L[237]);
if(R.isMethod($field570)) {
$ans525 = $field570.full_meth(ED12,anf_arg569);
} else {
if(!(R.isFunction($field570))) {
R.ffi.throwNonFunApp(L[237],$field570);
}
$ans525 = $field570.app(anf_arg569);
}
break;
case 27: var anf_array_val581 = $ans525;
var anf_arg571 = G(self524,"actual-arity",L[238]);
$step522 = 28;
$al526 = L[239];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al526,equal$always3);
}
$ans525 = equal$always3.app(anf_arg571,(1));
break;
case 28: var anf_arg572 = $ans525;
$al526 = L[239];
var anf_if586 = R.checkWrapBoolean(anf_arg572);
if(R.isPyretTrue(anf_if586)) {
$step522 = 29;
} else {
$step522 = 30;
}
break;
case 29: $step522 = 31;
$al526 = L[240];
$field573 = R.getColonFieldLoc(ED12,"text",L[240]);
if(R.isMethod($field573)) {
$ans525 = $field573.full_meth(ED12,("field"));
} else {
if(!(R.isFunction($field573))) {
R.ffi.throwNonFunApp(L[240],$field573);
}
$ans525 = $field573.app(("field"));
}
break;
case 30: $step522 = 31;
$al526 = L[241];
$field574 = R.getColonFieldLoc(ED12,"text",L[241]);
if(R.isMethod($field574)) {
$ans525 = $field574.full_meth(ED12,("fields"));
} else {
if(!(R.isFunction($field574))) {
R.ffi.throwNonFunApp(L[241],$field574);
}
$ans525 = $field574.app(("fields"));
}
break;
case 31: var anf_array_val582 = $ans525;
var anf_arg584 = [anf_array_val575,anf_array_val576,anf_array_val577,anf_array_val578,anf_array_val579,anf_array_val580,anf_array_val581,anf_array_val582];
$step522 = 32;
$al526 = L[225];
$field585 = R.getColonFieldLoc(anf_method_obj583,"make",L[225]);
if(R.isMethod($field585)) {
$ans525 = $field585.full_meth(anf_method_obj583,anf_arg584);
} else {
if(!(R.isFunction($field585))) {
R.ffi.throwNonFunApp(L[225],$field585);
}
$ans525 = $field585.app(anf_arg584);
}
break;
case 32: var anf_array_val588 = $ans525;
var anf_arg590 = [anf_array_val588];
$step522 = 33;
$al526 = L[203];
$field591 = R.getColonFieldLoc(anf_method_obj589,"make",L[203]);
if(R.isMethod($field591)) {
$ans525 = $field591.full_meth(anf_method_obj589,anf_arg590);
} else {
if(!(R.isFunction($field591))) {
R.ffi.throwNonFunApp(L[203],$field591);
}
$ans525 = $field591.app(anf_arg590);
}
break;
case 33: ++R.GAS;
return $ans525;
default: throw "No case numbered " + $step522 + " in $temp_full523";
}
}
} catch($e593) {
if(R.isCont($e593) && ($step522 !== 33)) {
$e593.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al526,$temp_full523,$step522,[self524],[anf_array_val581,anf_array_val580,anf_array_val579,anf_array_val578,anf_array_val577,anf_array_val576,anf_array_val575,anf_method_obj583,anf_array_val552,anf_array_val551,anf_array_val550,anf_array_val549,anf_array_val548,anf_array_val547,anf_array_val546,anf_method_obj554,anf_method_obj589]);
}
if(R.isPyretException($e593)) {
$e593.pyretStack.push($al526);
}
throw $e593;
}
};
var anf_variant_member1160 = R.makeMethod0($temp_full523);
var $temp_full595 = function($self596) {
var $step594 = 0;
var $ans597 = D;
var $al598 = L[257];
try {
if(R.isActivationRecord($self596)) {
$step594 = $self596.step;
$al598 = $self596.from;
$ans597 = $self596.ans;
self596 = $self596.args[0];
anf_array_val617 = $self596.vars[0];
anf_array_val616 = $self596.vars[1];
anf_method_obj619 = $self596.vars[2];
anf_method_obj623 = $self596.vars[3];
anf_array_val604 = $self596.vars[4];
anf_array_val603 = $self596.vars[5];
anf_method_obj606 = $self596.vars[6];
anf_method_obj610 = $self596.vars[7];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[257],1,$t);
}
var self596 = $self596;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step594) {
case 0: var anf_arg599 = G(self596,"should-be-singleton",L[244]);
$al598 = L[244];
var anf_if626 = R.checkWrapBoolean(anf_arg599);
if(R.isPyretTrue(anf_if626)) {
$step594 = 1;
} else {
$step594 = 6;
}
break;
case 1: var anf_method_obj610 = G(ED12,"error",L[245]);
var anf_method_obj606 = G(ED12,"para",L[246]);
$step594 = 2;
$al598 = L[247];
$field600 = R.getColonFieldLoc(ED12,"text",L[247]);
if(R.isMethod($field600)) {
$ans597 = $field600.full_meth(ED12,("The cases branch at"));
} else {
if(!(R.isFunction($field600))) {
R.ffi.throwNonFunApp(L[247],$field600);
}
$ans597 = $field600.app(("The cases branch at"));
}
break;
case 2: var anf_array_val603 = $ans597;
var anf_arg601 = G(self596,"branch-loc",L[248]);
$step594 = 3;
$al598 = L[250];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al598,draw$and$highlight33.$var);
}
$ans597 = draw$and$highlight33.$var.app(anf_arg601);
break;
case 3: var anf_array_val604 = $ans597;
$step594 = 4;
$al598 = L[249];
$field602 = R.getColonFieldLoc(ED12,"text",L[249]);
if(R.isMethod($field602)) {
$ans597 = $field602.full_meth(ED12,("has an argument list, but the variant is a singleton."));
} else {
if(!(R.isFunction($field602))) {
R.ffi.throwNonFunApp(L[249],$field602);
}
$ans597 = $field602.app(("has an argument list, but the variant is a singleton."));
}
break;
case 4: var anf_array_val605 = $ans597;
var anf_arg607 = [anf_array_val603,anf_array_val604,anf_array_val605];
$step594 = 5;
$al598 = L[246];
$field608 = R.getColonFieldLoc(anf_method_obj606,"make",L[246]);
if(R.isMethod($field608)) {
$ans597 = $field608.full_meth(anf_method_obj606,anf_arg607);
} else {
if(!(R.isFunction($field608))) {
R.ffi.throwNonFunApp(L[246],$field608);
}
$ans597 = $field608.app(anf_arg607);
}
break;
case 5: var anf_array_val609 = $ans597;
var anf_arg611 = [anf_array_val609];
$step594 = 11;
$al598 = L[245];
$field612 = R.getColonFieldLoc(anf_method_obj610,"make",L[245]);
if(R.isMethod($field612)) {
$ans597 = $field612.full_meth(anf_method_obj610,anf_arg611);
} else {
if(!(R.isFunction($field612))) {
R.ffi.throwNonFunApp(L[245],$field612);
}
$ans597 = $field612.app(anf_arg611);
}
break;
case 6: var anf_method_obj623 = G(ED12,"error",L[251]);
var anf_method_obj619 = G(ED12,"para",L[252]);
$step594 = 7;
$al598 = L[253];
$field613 = R.getColonFieldLoc(ED12,"text",L[253]);
if(R.isMethod($field613)) {
$ans597 = $field613.full_meth(ED12,("The cases branch at"));
} else {
if(!(R.isFunction($field613))) {
R.ffi.throwNonFunApp(L[253],$field613);
}
$ans597 = $field613.app(("The cases branch at"));
}
break;
case 7: var anf_array_val616 = $ans597;
var anf_arg614 = G(self596,"branch-loc",L[254]);
$step594 = 8;
$al598 = L[256];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al598,draw$and$highlight33.$var);
}
$ans597 = draw$and$highlight33.$var.app(anf_arg614);
break;
case 8: var anf_array_val617 = $ans597;
$step594 = 9;
$al598 = L[255];
$field615 = R.getColonFieldLoc(ED12,"text",L[255]);
if(R.isMethod($field615)) {
$ans597 = $field615.full_meth(ED12,("doesn't have an argument list, but the variant is not a singleton."));
} else {
if(!(R.isFunction($field615))) {
R.ffi.throwNonFunApp(L[255],$field615);
}
$ans597 = $field615.app(("doesn't have an argument list, but the variant is not a singleton."));
}
break;
case 9: var anf_array_val618 = $ans597;
var anf_arg620 = [anf_array_val616,anf_array_val617,anf_array_val618];
$step594 = 10;
$al598 = L[252];
$field621 = R.getColonFieldLoc(anf_method_obj619,"make",L[252]);
if(R.isMethod($field621)) {
$ans597 = $field621.full_meth(anf_method_obj619,anf_arg620);
} else {
if(!(R.isFunction($field621))) {
R.ffi.throwNonFunApp(L[252],$field621);
}
$ans597 = $field621.app(anf_arg620);
}
break;
case 10: var anf_array_val622 = $ans597;
var anf_arg624 = [anf_array_val622];
$step594 = 11;
$al598 = L[251];
$field625 = R.getColonFieldLoc(anf_method_obj623,"make",L[251]);
if(R.isMethod($field625)) {
$ans597 = $field625.full_meth(anf_method_obj623,anf_arg624);
} else {
if(!(R.isFunction($field625))) {
R.ffi.throwNonFunApp(L[251],$field625);
}
$ans597 = $field625.app(anf_arg624);
}
break;
case 11: ++R.GAS;
return $ans597;
default: throw "No case numbered " + $step594 + " in $temp_full595";
}
}
} catch($e627) {
if(R.isCont($e627) && ($step594 !== 11)) {
$e627.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al598,$temp_full595,$step594,[self596],[anf_array_val617,anf_array_val616,anf_method_obj619,anf_method_obj623,anf_array_val604,anf_array_val603,anf_method_obj606,anf_method_obj610]);
}
if(R.isPyretException($e627)) {
$e627.pyretStack.push($al598);
}
throw $e627;
}
};
var anf_variant_member1172 = R.makeMethod0($temp_full595);
var $temp_full629 = function($self630) {
var $step628 = 0;
var $ans631 = D;
var $al632 = L[318];
try {
if(R.isActivationRecord($self630)) {
$step628 = $self630.step;
$al632 = $self630.from;
$ans631 = $self630.ans;
self630 = $self630.args[0];
anf_array_val744 = $self630.vars[0];
anf_array_val736 = $self630.vars[1];
anf_array_val735 = $self630.vars[2];
anf_method_obj738 = $self630.vars[3];
anf_array_val743 = $self630.vars[4];
anf_array_val742 = $self630.vars[5];
anf_array_val725 = $self630.vars[6];
anf_array_val724 = $self630.vars[7];
anf_method_obj727 = $self630.vars[8];
anf_method_obj746 = $self630.vars[9];
anf_arg749 = $self630.vars[10];
exp$arg$str651 = $self630.vars[11];
arg$str664 = $self630.vars[12];
this$str660 = $self630.vars[13];
num$args635 = $self630.vars[14];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[318],1,$t);
}
var self630 = $self630;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step628) {
case 0: var anf_method_obj633 = G(self630,"args",L[258]);
$step628 = 1;
$al632 = L[259];
$field634 = R.getColonFieldLoc(anf_method_obj633,"length",L[259]);
if(R.isMethod($field634)) {
$ans631 = $field634.full_meth(anf_method_obj633);
} else {
if(!(R.isFunction($field634))) {
R.ffi.throwNonFunApp(L[259],$field634);
}
$ans631 = $field634.app();
}
break;
case 1: var num$args635 = $ans631;
$step628 = 2;
$al632 = L[260];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al632,equal$always3);
}
$ans631 = equal$always3.app(num$args635,(1));
break;
case 2: var anf_arg636 = $ans631;
$al632 = L[260];
var anf_if754 = R.checkWrapBoolean(anf_arg636);
if(R.isPyretTrue(anf_if754)) {
$step628 = 3;
} else {
$step628 = 4;
}
break;
case 3: $step628 = 5;
$ans631 = ("this");
break;
case 4: $step628 = 5;
$ans631 = ("these");
break;
case 5: var this$str660 = $ans631;
$step628 = 6;
$al632 = L[261];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al632,equal$always3);
}
$ans631 = equal$always3.app(num$args635,(1));
break;
case 6: var anf_arg637 = $ans631;
$al632 = L[261];
var anf_if753 = R.checkWrapBoolean(anf_arg637);
if(R.isPyretTrue(anf_if753)) {
$step628 = 7;
} else {
$step628 = 8;
}
break;
case 7: $step628 = 9;
$ans631 = ("argument:");
break;
case 8: $step628 = 9;
$ans631 = ("arguments:");
break;
case 9: var arg$str664 = $ans631;
var anf_arg638 = G(self630,"expected-arity",L[262]);
$step628 = 10;
$al632 = L[263];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al632,equal$always3);
}
$ans631 = equal$always3.app(anf_arg638,(1));
break;
case 10: var anf_arg639 = $ans631;
$al632 = L[263];
var anf_if752 = R.checkWrapBoolean(anf_arg639);
if(R.isPyretTrue(anf_if752)) {
$step628 = 11;
} else {
$step628 = 12;
}
break;
case 11: $step628 = 13;
$ans631 = ("argument");
break;
case 12: $step628 = 13;
$ans631 = ("arguments");
break;
case 13: var exp$arg$str651 = $ans631;
var $temp_lam641 = function($caller$loc642) {
var $step640 = 0;
var $ans643 = D;
var $al644 = L[300];
try {
if(R.isActivationRecord($caller$loc642)) {
$step640 = $caller$loc642.step;
$al644 = $caller$loc642.from;
$ans643 = $caller$loc642.ans;
caller$loc642 = $caller$loc642.args[0];
anf_array_val712 = $caller$loc642.vars[0];
anf_array_val702 = $caller$loc642.vars[1];
anf_array_val701 = $caller$loc642.vars[2];
anf_method_obj704 = $caller$loc642.vars[3];
anf_array_val711 = $caller$loc642.vars[4];
anf_array_val710 = $caller$loc642.vars[5];
anf_method_obj694 = $caller$loc642.vars[6];
anf_array_val709 = $caller$loc642.vars[7];
anf_array_val708 = $caller$loc642.vars[8];
anf_array_val686 = $caller$loc642.vars[9];
anf_array_val685 = $caller$loc642.vars[10];
anf_method_obj688 = $caller$loc642.vars[11];
anf_method_obj714 = $caller$loc642.vars[12];
anf_array_val675 = $caller$loc642.vars[13];
anf_array_val667 = $caller$loc642.vars[14];
anf_array_val666 = $caller$loc642.vars[15];
anf_method_obj669 = $caller$loc642.vars[16];
anf_array_val674 = $caller$loc642.vars[17];
anf_array_val673 = $caller$loc642.vars[18];
anf_array_val655 = $caller$loc642.vars[19];
anf_array_val654 = $caller$loc642.vars[20];
anf_method_obj657 = $caller$loc642.vars[21];
anf_method_obj677 = $caller$loc642.vars[22];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[300],1,$t);
}
var caller$loc642 = $caller$loc642;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step640) {
case 0: var anf_method_obj645 = G(self630,"fun-loc",L[264]);
$step640 = 1;
$al644 = L[265];
$field646 = R.getColonFieldLoc(anf_method_obj645,"is-builtin",L[265]);
if(R.isMethod($field646)) {
$ans643 = $field646.full_meth(anf_method_obj645);
} else {
if(!(R.isFunction($field646))) {
R.ffi.throwNonFunApp(L[265],$field646);
}
$ans643 = $field646.app();
}
break;
case 1: var anf_arg647 = $ans643;
$al644 = L[265];
var anf_if717 = R.checkWrapBoolean(anf_arg647);
if(R.isPyretTrue(anf_if717)) {
$step640 = 2;
} else {
$step640 = 15;
}
break;
case 2: var anf_method_obj677 = G(ED12,"error",L[266]);
var anf_method_obj657 = G(ED12,"para",L[267]);
$step640 = 3;
$al644 = L[268];
$field648 = R.getColonFieldLoc(ED12,"text",L[268]);
if(R.isMethod($field648)) {
$ans643 = $field648.full_meth(ED12,("Expected to get"));
} else {
if(!(R.isFunction($field648))) {
R.ffi.throwNonFunApp(L[268],$field648);
}
$ans643 = $field648.app(("Expected to get"));
}
break;
case 3: var anf_array_val654 = $ans643;
var anf_arg649 = G(self630,"expected-arity",L[269]);
$step640 = 4;
$al644 = L[270];
$field650 = R.getColonFieldLoc(ED12,"embed",L[270]);
if(R.isMethod($field650)) {
$ans643 = $field650.full_meth(ED12,anf_arg649);
} else {
if(!(R.isFunction($field650))) {
R.ffi.throwNonFunApp(L[270],$field650);
}
$ans643 = $field650.app(anf_arg649);
}
break;
case 4: var anf_array_val655 = $ans643;
$step640 = 5;
$al644 = L[280];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al644,_plus1);
}
$ans643 = _plus1.app(exp$arg$str651,(" at"));
break;
case 5: var anf_arg652 = $ans643;
$step640 = 6;
$al644 = L[271];
$field653 = R.getColonFieldLoc(ED12,"text",L[271]);
if(R.isMethod($field653)) {
$ans643 = $field653.full_meth(ED12,anf_arg652);
} else {
if(!(R.isFunction($field653))) {
R.ffi.throwNonFunApp(L[271],$field653);
}
$ans643 = $field653.app(anf_arg652);
}
break;
case 6: var anf_array_val656 = $ans643;
var anf_arg658 = [anf_array_val654,anf_array_val655,anf_array_val656];
$step640 = 7;
$al644 = L[267];
$field659 = R.getColonFieldLoc(anf_method_obj657,"make",L[267]);
if(R.isMethod($field659)) {
$ans643 = $field659.full_meth(anf_method_obj657,anf_arg658);
} else {
if(!(R.isFunction($field659))) {
R.ffi.throwNonFunApp(L[267],$field659);
}
$ans643 = $field659.app(anf_arg658);
}
break;
case 7: var anf_array_val673 = $ans643;
$step640 = 8;
$al644 = L[279];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al644,draw$and$highlight33.$var);
}
$ans643 = draw$and$highlight33.$var.app(caller$loc642);
break;
case 8: var anf_array_val674 = $ans643;
var anf_method_obj669 = G(ED12,"para",L[272]);
$step640 = 9;
$al644 = L[278];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al644,_plus1);
}
$ans643 = _plus1.app(("but got "),this$str660);
break;
case 9: var anf_arg661 = $ans643;
$step640 = 10;
$al644 = L[273];
$field662 = R.getColonFieldLoc(ED12,"text",L[273]);
if(R.isMethod($field662)) {
$ans643 = $field662.full_meth(ED12,anf_arg661);
} else {
if(!(R.isFunction($field662))) {
R.ffi.throwNonFunApp(L[273],$field662);
}
$ans643 = $field662.app(anf_arg661);
}
break;
case 10: var anf_array_val666 = $ans643;
$step640 = 11;
$al644 = L[274];
$field663 = R.getColonFieldLoc(ED12,"embed",L[274]);
if(R.isMethod($field663)) {
$ans643 = $field663.full_meth(ED12,num$args635);
} else {
if(!(R.isFunction($field663))) {
R.ffi.throwNonFunApp(L[274],$field663);
}
$ans643 = $field663.app(num$args635);
}
break;
case 11: var anf_array_val667 = $ans643;
$step640 = 12;
$al644 = L[275];
$field665 = R.getColonFieldLoc(ED12,"text",L[275]);
if(R.isMethod($field665)) {
$ans643 = $field665.full_meth(ED12,arg$str664);
} else {
if(!(R.isFunction($field665))) {
R.ffi.throwNonFunApp(L[275],$field665);
}
$ans643 = $field665.app(arg$str664);
}
break;
case 12: var anf_array_val668 = $ans643;
var anf_arg670 = [anf_array_val666,anf_array_val667,anf_array_val668];
$step640 = 13;
$al644 = L[272];
$field671 = R.getColonFieldLoc(anf_method_obj669,"make",L[272]);
if(R.isMethod($field671)) {
$ans643 = $field671.full_meth(anf_method_obj669,anf_arg670);
} else {
if(!(R.isFunction($field671))) {
R.ffi.throwNonFunApp(L[272],$field671);
}
$ans643 = $field671.app(anf_arg670);
}
break;
case 13: var anf_array_val675 = $ans643;
var anf_arg672 = G(self630,"args",L[276]);
$step640 = 14;
$al644 = L[277];
if(!(R.isFunction(vert$list$values56.$var))) {
R.ffi.throwNonFunApp($al644,vert$list$values56.$var);
}
$ans643 = vert$list$values56.$var.app(anf_arg672);
break;
case 14: var anf_array_val676 = $ans643;
var anf_arg678 = [anf_array_val673,anf_array_val674,anf_array_val675,anf_array_val676];
$step640 = 31;
$al644 = L[266];
$field679 = R.getColonFieldLoc(anf_method_obj677,"make",L[266]);
if(R.isMethod($field679)) {
$ans643 = $field679.full_meth(anf_method_obj677,anf_arg678);
} else {
if(!(R.isFunction($field679))) {
R.ffi.throwNonFunApp(L[266],$field679);
}
$ans643 = $field679.app(anf_arg678);
}
break;
case 15: var anf_method_obj714 = G(ED12,"error",L[281]);
var anf_method_obj688 = G(ED12,"para",L[282]);
$step640 = 16;
$al644 = L[283];
$field680 = R.getColonFieldLoc(ED12,"text",L[283]);
if(R.isMethod($field680)) {
$ans643 = $field680.full_meth(ED12,("Expected to get"));
} else {
if(!(R.isFunction($field680))) {
R.ffi.throwNonFunApp(L[283],$field680);
}
$ans643 = $field680.app(("Expected to get"));
}
break;
case 16: var anf_array_val685 = $ans643;
var anf_arg681 = G(self630,"expected-arity",L[284]);
$step640 = 17;
$al644 = L[285];
$field682 = R.getColonFieldLoc(ED12,"embed",L[285]);
if(R.isMethod($field682)) {
$ans643 = $field682.full_meth(ED12,anf_arg681);
} else {
if(!(R.isFunction($field682))) {
R.ffi.throwNonFunApp(L[285],$field682);
}
$ans643 = $field682.app(anf_arg681);
}
break;
case 17: var anf_array_val686 = $ans643;
$step640 = 18;
$al644 = L[299];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al644,_plus1);
}
$ans643 = _plus1.app(exp$arg$str651,(" when calling the function at"));
break;
case 18: var anf_arg683 = $ans643;
$step640 = 19;
$al644 = L[286];
$field684 = R.getColonFieldLoc(ED12,"text",L[286]);
if(R.isMethod($field684)) {
$ans643 = $field684.full_meth(ED12,anf_arg683);
} else {
if(!(R.isFunction($field684))) {
R.ffi.throwNonFunApp(L[286],$field684);
}
$ans643 = $field684.app(anf_arg683);
}
break;
case 19: var anf_array_val687 = $ans643;
var anf_arg689 = [anf_array_val685,anf_array_val686,anf_array_val687];
$step640 = 20;
$al644 = L[282];
$field690 = R.getColonFieldLoc(anf_method_obj688,"make",L[282]);
if(R.isMethod($field690)) {
$ans643 = $field690.full_meth(anf_method_obj688,anf_arg689);
} else {
if(!(R.isFunction($field690))) {
R.ffi.throwNonFunApp(L[282],$field690);
}
$ans643 = $field690.app(anf_arg689);
}
break;
case 20: var anf_array_val708 = $ans643;
var anf_arg691 = G(self630,"fun-loc",L[287]);
$step640 = 21;
$al644 = L[298];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al644,draw$and$highlight33.$var);
}
$ans643 = draw$and$highlight33.$var.app(anf_arg691);
break;
case 21: var anf_array_val709 = $ans643;
var anf_method_obj694 = G(ED12,"para",L[288]);
$step640 = 22;
$al644 = L[289];
$field692 = R.getColonFieldLoc(ED12,"text",L[289]);
if(R.isMethod($field692)) {
$ans643 = $field692.full_meth(ED12,("from"));
} else {
if(!(R.isFunction($field692))) {
R.ffi.throwNonFunApp(L[289],$field692);
}
$ans643 = $field692.app(("from"));
}
break;
case 22: var anf_array_val693 = $ans643;
var anf_arg695 = [anf_array_val693];
$step640 = 23;
$al644 = L[288];
$field696 = R.getColonFieldLoc(anf_method_obj694,"make",L[288]);
if(R.isMethod($field696)) {
$ans643 = $field696.full_meth(anf_method_obj694,anf_arg695);
} else {
if(!(R.isFunction($field696))) {
R.ffi.throwNonFunApp(L[288],$field696);
}
$ans643 = $field696.app(anf_arg695);
}
break;
case 23: var anf_array_val710 = $ans643;
$step640 = 24;
$al644 = L[297];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al644,draw$and$highlight33.$var);
}
$ans643 = draw$and$highlight33.$var.app(caller$loc642);
break;
case 24: var anf_array_val711 = $ans643;
var anf_method_obj704 = G(ED12,"para",L[290]);
$step640 = 25;
$al644 = L[296];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al644,_plus1);
}
$ans643 = _plus1.app(("but got "),this$str660);
break;
case 25: var anf_arg697 = $ans643;
$step640 = 26;
$al644 = L[291];
$field698 = R.getColonFieldLoc(ED12,"text",L[291]);
if(R.isMethod($field698)) {
$ans643 = $field698.full_meth(ED12,anf_arg697);
} else {
if(!(R.isFunction($field698))) {
R.ffi.throwNonFunApp(L[291],$field698);
}
$ans643 = $field698.app(anf_arg697);
}
break;
case 26: var anf_array_val701 = $ans643;
$step640 = 27;
$al644 = L[292];
$field699 = R.getColonFieldLoc(ED12,"embed",L[292]);
if(R.isMethod($field699)) {
$ans643 = $field699.full_meth(ED12,num$args635);
} else {
if(!(R.isFunction($field699))) {
R.ffi.throwNonFunApp(L[292],$field699);
}
$ans643 = $field699.app(num$args635);
}
break;
case 27: var anf_array_val702 = $ans643;
$step640 = 28;
$al644 = L[293];
$field700 = R.getColonFieldLoc(ED12,"text",L[293]);
if(R.isMethod($field700)) {
$ans643 = $field700.full_meth(ED12,arg$str664);
} else {
if(!(R.isFunction($field700))) {
R.ffi.throwNonFunApp(L[293],$field700);
}
$ans643 = $field700.app(arg$str664);
}
break;
case 28: var anf_array_val703 = $ans643;
var anf_arg705 = [anf_array_val701,anf_array_val702,anf_array_val703];
$step640 = 29;
$al644 = L[290];
$field706 = R.getColonFieldLoc(anf_method_obj704,"make",L[290]);
if(R.isMethod($field706)) {
$ans643 = $field706.full_meth(anf_method_obj704,anf_arg705);
} else {
if(!(R.isFunction($field706))) {
R.ffi.throwNonFunApp(L[290],$field706);
}
$ans643 = $field706.app(anf_arg705);
}
break;
case 29: var anf_array_val712 = $ans643;
var anf_arg707 = G(self630,"args",L[294]);
$step640 = 30;
$al644 = L[295];
if(!(R.isFunction(vert$list$values56.$var))) {
R.ffi.throwNonFunApp($al644,vert$list$values56.$var);
}
$ans643 = vert$list$values56.$var.app(anf_arg707);
break;
case 30: var anf_array_val713 = $ans643;
var anf_arg715 = [anf_array_val708,anf_array_val709,anf_array_val710,anf_array_val711,anf_array_val712,anf_array_val713];
$step640 = 31;
$al644 = L[281];
$field716 = R.getColonFieldLoc(anf_method_obj714,"make",L[281]);
if(R.isMethod($field716)) {
$ans643 = $field716.full_meth(anf_method_obj714,anf_arg715);
} else {
if(!(R.isFunction($field716))) {
R.ffi.throwNonFunApp(L[281],$field716);
}
$ans643 = $field716.app(anf_arg715);
}
break;
case 31: ++R.GAS;
return $ans643;
default: throw "No case numbered " + $step640 + " in $temp_lam641";
}
}
} catch($e718) {
if(R.isCont($e718) && ($step640 !== 31)) {
$e718.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al644,$temp_lam641,$step640,[caller$loc642],[anf_array_val712,anf_array_val702,anf_array_val701,anf_method_obj704,anf_array_val711,anf_array_val710,anf_method_obj694,anf_array_val709,anf_array_val708,anf_array_val686,anf_array_val685,anf_method_obj688,anf_method_obj714,anf_array_val675,anf_array_val667,anf_array_val666,anf_method_obj669,anf_array_val674,anf_array_val673,anf_array_val655,anf_array_val654,anf_method_obj657,anf_method_obj677]);
}
if(R.isPyretException($e718)) {
$e718.pyretStack.push($al644);
}
throw $e718;
}
};
var anf_arg749 = R.makeFunction($temp_lam641);
var anf_method_obj746 = G(ED12,"error",L[301]);
var anf_method_obj727 = G(ED12,"para",L[302]);
$step628 = 14;
$al632 = L[303];
$field719 = R.getColonFieldLoc(ED12,"text",L[303]);
if(R.isMethod($field719)) {
$ans631 = $field719.full_meth(ED12,("Expected to get"));
} else {
if(!(R.isFunction($field719))) {
R.ffi.throwNonFunApp(L[303],$field719);
}
$ans631 = $field719.app(("Expected to get"));
}
break;
case 14: var anf_array_val724 = $ans631;
var anf_arg720 = G(self630,"expected-arity",L[304]);
$step628 = 15;
$al632 = L[305];
$field721 = R.getColonFieldLoc(ED12,"embed",L[305]);
if(R.isMethod($field721)) {
$ans631 = $field721.full_meth(ED12,anf_arg720);
} else {
if(!(R.isFunction($field721))) {
R.ffi.throwNonFunApp(L[305],$field721);
}
$ans631 = $field721.app(anf_arg720);
}
break;
case 15: var anf_array_val725 = $ans631;
$step628 = 16;
$al632 = L[317];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al632,_plus1);
}
$ans631 = _plus1.app(exp$arg$str651,(" at"));
break;
case 16: var anf_arg722 = $ans631;
$step628 = 17;
$al632 = L[306];
$field723 = R.getColonFieldLoc(ED12,"text",L[306]);
if(R.isMethod($field723)) {
$ans631 = $field723.full_meth(ED12,anf_arg722);
} else {
if(!(R.isFunction($field723))) {
R.ffi.throwNonFunApp(L[306],$field723);
}
$ans631 = $field723.app(anf_arg722);
}
break;
case 17: var anf_array_val726 = $ans631;
var anf_arg728 = [anf_array_val724,anf_array_val725,anf_array_val726];
$step628 = 18;
$al632 = L[302];
$field729 = R.getColonFieldLoc(anf_method_obj727,"make",L[302]);
if(R.isMethod($field729)) {
$ans631 = $field729.full_meth(anf_method_obj727,anf_arg728);
} else {
if(!(R.isFunction($field729))) {
R.ffi.throwNonFunApp(L[302],$field729);
}
$ans631 = $field729.app(anf_arg728);
}
break;
case 18: var anf_array_val742 = $ans631;
var anf_arg730 = G(self630,"fun-loc",L[307]);
$step628 = 19;
$al632 = L[316];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al632,draw$and$highlight33.$var);
}
$ans631 = draw$and$highlight33.$var.app(anf_arg730);
break;
case 19: var anf_array_val743 = $ans631;
var anf_method_obj738 = G(ED12,"para",L[308]);
$step628 = 20;
$al632 = L[315];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al632,_plus1);
}
$ans631 = _plus1.app(("but got "),this$str660);
break;
case 20: var anf_arg731 = $ans631;
$step628 = 21;
$al632 = L[309];
$field732 = R.getColonFieldLoc(ED12,"text",L[309]);
if(R.isMethod($field732)) {
$ans631 = $field732.full_meth(ED12,anf_arg731);
} else {
if(!(R.isFunction($field732))) {
R.ffi.throwNonFunApp(L[309],$field732);
}
$ans631 = $field732.app(anf_arg731);
}
break;
case 21: var anf_array_val735 = $ans631;
$step628 = 22;
$al632 = L[310];
$field733 = R.getColonFieldLoc(ED12,"embed",L[310]);
if(R.isMethod($field733)) {
$ans631 = $field733.full_meth(ED12,num$args635);
} else {
if(!(R.isFunction($field733))) {
R.ffi.throwNonFunApp(L[310],$field733);
}
$ans631 = $field733.app(num$args635);
}
break;
case 22: var anf_array_val736 = $ans631;
$step628 = 23;
$al632 = L[311];
$field734 = R.getColonFieldLoc(ED12,"text",L[311]);
if(R.isMethod($field734)) {
$ans631 = $field734.full_meth(ED12,arg$str664);
} else {
if(!(R.isFunction($field734))) {
R.ffi.throwNonFunApp(L[311],$field734);
}
$ans631 = $field734.app(arg$str664);
}
break;
case 23: var anf_array_val737 = $ans631;
var anf_arg739 = [anf_array_val735,anf_array_val736,anf_array_val737];
$step628 = 24;
$al632 = L[308];
$field740 = R.getColonFieldLoc(anf_method_obj738,"make",L[308]);
if(R.isMethod($field740)) {
$ans631 = $field740.full_meth(anf_method_obj738,anf_arg739);
} else {
if(!(R.isFunction($field740))) {
R.ffi.throwNonFunApp(L[308],$field740);
}
$ans631 = $field740.app(anf_arg739);
}
break;
case 24: var anf_array_val744 = $ans631;
var anf_arg741 = G(self630,"args",L[312]);
$step628 = 25;
$al632 = L[314];
if(!(R.isFunction(vert$list$values56.$var))) {
R.ffi.throwNonFunApp($al632,vert$list$values56.$var);
}
$ans631 = vert$list$values56.$var.app(anf_arg741);
break;
case 25: var anf_array_val745 = $ans631;
var anf_arg747 = [anf_array_val742,anf_array_val743,anf_array_val744,anf_array_val745];
$step628 = 26;
$al632 = L[301];
$field748 = R.getColonFieldLoc(anf_method_obj746,"make",L[301]);
if(R.isMethod($field748)) {
$ans631 = $field748.full_meth(anf_method_obj746,anf_arg747);
} else {
if(!(R.isFunction($field748))) {
R.ffi.throwNonFunApp(L[301],$field748);
}
$ans631 = $field748.app(anf_arg747);
}
break;
case 26: var anf_arg750 = $ans631;
$step628 = 27;
$al632 = L[313];
$field751 = R.getColonFieldLoc(ED12,"maybe-stack-loc",L[313]);
if(R.isMethod($field751)) {
$ans631 = $field751.full_meth(ED12,(0),(true),anf_arg749,anf_arg750);
} else {
if(!(R.isFunction($field751))) {
R.ffi.throwNonFunApp(L[313],$field751);
}
$ans631 = $field751.app((0),(true),anf_arg749,anf_arg750);
}
break;
case 27: ++R.GAS;
return $ans631;
default: throw "No case numbered " + $step628 + " in $temp_full629";
}
}
} catch($e755) {
if(R.isCont($e755) && ($step628 !== 27)) {
$e755.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al632,$temp_full629,$step628,[self630],[anf_array_val744,anf_array_val736,anf_array_val735,anf_method_obj738,anf_array_val743,anf_array_val742,anf_array_val725,anf_array_val724,anf_method_obj727,anf_method_obj746,anf_arg749,exp$arg$str651,arg$str664,this$str660,num$args635]);
}
if(R.isPyretException($e755)) {
$e755.pyretStack.push($al632);
}
throw $e755;
}
};
var anf_variant_member1183 = R.makeMethod0($temp_full629);
var $temp_full757 = function($self758) {
var $step756 = 0;
var $ans759 = D;
var $al760 = L[327];
try {
if(R.isActivationRecord($self758)) {
$step756 = $self758.step;
$al760 = $self758.from;
$ans759 = $self758.ans;
self758 = $self758.args[0];
anf_array_val772 = $self758.vars[0];
anf_array_val765 = $self758.vars[1];
anf_array_val764 = $self758.vars[2];
anf_method_obj767 = $self758.vars[3];
anf_method_obj774 = $self758.vars[4];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[327],1,$t);
}
var self758 = $self758;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step756) {
case 0: var anf_method_obj774 = G(ED12,"error",L[319]);
var anf_method_obj767 = G(ED12,"para",L[320]);
$step756 = 1;
$al760 = L[321];
$field761 = R.getColonFieldLoc(ED12,"text",L[321]);
if(R.isMethod($field761)) {
$ans759 = $field761.full_meth(ED12,("Expected a function in the application expression at"));
} else {
if(!(R.isFunction($field761))) {
R.ffi.throwNonFunApp(L[321],$field761);
}
$ans759 = $field761.app(("Expected a function in the application expression at"));
}
break;
case 1: var anf_array_val764 = $ans759;
var anf_arg762 = G(self758,"loc",L[322]);
$step756 = 2;
$al760 = L[326];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al760,draw$and$highlight33.$var);
}
$ans759 = draw$and$highlight33.$var.app(anf_arg762);
break;
case 2: var anf_array_val765 = $ans759;
$step756 = 3;
$al760 = L[323];
$field763 = R.getColonFieldLoc(ED12,"text",L[323]);
if(R.isMethod($field763)) {
$ans759 = $field763.full_meth(ED12,(" but got:"));
} else {
if(!(R.isFunction($field763))) {
R.ffi.throwNonFunApp(L[323],$field763);
}
$ans759 = $field763.app((" but got:"));
}
break;
case 3: var anf_array_val766 = $ans759;
var anf_arg768 = [anf_array_val764,anf_array_val765,anf_array_val766];
$step756 = 4;
$al760 = L[320];
$field769 = R.getColonFieldLoc(anf_method_obj767,"make",L[320]);
if(R.isMethod($field769)) {
$ans759 = $field769.full_meth(anf_method_obj767,anf_arg768);
} else {
if(!(R.isFunction($field769))) {
R.ffi.throwNonFunApp(L[320],$field769);
}
$ans759 = $field769.app(anf_arg768);
}
break;
case 4: var anf_array_val772 = $ans759;
var anf_arg770 = G(self758,"non-fun-val",L[324]);
$step756 = 5;
$al760 = L[325];
$field771 = R.getColonFieldLoc(ED12,"embed",L[325]);
if(R.isMethod($field771)) {
$ans759 = $field771.full_meth(ED12,anf_arg770);
} else {
if(!(R.isFunction($field771))) {
R.ffi.throwNonFunApp(L[325],$field771);
}
$ans759 = $field771.app(anf_arg770);
}
break;
case 5: var anf_array_val773 = $ans759;
var anf_arg775 = [anf_array_val772,anf_array_val773];
$step756 = 6;
$al760 = L[319];
$field776 = R.getColonFieldLoc(anf_method_obj774,"make",L[319]);
if(R.isMethod($field776)) {
$ans759 = $field776.full_meth(anf_method_obj774,anf_arg775);
} else {
if(!(R.isFunction($field776))) {
R.ffi.throwNonFunApp(L[319],$field776);
}
$ans759 = $field776.app(anf_arg775);
}
break;
case 6: ++R.GAS;
return $ans759;
default: throw "No case numbered " + $step756 + " in $temp_full757";
}
}
} catch($e777) {
if(R.isCont($e777) && ($step756 !== 6)) {
$e777.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al760,$temp_full757,$step756,[self758],[anf_array_val772,anf_array_val765,anf_array_val764,anf_method_obj767,anf_method_obj774]);
}
if(R.isPyretException($e777)) {
$e777.pyretStack.push($al760);
}
throw $e777;
}
};
var anf_variant_member1195 = R.makeMethod0($temp_full757);
var $temp_full779 = function($self780) {
var $step778 = 0;
var $ans781 = D;
var $al782 = L[331];
try {
if(R.isActivationRecord($self780)) {
$step778 = $self780.step;
$al782 = $self780.from;
$ans781 = $self780.ans;
self780 = $self780.args[0];
anf_method_obj786 = $self780.vars[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[331],1,$t);
}
var self780 = $self780;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step778) {
case 0: var anf_method_obj786 = G(ED12,"error",L[328]);
$step778 = 1;
$al782 = L[330];
if(!(R.isFunction(tostring4))) {
R.ffi.throwNonFunApp($al782,tostring4);
}
$ans781 = tostring4.app(self780);
break;
case 1: var anf_arg783 = $ans781;
$step778 = 2;
$al782 = L[329];
$field784 = R.getColonFieldLoc(ED12,"text",L[329]);
if(R.isMethod($field784)) {
$ans781 = $field784.full_meth(ED12,anf_arg783);
} else {
if(!(R.isFunction($field784))) {
R.ffi.throwNonFunApp(L[329],$field784);
}
$ans781 = $field784.app(anf_arg783);
}
break;
case 2: var anf_array_val785 = $ans781;
var anf_arg787 = [anf_array_val785];
$step778 = 3;
$al782 = L[328];
$field788 = R.getColonFieldLoc(anf_method_obj786,"make",L[328]);
if(R.isMethod($field788)) {
$ans781 = $field788.full_meth(anf_method_obj786,anf_arg787);
} else {
if(!(R.isFunction($field788))) {
R.ffi.throwNonFunApp(L[328],$field788);
}
$ans781 = $field788.app(anf_arg787);
}
break;
case 3: ++R.GAS;
return $ans781;
default: throw "No case numbered " + $step778 + " in $temp_full779";
}
}
} catch($e789) {
if(R.isCont($e789) && ($step778 !== 3)) {
$e789.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al782,$temp_full779,$step778,[self780],[anf_method_obj786]);
}
if(R.isPyretException($e789)) {
$e789.pyretStack.push($al782);
}
throw $e789;
}
};
var anf_variant_member1206 = R.makeMethod0($temp_full779);
var $temp_full791 = function($self792) {
var $step790 = 0;
var $ans793 = D;
var $al794 = L[342];
try {
if(R.isActivationRecord($self792)) {
$step790 = $self792.step;
$al794 = $self792.from;
$ans793 = $self792.ans;
self792 = $self792.args[0];
anf_array_val806 = $self792.vars[0];
anf_array_val805 = $self792.vars[1];
anf_array_val804 = $self792.vars[2];
anf_array_val803 = $self792.vars[3];
anf_method_obj808 = $self792.vars[4];
anf_method_obj812 = $self792.vars[5];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[342],1,$t);
}
var self792 = $self792;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step790) {
case 0: var anf_method_obj812 = G(ED12,"error",L[332]);
var anf_method_obj808 = G(ED12,"para",L[333]);
$step790 = 1;
$al794 = L[334];
$field795 = R.getColonFieldLoc(ED12,"text",L[334]);
if(R.isMethod($field795)) {
$ans793 = $field795.full_meth(ED12,("The name"));
} else {
if(!(R.isFunction($field795))) {
R.ffi.throwNonFunApp(L[334],$field795);
}
$ans793 = $field795.app(("The name"));
}
break;
case 1: var anf_array_val803 = $ans793;
var anf_arg796 = G(self792,"name",L[335]);
$step790 = 2;
$al794 = L[336];
$field797 = R.getColonFieldLoc(ED12,"text",L[336]);
if(R.isMethod($field797)) {
$ans793 = $field797.full_meth(ED12,anf_arg796);
} else {
if(!(R.isFunction($field797))) {
R.ffi.throwNonFunApp(L[336],$field797);
}
$ans793 = $field797.app(anf_arg796);
}
break;
case 2: var anf_arg798 = $ans793;
$step790 = 3;
$al794 = L[337];
$field799 = R.getColonFieldLoc(ED12,"code",L[337]);
if(R.isMethod($field799)) {
$ans793 = $field799.full_meth(ED12,anf_arg798);
} else {
if(!(R.isFunction($field799))) {
R.ffi.throwNonFunApp(L[337],$field799);
}
$ans793 = $field799.app(anf_arg798);
}
break;
case 3: var anf_array_val804 = $ans793;
$step790 = 4;
$al794 = L[338];
$field800 = R.getColonFieldLoc(ED12,"text",L[338]);
if(R.isMethod($field800)) {
$ans793 = $field800.full_meth(ED12,("was used at"));
} else {
if(!(R.isFunction($field800))) {
R.ffi.throwNonFunApp(L[338],$field800);
}
$ans793 = $field800.app(("was used at"));
}
break;
case 4: var anf_array_val805 = $ans793;
var anf_arg801 = G(self792,"loc",L[339]);
$step790 = 5;
$al794 = L[341];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al794,draw$and$highlight33.$var);
}
$ans793 = draw$and$highlight33.$var.app(anf_arg801);
break;
case 5: var anf_array_val806 = $ans793;
$step790 = 6;
$al794 = L[340];
$field802 = R.getColonFieldLoc(ED12,"text",L[340]);
if(R.isMethod($field802)) {
$ans793 = $field802.full_meth(ED12,("before it was defined."));
} else {
if(!(R.isFunction($field802))) {
R.ffi.throwNonFunApp(L[340],$field802);
}
$ans793 = $field802.app(("before it was defined."));
}
break;
case 6: var anf_array_val807 = $ans793;
var anf_arg809 = [anf_array_val803,anf_array_val804,anf_array_val805,anf_array_val806,anf_array_val807];
$step790 = 7;
$al794 = L[333];
$field810 = R.getColonFieldLoc(anf_method_obj808,"make",L[333]);
if(R.isMethod($field810)) {
$ans793 = $field810.full_meth(anf_method_obj808,anf_arg809);
} else {
if(!(R.isFunction($field810))) {
R.ffi.throwNonFunApp(L[333],$field810);
}
$ans793 = $field810.app(anf_arg809);
}
break;
case 7: var anf_array_val811 = $ans793;
var anf_arg813 = [anf_array_val811];
$step790 = 8;
$al794 = L[332];
$field814 = R.getColonFieldLoc(anf_method_obj812,"make",L[332]);
if(R.isMethod($field814)) {
$ans793 = $field814.full_meth(anf_method_obj812,anf_arg813);
} else {
if(!(R.isFunction($field814))) {
R.ffi.throwNonFunApp(L[332],$field814);
}
$ans793 = $field814.app(anf_arg813);
}
break;
case 8: ++R.GAS;
return $ans793;
default: throw "No case numbered " + $step790 + " in $temp_full791";
}
}
} catch($e815) {
if(R.isCont($e815) && ($step790 !== 8)) {
$e815.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al794,$temp_full791,$step790,[self792],[anf_array_val806,anf_array_val805,anf_array_val804,anf_array_val803,anf_method_obj808,anf_method_obj812]);
}
if(R.isPyretException($e815)) {
$e815.pyretStack.push($al794);
}
throw $e815;
}
};
var anf_variant_member1220 = R.makeMethod0($temp_full791);
var $temp_full817 = function($self818) {
var $step816 = 0;
var $ans819 = D;
var $al820 = L[354];
try {
if(R.isActivationRecord($self818)) {
$step816 = $self818.step;
$al820 = $self818.from;
$ans819 = $self818.ans;
self818 = $self818.args[0];
anf_array_val836 = $self818.vars[0];
anf_method_obj828 = $self818.vars[1];
anf_method_obj838 = $self818.vars[2];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[354],1,$t);
}
var self818 = $self818;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step816) {
case 0: var anf_method_obj838 = G(ED12,"error",L[343]);
var anf_method_obj828 = G(ED12,"para",L[344]);
var anf_method_obj821 = G(self818,"names",L[345]);
$step816 = 1;
$al820 = L[346];
$field822 = R.getColonFieldLoc(anf_method_obj821,"length",L[346]);
if(R.isMethod($field822)) {
$ans819 = $field822.full_meth(anf_method_obj821);
} else {
if(!(R.isFunction($field822))) {
R.ffi.throwNonFunApp(L[346],$field822);
}
$ans819 = $field822.app();
}
break;
case 1: var anf_arg823 = $ans819;
$step816 = 2;
$al820 = L[347];
if(!(R.isFunction(equal$always3))) {
R.ffi.throwNonFunApp($al820,equal$always3);
}
$ans819 = equal$always3.app(anf_arg823,(1));
break;
case 2: var anf_arg824 = $ans819;
$al820 = L[347];
var anf_if841 = R.checkWrapBoolean(anf_arg824);
if(R.isPyretTrue(anf_if841)) {
$step816 = 3;
} else {
$step816 = 4;
}
break;
case 3: $step816 = 5;
$al820 = L[348];
$field825 = R.getColonFieldLoc(ED12,"text",L[348]);
if(R.isMethod($field825)) {
$ans819 = $field825.full_meth(ED12,("The following module failed to load:"));
} else {
if(!(R.isFunction($field825))) {
R.ffi.throwNonFunApp(L[348],$field825);
}
$ans819 = $field825.app(("The following module failed to load:"));
}
break;
case 4: $step816 = 5;
$al820 = L[349];
$field826 = R.getColonFieldLoc(ED12,"text",L[349]);
if(R.isMethod($field826)) {
$ans819 = $field826.full_meth(ED12,("The following modules failed to load:"));
} else {
if(!(R.isFunction($field826))) {
R.ffi.throwNonFunApp(L[349],$field826);
}
$ans819 = $field826.app(("The following modules failed to load:"));
}
break;
case 5: var anf_array_val827 = $ans819;
var anf_arg829 = [anf_array_val827];
$step816 = 6;
$al820 = L[344];
$field830 = R.getColonFieldLoc(anf_method_obj828,"make",L[344]);
if(R.isMethod($field830)) {
$ans819 = $field830.full_meth(anf_method_obj828,anf_arg829);
} else {
if(!(R.isFunction($field830))) {
R.ffi.throwNonFunApp(L[344],$field830);
}
$ans819 = $field830.app(anf_arg829);
}
break;
case 6: var anf_array_val836 = $ans819;
var anf_method_obj831 = G(self818,"names",L[350]);
var anf_arg832 = G(ED12,"text",L[351]);
$step816 = 7;
$al820 = L[352];
$field833 = R.getColonFieldLoc(anf_method_obj831,"map",L[352]);
if(R.isMethod($field833)) {
$ans819 = $field833.full_meth(anf_method_obj831,anf_arg832);
} else {
if(!(R.isFunction($field833))) {
R.ffi.throwNonFunApp(L[352],$field833);
}
$ans819 = $field833.app(anf_arg832);
}
break;
case 7: var anf_arg834 = $ans819;
$step816 = 8;
$al820 = L[353];
$field835 = R.getColonFieldLoc(ED12,"h-sequence",L[353]);
if(R.isMethod($field835)) {
$ans819 = $field835.full_meth(ED12,anf_arg834,(", "));
} else {
if(!(R.isFunction($field835))) {
R.ffi.throwNonFunApp(L[353],$field835);
}
$ans819 = $field835.app(anf_arg834,(", "));
}
break;
case 8: var anf_array_val837 = $ans819;
var anf_arg839 = [anf_array_val836,anf_array_val837];
$step816 = 9;
$al820 = L[343];
$field840 = R.getColonFieldLoc(anf_method_obj838,"make",L[343]);
if(R.isMethod($field840)) {
$ans819 = $field840.full_meth(anf_method_obj838,anf_arg839);
} else {
if(!(R.isFunction($field840))) {
R.ffi.throwNonFunApp(L[343],$field840);
}
$ans819 = $field840.app(anf_arg839);
}
break;
case 9: ++R.GAS;
return $ans819;
default: throw "No case numbered " + $step816 + " in $temp_full817";
}
}
} catch($e842) {
if(R.isCont($e842) && ($step816 !== 9)) {
$e842.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al820,$temp_full817,$step816,[self818],[anf_array_val836,anf_method_obj828,anf_method_obj838]);
}
if(R.isPyretException($e842)) {
$e842.pyretStack.push($al820);
}
throw $e842;
}
};
var anf_variant_member1231 = R.makeMethod0($temp_full817);
var $temp_full844 = function($self845) {
var $step843 = 0;
var $ans846 = D;
var $al847 = L[377];
try {
if(R.isActivationRecord($self845)) {
$step843 = $self845.step;
$al847 = $self845.from;
$ans846 = $self845.ans;
self845 = $self845.args[0];
anf_array_val886 = $self845.vars[0];
anf_array_val885 = $self845.vars[1];
anf_array_val884 = $self845.vars[2];
anf_method_obj888 = $self845.vars[3];
anf_method_obj892 = $self845.vars[4];
anf_arg895 = $self845.vars[5];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[377],1,$t);
}
var self845 = $self845;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step843) {
case 0: var $temp_lam849 = function($loc850) {
var $step848 = 0;
var $ans851 = D;
var $al852 = L[366];
try {
if(R.isActivationRecord($loc850)) {
$step848 = $loc850.step;
$al852 = $loc850.from;
$ans851 = $loc850.ans;
loc850 = $loc850.args[0];
anf_array_val866 = $loc850.vars[0];
anf_array_val865 = $loc850.vars[1];
anf_array_val864 = $loc850.vars[2];
anf_array_val863 = $loc850.vars[3];
anf_array_val862 = $loc850.vars[4];
anf_method_obj868 = $loc850.vars[5];
anf_method_obj872 = $loc850.vars[6];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[366],1,$t);
}
var loc850 = $loc850;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step848) {
case 0: var anf_method_obj872 = G(ED12,"error",L[355]);
var anf_method_obj868 = G(ED12,"para",L[356]);
$step848 = 1;
$al852 = L[357];
$field853 = R.getColonFieldLoc(ED12,"text",L[357]);
if(R.isMethod($field853)) {
$ans851 = $field853.full_meth(ED12,("Invalid array index"));
} else {
if(!(R.isFunction($field853))) {
R.ffi.throwNonFunApp(L[357],$field853);
}
$ans851 = $field853.app(("Invalid array index"));
}
break;
case 1: var anf_array_val862 = $ans851;
var anf_arg854 = G(self845,"index",L[358]);
$step848 = 2;
$al852 = L[359];
$field855 = R.getColonFieldLoc(ED12,"embed",L[359]);
if(R.isMethod($field855)) {
$ans851 = $field855.full_meth(ED12,anf_arg854);
} else {
if(!(R.isFunction($field855))) {
R.ffi.throwNonFunApp(L[359],$field855);
}
$ans851 = $field855.app(anf_arg854);
}
break;
case 2: var anf_arg856 = $ans851;
$step848 = 3;
$al852 = L[360];
$field857 = R.getColonFieldLoc(ED12,"code",L[360]);
if(R.isMethod($field857)) {
$ans851 = $field857.full_meth(ED12,anf_arg856);
} else {
if(!(R.isFunction($field857))) {
R.ffi.throwNonFunApp(L[360],$field857);
}
$ans851 = $field857.app(anf_arg856);
}
break;
case 3: var anf_array_val863 = $ans851;
$step848 = 4;
$al852 = L[361];
$field858 = R.getColonFieldLoc(ED12,"text",L[361]);
if(R.isMethod($field858)) {
$ans851 = $field858.full_meth(ED12,("around the function call at"));
} else {
if(!(R.isFunction($field858))) {
R.ffi.throwNonFunApp(L[361],$field858);
}
$ans851 = $field858.app(("around the function call at"));
}
break;
case 4: var anf_array_val864 = $ans851;
$step848 = 5;
$al852 = L[365];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al852,draw$and$highlight33.$var);
}
$ans851 = draw$and$highlight33.$var.app(loc850);
break;
case 5: var anf_array_val865 = $ans851;
$step848 = 6;
$al852 = L[362];
$field859 = R.getColonFieldLoc(ED12,"text",L[362]);
if(R.isMethod($field859)) {
$ans851 = $field859.full_meth(ED12,("because:"));
} else {
if(!(R.isFunction($field859))) {
R.ffi.throwNonFunApp(L[362],$field859);
}
$ans851 = $field859.app(("because:"));
}
break;
case 6: var anf_array_val866 = $ans851;
var anf_arg860 = G(self845,"reason",L[363]);
$step848 = 7;
$al852 = L[364];
$field861 = R.getColonFieldLoc(ED12,"text",L[364]);
if(R.isMethod($field861)) {
$ans851 = $field861.full_meth(ED12,anf_arg860);
} else {
if(!(R.isFunction($field861))) {
R.ffi.throwNonFunApp(L[364],$field861);
}
$ans851 = $field861.app(anf_arg860);
}
break;
case 7: var anf_array_val867 = $ans851;
var anf_arg869 = [anf_array_val862,anf_array_val863,anf_array_val864,anf_array_val865,anf_array_val866,anf_array_val867];
$step848 = 8;
$al852 = L[356];
$field870 = R.getColonFieldLoc(anf_method_obj868,"make",L[356]);
if(R.isMethod($field870)) {
$ans851 = $field870.full_meth(anf_method_obj868,anf_arg869);
} else {
if(!(R.isFunction($field870))) {
R.ffi.throwNonFunApp(L[356],$field870);
}
$ans851 = $field870.app(anf_arg869);
}
break;
case 8: var anf_array_val871 = $ans851;
var anf_arg873 = [anf_array_val871];
$step848 = 9;
$al852 = L[355];
$field874 = R.getColonFieldLoc(anf_method_obj872,"make",L[355]);
if(R.isMethod($field874)) {
$ans851 = $field874.full_meth(anf_method_obj872,anf_arg873);
} else {
if(!(R.isFunction($field874))) {
R.ffi.throwNonFunApp(L[355],$field874);
}
$ans851 = $field874.app(anf_arg873);
}
break;
case 9: ++R.GAS;
return $ans851;
default: throw "No case numbered " + $step848 + " in $temp_lam849";
}
}
} catch($e875) {
if(R.isCont($e875) && ($step848 !== 9)) {
$e875.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al852,$temp_lam849,$step848,[loc850],[anf_array_val866,anf_array_val865,anf_array_val864,anf_array_val863,anf_array_val862,anf_method_obj868,anf_method_obj872]);
}
if(R.isPyretException($e875)) {
$e875.pyretStack.push($al852);
}
throw $e875;
}
};
var anf_arg895 = R.makeFunction($temp_lam849);
var anf_method_obj892 = G(ED12,"error",L[367]);
var anf_method_obj888 = G(ED12,"para",L[368]);
$step843 = 1;
$al847 = L[369];
$field876 = R.getColonFieldLoc(ED12,"text",L[369]);
if(R.isMethod($field876)) {
$ans846 = $field876.full_meth(ED12,("Invalid array index"));
} else {
if(!(R.isFunction($field876))) {
R.ffi.throwNonFunApp(L[369],$field876);
}
$ans846 = $field876.app(("Invalid array index"));
}
break;
case 1: var anf_array_val884 = $ans846;
var anf_arg877 = G(self845,"index",L[370]);
$step843 = 2;
$al847 = L[371];
$field878 = R.getColonFieldLoc(ED12,"embed",L[371]);
if(R.isMethod($field878)) {
$ans846 = $field878.full_meth(ED12,anf_arg877);
} else {
if(!(R.isFunction($field878))) {
R.ffi.throwNonFunApp(L[371],$field878);
}
$ans846 = $field878.app(anf_arg877);
}
break;
case 2: var anf_arg879 = $ans846;
$step843 = 3;
$al847 = L[372];
$field880 = R.getColonFieldLoc(ED12,"code",L[372]);
if(R.isMethod($field880)) {
$ans846 = $field880.full_meth(ED12,anf_arg879);
} else {
if(!(R.isFunction($field880))) {
R.ffi.throwNonFunApp(L[372],$field880);
}
$ans846 = $field880.app(anf_arg879);
}
break;
case 3: var anf_array_val885 = $ans846;
$step843 = 4;
$al847 = L[373];
$field881 = R.getColonFieldLoc(ED12,"text",L[373]);
if(R.isMethod($field881)) {
$ans846 = $field881.full_meth(ED12,("because:"));
} else {
if(!(R.isFunction($field881))) {
R.ffi.throwNonFunApp(L[373],$field881);
}
$ans846 = $field881.app(("because:"));
}
break;
case 4: var anf_array_val886 = $ans846;
var anf_arg882 = G(self845,"reason",L[374]);
$step843 = 5;
$al847 = L[375];
$field883 = R.getColonFieldLoc(ED12,"text",L[375]);
if(R.isMethod($field883)) {
$ans846 = $field883.full_meth(ED12,anf_arg882);
} else {
if(!(R.isFunction($field883))) {
R.ffi.throwNonFunApp(L[375],$field883);
}
$ans846 = $field883.app(anf_arg882);
}
break;
case 5: var anf_array_val887 = $ans846;
var anf_arg889 = [anf_array_val884,anf_array_val885,anf_array_val886,anf_array_val887];
$step843 = 6;
$al847 = L[368];
$field890 = R.getColonFieldLoc(anf_method_obj888,"make",L[368]);
if(R.isMethod($field890)) {
$ans846 = $field890.full_meth(anf_method_obj888,anf_arg889);
} else {
if(!(R.isFunction($field890))) {
R.ffi.throwNonFunApp(L[368],$field890);
}
$ans846 = $field890.app(anf_arg889);
}
break;
case 6: var anf_array_val891 = $ans846;
var anf_arg893 = [anf_array_val891];
$step843 = 7;
$al847 = L[367];
$field894 = R.getColonFieldLoc(anf_method_obj892,"make",L[367]);
if(R.isMethod($field894)) {
$ans846 = $field894.full_meth(anf_method_obj892,anf_arg893);
} else {
if(!(R.isFunction($field894))) {
R.ffi.throwNonFunApp(L[367],$field894);
}
$ans846 = $field894.app(anf_arg893);
}
break;
case 7: var anf_arg896 = $ans846;
$step843 = 8;
$al847 = L[376];
$field897 = R.getColonFieldLoc(ED12,"maybe-stack-loc",L[376]);
if(R.isMethod($field897)) {
$ans846 = $field897.full_meth(ED12,(0),(true),anf_arg895,anf_arg896);
} else {
if(!(R.isFunction($field897))) {
R.ffi.throwNonFunApp(L[376],$field897);
}
$ans846 = $field897.app((0),(true),anf_arg895,anf_arg896);
}
break;
case 8: ++R.GAS;
return $ans846;
default: throw "No case numbered " + $step843 + " in $temp_full844";
}
}
} catch($e898) {
if(R.isCont($e898) && ($step843 !== 8)) {
$e898.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al847,$temp_full844,$step843,[self845],[anf_array_val886,anf_array_val885,anf_array_val884,anf_method_obj888,anf_method_obj892,anf_arg895]);
}
if(R.isPyretException($e898)) {
$e898.pyretStack.push($al847);
}
throw $e898;
}
};
var anf_variant_member1241 = R.makeMethod0($temp_full844);
var $temp_full900 = function($self901) {
var $step899 = 0;
var $ans902 = D;
var $al903 = L[417];
try {
if(R.isActivationRecord($self901)) {
$step899 = $self901.step;
$al903 = $self901.from;
$ans902 = $self901.ans;
self901 = $self901.args[0];
anf_method_obj973 = $self901.vars[0];
anf_array_val977 = $self901.vars[1];
anf_method_obj967 = $self901.vars[2];
anf_array_val976 = $self901.vars[3];
anf_method_obj961 = $self901.vars[4];
anf_method_obj979 = $self901.vars[5];
within$error949 = $self901.vars[6];
value2906 = $self901.vars[7];
value1904 = $self901.vars[8];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[417],1,$t);
}
var self901 = $self901;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step899) {
case 0: var value1904 = G(self901,"value1",L[378]);
var value2906 = G(self901,"value2",L[379]);
$step899 = 1;
$al903 = L[416];
if(!(R.isFunction(is$number5))) {
R.ffi.throwNonFunApp($al903,is$number5);
}
$ans902 = is$number5.app(value1904);
break;
case 1: var anf_arg905 = $ans902;
$al903 = L[380];
var anf_if983 = R.checkWrapBoolean(anf_arg905);
if(R.isPyretTrue(anf_if983)) {
$step899 = 2;
} else {
$step899 = 4;
}
break;
case 2: $step899 = 3;
$al903 = L[381];
if(!(R.isFunction(is$number5))) {
R.ffi.throwNonFunApp($al903,is$number5);
}
$ans902 = is$number5.app(value2906);
break;
case 3: var anf_arg907 = $ans902;
$step899 = 5;
$al903 = L[380];
$ans902 = R.checkWrapBoolean(anf_arg907);
break;
case 4: $step899 = 5;
$ans902 = (false);
break;
case 5: var anf_arg908 = $ans902;
$al903 = L[382];
var anf_if982 = R.checkWrapBoolean(anf_arg908);
if(R.isPyretTrue(anf_if982)) {
$step899 = 6;
} else {
$step899 = 20;
}
break;
case 6: var within$error949 = {"$var":D};
var $temp_lam910 = function($message911) {
var $step909 = 0;
var $ans912 = D;
var $al913 = L[395];
try {
if(R.isActivationRecord($message911)) {
$step909 = $message911.step;
$al913 = $message911.from;
$ans912 = $message911.ans;
message911 = $message911.args[0];
anf_array_val935 = $message911.vars[0];
anf_array_val934 = $message911.vars[1];
anf_method_obj937 = $message911.vars[2];
anf_array_val942 = $message911.vars[3];
anf_method_obj926 = $message911.vars[4];
anf_array_val941 = $message911.vars[5];
anf_method_obj921 = $message911.vars[6];
anf_array_val940 = $message911.vars[7];
anf_method_obj916 = $message911.vars[8];
anf_method_obj944 = $message911.vars[9];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[395],1,$t);
}
var message911 = $message911;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step909) {
case 0: var anf_method_obj944 = G(ED12,"error",L[383]);
var anf_method_obj916 = G(ED12,"para",L[384]);
$step909 = 1;
$al913 = L[385];
$field914 = R.getColonFieldLoc(ED12,"text",L[385]);
if(R.isMethod($field914)) {
$ans912 = $field914.full_meth(ED12,message911);
} else {
if(!(R.isFunction($field914))) {
R.ffi.throwNonFunApp(L[385],$field914);
}
$ans912 = $field914.app(message911);
}
break;
case 1: var anf_array_val915 = $ans912;
var anf_arg917 = [anf_array_val915];
$step909 = 2;
$al913 = L[384];
$field918 = R.getColonFieldLoc(anf_method_obj916,"make",L[384]);
if(R.isMethod($field918)) {
$ans912 = $field918.full_meth(anf_method_obj916,anf_arg917);
} else {
if(!(R.isFunction($field918))) {
R.ffi.throwNonFunApp(L[384],$field918);
}
$ans912 = $field918.app(anf_arg917);
}
break;
case 2: var anf_array_val940 = $ans912;
var anf_method_obj921 = G(ED12,"para",L[386]);
$step909 = 3;
$al913 = L[387];
$field919 = R.getColonFieldLoc(ED12,"embed",L[387]);
if(R.isMethod($field919)) {
$ans912 = $field919.full_meth(ED12,value1904);
} else {
if(!(R.isFunction($field919))) {
R.ffi.throwNonFunApp(L[387],$field919);
}
$ans912 = $field919.app(value1904);
}
break;
case 3: var anf_array_val920 = $ans912;
var anf_arg922 = [anf_array_val920];
$step909 = 4;
$al913 = L[386];
$field923 = R.getColonFieldLoc(anf_method_obj921,"make",L[386]);
if(R.isMethod($field923)) {
$ans912 = $field923.full_meth(anf_method_obj921,anf_arg922);
} else {
if(!(R.isFunction($field923))) {
R.ffi.throwNonFunApp(L[386],$field923);
}
$ans912 = $field923.app(anf_arg922);
}
break;
case 4: var anf_array_val941 = $ans912;
var anf_method_obj926 = G(ED12,"para",L[388]);
$step909 = 5;
$al913 = L[389];
$field924 = R.getColonFieldLoc(ED12,"embed",L[389]);
if(R.isMethod($field924)) {
$ans912 = $field924.full_meth(ED12,value2906);
} else {
if(!(R.isFunction($field924))) {
R.ffi.throwNonFunApp(L[389],$field924);
}
$ans912 = $field924.app(value2906);
}
break;
case 5: var anf_array_val925 = $ans912;
var anf_arg927 = [anf_array_val925];
$step909 = 6;
$al913 = L[388];
$field928 = R.getColonFieldLoc(anf_method_obj926,"make",L[388]);
if(R.isMethod($field928)) {
$ans912 = $field928.full_meth(anf_method_obj926,anf_arg927);
} else {
if(!(R.isFunction($field928))) {
R.ffi.throwNonFunApp(L[388],$field928);
}
$ans912 = $field928.app(anf_arg927);
}
break;
case 6: var anf_array_val942 = $ans912;
var anf_method_obj937 = G(ED12,"para",L[390]);
$step909 = 7;
$al913 = L[391];
$field929 = R.getColonFieldLoc(ED12,"text",L[391]);
if(R.isMethod($field929)) {
$ans912 = $field929.full_meth(ED12,("Consider using the "));
} else {
if(!(R.isFunction($field929))) {
R.ffi.throwNonFunApp(L[391],$field929);
}
$ans912 = $field929.app(("Consider using the "));
}
break;
case 7: var anf_array_val934 = $ans912;
$step909 = 8;
$al913 = L[392];
$field930 = R.getColonFieldLoc(ED12,"text",L[392]);
if(R.isMethod($field930)) {
$ans912 = $field930.full_meth(ED12,("within"));
} else {
if(!(R.isFunction($field930))) {
R.ffi.throwNonFunApp(L[392],$field930);
}
$ans912 = $field930.app(("within"));
}
break;
case 8: var anf_arg931 = $ans912;
$step909 = 9;
$al913 = L[393];
$field932 = R.getColonFieldLoc(ED12,"code",L[393]);
if(R.isMethod($field932)) {
$ans912 = $field932.full_meth(ED12,anf_arg931);
} else {
if(!(R.isFunction($field932))) {
R.ffi.throwNonFunApp(L[393],$field932);
}
$ans912 = $field932.app(anf_arg931);
}
break;
case 9: var anf_array_val935 = $ans912;
$step909 = 10;
$al913 = L[394];
$field933 = R.getColonFieldLoc(ED12,"text",L[394]);
if(R.isMethod($field933)) {
$ans912 = $field933.full_meth(ED12,(" function to compare them instead."));
} else {
if(!(R.isFunction($field933))) {
R.ffi.throwNonFunApp(L[394],$field933);
}
$ans912 = $field933.app((" function to compare them instead."));
}
break;
case 10: var anf_array_val936 = $ans912;
var anf_arg938 = [anf_array_val934,anf_array_val935,anf_array_val936];
$step909 = 11;
$al913 = L[390];
$field939 = R.getColonFieldLoc(anf_method_obj937,"make",L[390]);
if(R.isMethod($field939)) {
$ans912 = $field939.full_meth(anf_method_obj937,anf_arg938);
} else {
if(!(R.isFunction($field939))) {
R.ffi.throwNonFunApp(L[390],$field939);
}
$ans912 = $field939.app(anf_arg938);
}
break;
case 11: var anf_array_val943 = $ans912;
var anf_arg945 = [anf_array_val940,anf_array_val941,anf_array_val942,anf_array_val943];
$step909 = 12;
$al913 = L[383];
$field946 = R.getColonFieldLoc(anf_method_obj944,"make",L[383]);
if(R.isMethod($field946)) {
$ans912 = $field946.full_meth(anf_method_obj944,anf_arg945);
} else {
if(!(R.isFunction($field946))) {
R.ffi.throwNonFunApp(L[383],$field946);
}
$ans912 = $field946.app(anf_arg945);
}
break;
case 12: ++R.GAS;
return $ans912;
default: throw "No case numbered " + $step909 + " in $temp_lam910";
}
}
} catch($e947) {
if(R.isCont($e947) && ($step909 !== 12)) {
$e947.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al913,$temp_lam910,$step909,[message911],[anf_array_val935,anf_array_val934,anf_method_obj937,anf_array_val942,anf_method_obj926,anf_array_val941,anf_method_obj921,anf_array_val940,anf_method_obj916,anf_method_obj944]);
}
if(R.isPyretException($e947)) {
$e947.pyretStack.push($al913);
}
throw $e947;
}
};
var anf_assign948 = R.makeFunction($temp_lam910);
within$error949.$var = anf_assign948;
$step899 = 7;
$al903 = L[406];
if(!(R.isFunction(num$is$roughnum6))) {
R.ffi.throwNonFunApp($al903,num$is$roughnum6);
}
$ans902 = num$is$roughnum6.app(value1904);
break;
case 7: var anf_arg950 = $ans902;
$al903 = L[396];
var anf_if958 = R.checkWrapBoolean(anf_arg950);
if(R.isPyretTrue(anf_if958)) {
$step899 = 8;
} else {
$step899 = 10;
}
break;
case 8: $step899 = 9;
$al903 = L[397];
if(!(R.isFunction(num$is$roughnum6))) {
R.ffi.throwNonFunApp($al903,num$is$roughnum6);
}
$ans902 = num$is$roughnum6.app(value2906);
break;
case 9: var anf_arg951 = $ans902;
$step899 = 11;
$al903 = L[396];
$ans902 = R.checkWrapBoolean(anf_arg951);
break;
case 10: $step899 = 11;
$ans902 = (false);
break;
case 11: var anf_arg952 = $ans902;
$al903 = L[396];
var anf_if957 = R.checkWrapBoolean(anf_arg952);
if(R.isPyretTrue(anf_if957)) {
$step899 = 12;
} else {
$step899 = 13;
}
break;
case 12: $step899 = 27;
$al903 = L[398];
if(!(R.isFunction(within$error949.$var))) {
R.ffi.throwNonFunApp($al903,within$error949.$var);
}
$ans902 = within$error949.$var.app(("Attempted to compare two Roughnums for equality, which is not allowed:"));
break;
case 13: $step899 = 14;
$al903 = L[405];
if(!(R.isFunction(num$is$roughnum6))) {
R.ffi.throwNonFunApp($al903,num$is$roughnum6);
}
$ans902 = num$is$roughnum6.app(value1904);
break;
case 14: var anf_arg953 = $ans902;
$al903 = L[399];
var anf_if956 = R.checkWrapBoolean(anf_arg953);
if(R.isPyretTrue(anf_if956)) {
$step899 = 15;
} else {
$step899 = 16;
}
break;
case 15: $step899 = 27;
$al903 = L[400];
if(!(R.isFunction(within$error949.$var))) {
R.ffi.throwNonFunApp($al903,within$error949.$var);
}
$ans902 = within$error949.$var.app(("Attempted to compare a Roughnum to an Exactnum for equality, which is not allowed:"));
break;
case 16: $step899 = 17;
$al903 = L[404];
if(!(R.isFunction(num$is$roughnum6))) {
R.ffi.throwNonFunApp($al903,num$is$roughnum6);
}
$ans902 = num$is$roughnum6.app(value2906);
break;
case 17: var anf_arg954 = $ans902;
$al903 = L[401];
var anf_if955 = R.checkWrapBoolean(anf_arg954);
if(R.isPyretTrue(anf_if955)) {
$step899 = 18;
} else {
$step899 = 19;
}
break;
case 18: $step899 = 27;
$al903 = L[402];
if(!(R.isFunction(within$error949.$var))) {
R.ffi.throwNonFunApp($al903,within$error949.$var);
}
$ans902 = within$error949.$var.app(("Attempted to compare an Exactnum to a Roughnum for equality, which is not allowed:"));
break;
case 19: $step899 = 27;
$al903 = L[403];
$ans902 = R.throwNoBranchesMatched(L[403],("if"));
break;
case 20: var anf_method_obj979 = G(ED12,"error",L[407]);
var anf_method_obj961 = G(ED12,"para",L[408]);
$step899 = 21;
$al903 = L[409];
$field959 = R.getColonFieldLoc(ED12,"text",L[409]);
if(R.isMethod($field959)) {
$ans902 = $field959.full_meth(ED12,("Attempted to compare two incomparable values: "));
} else {
if(!(R.isFunction($field959))) {
R.ffi.throwNonFunApp(L[409],$field959);
}
$ans902 = $field959.app(("Attempted to compare two incomparable values: "));
}
break;
case 21: var anf_array_val960 = $ans902;
var anf_arg962 = [anf_array_val960];
$step899 = 22;
$al903 = L[408];
$field963 = R.getColonFieldLoc(anf_method_obj961,"make",L[408]);
if(R.isMethod($field963)) {
$ans902 = $field963.full_meth(anf_method_obj961,anf_arg962);
} else {
if(!(R.isFunction($field963))) {
R.ffi.throwNonFunApp(L[408],$field963);
}
$ans902 = $field963.app(anf_arg962);
}
break;
case 22: var anf_array_val976 = $ans902;
var anf_method_obj967 = G(ED12,"para",L[410]);
var anf_arg964 = G(self901,"value1",L[411]);
$step899 = 23;
$al903 = L[412];
$field965 = R.getColonFieldLoc(ED12,"embed",L[412]);
if(R.isMethod($field965)) {
$ans902 = $field965.full_meth(ED12,anf_arg964);
} else {
if(!(R.isFunction($field965))) {
R.ffi.throwNonFunApp(L[412],$field965);
}
$ans902 = $field965.app(anf_arg964);
}
break;
case 23: var anf_array_val966 = $ans902;
var anf_arg968 = [anf_array_val966];
$step899 = 24;
$al903 = L[410];
$field969 = R.getColonFieldLoc(anf_method_obj967,"make",L[410]);
if(R.isMethod($field969)) {
$ans902 = $field969.full_meth(anf_method_obj967,anf_arg968);
} else {
if(!(R.isFunction($field969))) {
R.ffi.throwNonFunApp(L[410],$field969);
}
$ans902 = $field969.app(anf_arg968);
}
break;
case 24: var anf_array_val977 = $ans902;
var anf_method_obj973 = G(ED12,"para",L[413]);
var anf_arg970 = G(self901,"value2",L[414]);
$step899 = 25;
$al903 = L[415];
$field971 = R.getColonFieldLoc(ED12,"embed",L[415]);
if(R.isMethod($field971)) {
$ans902 = $field971.full_meth(ED12,anf_arg970);
} else {
if(!(R.isFunction($field971))) {
R.ffi.throwNonFunApp(L[415],$field971);
}
$ans902 = $field971.app(anf_arg970);
}
break;
case 25: var anf_array_val972 = $ans902;
var anf_arg974 = [anf_array_val972];
$step899 = 26;
$al903 = L[413];
$field975 = R.getColonFieldLoc(anf_method_obj973,"make",L[413]);
if(R.isMethod($field975)) {
$ans902 = $field975.full_meth(anf_method_obj973,anf_arg974);
} else {
if(!(R.isFunction($field975))) {
R.ffi.throwNonFunApp(L[413],$field975);
}
$ans902 = $field975.app(anf_arg974);
}
break;
case 26: var anf_array_val978 = $ans902;
var anf_arg980 = [anf_array_val976,anf_array_val977,anf_array_val978];
$step899 = 27;
$al903 = L[407];
$field981 = R.getColonFieldLoc(anf_method_obj979,"make",L[407]);
if(R.isMethod($field981)) {
$ans902 = $field981.full_meth(anf_method_obj979,anf_arg980);
} else {
if(!(R.isFunction($field981))) {
R.ffi.throwNonFunApp(L[407],$field981);
}
$ans902 = $field981.app(anf_arg980);
}
break;
case 27: ++R.GAS;
return $ans902;
default: throw "No case numbered " + $step899 + " in $temp_full900";
}
}
} catch($e984) {
if(R.isCont($e984) && ($step899 !== 27)) {
$e984.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al903,$temp_full900,$step899,[self901],[anf_method_obj973,anf_array_val977,anf_method_obj967,anf_array_val976,anf_method_obj961,anf_method_obj979,within$error949,value2906,value1904]);
}
if(R.isPyretException($e984)) {
$e984.pyretStack.push($al903);
}
throw $e984;
}
};
var anf_variant_member1254 = R.makeMethod0($temp_full900);
var $temp_full986 = function($self987) {
var $step985 = 0;
var $ans988 = D;
var $al989 = L[420];
try {
if(R.isActivationRecord($self987)) {
$step985 = $self987.step;
$al989 = $self987.from;
$ans988 = $self987.ans;
self987 = $self987.args[0];
anf_method_obj992 = $self987.vars[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[420],1,$t);
}
var self987 = $self987;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step985) {
case 0: var anf_method_obj992 = G(ED12,"error",L[418]);
$step985 = 1;
$al989 = L[419];
$field990 = R.getColonFieldLoc(ED12,"text",L[419]);
if(R.isMethod($field990)) {
$ans988 = $field990.full_meth(ED12,("Program stopped by user"));
} else {
if(!(R.isFunction($field990))) {
R.ffi.throwNonFunApp(L[419],$field990);
}
$ans988 = $field990.app(("Program stopped by user"));
}
break;
case 1: var anf_array_val991 = $ans988;
var anf_arg993 = [anf_array_val991];
$step985 = 2;
$al989 = L[418];
$field994 = R.getColonFieldLoc(anf_method_obj992,"make",L[418]);
if(R.isMethod($field994)) {
$ans988 = $field994.full_meth(anf_method_obj992,anf_arg993);
} else {
if(!(R.isFunction($field994))) {
R.ffi.throwNonFunApp(L[418],$field994);
}
$ans988 = $field994.app(anf_arg993);
}
break;
case 2: ++R.GAS;
return $ans988;
default: throw "No case numbered " + $step985 + " in $temp_full986";
}
}
} catch($e995) {
if(R.isCont($e995) && ($step985 !== 2)) {
$e995.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al989,$temp_full986,$step985,[self987],[anf_method_obj992]);
}
if(R.isPyretException($e995)) {
$e995.pyretStack.push($al989);
}
throw $e995;
}
};
var anf_singleton_variant_member1266 = R.makeMethod0($temp_full986);
var $temp_full997 = function($self998) {
var $step996 = 0;
var $ans999 = D;
var $al1000 = L[423];
try {
if(R.isActivationRecord($self998)) {
$step996 = $self998.step;
$al1000 = $self998.from;
$ans999 = $self998.ans;
self998 = $self998.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[423],1,$t);
}
var self998 = $self998;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step996) {
case 0: var anf_arg1001 = G(self998,"value",L[421]);
$step996 = 1;
$al1000 = L[422];
$field1002 = R.getColonFieldLoc(ED12,"embed",L[422]);
if(R.isMethod($field1002)) {
$ans999 = $field1002.full_meth(ED12,anf_arg1001);
} else {
if(!(R.isFunction($field1002))) {
R.ffi.throwNonFunApp(L[422],$field1002);
}
$ans999 = $field1002.app(anf_arg1001);
}
break;
case 1: ++R.GAS;
return $ans999;
default: throw "No case numbered " + $step996 + " in $temp_full997";
}
}
} catch($e1003) {
if(R.isCont($e1003) && ($step996 !== 1)) {
$e1003.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1000,$temp_full997,$step996,[self998],[]);
}
if(R.isPyretException($e1003)) {
$e1003.pyretStack.push($al1000);
}
throw $e1003;
}
};
var anf_variant_member1274 = R.makeMethod0($temp_full997);
var $message$exception_getfields1010 = function(f) {
return f(this.dict["message"]);
};
var $message$exception_getfieldsref1008 = function(f,refmask) {
return f(R.derefField(this.dict["message"],false,refmask[0]));
};
var $message$exception_mutablemask1009 = [false];
var $message$exception$base1004 = {"$fieldNames":["message"],
"render-reason":anf_variant_member1007,
"_match":R.makeMatch("message-exception",1)};
var $message$exception$brands1006 = {"$brand$message$exception":true};
$message$exception$brands1006[RuntimeError21._brand] = true;
var message$exception1012 = R.makeVariantConstructor(L[426],function() {
return [$type$String7];
},["message1013"],[L[425]],[false],["message1013"],$message$exception_mutablemask1009,$message$exception$base1004,$message$exception$brands1006,"message-exception",$message$exception_getfieldsref1008,$message$exception_getfields1010,$message$exception$base1004);
var $no$cases$matched_getfields1020 = function(f) {
return f(this.dict["loc"],this.dict["val"]);
};
var $no$cases$matched_getfieldsref1018 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["val"],false,refmask[1]));
};
var $no$cases$matched_mutablemask1019 = [false,false];
var $no$cases$matched$base1014 = {"$fieldNames":["loc","val"],
"render-reason":anf_variant_member1017,
"_match":R.makeMatch("no-cases-matched",2)};
var $no$cases$matched$brands1016 = {"$brand$no$cases$matched":true};
$no$cases$matched$brands1016[RuntimeError21._brand] = true;
var no$cases$matched1022 = R.makeVariantConstructor(L[428],function() {
return [];
},[],[],[false,false],["loc1023","val1024"],$no$cases$matched_mutablemask1019,$no$cases$matched$base1014,$no$cases$matched$brands1016,"no-cases-matched",$no$cases$matched_getfieldsref1018,$no$cases$matched_getfields1020,$no$cases$matched$base1014);
var $no$branches$matched_getfields1031 = function(f) {
return f(this.dict["loc"],this.dict["expression"]);
};
var $no$branches$matched_getfieldsref1029 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["expression"],false,refmask[1]));
};
var $no$branches$matched_mutablemask1030 = [false,false];
var $no$branches$matched$base1025 = {"$fieldNames":["loc","expression"],
"render-reason":anf_variant_member1028,
"_match":R.makeMatch("no-branches-matched",2)};
var $no$branches$matched$brands1027 = {"$brand$no$branches$matched":true};
$no$branches$matched$brands1027[RuntimeError21._brand] = true;
var no$branches$matched1033 = R.makeVariantConstructor(L[431],function() {
return [$type$String7];
},["expression1034"],[L[430]],[false,false],["loc1035","expression1034"],$no$branches$matched_mutablemask1030,$no$branches$matched$base1025,$no$branches$matched$brands1027,"no-branches-matched",$no$branches$matched_getfieldsref1029,$no$branches$matched_getfields1031,$no$branches$matched$base1025);
var $internal$error_getfields1042 = function(f) {
return f(this.dict["message"],this.dict["info-args"]);
};
var $internal$error_getfieldsref1040 = function(f,refmask) {
return f(R.derefField(this.dict["message"],false,refmask[0]),R.derefField(this.dict["info-args"],false,refmask[1]));
};
var $internal$error_mutablemask1041 = [false,false];
var $internal$error$base1036 = {"$fieldNames":["message","info-args"],
"render-reason":anf_variant_member1039,
"_match":R.makeMatch("internal-error",2)};
var $internal$error$brands1038 = {"$brand$internal$error":true};
$internal$error$brands1038[RuntimeError21._brand] = true;
var internal$error1044 = R.makeVariantConstructor(L[433],function() {
return [];
},[],[],[false,false],["message1045","info$args1046"],$internal$error_mutablemask1041,$internal$error$base1036,$internal$error$brands1038,"internal-error",$internal$error_getfieldsref1040,$internal$error_getfields1042,$internal$error$base1036);
var $field$not$found_getfields1053 = function(f) {
return f(this.dict["loc"],this.dict["obj"],this.dict["field"]);
};
var $field$not$found_getfieldsref1051 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["obj"],false,refmask[1]),R.derefField(this.dict["field"],false,refmask[2]));
};
var $field$not$found_mutablemask1052 = [false,false,false];
var $field$not$found$base1047 = {"$fieldNames":["loc","obj","field"],
"render-reason":anf_variant_member1050,
"_match":R.makeMatch("field-not-found",3)};
var $field$not$found$brands1049 = {"$brand$field$not$found":true};
$field$not$found$brands1049[RuntimeError21._brand] = true;
var field$not$found1055 = R.makeVariantConstructor(L[436],function() {
return [$type$String7];
},["field1056"],[L[435]],[false,false,false],["loc1057","obj1058","field1056"],$field$not$found_mutablemask1052,$field$not$found$base1047,$field$not$found$brands1049,"field-not-found",$field$not$found_getfieldsref1051,$field$not$found_getfields1053,$field$not$found$base1047);
var $lookup$non$object_getfields1065 = function(f) {
return f(this.dict["loc"],this.dict["non-obj"],this.dict["field"]);
};
var $lookup$non$object_getfieldsref1063 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["non-obj"],false,refmask[1]),R.derefField(this.dict["field"],false,refmask[2]));
};
var $lookup$non$object_mutablemask1064 = [false,false,false];
var $lookup$non$object$base1059 = {"$fieldNames":["loc","non-obj","field"],
"render-reason":anf_variant_member1062,
"_match":R.makeMatch("lookup-non-object",3)};
var $lookup$non$object$brands1061 = {"$brand$lookup$non$object":true};
$lookup$non$object$brands1061[RuntimeError21._brand] = true;
var lookup$non$object1067 = R.makeVariantConstructor(L[439],function() {
return [$type$String7];
},["field1068"],[L[438]],[false,false,false],["loc1069","non$obj1070","field1068"],$lookup$non$object_mutablemask1064,$lookup$non$object$base1059,$lookup$non$object$brands1061,"lookup-non-object",$lookup$non$object_getfieldsref1063,$lookup$non$object_getfields1065,$lookup$non$object$base1059);
var $extend$non$object_getfields1077 = function(f) {
return f(this.dict["loc"],this.dict["non-obj"]);
};
var $extend$non$object_getfieldsref1075 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["non-obj"],false,refmask[1]));
};
var $extend$non$object_mutablemask1076 = [false,false];
var $extend$non$object$base1071 = {"$fieldNames":["loc","non-obj"],
"render-reason":anf_variant_member1074,
"_match":R.makeMatch("extend-non-object",2)};
var $extend$non$object$brands1073 = {"$brand$extend$non$object":true};
$extend$non$object$brands1073[RuntimeError21._brand] = true;
var extend$non$object1079 = R.makeVariantConstructor(L[441],function() {
return [];
},[],[],[false,false],["loc1080","non$obj1081"],$extend$non$object_mutablemask1076,$extend$non$object$base1071,$extend$non$object$brands1073,"extend-non-object",$extend$non$object_getfieldsref1075,$extend$non$object_getfields1077,$extend$non$object$base1071);
var $non$boolean$condition_getfields1088 = function(f) {
return f(this.dict["loc"],this.dict["typ"],this.dict["value"]);
};
var $non$boolean$condition_getfieldsref1086 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["typ"],false,refmask[1]),R.derefField(this.dict["value"],false,refmask[2]));
};
var $non$boolean$condition_mutablemask1087 = [false,false,false];
var $non$boolean$condition$base1082 = {"$fieldNames":["loc","typ","value"],
"render-reason":anf_variant_member1085,
"_match":R.makeMatch("non-boolean-condition",3)};
var $non$boolean$condition$brands1084 = {"$brand$non$boolean$condition":true};
$non$boolean$condition$brands1084[RuntimeError21._brand] = true;
var non$boolean$condition1090 = R.makeVariantConstructor(L[443],function() {
return [];
},[],[],[false,false,false],["loc1091","typ1092","value1093"],$non$boolean$condition_mutablemask1087,$non$boolean$condition$base1082,$non$boolean$condition$brands1084,"non-boolean-condition",$non$boolean$condition_getfieldsref1086,$non$boolean$condition_getfields1088,$non$boolean$condition$base1082);
var $non$boolean$op_getfields1100 = function(f) {
return f(this.dict["loc"],this.dict["position"],this.dict["typ"],this.dict["value"]);
};
var $non$boolean$op_getfieldsref1098 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["position"],false,refmask[1]),R.derefField(this.dict["typ"],false,refmask[2]),R.derefField(this.dict["value"],false,refmask[3]));
};
var $non$boolean$op_mutablemask1099 = [false,false,false,false];
var $non$boolean$op$base1094 = {"$fieldNames":["loc","position","typ","value"],
"render-reason":anf_variant_member1097,
"_match":R.makeMatch("non-boolean-op",4)};
var $non$boolean$op$brands1096 = {"$brand$non$boolean$op":true};
$non$boolean$op$brands1096[RuntimeError21._brand] = true;
var non$boolean$op1102 = R.makeVariantConstructor(L[445],function() {
return [];
},[],[],[false,false,false,false],["loc1103","position1104","typ1105","value1106"],$non$boolean$op_mutablemask1099,$non$boolean$op$base1094,$non$boolean$op$brands1096,"non-boolean-op",$non$boolean$op_getfieldsref1098,$non$boolean$op_getfields1100,$non$boolean$op$base1094);
var $generic$type$mismatch_getfields1113 = function(f) {
return f(this.dict["val"],this.dict["typ"]);
};
var $generic$type$mismatch_getfieldsref1111 = function(f,refmask) {
return f(R.derefField(this.dict["val"],false,refmask[0]),R.derefField(this.dict["typ"],false,refmask[1]));
};
var $generic$type$mismatch_mutablemask1112 = [false,false];
var $generic$type$mismatch$base1107 = {"$fieldNames":["val","typ"],
"render-reason":anf_variant_member1110,
"_match":R.makeMatch("generic-type-mismatch",2)};
var $generic$type$mismatch$brands1109 = {"$brand$generic$type$mismatch":true};
$generic$type$mismatch$brands1109[RuntimeError21._brand] = true;
var generic$type$mismatch1115 = R.makeVariantConstructor(L[448],function() {
return [$type$String7];
},["typ1116"],[L[447]],[false,false],["val1117","typ1116"],$generic$type$mismatch_mutablemask1112,$generic$type$mismatch$base1107,$generic$type$mismatch$brands1109,"generic-type-mismatch",$generic$type$mismatch_getfieldsref1111,$generic$type$mismatch_getfields1113,$generic$type$mismatch$base1107);
var $outside$numeric$range_getfields1124 = function(f) {
return f(this.dict["val"],this.dict["low"],this.dict["high"]);
};
var $outside$numeric$range_getfieldsref1122 = function(f,refmask) {
return f(R.derefField(this.dict["val"],false,refmask[0]),R.derefField(this.dict["low"],false,refmask[1]),R.derefField(this.dict["high"],false,refmask[2]));
};
var $outside$numeric$range_mutablemask1123 = [false,false,false];
var $outside$numeric$range$base1118 = {"$fieldNames":["val","low","high"],
"render-reason":anf_variant_member1121,
"_match":R.makeMatch("outside-numeric-range",3)};
var $outside$numeric$range$brands1120 = {"$brand$outside$numeric$range":true};
$outside$numeric$range$brands1120[RuntimeError21._brand] = true;
var outside$numeric$range1126 = R.makeVariantConstructor(L[450],function() {
return [];
},[],[],[false,false,false],["val1127","low1128","high1129"],$outside$numeric$range_mutablemask1123,$outside$numeric$range$base1118,$outside$numeric$range$brands1120,"outside-numeric-range",$outside$numeric$range_getfieldsref1122,$outside$numeric$range_getfields1124,$outside$numeric$range$base1118);
var $num$string$binop$error_getfields1136 = function(f) {
return f(this.dict["val1"],this.dict["val2"],this.dict["opname"],this.dict["opdesc"],this.dict["methodname"]);
};
var $num$string$binop$error_getfieldsref1134 = function(f,refmask) {
return f(R.derefField(this.dict["val1"],false,refmask[0]),R.derefField(this.dict["val2"],false,refmask[1]),R.derefField(this.dict["opname"],false,refmask[2]),R.derefField(this.dict["opdesc"],false,refmask[3]),R.derefField(this.dict["methodname"],false,refmask[4]));
};
var $num$string$binop$error_mutablemask1135 = [false,false,false,false,false];
var $num$string$binop$error$base1130 = {"$fieldNames":["val1","val2","opname","opdesc","methodname"],
"render-reason":anf_variant_member1133,
"_match":R.makeMatch("num-string-binop-error",5)};
var $num$string$binop$error$brands1132 = {"$brand$num$string$binop$error":true};
$num$string$binop$error$brands1132[RuntimeError21._brand] = true;
var num$string$binop$error1138 = R.makeVariantConstructor(L[452],function() {
return [];
},[],[],[false,false,false,false,false],["val11139","val21140","opname1141","opdesc1142","methodname1143"],$num$string$binop$error_mutablemask1135,$num$string$binop$error$base1130,$num$string$binop$error$brands1132,"num-string-binop-error",$num$string$binop$error_getfieldsref1134,$num$string$binop$error_getfields1136,$num$string$binop$error$base1130);
var $numeric$binop$error_getfields1150 = function(f) {
return f(this.dict["val1"],this.dict["val2"],this.dict["opname"],this.dict["methodname"]);
};
var $numeric$binop$error_getfieldsref1148 = function(f,refmask) {
return f(R.derefField(this.dict["val1"],false,refmask[0]),R.derefField(this.dict["val2"],false,refmask[1]),R.derefField(this.dict["opname"],false,refmask[2]),R.derefField(this.dict["methodname"],false,refmask[3]));
};
var $numeric$binop$error_mutablemask1149 = [false,false,false,false];
var $numeric$binop$error$base1144 = {"$fieldNames":["val1","val2","opname","methodname"],
"render-reason":anf_variant_member1147,
"_match":R.makeMatch("numeric-binop-error",4)};
var $numeric$binop$error$brands1146 = {"$brand$numeric$binop$error":true};
$numeric$binop$error$brands1146[RuntimeError21._brand] = true;
var numeric$binop$error1152 = R.makeVariantConstructor(L[454],function() {
return [];
},[],[],[false,false,false,false],["val11153","val21154","opname1155","methodname1156"],$numeric$binop$error_mutablemask1149,$numeric$binop$error$base1144,$numeric$binop$error$brands1146,"numeric-binop-error",$numeric$binop$error_getfieldsref1148,$numeric$binop$error_getfields1150,$numeric$binop$error$base1144);
var $cases$arity$mismatch_getfields1163 = function(f) {
return f(this.dict["branch-loc"],this.dict["num-args"],this.dict["actual-arity"]);
};
var $cases$arity$mismatch_getfieldsref1161 = function(f,refmask) {
return f(R.derefField(this.dict["branch-loc"],false,refmask[0]),R.derefField(this.dict["num-args"],false,refmask[1]),R.derefField(this.dict["actual-arity"],false,refmask[2]));
};
var $cases$arity$mismatch_mutablemask1162 = [false,false,false];
var $cases$arity$mismatch$base1157 = {"$fieldNames":["branch-loc","num-args","actual-arity"],
"render-reason":anf_variant_member1160,
"_match":R.makeMatch("cases-arity-mismatch",3)};
var $cases$arity$mismatch$brands1159 = {"$brand$cases$arity$mismatch":true};
$cases$arity$mismatch$brands1159[RuntimeError21._brand] = true;
var cases$arity$mismatch1165 = R.makeVariantConstructor(L[456],function() {
return [];
},[],[],[false,false,false],["branch$loc1166","num$args1167","actual$arity1168"],$cases$arity$mismatch_mutablemask1162,$cases$arity$mismatch$base1157,$cases$arity$mismatch$brands1159,"cases-arity-mismatch",$cases$arity$mismatch_getfieldsref1161,$cases$arity$mismatch_getfields1163,$cases$arity$mismatch$base1157);
var $cases$singleton$mismatch_getfields1175 = function(f) {
return f(this.dict["branch-loc"],this.dict["should-be-singleton"]);
};
var $cases$singleton$mismatch_getfieldsref1173 = function(f,refmask) {
return f(R.derefField(this.dict["branch-loc"],false,refmask[0]),R.derefField(this.dict["should-be-singleton"],false,refmask[1]));
};
var $cases$singleton$mismatch_mutablemask1174 = [false,false];
var $cases$singleton$mismatch$base1169 = {"$fieldNames":["branch-loc","should-be-singleton"],
"render-reason":anf_variant_member1172,
"_match":R.makeMatch("cases-singleton-mismatch",2)};
var $cases$singleton$mismatch$brands1171 = {"$brand$cases$singleton$mismatch":true};
$cases$singleton$mismatch$brands1171[RuntimeError21._brand] = true;
var cases$singleton$mismatch1177 = R.makeVariantConstructor(L[459],function() {
return [$type$Boolean9];
},["should$be$singleton1178"],[L[458]],[false,false],["branch$loc1179","should$be$singleton1178"],$cases$singleton$mismatch_mutablemask1174,$cases$singleton$mismatch$base1169,$cases$singleton$mismatch$brands1171,"cases-singleton-mismatch",$cases$singleton$mismatch_getfieldsref1173,$cases$singleton$mismatch_getfields1175,$cases$singleton$mismatch$base1169);
var $arity$mismatch_getfields1186 = function(f) {
return f(this.dict["fun-loc"],this.dict["expected-arity"],this.dict["args"]);
};
var $arity$mismatch_getfieldsref1184 = function(f,refmask) {
return f(R.derefField(this.dict["fun-loc"],false,refmask[0]),R.derefField(this.dict["expected-arity"],false,refmask[1]),R.derefField(this.dict["args"],false,refmask[2]));
};
var $arity$mismatch_mutablemask1185 = [false,false,false];
var $arity$mismatch$base1180 = {"$fieldNames":["fun-loc","expected-arity","args"],
"render-reason":anf_variant_member1183,
"_match":R.makeMatch("arity-mismatch",3)};
var $arity$mismatch$brands1182 = {"$brand$arity$mismatch":true};
$arity$mismatch$brands1182[RuntimeError21._brand] = true;
var arity$mismatch1188 = R.makeVariantConstructor(L[461],function() {
return [];
},[],[],[false,false,false],["fun$loc1189","expected$arity1190","args1191"],$arity$mismatch_mutablemask1185,$arity$mismatch$base1180,$arity$mismatch$brands1182,"arity-mismatch",$arity$mismatch_getfieldsref1184,$arity$mismatch_getfields1186,$arity$mismatch$base1180);
var $non$function$app_getfields1198 = function(f) {
return f(this.dict["loc"],this.dict["non-fun-val"]);
};
var $non$function$app_getfieldsref1196 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["non-fun-val"],false,refmask[1]));
};
var $non$function$app_mutablemask1197 = [false,false];
var $non$function$app$base1192 = {"$fieldNames":["loc","non-fun-val"],
"render-reason":anf_variant_member1195,
"_match":R.makeMatch("non-function-app",2)};
var $non$function$app$brands1194 = {"$brand$non$function$app":true};
$non$function$app$brands1194[RuntimeError21._brand] = true;
var non$function$app1200 = R.makeVariantConstructor(L[463],function() {
return [];
},[],[],[false,false],["loc1201","non$fun$val1202"],$non$function$app_mutablemask1197,$non$function$app$base1192,$non$function$app$brands1194,"non-function-app",$non$function$app_getfieldsref1196,$non$function$app_getfields1198,$non$function$app$base1192);
var $bad$app_getfields1209 = function(f) {
return f(this.dict["loc"],this.dict["fun-name"],this.dict["message"],this.dict["arg-position"],this.dict["arg-val"]);
};
var $bad$app_getfieldsref1207 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["fun-name"],false,refmask[1]),R.derefField(this.dict["message"],false,refmask[2]),R.derefField(this.dict["arg-position"],false,refmask[3]),R.derefField(this.dict["arg-val"],false,refmask[4]));
};
var $bad$app_mutablemask1208 = [false,false,false,false,false];
var $bad$app$base1203 = {"$fieldNames":["loc","fun-name","message","arg-position","arg-val"],
"render-reason":anf_variant_member1206,
"_match":R.makeMatch("bad-app",5)};
var $bad$app$brands1205 = {"$brand$bad$app":true};
$bad$app$brands1205[RuntimeError21._brand] = true;
var bad$app1211 = R.makeVariantConstructor(L[468],function() {
return [$type$String7,$type$String7,$type$Number8];
},["fun$name1212","message1213","arg$position1214"],[L[465],L[466],L[467]],[false,false,false,false,false],["loc1215","fun$name1212","message1213","arg$position1214","arg$val1216"],$bad$app_mutablemask1208,$bad$app$base1203,$bad$app$brands1205,"bad-app",$bad$app_getfieldsref1207,$bad$app_getfields1209,$bad$app$base1203);
var $uninitialized$id_getfields1223 = function(f) {
return f(this.dict["loc"],this.dict["name"]);
};
var $uninitialized$id_getfieldsref1221 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["name"],false,refmask[1]));
};
var $uninitialized$id_mutablemask1222 = [false,false];
var $uninitialized$id$base1217 = {"$fieldNames":["loc","name"],
"render-reason":anf_variant_member1220,
"_match":R.makeMatch("uninitialized-id",2)};
var $uninitialized$id$brands1219 = {"$brand$uninitialized$id":true};
$uninitialized$id$brands1219[RuntimeError21._brand] = true;
var uninitialized$id1225 = R.makeVariantConstructor(L[471],function() {
return [$type$String7];
},["name1226"],[L[470]],[false,false],["loc1227","name1226"],$uninitialized$id_mutablemask1222,$uninitialized$id$base1217,$uninitialized$id$brands1219,"uninitialized-id",$uninitialized$id_getfieldsref1221,$uninitialized$id_getfields1223,$uninitialized$id$base1217);
var $module$load$failure_getfields1234 = function(f) {
return f(this.dict["names"]);
};
var $module$load$failure_getfieldsref1232 = function(f,refmask) {
return f(R.derefField(this.dict["names"],false,refmask[0]));
};
var $module$load$failure_mutablemask1233 = [false];
var $module$load$failure$base1228 = {"$fieldNames":["names"],
"render-reason":anf_variant_member1231,
"_match":R.makeMatch("module-load-failure",1)};
var $module$load$failure$brands1230 = {"$brand$module$load$failure":true};
$module$load$failure$brands1230[RuntimeError21._brand] = true;
var module$load$failure1236 = R.makeVariantConstructor(L[473],function() {
return [];
},[],[],[false],["names1237"],$module$load$failure_mutablemask1233,$module$load$failure$base1228,$module$load$failure$brands1230,"module-load-failure",$module$load$failure_getfieldsref1232,$module$load$failure_getfields1234,$module$load$failure$base1228);
var $invalid$array$index_getfields1244 = function(f) {
return f(this.dict["method-name"],this.dict["array"],this.dict["index"],this.dict["reason"]);
};
var $invalid$array$index_getfieldsref1242 = function(f,refmask) {
return f(R.derefField(this.dict["method-name"],false,refmask[0]),R.derefField(this.dict["array"],false,refmask[1]),R.derefField(this.dict["index"],false,refmask[2]),R.derefField(this.dict["reason"],false,refmask[3]));
};
var $invalid$array$index_mutablemask1243 = [false,false,false,false];
var $invalid$array$index$base1238 = {"$fieldNames":["method-name","array","index","reason"],
"render-reason":anf_variant_member1241,
"_match":R.makeMatch("invalid-array-index",4)};
var $invalid$array$index$brands1240 = {"$brand$invalid$array$index":true};
$invalid$array$index$brands1240[RuntimeError21._brand] = true;
var invalid$array$index1246 = R.makeVariantConstructor(L[478],function() {
return [$type$String7,$type$Number8,$type$String7];
},["method$name1247","index1248","reason1249"],[L[475],L[476],L[477]],[false,false,false,false],["method$name1247","array1250","index1248","reason1249"],$invalid$array$index_mutablemask1243,$invalid$array$index$base1238,$invalid$array$index$brands1240,"invalid-array-index",$invalid$array$index_getfieldsref1242,$invalid$array$index_getfields1244,$invalid$array$index$base1238);
var $equality$failure_getfields1257 = function(f) {
return f(this.dict["reason"],this.dict["value1"],this.dict["value2"]);
};
var $equality$failure_getfieldsref1255 = function(f,refmask) {
return f(R.derefField(this.dict["reason"],false,refmask[0]),R.derefField(this.dict["value1"],false,refmask[1]),R.derefField(this.dict["value2"],false,refmask[2]));
};
var $equality$failure_mutablemask1256 = [false,false,false];
var $equality$failure$base1251 = {"$fieldNames":["reason","value1","value2"],
"render-reason":anf_variant_member1254,
"_match":R.makeMatch("equality-failure",3)};
var $equality$failure$brands1253 = {"$brand$equality$failure":true};
$equality$failure$brands1253[RuntimeError21._brand] = true;
var equality$failure1259 = R.makeVariantConstructor(L[481],function() {
return [$type$String7];
},["reason1260"],[L[480]],[false,false,false],["reason1260","value11261","value21262"],$equality$failure_mutablemask1256,$equality$failure$base1251,$equality$failure$brands1253,"equality-failure",$equality$failure_getfieldsref1255,$equality$failure_getfields1257,$equality$failure$base1251);
var $user$break_getfields1269 = function(f) {
return f();
};
var $user$break_getfieldsref1267 = function(f) {
return f();
};
var $user$break_mutablemask1268 = [];
var $user$break$base1263 = {"render-reason":anf_singleton_variant_member1266,
"_match":R.makeMatch("user-break",0)};
var $user$break$brands1265 = {"$brand$user$break":true};
$user$break$brands1265[RuntimeError21._brand] = true;
var $user$exception_getfields1277 = function(f) {
return f(this.dict["value"]);
};
var $user$exception_getfieldsref1275 = function(f,refmask) {
return f(R.derefField(this.dict["value"],false,refmask[0]));
};
var $user$exception_mutablemask1276 = [false];
var $user$exception$base1271 = {"$fieldNames":["value"],
"render-reason":anf_variant_member1274,
"_match":R.makeMatch("user-exception",1)};
var $user$exception$brands1273 = {"$brand$user$exception":true};
$user$exception$brands1273[RuntimeError21._brand] = true;
var user$exception1279 = R.makeVariantConstructor(L[484],function() {
return [];
},[],[],[false],["value1280"],$user$exception_mutablemask1276,$user$exception$base1271,$user$exception$brands1273,"user-exception",$user$exception_getfieldsref1275,$user$exception_getfields1277,$user$exception$base1271);
var anf_assign1282 = R.makeObject({"RuntimeError":R.makeFunction(function($val1281) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[485],1,$t);
}
return R.makeBoolean(R.hasBrand($val1281,RuntimeError21._brand));
}),
"is-message-exception":R.makeFunction(function($val1011) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[424],1,$t);
}
return R.makeBoolean(R.hasBrand($val1011,"$brand$message$exception"));
}),
"message-exception":message$exception1012,
"is-no-cases-matched":R.makeFunction(function($val1021) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[427],1,$t);
}
return R.makeBoolean(R.hasBrand($val1021,"$brand$no$cases$matched"));
}),
"no-cases-matched":no$cases$matched1022,
"is-no-branches-matched":R.makeFunction(function($val1032) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[429],1,$t);
}
return R.makeBoolean(R.hasBrand($val1032,"$brand$no$branches$matched"));
}),
"no-branches-matched":no$branches$matched1033,
"is-internal-error":R.makeFunction(function($val1043) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[432],1,$t);
}
return R.makeBoolean(R.hasBrand($val1043,"$brand$internal$error"));
}),
"internal-error":internal$error1044,
"is-field-not-found":R.makeFunction(function($val1054) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[434],1,$t);
}
return R.makeBoolean(R.hasBrand($val1054,"$brand$field$not$found"));
}),
"field-not-found":field$not$found1055,
"is-lookup-non-object":R.makeFunction(function($val1066) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[437],1,$t);
}
return R.makeBoolean(R.hasBrand($val1066,"$brand$lookup$non$object"));
}),
"lookup-non-object":lookup$non$object1067,
"is-extend-non-object":R.makeFunction(function($val1078) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[440],1,$t);
}
return R.makeBoolean(R.hasBrand($val1078,"$brand$extend$non$object"));
}),
"extend-non-object":extend$non$object1079,
"is-non-boolean-condition":R.makeFunction(function($val1089) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[442],1,$t);
}
return R.makeBoolean(R.hasBrand($val1089,"$brand$non$boolean$condition"));
}),
"non-boolean-condition":non$boolean$condition1090,
"is-non-boolean-op":R.makeFunction(function($val1101) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[444],1,$t);
}
return R.makeBoolean(R.hasBrand($val1101,"$brand$non$boolean$op"));
}),
"non-boolean-op":non$boolean$op1102,
"is-generic-type-mismatch":R.makeFunction(function($val1114) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[446],1,$t);
}
return R.makeBoolean(R.hasBrand($val1114,"$brand$generic$type$mismatch"));
}),
"generic-type-mismatch":generic$type$mismatch1115,
"is-outside-numeric-range":R.makeFunction(function($val1125) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[449],1,$t);
}
return R.makeBoolean(R.hasBrand($val1125,"$brand$outside$numeric$range"));
}),
"outside-numeric-range":outside$numeric$range1126,
"is-num-string-binop-error":R.makeFunction(function($val1137) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[451],1,$t);
}
return R.makeBoolean(R.hasBrand($val1137,"$brand$num$string$binop$error"));
}),
"num-string-binop-error":num$string$binop$error1138,
"is-numeric-binop-error":R.makeFunction(function($val1151) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[453],1,$t);
}
return R.makeBoolean(R.hasBrand($val1151,"$brand$numeric$binop$error"));
}),
"numeric-binop-error":numeric$binop$error1152,
"is-cases-arity-mismatch":R.makeFunction(function($val1164) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[455],1,$t);
}
return R.makeBoolean(R.hasBrand($val1164,"$brand$cases$arity$mismatch"));
}),
"cases-arity-mismatch":cases$arity$mismatch1165,
"is-cases-singleton-mismatch":R.makeFunction(function($val1176) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[457],1,$t);
}
return R.makeBoolean(R.hasBrand($val1176,"$brand$cases$singleton$mismatch"));
}),
"cases-singleton-mismatch":cases$singleton$mismatch1177,
"is-arity-mismatch":R.makeFunction(function($val1187) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[460],1,$t);
}
return R.makeBoolean(R.hasBrand($val1187,"$brand$arity$mismatch"));
}),
"arity-mismatch":arity$mismatch1188,
"is-non-function-app":R.makeFunction(function($val1199) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[462],1,$t);
}
return R.makeBoolean(R.hasBrand($val1199,"$brand$non$function$app"));
}),
"non-function-app":non$function$app1200,
"is-bad-app":R.makeFunction(function($val1210) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[464],1,$t);
}
return R.makeBoolean(R.hasBrand($val1210,"$brand$bad$app"));
}),
"bad-app":bad$app1211,
"is-uninitialized-id":R.makeFunction(function($val1224) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[469],1,$t);
}
return R.makeBoolean(R.hasBrand($val1224,"$brand$uninitialized$id"));
}),
"uninitialized-id":uninitialized$id1225,
"is-module-load-failure":R.makeFunction(function($val1235) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[472],1,$t);
}
return R.makeBoolean(R.hasBrand($val1235,"$brand$module$load$failure"));
}),
"module-load-failure":module$load$failure1236,
"is-invalid-array-index":R.makeFunction(function($val1245) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[474],1,$t);
}
return R.makeBoolean(R.hasBrand($val1245,"$brand$invalid$array$index"));
}),
"invalid-array-index":invalid$array$index1246,
"is-equality-failure":R.makeFunction(function($val1258) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[479],1,$t);
}
return R.makeBoolean(R.hasBrand($val1258,"$brand$equality$failure"));
}),
"equality-failure":equality$failure1259,
"is-user-break":R.makeFunction(function($val1270) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[482],1,$t);
}
return R.makeBoolean(R.hasBrand($val1270,"$brand$user$break"));
}),
"user-break":R.makeDataValue($user$break$base1263,$user$break$brands1265,"user-break",$user$break_getfieldsref1267,$user$break_getfields1269,-1,$user$break_mutablemask1268,$user$break$base1263),
"is-user-exception":R.makeFunction(function($val1278) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[483],1,$t);
}
return R.makeBoolean(R.hasBrand($val1278,"$brand$user$exception"));
}),
"user-exception":user$exception1279});
RuntimeError1283.$var = anf_assign1282;
var anf_assign1284 = G(RuntimeError1283.$var,"RuntimeError",L[485]);
RuntimeError1285.$var = anf_assign1284;
var anf_assign1286 = G(RuntimeError1283.$var,"RuntimeError",L[485]);
is$RuntimeError1287.$var = anf_assign1286;
var anf_assign1288 = G(RuntimeError1283.$var,"is-message-exception",L[424]);
is$message$exception1289.$var = anf_assign1288;
var anf_assign1290 = G(RuntimeError1283.$var,"message-exception",L[424]);
message$exception1291.$var = anf_assign1290;
var anf_assign1292 = G(RuntimeError1283.$var,"is-no-cases-matched",L[427]);
is$no$cases$matched1293.$var = anf_assign1292;
var anf_assign1294 = G(RuntimeError1283.$var,"no-cases-matched",L[427]);
no$cases$matched1295.$var = anf_assign1294;
var anf_assign1296 = G(RuntimeError1283.$var,"is-no-branches-matched",L[429]);
is$no$branches$matched1297.$var = anf_assign1296;
var anf_assign1298 = G(RuntimeError1283.$var,"no-branches-matched",L[429]);
no$branches$matched1299.$var = anf_assign1298;
var anf_assign1300 = G(RuntimeError1283.$var,"is-internal-error",L[432]);
is$internal$error1301.$var = anf_assign1300;
var anf_assign1302 = G(RuntimeError1283.$var,"internal-error",L[432]);
internal$error1303.$var = anf_assign1302;
var anf_assign1304 = G(RuntimeError1283.$var,"is-field-not-found",L[434]);
is$field$not$found1305.$var = anf_assign1304;
var anf_assign1306 = G(RuntimeError1283.$var,"field-not-found",L[434]);
field$not$found1307.$var = anf_assign1306;
var anf_assign1308 = G(RuntimeError1283.$var,"is-lookup-non-object",L[437]);
is$lookup$non$object1309.$var = anf_assign1308;
var anf_assign1310 = G(RuntimeError1283.$var,"lookup-non-object",L[437]);
lookup$non$object1311.$var = anf_assign1310;
var anf_assign1312 = G(RuntimeError1283.$var,"is-extend-non-object",L[440]);
is$extend$non$object1313.$var = anf_assign1312;
var anf_assign1314 = G(RuntimeError1283.$var,"extend-non-object",L[440]);
extend$non$object1315.$var = anf_assign1314;
var anf_assign1316 = G(RuntimeError1283.$var,"is-non-boolean-condition",L[442]);
is$non$boolean$condition1317.$var = anf_assign1316;
var anf_assign1318 = G(RuntimeError1283.$var,"non-boolean-condition",L[442]);
non$boolean$condition1319.$var = anf_assign1318;
var anf_assign1320 = G(RuntimeError1283.$var,"is-non-boolean-op",L[444]);
is$non$boolean$op1321.$var = anf_assign1320;
var anf_assign1322 = G(RuntimeError1283.$var,"non-boolean-op",L[444]);
non$boolean$op1323.$var = anf_assign1322;
var anf_assign1324 = G(RuntimeError1283.$var,"is-generic-type-mismatch",L[446]);
is$generic$type$mismatch1325.$var = anf_assign1324;
var anf_assign1326 = G(RuntimeError1283.$var,"generic-type-mismatch",L[446]);
generic$type$mismatch1327.$var = anf_assign1326;
var anf_assign1328 = G(RuntimeError1283.$var,"is-outside-numeric-range",L[449]);
is$outside$numeric$range1329.$var = anf_assign1328;
var anf_assign1330 = G(RuntimeError1283.$var,"outside-numeric-range",L[449]);
outside$numeric$range1331.$var = anf_assign1330;
var anf_assign1332 = G(RuntimeError1283.$var,"is-num-string-binop-error",L[451]);
is$num$string$binop$error1333.$var = anf_assign1332;
var anf_assign1334 = G(RuntimeError1283.$var,"num-string-binop-error",L[451]);
num$string$binop$error1335.$var = anf_assign1334;
var anf_assign1336 = G(RuntimeError1283.$var,"is-numeric-binop-error",L[453]);
is$numeric$binop$error1337.$var = anf_assign1336;
var anf_assign1338 = G(RuntimeError1283.$var,"numeric-binop-error",L[453]);
numeric$binop$error1339.$var = anf_assign1338;
var anf_assign1340 = G(RuntimeError1283.$var,"is-cases-arity-mismatch",L[455]);
is$cases$arity$mismatch1341.$var = anf_assign1340;
var anf_assign1342 = G(RuntimeError1283.$var,"cases-arity-mismatch",L[455]);
cases$arity$mismatch1343.$var = anf_assign1342;
var anf_assign1344 = G(RuntimeError1283.$var,"is-cases-singleton-mismatch",L[457]);
is$cases$singleton$mismatch1345.$var = anf_assign1344;
var anf_assign1346 = G(RuntimeError1283.$var,"cases-singleton-mismatch",L[457]);
cases$singleton$mismatch1347.$var = anf_assign1346;
var anf_assign1348 = G(RuntimeError1283.$var,"is-arity-mismatch",L[460]);
is$arity$mismatch1349.$var = anf_assign1348;
var anf_assign1350 = G(RuntimeError1283.$var,"arity-mismatch",L[460]);
arity$mismatch1351.$var = anf_assign1350;
var anf_assign1352 = G(RuntimeError1283.$var,"is-non-function-app",L[462]);
is$non$function$app1353.$var = anf_assign1352;
var anf_assign1354 = G(RuntimeError1283.$var,"non-function-app",L[462]);
non$function$app1355.$var = anf_assign1354;
var anf_assign1356 = G(RuntimeError1283.$var,"is-bad-app",L[464]);
is$bad$app1357.$var = anf_assign1356;
var anf_assign1358 = G(RuntimeError1283.$var,"bad-app",L[464]);
bad$app1359.$var = anf_assign1358;
var anf_assign1360 = G(RuntimeError1283.$var,"is-uninitialized-id",L[469]);
is$uninitialized$id1361.$var = anf_assign1360;
var anf_assign1362 = G(RuntimeError1283.$var,"uninitialized-id",L[469]);
uninitialized$id1363.$var = anf_assign1362;
var anf_assign1364 = G(RuntimeError1283.$var,"is-module-load-failure",L[472]);
is$module$load$failure1365.$var = anf_assign1364;
var anf_assign1366 = G(RuntimeError1283.$var,"module-load-failure",L[472]);
module$load$failure1367.$var = anf_assign1366;
var anf_assign1368 = G(RuntimeError1283.$var,"is-invalid-array-index",L[474]);
is$invalid$array$index1369.$var = anf_assign1368;
var anf_assign1370 = G(RuntimeError1283.$var,"invalid-array-index",L[474]);
invalid$array$index1371.$var = anf_assign1370;
var anf_assign1372 = G(RuntimeError1283.$var,"is-equality-failure",L[479]);
is$equality$failure1373.$var = anf_assign1372;
var anf_assign1374 = G(RuntimeError1283.$var,"equality-failure",L[479]);
equality$failure1375.$var = anf_assign1374;
var anf_assign1376 = G(RuntimeError1283.$var,"is-user-break",L[482]);
is$user$break1377.$var = anf_assign1376;
var anf_assign1378 = G(RuntimeError1283.$var,"user-break",L[482]);
user$break1379.$var = anf_assign1378;
var anf_assign1380 = G(RuntimeError1283.$var,"is-user-exception",L[483]);
is$user$exception1381.$var = anf_assign1380;
var anf_assign1382 = G(RuntimeError1283.$var,"user-exception",L[483]);
user$exception1383.$var = anf_assign1382;
var $temp_full1385 = function($self1386) {
var $step1384 = 0;
var $ans1387 = D;
var $al1388 = L[488];
try {
if(R.isActivationRecord($self1386)) {
$step1384 = $self1386.step;
$al1388 = $self1386.from;
$ans1387 = $self1386.ans;
self1386 = $self1386.args[0];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[488],1,$t);
}
var self1386 = $self1386;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1384) {
case 0: $step1384 = 1;
$al1388 = L[486];
$field1389 = R.getColonFieldLoc(self1386,"_tostring",L[486]);
if(R.isMethod($field1389)) {
$ans1387 = $field1389.full_meth(self1386,tostring4);
} else {
if(!(R.isFunction($field1389))) {
R.ffi.throwNonFunApp(L[486],$field1389);
}
$ans1387 = $field1389.app(tostring4);
}
break;
case 1: var anf_arg1390 = $ans1387;
$step1384 = 2;
$al1388 = L[487];
$field1391 = R.getColonFieldLoc(ED12,"text",L[487]);
if(R.isMethod($field1391)) {
$ans1387 = $field1391.full_meth(ED12,anf_arg1390);
} else {
if(!(R.isFunction($field1391))) {
R.ffi.throwNonFunApp(L[487],$field1391);
}
$ans1387 = $field1391.app(anf_arg1390);
}
break;
case 2: ++R.GAS;
return $ans1387;
default: throw "No case numbered " + $step1384 + " in $temp_full1385";
}
}
} catch($e1392) {
if(R.isCont($e1392) && ($step1384 !== 2)) {
$e1392.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1388,$temp_full1385,$step1384,[self1386],[]);
}
if(R.isPyretException($e1392)) {
$e1392.pyretStack.push($al1388);
}
throw $e1392;
}
};
var anf_shared1652 = R.makeMethod0($temp_full1385);
var $temp_full1394 = function($self1395) {
var $step1393 = 0;
var $ans1396 = D;
var $al1397 = L[532];
try {
if(R.isActivationRecord($self1395)) {
$step1393 = $self1395.step;
$al1397 = $self1395.from;
$ans1396 = $self1395.ans;
self1395 = $self1395.args[0];
anf_array_val1497 = $self1395.vars[0];
anf_method_obj1489 = $self1395.vars[1];
anf_method_obj1499 = $self1395.vars[2];
anf_array_val1502 = $self1395.vars[3];
anf_array_val1482 = $self1395.vars[4];
anf_method_obj1484 = $self1395.vars[5];
anf_method_obj1504 = $self1395.vars[6];
extra1493 = $self1395.vars[7];
anf_array_val1469 = $self1395.vars[8];
anf_method_obj1471 = $self1395.vars[9];
anf_array_val1475 = $self1395.vars[10];
anf_array_val1460 = $self1395.vars[11];
anf_array_val1459 = $self1395.vars[12];
anf_array_val1458 = $self1395.vars[13];
anf_method_obj1462 = $self1395.vars[14];
anf_array_val1474 = $self1395.vars[15];
anf_method_obj1449 = $self1395.vars[16];
anf_method_obj1477 = $self1395.vars[17];
missing1492 = $self1395.vars[18];
anf_method_obj1438 = $self1395.vars[19];
anf_array_val1442 = $self1395.vars[20];
anf_array_val1429 = $self1395.vars[21];
anf_array_val1428 = $self1395.vars[22];
anf_array_val1427 = $self1395.vars[23];
anf_array_val1426 = $self1395.vars[24];
anf_array_val1425 = $self1395.vars[25];
anf_array_val1424 = $self1395.vars[26];
anf_array_val1423 = $self1395.vars[27];
anf_array_val1422 = $self1395.vars[28];
anf_array_val1421 = $self1395.vars[29];
anf_method_obj1431 = $self1395.vars[30];
anf_array_val1441 = $self1395.vars[31];
anf_method_obj1400 = $self1395.vars[32];
anf_method_obj1444 = $self1395.vars[33];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[532],1,$t);
}
var self1395 = $self1395;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1393) {
case 0: var anf_method_obj1444 = G(ED12,"error",L[489]);
var anf_method_obj1400 = G(ED12,"para",L[490]);
$step1393 = 1;
$al1397 = L[491];
$field1398 = R.getColonFieldLoc(ED12,"text",L[491]);
if(R.isMethod($field1398)) {
$ans1396 = $field1398.full_meth(ED12,("The program is missing something"));
} else {
if(!(R.isFunction($field1398))) {
R.ffi.throwNonFunApp(L[491],$field1398);
}
$ans1396 = $field1398.app(("The program is missing something"));
}
break;
case 1: var anf_array_val1399 = $ans1396;
var anf_arg1401 = [anf_array_val1399];
$step1393 = 2;
$al1397 = L[490];
$field1402 = R.getColonFieldLoc(anf_method_obj1400,"make",L[490]);
if(R.isMethod($field1402)) {
$ans1396 = $field1402.full_meth(anf_method_obj1400,anf_arg1401);
} else {
if(!(R.isFunction($field1402))) {
R.ffi.throwNonFunApp(L[490],$field1402);
}
$ans1396 = $field1402.app(anf_arg1401);
}
break;
case 2: var anf_array_val1441 = $ans1396;
var anf_method_obj1431 = G(ED12,"para-nospace",L[492]);
$step1393 = 3;
$al1397 = L[493];
$field1403 = R.getColonFieldLoc(ED12,"text",L[493]);
if(R.isMethod($field1403)) {
$ans1396 = $field1403.full_meth(ED12,("Look carefully before the "));
} else {
if(!(R.isFunction($field1403))) {
R.ffi.throwNonFunApp(L[493],$field1403);
}
$ans1396 = $field1403.app(("Look carefully before the "));
}
break;
case 3: var anf_array_val1421 = $ans1396;
$step1393 = 4;
$al1397 = L[494];
$field1404 = R.getColonFieldLoc(ED12,"text",L[494]);
if(R.isMethod($field1404)) {
$ans1396 = $field1404.full_meth(ED12,("highlighted text"));
} else {
if(!(R.isFunction($field1404))) {
R.ffi.throwNonFunApp(L[494],$field1404);
}
$ans1396 = $field1404.app(("highlighted text"));
}
break;
case 4: var anf_arg1405 = $ans1396;
$step1393 = 5;
$al1397 = L[495];
$field1406 = R.getColonFieldLoc(ED12,"styled",L[495]);
if(R.isMethod($field1406)) {
$ans1396 = $field1406.full_meth(ED12,anf_arg1405,("error-highlight"));
} else {
if(!(R.isFunction($field1406))) {
R.ffi.throwNonFunApp(L[495],$field1406);
}
$ans1396 = $field1406.app(anf_arg1405,("error-highlight"));
}
break;
case 5: var anf_array_val1422 = $ans1396;
$step1393 = 6;
$al1397 = L[496];
$field1407 = R.getColonFieldLoc(ED12,"text",L[496]);
if(R.isMethod($field1407)) {
$ans1396 = $field1407.full_meth(ED12,(".  Is something missing just before it?"));
} else {
if(!(R.isFunction($field1407))) {
R.ffi.throwNonFunApp(L[496],$field1407);
}
$ans1396 = $field1407.app((".  Is something missing just before it?"));
}
break;
case 6: var anf_array_val1423 = $ans1396;
$step1393 = 7;
$al1397 = L[497];
$field1408 = R.getColonFieldLoc(ED12,"text",L[497]);
if(R.isMethod($field1408)) {
$ans1396 = $field1408.full_meth(ED12,("  Common missing items are colons ("));
} else {
if(!(R.isFunction($field1408))) {
R.ffi.throwNonFunApp(L[497],$field1408);
}
$ans1396 = $field1408.app(("  Common missing items are colons ("));
}
break;
case 7: var anf_array_val1424 = $ans1396;
$step1393 = 8;
$al1397 = L[498];
$field1409 = R.getColonFieldLoc(ED12,"text",L[498]);
if(R.isMethod($field1409)) {
$ans1396 = $field1409.full_meth(ED12,(":"));
} else {
if(!(R.isFunction($field1409))) {
R.ffi.throwNonFunApp(L[498],$field1409);
}
$ans1396 = $field1409.app((":"));
}
break;
case 8: var anf_arg1410 = $ans1396;
$step1393 = 9;
$al1397 = L[499];
$field1411 = R.getColonFieldLoc(ED12,"code",L[499]);
if(R.isMethod($field1411)) {
$ans1396 = $field1411.full_meth(ED12,anf_arg1410);
} else {
if(!(R.isFunction($field1411))) {
R.ffi.throwNonFunApp(L[499],$field1411);
}
$ans1396 = $field1411.app(anf_arg1410);
}
break;
case 9: var anf_array_val1425 = $ans1396;
$step1393 = 10;
$al1397 = L[500];
$field1412 = R.getColonFieldLoc(ED12,"text",L[500]);
if(R.isMethod($field1412)) {
$ans1396 = $field1412.full_meth(ED12,("), commas ("));
} else {
if(!(R.isFunction($field1412))) {
R.ffi.throwNonFunApp(L[500],$field1412);
}
$ans1396 = $field1412.app(("), commas ("));
}
break;
case 10: var anf_array_val1426 = $ans1396;
$step1393 = 11;
$al1397 = L[501];
$field1413 = R.getColonFieldLoc(ED12,"text",L[501]);
if(R.isMethod($field1413)) {
$ans1396 = $field1413.full_meth(ED12,(","));
} else {
if(!(R.isFunction($field1413))) {
R.ffi.throwNonFunApp(L[501],$field1413);
}
$ans1396 = $field1413.app((","));
}
break;
case 11: var anf_arg1414 = $ans1396;
$step1393 = 12;
$al1397 = L[502];
$field1415 = R.getColonFieldLoc(ED12,"code",L[502]);
if(R.isMethod($field1415)) {
$ans1396 = $field1415.full_meth(ED12,anf_arg1414);
} else {
if(!(R.isFunction($field1415))) {
R.ffi.throwNonFunApp(L[502],$field1415);
}
$ans1396 = $field1415.app(anf_arg1414);
}
break;
case 12: var anf_array_val1427 = $ans1396;
$step1393 = 13;
$al1397 = L[503];
$field1416 = R.getColonFieldLoc(ED12,"text",L[503]);
if(R.isMethod($field1416)) {
$ans1396 = $field1416.full_meth(ED12,("), string markers ("));
} else {
if(!(R.isFunction($field1416))) {
R.ffi.throwNonFunApp(L[503],$field1416);
}
$ans1396 = $field1416.app(("), string markers ("));
}
break;
case 13: var anf_array_val1428 = $ans1396;
$step1393 = 14;
$al1397 = L[504];
$field1417 = R.getColonFieldLoc(ED12,"text",L[504]);
if(R.isMethod($field1417)) {
$ans1396 = $field1417.full_meth(ED12,("\""));
} else {
if(!(R.isFunction($field1417))) {
R.ffi.throwNonFunApp(L[504],$field1417);
}
$ans1396 = $field1417.app(("\""));
}
break;
case 14: var anf_arg1418 = $ans1396;
$step1393 = 15;
$al1397 = L[505];
$field1419 = R.getColonFieldLoc(ED12,"code",L[505]);
if(R.isMethod($field1419)) {
$ans1396 = $field1419.full_meth(ED12,anf_arg1418);
} else {
if(!(R.isFunction($field1419))) {
R.ffi.throwNonFunApp(L[505],$field1419);
}
$ans1396 = $field1419.app(anf_arg1418);
}
break;
case 15: var anf_array_val1429 = $ans1396;
$step1393 = 16;
$al1397 = L[506];
$field1420 = R.getColonFieldLoc(ED12,"text",L[506]);
if(R.isMethod($field1420)) {
$ans1396 = $field1420.full_meth(ED12,("), and keywords."));
} else {
if(!(R.isFunction($field1420))) {
R.ffi.throwNonFunApp(L[506],$field1420);
}
$ans1396 = $field1420.app(("), and keywords."));
}
break;
case 16: var anf_array_val1430 = $ans1396;
var anf_arg1432 = [anf_array_val1421,anf_array_val1422,anf_array_val1423,anf_array_val1424,anf_array_val1425,anf_array_val1426,anf_array_val1427,anf_array_val1428,anf_array_val1429,anf_array_val1430];
$step1393 = 17;
$al1397 = L[492];
$field1433 = R.getColonFieldLoc(anf_method_obj1431,"make",L[492]);
if(R.isMethod($field1433)) {
$ans1396 = $field1433.full_meth(anf_method_obj1431,anf_arg1432);
} else {
if(!(R.isFunction($field1433))) {
R.ffi.throwNonFunApp(L[492],$field1433);
}
$ans1396 = $field1433.app(anf_arg1432);
}
break;
case 17: var anf_array_val1442 = $ans1396;
var anf_method_obj1438 = G(ED12,"para",L[507]);
$step1393 = 18;
$al1397 = L[508];
$field1434 = R.getColonFieldLoc(ED12,"text",L[508]);
if(R.isMethod($field1434)) {
$ans1396 = $field1434.full_meth(ED12,("Usually, inserting the missing item will fix this error."));
} else {
if(!(R.isFunction($field1434))) {
R.ffi.throwNonFunApp(L[508],$field1434);
}
$ans1396 = $field1434.app(("Usually, inserting the missing item will fix this error."));
}
break;
case 18: var anf_arg1435 = $ans1396;
$step1393 = 19;
$al1397 = L[509];
$field1436 = R.getColonFieldLoc(ED12,"styled",L[509]);
if(R.isMethod($field1436)) {
$ans1396 = $field1436.full_meth(ED12,anf_arg1435,("hint"));
} else {
if(!(R.isFunction($field1436))) {
R.ffi.throwNonFunApp(L[509],$field1436);
}
$ans1396 = $field1436.app(anf_arg1435,("hint"));
}
break;
case 19: var anf_array_val1437 = $ans1396;
var anf_arg1439 = [anf_array_val1437];
$step1393 = 20;
$al1397 = L[507];
$field1440 = R.getColonFieldLoc(anf_method_obj1438,"make",L[507]);
if(R.isMethod($field1440)) {
$ans1396 = $field1440.full_meth(anf_method_obj1438,anf_arg1439);
} else {
if(!(R.isFunction($field1440))) {
R.ffi.throwNonFunApp(L[507],$field1440);
}
$ans1396 = $field1440.app(anf_arg1439);
}
break;
case 20: var anf_array_val1443 = $ans1396;
var anf_arg1445 = [anf_array_val1441,anf_array_val1442,anf_array_val1443];
$step1393 = 21;
$al1397 = L[489];
$field1446 = R.getColonFieldLoc(anf_method_obj1444,"make",L[489]);
if(R.isMethod($field1446)) {
$ans1396 = $field1446.full_meth(anf_method_obj1444,anf_arg1445);
} else {
if(!(R.isFunction($field1446))) {
R.ffi.throwNonFunApp(L[489],$field1446);
}
$ans1396 = $field1446.app(anf_arg1445);
}
break;
case 21: var missing1492 = $ans1396;
var anf_method_obj1477 = G(ED12,"error",L[510]);
var anf_method_obj1449 = G(ED12,"para",L[511]);
$step1393 = 22;
$al1397 = L[512];
$field1447 = R.getColonFieldLoc(ED12,"text",L[512]);
if(R.isMethod($field1447)) {
$ans1396 = $field1447.full_meth(ED12,("The program contains something extra"));
} else {
if(!(R.isFunction($field1447))) {
R.ffi.throwNonFunApp(L[512],$field1447);
}
$ans1396 = $field1447.app(("The program contains something extra"));
}
break;
case 22: var anf_array_val1448 = $ans1396;
var anf_arg1450 = [anf_array_val1448];
$step1393 = 23;
$al1397 = L[511];
$field1451 = R.getColonFieldLoc(anf_method_obj1449,"make",L[511]);
if(R.isMethod($field1451)) {
$ans1396 = $field1451.full_meth(anf_method_obj1449,anf_arg1450);
} else {
if(!(R.isFunction($field1451))) {
R.ffi.throwNonFunApp(L[511],$field1451);
}
$ans1396 = $field1451.app(anf_arg1450);
}
break;
case 23: var anf_array_val1474 = $ans1396;
var anf_method_obj1462 = G(ED12,"para-nospace",L[513]);
$step1393 = 24;
$al1397 = L[514];
$field1452 = R.getColonFieldLoc(ED12,"text",L[514]);
if(R.isMethod($field1452)) {
$ans1396 = $field1452.full_meth(ED12,("Look carefully before the "));
} else {
if(!(R.isFunction($field1452))) {
R.ffi.throwNonFunApp(L[514],$field1452);
}
$ans1396 = $field1452.app(("Look carefully before the "));
}
break;
case 24: var anf_array_val1458 = $ans1396;
$step1393 = 25;
$al1397 = L[515];
$field1453 = R.getColonFieldLoc(ED12,"text",L[515]);
if(R.isMethod($field1453)) {
$ans1396 = $field1453.full_meth(ED12,("highlighted text"));
} else {
if(!(R.isFunction($field1453))) {
R.ffi.throwNonFunApp(L[515],$field1453);
}
$ans1396 = $field1453.app(("highlighted text"));
}
break;
case 25: var anf_arg1454 = $ans1396;
$step1393 = 26;
$al1397 = L[516];
$field1455 = R.getColonFieldLoc(ED12,"styled",L[516]);
if(R.isMethod($field1455)) {
$ans1396 = $field1455.full_meth(ED12,anf_arg1454,("error-highlight"));
} else {
if(!(R.isFunction($field1455))) {
R.ffi.throwNonFunApp(L[516],$field1455);
}
$ans1396 = $field1455.app(anf_arg1454,("error-highlight"));
}
break;
case 26: var anf_array_val1459 = $ans1396;
$step1393 = 27;
$al1397 = L[517];
$field1456 = R.getColonFieldLoc(ED12,"text",L[517]);
if(R.isMethod($field1456)) {
$ans1396 = $field1456.full_meth(ED12,(".  Does it contains something extra?"));
} else {
if(!(R.isFunction($field1456))) {
R.ffi.throwNonFunApp(L[517],$field1456);
}
$ans1396 = $field1456.app((".  Does it contains something extra?"));
}
break;
case 27: var anf_array_val1460 = $ans1396;
$step1393 = 28;
$al1397 = L[518];
$field1457 = R.getColonFieldLoc(ED12,"text",L[518]);
if(R.isMethod($field1457)) {
$ans1396 = $field1457.full_meth(ED12,("  A common source of errors is typing too much text or in the wrong order."));
} else {
if(!(R.isFunction($field1457))) {
R.ffi.throwNonFunApp(L[518],$field1457);
}
$ans1396 = $field1457.app(("  A common source of errors is typing too much text or in the wrong order."));
}
break;
case 28: var anf_array_val1461 = $ans1396;
var anf_arg1463 = [anf_array_val1458,anf_array_val1459,anf_array_val1460,anf_array_val1461];
$step1393 = 29;
$al1397 = L[513];
$field1464 = R.getColonFieldLoc(anf_method_obj1462,"make",L[513]);
if(R.isMethod($field1464)) {
$ans1396 = $field1464.full_meth(anf_method_obj1462,anf_arg1463);
} else {
if(!(R.isFunction($field1464))) {
R.ffi.throwNonFunApp(L[513],$field1464);
}
$ans1396 = $field1464.app(anf_arg1463);
}
break;
case 29: var anf_array_val1475 = $ans1396;
var anf_method_obj1471 = G(ED12,"para",L[519]);
$step1393 = 30;
$al1397 = L[520];
$field1465 = R.getColonFieldLoc(ED12,"text",L[520]);
if(R.isMethod($field1465)) {
$ans1396 = $field1465.full_meth(ED12,("Usually, removing the extra item will fix this error."));
} else {
if(!(R.isFunction($field1465))) {
R.ffi.throwNonFunApp(L[520],$field1465);
}
$ans1396 = $field1465.app(("Usually, removing the extra item will fix this error."));
}
break;
case 30: var anf_arg1466 = $ans1396;
$step1393 = 31;
$al1397 = L[521];
$field1467 = R.getColonFieldLoc(ED12,"styled",L[521]);
if(R.isMethod($field1467)) {
$ans1396 = $field1467.full_meth(ED12,anf_arg1466,("hint"));
} else {
if(!(R.isFunction($field1467))) {
R.ffi.throwNonFunApp(L[521],$field1467);
}
$ans1396 = $field1467.app(anf_arg1466,("hint"));
}
break;
case 31: var anf_array_val1469 = $ans1396;
$step1393 = 32;
$al1397 = L[522];
$field1468 = R.getColonFieldLoc(ED12,"text",L[522]);
if(R.isMethod($field1468)) {
$ans1396 = $field1468.full_meth(ED12,("However, you may have meant to keep this text, so think before you delete!"));
} else {
if(!(R.isFunction($field1468))) {
R.ffi.throwNonFunApp(L[522],$field1468);
}
$ans1396 = $field1468.app(("However, you may have meant to keep this text, so think before you delete!"));
}
break;
case 32: var anf_array_val1470 = $ans1396;
var anf_arg1472 = [anf_array_val1469,anf_array_val1470];
$step1393 = 33;
$al1397 = L[519];
$field1473 = R.getColonFieldLoc(anf_method_obj1471,"make",L[519]);
if(R.isMethod($field1473)) {
$ans1396 = $field1473.full_meth(anf_method_obj1471,anf_arg1472);
} else {
if(!(R.isFunction($field1473))) {
R.ffi.throwNonFunApp(L[519],$field1473);
}
$ans1396 = $field1473.app(anf_arg1472);
}
break;
case 33: var anf_array_val1476 = $ans1396;
var anf_arg1478 = [anf_array_val1474,anf_array_val1475,anf_array_val1476];
$step1393 = 34;
$al1397 = L[510];
$field1479 = R.getColonFieldLoc(anf_method_obj1477,"make",L[510]);
if(R.isMethod($field1479)) {
$ans1396 = $field1479.full_meth(anf_method_obj1477,anf_arg1478);
} else {
if(!(R.isFunction($field1479))) {
R.ffi.throwNonFunApp(L[510],$field1479);
}
$ans1396 = $field1479.app(anf_arg1478);
}
break;
case 34: var extra1493 = $ans1396;
var anf_method_obj1504 = G(ED12,"error",L[523]);
var anf_method_obj1484 = G(ED12,"para",L[524]);
$step1393 = 35;
$al1397 = L[525];
$field1480 = R.getColonFieldLoc(ED12,"text",L[525]);
if(R.isMethod($field1480)) {
$ans1396 = $field1480.full_meth(ED12,("Pyret didn't understand your program around"));
} else {
if(!(R.isFunction($field1480))) {
R.ffi.throwNonFunApp(L[525],$field1480);
}
$ans1396 = $field1480.app(("Pyret didn't understand your program around"));
}
break;
case 35: var anf_array_val1482 = $ans1396;
var anf_arg1481 = G(self1395,"loc",L[526]);
$step1393 = 36;
$al1397 = L[531];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al1397,draw$and$highlight33.$var);
}
$ans1396 = draw$and$highlight33.$var.app(anf_arg1481);
break;
case 36: var anf_array_val1483 = $ans1396;
var anf_arg1485 = [anf_array_val1482,anf_array_val1483];
$step1393 = 37;
$al1397 = L[524];
$field1486 = R.getColonFieldLoc(anf_method_obj1484,"make",L[524]);
if(R.isMethod($field1486)) {
$ans1396 = $field1486.full_meth(anf_method_obj1484,anf_arg1485);
} else {
if(!(R.isFunction($field1486))) {
R.ffi.throwNonFunApp(L[524],$field1486);
}
$ans1396 = $field1486.app(anf_arg1485);
}
break;
case 37: var anf_array_val1502 = $ans1396;
var anf_method_obj1499 = G(ED12,"opt",L[527]);
var anf_method_obj1489 = G(ED12,"para",L[528]);
$step1393 = 38;
$al1397 = L[529];
$field1487 = R.getColonFieldLoc(ED12,"text",L[529]);
if(R.isMethod($field1487)) {
$ans1396 = $field1487.full_meth(ED12,("Typical reasons for getting this error are"));
} else {
if(!(R.isFunction($field1487))) {
R.ffi.throwNonFunApp(L[529],$field1487);
}
$ans1396 = $field1487.app(("Typical reasons for getting this error are"));
}
break;
case 38: var anf_array_val1488 = $ans1396;
var anf_arg1490 = [anf_array_val1488];
$step1393 = 39;
$al1397 = L[528];
$field1491 = R.getColonFieldLoc(anf_method_obj1489,"make",L[528]);
if(R.isMethod($field1491)) {
$ans1396 = $field1491.full_meth(anf_method_obj1489,anf_arg1490);
} else {
if(!(R.isFunction($field1491))) {
R.ffi.throwNonFunApp(L[528],$field1491);
}
$ans1396 = $field1491.app(anf_arg1490);
}
break;
case 39: var anf_array_val1497 = $ans1396;
var anf_method_obj1494 = G(ED12,"bulleted",L[530]);
var anf_arg1495 = [missing1492,extra1493];
$step1393 = 40;
$al1397 = L[530];
$field1496 = R.getColonFieldLoc(anf_method_obj1494,"make",L[530]);
if(R.isMethod($field1496)) {
$ans1396 = $field1496.full_meth(anf_method_obj1494,anf_arg1495);
} else {
if(!(R.isFunction($field1496))) {
R.ffi.throwNonFunApp(L[530],$field1496);
}
$ans1396 = $field1496.app(anf_arg1495);
}
break;
case 40: var anf_array_val1498 = $ans1396;
var anf_arg1500 = [anf_array_val1497,anf_array_val1498];
$step1393 = 41;
$al1397 = L[527];
$field1501 = R.getColonFieldLoc(anf_method_obj1499,"make",L[527]);
if(R.isMethod($field1501)) {
$ans1396 = $field1501.full_meth(anf_method_obj1499,anf_arg1500);
} else {
if(!(R.isFunction($field1501))) {
R.ffi.throwNonFunApp(L[527],$field1501);
}
$ans1396 = $field1501.app(anf_arg1500);
}
break;
case 41: var anf_array_val1503 = $ans1396;
var anf_arg1505 = [anf_array_val1502,anf_array_val1503];
$step1393 = 42;
$al1397 = L[523];
$field1506 = R.getColonFieldLoc(anf_method_obj1504,"make",L[523]);
if(R.isMethod($field1506)) {
$ans1396 = $field1506.full_meth(anf_method_obj1504,anf_arg1505);
} else {
if(!(R.isFunction($field1506))) {
R.ffi.throwNonFunApp(L[523],$field1506);
}
$ans1396 = $field1506.app(anf_arg1505);
}
break;
case 42: ++R.GAS;
return $ans1396;
default: throw "No case numbered " + $step1393 + " in $temp_full1394";
}
}
} catch($e1507) {
if(R.isCont($e1507) && ($step1393 !== 42)) {
$e1507.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1397,$temp_full1394,$step1393,[self1395],[anf_array_val1497,anf_method_obj1489,anf_method_obj1499,anf_array_val1502,anf_array_val1482,anf_method_obj1484,anf_method_obj1504,extra1493,anf_array_val1469,anf_method_obj1471,anf_array_val1475,anf_array_val1460,anf_array_val1459,anf_array_val1458,anf_method_obj1462,anf_array_val1474,anf_method_obj1449,anf_method_obj1477,missing1492,anf_method_obj1438,anf_array_val1442,anf_array_val1429,anf_array_val1428,anf_array_val1427,anf_array_val1426,anf_array_val1425,anf_array_val1424,anf_array_val1423,anf_array_val1422,anf_array_val1421,anf_method_obj1431,anf_array_val1441,anf_method_obj1400,anf_method_obj1444]);
}
if(R.isPyretException($e1507)) {
$e1507.pyretStack.push($al1397);
}
throw $e1507;
}
};
var anf_variant_member1656 = R.makeMethod0($temp_full1394);
var $temp_full1509 = function($self1510) {
var $step1508 = 0;
var $ans1511 = D;
var $al1512 = L[537];
try {
if(R.isActivationRecord($self1510)) {
$step1508 = $self1510.step;
$al1512 = $self1510.from;
$ans1511 = $self1510.ans;
self1510 = $self1510.args[0];
anf_array_val1515 = $self1510.vars[0];
anf_method_obj1517 = $self1510.vars[1];
anf_method_obj1521 = $self1510.vars[2];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[537],1,$t);
}
var self1510 = $self1510;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1508) {
case 0: var anf_method_obj1521 = G(ED12,"error",L[533]);
var anf_method_obj1517 = G(ED12,"para",L[534]);
$step1508 = 1;
$al1512 = L[535];
$field1513 = R.getColonFieldLoc(ED12,"text",L[535]);
if(R.isMethod($field1513)) {
$ans1511 = $field1513.full_meth(ED12,("Pyret didn't understand the very end of your program."));
} else {
if(!(R.isFunction($field1513))) {
R.ffi.throwNonFunApp(L[535],$field1513);
}
$ans1511 = $field1513.app(("Pyret didn't understand the very end of your program."));
}
break;
case 1: var anf_array_val1515 = $ans1511;
$step1508 = 2;
$al1512 = L[536];
$field1514 = R.getColonFieldLoc(ED12,"text",L[536]);
if(R.isMethod($field1514)) {
$ans1511 = $field1514.full_meth(ED12,("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end."));
} else {
if(!(R.isFunction($field1514))) {
R.ffi.throwNonFunApp(L[536],$field1514);
}
$ans1511 = $field1514.app(("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end."));
}
break;
case 2: var anf_array_val1516 = $ans1511;
var anf_arg1518 = [anf_array_val1515,anf_array_val1516];
$step1508 = 3;
$al1512 = L[534];
$field1519 = R.getColonFieldLoc(anf_method_obj1517,"make",L[534]);
if(R.isMethod($field1519)) {
$ans1511 = $field1519.full_meth(anf_method_obj1517,anf_arg1518);
} else {
if(!(R.isFunction($field1519))) {
R.ffi.throwNonFunApp(L[534],$field1519);
}
$ans1511 = $field1519.app(anf_arg1518);
}
break;
case 3: var anf_array_val1520 = $ans1511;
var anf_arg1522 = [anf_array_val1520];
$step1508 = 4;
$al1512 = L[533];
$field1523 = R.getColonFieldLoc(anf_method_obj1521,"make",L[533]);
if(R.isMethod($field1523)) {
$ans1511 = $field1523.full_meth(anf_method_obj1521,anf_arg1522);
} else {
if(!(R.isFunction($field1523))) {
R.ffi.throwNonFunApp(L[533],$field1523);
}
$ans1511 = $field1523.app(anf_arg1522);
}
break;
case 4: ++R.GAS;
return $ans1511;
default: throw "No case numbered " + $step1508 + " in $temp_full1509";
}
}
} catch($e1524) {
if(R.isCont($e1524) && ($step1508 !== 4)) {
$e1524.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1512,$temp_full1509,$step1508,[self1510],[anf_array_val1515,anf_method_obj1517,anf_method_obj1521]);
}
if(R.isPyretException($e1524)) {
$e1524.pyretStack.push($al1512);
}
throw $e1524;
}
};
var anf_variant_member1667 = R.makeMethod0($temp_full1509);
var $temp_full1526 = function($self1527) {
var $step1525 = 0;
var $ans1528 = D;
var $al1529 = L[544];
try {
if(R.isActivationRecord($self1527)) {
$step1525 = $self1527.step;
$al1529 = $self1527.from;
$ans1528 = $self1527.ans;
self1527 = $self1527.args[0];
anf_array_val1534 = $self1527.vars[0];
anf_array_val1533 = $self1527.vars[1];
anf_method_obj1536 = $self1527.vars[2];
anf_method_obj1540 = $self1527.vars[3];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[544],1,$t);
}
var self1527 = $self1527;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1525) {
case 0: var anf_method_obj1540 = G(ED12,"error",L[538]);
var anf_method_obj1536 = G(ED12,"para-nospace",L[539]);
$step1525 = 1;
$al1529 = L[540];
$field1530 = R.getColonFieldLoc(ED12,"text",L[540]);
if(R.isMethod($field1530)) {
$ans1528 = $field1530.full_meth(ED12,("Pyret thinks your program has an incomplete string literal around "));
} else {
if(!(R.isFunction($field1530))) {
R.ffi.throwNonFunApp(L[540],$field1530);
}
$ans1528 = $field1530.app(("Pyret thinks your program has an incomplete string literal around "));
}
break;
case 1: var anf_array_val1533 = $ans1528;
var anf_arg1531 = G(self1527,"loc",L[541]);
$step1525 = 2;
$al1529 = L[543];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al1529,draw$and$highlight33.$var);
}
$ans1528 = draw$and$highlight33.$var.app(anf_arg1531);
break;
case 2: var anf_array_val1534 = $ans1528;
$step1525 = 3;
$al1529 = L[542];
$field1532 = R.getColonFieldLoc(ED12,"text",L[542]);
if(R.isMethod($field1532)) {
$ans1528 = $field1532.full_meth(ED12,("; you may be missing closing punctuation."));
} else {
if(!(R.isFunction($field1532))) {
R.ffi.throwNonFunApp(L[542],$field1532);
}
$ans1528 = $field1532.app(("; you may be missing closing punctuation."));
}
break;
case 3: var anf_array_val1535 = $ans1528;
var anf_arg1537 = [anf_array_val1533,anf_array_val1534,anf_array_val1535];
$step1525 = 4;
$al1529 = L[539];
$field1538 = R.getColonFieldLoc(anf_method_obj1536,"make",L[539]);
if(R.isMethod($field1538)) {
$ans1528 = $field1538.full_meth(anf_method_obj1536,anf_arg1537);
} else {
if(!(R.isFunction($field1538))) {
R.ffi.throwNonFunApp(L[539],$field1538);
}
$ans1528 = $field1538.app(anf_arg1537);
}
break;
case 4: var anf_array_val1539 = $ans1528;
var anf_arg1541 = [anf_array_val1539];
$step1525 = 5;
$al1529 = L[538];
$field1542 = R.getColonFieldLoc(anf_method_obj1540,"make",L[538]);
if(R.isMethod($field1542)) {
$ans1528 = $field1542.full_meth(anf_method_obj1540,anf_arg1541);
} else {
if(!(R.isFunction($field1542))) {
R.ffi.throwNonFunApp(L[538],$field1542);
}
$ans1528 = $field1542.app(anf_arg1541);
}
break;
case 5: ++R.GAS;
return $ans1528;
default: throw "No case numbered " + $step1525 + " in $temp_full1526";
}
}
} catch($e1543) {
if(R.isCont($e1543) && ($step1525 !== 5)) {
$e1543.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1529,$temp_full1526,$step1525,[self1527],[anf_array_val1534,anf_array_val1533,anf_method_obj1536,anf_method_obj1540]);
}
if(R.isPyretException($e1543)) {
$e1543.pyretStack.push($al1529);
}
throw $e1543;
}
};
var anf_variant_member1677 = R.makeMethod0($temp_full1526);
var $temp_full1545 = function($self1546) {
var $step1544 = 0;
var $ans1547 = D;
var $al1548 = L[551];
try {
if(R.isActivationRecord($self1546)) {
$step1544 = $self1546.step;
$al1548 = $self1546.from;
$ans1547 = $self1546.ans;
self1546 = $self1546.args[0];
anf_array_val1553 = $self1546.vars[0];
anf_array_val1552 = $self1546.vars[1];
anf_method_obj1555 = $self1546.vars[2];
anf_method_obj1559 = $self1546.vars[3];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[551],1,$t);
}
var self1546 = $self1546;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1544) {
case 0: var anf_method_obj1559 = G(ED12,"error",L[545]);
var anf_method_obj1555 = G(ED12,"para-nospace",L[546]);
$step1544 = 1;
$al1548 = L[547];
$field1549 = R.getColonFieldLoc(ED12,"text",L[547]);
if(R.isMethod($field1549)) {
$ans1547 = $field1549.full_meth(ED12,("The operator at "));
} else {
if(!(R.isFunction($field1549))) {
R.ffi.throwNonFunApp(L[547],$field1549);
}
$ans1547 = $field1549.app(("The operator at "));
}
break;
case 1: var anf_array_val1552 = $ans1547;
var anf_arg1550 = G(self1546,"loc",L[548]);
$step1544 = 2;
$al1548 = L[550];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al1548,draw$and$highlight33.$var);
}
$ans1547 = draw$and$highlight33.$var.app(anf_arg1550);
break;
case 2: var anf_array_val1553 = $ans1547;
$step1544 = 3;
$al1548 = L[549];
$field1551 = R.getColonFieldLoc(ED12,"text",L[549]);
if(R.isMethod($field1551)) {
$ans1547 = $field1551.full_meth(ED12,(" has no surrounding whitespace."));
} else {
if(!(R.isFunction($field1551))) {
R.ffi.throwNonFunApp(L[549],$field1551);
}
$ans1547 = $field1551.app((" has no surrounding whitespace."));
}
break;
case 3: var anf_array_val1554 = $ans1547;
var anf_arg1556 = [anf_array_val1552,anf_array_val1553,anf_array_val1554];
$step1544 = 4;
$al1548 = L[546];
$field1557 = R.getColonFieldLoc(anf_method_obj1555,"make",L[546]);
if(R.isMethod($field1557)) {
$ans1547 = $field1557.full_meth(anf_method_obj1555,anf_arg1556);
} else {
if(!(R.isFunction($field1557))) {
R.ffi.throwNonFunApp(L[546],$field1557);
}
$ans1547 = $field1557.app(anf_arg1556);
}
break;
case 4: var anf_array_val1558 = $ans1547;
var anf_arg1560 = [anf_array_val1558];
$step1544 = 5;
$al1548 = L[545];
$field1561 = R.getColonFieldLoc(anf_method_obj1559,"make",L[545]);
if(R.isMethod($field1561)) {
$ans1547 = $field1561.full_meth(anf_method_obj1559,anf_arg1560);
} else {
if(!(R.isFunction($field1561))) {
R.ffi.throwNonFunApp(L[545],$field1561);
}
$ans1547 = $field1561.app(anf_arg1560);
}
break;
case 5: ++R.GAS;
return $ans1547;
default: throw "No case numbered " + $step1544 + " in $temp_full1545";
}
}
} catch($e1562) {
if(R.isCont($e1562) && ($step1544 !== 5)) {
$e1562.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1548,$temp_full1545,$step1544,[self1546],[anf_array_val1553,anf_array_val1552,anf_method_obj1555,anf_method_obj1559]);
}
if(R.isPyretException($e1562)) {
$e1562.pyretStack.push($al1548);
}
throw $e1562;
}
};
var anf_variant_member1687 = R.makeMethod0($temp_full1545);
var $temp_full1564 = function($self1565) {
var $step1563 = 0;
var $ans1566 = D;
var $al1567 = L[558];
try {
if(R.isActivationRecord($self1565)) {
$step1563 = $self1565.step;
$al1567 = $self1565.from;
$ans1566 = $self1565.ans;
self1565 = $self1565.args[0];
anf_array_val1572 = $self1565.vars[0];
anf_array_val1571 = $self1565.vars[1];
anf_method_obj1574 = $self1565.vars[2];
anf_method_obj1578 = $self1565.vars[3];
} else {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[558],1,$t);
}
var self1565 = $self1565;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1563) {
case 0: var anf_method_obj1578 = G(ED12,"error",L[552]);
var anf_method_obj1574 = G(ED12,"para-nospace",L[553]);
$step1563 = 1;
$al1567 = L[554];
$field1568 = R.getColonFieldLoc(ED12,"text",L[554]);
if(R.isMethod($field1568)) {
$ans1566 = $field1568.full_meth(ED12,("Pyret thinks your program probably has a number at "));
} else {
if(!(R.isFunction($field1568))) {
R.ffi.throwNonFunApp(L[554],$field1568);
}
$ans1566 = $field1568.app(("Pyret thinks your program probably has a number at "));
}
break;
case 1: var anf_array_val1571 = $ans1566;
var anf_arg1569 = G(self1565,"loc",L[555]);
$step1563 = 2;
$al1567 = L[557];
if(!(R.isFunction(draw$and$highlight33.$var))) {
R.ffi.throwNonFunApp($al1567,draw$and$highlight33.$var);
}
$ans1566 = draw$and$highlight33.$var.app(anf_arg1569);
break;
case 2: var anf_array_val1572 = $ans1566;
$step1563 = 3;
$al1567 = L[556];
$field1570 = R.getColonFieldLoc(ED12,"text",L[556]);
if(R.isMethod($field1570)) {
$ans1566 = $field1570.full_meth(ED12,("; number literals in Pyret require at least one digit before the decimal point."));
} else {
if(!(R.isFunction($field1570))) {
R.ffi.throwNonFunApp(L[556],$field1570);
}
$ans1566 = $field1570.app(("; number literals in Pyret require at least one digit before the decimal point."));
}
break;
case 3: var anf_array_val1573 = $ans1566;
var anf_arg1575 = [anf_array_val1571,anf_array_val1572,anf_array_val1573];
$step1563 = 4;
$al1567 = L[553];
$field1576 = R.getColonFieldLoc(anf_method_obj1574,"make",L[553]);
if(R.isMethod($field1576)) {
$ans1566 = $field1576.full_meth(anf_method_obj1574,anf_arg1575);
} else {
if(!(R.isFunction($field1576))) {
R.ffi.throwNonFunApp(L[553],$field1576);
}
$ans1566 = $field1576.app(anf_arg1575);
}
break;
case 4: var anf_array_val1577 = $ans1566;
var anf_arg1579 = [anf_array_val1577];
$step1563 = 5;
$al1567 = L[552];
$field1580 = R.getColonFieldLoc(anf_method_obj1578,"make",L[552]);
if(R.isMethod($field1580)) {
$ans1566 = $field1580.full_meth(anf_method_obj1578,anf_arg1579);
} else {
if(!(R.isFunction($field1580))) {
R.ffi.throwNonFunApp(L[552],$field1580);
}
$ans1566 = $field1580.app(anf_arg1579);
}
break;
case 5: ++R.GAS;
return $ans1566;
default: throw "No case numbered " + $step1563 + " in $temp_full1564";
}
}
} catch($e1581) {
if(R.isCont($e1581) && ($step1563 !== 5)) {
$e1581.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1567,$temp_full1564,$step1563,[self1565],[anf_array_val1572,anf_array_val1571,anf_method_obj1574,anf_method_obj1578]);
}
if(R.isPyretException($e1581)) {
$e1581.pyretStack.push($al1567);
}
throw $e1581;
}
};
var anf_variant_member1697 = R.makeMethod0($temp_full1564);
var $temp_full1583 = function($self1584,$tostring1585) {
var $step1582 = 0;
var $ans1586 = D;
var $al1587 = L[562];
try {
if(R.isActivationRecord($self1584)) {
$step1582 = $self1584.step;
$al1587 = $self1584.from;
$ans1586 = $self1584.ans;
self1584 = $self1584.args[0];
tostring1585 = $self1584.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[562],2,$t);
}
var self1584 = $self1584;
var tostring1585 = $tostring1585;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1582) {
case 0: var anf_method_obj1588 = G(self1584,"loc",L[559]);
$step1582 = 1;
$al1587 = L[560];
$field1589 = R.getColonFieldLoc(anf_method_obj1588,"format",L[560]);
if(R.isMethod($field1589)) {
$ans1586 = $field1589.full_meth(anf_method_obj1588,(true));
} else {
if(!(R.isFunction($field1589))) {
R.ffi.throwNonFunApp(L[560],$field1589);
}
$ans1586 = $field1589.app((true));
}
break;
case 1: var anf_arg1590 = $ans1586;
$step1582 = 2;
$al1587 = L[561];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1587,_plus1);
}
$ans1586 = _plus1.app(("Empty block at "),anf_arg1590);
break;
case 2: ++R.GAS;
return $ans1586;
default: throw "No case numbered " + $step1582 + " in $temp_full1583";
}
}
} catch($e1591) {
if(R.isCont($e1591) && ($step1582 !== 2)) {
$e1591.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1587,$temp_full1583,$step1582,[self1584,tostring1585],[]);
}
if(R.isPyretException($e1591)) {
$e1591.pyretStack.push($al1587);
}
throw $e1591;
}
};
var anf_variant_member1707 = R.makeMethod1($temp_full1583);
var $temp_full1593 = function($self1594,$tostring1595) {
var $step1592 = 0;
var $ans1596 = D;
var $al1597 = L[566];
try {
if(R.isActivationRecord($self1594)) {
$step1592 = $self1594.step;
$al1597 = $self1594.from;
$ans1596 = $self1594.ans;
self1594 = $self1594.args[0];
tostring1595 = $self1594.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[566],2,$t);
}
var self1594 = $self1594;
var tostring1595 = $tostring1595;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1592) {
case 0: var anf_method_obj1598 = G(self1594,"loc",L[563]);
$step1592 = 1;
$al1597 = L[564];
$field1599 = R.getColonFieldLoc(anf_method_obj1598,"format",L[564]);
if(R.isMethod($field1599)) {
$ans1596 = $field1599.full_meth(anf_method_obj1598,(true));
} else {
if(!(R.isFunction($field1599))) {
R.ffi.throwNonFunApp(L[564],$field1599);
}
$ans1596 = $field1599.app((true));
}
break;
case 1: var anf_arg1600 = $ans1596;
$step1592 = 2;
$al1597 = L[565];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1597,_plus1);
}
$ans1596 = _plus1.app(("Expected a val binding or an expression, but got something else "),anf_arg1600);
break;
case 2: ++R.GAS;
return $ans1596;
default: throw "No case numbered " + $step1592 + " in $temp_full1593";
}
}
} catch($e1601) {
if(R.isCont($e1601) && ($step1592 !== 2)) {
$e1601.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1597,$temp_full1593,$step1592,[self1594,tostring1595],[]);
}
if(R.isPyretException($e1601)) {
$e1601.pyretStack.push($al1597);
}
throw $e1601;
}
};
var anf_variant_member1717 = R.makeMethod1($temp_full1593);
var $temp_full1603 = function($self1604,$tostring1605) {
var $step1602 = 0;
var $ans1606 = D;
var $al1607 = L[570];
try {
if(R.isActivationRecord($self1604)) {
$step1602 = $self1604.step;
$al1607 = $self1604.from;
$ans1606 = $self1604.ans;
self1604 = $self1604.args[0];
tostring1605 = $self1604.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[570],2,$t);
}
var self1604 = $self1604;
var tostring1605 = $tostring1605;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1602) {
case 0: var anf_method_obj1608 = G(self1604,"loc",L[567]);
$step1602 = 1;
$al1607 = L[568];
$field1609 = R.getColonFieldLoc(anf_method_obj1608,"format",L[568]);
if(R.isMethod($field1609)) {
$ans1606 = $field1609.full_meth(anf_method_obj1608,(true));
} else {
if(!(R.isFunction($field1609))) {
R.ffi.throwNonFunApp(L[568],$field1609);
}
$ans1606 = $field1609.app((true));
}
break;
case 1: var anf_arg1610 = $ans1606;
$step1602 = 2;
$al1607 = L[569];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1607,_plus1);
}
$ans1606 = _plus1.app(("Expected a val binding or an expression, but got something else "),anf_arg1610);
break;
case 2: ++R.GAS;
return $ans1606;
default: throw "No case numbered " + $step1602 + " in $temp_full1603";
}
}
} catch($e1611) {
if(R.isCont($e1611) && ($step1602 !== 2)) {
$e1611.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1607,$temp_full1603,$step1602,[self1604,tostring1605],[]);
}
if(R.isPyretException($e1611)) {
$e1611.pyretStack.push($al1607);
}
throw $e1611;
}
};
var anf_variant_member1727 = R.makeMethod1($temp_full1603);
var $temp_full1613 = function($self1614,$tostring1615) {
var $step1612 = 0;
var $ans1616 = D;
var $al1617 = L[574];
try {
if(R.isActivationRecord($self1614)) {
$step1612 = $self1614.step;
$al1617 = $self1614.from;
$ans1616 = $self1614.ans;
self1614 = $self1614.args[0];
tostring1615 = $self1614.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[574],2,$t);
}
var self1614 = $self1614;
var tostring1615 = $tostring1615;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1612) {
case 0: var anf_method_obj1618 = G(self1614,"loc",L[571]);
$step1612 = 1;
$al1617 = L[572];
$field1619 = R.getColonFieldLoc(anf_method_obj1618,"format",L[572]);
if(R.isMethod($field1619)) {
$ans1616 = $field1619.full_meth(anf_method_obj1618,(true));
} else {
if(!(R.isFunction($field1619))) {
R.ffi.throwNonFunApp(L[572],$field1619);
}
$ans1616 = $field1619.app((true));
}
break;
case 1: var anf_arg1620 = $ans1616;
$step1612 = 2;
$al1617 = L[573];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1617,_plus1);
}
$ans1616 = _plus1.app(("fun-missing-colon: "),anf_arg1620);
break;
case 2: ++R.GAS;
return $ans1616;
default: throw "No case numbered " + $step1612 + " in $temp_full1613";
}
}
} catch($e1621) {
if(R.isCont($e1621) && ($step1612 !== 2)) {
$e1621.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1617,$temp_full1613,$step1612,[self1614,tostring1615],[]);
}
if(R.isPyretException($e1621)) {
$e1621.pyretStack.push($al1617);
}
throw $e1621;
}
};
var anf_variant_member1737 = R.makeMethod1($temp_full1613);
var $temp_full1623 = function($self1624,$tostring1625) {
var $step1622 = 0;
var $ans1626 = D;
var $al1627 = L[578];
try {
if(R.isActivationRecord($self1624)) {
$step1622 = $self1624.step;
$al1627 = $self1624.from;
$ans1626 = $self1624.ans;
self1624 = $self1624.args[0];
tostring1625 = $self1624.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[578],2,$t);
}
var self1624 = $self1624;
var tostring1625 = $tostring1625;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1622) {
case 0: var anf_method_obj1628 = G(self1624,"loc",L[575]);
$step1622 = 1;
$al1627 = L[576];
$field1629 = R.getColonFieldLoc(anf_method_obj1628,"format",L[576]);
if(R.isMethod($field1629)) {
$ans1626 = $field1629.full_meth(anf_method_obj1628,(true));
} else {
if(!(R.isFunction($field1629))) {
R.ffi.throwNonFunApp(L[576],$field1629);
}
$ans1626 = $field1629.app((true));
}
break;
case 1: var anf_arg1630 = $ans1626;
$step1622 = 2;
$al1627 = L[577];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1627,_plus1);
}
$ans1626 = _plus1.app(("fun-missing-end: "),anf_arg1630);
break;
case 2: ++R.GAS;
return $ans1626;
default: throw "No case numbered " + $step1622 + " in $temp_full1623";
}
}
} catch($e1631) {
if(R.isCont($e1631) && ($step1622 !== 2)) {
$e1631.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1627,$temp_full1623,$step1622,[self1624,tostring1625],[]);
}
if(R.isPyretException($e1631)) {
$e1631.pyretStack.push($al1627);
}
throw $e1631;
}
};
var anf_variant_member1747 = R.makeMethod1($temp_full1623);
var $temp_full1633 = function($self1634,$tostring1635) {
var $step1632 = 0;
var $ans1636 = D;
var $al1637 = L[582];
try {
if(R.isActivationRecord($self1634)) {
$step1632 = $self1634.step;
$al1637 = $self1634.from;
$ans1636 = $self1634.ans;
self1634 = $self1634.args[0];
tostring1635 = $self1634.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[582],2,$t);
}
var self1634 = $self1634;
var tostring1635 = $tostring1635;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1632) {
case 0: var anf_method_obj1638 = G(self1634,"loc",L[579]);
$step1632 = 1;
$al1637 = L[580];
$field1639 = R.getColonFieldLoc(anf_method_obj1638,"format",L[580]);
if(R.isMethod($field1639)) {
$ans1636 = $field1639.full_meth(anf_method_obj1638,(true));
} else {
if(!(R.isFunction($field1639))) {
R.ffi.throwNonFunApp(L[580],$field1639);
}
$ans1636 = $field1639.app((true));
}
break;
case 1: var anf_arg1640 = $ans1636;
$step1632 = 2;
$al1637 = L[581];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1637,_plus1);
}
$ans1636 = _plus1.app(("args-missing-comma: "),anf_arg1640);
break;
case 2: ++R.GAS;
return $ans1636;
default: throw "No case numbered " + $step1632 + " in $temp_full1633";
}
}
} catch($e1641) {
if(R.isCont($e1641) && ($step1632 !== 2)) {
$e1641.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1637,$temp_full1633,$step1632,[self1634,tostring1635],[]);
}
if(R.isPyretException($e1641)) {
$e1641.pyretStack.push($al1637);
}
throw $e1641;
}
};
var anf_variant_member1757 = R.makeMethod1($temp_full1633);
var $temp_full1643 = function($self1644,$tostring1645) {
var $step1642 = 0;
var $ans1646 = D;
var $al1647 = L[586];
try {
if(R.isActivationRecord($self1644)) {
$step1642 = $self1644.step;
$al1647 = $self1644.from;
$ans1646 = $self1644.ans;
self1644 = $self1644.args[0];
tostring1645 = $self1644.args[1];
} else {
var $l = arguments.length;
if($l !== 2) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[586],2,$t);
}
var self1644 = $self1644;
var tostring1645 = $tostring1645;
}
if(--R.GAS <= 0) {
R.EXN_STACKHEIGHT = 0;
throw R.makeCont();
}
while(true) {
switch($step1642) {
case 0: var anf_method_obj1648 = G(self1644,"loc",L[583]);
$step1642 = 1;
$al1647 = L[584];
$field1649 = R.getColonFieldLoc(anf_method_obj1648,"format",L[584]);
if(R.isMethod($field1649)) {
$ans1646 = $field1649.full_meth(anf_method_obj1648,(true));
} else {
if(!(R.isFunction($field1649))) {
R.ffi.throwNonFunApp(L[584],$field1649);
}
$ans1646 = $field1649.app((true));
}
break;
case 1: var anf_arg1650 = $ans1646;
$step1642 = 2;
$al1647 = L[585];
if(!(R.isFunction(_plus1))) {
R.ffi.throwNonFunApp($al1647,_plus1);
}
$ans1646 = _plus1.app(("app-args-missing-comma: "),anf_arg1650);
break;
case 2: ++R.GAS;
return $ans1646;
default: throw "No case numbered " + $step1642 + " in $temp_full1643";
}
}
} catch($e1651) {
if(R.isCont($e1651) && ($step1642 !== 2)) {
$e1651.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al1647,$temp_full1643,$step1642,[self1644,tostring1645],[]);
}
if(R.isPyretException($e1651)) {
$e1651.pyretStack.push($al1647);
}
throw $e1651;
}
};
var anf_variant_member1767 = R.makeMethod1($temp_full1643);
var $parse$error$next$token_getfields1659 = function(f) {
return f(this.dict["loc"],this.dict["next-token"]);
};
var $parse$error$next$token_getfieldsref1657 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]),R.derefField(this.dict["next-token"],false,refmask[1]));
};
var $parse$error$next$token_mutablemask1658 = [false,false];
var $parse$error$next$token$base1653 = {"$fieldNames":["loc","next-token"],
"render-reason":anf_shared1652,
"render-reason":anf_variant_member1656,
"_match":R.makeMatch("parse-error-next-token",2)};
var $parse$error$next$token$brands1655 = {"$brand$parse$error$next$token":true};
$parse$error$next$token$brands1655[ParseError22._brand] = true;
var parse$error$next$token1661 = R.makeVariantConstructor(L[589],function() {
return [$type$String7];
},["next$token1662"],[L[588]],[false,false],["loc1663","next$token1662"],$parse$error$next$token_mutablemask1658,$parse$error$next$token$base1653,$parse$error$next$token$brands1655,"parse-error-next-token",$parse$error$next$token_getfieldsref1657,$parse$error$next$token_getfields1659,$parse$error$next$token$base1653);
var $parse$error$eof_getfields1670 = function(f) {
return f(this.dict["loc"]);
};
var $parse$error$eof_getfieldsref1668 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $parse$error$eof_mutablemask1669 = [false];
var $parse$error$eof$base1664 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"render-reason":anf_variant_member1667,
"_match":R.makeMatch("parse-error-eof",1)};
var $parse$error$eof$brands1666 = {"$brand$parse$error$eof":true};
$parse$error$eof$brands1666[ParseError22._brand] = true;
var parse$error$eof1672 = R.makeVariantConstructor(L[591],function() {
return [];
},[],[],[false],["loc1673"],$parse$error$eof_mutablemask1669,$parse$error$eof$base1664,$parse$error$eof$brands1666,"parse-error-eof",$parse$error$eof_getfieldsref1668,$parse$error$eof_getfields1670,$parse$error$eof$base1664);
var $parse$error$unterminated$string_getfields1680 = function(f) {
return f(this.dict["loc"]);
};
var $parse$error$unterminated$string_getfieldsref1678 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $parse$error$unterminated$string_mutablemask1679 = [false];
var $parse$error$unterminated$string$base1674 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"render-reason":anf_variant_member1677,
"_match":R.makeMatch("parse-error-unterminated-string",1)};
var $parse$error$unterminated$string$brands1676 = {"$brand$parse$error$unterminated$string":true};
$parse$error$unterminated$string$brands1676[ParseError22._brand] = true;
var parse$error$unterminated$string1682 = R.makeVariantConstructor(L[593],function() {
return [];
},[],[],[false],["loc1683"],$parse$error$unterminated$string_mutablemask1679,$parse$error$unterminated$string$base1674,$parse$error$unterminated$string$brands1676,"parse-error-unterminated-string",$parse$error$unterminated$string_getfieldsref1678,$parse$error$unterminated$string_getfields1680,$parse$error$unterminated$string$base1674);
var $parse$error$bad$operator_getfields1690 = function(f) {
return f(this.dict["loc"]);
};
var $parse$error$bad$operator_getfieldsref1688 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $parse$error$bad$operator_mutablemask1689 = [false];
var $parse$error$bad$operator$base1684 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"render-reason":anf_variant_member1687,
"_match":R.makeMatch("parse-error-bad-operator",1)};
var $parse$error$bad$operator$brands1686 = {"$brand$parse$error$bad$operator":true};
$parse$error$bad$operator$brands1686[ParseError22._brand] = true;
var parse$error$bad$operator1692 = R.makeVariantConstructor(L[595],function() {
return [];
},[],[],[false],["loc1693"],$parse$error$bad$operator_mutablemask1689,$parse$error$bad$operator$base1684,$parse$error$bad$operator$brands1686,"parse-error-bad-operator",$parse$error$bad$operator_getfieldsref1688,$parse$error$bad$operator_getfields1690,$parse$error$bad$operator$base1684);
var $parse$error$bad$number_getfields1700 = function(f) {
return f(this.dict["loc"]);
};
var $parse$error$bad$number_getfieldsref1698 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $parse$error$bad$number_mutablemask1699 = [false];
var $parse$error$bad$number$base1694 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"render-reason":anf_variant_member1697,
"_match":R.makeMatch("parse-error-bad-number",1)};
var $parse$error$bad$number$brands1696 = {"$brand$parse$error$bad$number":true};
$parse$error$bad$number$brands1696[ParseError22._brand] = true;
var parse$error$bad$number1702 = R.makeVariantConstructor(L[597],function() {
return [];
},[],[],[false],["loc1703"],$parse$error$bad$number_mutablemask1699,$parse$error$bad$number$base1694,$parse$error$bad$number$brands1696,"parse-error-bad-number",$parse$error$bad$number_getfieldsref1698,$parse$error$bad$number_getfields1700,$parse$error$bad$number$base1694);
var $empty$block_getfields1710 = function(f) {
return f(this.dict["loc"]);
};
var $empty$block_getfieldsref1708 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $empty$block_mutablemask1709 = [false];
var $empty$block$base1704 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1707,
"_match":R.makeMatch("empty-block",1)};
var $empty$block$brands1706 = {"$brand$empty$block":true};
$empty$block$brands1706[ParseError22._brand] = true;
var empty$block1712 = R.makeVariantConstructor(L[599],function() {
return [];
},[],[],[false],["loc1713"],$empty$block_mutablemask1709,$empty$block$base1704,$empty$block$brands1706,"empty-block",$empty$block_getfieldsref1708,$empty$block_getfields1710,$empty$block$base1704);
var $bad$block$stmt_getfields1720 = function(f) {
return f(this.dict["loc"]);
};
var $bad$block$stmt_getfieldsref1718 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $bad$block$stmt_mutablemask1719 = [false];
var $bad$block$stmt$base1714 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1717,
"_match":R.makeMatch("bad-block-stmt",1)};
var $bad$block$stmt$brands1716 = {"$brand$bad$block$stmt":true};
$bad$block$stmt$brands1716[ParseError22._brand] = true;
var bad$block$stmt1722 = R.makeVariantConstructor(L[601],function() {
return [];
},[],[],[false],["loc1723"],$bad$block$stmt_mutablemask1719,$bad$block$stmt$base1714,$bad$block$stmt$brands1716,"bad-block-stmt",$bad$block$stmt_getfieldsref1718,$bad$block$stmt_getfields1720,$bad$block$stmt$base1714);
var $bad$check$block$stmt_getfields1730 = function(f) {
return f(this.dict["loc"]);
};
var $bad$check$block$stmt_getfieldsref1728 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $bad$check$block$stmt_mutablemask1729 = [false];
var $bad$check$block$stmt$base1724 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1727,
"_match":R.makeMatch("bad-check-block-stmt",1)};
var $bad$check$block$stmt$brands1726 = {"$brand$bad$check$block$stmt":true};
$bad$check$block$stmt$brands1726[ParseError22._brand] = true;
var bad$check$block$stmt1732 = R.makeVariantConstructor(L[603],function() {
return [];
},[],[],[false],["loc1733"],$bad$check$block$stmt_mutablemask1729,$bad$check$block$stmt$base1724,$bad$check$block$stmt$brands1726,"bad-check-block-stmt",$bad$check$block$stmt_getfieldsref1728,$bad$check$block$stmt_getfields1730,$bad$check$block$stmt$base1724);
var $fun$missing$colon_getfields1740 = function(f) {
return f(this.dict["loc"]);
};
var $fun$missing$colon_getfieldsref1738 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $fun$missing$colon_mutablemask1739 = [false];
var $fun$missing$colon$base1734 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1737,
"_match":R.makeMatch("fun-missing-colon",1)};
var $fun$missing$colon$brands1736 = {"$brand$fun$missing$colon":true};
$fun$missing$colon$brands1736[ParseError22._brand] = true;
var fun$missing$colon1742 = R.makeVariantConstructor(L[605],function() {
return [];
},[],[],[false],["loc1743"],$fun$missing$colon_mutablemask1739,$fun$missing$colon$base1734,$fun$missing$colon$brands1736,"fun-missing-colon",$fun$missing$colon_getfieldsref1738,$fun$missing$colon_getfields1740,$fun$missing$colon$base1734);
var $fun$missing$end_getfields1750 = function(f) {
return f(this.dict["loc"]);
};
var $fun$missing$end_getfieldsref1748 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $fun$missing$end_mutablemask1749 = [false];
var $fun$missing$end$base1744 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1747,
"_match":R.makeMatch("fun-missing-end",1)};
var $fun$missing$end$brands1746 = {"$brand$fun$missing$end":true};
$fun$missing$end$brands1746[ParseError22._brand] = true;
var fun$missing$end1752 = R.makeVariantConstructor(L[607],function() {
return [];
},[],[],[false],["loc1753"],$fun$missing$end_mutablemask1749,$fun$missing$end$base1744,$fun$missing$end$brands1746,"fun-missing-end",$fun$missing$end_getfieldsref1748,$fun$missing$end_getfields1750,$fun$missing$end$base1744);
var $args$missing$comma_getfields1760 = function(f) {
return f(this.dict["loc"]);
};
var $args$missing$comma_getfieldsref1758 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $args$missing$comma_mutablemask1759 = [false];
var $args$missing$comma$base1754 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1757,
"_match":R.makeMatch("args-missing-comma",1)};
var $args$missing$comma$brands1756 = {"$brand$args$missing$comma":true};
$args$missing$comma$brands1756[ParseError22._brand] = true;
var args$missing$comma1762 = R.makeVariantConstructor(L[609],function() {
return [];
},[],[],[false],["loc1763"],$args$missing$comma_mutablemask1759,$args$missing$comma$base1754,$args$missing$comma$brands1756,"args-missing-comma",$args$missing$comma_getfieldsref1758,$args$missing$comma_getfields1760,$args$missing$comma$base1754);
var $app$args$missing$comma_getfields1770 = function(f) {
return f(this.dict["loc"]);
};
var $app$args$missing$comma_getfieldsref1768 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $app$args$missing$comma_mutablemask1769 = [false];
var $app$args$missing$comma$base1764 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_tostring":anf_variant_member1767,
"_match":R.makeMatch("app-args-missing-comma",1)};
var $app$args$missing$comma$brands1766 = {"$brand$app$args$missing$comma":true};
$app$args$missing$comma$brands1766[ParseError22._brand] = true;
var app$args$missing$comma1772 = R.makeVariantConstructor(L[611],function() {
return [];
},[],[],[false],["loc1773"],$app$args$missing$comma_mutablemask1769,$app$args$missing$comma$base1764,$app$args$missing$comma$brands1766,"app-args-missing-comma",$app$args$missing$comma_getfieldsref1768,$app$args$missing$comma_getfields1770,$app$args$missing$comma$base1764);
var $missing$end_getfields1779 = function(f) {
return f(this.dict["loc"]);
};
var $missing$end_getfieldsref1777 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $missing$end_mutablemask1778 = [false];
var $missing$end$base1774 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_match":R.makeMatch("missing-end",1)};
var $missing$end$brands1776 = {"$brand$missing$end":true};
$missing$end$brands1776[ParseError22._brand] = true;
var missing$end1781 = R.makeVariantConstructor(L[613],function() {
return [];
},[],[],[false],["loc1782"],$missing$end_mutablemask1778,$missing$end$base1774,$missing$end$brands1776,"missing-end",$missing$end_getfieldsref1777,$missing$end_getfields1779,$missing$end$base1774);
var $missing$comma_getfields1788 = function(f) {
return f(this.dict["loc"]);
};
var $missing$comma_getfieldsref1786 = function(f,refmask) {
return f(R.derefField(this.dict["loc"],false,refmask[0]));
};
var $missing$comma_mutablemask1787 = [false];
var $missing$comma$base1783 = {"$fieldNames":["loc"],
"render-reason":anf_shared1652,
"_match":R.makeMatch("missing-comma",1)};
var $missing$comma$brands1785 = {"$brand$missing$comma":true};
$missing$comma$brands1785[ParseError22._brand] = true;
var missing$comma1790 = R.makeVariantConstructor(L[615],function() {
return [];
},[],[],[false],["loc1791"],$missing$comma_mutablemask1787,$missing$comma$base1783,$missing$comma$brands1785,"missing-comma",$missing$comma_getfieldsref1786,$missing$comma_getfields1788,$missing$comma$base1783);
var anf_assign1793 = R.makeObject({"ParseError":R.makeFunction(function($val1792) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[616],1,$t);
}
return R.makeBoolean(R.hasBrand($val1792,ParseError22._brand));
}),
"is-parse-error-next-token":R.makeFunction(function($val1660) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[587],1,$t);
}
return R.makeBoolean(R.hasBrand($val1660,"$brand$parse$error$next$token"));
}),
"parse-error-next-token":parse$error$next$token1661,
"is-parse-error-eof":R.makeFunction(function($val1671) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[590],1,$t);
}
return R.makeBoolean(R.hasBrand($val1671,"$brand$parse$error$eof"));
}),
"parse-error-eof":parse$error$eof1672,
"is-parse-error-unterminated-string":R.makeFunction(function($val1681) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[592],1,$t);
}
return R.makeBoolean(R.hasBrand($val1681,"$brand$parse$error$unterminated$string"));
}),
"parse-error-unterminated-string":parse$error$unterminated$string1682,
"is-parse-error-bad-operator":R.makeFunction(function($val1691) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[594],1,$t);
}
return R.makeBoolean(R.hasBrand($val1691,"$brand$parse$error$bad$operator"));
}),
"parse-error-bad-operator":parse$error$bad$operator1692,
"is-parse-error-bad-number":R.makeFunction(function($val1701) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[596],1,$t);
}
return R.makeBoolean(R.hasBrand($val1701,"$brand$parse$error$bad$number"));
}),
"parse-error-bad-number":parse$error$bad$number1702,
"is-empty-block":R.makeFunction(function($val1711) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[598],1,$t);
}
return R.makeBoolean(R.hasBrand($val1711,"$brand$empty$block"));
}),
"empty-block":empty$block1712,
"is-bad-block-stmt":R.makeFunction(function($val1721) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[600],1,$t);
}
return R.makeBoolean(R.hasBrand($val1721,"$brand$bad$block$stmt"));
}),
"bad-block-stmt":bad$block$stmt1722,
"is-bad-check-block-stmt":R.makeFunction(function($val1731) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[602],1,$t);
}
return R.makeBoolean(R.hasBrand($val1731,"$brand$bad$check$block$stmt"));
}),
"bad-check-block-stmt":bad$check$block$stmt1732,
"is-fun-missing-colon":R.makeFunction(function($val1741) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[604],1,$t);
}
return R.makeBoolean(R.hasBrand($val1741,"$brand$fun$missing$colon"));
}),
"fun-missing-colon":fun$missing$colon1742,
"is-fun-missing-end":R.makeFunction(function($val1751) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[606],1,$t);
}
return R.makeBoolean(R.hasBrand($val1751,"$brand$fun$missing$end"));
}),
"fun-missing-end":fun$missing$end1752,
"is-args-missing-comma":R.makeFunction(function($val1761) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[608],1,$t);
}
return R.makeBoolean(R.hasBrand($val1761,"$brand$args$missing$comma"));
}),
"args-missing-comma":args$missing$comma1762,
"is-app-args-missing-comma":R.makeFunction(function($val1771) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[610],1,$t);
}
return R.makeBoolean(R.hasBrand($val1771,"$brand$app$args$missing$comma"));
}),
"app-args-missing-comma":app$args$missing$comma1772,
"is-missing-end":R.makeFunction(function($val1780) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[612],1,$t);
}
return R.makeBoolean(R.hasBrand($val1780,"$brand$missing$end"));
}),
"missing-end":missing$end1781,
"is-missing-comma":R.makeFunction(function($val1789) {
var $l = arguments.length;
if($l !== 1) {
var $t = new Array($l);
for(var $i = 0;$i < $l;++$i) {
$t[$i] = arguments[$i];
}
R.checkArityC(L[614],1,$t);
}
return R.makeBoolean(R.hasBrand($val1789,"$brand$missing$comma"));
}),
"missing-comma":missing$comma1790});
ParseError1794.$var = anf_assign1793;
var anf_assign1795 = G(ParseError1794.$var,"ParseError",L[616]);
ParseError1796.$var = anf_assign1795;
var anf_assign1797 = G(ParseError1794.$var,"ParseError",L[616]);
is$ParseError1798.$var = anf_assign1797;
var anf_assign1799 = G(ParseError1794.$var,"is-parse-error-next-token",L[587]);
is$parse$error$next$token1800.$var = anf_assign1799;
var anf_assign1801 = G(ParseError1794.$var,"parse-error-next-token",L[587]);
parse$error$next$token1802.$var = anf_assign1801;
var anf_assign1803 = G(ParseError1794.$var,"is-parse-error-eof",L[590]);
is$parse$error$eof1804.$var = anf_assign1803;
var anf_assign1805 = G(ParseError1794.$var,"parse-error-eof",L[590]);
parse$error$eof1806.$var = anf_assign1805;
var anf_assign1807 = G(ParseError1794.$var,"is-parse-error-unterminated-string",L[592]);
is$parse$error$unterminated$string1808.$var = anf_assign1807;
var anf_assign1809 = G(ParseError1794.$var,"parse-error-unterminated-string",L[592]);
parse$error$unterminated$string1810.$var = anf_assign1809;
var anf_assign1811 = G(ParseError1794.$var,"is-parse-error-bad-operator",L[594]);
is$parse$error$bad$operator1812.$var = anf_assign1811;
var anf_assign1813 = G(ParseError1794.$var,"parse-error-bad-operator",L[594]);
parse$error$bad$operator1814.$var = anf_assign1813;
var anf_assign1815 = G(ParseError1794.$var,"is-parse-error-bad-number",L[596]);
is$parse$error$bad$number1816.$var = anf_assign1815;
var anf_assign1817 = G(ParseError1794.$var,"parse-error-bad-number",L[596]);
parse$error$bad$number1818.$var = anf_assign1817;
var anf_assign1819 = G(ParseError1794.$var,"is-empty-block",L[598]);
is$empty$block1820.$var = anf_assign1819;
var anf_assign1821 = G(ParseError1794.$var,"empty-block",L[598]);
empty$block1822.$var = anf_assign1821;
var anf_assign1823 = G(ParseError1794.$var,"is-bad-block-stmt",L[600]);
is$bad$block$stmt1824.$var = anf_assign1823;
var anf_assign1825 = G(ParseError1794.$var,"bad-block-stmt",L[600]);
bad$block$stmt1826.$var = anf_assign1825;
var anf_assign1827 = G(ParseError1794.$var,"is-bad-check-block-stmt",L[602]);
is$bad$check$block$stmt1828.$var = anf_assign1827;
var anf_assign1829 = G(ParseError1794.$var,"bad-check-block-stmt",L[602]);
bad$check$block$stmt1830.$var = anf_assign1829;
var anf_assign1831 = G(ParseError1794.$var,"is-fun-missing-colon",L[604]);
is$fun$missing$colon1832.$var = anf_assign1831;
var anf_assign1833 = G(ParseError1794.$var,"fun-missing-colon",L[604]);
fun$missing$colon1834.$var = anf_assign1833;
var anf_assign1835 = G(ParseError1794.$var,"is-fun-missing-end",L[606]);
is$fun$missing$end1836.$var = anf_assign1835;
var anf_assign1837 = G(ParseError1794.$var,"fun-missing-end",L[606]);
fun$missing$end1838.$var = anf_assign1837;
var anf_assign1839 = G(ParseError1794.$var,"is-args-missing-comma",L[608]);
is$args$missing$comma1840.$var = anf_assign1839;
var anf_assign1841 = G(ParseError1794.$var,"args-missing-comma",L[608]);
args$missing$comma1842.$var = anf_assign1841;
var anf_assign1843 = G(ParseError1794.$var,"is-app-args-missing-comma",L[610]);
is$app$args$missing$comma1844.$var = anf_assign1843;
var anf_assign1845 = G(ParseError1794.$var,"app-args-missing-comma",L[610]);
app$args$missing$comma1846.$var = anf_assign1845;
var anf_assign1847 = G(ParseError1794.$var,"is-missing-end",L[612]);
is$missing$end1848.$var = anf_assign1847;
var anf_assign1849 = G(ParseError1794.$var,"missing-end",L[612]);
missing$end1850.$var = anf_assign1849;
var anf_assign1851 = G(ParseError1794.$var,"is-missing-comma",L[614]);
is$missing$comma1852.$var = anf_assign1851;
var anf_assign1853 = G(ParseError1794.$var,"missing-comma",L[614]);
missing$comma1854.$var = anf_assign1853;
var provides1860 = R.makeObject({"draw-and-highlight":draw$and$highlight33.$var,
"vert-list-values":vert$list$values56.$var,
"RuntimeError":RuntimeError1285.$var,
"is-RuntimeError":is$RuntimeError1287.$var,
"message-exception":message$exception1291.$var,
"is-message-exception":is$message$exception1289.$var,
"no-cases-matched":no$cases$matched1295.$var,
"is-no-cases-matched":is$no$cases$matched1293.$var,
"no-branches-matched":no$branches$matched1299.$var,
"is-no-branches-matched":is$no$branches$matched1297.$var,
"internal-error":internal$error1303.$var,
"is-internal-error":is$internal$error1301.$var,
"field-not-found":field$not$found1307.$var,
"is-field-not-found":is$field$not$found1305.$var,
"lookup-non-object":lookup$non$object1311.$var,
"is-lookup-non-object":is$lookup$non$object1309.$var,
"extend-non-object":extend$non$object1315.$var,
"is-extend-non-object":is$extend$non$object1313.$var,
"non-boolean-condition":non$boolean$condition1319.$var,
"is-non-boolean-condition":is$non$boolean$condition1317.$var,
"non-boolean-op":non$boolean$op1323.$var,
"is-non-boolean-op":is$non$boolean$op1321.$var,
"generic-type-mismatch":generic$type$mismatch1327.$var,
"is-generic-type-mismatch":is$generic$type$mismatch1325.$var,
"outside-numeric-range":outside$numeric$range1331.$var,
"is-outside-numeric-range":is$outside$numeric$range1329.$var,
"num-string-binop-error":num$string$binop$error1335.$var,
"is-num-string-binop-error":is$num$string$binop$error1333.$var,
"numeric-binop-error":numeric$binop$error1339.$var,
"is-numeric-binop-error":is$numeric$binop$error1337.$var,
"cases-arity-mismatch":cases$arity$mismatch1343.$var,
"is-cases-arity-mismatch":is$cases$arity$mismatch1341.$var,
"cases-singleton-mismatch":cases$singleton$mismatch1347.$var,
"is-cases-singleton-mismatch":is$cases$singleton$mismatch1345.$var,
"arity-mismatch":arity$mismatch1351.$var,
"is-arity-mismatch":is$arity$mismatch1349.$var,
"non-function-app":non$function$app1355.$var,
"is-non-function-app":is$non$function$app1353.$var,
"bad-app":bad$app1359.$var,
"is-bad-app":is$bad$app1357.$var,
"uninitialized-id":uninitialized$id1363.$var,
"is-uninitialized-id":is$uninitialized$id1361.$var,
"module-load-failure":module$load$failure1367.$var,
"is-module-load-failure":is$module$load$failure1365.$var,
"invalid-array-index":invalid$array$index1371.$var,
"is-invalid-array-index":is$invalid$array$index1369.$var,
"equality-failure":equality$failure1375.$var,
"is-equality-failure":is$equality$failure1373.$var,
"user-break":user$break1379.$var,
"is-user-break":is$user$break1377.$var,
"user-exception":user$exception1383.$var,
"is-user-exception":is$user$exception1381.$var,
"ParseError":ParseError1796.$var,
"is-ParseError":is$ParseError1798.$var,
"parse-error-next-token":parse$error$next$token1802.$var,
"is-parse-error-next-token":is$parse$error$next$token1800.$var,
"parse-error-eof":parse$error$eof1806.$var,
"is-parse-error-eof":is$parse$error$eof1804.$var,
"parse-error-unterminated-string":parse$error$unterminated$string1810.$var,
"is-parse-error-unterminated-string":is$parse$error$unterminated$string1808.$var,
"parse-error-bad-operator":parse$error$bad$operator1814.$var,
"is-parse-error-bad-operator":is$parse$error$bad$operator1812.$var,
"parse-error-bad-number":parse$error$bad$number1818.$var,
"is-parse-error-bad-number":is$parse$error$bad$number1816.$var,
"empty-block":empty$block1822.$var,
"is-empty-block":is$empty$block1820.$var,
"bad-block-stmt":bad$block$stmt1826.$var,
"is-bad-block-stmt":is$bad$block$stmt1824.$var,
"bad-check-block-stmt":bad$check$block$stmt1830.$var,
"is-bad-check-block-stmt":is$bad$check$block$stmt1828.$var,
"fun-missing-colon":fun$missing$colon1834.$var,
"is-fun-missing-colon":is$fun$missing$colon1832.$var,
"fun-missing-end":fun$missing$end1838.$var,
"is-fun-missing-end":is$fun$missing$end1836.$var,
"args-missing-comma":args$missing$comma1842.$var,
"is-args-missing-comma":is$args$missing$comma1840.$var,
"app-args-missing-comma":app$args$missing$comma1846.$var,
"is-app-args-missing-comma":is$app$args$missing$comma1844.$var,
"missing-end":missing$end1850.$var,
"is-missing-end":is$missing$end1848.$var,
"missing-comma":missing$comma1854.$var,
"is-missing-comma":is$missing$comma1852.$var});
$step16 = 1;
$al20 = L[485];
$field1855 = R.getColonFieldLoc(builtins10,"current-checker",L[485]);
if(R.isMethod($field1855)) {
$ans19 = $field1855.full_meth(builtins10);
} else {
if(!(R.isFunction($field1855))) {
R.ffi.throwNonFunApp(L[485],$field1855);
}
$ans19 = $field1855.app();
}
break;
case 1: var anf_method_obj1856 = $ans19;
$step16 = 2;
$al20 = L[485];
$field1857 = R.getColonFieldLoc(anf_method_obj1856,"results",L[485]);
if(R.isMethod($field1857)) {
$ans19 = $field1857.full_meth(anf_method_obj1856);
} else {
if(!(R.isFunction($field1857))) {
R.ffi.throwNonFunApp(L[485],$field1857);
}
$ans19 = $field1857.app();
}
break;
case 2: var checks1861 = $ans19;
$step16 = 3;
$ans19 = R.makeObject({"answer":nothing11,
"namespace":NAMESPACE,
"defined-values":{"missing-comma":missing$comma1854.$var,
"is-missing-comma":is$missing$comma1852.$var,
"missing-end":missing$end1850.$var,
"is-missing-end":is$missing$end1848.$var,
"app-args-missing-comma":app$args$missing$comma1846.$var,
"is-app-args-missing-comma":is$app$args$missing$comma1844.$var,
"args-missing-comma":args$missing$comma1842.$var,
"is-args-missing-comma":is$args$missing$comma1840.$var,
"fun-missing-end":fun$missing$end1838.$var,
"is-fun-missing-end":is$fun$missing$end1836.$var,
"fun-missing-colon":fun$missing$colon1834.$var,
"is-fun-missing-colon":is$fun$missing$colon1832.$var,
"bad-check-block-stmt":bad$check$block$stmt1830.$var,
"is-bad-check-block-stmt":is$bad$check$block$stmt1828.$var,
"bad-block-stmt":bad$block$stmt1826.$var,
"is-bad-block-stmt":is$bad$block$stmt1824.$var,
"empty-block":empty$block1822.$var,
"is-empty-block":is$empty$block1820.$var,
"parse-error-bad-number":parse$error$bad$number1818.$var,
"is-parse-error-bad-number":is$parse$error$bad$number1816.$var,
"parse-error-bad-operator":parse$error$bad$operator1814.$var,
"is-parse-error-bad-operator":is$parse$error$bad$operator1812.$var,
"parse-error-unterminated-string":parse$error$unterminated$string1810.$var,
"is-parse-error-unterminated-string":is$parse$error$unterminated$string1808.$var,
"parse-error-eof":parse$error$eof1806.$var,
"is-parse-error-eof":is$parse$error$eof1804.$var,
"parse-error-next-token":parse$error$next$token1802.$var,
"is-parse-error-next-token":is$parse$error$next$token1800.$var,
"is-ParseError":is$ParseError1798.$var,
"ParseError":ParseError1796.$var,
"user-exception":user$exception1383.$var,
"is-user-exception":is$user$exception1381.$var,
"user-break":user$break1379.$var,
"is-user-break":is$user$break1377.$var,
"equality-failure":equality$failure1375.$var,
"is-equality-failure":is$equality$failure1373.$var,
"invalid-array-index":invalid$array$index1371.$var,
"is-invalid-array-index":is$invalid$array$index1369.$var,
"module-load-failure":module$load$failure1367.$var,
"is-module-load-failure":is$module$load$failure1365.$var,
"uninitialized-id":uninitialized$id1363.$var,
"is-uninitialized-id":is$uninitialized$id1361.$var,
"bad-app":bad$app1359.$var,
"is-bad-app":is$bad$app1357.$var,
"non-function-app":non$function$app1355.$var,
"is-non-function-app":is$non$function$app1353.$var,
"arity-mismatch":arity$mismatch1351.$var,
"is-arity-mismatch":is$arity$mismatch1349.$var,
"cases-singleton-mismatch":cases$singleton$mismatch1347.$var,
"is-cases-singleton-mismatch":is$cases$singleton$mismatch1345.$var,
"cases-arity-mismatch":cases$arity$mismatch1343.$var,
"is-cases-arity-mismatch":is$cases$arity$mismatch1341.$var,
"numeric-binop-error":numeric$binop$error1339.$var,
"is-numeric-binop-error":is$numeric$binop$error1337.$var,
"num-string-binop-error":num$string$binop$error1335.$var,
"is-num-string-binop-error":is$num$string$binop$error1333.$var,
"outside-numeric-range":outside$numeric$range1331.$var,
"is-outside-numeric-range":is$outside$numeric$range1329.$var,
"generic-type-mismatch":generic$type$mismatch1327.$var,
"is-generic-type-mismatch":is$generic$type$mismatch1325.$var,
"non-boolean-op":non$boolean$op1323.$var,
"is-non-boolean-op":is$non$boolean$op1321.$var,
"non-boolean-condition":non$boolean$condition1319.$var,
"is-non-boolean-condition":is$non$boolean$condition1317.$var,
"extend-non-object":extend$non$object1315.$var,
"is-extend-non-object":is$extend$non$object1313.$var,
"lookup-non-object":lookup$non$object1311.$var,
"is-lookup-non-object":is$lookup$non$object1309.$var,
"field-not-found":field$not$found1307.$var,
"is-field-not-found":is$field$not$found1305.$var,
"internal-error":internal$error1303.$var,
"is-internal-error":is$internal$error1301.$var,
"no-branches-matched":no$branches$matched1299.$var,
"is-no-branches-matched":is$no$branches$matched1297.$var,
"no-cases-matched":no$cases$matched1295.$var,
"is-no-cases-matched":is$no$cases$matched1293.$var,
"message-exception":message$exception1291.$var,
"is-message-exception":is$message$exception1289.$var,
"is-RuntimeError":is$RuntimeError1287.$var,
"RuntimeError":RuntimeError1285.$var,
"vert-list-values":vert$list$values56.$var,
"draw-and-highlight":draw$and$highlight33.$var,
"ED":ED12},
"defined-types":{"ParseError":ParseError1859,
"RuntimeError":RuntimeError1858,
"ED":ED13},
"provide-plus-types":R.makeObject({"values":provides1860,
"types":{"RuntimeError":RuntimeError1858,
"ParseError":ParseError1859}}),
"checks":checks1861});
break;
case 3: ++R.GAS;
return $ans19;
default: throw "No case numbered " + $step16 + " in $toplevel17";
}
}
} catch($e1863) {
if(R.isCont($e1863) && ($step16 !== 3)) {
$e1863.stack[R.EXN_STACKHEIGHT++] = R.makeActivationRecord($al20,$toplevel17,$step16,[],[provides1860,missing$comma1854,is$missing$comma1852,missing$end1850,is$missing$end1848,app$args$missing$comma1846,is$app$args$missing$comma1844,args$missing$comma1842,is$args$missing$comma1840,fun$missing$end1838,is$fun$missing$end1836,fun$missing$colon1834,is$fun$missing$colon1832,bad$check$block$stmt1830,is$bad$check$block$stmt1828,bad$block$stmt1826,is$bad$block$stmt1824,empty$block1822,is$empty$block1820,parse$error$bad$number1818,is$parse$error$bad$number1816,parse$error$bad$operator1814,is$parse$error$bad$operator1812,parse$error$unterminated$string1810,is$parse$error$unterminated$string1808,parse$error$eof1806,is$parse$error$eof1804,parse$error$next$token1802,is$parse$error$next$token1800,is$ParseError1798,ParseError1796,user$exception1383,is$user$exception1381,user$break1379,is$user$break1377,equality$failure1375,is$equality$failure1373,invalid$array$index1371,is$invalid$array$index1369,module$load$failure1367,is$module$load$failure1365,uninitialized$id1363,is$uninitialized$id1361,bad$app1359,is$bad$app1357,non$function$app1355,is$non$function$app1353,arity$mismatch1351,is$arity$mismatch1349,cases$singleton$mismatch1347,is$cases$singleton$mismatch1345,cases$arity$mismatch1343,is$cases$arity$mismatch1341,numeric$binop$error1339,is$numeric$binop$error1337,num$string$binop$error1335,is$num$string$binop$error1333,outside$numeric$range1331,is$outside$numeric$range1329,generic$type$mismatch1327,is$generic$type$mismatch1325,non$boolean$op1323,is$non$boolean$op1321,non$boolean$condition1319,is$non$boolean$condition1317,extend$non$object1315,is$extend$non$object1313,lookup$non$object1311,is$lookup$non$object1309,field$not$found1307,is$field$not$found1305,internal$error1303,is$internal$error1301,no$branches$matched1299,is$no$branches$matched1297,no$cases$matched1295,is$no$cases$matched1293,message$exception1291,is$message$exception1289,is$RuntimeError1287,RuntimeError1285,vert$list$values56,draw$and$highlight33,ParseError1859,RuntimeError1858]);
}
if(R.isPyretException($e1863)) {
$e1863.pyretStack.push($al20);
}
throw $e1863;
}
};
return R.safeCall($toplevel17,function(moduleVal) {
R.modules["$src/arr/base/error.arr14"] = moduleVal;
return moduleVal;
},"Evaluating $toplevel");
}})
