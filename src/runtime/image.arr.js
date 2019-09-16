const jsnums = require("./js-numbers.js");
var RUNTIME = require('./runtime.js');

var hasOwnProperty = {}.hasOwnProperty;

/* @stopify flat */
function Color(r, g, b, a) {
  this.red = r;
  this.green = g;
  this.blue = b;
  this.alpha = a;
}

/* @stopify flat */
function isNum(n) { return typeof n === "number"; }

//////////////////////////////////////////////////////////////////////
var makeColor = /* @stopify flat */ function (r, g, b, a) {
  if (a === undefined) { a = 1; }
  if ([r, g, b, a].filter(isNum).length !== 4) {
    throw new Error("Internal error: non-number in makeColor argList ", [r, g, b, a]);
  }
  return new Color(r, g, b, a);
};


/* @stopify flat */
function clamp(num, min, max) {
  if (num < min) { return min; }
  else if (num > max) { return max; }
  else { return num; }
}
var isColor = /* @stopify flat */ function (c) { return typeof c === "object" && c instanceof Color; };
var colorRed = /* @stopify flat */ function (c) { return clamp(c.red); }
var colorGreen = /* @stopify flat */ function (c) { return clamp(c.green); }
var colorBlue = /* @stopify flat */ function (c) { return clamp(c.blue); }
var colorAlpha = /* @stopify flat */ function (c) { return clamp(c.alpha); }

var convertColor = /* @stopify flat */ function (c) {
  if (typeof c === "string") { return colorDb.get(c); }
  else { return c; }
}

// Color database
var ColorDb = /* @stopify flat */ function () {
  this.colors = {};
  this.colorNames = {};
};

ColorDb.prototype.put = /* @stopify flat */ function (name, color) {
  this.colors[name] = color;
  var str = // NOTE(ben): Not flooring the numbers here, because they will all be integers anyway
    colorRed(color) + ", " +
    colorGreen(color) + ", " +
    colorBlue(color) + ", " +
    colorAlpha(color);
  if (this.colorNames[str] === undefined) {
    this.colorNames[str] = name;
  }
};

ColorDb.prototype.get = /* @stopify flat */ function (name) {
  return this.colors[name.toString().toUpperCase()];
};

ColorDb.prototype.colorName = /* @stopify flat */ function colorName(colorStr) {
  var ans = this.colorNames[colorStr];
  if (ans !== undefined) ans = ans.toLowerCase();
  return ans;
}

// FIXME: update toString to handle the primitive field values.

var colorDb = new ColorDb();
colorDb.put("DARK-RED", makeColor(139, 0, 0));
colorDb.put("FIRE-BRICK", makeColor(178, 34, 34));
colorDb.put("DEEP-PINK", makeColor(255, 20, 147));
colorDb.put("INDIAN-RED", makeColor(205, 92, 92));
colorDb.put("MEDIUM-VIOLET-RED", makeColor(199, 21, 133));
colorDb.put("VIOLET-RED", makeColor(208, 32, 144));
colorDb.put("LIGHT-CORAL", makeColor(240, 128, 128));
colorDb.put("HOT-PINK", makeColor(255, 105, 180));
colorDb.put("PALE-VIOLET-RED", makeColor(219, 112, 147));
colorDb.put("LIGHT-PINK", makeColor(255, 182, 193));
colorDb.put("ROSY-BROWN", makeColor(188, 143, 143));
colorDb.put("LAVENDER-BLUSH", makeColor(255, 240, 245));
colorDb.put("SADDLE-BROWN", makeColor(139, 69, 19));
colorDb.put("DARK-ORANGE", makeColor(255, 140, 0));
colorDb.put("DARK-GOLDENRON", makeColor(184, 134, 11));
colorDb.put("SANDY-BROWN", makeColor(244, 164, 96));
colorDb.put("LIGHT-SALMON", makeColor(255, 160, 122));
colorDb.put("DARK-SALMON", makeColor(233, 150, 122));
colorDb.put("NAVAJO-WHITE", makeColor(255, 222, 173));
colorDb.put("PEACH-PUFF", makeColor(255, 218, 185));
colorDb.put("DARK-KHAKI", makeColor(189, 183, 107));
colorDb.put("PALE-GOLDENROD", makeColor(238, 232, 170));
colorDb.put("BLANCHE-DIAMOND", makeColor(255, 235, 205));
colorDb.put("MEDIUM-GOLDENROD", makeColor(234, 234, 173));
colorDb.put("PAPAYA-WHIP", makeColor(255, 239, 213));
colorDb.put("MISTY-ROSE", makeColor(255, 228, 225));
colorDb.put("LEMON-CHIFFON", makeColor(255, 250, 205));
colorDb.put("ANTIQUE-WHITE", makeColor(250, 235, 215));
colorDb.put("CORN-SILK", makeColor(255, 248, 220));
colorDb.put("LIGHT-GOLDENRON-YELLOW", makeColor(250, 250, 210));
colorDb.put("OLD-LACE", makeColor(253, 245, 230));
colorDb.put("LIGHT-YELLOW", makeColor(255, 255, 224));
colorDb.put("FLORAL-WHITE", makeColor(255, 250, 240));
colorDb.put("LAWN-GREEN", makeColor(124, 252, 0));
colorDb.put("GREEN-YELLOW", makeColor(173, 255, 47));
colorDb.put("YELLOW-GREEN", makeColor(154, 205, 50));
colorDb.put("MEDIUM-FOREST-GREEN", makeColor(107, 142, 35));
colorDb.put("OLIVE-DRAB", makeColor(107, 142, 35));
colorDb.put("MEDIUM-FOREST-GREEN", makeColor(107, 142, 35));
colorDb.put("DARK-OLIVE-GREEN", makeColor(85, 107, 47));
colorDb.put("DARK-SEA-GREEN", makeColor(143, 188, 139));
colorDb.put("DARK-GREEN", makeColor(0, 100, 0));
colorDb.put("LIME-GREEN", makeColor(50, 205, 50));
colorDb.put("FOREST-GREEN", makeColor(34, 139, 34));
colorDb.put("SPRING-GREEN", makeColor(0, 255, 127));
colorDb.put("MEDIUM-SPRING-GREEN", makeColor(0, 250, 154));
colorDb.put("SEA-GREEN", makeColor(46, 139, 87));
colorDb.put("MEDIUM-SEA-GREEN", makeColor(60, 179, 113));
colorDb.put("LIGHT-GREEN", makeColor(144, 238, 144));
colorDb.put("PALE-GREEN", makeColor(152, 251, 152));
colorDb.put("MEDIUM-AQUAMARINE", makeColor(102, 205, 170));
colorDb.put("LIGHT-SEA-GREEN", makeColor(32, 178, 170));
colorDb.put("MEDIUM-TURQUOISE", makeColor(72, 209, 204));
colorDb.put("MINT-CREAM", makeColor(245, 255, 250));
colorDb.put("ROYAL-BLUE", makeColor(65, 105, 225));
colorDb.put("DODGER-BLUE", makeColor(30, 144, 255));
colorDb.put("DEEP-SKY-BLUE", makeColor(0, 191, 255));
colorDb.put("CORNFLOWER-BLUE", makeColor(100, 149, 237));
colorDb.put("STEEL-BLUE", makeColor(70, 130, 180));
colorDb.put("LIGHT-SKY-BLUE", makeColor(135, 206, 250));
colorDb.put("DARK-TURQUOISE", makeColor(0, 206, 209));
colorDb.put("DARKTURQUOISE", makeColor(0, 206, 209));
colorDb.put("SKY-BLUE", makeColor(135, 206, 235));
colorDb.put("SKYBLUE", makeColor(135, 206, 235));
colorDb.put("CADET-BLUE", makeColor(96, 160, 160));
colorDb.put("DARK-SLATE-GRAY", makeColor(47, 79, 79));
colorDb.put("LIGHT-STEEL-BLUE", makeColor(176, 196, 222));
colorDb.put("LIGHT-BLUE", makeColor(173, 216, 230));
colorDb.put("POWDER-BLUE", makeColor(176, 224, 230));
colorDb.put("PALE-TURQUOISE", makeColor(175, 238, 238));
colorDb.put("LIGHT-CYAN", makeColor(224, 255, 255));
colorDb.put("ALICE-BLUE", makeColor(240, 248, 255));
colorDb.put("MEDIUM-BLUE", makeColor(0, 0, 205));
colorDb.put("DARK-BLUE", makeColor(0, 0, 139));
colorDb.put("MIDNIGHT-BLUE", makeColor(25, 25, 112));
colorDb.put("BLUE-VIOLET", makeColor(138, 43, 226));
colorDb.put("MEDIUM-SLATE-BLUE", makeColor(123, 104, 238));
colorDb.put("SLATE-BLUE", makeColor(106, 90, 205));
colorDb.put("DARK-SLATE-BLUE", makeColor(72, 61, 139));
colorDb.put("DARK-VIOLET", makeColor(148, 0, 211));
colorDb.put("DARK-ORCHID", makeColor(153, 50, 204));
colorDb.put("MEDIUM-PURPLE", makeColor(147, 112, 219));
colorDb.put("CORNFLOWER-BLUE", makeColor(68, 64, 108));
colorDb.put("MEDIUM-ORCHID", makeColor(186, 85, 211));
colorDb.put("DARK-MAGENTA", makeColor(139, 0, 139));
colorDb.put("GHOST-WHITE", makeColor(248, 248, 255));
colorDb.put("WHITE-SMOKE", makeColor(245, 245, 245));
colorDb.put("LIGHT-GRAY", makeColor(211, 211, 211));
colorDb.put("DARK-GRAY", makeColor(169, 169, 169));
colorDb.put("DIM-GRAY", makeColor(105, 105, 105));

colorDb.put("ORANGE", makeColor(255, 165, 0));
colorDb.put("RED", makeColor(255, 0, 0));
colorDb.put("ORANGERED", makeColor(255, 69, 0));
colorDb.put("TOMATO", makeColor(255, 99, 71));
colorDb.put("DARKRED", makeColor(139, 0, 0));
colorDb.put("RED", makeColor(255, 0, 0));
colorDb.put("FIREBRICK", makeColor(178, 34, 34));
colorDb.put("CRIMSON", makeColor(220, 20, 60));
colorDb.put("DEEPPINK", makeColor(255, 20, 147));
colorDb.put("MAROON", makeColor(176, 48, 96));
colorDb.put("INDIAN RED", makeColor(205, 92, 92));
colorDb.put("INDIANRED", makeColor(205, 92, 92));
colorDb.put("MEDIUM VIOLET RED", makeColor(199, 21, 133));
colorDb.put("MEDIUMVIOLETRED", makeColor(199, 21, 133));
colorDb.put("VIOLET RED", makeColor(208, 32, 144));
colorDb.put("VIOLETRED", makeColor(208, 32, 144));
colorDb.put("LIGHTCORAL", makeColor(240, 128, 128));
colorDb.put("HOTPINK", makeColor(255, 105, 180));
colorDb.put("PALEVIOLETRED", makeColor(219, 112, 147));
colorDb.put("LIGHTPINK", makeColor(255, 182, 193));
colorDb.put("ROSYBROWN", makeColor(188, 143, 143));
colorDb.put("PINK", makeColor(255, 192, 203));
colorDb.put("ORCHID", makeColor(218, 112, 214));
colorDb.put("LAVENDERBLUSH", makeColor(255, 240, 245));
colorDb.put("SNOW", makeColor(255, 250, 250));
colorDb.put("CHOCOLATE", makeColor(210, 105, 30));
colorDb.put("SADDLEBROWN", makeColor(139, 69, 19));
colorDb.put("BROWN", makeColor(132, 60, 36));
colorDb.put("DARKORANGE", makeColor(255, 140, 0));
colorDb.put("CORAL", makeColor(255, 127, 80));
colorDb.put("SIENNA", makeColor(160, 82, 45));
colorDb.put("ORANGE", makeColor(255, 165, 0));
colorDb.put("SALMON", makeColor(250, 128, 114));
colorDb.put("PERU", makeColor(205, 133, 63));
colorDb.put("DARKGOLDENROD", makeColor(184, 134, 11));
colorDb.put("GOLDENROD", makeColor(218, 165, 32));
colorDb.put("SANDYBROWN", makeColor(244, 164, 96));
colorDb.put("LIGHTSALMON", makeColor(255, 160, 122));
colorDb.put("DARKSALMON", makeColor(233, 150, 122));
colorDb.put("GOLD", makeColor(255, 215, 0));
colorDb.put("YELLOW", makeColor(255, 255, 0));
colorDb.put("OLIVE", makeColor(128, 128, 0));
colorDb.put("BURLYWOOD", makeColor(222, 184, 135));
colorDb.put("TAN", makeColor(210, 180, 140));
colorDb.put("NAVAJOWHITE", makeColor(255, 222, 173));
colorDb.put("PEACHPUFF", makeColor(255, 218, 185));
colorDb.put("KHAKI", makeColor(240, 230, 140));
colorDb.put("DARKKHAKI", makeColor(189, 183, 107));
colorDb.put("MOCCASIN", makeColor(255, 228, 181));
colorDb.put("WHEAT", makeColor(245, 222, 179));
colorDb.put("BISQUE", makeColor(255, 228, 196));
colorDb.put("PALEGOLDENROD", makeColor(238, 232, 170));
colorDb.put("BLANCHEDALMOND", makeColor(255, 235, 205));
colorDb.put("MEDIUM GOLDENROD", makeColor(234, 234, 173));
colorDb.put("MEDIUMGOLDENROD", makeColor(234, 234, 173));
colorDb.put("PAPAYAWHIP", makeColor(255, 239, 213));
colorDb.put("MISTYROSE", makeColor(255, 228, 225));
colorDb.put("LEMONCHIFFON", makeColor(255, 250, 205));
colorDb.put("ANTIQUEWHITE", makeColor(250, 235, 215));
colorDb.put("CORNSILK", makeColor(255, 248, 220));
colorDb.put("LIGHTGOLDENRODYELLOW", makeColor(250, 250, 210));
colorDb.put("OLDLACE", makeColor(253, 245, 230));
colorDb.put("LINEN", makeColor(250, 240, 230));
colorDb.put("LIGHTYELLOW", makeColor(255, 255, 224));
colorDb.put("SEASHELL", makeColor(255, 245, 238));
colorDb.put("BEIGE", makeColor(245, 245, 220));
colorDb.put("FLORALWHITE", makeColor(255, 250, 240));
colorDb.put("IVORY", makeColor(255, 255, 240));
colorDb.put("GREEN", makeColor(0, 255, 0));
colorDb.put("LAWNGREEN", makeColor(124, 252, 0));
colorDb.put("CHARTREUSE", makeColor(127, 255, 0));
colorDb.put("GREEN YELLOW", makeColor(173, 255, 47));
colorDb.put("GREENYELLOW", makeColor(173, 255, 47));
colorDb.put("YELLOW GREEN", makeColor(154, 205, 50));
colorDb.put("YELLOWGREEN", makeColor(154, 205, 50));
colorDb.put("MEDIUM FOREST GREEN", makeColor(107, 142, 35));
colorDb.put("OLIVEDRAB", makeColor(107, 142, 35));
colorDb.put("MEDIUMFORESTGREEN", makeColor(107, 142, 35));
colorDb.put("DARK OLIVE GREEN", makeColor(85, 107, 47));
colorDb.put("DARKOLIVEGREEN", makeColor(85, 107, 47));
colorDb.put("DARKSEAGREEN", makeColor(143, 188, 139));
colorDb.put("LIME", makeColor(0, 255, 0));
colorDb.put("DARK GREEN", makeColor(0, 100, 0));
colorDb.put("DARKGREEN", makeColor(0, 100, 0));
colorDb.put("LIME GREEN", makeColor(50, 205, 50));
colorDb.put("LIMEGREEN", makeColor(50, 205, 50));
colorDb.put("FOREST GREEN", makeColor(34, 139, 34));
colorDb.put("FORESTGREEN", makeColor(34, 139, 34));
colorDb.put("SPRING GREEN", makeColor(0, 255, 127));
colorDb.put("SPRINGGREEN", makeColor(0, 255, 127));
colorDb.put("MEDIUM SPRING GREEN", makeColor(0, 250, 154));
colorDb.put("MEDIUMSPRINGGREEN", makeColor(0, 250, 154));
colorDb.put("SEA GREEN", makeColor(46, 139, 87));
colorDb.put("SEAGREEN", makeColor(46, 139, 87));
colorDb.put("MEDIUM SEA GREEN", makeColor(60, 179, 113));
colorDb.put("MEDIUMSEAGREEN", makeColor(60, 179, 113));
colorDb.put("AQUAMARINE", makeColor(112, 216, 144));
colorDb.put("LIGHTGREEN", makeColor(144, 238, 144));
colorDb.put("PALE GREEN", makeColor(152, 251, 152));
colorDb.put("PALEGREEN", makeColor(152, 251, 152));
colorDb.put("MEDIUM AQUAMARINE", makeColor(102, 205, 170));
colorDb.put("MEDIUMAQUAMARINE", makeColor(102, 205, 170));
colorDb.put("TURQUOISE", makeColor(64, 224, 208));
colorDb.put("LIGHTSEAGREEN", makeColor(32, 178, 170));
colorDb.put("MEDIUM TURQUOISE", makeColor(72, 209, 204));
colorDb.put("MEDIUMTURQUOISE", makeColor(72, 209, 204));
colorDb.put("HONEYDEW", makeColor(240, 255, 240));
colorDb.put("MINTCREAM", makeColor(245, 255, 250));
colorDb.put("ROYALBLUE", makeColor(65, 105, 225));
colorDb.put("DODGERBLUE", makeColor(30, 144, 255));
colorDb.put("DEEPSKYBLUE", makeColor(0, 191, 255));
colorDb.put("CORNFLOWERBLUE", makeColor(100, 149, 237));
colorDb.put("STEEL BLUE", makeColor(70, 130, 180));
colorDb.put("STEELBLUE", makeColor(70, 130, 180));
colorDb.put("LIGHTSKYBLUE", makeColor(135, 206, 250));
colorDb.put("DARK TURQUOISE", makeColor(0, 206, 209));
colorDb.put("DARKTURQUOISE", makeColor(0, 206, 209));
colorDb.put("CYAN", makeColor(0, 255, 255));
colorDb.put("AQUA", makeColor(0, 255, 255));
colorDb.put("DARKCYAN", makeColor(0, 139, 139));
colorDb.put("TEAL", makeColor(0, 128, 128));
colorDb.put("SKY BLUE", makeColor(135, 206, 235));
colorDb.put("SKYBLUE", makeColor(135, 206, 235));
colorDb.put("CADET BLUE", makeColor(96, 160, 160));
colorDb.put("CADETBLUE", makeColor(95, 158, 160));
colorDb.put("DARK SLATE GRAY", makeColor(47, 79, 79));
colorDb.put("DARKSLATEGRAY", makeColor(47, 79, 79));
colorDb.put("LIGHTSLATEGRAY", makeColor(119, 136, 153));
colorDb.put("SLATEGRAY", makeColor(112, 128, 144));
colorDb.put("LIGHT STEEL BLUE", makeColor(176, 196, 222));
colorDb.put("LIGHTSTEELBLUE", makeColor(176, 196, 222));
colorDb.put("LIGHT BLUE", makeColor(173, 216, 230));
colorDb.put("LIGHTBLUE", makeColor(173, 216, 230));
colorDb.put("POWDERBLUE", makeColor(176, 224, 230));
colorDb.put("PALETURQUOISE", makeColor(175, 238, 238));
colorDb.put("LIGHTCYAN", makeColor(224, 255, 255));
colorDb.put("ALICEBLUE", makeColor(240, 248, 255));
colorDb.put("AZURE", makeColor(240, 255, 255));
colorDb.put("MEDIUM BLUE", makeColor(0, 0, 205));
colorDb.put("MEDIUMBLUE", makeColor(0, 0, 205));
colorDb.put("DARKBLUE", makeColor(0, 0, 139));
colorDb.put("MIDNIGHT BLUE", makeColor(25, 25, 112));
colorDb.put("MIDNIGHTBLUE", makeColor(25, 25, 112));
colorDb.put("NAVY", makeColor(36, 36, 140));
colorDb.put("BLUE", makeColor(0, 0, 255));
colorDb.put("INDIGO", makeColor(75, 0, 130));
colorDb.put("BLUE VIOLET", makeColor(138, 43, 226));
colorDb.put("BLUEVIOLET", makeColor(138, 43, 226));
colorDb.put("MEDIUM SLATE BLUE", makeColor(123, 104, 238));
colorDb.put("MEDIUMSLATEBLUE", makeColor(123, 104, 238));
colorDb.put("SLATE BLUE", makeColor(106, 90, 205));
colorDb.put("SLATEBLUE", makeColor(106, 90, 205));
colorDb.put("PURPLE", makeColor(160, 32, 240));
colorDb.put("DARK SLATE BLUE", makeColor(72, 61, 139));
colorDb.put("DARKSLATEBLUE", makeColor(72, 61, 139));
colorDb.put("DARKVIOLET", makeColor(148, 0, 211));
colorDb.put("DARK ORCHID", makeColor(153, 50, 204));
colorDb.put("DARKORCHID", makeColor(153, 50, 204));
colorDb.put("MEDIUMPURPLE", makeColor(147, 112, 219));
colorDb.put("CORNFLOWER BLUE", makeColor(68, 64, 108));
colorDb.put("MEDIUM ORCHID", makeColor(186, 85, 211));
colorDb.put("MEDIUMORCHID", makeColor(186, 85, 211));
colorDb.put("MAGENTA", makeColor(255, 0, 255));
colorDb.put("FUCHSIA", makeColor(255, 0, 255));
colorDb.put("DARKMAGENTA", makeColor(139, 0, 139));
colorDb.put("VIOLET", makeColor(238, 130, 238));
colorDb.put("PLUM", makeColor(221, 160, 221));
colorDb.put("LAVENDER", makeColor(230, 230, 250));
colorDb.put("THISTLE", makeColor(216, 191, 216));
colorDb.put("GHOSTWHITE", makeColor(248, 248, 255));
colorDb.put("WHITE", makeColor(255, 255, 255));
colorDb.put("WHITESMOKE", makeColor(245, 245, 245));
colorDb.put("GAINSBORO", makeColor(220, 220, 220));
colorDb.put("LIGHT GRAY", makeColor(211, 211, 211));
colorDb.put("LIGHTGRAY", makeColor(211, 211, 211));
colorDb.put("SILVER", makeColor(192, 192, 192));
colorDb.put("GRAY", makeColor(190, 190, 190));
colorDb.put("DARK GRAY", makeColor(169, 169, 169));
colorDb.put("DARKGRAY", makeColor(169, 169, 169));
colorDb.put("DIM GRAY", makeColor(105, 105, 105));
colorDb.put("DIMGRAY", makeColor(105, 105, 105));
colorDb.put("BLACK", makeColor(0, 0, 0));
colorDb.put("TRANSPARENT", makeColor(0, 0, 0, 0));

// clone: object -> object
// Copies an object.  The new object should respond like the old
// object, including to things like instanceof.

// NOTE(joe): There are much better ways to do this.  This is from
// whalesong/whalesong/js-assembler/runtime-src/baselib.js
// and we're keeping it for now (March 31, 2014) to avoid changing
// potentially fragile prototype semantics
var clone = function (obj) {
  var property;
  var C = function () { };
  C.prototype = obj;
  var c = new C();
  for (property in obj) {
    if (hasOwnProperty.call(obj, property)) {
      c[property] = obj[property];
    }
  }
  return c;
};
// TODO(joe): not sufficient
var equals = /* @stopify flat */ function (v1, v2) { return v1 === v2; };

var imageEquals = /* @stopify flat */ function (left, right) {
  if (!isImage(left) || !isImage(right)) { return false; }
  return left.equals(right);
}
var imageDifference = /* @stopify flat */ function (left, right) {
  if (!isImage(left) || !isImage(right)) { return false; }
  return left.difference(right);
}
//////////////////////////////////////////////////////////////////////

var heir = Object.create;

var isAngle = /* @stopify flat */ function (x) {
  return jsnums.isReal(x) &&
    jsnums.greaterThanOrEqual(x, 0, RUNTIME.NumberErrbacks) &&
    jsnums.lessThan(x, 360, RUNTIME.NumberErrbacks);
};

// Produces true if the value is a color or a color string.
// On the Racket side of things, this is exposed as image-color?.
var isColorOrColorString = /* @stopify flat */ function (thing) {
  return (isColor(thing) ||
    ((typeof (thing) === "string" &&
      typeof (colorDb.get(thing)) != 'undefined')));
};

//////////////////////////////////////////////////////////////////////
// colorString : hexColor Style -> rgba
// Style can be a number (0-1), "solid", "outline" or null
// The above value which is non-number is equivalent to a number 255
var colorString = /* @stopify flat */ function (aColor, aStyle) {
  var styleAlpha = isNaN(aStyle) ? 1.0 : aStyle,
    cAlpha = colorAlpha(aColor);
  // NOTE(ben): Flooring the numbers here so that it's a valid RGBA style string
  return "rgba(" + Math.floor(colorRed(aColor)) + ", " +
    Math.floor(colorGreen(aColor)) + ", " +
    Math.floor(colorBlue(aColor)) + ", " +
    styleAlpha * cAlpha + ")";
};

/* @stopify flat */
function RGBtoLAB(r, g, b) {
  /* @stopify flat */ function RGBtoXYZ(r, g, b) {
      /* @stopify flat */ function process(v) {
      v = parseFloat(v / 255);
      return (v > 0.04045 ? Math.pow((v + 0.055) / 1.055, 2.4) : v / 12.92) * 100;
    }
    var var_R = process(r), var_G = process(g), var_B = process(b);
    //Observer. = 2°, Illuminant = D65
    var X = var_R * 0.4124 + var_G * 0.3576 + var_B * 0.1805;
    var Y = var_R * 0.2126 + var_G * 0.7152 + var_B * 0.0722;
    var Z = var_R * 0.0193 + var_G * 0.1192 + var_B * 0.9505;
    return [X, Y, Z];
  }

  /* @stopify flat */ function XYZtoLAB(x, y, z) {
    var var_X = x / 95.047;           //ref_X =  95.047   Observer= 2°, Illuminant= D65
    var var_Y = y / 100.000;          //ref_Y = 100.000
    var var_Z = z / 108.883;          //ref_Z = 108.883
    function process(v) { return v > 0.008856 ? Math.pow(v, 1 / 3) : (7.787 * v) + (16 / 116); }
    var_X = process(var_X); var_Y = process(var_Y); var_Z = process(var_Z);
    var CIE_L = (116 * var_Y) - 16;
    var CIE_a = 500 * (var_X - var_Y);
    var CIE_b = 200 * (var_Y - var_Z);
    return [CIE_L, CIE_a, CIE_b];
  }
  var xyz = RGBtoXYZ(r, g, b), lab = XYZtoLAB(xyz[0], xyz[1], xyz[2]);
  return { l: lab[0], a: lab[1], b: lab[2] };
}
var colorLabs = [], colorRgbs = colorDb.colors;
for (var p in colorRgbs) {
  if (colorRgbs.hasOwnProperty(p)) {
    // NOTE(ben): Not flooring numbers here, since RGBtoLAB supports float values
    var lab = RGBtoLAB(colorRed(colorRgbs[p]),
      colorGreen(colorRgbs[p]),
      colorBlue(colorRgbs[p]));
    colorLabs.push({ name: p, l: lab.l, a: lab.a, b: lab.b });
  }
}

//////////////////////////////////////////////////////////////////////
// colorToSpokenString : hexColor Style -> String
// Describes the color using the nearest HTML color name
// Style can be "solid" (1.0), "outline" (1.0), a number (0-1.0) or null (1.0)
/* @stopify flat */ function colorToSpokenString(aColor, aStyle) {
  if (aStyle === 0) return " transparent ";
  // NOTE(ben): Not flooring numbers here, since RGBtoLAB supports float values
  var lab1 = RGBtoLAB(colorRed(aColor),
    colorGreen(aColor),
    colorBlue(aColor));
  var distances = colorLabs.map(/* @stopify flat */ function (lab2) {
    return {
      l: lab2.l, a: lab2.a, b: lab2.b, name: lab2.name,
      d: Math.sqrt(Math.pow(lab1.l - lab2.l, 2)
        + Math.pow(lab1.a - lab2.a, 2)
        + Math.pow(lab1.b - lab2.b, 2))
    }
  });
  distances = distances.sort(/* @stopify flat */ function (a, b) { return a.d < b.d ? -1 : a.d > b.d ? 1 : 0; });
  var match = distances[0].name;
  var style = isNaN(aStyle) ? (aStyle === "solid" ? " solid" : "n outline") : " translucent ";
  return style + " " + match.toLowerCase();
}


var isSideCount = /* @stopify flat */ function (x) {
  return typeof x === "number" && x >= 3;
  // return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 3, RUNTIME.NumberErrbacks);
};

var isStepCount = /* @stopify flat */ function (x) {
  return typeof x === "number" && x >= 1;
  // return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 1, RUNTIME.NumberErrbacks);
};

var isPointsCount = /* @stopify flat */ function (x) {
  return typeof x === "number" && x >= 2;
  // return jsnums.isInteger(x) && jsnums.greaterThanOrEqual(x, 2, RUNTIME.NumberErrbacks);
};

// Produces true if thing is an image-like object.
var isImage = /* @stopify flat */ function (thing) {
  if (typeof (thing.getHeight) !== 'function')
    return false;
  if (typeof (thing.getWidth) !== 'function')
    return false;
  if (typeof (thing.getBaseline) !== 'function')
    return false;
  if (typeof (thing.updatePinhole) !== 'function')
    return false;
  if (typeof (thing.render) !== 'function')
    return false;
  return true;
};


// given two arrays of {x,y} structs, determine their equivalence
var verticesEqual = /* @stopify flat */ function (v1, v2) {
  if (v1.length !== v2.length) { return false; }
  var v1_str = v1.map(/* @stopify flat */ function (o) { return "x:" + o.x + ",y:" + o.y }).join(","),
    v2_str = v2.map(/* @stopify flat */ function (o) { return "x:" + o.x + ",y:" + o.y }).join(",");
  // v1 == rot(v2) if append(v1,v1) includes v2
  return (v1_str + "," + v1_str).includes(v2_str);
};

// given an array of (x, y) pairs, unzip them into separate arrays
var unzipVertices = /* @stopify flat */ function (vertices) {
  return {
    xs: vertices.map(/* @stopify flat */ function (v) { return v.x }),
    ys: vertices.map(/* @stopify flat */ function (v) { return v.y })
  };
};
// given an array of vertices, find the width of the shape
var findWidth = /* @stopify flat */ function (vertices) {
  var xs = unzipVertices(vertices).xs;
  return Math.max.apply(Math, xs) - Math.min.apply(Math, xs);
}
// given an array of vertices, find the height of the shape
var findHeight = /* @stopify flat */ function (vertices) {
  var ys = unzipVertices(vertices).ys;
  return Math.max.apply(Math, ys) - Math.min.apply(Math, ys);
}
// given a list of vertices and a translationX/Y, shift them
var translateVertices = /* @stopify flat */ function (vertices) {
  var vs = unzipVertices(vertices);
  var translateX = -Math.min.apply(Math, vs.xs);
  var translateY = -Math.min.apply(Math, vs.ys);
  return vertices.map(/* @stopify flat */ function (v) {
    return { x: v.x + translateX, y: v.y + translateY };
  })
}


// Base class for all images.
var BaseImage = /* @stopify flat */ function () { this.$brand = "image"; };

BaseImage.prototype.updatePinhole = /* @stopify flat */ function (x, y) {
  var aCopy = clone(this);
  aCopy.pinholeX = x;
  aCopy.pinholeY = y;
  return aCopy;
};

BaseImage.prototype.getHeight = /* @stopify flat */ function () {
  return Math.round(this.height);
};

BaseImage.prototype.getWidth = /* @stopify flat */ function () {
  return Math.round(this.width);
};

BaseImage.prototype.getBaseline = /* @stopify flat */ function () {
  return Math.round(this.height);
};

// return the vertex array if it exists, otherwise make one using height and width
BaseImage.prototype.getVertices = /* @stopify flat */ function () {
  if (this.vertices) { return this.vertices; }
  else {
    return [{ x: 0, y: 0 },
    { x: this.width, y: 0 },
    { x: 0, y: this.height },
    { x: this.width, y: this.height }];
  }
};

// render: context fixnum fixnum: -> void
// Render the image, where the upper-left corner of the image is drawn at
// (x, y).
// If the image isn't vertex-based, throw an error
// Otherwise, stroke and fill the vertices.
BaseImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  if (!this.vertices) {
    throw new Error('BaseImage.render is not implemented for this type!');
  }
  ctx.save();
  ctx.beginPath();

  // we care about the stroke because drawing to a canvas is *different* for
  // fill v. stroke! If it's outline, we can draw on the pixel boundaries and
  // stroke within them. If it's stroke, we need to draw _inside_ those 
  // boundaries, adjusting by a half-pixel towards the center.
  var isSolid = this.style.toString().toLowerCase() !== "outline";

  var vertices;
  // pixel-perfect vertices fail on Chrome, and certain versions of FF,
  // so we disable the offset for equality tests and solid images
  if (ctx.isEqualityTest || isSolid) {
    vertices = this.vertices;
  } else {
    // find the midpoint of the xs and ys from vertices
    var midX = findWidth(this.vertices) / 2;
    var midY = findHeight(this.vertices) / 2;

    // compute 0.5px offsets to ensure that we draw on the pixel
    // and not the pixel boundary
    vertices = this.vertices.map(/* @stopify flat */ function (v) {
      return {
        x: v.x + (v.x < midX ? 0.5 : -0.5),
        y: v.y + (v.y < midY ? 0.5 : -0.5)
      };
    });
  }

  ctx.moveTo(x + vertices[0].x, y + vertices[0].y);
  vertices.forEach(/* @stopify flat */ function (v) { ctx.lineTo(x + v.x, y + v.y); });
  ctx.closePath();

  if (isSolid) {
    ctx.fillStyle = colorString(this.color, this.style);
    ctx.fill();
  } else {
    ctx.strokeStyle = colorString(this.color);
    ctx.stroke();
  }
  ctx.restore();
};

// makeCanvas: number number -> canvas
// Constructs a canvas object of a particular width and height.
var makeCanvas = /* @stopify flat */ function (width, height) {
  var canvas = document.createElement("canvas");
  canvas.width = width;
  canvas.height = height;
  canvas.style.width = canvas.width + "px";
  canvas.style.height = canvas.height + "px";
  return canvas;
};

// Images are expected to define a render() method, which is used
// here to draw to the canvas.
BaseImage.prototype.toDomNode = function (params) {
  var that = this;
  var width = that.getWidth();
  var height = that.getHeight();
  var canvas = makeCanvas(width, height);
  var ctx;
  console.log("In toDomNode");
  // KLUDGE: on IE, the canvas rendering functions depend on a
  // context where the canvas is attached to the DOM tree.
  // We initialize an afterAttach hook; the client's responsible
  // for calling this after the dom node is attached to the
  // document.
  var onAfterAttach = function (event) {
    // jQuery(canvas).unbind('afterAttach', onAfterAttach);
    ctx = this.getContext("2d");
    that.render(ctx, 0, 0);
  };
  //jQuery(canvas).bind('afterAttach', onAfterAttach);

  // Canvases lose their drawn content on cloning.  data may help us to preserve it.
  //jQuery(canvas).data('toRender', onAfterAttach);
  // ARIA: use "image" as default text.
  canvas.ariaText = this.ariaText || "image";
  return canvas;
};

BaseImage.prototype.toWrittenString = /* @stopify flat */ function (cache) { return "<image>"; }
BaseImage.prototype.toDisplayedString = /* @stopify flat */ function (cache) { return "<image>"; }


// Best-Guess equivalence for images. If they're vertex-based we're in luck,
// otherwise we go pixel-by-pixel. It's up to exotic image types to provide
// more efficient ways of comparing one another
BaseImage.prototype.equals = /* @stopify flat */ function (other) {
  if (this.getWidth() !== other.getWidth() ||
    this.getHeight() !== other.getHeight()) { return false; }
  // if they're both vertex-based images, all we need to compare are
  // their styles, vertices and color
  if (this.vertices && other.vertices) {
    return (this.style === other.style &&
      verticesEqual(this.vertices, other.vertices) &&
      equals(this.color, other.color));
  }
  // if it's something more sophisticated, render both images to canvases
  // First check canvas dimensions, then go pixel-by-pixel
  var c1 = this.toDomNode(), c2 = other.toDomNode();
  c1.style.visibility = c2.style.visibility = "hidden";
  if (c1.width !== c2.width || c1.height !== c2.height) { return false; }
  try {
    var ctx1 = c1.getContext('2d'), ctx2 = c2.getContext('2d');
    ctx1.isEqualityTest = true;
    ctx2.isEqualityTest = true;
    this.render(ctx1, 0, 0); other.render(ctx2, 0, 0);
    // create temporary canvases
    var slice1 = document.createElement('canvas').getContext('2d'),
      slice2 = document.createElement('canvas').getContext('2d');
    var tileW = Math.min(10000, c1.width); // use only the largest tiles we need for these images
    var tileH = Math.min(10000, c1.height);
    for (var y = 0; y < c1.height; y += tileH) {
      for (var x = 0; x < c1.width; x += tileW) {
        tileW = Math.min(tileW, c1.width - x); // can we use smaller tiles for what's left?
        tileH = Math.min(tileH, c1.height - y);
        slice1.canvas.width = slice2.canvas.width = tileW;
        slice1.canvas.height = slice2.canvas.height = tileH;
        console.log('processing chunk from (' + x + ',' + y + ') to (' + (x + tileW) + ',' + (y + tileH) + ')');
        slice1.clearRect(0, 0, tileW, tileH);
        slice1.drawImage(c1, x, y, tileW, tileH, 0, 0, tileW, tileH);
        slice2.clearRect(0, 0, tileW, tileH);
        slice2.drawImage(c2, x, y, tileW, tileH, 0, 0, tileW, tileH);
        var d1 = slice1.canvas.toDataURL(),
          d2 = slice2.canvas.toDataURL(),
          h1 = md5(d1), h2 = md5(d2);
        if (h1 !== h2) return false;
      }
    }
    // Slow-path can fail with CORS or image-loading problems
  } catch (e) {
    console.log('Couldn\'t compare images:', e);
    return false;
  }
  // if, after all this, we're still good...then they're equal!
  return true;
};

/* Calculates the difference between two images, and returns it
       as a Pyret Either<String, Number>
       The difference is calculated from the formula at
       http://stackoverflow.com/questions/9136524/are-there-any-javascript-libs-to-pixel-compare-images-using-html5-canvas-or-any
       values in the low double digits indicate pretty similar images, in the
       low hundreds something is clearly off.
    */
BaseImage.prototype.difference = function (other) {
  if (Math.floor(this.width) !== Math.floor(other.getWidth()) ||
    Math.floor(this.height) !== Math.floor(other.getHeight())) {
    return RUNTIME.ffi.makeLeft("different-size([" + this.width + ", " + this.height + "], [" +
      other.getWidth() + ", " + other.getHeight() + "])");
  }

  // http://stackoverflow.com/questions/9136524/are-there-any-javascript-libs-to-pixel-compare-images-using-html5-canvas-or-any
  function rmsDiff(data1, data2) {
    var squares = 0;
    for (var i = 0; i < data1.length; i++) {
      squares += (data1[i] - data2[i]) * (data1[i] - data2[i]);
    }
    var rms = Math.sqrt(squares / data1.length);
    return rms;
  }

  // if it's something more sophisticated, render both images to canvases
  // First check canvas dimensions, then go pixel-by-pixel
  var c1 = this.toDomNode(), c2 = other.toDomNode();
  c1.style.visibility = c2.style.visibility = "hidden";
  var w1 = Math.floor(c1.width),
    h1 = Math.floor(c1.height),
    w2 = Math.floor(c2.width),
    h2 = Math.floor(c2.height);
  if (w1 !== w2 || h1 !== h2) {
    return RUNTIME.makeLeft("different-size-dom([" + c1.width + ", " + c1.height + "], [" +
      c2.width + ", " + c2.height + "])");
  }
  var ctx1 = c1.getContext('2d'), ctx2 = c2.getContext('2d');
  this.render(ctx1, 0, 0);
  other.render(ctx2, 0, 0);
  try {
    var data1 = ctx1.getImageData(0, 0, w1, h1),
      data2 = ctx2.getImageData(0, 0, w2, h2);
    var pixels1 = data1.data,
      pixels2 = data2.data;
    return RUNTIME.ffi.makeRight(rmsDiff(pixels1, pixels2));
  } catch (e) {
    // if we violate CORS, just bail
    return RUNTIME.ffi.makeLeft("exception: " + String(e));
  }
};

var isMode = /* @stopify flat */ function (x) {
  return ((typeof (x) === 'string' || x instanceof String) &&
    (x.toString().toLowerCase() == "solid" ||
      x.toString().toLowerCase() == "outline")) ||
    ((jsnums.isReal(x)) &&
      (jsnums.greaterThanOrEqual(x, 0, RUNTIME.NumberErrbacks) &&
        jsnums.lessThanOrEqual(x, 1, RUNTIME.NumberErrbacks)));
};

var isPlaceX = /* @stopify flat */ function (x) {
  return ((typeof (x) === 'string' || x instanceof String) &&
    (x.toString().toLowerCase() === "left" ||
      x.toString().toLowerCase() === "right" ||
      x.toString().toLowerCase() === "center" ||
      x.toString().toLowerCase() === "middle"));
};

var isPlaceY = /* @stopify flat */ function (x) {
  return ((typeof (x) === 'string' || x instanceof String) &&
    (x.toString().toLowerCase() === "top" ||
      x.toString().toLowerCase() === "bottom" ||
      x.toString().toLowerCase() === "baseline" ||
      x.toString().toLowerCase() === "center" ||
      x.toString().toLowerCase() === "middle"));
};

// isScene: any -> boolean
// Produces true when x is a scene.
var isScene = /* @stopify flat */ function (x) {
  return ((x != undefined) && (x != null) && (x instanceof SceneImage));
};

//////////////////////////////////////////////////////////////////////
// SceneImage: primitive-number primitive-number (listof image) -> Scene
var SceneImage = /* @stopify flat */ function (width, height, children, withBorder) {
  BaseImage.call(this);
  this.width = width;
  this.height = height;
  this.children = children; // arrayof [image, number, number]
  this.withBorder = withBorder;
  this.ariaText = " scene that is " + width + " by " + height + ". children are: ";
  this.ariaText += children.map(function (c, i) {
    return "child " + (i + 1) + ": " + c[0].ariaText + ", positioned at " + c[1] + "," + c[2] + " ";
  }).join(". ");
};
SceneImage.prototype = heir(BaseImage.prototype);

// add: image primitive-number primitive-number -> Scene
SceneImage.prototype.add = /* @stopify flat */ function (anImage, x, y) {
  return new SceneImage(this.width,
    this.height,
    this.children.concat([[anImage,
      x - anImage.getWidth() / 2,
      y - anImage.getHeight() / 2]]),
    this.withBorder);
};

// render: 2d-context primitive-number primitive-number -> void
SceneImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  var childImage, childX, childY;
  // create a clipping region around the boundaries of the Scene
  ctx.save();
  ctx.fillStyle = "rgba(0,0,0,0)";
  ctx.fillRect(x, y, this.width, this.height);
  ctx.restore();
  // save the context, reset the path, and clip to the path around the scene edge
  ctx.save();
  ctx.beginPath();
  ctx.rect(x, y, this.width, this.height);
  ctx.clip();
  // Ask every object to render itself inside the region
  this.children.forEach(function (child) {
    // then, render the child images
    childImage = child[0];
    childX = child[1];
    childY = child[2];
    childImage.render(ctx, childX + x, childY + y);
  });
  // unclip
  ctx.restore();

  if (this.withBorder) {
    ctx.strokeStyle = 'black';
    ctx.strokeRect(x, y, this.width, this.height);
  }
};

SceneImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof SceneImage &&
    this.width == other.width &&
    this.height == other.height &&
    this.children.length == other.children.length &&
    this.children.every(function (child1, i) {
      var child2 = other.children[i];
      return (child1[1] == child2[1] &&
        child1[2] == child2[2] &&
        child1[0].equals(child2[0]));
    }))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// OverlayImage: image image placeX placeY -> image
// Creates an image that overlays img1 on top of the
// other image img2.
var OverlayImage = /* @stopify flat */ function (img1, img2, placeX, placeY) {
  BaseImage.call(this);

  // An overlay image consists of width, height, x1, y1, x2, and
  // y2.  We need to compute these based on the inputs img1,
  // img2, placex, and placey.

  // placeX and placeY may be non-numbers, in which case their values
  // depend on the img1 and img2 geometry.

  var x1, y1, x2, y2;

  if (placeX === "left") {
    x1 = 0;
    x2 = 0;
  } else if (placeX === "right") {
    x1 = Math.max(img1.width, img2.width) - img1.width;
    x2 = Math.max(img1.width, img2.width) - img2.width;
  } else if (placeX === "beside") {
    x1 = 0;
    x2 = img1.width;
  } else if (placeX === "middle" || placeX === "center") {
    x1 = Math.max(img1.width, img2.width) / 2 - img1.width / 2;
    x2 = Math.max(img1.width, img2.width) / 2 - img2.width / 2;
  } else {
    x1 = Math.max(placeX, 0) - placeX;
    x2 = Math.max(placeX, 0);
  }

  if (placeY === "top") {
    y1 = 0;
    y2 = 0;
  } else if (placeY === "bottom") {
    y1 = Math.max(img1.height, img2.height) - img1.height;
    y2 = Math.max(img1.height, img2.height) - img2.height;
  } else if (placeY === "above") {
    y1 = 0;
    y2 = img1.height;
  } else if (placeY === "baseline") {
    y1 = Math.max(img1.getBaseline(), img2.getBaseline()) - img1.getBaseline();
    y2 = Math.max(img1.getBaseline(), img2.getBaseline()) - img2.getBaseline();
  } else if (placeY === "middle" || placeY === "center") {
    y1 = Math.max(img1.height, img2.height) / 2 - img1.height / 2;
    y2 = Math.max(img1.height, img2.height) / 2 - img2.height / 2;
  } else {
    y1 = Math.max(placeY, 0) - placeY;
    y2 = Math.max(placeY, 0);
  }

  // calculate the vertices of this image by translating the vertices of the sub-images
  var i, v1 = img1.getVertices(), v2 = img2.getVertices(), xs = [], ys = [];
  v1 = v1.map(function (v) { return { x: v.x + x1, y: v.y + y1 }; });
  v2 = v2.map(function (v) { return { x: v.x + x2, y: v.y + y2 }; });

  // store the vertices as something private, so this.getVertices() will still return undefined
  this._vertices = v1.concat(v2);

  // store the offsets for rendering
  this.x1 = x1;
  this.y1 = y1;
  this.x2 = x2;
  this.y2 = y2;
  this.img1 = img1;
  this.img2 = img2;
  var positionText;
  if ((["middle", "center"].indexOf(placeX) > -1) && (["middle", "center"].indexOf(placeY) > -1)) {
    positionText = " centered above ";
  } else if (placeX === "left") {
    positionText = " left-aligned ";
  } else if (placeX === "right") {
    positionText = " right-aligned ";
  } else if (placeX === "beside") {
    positionText = " beside ";
  } else if (!isNaN(placeX)) {
    positionText = " shifted left by " + placeX;
  }
  if (placeY === "top") {
    positionText += " top-aligned ";
  } else if (placeY === "bottom") {
    positionText += " bottom-aligned ";
  } else if (placeY === "above") {
    positionText += " above ";
  } else if (!isNaN(placeY)) {
    positionText += " , shifted up by " + placeY;
  }
  this.width = findWidth(this._vertices);
  this.height = findHeight(this._vertices);
  this.ariaText = " an overlay: first image is" + img1.ariaText + positionText + img2.ariaText;
};

OverlayImage.prototype = heir(BaseImage.prototype);

OverlayImage.prototype.getVertices = /* @stopify flat */ function () { return this._vertices; };

OverlayImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.save();
  this.img2.render(ctx, x + this.x2, y + this.y2);
  this.img1.render(ctx, x + this.x1, y + this.y1);
  ctx.restore();
};

OverlayImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof OverlayImage &&
    this.width === other.width &&
    this.height === other.height &&
    this.x1 === other.x1 &&
    this.y1 === other.y1 &&
    this.x2 === other.x2 &&
    this.y2 === other.y2 &&
    imageEquals(this.img1, other.img1) &&
    imageEquals(this.img2, other.img2))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// rotate: angle image -> image
// Rotates image by angle degrees in a counter-clockwise direction.
// TODO: special case for ellipse?
var RotateImage = /* @stopify flat */ function (angle, img) {
  BaseImage.call(this);
  // optimization for trying to rotate a circle
  if ((img instanceof EllipseImage) && (img.width == img.height)) {
    angle = 0;
  }
  var sin = Math.sin(angle * Math.PI / 180);
  var cos = Math.cos(angle * Math.PI / 180);

  // rotate each point as if it were rotated about (0,0)
  var vertices = img.getVertices().map(function (v) {
    return { x: v.x * cos - v.y * sin, y: v.x * sin + v.y * cos };
  });

  // extract the xs and ys separately
  var vs = unzipVertices(vertices);

  // store the vertices as something private, so this.getVertices() will still return undefined
  this._vertices = translateVertices(vertices);
  this.img = img;
  this.width = findWidth(vertices);
  this.height = findHeight(vertices);
  this.angle = Math.round(angle);
  this.translateX = -Math.min.apply(Math, vs.xs);
  this.translateY = -Math.min.apply(Math, vs.ys);
  this.ariaText = "Rotated image, " + angle + " degrees: " + img.ariaText;
};

RotateImage.prototype = heir(BaseImage.prototype);

RotateImage.prototype.getVertices = /* @stopify flat */ function () { return this._vertices; };

// translate the canvas using the calculated values, then draw at the rotated (x,y) offset.
RotateImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.save();
  ctx.translate(x + this.translateX, y + this.translateY);
  ctx.rotate(this.angle * Math.PI / 180);
  this.img.render(ctx, 0, 0);
  ctx.restore();
};

RotateImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof RotateImage &&
    this.width === other.width &&
    this.height === other.height &&
    this.angle === other.angle &&
    this.translateX === other.translateX &&
    this.translateY === other.translateY &&
    imageEquals(this.img, other.img))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// FlipImage: image string -> image
// Flip an image either horizontally or vertically
var FlipImage = /* @stopify flat */ function (img, direction) {
  BaseImage.call(this);
  this.img = img;
  this.width = img.getWidth();
  this.height = img.getHeight();
  this.direction = direction;
  this.ariaText = direction + "ly flipped image: " + img.ariaText;
};

FlipImage.prototype = heir(BaseImage.prototype);

FlipImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  // when flipping an image of dimension M and offset by N across an axis,
  // we need to translate the canvas by M+2N in the opposite direction
  ctx.save();
  if (this.direction === "horizontal") {
    ctx.scale(-1, 1);
    ctx.translate(-(this.width + 2 * x), 0);
    this.img.render(ctx, x, y);
  }
  if (this.direction === "vertical") {
    ctx.scale(1, -1);
    ctx.translate(0, -(this.height + 2 * y));
    this.img.render(ctx, x, y);
  }
  ctx.restore();
};

FlipImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof FlipImage &&
    this.width === other.width &&
    this.height === other.height &&
    this.direction === other.direction &&
    imageEquals(this.img, other.img))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// FrameImage: factor factor image -> image
// Stick a frame around the image
var FrameImage = /* @stopify flat */ function (img) {
  BaseImage.call(this);
  this.img = img;
  this.width = img.width;
  this.height = img.height;
  this.ariaText = " Framed image: " + img.ariaText;
};

FrameImage.prototype = heir(BaseImage.prototype);

// scale the context, and pass it to the image's render function
FrameImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.save();
  this.img.render(ctx, x, y);
  ctx.beginPath();
  ctx.strokeStyle = "black";
  ctx.strokeRect(x, y, this.width, this.height);
  ctx.closePath();
  ctx.restore();
};

FrameImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof FrameImage &&
    BaseImage.prototype.equals.call(this, other))
    || imageEquals(this.img, other.img);
};

//////////////////////////////////////////////////////////////////////
// CropImage: startX startY width height image -> image
// Crop an image
var CropImage = /* @stopify flat */ function (x, y, width, height, img) {
  BaseImage.call(this);
  this.x = x;
  this.y = y;
  this.width = width;
  this.height = height;
  this.img = img;
  this.ariaText = "Cropped image, from " + x + ", " + y + " to " + (x + width) + ", " + (y + height) + ": " + img.ariaText;
};

CropImage.prototype = heir(BaseImage.prototype);

CropImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.save();
  ctx.beginPath();
  ctx.rect(x, y, this.width, this.height);
  ctx.clip();
  ctx.translate(-this.x, -this.y);
  this.img.render(ctx, x, y);
  ctx.restore();
};

CropImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof CropImage &&
    this.width === other.width &&
    this.height === other.height &&
    this.x === other.x &&
    this.y === other.y &&
    imageEquals(this.img, other.img))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// ScaleImage: factor factor image -> image
// Scale an image
var ScaleImage = /* @stopify flat */ function (xFactor, yFactor, img) {
  BaseImage.call(this);
  // grab the img vertices, scale them, and save the result to this_vertices
  this._vertices = img.getVertices().map(function (v) {
    return { x: v.x * xFactor, y: v.y * yFactor };
  });

  this.img = img;
  this.width = img.width * xFactor;
  this.height = img.height * yFactor;
  this.xFactor = xFactor;
  this.yFactor = yFactor;
  this.ariaText = "Scaled Image, " + (xFactor === yFactor ? "by " + xFactor
    : "horizontally by " + xFactor + " and vertically by " + yFactor) + ". " + img.ariaText;
};

ScaleImage.prototype = heir(BaseImage.prototype);

ScaleImage.prototype.getVertices = function () { return this._vertices; };

// scale the context, and pass it to the image's render function
ScaleImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.save();
  ctx.scale(this.xFactor, this.yFactor);
  this.img.render(ctx, x / this.xFactor, y / this.yFactor);
  ctx.restore();
};

ScaleImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof ScaleImage &&
    this.width === other.width &&
    this.height === other.height &&
    this.xFactor === other.xFactor &&
    this.yFactor === other.yFactor &&
    imageEquals(this.img, other.img))
    || BaseImage.prototype.equals.call(this, other);
};


var textContainer, textParent;
//////////////////////////////////////////////////////////////////////
// TextImage: String Number Color String String String String any/c -> Image
var TextImage = /* @stopify flat */ function (str, size, color, face, family, style, weight, underline) {
  BaseImage.call(this);
  this.str = str;
  this.size = size;   // 18
  this.color = color;  // red
  this.face = face;   // Gill Sans
  this.family = family; // 'swiss
  this.style = (style === "slant") ? "oblique" : style;  // Racket's "slant" -> CSS's "oblique"
  this.weight = (weight === "light") ? "lighter" : weight; // Racket's "light" -> CSS's "lighter"
  this.underline = underline;
  // NOTE: we *ignore* font-family, as it causes a number of font bugs due the browser inconsistencies
  // example: "bold italic 20px 'Times', sans-serif".
  // Default weight is "normal", face is "Arial"
  this.font = (this.style + " " + this.weight + " " + this.size + "px " + '"' + this.face + '", ' + this.family);

  // We don't trust ctx.measureText, since (a) it's buggy and (b) it doesn't measure height
  // based off of the amazing work at http://mudcu.be/journal/2011/01/html5-typographic-metrics/#baselineCanvas
  // PENDING CANVAS V5 API: http://www.whatwg.org/specs/web-apps/current-work/#textmetrics

  // build a DOM node with the same styling as the canvas, then measure it
  if (textContainer === undefined) {
    textContainer = document.createElement("div");
    textContainer.style.cssText = "position: absolute; top: 0px; left: 0px; visibility: hidden; white-space: pre;";
    textParent = document.createElement("span");
    textParent.style.display = "inline";
    textContainer.appendChild(textParent);
    document.body.appendChild(textContainer);
  }
  textParent.style.font = this.font;                // use the same font settings as the context
  textParent.textContent = str; // this will blow away any old content

  // getting (more accurate) css equivalent of ctx.measureText()
  var bounds = textParent.getBoundingClientRect(); // make a single blocking call
  this.width = bounds.width;
  this.height = bounds.height;
  this.alphaBaseline = 0;

  this.ariaText = " the string " + str + ", colored " + colorToSpokenString(color, 'solid') + " of size " + size;
};

TextImage.prototype = heir(BaseImage.prototype);

TextImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.save();
  ctx.textAlign = 'left';
  ctx.textBaseline = 'top';
  ctx.font = this.font;

  // if 'outline' is enabled, use strokeText. Otherwise use fillText
  ctx.fillStyle = this.outline ? 'white' : colorString(this.color);
  ctx.fillText(this.str, x, y);
  if (this.outline) {
    ctx.strokeStyle = colorString(this.color);
    ctx.strokeText(this.str, x, y);
  }
  if (this.underline) {
    ctx.beginPath();
    ctx.moveTo(x, y + this.size);
    // we use this.size, as it is more accurate for underlining than this.height
    ctx.lineTo(x + this.width, y + this.size);
    ctx.closePath();
    ctx.strokeStyle = colorString(this.color);
    ctx.stroke();
  }
  ctx.restore();
};

TextImage.prototype.getBaseline = /* @stopify flat */ function () {
  return this.alphaBaseline;
};

TextImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof TextImage &&
    this.str === other.str &&
    this.size === other.size &&
    this.face === other.face &&
    this.family === other.family &&
    this.style === other.style &&
    this.weight === other.weight &&
    this.font === other.font &&
    this.underline === other.underline &&
    equals(this.color, other.color))
    || BaseImage.prototype.equals.call(this, other);
};

/////////////////////////////////////////////////////////////////////
//TriangleImage: Number Number Number Mode Color -> Image
// Draws a triangle with the base = sideC, and the angle between sideC
// and sideB being angleA
// See http://docs.racket-lang.org/teachpack/2htdpimage.html#(def._((lib._2htdp/image..rkt)._triangle))
var TriangleImage = /* @stopify flat */ function (sideC, angleA, sideB, style, color) {
  BaseImage.call(this);
  var thirdX = sideB * Math.cos(angleA * Math.PI / 180);
  var thirdY = sideB * Math.sin(angleA * Math.PI / 180);
  var offsetX = 0 - Math.min(0, thirdX); // angleA could be obtuse

  var vertices = [];
  // if angle < 180 start at the top of the canvas, otherwise start at the bottom
  if (thirdY > 0) {
    vertices.push({ x: offsetX + 0, y: 0 });
    vertices.push({ x: offsetX + sideC, y: 0 });
    vertices.push({ x: offsetX + thirdX, y: thirdY });
  } else {
    vertices.push({ x: offsetX + 0, y: -thirdY });
    vertices.push({ x: offsetX + sideC, y: -thirdY });
    vertices.push({ x: offsetX + thirdX, y: 0 });
  }

  this.width = Math.max(sideC, thirdX) + offsetX;
  this.height = Math.abs(thirdY);
  this.style = style;
  this.color = color;
  this.vertices = vertices;
  this.ariaText = " a" + colorToSpokenString(color, style) + " triangle whose base is of length " + sideC
    + ", with an angle of " + (angleA % 180) + " degrees between it and a side of length " + sideB;
};
TriangleImage.prototype = heir(BaseImage.prototype);

var less = function (lhs, rhs) {
  return (rhs - lhs) > 0.00001;
};

// excess : compute the Euclidean excess
//  Note: If the excess is 0, then C is 90 deg.
//        If the excess is negative, then C is obtuse.
//        If the excess is positive, then C is acuse.
function excess(sideA, sideB, sideC) {
  return sideA * sideA + sideB * sideB - sideC * sideC;
};

var TriangleSSS = /* @stopify flat */ function (sideA, sideB, sideC, style, color) {
  if (less(sideA + sideB, sideC) ||
    less(sideB + sideC, sideA) ||
    less(sideA + sideC, sideB)) {
    throw new Error("The given sides will not form a triangle: "
      + sideA + ", " + sideB + ", " + sideC);
  }
  var angleA = Math.acos(excess(sideB, sideC, sideA)
    / (2 * sideB * sideC)) * (180 / Math.PI);

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

var TriangleASS = /* @stopify flat */ function (angleA, sideB, sideC, style, color) {
  if (less(180, angleA)) {
    throw new Error("The given angle, side and side will not form a triangle: "
      + angleA + ", " + sideB + ", " + sideC);
  }

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

// return c^2 = a^2 + b^2 - 2ab cos(C)
function cosRel(sideA, sideB, angleC) {
  return (sideA * sideA) + (sideB * sideB) - (2 * sideA * sideB * Math.cos(angleC * Math.PI / 180));
}

var TriangleSAS = /* @stopify flat */ function (sideA, angleB, sideC, style, color) {
  var sideB2 = cosRel(sideA, sideC, angleB);
  var sideB = Math.sqrt(sideB2);

  if (sideB2 <= 0) {
    throw new Error("The given side, angle and side will not form a triangle: "
      + sideA + ", " + angleB + ", " + sideC);
  } else {
    if (less(sideA + sideC, sideB) ||
      less(sideB + sideC, sideA) ||
      less(sideA + sideB, sideC)) {
      throw new Error("The given side, angle and side will not form a triangle: "
        + sideA + ", " + angleB + ", " + sideC);
    } else {
      if (less(sideA + sideC, sideB) ||
        less(sideB + sideC, sideA) ||
        less(sideA + sideB, sideC)) {
        throw new Error("The given side, angle and side will not form a triangle: "
          + sideA + ", " + angleB + ", " + sideC);
      }
    }
  }

  var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC))
    * (180 / Math.PI);

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

var TriangleSSA = /* @stopify flat */ function (sideA, sideB, angleC, style, color) {
  if (less(180, angleC)) {
    throw new Error("The given side, side and angle will not form a triangle: "
      + sideA + ", " + sideB + ", " + angleC);
  }
  var sideC2 = cosRel(sideA, sideB, angleC);
  var sideC = Math.sqrt(sideC2);

  if (sideC2 <= 0) {
    throw new Error("The given side, side and angle will not form a triangle: "
      + sideA + ", " + sideB + ", " + angleC);
  } else {
    if (less(sideA + sideB, sideC) ||
      less(sideC + sideB, sideA) ||
      less(sideA + sideC, sideB)) {
      throw new Error("The given side, side and angle will not form a triangle: "
        + sideA + ", " + sideB + ", " + angleC);
    }
  }

  var angleA = Math.acos(excess(sideB, sideC, sideA) / (2 * sideB * sideC))
    * (180 / Math.PI);

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

var TriangleAAS = /* @stopify flat */ function (angleA, angleB, sideC, style, color) {
  var angleC = (180 - angleA - angleB);
  if (less(angleC, 0)) {
    throw new Error("The given angle, angle and side will not form a triangle: "
      + angleA + ", " + angleB + ", " + sideC);
  }
  var hypotenuse = sideC / (Math.sin(angleC * Math.PI / 180))
  var sideB = hypotenuse * Math.sin(angleB * Math.PI / 180);

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

var TriangleASA = /* @stopify flat */ function (angleA, sideB, angleC, style, color) {
  var angleB = 180 - angleA - angleC;
  if (less(angleB, 0)) {
    throw new Error("The given angle, side and angle will not form a triangle: "
      + angleA + ", " + sideB + ", " + angleC);
  }
  var base = (sideB * Math.sin(angleA * Math.PI / 180))
    / (Math.sin(angleB * Math.PI / 180));
  var sideC = (sideB * Math.sin(angleC * Math.PI / 180))
    / (Math.sin(angleB * Math.PI / 180));

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

var TriangleSAA = /* @stopify flat */ function (sideA, angleB, angleC, style, color) {
  var angleA = (180 - angleC - angleB);
  if (less(angleA, 0)) {
    throw new Error("The given side, angle and angle will not form a triangle: "
      + sideA + ", " + angleB + ", " + angleC);
  }
  var hypotenuse = sideA / (Math.sin(angleA * Math.PI / 180));
  var sideC = hypotenuse * Math.sin(angleC * Math.PI / 180);
  var sideB = hypotenuse * Math.sin(angleB * Math.PI / 180);

  return new TriangleImage(sideC, angleA, sideB, style, color);
};

//////////////////////////////////////////////////////////////////////
//Ellipse : Number Number Mode Color -> Image
var EllipseImage = /* @stopify flat */ function (width, height, style, color) {
  BaseImage.call(this);
  this.width = width;
  this.height = height;
  this.style = style;
  this.color = color;
  this.ariaText = " a" + colorToSpokenString(color, style) + ((width === height) ? " circle of radius " + (width / 2)
    : " ellipse of width " + width + " and height " + height);
};

EllipseImage.prototype = heir(BaseImage.prototype);

EllipseImage.prototype.render = /* @stopify flat */ function (ctx, aX, aY) {
  ctx.save();
  ctx.beginPath();

  // if it's a solid ellipse...
  var isSolid = this.style.toString().toLowerCase() !== "outline";
  var adjust = isSolid ? 0 : 0.5;
  // ...account for the 1px border width
  var width = this.width - 2 * adjust, height = this.height - 2 * adjust;
  aX += adjust; aY += adjust;

  // Most of this code is taken from:
  // http://webreflection.blogspot.com/2009/01/ellipse-and-circle-for-canvas-2d.html
  var hB = (width / 2) * 0.5522848,
    vB = (height / 2) * 0.5522848,
    eX = aX + width,
    eY = aY + height,
    mX = aX + width / 2,
    mY = aY + height / 2;
  ctx.moveTo(aX, mY);
  ctx.bezierCurveTo(aX, mY - vB, mX - hB, aY, mX, aY);
  ctx.bezierCurveTo(mX + hB, aY, eX, mY - vB, eX, mY);
  ctx.bezierCurveTo(eX, mY + vB, mX + hB, eY, mX, eY);
  ctx.bezierCurveTo(mX - hB, eY, aX, mY + vB, aX, mY);
  ctx.closePath();
  if (this.style.toString().toLowerCase() === "outline") {
    ctx.strokeStyle = colorString(this.color);
    ctx.stroke();
  } else {
    ctx.fillStyle = colorString(this.color, this.style);
    ctx.fill();
  }

  ctx.restore();
};

EllipseImage.prototype.equals = /* @stopify flat */ function (other) {
  return ((other instanceof EllipseImage) &&
    this.width === other.width &&
    this.height === other.height &&
    this.style === other.style &&
    equals(this.color, other.color))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// RectangleImage: Number Number Mode Color -> Image
var RectangleImage = /* @stopify flat */ function (width, height, style, color) {
  BaseImage.call(this);
  this.width = Math.max(1, width);   // an outline rectangle with no delta X or delta Y
  this.height = Math.max(1, height);  // should still take up one visible pixel
  this.style = style;
  this.color = color;
  this.vertices = [{ x: 0, y: height }, { x: 0, y: 0 }, { x: width, y: 0 }, { x: width, y: height }];
  this.ariaText = " a" + colorToSpokenString(color, style) + ((width === height) ? " square of size " + width
    : " rectangle of width " + width + " and height " + height);
};
RectangleImage.prototype = heir(BaseImage.prototype);

//////////////////////////////////////////////////////////////////////
// RhombusImage: Number Number Mode Color -> Image
var RhombusImage = /* @stopify flat */ function (side, angle, style, color) {
  BaseImage.call(this);
  // sin(angle/2-in-radians) * side = half of base
  // cos(angle/2-in-radians) * side = half of height
  this.width = Math.sin(angle / 2 * Math.PI / 180) * side * 2;
  this.height = Math.abs(Math.cos(angle / 2 * Math.PI / 180)) * side * 2;
  this.style = style;
  this.color = color;
  this.vertices = [{ x: this.width / 2, y: 0 },
  { x: this.width, y: this.height / 2 },
  { x: this.width / 2, y: this.height },
  { x: 0, y: this.height / 2 }];
  this.ariaText = " a" + colorToSpokenString(color, style) + " rhombus of size " + side + " and angle " + angle;
};
RhombusImage.prototype = heir(BaseImage.prototype);

//////////////////////////////////////////////////////////////////////
// Line: Number Number Color Boolean -> Image
var LineImage = /* @stopify flat */ function (x, y, color) {
  BaseImage.call(this);
  var vertices;
  if (x >= 0) {
    if (y >= 0) { vertices = [{ x: 0, y: 0 }, { x: x, y: y }]; }
    else { vertices = [{ x: 0, y: -y }, { x: x, y: 0 }]; }
  } else {
    if (y >= 0) { vertices = [{ x: -x, y: 0 }, { x: 0, y: y }]; }
    else { vertices = [{ x: -x, y: -y }, { x: 0, y: 0 }]; }
  }

  this.width = Math.max(1, Math.abs(x)); // a line with no delta X should still take up one visible pixel
  this.height = Math.max(1, Math.abs(y)); // a line with no delta Y should still take up one visible pixel
  this.style = "outline"; // all vertex-based images must have a style
  this.color = color;
  this.vertices = vertices;
  this.ariaText = " a" + colorToSpokenString(color, 'solid') + " line of width " + x + " and height " + y;
};
LineImage.prototype = heir(BaseImage.prototype);

//////////////////////////////////////////////////////////////////////
// StarImage: fixnum fixnum fixnum color -> image
// Most of this code here adapted from the Canvas tutorial at:
// http://developer.apple.com/safari/articles/makinggraphicswithcanvas.html
var StarImage = /* @stopify flat */ function (points, outer, inner, style, color) {
  BaseImage.call(this);
  var maxRadius = Math.max(inner, outer);
  var vertices = [];

  var oneDegreeAsRadian = Math.PI / 180;
  for (var pt = 0; pt < (points * 2) + 1; pt++) {
    var rads = ((360 / (2 * points)) * pt) * oneDegreeAsRadian - 0.5;
    var whichRadius = (pt % 2 === 1) ? outer : inner;
    vertices.push({
      x: maxRadius + (Math.sin(rads) * whichRadius),
      y: maxRadius + (Math.cos(rads) * whichRadius)
    });
  }
  // calculate width and height of the bounding box
  this.width = findWidth(vertices);
  this.height = findHeight(vertices);
  this.style = style;
  this.color = color;
  this.vertices = translateVertices(vertices);
  this.ariaText = " a" + colorToSpokenString(color, style) + ", " + points +
    "pointed star with inner radius " + inner + " and outer radius " + outer;
};
StarImage.prototype = heir(BaseImage.prototype);

//////////////////////////////////////////////////////////////////////
// PolygonImage: Number Count Step Mode Color -> Image
//
// See http://www.algebra.com/algebra/homework/Polygons/Inscribed-and-circumscribed-polygons.lesson
// the polygon is inscribed in a circle, whose radius is length/2sin(pi/count)
// another circle is inscribed in the polygon, whose radius is length/2tan(pi/count)
// rotate a 3/4 quarter turn plus half the angle length to keep bottom base level
var PolygonImage = /* @stopify flat */ function (length, count, step, style, color) {
  BaseImage.call(this);
  this.outerRadius = Math.round(length / (2 * Math.sin(Math.PI / count)));
  this.innerRadius = Math.round(length / (2 * Math.tan(Math.PI / count)));
  var adjust = (3 * Math.PI / 2) + Math.PI / count;

  // rotate around outer circle, storing x and y coordinates
  var radians = 0, vertices = [];
  for (var i = 0; i < count; i++) {
    radians = radians + (step * 2 * Math.PI / count);
    vertices.push({
      x: Math.round(this.outerRadius * Math.cos(radians - adjust)),
      y: Math.round(this.outerRadius * Math.sin(radians - adjust))
    });
  }

  this.width = findWidth(vertices);
  this.height = findHeight(vertices);
  this.style = style;
  this.color = color;
  this.vertices = translateVertices(vertices);
  this.ariaText = " a" + colorToSpokenString(color, style) + ", " + count
    + " sided polygon with each side of length " + length;
};
PolygonImage.prototype = heir(BaseImage.prototype);

var colorAtPosition = /* @stopify flat */ function (img, x, y) {
  var width = img.getWidth(),
    height = img.getHeight(),
    canvas = makeCanvas(width, height),
    ctx = canvas.getContext("2d"),
    r, g, b, a;

  if (x >= width) {
    throw new Error("color-at-position: The given x coordinate, " + x
      + ", must be between 0 (inclusive) and the image width (exclusive), which is " + img.getWidth());
  }
  if (y >= height) {
    throw new Error("color-at-position: The given y coordinate, " + y
      + ", must be between 0 (inclusive) and the image height (exclusive), which is " + img.getHeight());
  }

  img.render(ctx, 0, 0);
  imageData = ctx.getImageData(0, 0, width, height);
  data = imageData.data,
    index = (y * width + x) * 4;

  r = data[index]
  g = data[index + 1];
  b = data[index + 2];
  a = data[index + 3] / 255;

  return makeColor(r, g, b, a);
};

var imageToColorList = /* @stopify flat */ function (img) {
  var width = img.getWidth(),
    height = img.getHeight(),
    canvas = makeCanvas(width, height),
    ctx = canvas.getContext("2d"),
    imageData,
    data,
    i,
    r, g, b, a;
  img.render(ctx, 0, 0);
  imageData = ctx.getImageData(0, 0, width, height);
  data = imageData.data;
  var colors = [];
  for (i = 0; i < data.length; i += 4) {
    r = data[i];
    g = data[i + 1];
    b = data[i + 2];
    a = data[i + 3] / 255;
    colors.push(makeColor(r, g, b, a));
  }
  return colors;
};

var colorListToImage = /* @stopify flat */ function (listOfColors,
  width,
  height,
  pinholeX,
  pinholeY) {
  // make list of color names to list of colors
  var lOfC = [];
  if (typeof listOfColors[0] === "string") {
    for (let i = 0; i < listOfColors.length; i++) {
      lOfC.push(colorDb.get(String(listOfColors[i])));
    }
  } else if (isColor(listOfColors[0])) {
    lOfC = listOfColors;
  } else {
    throw new Error("List is not made of Colors or name of colors");
  }
  var canvas = makeCanvas(jsnums.toFixnum(width),
    jsnums.toFixnum(height)),
    ctx = canvas.getContext("2d"),
    imageData = ctx.createImageData(jsnums.toFixnum(width),
      jsnums.toFixnum(height)),
    aColor,
    data = imageData.data,
    jsLOC = lOfC;
  for (var i = 0; i < jsLOC.length * 4; i += 4) {
    aColor = jsLOC[i / 4];
    // NOTE(ben): Flooring colors here to make this a proper RGBA image
    data[i] = Math.floor(colorRed(aColor));
    data[i + 1] = Math.floor(colorGreen(aColor));
    data[i + 2] = Math.floor(colorBlue(aColor));
    data[i + 3] = colorAlpha(aColor) * 255;
  }

  return new ImageDataImage(imageData);
};

//////////////////////////////////////////////////////////////////////
// ImageDataImage: imageData -> image
// Given an array of pixel data, create an image
var ImageDataImage = /* @stopify flat */ function (imageData) {
  BaseImage.call(this);
  this.imageData = imageData;
  this.width = imageData.width;
  this.height = imageData.height;
};

ImageDataImage.prototype = heir(BaseImage.prototype);

ImageDataImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  ctx.putImageData(this.imageData, x, y);
};

var PutImage = /* @stopify flat */ function (img, x, y, bg) {
  if (isScene(bg)) {
    return bg.add(img, x, bg.getHeight() - y);
  } else {
    var newScene = new ScaleImage(bg.getWidth(), bg.getHeight(), [], false);
    newScene = newScene.add(bg, bg.getWidth() / 2, bg.getHeight() / 2);
    newScene = newScene.add(img, x, bg.getHeight() - y);
    return newScene;
  }
};

var PlaceImage = /* @stopify flat */ function (img, x, y, bg) {
  if (isScene(bg)) {
    return bg.add(img, x, y);
  } else {
    var newScene = new ScaleImage(bg.getWidth(), bg.getHeight(), [], false);
    newScene = newScene.add(bg, bg.getWidth() / 2, bg.getHeight() / 2);
    newScene = newScene.add(img, x, y);
    return newScene;
  }
};

var PlaceImageAlign = /* @stopify flat */ function (img, x, y, placeX, placeY, bg) {
  if (placeX == "left") { x = x + img.getWidth() / 2; }
  else if (placeX == "right") { x = x - img.getWidth() / 2; }
  if (placeY == "top") { y = y + img.getHeight() / 2; }
  else if (placeY == "bottom") { y = y - img.getHeight() / 2; }

  if (isScene(bg)) {
    return bg.add(img, x, y);
  } else {
    var newScene = new ScaleImage(bg.getWidth(), bg.getHeight(), [], false);
    newScene = newScene.add(bg, bg.getWidth() / 2, bg.getHeight() / 2);
    newScene = newScene.add(img, x, y);
    return newScene;
  }
};

var SceneLineImage = /* @stopify flat */ function (img, x1, y1, x2, y2, color) {
  var line = new LineImage(x2 - x1, y2 - y1, color);
  var newScene = new SceneImage(img.getWidth(), img.getHeight(), [], true);
  newScene = newScene.add(img, img.getWidth()/2, img.getHeight()/2);
  return newScene.add(line, line.getWidth()/2+Math.min(x1, x2),
    line.getHeight()/2+Math.min(y1, y2));
};

var ImageUrlImage = function (url) {
  return RUNTIME.pauseStack(function (restarter) {
    console.log("Before rawImage = Image");
    var rawImage = new Image();
    console.log("After rawImage = Image: ", rawImage);
    debugger;
    /*if (RUNTIME.hasParam("imgUrlProxy")) {
      url = RUNTIME.getParam("imgUrlProxy")(url);
    }*/
    rawImage.onload = function () {
      restarter.resume(new FileImage(String(url), rawImage));
    };
    rawImage.onerror = function (e) {
      restarter.error(new Error("unable to load " + url + ": " + e.message));
    };
    rawImage.src = String(url);
  });
};

//////////////////////////////////////////////////////////////////////
// FileImage: string node -> Image
var FileImage = /* @stopify flat */ function (src, rawImage) {
  BaseImage.call(this);
  var self = this;
  this.src = src;
  this.isLoaded = false;
  this.ariaText = " image file from " + decodeURIComponent(src).slice(16);

  // animationHack: see installHackToSupportAnimatedGifs() for details.
  this.animationHackImg = undefined;

  if (rawImage && rawImage.complete) {
    this.img = rawImage;
    this.isLoaded = true;
    self.width = self.img.width;
    self.height = self.img.height;
  } else {
    // fixme: we may want to do something blocking here for
    // onload, since we don't know at this time what the file size
    // should be, nor will drawImage do the right thing until the
    // file is loaded.
    this.img = new Image();
    this.img.onload = function () {
      self.isLoaded = true;
      self.width = self.img.width;
      self.height = self.img.height;
    };
    this.img.onerror = function (e) {
      self.img.onerror = "";
      self.img.src = "http://www.wescheme.org/images/broken.png";
    }
    this.img.src = src;
  }
}
FileImage.prototype = heir(BaseImage.prototype);

var imageCache = {};
FileImage.makeInstance = /* @stopify flat */ function (path, rawImage) {
  if (!(path in imageCache)) {
    imageCache[path] = new FileImage(path, rawImage);
  }
  return imageCache[path];
};

FileImage.installInstance = /* @stopify flat */ function (path, rawImage) {
  imageCache[path] = new FileImage(path, rawImage);
};

FileImage.installBrokenImage = /* @stopify flat */ function (path) {
  imageCache[path] = new TextImage("Unable to load " + path, 10, colorDb.get("red"),
    "normal", "Optimer", "", "", false);
};

FileImage.prototype.render = /* @stopify flat */ function (ctx, x, y) {
  this.installHackToSupportAnimatedGifs();
  ctx.drawImage(this.animationHackImg, x, y);
};

// The following is a hack that we use to allow animated gifs to show
// as animating on the canvas.
FileImage.prototype.installHackToSupportAnimatedGifs = /* @stopify flat */ function () {
  if (this.animationHackImg) { return; }
  this.animationHackImg = this.img.cloneNode(true);
  document.body.appendChild(this.animationHackImg);
  this.animationHackImg.style.position = 'absolute';
  this.animationHackImg.style.top = '-50000px';
};

FileImage.prototype.getWidth = /* @stopify flat */ function () {
  return Math.round(this.img.width);
};

FileImage.prototype.getHeight = /* @stopify flat */ function () {
  return Math.round(this.img.height);
};

FileImage.prototype.equals = /* @stopify flat */ function (other) {
  return (other instanceof FileImage) && this.src === other.src
    || BaseImage.prototype.equals.call(this, other);
};

var DoesPauseAndKontinue = function (mode) {
  return RUNTIME.pauseStack(function (restarter) {
    if (mode === "error") {
      restarter.error(new Error("Error case of DoesPauseAndKontinue"));
    } else {
      restarter.resume(new EllipseImage(30, 30, "solid", convertColor("green")));
    }
  });
}


return module.exports = {
  triangle: /* @stopify flat */ function (size, style, color) {
    return new TriangleImage(size, 360 - 60, size, style, convertColor(color));
  },
  "right-triangle": /* @stopify flat */ function (side1, side2, style, color) {
    return new TriangleImage(side1, 360 - 90, side2, style, convertColor(color));
  },
  "isosceles-triangle": /* @stopify flat */ function (side, angle, style, color) {
    return new TriangleImage((2 * side * Math.sin((angle * Math.PI / 180) / 2)),
      360 - ((180 - angle) / 2), side, style, convertColor(color));
  },
  "triangle-sss": /* @stopify flat */ function (sideA, sideB, sideC, style, color) {
    return TriangleSSS(sideA, sideB, sideC, style, convertColor(color));
  },
  "triangle-ass": /* @stopify flat */ function (angleA, sideB, sideC, style, color) {
    return TriangleASS(angleA, sideB, sideC, style, convertColor(color));
  },
  "triangle-sas": /* @stopify flat */ function (sideA, angleB, sideC, style, color) {
    return TriangleSAS(sideA, angleB, sideC, style, convertColor(color));
  },
  "triangle-ssa": /* @stopify flat */ function (sideA, sideB, angleC, style, color) {
    return TriangleSSA(sideA, sideB, angleC, style, convertColor(color));
  },
  "triangle-aas": /* @stopify flat */ function (angleA, angleB, sideC, style, color) {
    return TriangleAAS(angleA, angleB, sideC, style, convertColor(color));
  },
  "triangle-asa": /* @stopify flat */ function (angleA, sideB, angleC, style, color) {
    return TriangleASA(angleA, sideB, angleC, style, convertColor(color));
  },
  "triangle-saa": /* @stopify flat */ function (sideA, angleB, angleC, style, color) {
    return TriangleSAA(sideA, angleB, angleC, style, convertColor(color));
  },
  ellipse: /* @stopify flat */ function (width, height, style, color) {
    return new EllipseImage(width, height, style, convertColor(color));
  },
  circle: /* @stopify flat */ function (radius, style, color) {
    return new EllipseImage(2 * radius, 2 * radius, style, convertColor(color));
  },
  rectangle: /* @stopify flat */ function (width, height, style, color) {
    return new RectangleImage(width, height, style, convertColor(color));
  },
  square: /* @stopify flat */ function (length, style, color) {
    return new RectangleImage(length, length, style, convertColor(color));
  },
  rhombus: /* @stopify flat */ function (side, angle, style, color) {
    return new RhombusImage(side, angle, style, convertColor(color));
  },
  line: /* @stopify flat */ function (x, y, color) {
    return new LineImage(x, y, convertColor(color));
  },
  "add-line": /* @stopify flat */ function (img, x1, y1, x2, y2, color) {
    return new OverlayImage(new LineImage((x2 - x1), (y2 - y1), convertColor(color)), img, Math.min(x1, x2), Math.min(y1, y2));
  },
  star: /* @stopify flat */ function (side, style, color) {
    return new PolygonImage(side, 5, 2, style, convertColor(color));
  },
  "radial-star": /* @stopify flat */ function (points, outer, inner, style, color) {
    return new StarImage(points, inner, outer, style, convertColor(color));
  },
  "star-sized": /* @stopify flat */ function (points, outer, inner, style, color) {
    return new StarImage(points, inner, outer, style, convertColor(color));
  },
  "star-polygon": /* @stopify flat */ function (length, count, step, style, color) {
    return new PolygonImage(length, count, step, style, convertColor(color));
  },
  "regular-polygon": /* @stopify flat */ function (length, count, style, color) {
    return new PolygonImage(length, count, 1, style, convertColor(color));
  },
  overlay: /* @stopify flat */ function (img1, img2) {
    return new OverlayImage(img1, img2, "center", "center");
  },
  "overlay-align": /* @stopify flat */ function (X, Y, img1, img2) {
    return new OverlayImage(img1, img2, X, Y);
  },
  "overlay-xy": /* @stopify flat */ function (img1, dx, dy, img2) {
    return new OverlayImage(img1, img2, dx, dy);
  },
  underlay: /* @stopify flat */ function (img1, img2) {
    return new OverlayImage(img2, img1, "center", "center");
  },
  "underlay-align": /* @stopify flat */ function (X, Y, img1, img2) {
    return new OverlayImage(img2, img1, X, Y);
  },
  "underlay-xy": /* @stopify flat */ function (img1, dx, dy, img2) {
    return new OverlayImage(img2, img1, -dx, -dy);
  },
  beside: /* @stopify flat */ function (img1, img2) {
    return new OverlayImage(img1, img2, "beside", "middle");
  },
  "beside-align": /* @stopify flat */ function (Y, img1, img2) {
    return new OverlayImage(img1, img2, "beside", Y);
  },
  above: /* @stopify flat */ function (img1, img2) {
    return new OverlayImage(img1, img2, "middle", "above");
  },
  "above-align": /* @stopify flat */ function (X, img1, img2) {
    return new OverlayImage(img1, img2, X, "above");
  },
  rotate: /* @stopify flat */ function (angle, img) {
    return new RotateImage(angle, img);
  },
  text: /* @stopify flat */ function (str, size, color) {
    return new TextImage(str, size, convertColor(color), "normal", "Optimizer", "normal", "normal", false);
  },
  "text-font": /* @stopify flat */ function (str, size, color, face, family, style, weight, underline) {
    return new TextImage(str, size, convertColor(color), face, family, style, weight, underline);
  },
  "flip-horizontal": /* @stopify flat */  function (img) {
    return new FlipImage(img, "horizontal");
  },
  "flip-vertical": /* @stopify flat */  function (img) {
    return new FlipImage(img, "vertical");
  },
  frame: /* @stopify flat */  function (img) {
    return new FrameImage(img);
  },
  crop: /* @stopify flat */ function (x, y, width, height, img) {
    return new CropImage(x, y, width, height, img);
  },
  scale: /* @stopify flat */ function (factor, img) {
    return new ScaleImage(factor, factor, img);
  },
  "scale-xy": /* @stopify flat */ function (xFactor, yFactor, img) {
    return new ScaleImage(xFactor, yFactor, img);
  },
  "empty-scene": /* @stopify flat */ function (width, height) {
    return new SceneImage(width, height, [], true);
  },
  "empty-image": new SceneImage(0, 0, [], true),
  "is-image": /* @stopify flat */ function (img) {
    return isImage(img);
  },
  "image-width": /* @stopify flat */ function (img) {
    return img.getWidth();
  },
  "image-height": /* @stopify flat */ function (img) {
    return img.getHeight();
  },
  "image-baseline": /* @stopify flat */ function (img) {
    return img.getBaseline();
  },
  "name-to-color": /* @stopify flat */ function (name) {
    return colorDb.get(String(name)) || false;
  },
  "is-image-color": /* @stopify flat */ function (c) {
    return isColorOrColorString(c);
  },
  "images-equal": /* @stopify flat */ function (img1, img2) {
    return imageEquals(img1, img2);
  },
  "images-difference": /* @stopify flat */ function (img1, img2) {
    return imageDifference(img1, img2);
  },
  "is-side-count": /* @stopify flat */ function (sth) {
    return isSideCount(sth);
  },
  "is-step-count": /* @stopify flat */ function (num) {
    return isStepCount(num);
  },
  "is-mode": /* @stopify flat */ function (x) {
    return isMode(x);
  },
  "is-angle": /* @stopify flat */ function (x) {
    return isAngle(x);
  },
  "is-x-place": /* @stopify flat */ function (x) {
    return isPlaceX(x);
  },
  "is-y-place": /* @stopify flat */ function (x) {
    return isPlaceY(x);
  },
  "color-at-position": /* @stopify flat */ function (img, x, y) {
    return colorAtPosition(img, x, y);
  },
  "image-to-color-list": /* @stopify flat */ function (img) {
    return imageToColorList(img);
  },
  "color-list-to-image": /* @stopify flat */ function (lOfC, width, height, pinX, pinY) {
    return colorListToImage(lOfC, width, height, pinX, pinY);
  },
  "color-list-to-bitmap": /* @stopify flat */ function (lOfC, width, height) {
    return colorListToImage(lOfC, width, height, 0, 0);
  },
  "put-image": /* @stopify flat */ function (image, x, y, background) {
    return PutImage(image, x, y, background);
  },
  "place-image": /* @stopify flat */ function (image, x, y, background) {
    return PlaceImage(image, x, y, background);
  },
  "place-image-align": /* @stopify flat */ function (image, x, y, placeX, placeY, background) {
    return PlaceImageAlign(image, x, y, placeX, placeY, background);
  },
  "scene-line": /* @stopify flat */ function (img, x1, y1, x2, y2, color) {
    return SceneLineImage(img, x1, y1, x2, y2, convertColor(color));
  },
  "image-url": /* @stopify flat */ function (url) {
    return ImageUrlImage(url);
  },
  "bitmap-url": /* @stopify flat */ function (url) {
    return ImageUrlImage(url);
  }
};
