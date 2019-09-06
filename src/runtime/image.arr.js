
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
    ((RUNTIME.isString(thing) &&
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

//////////////////////////////////////////////////////////////////////
//Ellipse : Number Number Mode Color -> Image
var EllipseImage = function (width, height, style, color) {
  BaseImage.call(this);
  this.width = width;
  this.height = height;
  this.style = style;
  this.color = color;
  this.ariaText = " a" + colorToSpokenString(color, style) + ((width === height) ? " circle of radius " + (width / 2)
    : " ellipse of width " + width + " and height " + height);
};

EllipseImage.prototype = heir(BaseImage.prototype);

EllipseImage.prototype.render = function (ctx, aX, aY) {
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

EllipseImage.prototype.equals = function (other) {
  return ((other instanceof EllipseImage) &&
    this.width === other.width &&
    this.height === other.height &&
    this.style === other.style &&
    equals(this.color, other.color))
    || BaseImage.prototype.equals.call(this, other);
};

//////////////////////////////////////////////////////////////////////
// RectangleImage: Number Number Mode Color -> Image
var RectangleImage = function (width, height, style, color) {
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
var RhombusImage = function (side, angle, style, color) {
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


return module.exports = {
  triangle: /* @stopify flat */ function (size, style, color) {
    return new TriangleImage(size, 360 - 60, size, style, convertColor(color));
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
  square: /* @stopify flat */ function(length, style, color) {
    return new RectangleImage(length, length, style, convertColor(color));
  },
  rhombus: /* @stopify flat */ function(side, angle, style, color) {
    return new RhombusImage(side, angle, style, convertColor(color));
  }
};
