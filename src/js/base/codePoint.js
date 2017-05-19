"use strict";
define([], function() {

  /* NOTE(joe): Polyfills for Unicode, from
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/fromCodePoint
   * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/codePointAt
   *
   * *Not* adding them as methods on String because that breaks sandbox guarantees
   * in things like Caja, so just make them functions available in this scope
   */
  /*! http://mths.be/codepointat v0.1.0 by @mathias */
  var codePointAt = function(string, position) {
    var size = string.length;
    // `ToInteger`
    var index = position ? Number(position) : 0;
    if (index != index) { // better `isNaN`
      index = 0;
    }
    // Account for out-of-bounds indices:
    if (index < 0 || index >= size) {
      return undefined;
    }
    // Get the first code unit
    var first = string.charCodeAt(index);
    var second;
    if ( // check if it is the start of a surrogate pair
      first >= 0xD800 && first <= 0xDBFF && // high surrogate
      size > index + 1 // there is a next code unit
    ) {
      second = string.charCodeAt(index + 1);
      if (second >= 0xDC00 && second <= 0xDFFF) { // low surrogate
        // http://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
        return (first - 0xD800) * 0x400 + second - 0xDC00 + 0x10000;
      }
    }
    return first;
  };

  /*! http://mths.be/fromcodepoint v0.1.0 by @mathias */
  var stringFromCharCode = String.fromCharCode;
  var floor = Math.floor;
  var fromCodePoint = function(codePoints) {
    var MAX_SIZE = 0x4000;
    var codeUnits = [];
    var highSurrogate;
    var lowSurrogate;
    var index = -1;
    var length = arguments.length;
    if (!length) {
      return '';
    }
    var result = '';
    while (++index < length) {
      var codePoint = Number(arguments[index]);
      if (
        !isFinite(codePoint) || // `NaN`, `+Infinity`, or `-Infinity`
        codePoint < 0 || // not a valid Unicode code point
        codePoint > 0x10FFFF || // not a valid Unicode code point
        floor(codePoint) != codePoint // not an integer
      ) {
        throw RangeError('Invalid code point: ' + codePoint);
      }
      if (codePoint <= 0xFFFF) { // BMP code point
        codeUnits.push(codePoint);
      } else { // Astral code point; split in surrogate halves
        // http://mathiasbynens.be/notes/javascript-encoding#surrogate-formulae
        codePoint -= 0x10000;
        highSurrogate = (codePoint >> 10) + 0xD800;
        lowSurrogate = (codePoint % 0x400) + 0xDC00;
        codeUnits.push(highSurrogate, lowSurrogate);
      }
      if (index + 1 == length || codeUnits.length > MAX_SIZE) {
        result += stringFromCharCode.apply(null, codeUnits);
        codeUnits.length = 0;
      }
    }
    return result;
  };

  return {
    codePointAt: codePointAt,
    fromCodePoint: fromCodePoint
  };
});
