let NUMBER = require("./js-numbers.js");
let OPTION = require("./option.arr.js");

// NOTE(alex): removed from runtime.ts to break cyclic import between runtime.ts and option module
function stringToNumber(s: string): any {
  var result = NUMBER['fromString'](s);
  if (result === false) {
    return OPTION['none'];
  } else {
    return OPTION['some'](result);
  }
}


module.exports = {
  'length': function( str ) {
    return str.length;
  },
  'string-to-lower': function( str ) {
    return str.toLowerCase();
  },
  'concat': function( strA, strB ) {
    return strA.concat( strB );
  },
  'substring': function( str, start, end ) {
    return str.substring( start, end );
  },
  'charAt': function( str, index ) {
    return str.charAt( index );
  },
  'split': function( str, separator ) {
    return str.split( separator );
  },
  'split-pattern': function( str, pattern ) {
    return str.split( RegExp( pattern ) );
  },
  'match': function( str, pattern ) {
    return str.match( RegExp( pattern ) );
  },
  "string-to-number": stringToNumber,
};
