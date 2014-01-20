var jsnums = require('./js-numbers/src/js-numbers.js');
define(["./js-numbers/src/js-numbers"], function(jsnums) {

  function getBaseStringDict(rt) {
      //Grab the stuff we need from the runtime
      var makeMethod = rt['makeMethod'];
      var makeBoolean = rt['makeBoolean'];
      var makeNothing = rt['makeNothing'];
      var makeNumber = rt['makeNumber'];
      var makeString = rt['makeString'];

      var checkIf = rt['checkIf'];
      var makeNumberBig = rt['makeNumberBig'];
      var isNumber = rt['isNumber'];
      var isString = rt['isString'];
      var makeMessageException = rt['makeMessageException'];

      //Make the dictionary
      return {
          /**@type {PMethod}*/
          '_plus' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PString}
          */
          method(left, right) {
              checkIf(left, isString);
              checkIf(right, isString);

              return makeString(left.s.concat(right.s));
          },

          /**@type {PMethod}*/
          'append' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PString}
          */
          method(left, right) {
              checkIf(left, isString);
              checkIf(right, isString);

              return makeString(left.s.concat(right.s));
          },

          /**@type {PMethod}*/
          'contains' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PBoolean}
          */
          method(left, right) {
              checkIf(left, isString);
              checkIf(right, isString);

              return makeBoolean(left.s.indexOf(right.s) !== -1);
          },

          /**@type {PMethod}*/
          'char-at' : 
          /**
            @param {!PString} str
            @param {!PNumber} n
            @return {!PString}
          */
          method(str, n) {
              checkIf(str, isString);
              checkIf(n, isNumber);
          
              //TODO: Handle bignums that are beyond javascript
              return makeString(String(str.s.charAt(n.n.toFixnum())));
          },


          /**@type {PMethod}*/
          'tostring' : 
          /**
            @param {!PString} me
            @return {!PString}
          */
          method(me) {
              checkIf(me, isString);
              return makeString(me.s);
          },

          /**@type {PMethod}*/
          'to-upper' : 
          /**
            @param {!PString} me
            @return {!PString}
          */
          method(me) {
              checkIf(me, isString);
              return makeString(me.s.toUpperCase());
          },

          /**@type {PMethod}*/
          'to-lower' : 
          /**
            @param {!PString} me
            @return {!PString}
          */
          method(me) {
              checkIf(me, isString);
              return makeString(me.s.toLowerCase());
          },

          /**@type {PMethod}*/
          'tonumber' : 
          /**
            @param {!PString} me
            @return {!PNumber | !PNothing}
          */
          method(me) {
              checkIf(me, isString);
              var num = jsnums.fromString(me.s);
              if(num !== false) {
                  return makeNumberBig(/**@type {Bignum}*/ (num));
              }
              else {
                  return makeNothing();
              }
          },

          /**@type {PMethod}*/
          'length' : 
          /**
            @param {!PString} me
            @return {!PNumber}
          */
          method(me) {
              checkIf(me, isString);
              return makeNumber(me.s.length);
          }, 

          /**@type {PMethod}*/
          '_lessthan' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PBoolean}
          */
          method(left, right) {
              checkIf(left, isNumber);
              checkIf(right, isNumber);

              return makeBoolean(left.s < right.s);
          },

          /**@type {PMethod}*/
          '_greaterthan' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PBoolean}
          */
          method(left, right) {
              checkIf(left, isNumber);
              checkIf(right, isNumber);

              return makeBoolean(left.s > right.s);
          },
          /**@type {PMethod}*/
          '_lessequal' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PBoolean}
          */
          method(left, right) {
              checkIf(left, isNumber);
              checkIf(right, isNumber);

              return makeBoolean(left.s <= right.s);
          },
          /**@type {PMethod}*/
          '_greaterequal' : 
          /**
            @param {!PString} left
            @param {!PString} right
            @return {!PBoolean}
          */
          method(left, right) {
              checkIf(left, isNumber);
              checkIf(right, isNumber);

              return makeBoolean(left.s >= right.s);
          }
      };
  }
  return {getBaseStringDict: getBaseStringDict};
});
