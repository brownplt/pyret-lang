define(["require", "../../../lib/js-numbers/src/js-numbers"], function(require, jsnums) {

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
          'replace' : 
          /**
            @param {!PString} str
            @param {!PString} replacee
            @param {!PBoolean} replacand 
            @return {!PBase}
          */
          method(str, replacee, replacand) {
              checkIf(str, isString);
              checkIf(replacee, isString);
              checkIf(replacand, isString);
              return makeString(str.s.replace(new RegExp(replacee.s,'g'), replacand.s));
          },

          /**@type {PMethod}*/
          'split' : 
          /**
            @param {!PString} str
            @param {!PString} splitstr
            @param {!PBoolean} repeated 
            @return {!PBase}
          */
          method(str, splitstr, repeated) {
              checkIf(str, isString);
              checkIf(splitstr, isString);

              var list = require("./ffi-helpers")(rt, rt.namespace);

              return list.makeList(str.s.split(splitstr.s).map(rt.makeString));
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
              return makeString(String(str.s.charAt(jsnums.toFixnum(n.n))));
          },

          /**@type {PMethod}*/
          'repeat' : 
          /**
            @param {!PString} str
            @param {!PNumber} n
            @return {!PString}
          */
          method(str, n) {
              checkIf(str, isString);
              checkIf(n, isNumber);
              var resultStr = "";
              // TODO(joe): loop up to a fixnum?
              for(var i = 0; i < jsnums.toFixnum(n.n); i++) {
                  resultStr += str.s;
              }
              return makeString(resultStr);
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
          'substring' : 
          /**
            @param {!PString} me
            @param {!PNumber} min
            @param {!PNumber} max
            @return {!PString}
          */
          makeMethod(function(me) { return function(min, max) {
                checkIf(me, isString);
                checkIf(min, isNumber);
                checkIf(min, isNumber);
                return makeString(me.s.substring(jsnums.toFixnum(min.n), jsnums.toFixnum(max.n)));
              };
            },
            function(me, min, max) {
                checkIf(me, isString);
                checkIf(min, isNumber);
                checkIf(min, isNumber);
                return makeString(me.s.substring(jsnums.toFixnum(min.n), jsnums.toFixnum(max.n)));
            }),

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
              checkIf(left, isString);
              checkIf(right, isString);

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
              checkIf(left, isString);
              checkIf(right, isString);

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
              checkIf(left, isString);
              checkIf(right, isString);

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
              checkIf(left, isString);
              checkIf(right, isString);

              return makeBoolean(left.s >= right.s);
          } 
      };
  }
  return {getBaseStringDict: getBaseStringDict};
});
