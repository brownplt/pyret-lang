var jsnums = require('./js-numbers/src/js-numbers.js');

function getBaseNumberDict(rt){
    //Grab the stuff we need from the runtime
    var makeMethod = rt['makeMethod'];
    var makeString = rt['makeString'];
    var makeBoolean = rt['makeBoolean'];
    var checkIf = rt['checkIf'];
    var makeNumberBig = rt['makeNumberBig'];
    var isNumber = rt['isNumber'];
    var isString = rt['isString'];
    var makeMessageException = rt['makeMessageException'];

    //Const for checking for 0
    var big_zero = jsnums.fromFixnum(0);

    //Make the dictionary
    return {
        /**@type {PMethod}*/
        '_plus' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeNumberBig(jsnums.add(left.n,right.n));
        },

        /**@type {PMethod}*/
        '_minus' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeNumberBig(jsnums.subtract(left.n,right.n));
        },

        /**@type {PMethod}*/
        '_times' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeNumberBig(jsnums.multiply(left.n,right.n));
        },

        /**@type {PMethod}*/
        '_divide' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PNumber}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);
            if(jsnums.equals(big_zero, right.n)) {
                throw makeMessageException("Division by zero");
            }
            return makeNumberBig(jsnums.divide(left.n,right.n));
        },

        /**@type {PMethod}*/
        '_equals' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(jsnums.equals(left.n,right.n));
        },

        /**@type {PMethod}*/
        '_lessthan' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(jsnums.lessThan(left.n,right.n));
        },

        /**@type {PMethod}*/
        '_greaterthan' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(jsnums.greaterThan(left.n,right.n));
        },
        /**@type {PMethod}*/
        '_lessequal' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(jsnums.lessThanOrEqual(left.n,right.n));
        },
        /**@type {PMethod}*/
        '_greaterequal' : 
        /**
          @param {!PNumber} left
          @param {!PNumber} right
          @return {!PBoolean}
        */
        method(left, right) {
            checkIf(left, isNumber);
            checkIf(right, isNumber);

            return makeBoolean(jsnums.greaterThanOrEqual(left.n,right.n));
        },

        /**@type {PMethod}*/
        'tostring' : 
        /**
          @param {!PNumber} me
          @return {!PString}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeString(me.n.toString());
        },

        /**@type {PMethod}*/
        'sin' : 
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeNumberBig(jsnums.sin(me.n));
        },

        /**@type {PMethod}*/
        'cos' : 
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeNumberBig(jsnums.cos(me.n));
        },

        /**@type {PMethod}*/
        'tan' : 
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeNumberBig(jsnums.tan(me.n));
        },

        /**@type {PMethod}*/
        'asin' : 
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeNumberBig(jsnums.asin(me.n));
        },

        /**@type {PMethod}*/
        'acos' : 
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeNumberBig(jsnums.acos(me.n));
        },

        /**@type {PMethod}*/
        'atan' : 
        /**
          @param {!PNumber} me
          @return {!PNumber}
        */
        method(me) {
            checkIf(me, isNumber);
            return makeNumberBig(jsnums.atan(me.n));
        }
    }
};

exports['getBaseNumberDict'] = getBaseNumberDict;
