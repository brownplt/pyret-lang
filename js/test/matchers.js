
var jsnums = require('../js-numbers/src/js-numbers.js');

function addPyretMatchers(jasmine) {
  jasmine.addMatchers({
        toHaveEmptyDict : function() {
            return (this.actual.dict !== undefined) && (Object.keys(this.actual.dict).length === 0);
        },
        toHaveNoBrands : function() {
            return (this.actual.brands !== undefined) && (this.actual.brands.length === 0);
        },
        //Tests equality with ===, must be exact same
        toBeIdentical : function(expect) {
            return this.actual === expect;
        },
        toBigEqual : function(expect) {
            return jsnums['equals'](this.actual, jsnums.fromFixnum(expect));
        },
        //Tests equality of bignums
        toBeBig : function(expect) {
            return jsnums['equals'](this.actual, expect);
        }
    });
}

exports['addPyretMatchers'] = addPyretMatchers;
