
define(["../../lib/js-numbers/src/js-numbers"], function (jsnums) {
    console.log("loading matchers");

    function addPyretMatchers(jasmine) {
        jasmine.addMatchers({
            toHaveEmptyDict : function() {
                return (this.actual.dict !== undefined) && (Object.keys(this.actual.dict).length === 0);
            },
            toHaveNoBrands : function() {
                for(var i in this.actual) {
                  if(i.indexOf("$brand") === 0) { return false; }
                  return true;
                }
            },
            //Tests equality with ===, must be exact same
            toBeIdentical : function(expect) {
                this.message = function() {
                  return ["Custom expectation: values were not identical"];
                };
                return this.actual === expect;
            },
            toBeSameAs : function(rt, expect) {
                this.message = function() {
                  return ["Custom expectation: " + rt.toReprJS(this.actual) + " to equal " + rt.toReprJS(expect)];
                };
                return rt.same(this.actual, expect);
            },
            toBigEqual : function(expect) {
                return jsnums['equals'](this.actual, jsnums.fromFixnum(expect));
            },
            //Tests equality of bignums
            toBeBig : function(expect) {
                return jsnums['equals'](this.actual, expect);
            },
            toBeSuccess : function(rt) {
                this.message = function() {
                  return ["Custom expectation: " + rt.toReprJS(this.actual.result) + " to be success."];
                };
                return rt.isSuccessResult(this.actual);
            },
            toBeFailure : function(rt) {
                this.message = function() {
                  return ["Custom expectation: " + this.actual + " to be failure."];
                };
                return rt.isFailureResult(this.actual);
            },
            toPassPredicate : function(pred) {
                this.message = function() {
                  return ["Custom expectation: failed predicate"];
                };
                return pred(this.actual);
            },
            toBeCompileError : function(cs, rt) {
                this.message = function() {
                  return ["Custom expectation: " + rt.toReprJS(this.actual) + " to be compile error"];
                };
                return rt.unwrap(gf(cs, "is-err").app(this.actual)) === true;
            },
            toBeInstanceOf : function(cls) {
                return this.actual instanceof cls;
            }
        });
    }

    return { addPyretMatchers: addPyretMatchers };

});
