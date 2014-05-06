
define(["js/js-numbers"], function (jsnums) {
    console.log("loading matchers");

    function addPyretMatchers(jasmine) {
        jasmine.addMatchers({
            toHaveEmptyDict : function() {
                return (this.actual.dict !== undefined) && (Object.keys(this.actual.dict).length === 0);
            },
            toHaveNoBrands : function() {
              for(var i in this.actual) {
                if(i.indexOf("$brand") === 0) { return false; }
              }
              return true;
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
                  if (rt.isFailureResult(this.actual)) {
                    console.log("Exn: ", this.actual.exn);
                    console.log("String Exn: ", String(this.actual.exn));
                    var err = this.actual.exn[0];
                    if(err) {
                      var errstr = rt.unwrap(rt.getField(err, "tostring").app());
                      return [errstr];
                    } else {
                      return [String(this.actual.exn)];
                    }
                  }
                  return ["Custom expectation: " + JSON.stringify(this.actual).substring(0, 200) + " to be success."];
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
            },
            toContainString : function(s) {
                return this.actual.indexOf(s) !== -1;
            },
            toBeMessageExn : function(rt, s) {
                return rt1.unwrap(rt1.getField(e, "message")).indexOf(s) !== -1;
            },
            toThrowRuntimeExn : function(s) {
                try {
                  this.actual();
                  return false;
                } catch(e) {
                  if (e.toString().indexOf(s) >= 0) return true;
                  this.message = function() {
                    return ["Expected " + JSON.stringify(s) + " but got " + JSON.stringify(e.toString())];
                  }
                  return false;
                }
            }
        });
    }

    return { addPyretMatchers: addPyretMatchers };

});
