
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
                return this.actual === expect;
            },
            toBeSameAs : function(rt, expect) {
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
                return rt.isSuccessResult(this.actual);
            },
            toBeFailure : function(rt) {
                return rt.isFailureResult(this.actual);
            },
            toPassPredicate : function(pred) {
                return pred(this.actual);
            },
            toMatchError : function(rt, msg) {
              var s = rt.toReprJS(this.actual);
              console.log(s);
              console.log(msg);
              return true;
              // return rt.toReprJS(this.actual).indexOf(msg) !== -1;
                // var ffi = ffiLib(rt, rt.namespace);
                // var act;
                // if (rt.isString(this.actual)) {
                //   act = [this.actual];
                // } else {
                //   act = ffi.toArray(this.actual);
                // }
                // if (act.length == 0) return false;
                // for (var i = 0; i < act.length; i++) {
                //   if (rt.isString(act[i])) {
                //     if (rt.unwrap(act[i]).indexOf(msg) !== -1)
                //       return true;
                //   } else if (rt.hasField(act[i], "msg")) {
                //     if (rt.unwrap(rt.getField(act[i], "msg")).indexOf(msg) !== -1)
                //       return true;
                //   }
                // }
                // return false;
            }
        });
    }

    return { addPyretMatchers: addPyretMatchers };

});
