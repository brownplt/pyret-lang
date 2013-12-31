var $ = require('jquery');
var LIB = {
  name: "compile",
  lib: function(context, runtime) {
    var rt = runtime;
    var debug = context.debug;
    var compilerURL = context.compilerURL;
    var mooringsNamespace = context.mooringsNamespace;
    return rt.runtime.makeObject({
      compile: rt.runtime.makeFunction(function(str, ids, cb) {
        if (rt.runtime.isString(ids)) {
          ids = ids.s;
        } else {
          throw "Lists not supported yet in compile.";
        }
        var result = $.ajax(compilerURL, {
            type: "POST",
            datatype: "json",
            data: {
              src: str.s,
              options: JSON.stringify({
                  check: true,
                  ids: ids
                })
            }
          });
        result.then(function(r) {
          debug("Compiled result is: ", JSON.parse(r));
          rt.runtime.applyFunc(cb, [rt.runtime.makeOpaque(JSON.parse(r))]);
        });
        return rt.runtime.makeOpaque(result);
      }),
      eval: rt.runtime.makeFunction(function(compiled, namespace) {
        var ns = namespace.getVal();
        compiled = compiled.getVal();
        debug("Compiled to run is: ", compiled);
        debug("Compiled to run is: ", compiled["js-src"]);
        var answer = (1,eval)(compiled["js-src"])(rt.runtime, runtime.namespace.merge(ns));
        debug("Answer is: ", answer.val);
        return answer.val;
      }),
      mooringsNamespace: rt.runtime.makeOpaque(mooringsNamespace)
    });
  }
};
if(typeof exports !== "undefined") {
  exports.lib = LIB;
}
