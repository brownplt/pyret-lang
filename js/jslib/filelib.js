var fs = require('fs');
var LIB = {
  name: "filelib",
  lib: function(context, runtime) {
    var rt = runtime;
    return rt.runtime.makeObject({
      "read-file": rt.runtime.makeFunction(function(path) {
        rt.runtime.checkPrimitive(rt.runtime.isString, "read-file", [path]);
        var file = fs.readFileSync(path.s, {encoding: 'utf8'});
        if (typeof file === 'string') {
          return rt.runtime.makeString(file);
        } else {
          throw rt.runtime.applyFunc(rt.namespace.get('raise'), [file]);
        }
      }),
      "is-dir": rt.runtime.makeFunction(function(path) {
        rt.runtime.checkPrimitive(rt.runtime.isString, "is-dir", [path]);
        var stat = fs.statSync(path.s);
        return rt.runtime.makeBool(stat.isDirectory());
      }),
      "is-file": rt.runtime.makeFunction(function(path) {
        rt.runtime.checkPrimitive(rt.runtime.isString, "is-file", [path]);
        var stat = fs.statSync(path.s);
        return rt.runtime.makeBool(stat.isFile());
      }),
      "read-dir": rt.runtime.makeFunction(function(path) {
        rt.runtime.checkPrimitive(rt.runtime.isString, "read-dir", [path]);
        var link = context.mooringsNamespace.get('link');
        var empty = context.mooringsNamespace.get('empty');
        try {
          var files = fs.readdirSync(path.s);
        } catch(e) {
          throw rt.runtime.applyFunc(rt.namespace.get('raise'), [rt.runtime.makeObject({
              errno: rt.runtime.makeNumber(e.errno),
              code: rt.runtime.makeString(e.code),
              message: rt.runtime.makeString(e.toString())
            })]);
        }
        var retList = empty;
        files.forEach(function(f) {
          retList = rt.runtime.applyFunc(link, [rt.runtime.makeString(f), retList]);
        });
        return retList;
      })
    });
  }
};

if(typeof exports !== "undefined") {
  exports.lib = LIB;
}
