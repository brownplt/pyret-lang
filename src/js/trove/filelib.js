define(["js/runtime-util", "fs"], function(util, fs) {

  return util.memoModule("filelib", function(RUNTIME, NAMESPACE) {
    
    function InputFile(name) {
      this.name = name;
    }

    function OutputFile(name, append) {
      this.name = name;
      this.append = append;
    }

    return RUNTIME.makeObject({
        provide: RUNTIME.makeObject({
            "open-input-file": RUNTIME.makeFunction(function(filename) {
                RUNTIME.checkIf(filename, RUNTIME.isString);
                var s = RUNTIME.unwrap(filename);
                return RUNTIME.makeOpaque(new InputFile(s));
              }),
            "open-output-file": RUNTIME.makeFunction(function(filename, append) {
                RUNTIME.checkIf(filename, RUNTIME.isString);
                RUNTIME.checkIf(append, RUNTIME.isBoolean);
                var s = RUNTIME.unwrap(filename);
                var b = RUNTIME.unwrap(append);
                return RUNTIME.makeOpaque(new OutputFile(s, b));
              }),
            "read-file": RUNTIME.makeFunction(function(file) {
                RUNTIME.checkIf(file, RUNTIME.isOpaque);
                var v = file.val;
                if(v instanceof InputFile) {
                  return RUNTIME.makeString(fs.readFileSync(v.name, {encoding: 'utf8'}));
                }
                else {
                  throw Error("Expected file in read-file, but got something else");
                }
              }),
            "display": RUNTIME.makeFunction(function(file, val) {
                RUNTIME.checkIf(file, RUNTIME.isOpaque);
                RUNTIME.checkIf(val, RUNTIME.isString);
                var v = file.val;
                var s = RUNTIME.unwrap(val);
                if(v instanceof OutputFile) {
                  fs.writeFileSync(v.name, s, {encoding: 'utf8'});
                  return NAMESPACE.get('nothing');
                }
                else {
                  throw Error("Expected file in read-file, but got something else");
                }
              }),
            "close-output-file": RUNTIME.makeFunction(function(file) { }),
            "close-input-file": RUNTIME.makeFunction(function(file) { })
          }),
        answer: NAMESPACE.get("nothing")
      });
  });    
});

