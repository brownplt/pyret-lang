define(["js/runtime-util", "fs", "js/ffi-helpers"], function(util, fs, ffiLib) {
  return util.definePyretModule("filelib",
    [],
    [],
    {},
    function(RUNTIME, NAMESPACE) {
      return RUNTIME.loadJSModules(NAMESPACE, [ffiLib], function(ffi) {

      function InputFile(name) {
        this.name = name;
      }

      function OutputFile(name, append) {
        this.name = name;
        this.append = append;
      }

      return RUNTIME.makeObject({
          "provide-plus-types": RUNTIME.makeObject({
              types: { },
              values: RUNTIME.makeObject({
              "open-input-file": RUNTIME.makeFunction(function(filename) {
                  ffi.checkArity(1, arguments, "open-input-file");
                  RUNTIME.checkString(filename);
                  var s = RUNTIME.unwrap(filename);
                  return RUNTIME.makeOpaque(new InputFile(s));
                }),
              "open-output-file": RUNTIME.makeFunction(function(filename, append) {
                  ffi.checkArity(2, arguments, "open-output-file");
                  RUNTIME.checkString(filename);
                  RUNTIME.checkBoolean(append);
                  var s = RUNTIME.unwrap(filename);
                  var b = RUNTIME.unwrap(append);
                  return RUNTIME.makeOpaque(new OutputFile(s, b));
                }),
              "read-file": RUNTIME.makeFunction(function(file) {
                  ffi.checkArity(1, arguments, "read-file");
                  RUNTIME.checkOpaque(file);
                  var v = file.val;
                  if(v instanceof InputFile) {
                    return RUNTIME.makeString(fs.readFileSync(v.name, {encoding: 'utf8'}));
                  }
                  else {
                    throw Error("Expected file in read-file, but got something else");
                  }
                }),
              "display": RUNTIME.makeFunction(function(file, val) {
                  ffi.checkArity(2, arguments, "display");
                  RUNTIME.checkOpaque(file);
                  RUNTIME.checkString(val);
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
              "file-times": RUNTIME.makeFunction(function(file) {
                  ffi.checkArity(1, arguments, "file-times");
                  RUNTIME.checkOpaque(file);
                  var v = file.val;
                  if(!(v instanceof InputFile || v instanceof OutputFile)) {
                    RUNTIME.ffi.throwMessageException("Expected a file, but got something else");
                  }
                  if(!fs.existsSync(v.name)) {
                    RUNTIME.ffi.throwMessageException("File " + v.name + " did not exist when getting file-times");
                  }
                  var stats = fs.lstatSync(v.name);
                  return RUNTIME.makeObject({
                    mtime: Number(stats.mtime),
                    atime: Number(stats.atime),
                    ctime: Number(stats.ctime)
                  });
                }),
              "real-path": RUNTIME.makeFunction(function(path) {
                  ffi.checkArity(1, arguments, "real-path");
                  RUNTIME.checkString(path);
                  var s = RUNTIME.unwrap(path);
                  var newpath = fs.realpathSync(s);
                  return RUNTIME.makeString(newpath);
                }),
              "exists": RUNTIME.makeFunction(function(path) {
                  ffi.checkArity(1, arguments, "exists");
                  RUNTIME.checkString(path);
                  var s = RUNTIME.unwrap(path);
                  var e = fs.existsSync(s);
                  return RUNTIME.makeBoolean(e);
                }),
              "close-output-file": RUNTIME.makeFunction(function(file) { 
                  ffi.checkArity(1, arguments, "close-output-file");
                }),
              "close-input-file": RUNTIME.makeFunction(function(file) { 
                  ffi.checkArity(1, arguments, "close-input-file");
                }),
              "list-files": RUNTIME.makeFunction(function(directory) {
                  ffi.checkArity(1, arguments, "list-files");
                  RUNTIME.checkString(directory);
                  var dir = RUNTIME.unwrap(directory);
                  var contents = fs.readdirSync(dir)
                  return ffi.makeList(contents.map(RUNTIME.makeString))
                })
              })
            }),
          answer: NAMESPACE.get("nothing")
        });
      });

    });
});

