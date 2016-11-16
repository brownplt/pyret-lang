({
  requires: [],
  provides: {
    values: {
      "open-input-file": "tany",
      "open-output-file": "tany",
      "read-file": "tany",
      "display": "tany",
      "flush-output-file": "tany",                  
      "file-times": "tany",
      "real-path": "tany",
      "exists": "tany",
      "close-output-file": "tany",
      "close-input-file": "tany",
      "create-dir": "tany",
      "list-files": "tany"
    },
    aliases: {},
    datatypes: {}
  },
  nativeRequires: ["fs"],
  theModule: function(RUNTIME, NAMESPACE, uri, fs) {
    function InputFile(name) {
      this.name = name;
      this.fd = fs.openSync(name, "r")
    }

    function OutputFile(name, append) {
      this.name = name;
      this.fd = fs.openSync(name, (append ? "a" : "w"));
    }

    var vals = {
      "open-input-file": RUNTIME.makeFunction(function(filename) {
          RUNTIME.ffi.checkArity(1, arguments, "open-input-file");
          RUNTIME.checkString(filename);
          var s = RUNTIME.unwrap(filename);
          return RUNTIME.makeOpaque(new InputFile(s));
        }, "open-input-file"),
      "open-output-file": RUNTIME.makeFunction(function(filename, append) {
          RUNTIME.ffi.checkArity(2, arguments, "open-output-file");
          RUNTIME.checkString(filename);
          RUNTIME.checkBoolean(append);
          var s = RUNTIME.unwrap(filename);
          var b = RUNTIME.unwrap(append);
          return RUNTIME.makeOpaque(new OutputFile(s, b));
        }, "open-output-file"),
      "read-file": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "read-file");
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(v instanceof InputFile) {
            if (v.fd) {
              return RUNTIME.makeString(fs.readFileSync(v.fd, {encoding: 'utf8'}));
            } else {
              throw Error("Attempting to read an already-closed file");
            }
          }
          else {
            throw Error("Expected file in read-file, but got something else");
          }
        }, "read-file"),
      "display": RUNTIME.makeFunction(function(file, val) {
          RUNTIME.ffi.checkArity(2, arguments, "display");
          RUNTIME.checkOpaque(file);
          RUNTIME.checkString(val);
          var v = file.val;
          var s = RUNTIME.unwrap(val);
          if(v instanceof OutputFile) {
            if (v.fd) {
              fs.writeSync(v.fd, s, {encoding: 'utf8'});
              return NAMESPACE.get('nothing');
            } else {
              console.error("Failed to display to " + v.name);
              throw Error("Attempting to write to an already-closed file");
            }
          }
          else {
            throw Error("Expected file in display, but got something else");
          }
        }, "display"),
      "flush-output-file": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "flush-output-file");
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(v instanceof OutputFile) {
            if (v.fd) {
              fs.fsyncSync(v.fd);
              return NAMESPACE.get('nothing');
            } else {
              throw Error("Attempting to flush an already-closed file");
            }
          }
          else {
            throw Error("Expected file in read-file, but got something else");
          }
        }, "flush-output-file"),                  
      "file-times": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "file-times");
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
        }, "file-times"),
      "real-path": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "real-path");
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);
          var newpath;
          try {
            newpath = fs.realpathSync(s);
          } catch(e) {
            newpath = s; // should this be an error instead?
          }
          return RUNTIME.makeString(newpath);
        }, "real-path"),
      "exists": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "exists");
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);
          var e = fs.existsSync(s);
          return RUNTIME.makeBoolean(e);
        }, "exists"),
      "close-output-file": RUNTIME.makeFunction(function(file) { 
          RUNTIME.ffi.checkArity(1, arguments, "close-output-file");
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(v instanceof OutputFile) {
            if (v.fd) {
              fs.closeSync(v.fd);
              v.fd = false;
              return NAMESPACE.get('nothing');
            } else {
              throw Error("Attempting to close an already-closed file");
            }
          }
          else {
            throw Error("Expected file in close-output-file, but got something else");
          }                  
        }, "close-output-file"),
      "close-input-file": RUNTIME.makeFunction(function(file) { 
          RUNTIME.ffi.checkArity(1, arguments, "close-input-file");
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(v instanceof InputFile) {
            if (v.fd) {
              fs.closeSync(v.fd);
              v.fd = false;
              return NAMESPACE.get('nothing');
            } else {
              throw Error("Attempting to close an already-closed file");
            }
          }
          else {
            throw Error("Expected file in close-input-file, but got something else");
          }                  
        }, "close-input-file"),
      "create-dir": RUNTIME.makeFunction(function(directory) {
        RUNTIME.ffi.checkArity(1, arguments, "create-dir");
        RUNTIME.checkString(directory);
        fs.mkdirSync(directory);
        return true;
      }, "create-dir"),
      "list-files": RUNTIME.makeFunction(function(directory) {
          RUNTIME.ffi.checkArity(1, arguments, "list-files");
          RUNTIME.checkString(directory);
          var dir = RUNTIME.unwrap(directory);
          var contents = fs.readdirSync(dir)
          return RUNTIME.ffi.makeList(contents.map(RUNTIME.makeString))
      }, "list-files")
    };

    return RUNTIME.makeModuleReturn(vals, {});
  }
})

