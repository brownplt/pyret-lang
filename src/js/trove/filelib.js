({
  requires: [
    { "import-type": "builtin", name: "string-dict" },
  ],
  provides: {
    values: {
      "open-input-file": "tany",
      "open-output-file": "tany",
      "read-file": "tany",
      "file-to-string": "tany",
      "display": "tany",
      "flush-output-file": "tany",
      "file-times": "tany",
      "real-path": "tany",
      "exists": "tany",
      "is-file": "tany",
      "is-dir": "tany",
      "close-output-file": "tany",
      "close-input-file": "tany",
      "create-dir": "tany",
      "create-dir-tree": "tany",
      "list-files": "tany"
    }
  },
  nativeRequires: ["fs"],
  theModule: function(RUNTIME, NAMESPACE, uri, SD, fs) {
    function InputFile(name) {
      this.name = name;
      this.fd = fs.openSync(name, "r");
    }

    function OutputFile(name, append) {
      this.name = name;
      this.fd = fs.openSync(name, (append ? "a" : "w"));
    }
    var checkISD = RUNTIME.getField(SD, "internal").checkISD;
    var isISD = RUNTIME.getField(SD, "internal").isISD;
    function makeDirs(baseDir, tree) {
      if (!fs.existsSync(baseDir)) fs.mkdirSync(baseDir);
      if (isISD(tree)) {
        RUNTIME.ffi.toArray(RUNTIME.getField(tree, "keys-list").app()).forEach(function(sub) {
          var kid = RUNTIME.getColonField(tree, "get-value").full_meth(tree, sub);
          makeDirs(baseDir + "/" + sub, kid);
        });
      }
    }

    var vals = {
      "open-input-file": RUNTIME.makeFunction(function(filename) {
          RUNTIME.ffi.checkArity(1, arguments, "open-input-file", false);
          RUNTIME.checkString(filename);
          var s = RUNTIME.unwrap(filename);
          return RUNTIME.makeOpaque(new InputFile(s));
        }, "open-input-file"),
      "open-output-file": RUNTIME.makeFunction(function(filename, append) {
          RUNTIME.ffi.checkArity(2, arguments, "open-output-file", false);
          RUNTIME.checkString(filename);
          RUNTIME.checkBoolean(append);
          var s = RUNTIME.unwrap(filename);
          var b = RUNTIME.unwrap(append);
          return RUNTIME.makeOpaque(new OutputFile(s, b));
        }, "open-output-file"),
      "read-file": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "read-file", false);
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
      "file-to-string": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "file-to-string", false);
          RUNTIME.checkString(path);
          return RUNTIME.pauseStack(async restarter => {
            fs.readFile(path, 'utf8', (err, data) => {
              if (err) {
                restarter.error(RUNTIME.ffi.makeMessageException(String(err)));
              } else {
                restarter.resume(data);
              }
            });
          })
        }, "file-to-string"),

      "display": RUNTIME.makeFunction(function(file, val) {
          RUNTIME.ffi.checkArity(2, arguments, "display", false);
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
          RUNTIME.ffi.checkArity(1, arguments, "flush-output-file", false);
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
          RUNTIME.ffi.checkArity(1, arguments, "file-times", false);
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
          RUNTIME.ffi.checkArity(1, arguments, "real-path", false);
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
          RUNTIME.ffi.checkArity(1, arguments, "exists", false);
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);
          var e = fs.existsSync(s);
          return RUNTIME.makeBoolean(e);
        }, "exists"),
      "is-file": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "is-file", false);
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);
          if (!fs.existsSync(s)) {
            return RUNTIME.makeBoolean(false);
          }
          var stats = fs.lstatSync(s);
          return RUNTIME.makeBoolean(stats.isFile());
        }, "is-file"),
      "is-dir": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "is-file", false);
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);
          if (!fs.existsSync(s)) {
            return RUNTIME.makeBoolean(false);
          }
          var stats = fs.lstatSync(s);
          return RUNTIME.makeBoolean(stats.isDirectory());
        }, "is-dir"),
      "close-output-file": RUNTIME.makeFunction(function(file) { 
          RUNTIME.ffi.checkArity(1, arguments, "close-output-file", false);
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
          RUNTIME.ffi.checkArity(1, arguments, "close-input-file", false);
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
        RUNTIME.ffi.checkArity(1, arguments, "create-dir", false);
        RUNTIME.checkString(directory);
        fs.mkdirSync(directory);
        return true;
      }, "create-dir"),
      "create-dir-tree": RUNTIME.makeFunction(function(baseDir, subtree) {
        RUNTIME.ffi.checkArity(2, arguments, "create-dir-tree", false);
        RUNTIME.checkString(baseDir);
        checkISD(subtree);
        makeDirs(baseDir, subtree);
        return true;
      }, "create-dir-tree"),
      "list-files": RUNTIME.makeFunction(function(directory) {
          RUNTIME.ffi.checkArity(1, arguments, "list-files", false);
          RUNTIME.checkString(directory);
          var dir = RUNTIME.unwrap(directory);
          var contents = fs.readdirSync(dir)
          return RUNTIME.ffi.makeList(contents.map(RUNTIME.makeString))
      }, "list-files")
    };

    return RUNTIME.makeModuleReturn(vals, {});
  }
})

