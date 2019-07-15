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
    }
  },
  nativeRequires: ["fs"],
  theModule: function(RUNTIME, NAMESPACE, uri, fs) {
    function InputFile(name, fd) {
      this.name = name;
      this.fd = fd;
    }

    function OutputFile(name, fd) {
      this.name = name;
      this.fd = fd;
    }

    // Use ASYNC file operations with Pyret
    // HowTo: https://www.pyret.org/docs/latest/s_running.html#%28part._.Asynchronous_.J.S_and_.Pyret%29
    var vals = {
      "open-input-file": RUNTIME.makeFunction(function(filename) {
          RUNTIME.ffi.checkArity(1, arguments, "open-input-file", false);
          RUNTIME.checkString(filename);
          var stringName = RUNTIME.unwrap(filename);
          return RUNTIME.pauseStack(function(restarter) {
            fs.open(stringName, "r", function(err, fd) {
              restarter.resume(RUNTIME.makeOpaque(new InputFile(stringName, fd)));
            });
          });
        }, "open-input-file"),
      "open-output-file": RUNTIME.makeFunction(function(filename, append) {
          RUNTIME.ffi.checkArity(2, arguments, "open-output-file", false);
          RUNTIME.checkString(filename);
          RUNTIME.checkBoolean(append);
          var stringName = RUNTIME.unwrap(filename);
          var appendOption = RUNTIME.unwrap(append);
          return RUNTIME.pauseStack(function(restarter) {
            fs.open(stringName, (appendOption ? "a" : "w"), function(err, fd) {
              restarter.resume(RUNTIME.makeOpaque(
                new OutputFile(stringName, fd)));
            });
          });
        }, "open-output-file"),
      "read-file": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "read-file", false);
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(v instanceof InputFile) {
            if (v.name) {
              return RUNTIME.pauseStack(function(restarter) {
                fs.readFile(v.name, {encoding: 'utf8'}, function(err, data) {
                  // NOTE(alex): ignore errors for now
                  restarter.resume(RUNTIME.makeString(data));
                });
              });
            } else {
              throw Error("Attempting to read an already-closed file");
            }
          }
          else {
            throw Error("Expected file in read-file, but got something else");
          }
        }, "read-file"),
      "display": RUNTIME.makeFunction(function(file, val) {
          RUNTIME.ffi.checkArity(2, arguments, "display", false);
          RUNTIME.checkOpaque(file);
          RUNTIME.checkString(val);
          var v = file.val;
          var s = RUNTIME.unwrap(val);
          if(v instanceof OutputFile) {
            if (v.name) {
              return RUNTIME.pauseStack(function(restarter) {
                fs.writeFile(v.name, s, {
                  encoding: 'utf8', 
                  flag: 'a+'
                }, function(err, bytesWritten, buffer) {
                  // NOTE(alex): ignore errors for now
                  restarter.resume(NAMESPACE.get('nothing'));
                });
              });
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
              return RUNTIME.pauseStack(function(restarter) {
                fs.fsync(v.fd, function(err) {
                  // NOTE(alex): ignore errors for now
                  restarter.resume(NAMESPACE.get('nothing'));
                });
              });
            } else {
              throw Error("Attempting to flush an already-closed file");
            }
          }
          else {
            throw Error("Expected file in read-file, but got something else");
          }
        }, "flush-output-file"),                  
      "mtimes": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "mtimes", false);
          RUNTIME.checkString(path);
          return RUNTIME.pauseStack(function(restarter) {
            fs.exists(path, function(exists) {
              if (!exists) {
                restarter.error(
                  RUNTIME.ffi.makeMessageException("File " + path + " did not exist when getting file-times"));
              }

              fs.lstat(path, function(error, stats) {
                  restarter.resume(RUNTIME.makeObject({
                    mtime: Number(stats.mtime),
                    atime: Number(stats.atime),
                    ctime: Number(stats.ctime)
                  }));
              });
            })
          });
          
        }, "file-times"),
      "file-times": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "file-times", false);
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(!(v instanceof InputFile || v instanceof OutputFile)) {
            RUNTIME.ffi.throwMessageException("Expected a file, but got something else");
          }
          return RUNTIME.pauseStack(function(restarter) {
            fs.exists(v.name, function(exists) {
              if (!exists) {
                restarter.error(
                  RUNTIME.ffi.makeMessageException("File " + v.name + " did not exist when getting file-times"));
              }

              fs.lstat(v.name, function(error, stats) {
                  restarter.resume(RUNTIME.makeObject({
                    mtime: Number(stats.mtime),
                    atime: Number(stats.atime),
                    ctime: Number(stats.ctime)
                  }));
              });
            })
          });
          
        }, "file-times"),
      "real-path": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "real-path", false);
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);

          return RUNTIME.pauseStack(function(restarter) {
            fs.realpath(s, function(err, resolvedPath) {
              var newPath;
              if (err) {
                newPath = s;
              } else {
                newPath = resolvedPath;
              }

              restarter.resume(RUNTIME.makeString(newPath));
            });
          });
          
        }, "real-path"),
      "exists": RUNTIME.makeFunction(function(path) {
          RUNTIME.ffi.checkArity(1, arguments, "exists", false);
          RUNTIME.checkString(path);
          var s = RUNTIME.unwrap(path);
          return RUNTIME.pauseStack(function(restarter) {
            fs.exists(s, function(exists) {
              restarter.resume(RUNTIME.makeBoolean(exists));
            });
          });
        }, "exists"),
      "close-output-file": RUNTIME.makeFunction(function(file) { 
          RUNTIME.ffi.checkArity(1, arguments, "close-output-file", false);
          RUNTIME.checkOpaque(file);
          var v = file.val;
          if(v instanceof OutputFile) {
            if (v.fd) {
              return RUNTIME.pauseStack(function(restarter) {
                fs.close(v.fd, function(err) {
                  // NOTE(alex): ignore errors for now
                  v.fd = false;
                  restarter.resume(NAMESPACE.get('nothing'));
                });
              });
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
              return RUNTIME.pauseStack(function(restarter) {
                fs.close(v.fd, function(err) {
                  // NOTE(alex): ignore errors for now
                  v.fd = false;
                  restarter.resume(NAMESPACE.get('nothing'));
                });
              });
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
        return RUNTIME.pauseStack(function(restarter) {
          fs.mkdir(directory, function(err) {
            // NOTE(alex): ignore errors for now
            restarter.resume(true);
          });
        });
      }, "create-dir"),
      "list-files": RUNTIME.makeFunction(function(directory) {
          RUNTIME.ffi.checkArity(1, arguments, "list-files", false);
          RUNTIME.checkString(directory);
          var dir = RUNTIME.unwrap(directory);
          return RUNTIME.pauseStack(function(restarter) {
            fs.readdir(dir, function(err, files) {
              // NOTE(alex): ignore errors for now
              var contents = files;
              restarter.resume(RUNTIME.ffi.makeList(contents.map(RUNTIME.makeString)));
            });
          });
      }, "list-files"),
      "symlink": RUNTIME.makeFunction(function(target, path, fileOrDir) {
        return RUNTIME.pauseStack(function(restarter) {
          fs.symlink(target, path, fileOrDir, function(err) {
            // NOTE(alex): ignore errors for now
            restarter.resume(true);
          });
        });
      })
    };

    return RUNTIME.makeModuleReturn(vals, {});
  }
})

