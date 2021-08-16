import { List, PFunction, PObject, Runtime } from "./ts-impl-types";
import type {
  readFile,
  open,
  writeFile,
  fsync,
  exists,
  lstat,
  realpath,
  close,
  mkdir,
  readdir,
  symlink,
} from 'fs';

type FS = {
  readFile: typeof readFile,
  open: typeof open,
  writeFile: typeof writeFile,
  fsync: typeof fsync,
  exists: typeof exists,
  lstat: typeof lstat,
  realpath: typeof realpath,
  close: typeof close,
  mkdir: typeof mkdir,
  readdir: typeof readdir,
  symlink: typeof symlink,
};

export type InputFile = {val: {
  name: string,
  fd: number | false,
}};
export type OutputFile = {val: {
  name: string,
  fd: number | false,
}};
export type Exports = {
  dict: {
    values: {
      dict: {
        'open-input-file': PFunction<(filename: string) => InputFile>,
        'open-output-file': PFunction<(filename: string, append: boolean) => OutputFile>,
        'read-file-path': PFunction<(file: string) => string>,
        'read-file': PFunction<(file: InputFile) => string>,
        'display': PFunction<(file: OutputFile, val: any) => Runtime['nothing']>,
        'flush-output-file': PFunction<(file: OutputFile) => Runtime['nothing']>,
        'mtimes': PFunction<(path: string) => PObject<{mtime: number, atime: number, ctime: number}>>,
        'file-times': PFunction<(file: InputFile) => PObject<{mtime: number, atime: number, ctime: number}>>,
        'real-path': PFunction<(path: string) => string>,
        'exists': PFunction<(path: string) => boolean>,
        'close-output-file': PFunction<(file: OutputFile) => Runtime['nothing']>,
        'close-input-file': PFunction<(file: InputFile) => Runtime['nothing']>,
        'create-dir': PFunction<(directory: string) => boolean>,
        'list-files': PFunction<(directory: string) => List<string>>,
        'symlink': PFunction<(target: string, path: string, fileOrDir: symlink.Type) => boolean>,
      }
    }
  }
}
({
  requires: [],
  provides: {
    values: {
      "open-input-file": "tany",
      "open-output-file": "tany",
      "read-file-path": "tany",
      "read-file": "tany",
      "display": "tany",
      "flush-output-file": "tany",
      "file-times": "tany",
      "real-path": "tany",
      "exists": "tany",
      "close-output-file": "tany",
      "close-input-file": "tany",
      "create-dir": "tany",
      "list-files": "tany",
      "mtimes": "tany"
    }
  },
  nativeRequires: ["fs"],
  theModule: function(RUNTIME: Runtime, NAMESPACE, uri, fs: FS) {
    function InputFile(name: string, fd: number) {
      this.name = name;
      this.fd = fd;
    }

    function OutputFile(name: string, fd: number) {
      this.name = name;
      this.fd = fd;
    }

    // Use ASYNC file operations with Pyret
    // HowTo: https://www.pyret.org/docs/latest/s_running.html#%28part._.Asynchronous_.J.S_and_.Pyret%29
    const vals: Exports['dict']['values']['dict'] = {
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
      "read-file-path": RUNTIME.makeFunction(function(file) {
          RUNTIME.ffi.checkArity(1, arguments, "read-file", false);
          RUNTIME.checkString(file);
          return RUNTIME.pauseStack(function(restarter) {
            fs.readFile(file, {encoding: 'utf8'}, function(err, data) {
              // NOTE(alex): ignore errors for now
              restarter.resume(RUNTIME.makeString(data));
            });
          });
        }, "read-file-path"),
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
                }, function() {
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
              const fd = v.fd;
              return RUNTIME.pauseStack(function(restarter) {
                fs.fsync(fd, function(err) {
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
          
        }, "mtimes"),
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
              const fd = v.fd;
              return RUNTIME.pauseStack(function(restarter) {
                fs.close(fd, function(err) {
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
              const fd = v.fd;
              return RUNTIME.pauseStack(function(restarter) {
                fs.close(fd, function(err) {
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

