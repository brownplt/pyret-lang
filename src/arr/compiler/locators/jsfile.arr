provide *
import js-file("../builtin-modules") as B
import js-file("../ts-pathlib") as P
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CM
import file("../file.arr") as F

make-dep = CM.make-dep
convert-provides = CM.provides-from-raw-provides

fun make-jsfile-locator(path):
  raw = B.builtin-raw-locator(path)
  {
    method get-uncached(_): none end,
    method needs-compile(_, _): false end,
    method get-modified-time(self):
      F.file-times(path + ".arr.js").mtime
    end,
    method get-options(self, options):
      options.{ check-mode: false }
    end,
    method get-module(_):
      raise("Should never fetch source for builtin module " + path)
    end,
    method get-extra-imports(self):
      CM.minimal-imports
    end,
    method get-dependencies(_):
      deps = raw.get-raw-dependencies()
      raw-array-to-list(deps).map(make-dep)
    end,
    method get-native-modules(_):
      natives = raw.get-raw-native-modules()
      raw-array-to-list(natives).map(CM.requirejs)
    end,
    method get-globals(_):
      CM.standard-globals
    end,

    method uri(_): "jsfile://" + string-replace(P.resolve(path + ".arr"), P.path-sep, "/") end,
    method name(_): P.basename(path, "") end,

    method set-compiled(_, _, _): nothing end,
    method get-compiled(self, options):
      provs = convert-provides(self.uri(), {
        uri: self.uri(),
        modules: raw-array-to-list(raw.get-raw-module-provides()),
        values: raw-array-to-list(raw.get-raw-value-provides()),
        aliases: raw-array-to-list(raw.get-raw-alias-provides()),
        datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
      })
      CL.arr-js-file(provs, P.resolve(path + ".arr.json"), P.resolve(path + ".arr.js"))
    end,

    method _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end
