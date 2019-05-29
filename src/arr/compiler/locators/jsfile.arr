provide *
import js-file("../builtin-modules") as B
import string-dict as SD
import pathlib as P
import file("./builtin.arr") as BL
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CM
import file("../file.arr") as F
import file("../type-structs.arr") as T
import file("../js-of-pyret.arr") as JSP

mtd = [SD.string-dict:]

make-dep = BL.make-dep
convert-provides = BL.convert-provides
const-dict = BL.const-dict

fun make-jsfile-locator(path):
  raw = B.builtin-raw-locator(path)
  {
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

    method uri(_): "jsfile://" + string-replace(F.real-path(path + ".arr"), P.path-sep, "/") end,
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
      CL.arr-js-file(provs, F.real-path(path + ".arr.json"), F.real-path(path + ".arr.js"))
    end,

    method _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end
