provide {
  make-builtin-locator: make-builtin-locator,
  set-builtin-js-dir: set-builtin-js-dir,
  set-builtin-arr-dir: set-builtin-arr-dir
} end
import namespace-lib as N
import builtin-modules as B
import string-dict as SD
import file as F
import pathlib as P
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CM
import file("../type-structs.arr") as T
import file("../js-of-pyret.arr") as JSP

mtd = [SD.string-dict:]

# NOTE(joe): These conversions are done in Pyret-land because
# "builtin-modules" in JS is as spartan as possible to make module load-order
# dependencies as painless as possible; if importing builtin modules required
# access to compile-lib and sets and so on, it would be very difficult to
# bootstrap things.  So make-dep and make-provides handle this transition

fun make-dep(raw-dep):
 if raw-dep.import-type == "builtin":
    CM.builtin(raw-dep.name)
  else:
    CM.dependency(raw-dep.protocol, raw-array-to-list(raw-dep.args))
  end
end

fun convert-provides(uri, provides):
  CM.provides-from-raw-provides(uri, provides)
end

fun const-dict<a>(strs :: List<String>, val :: a) -> SD.StringDict<a>:
  for fold(d from mtd, s from strs):
    d.set(s, val)
  end
end

var builtin-js-dir = "src/js/trove/"
var builtin-arr-dir = "src/arr/trove/"

fun set-builtin-js-dir(s :: String):
  builtin-js-dir := s
end

fun set-builtin-arr-dir(s :: String):
  builtin-arr-dir := s
end

fun make-builtin-js-locator(basedir, builtin-name):
  raw = B.builtin-raw-locator(P.join(basedir, builtin-name))
  {
    needs-compile(_, _): false end,
    get-module(_): 
      raise("Should never fetch source for builtin module " + builtin-name)
    end,
    get-extra-imports(self):
      CM.standard-imports
    end,
    get-dependencies(_): 
      deps = raw.get-raw-dependencies()
      raw-array-to-list(deps).map(make-dep)
    end,
    get-globals(_):
      raise("Should never get compile-env for builtin module " + builtin-name)
    end,
    get-namespace(_, some-runtime):
      N.make-base-namespace(some-runtime)
    end,

    uri(_): "pyret-builtin://" + builtin-name end,
    name(_): builtin-name end,

    set-compiled(_, _): nothing end,
    get-compiled(self):
      provs = convert-provides(self.uri(), {
        uri: self.uri(),
        values: raw-array-to-list(raw.get-raw-value-provides()),
        aliases: raw-array-to-list(raw.get-raw-alias-provides()),
        datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
      })
      some(CL.module-as-string(provs, CM.minimal-builtins, CM.ok(JSP.ccp-string(raw.get-raw-compiled()))))
    end,

    _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

fun make-builtin-arr-locator(basedir, builtin-name):
  {
    get-module(self):
      when not(F.file-exists(self.path)):
        raise("File " + self.path + " does not exist")
      end
      f = F.input-file(self.path)
      str = CL.pyret-string(f.read-file())
      f.close-file()
      str
    end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    get-dependencies(self):
      CL.get-dependencies(self.get-module(), self.uri())
    end,
    get-extra-imports(self):
      CM.minimal-imports
    end,
    get-globals(self):
      CM.standard-globals
    end,
    set-compiled(self, cr, deps): nothing end,
    needs-compile(self, provides): true end,
    get-compiled(self): none end,
    uri(self): "builtin://" + builtin-name end,
    name(self): builtin-name end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
  }
end

fun make-builtin-locator(builtin-name :: String) -> CL.Locator:
  ask:
    | F.file-exists(P.join(builtin-arr-dir, builtin-name + ".arr")) then:
      make-builtin-arr-locator(builtin-arr-dir, builtin-name)
    | F.file-exists(P.join(builtin-js-dir, builtin-name + ".js")) then:
      make-builtin-js-locator(builtin-js-dir, builtin-name)
    | otherwise:
      raise("Could not find module " + builtin-name + " in either of " + builtin-js-dir + " or " + builtin-arr-dir)
  end
end

