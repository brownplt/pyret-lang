provide {
  make-builtin-locator: make-builtin-locator
} end
import namespace-lib as N
import builtin-modules as B
import string-dict as SD
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM

mtd = [SD.string-dict:]

# NOTE(joe): These conversions are done in Pyret-land because
# "builtin-modules" in JS is as spartan as possible to make module load-order
# dependencies as painless as possible; if importing builtin modules required
# access to compile-lib and sets and so on, it would be very difficult to
# bootstrap things.  So make-dep and make-provides handle this transition

fun make-dep(raw-dep):
  print(raw-dep.name)
  print(torepr(raw-dep))
  if raw-dep.import-type == "builtin":
    CM.builtin(raw-dep.name)
  else:
    CM.dependency(raw-dep.protocol, raw-array-to-list(raw-dep.args))
  end
end

fun const-dict<a>(strs :: List<String>, val :: a) -> SD.StringDict<a>:
  for fold(d from mtd, s from strs):
    d.set(s, val)
  end
end

fun make-provides(raw-provides):
  CM.provides(const-dict(raw-array-to-list(raw-provides), CM.v-just-there), mtd)
end

fun needs-includes(modname):
  [list:
    "checker"
    ].member(modname)
end

fun make-builtin-locator(builtin-name :: String) -> CL.Locator:
  raw = B.builtin-raw-locator(builtin-name)  
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
    get-provides(_):
      provides = raw.get-raw-provides()
      make-provides(provides) 
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
    get-compiled(_): some(CL.pre-loaded(raw.get-raw-compiled())) end,

    _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

