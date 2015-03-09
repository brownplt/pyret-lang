provide {
  make-builtin-locator: make-builtin-locator
} end
import namespace-lib as N
import builtin-modules as B
import "compiler/compile-lib.arr" as CL

# NOTE(joe): These conversions are done in Pyret-land because
# "builtin-modules" in JS is as spartan as possible to make module load-order
# dependencies as painless as possible; if importing builtin modules required
# access to compile-lib and sets and so on, it would be very difficult to
# bootstrap things.  So make-dep and make-provides handle this transition

fun make-dep(raw-dep):
  if raw-dep.import-type == "builtin":
    CL.builtin(raw-dep.name)
  else:
    CL.dependency(raw-dep.protocol, raw-array-to-list(raw-dep.args))
  end
end

fun make-provides(raw-provides):
  sets.list-to-list-set(raw-array-to-list(raw-provides))
end

fun make-builtin-locator(builtin-name :: String) -> CL.Locator:
  raw = B.builtin-raw-locator(builtin-name)  
  {
    needs-compile(_, _): false end,
    get-module(_): 
      raise("Should never fetch source for builtin module " + builtin-name)
    end,
    get-dependencies(_): 
      deps = raw.get-raw-dependencies()
      sets.list-to-list-set(raw-array-to-list(deps).map(make-dep))
    end,
    get-provides(_):
      provides = raw.get-raw-provides()
      make-provides(provides) 
    end,
    get-compile-env(_):
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

