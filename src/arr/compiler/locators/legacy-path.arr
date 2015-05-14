provide {
  legacy-path-locator: legacy-path-locator
} end
import namespace-lib as N
import legacy-path as P
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS

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

fun legacy-path-locator(builtin-name :: String) -> CL.Locator:
  raw = P.legacy-path-raw-locator(builtin-name)  
  {
    needs-compile(_, _): false end,
    get-module(_): 
      raise("Should never fetch source for legacy module " + builtin-name)
    end,
    get-extra-imports(self):
      CS.standard-imports
    end,
    get-dependencies(self):
      raw-deps = raw.get-raw-dependencies()
      mod-deps = raw-array-to-list(raw-deps).map(make-dep)
      env-deps = for map(e from self.get-extra-imports().imports):
        e.dependency
      end
      mod-deps.append(env-deps)
    end,
    get-provides(_):
      provides = raw.get-raw-provides()
      make-provides(provides) 
    end,
    get-compile-env(_):
      raise("Should never get compile-env for legacy module " + builtin-name)
    end,
    get-namespace(_, some-runtime):
      N.make-base-namespace(some-runtime)
    end,

    uri(_): "legacy-path://" + builtin-name end,
    name(_): builtin-name end,
    
    set-compiled(_, _): nothing end,
    get-compiled(_): some(CL.pre-loaded(raw.get-raw-compiled())) end,

    _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

