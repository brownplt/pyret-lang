provide {
  legacy-path-locator: legacy-path-locator
} end
import namespace-lib as N
import legacy-path as P
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CS
import string-dict as SD

mtd = [SD.string-dict:]

fun make-dep(raw-dep):
  if raw-dep.import-type == "builtin":
    CL.builtin(raw-dep.name)
  else:
    CL.dependency(raw-dep.protocol, raw-array-to-list(raw-dep.args))
  end
end

fun const-dict<a>(strs :: List<String>, val :: a) -> SD.StringDict<a>:
  for fold(d from mtd, s from strs):
    d.set(s, val)
  end
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
    get-globals(_):
      raise("Should never get-globals for legacy module " + builtin-name)
    end,
    get-namespace(_, some-runtime):
      N.make-base-namespace(some-runtime)
    end,

    uri(_): "legacy-path://" + builtin-name end,
    name(_): builtin-name end,
    
    set-compiled(_, _): nothing end,
    get-compiled(self):
      provs = CS.provides(self.uri(), mtd, mtd, mtd)
      some(CL.pre-loaded(provs, CS.minimal-builtins, raw.get-raw-compiled()))
    end,

    _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

