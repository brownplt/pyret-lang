provide *

import builtin-modules as B
import namespace-lib as N
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CS
import file("../js-of-pyret.arr") as JSP
import file as F
import pathlib as P

# Still unsure if just a path is the right input for this.
#data FileLocator:
#  | file-locator(path :: String, cenv :: CS.CompileEnv)
#    with:

fun mockable-file-locator(file-ops):
  lam(path, globals): {
    path: path,
    globals: globals,
    get-module(self):
      when not(file-ops.file-exists(self.path)):
        raise("File " + self.path + " does not exist")
      end
      f = file-ops.input-file(self.path)
      str = CL.pyret-string(f.read-file())
      f.close-file()
      str
    end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    get-dependencies(self):
      CL.get-standard-dependencies(self.get-module(), self.uri())
    end,
    get-native-modules(self):
      [list:]
    end,
    get-extra-imports(self):
      CS.standard-imports
    end,
    get-globals(self): self.globals end,
    set-compiled(self, cr, deps):
      cases(CS.CompileResult) cr.result-printer:
        | ok(ccp) =>
          cpath = self.path + ".js"
          f = file-ops.output-file(cpath)
          f.display(ccp.pyret-to-js-runnable())
          f.close-file()
        | err(_) => nothing
      end
    end,
    needs-compile(self, provides):
      cpath = self.path + ".js"
      if file-ops.file-exists(self.path) and file-ops.file-exists(cpath):
        stimes = file-ops.file-times(self.path)
        ctimes = file-ops.file-times(cpath)
        ctimes.mtime <= stimes.mtime
      else:
        true
      end
    end,
    get-compiled(self):
      cpath = path + ".js"
      if F.file-exists(path) and F.file-exists(cpath):
        # NOTE(joe):
        # Since we're not explicitly acquiring locks on files, there is a race
        # condition in the next few lines – a user could potentially delete or
        # overwrite the original file for the source while this method is
        # running.  We can explicitly open and lock files with appropriate
        # APIs to mitigate this in the happy, sunny future.
        stimes = F.file-times(path)
        ctimes = F.file-times(cpath)
        if ctimes.mtime > stimes.mtime:
          raw = B.builtin-raw-locator(path)
          provs = CS.provides-from-raw-provides(self.uri(), {
            uri: self.uri(),
            values: raw-array-to-list(raw.get-raw-value-provides()),
            aliases: raw-array-to-list(raw.get-raw-alias-provides()),
            datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
          })
          some(CL.module-as-string(provs, CS.minimal-builtins, CS.ok(JSP.ccp-string(raw.get-raw-compiled()))))
        else:
          none
        end
      else:
        none
      end
    end,
    uri(self): "file://" + string-replace(F.real-path(self.path), P.path-sep, "/") end,
    name(self): self.path end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
  } end
end

file-locator = mockable-file-locator({
    input-file: F.input-file,
    output-file: F.output-file(_, false),
    file-exists: F.file-exists,
    file-times: F.file-times,
})
