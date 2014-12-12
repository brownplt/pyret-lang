provide *

import namespace-lib as N
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS
import "compiler/js-of-pyret.arr" as JSP
import file as F

# Still unsure if just a path is the right input for this.
#data FileLocator:
#  | file-locator(path :: String, cenv :: CS.CompileEnv)
#    with:

fun mockable-file-locator(file-ops):
  lam(path, cenv): {
    path: path,
    cenv: cenv,
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
      CL.get-dependencies(self.get-module(), self.uri())
    end,
    get-provides(self):
      CL.get-provides(self.get-module(), self.uri())
    end,
    get-compile-env(self): self.cenv end,
    set-compiled(self, cr, deps):
      cases(CS.CompileResult) cr:
        | ok(ccp) =>
          cpath = self.path + ".js"
          f = file-ops.output-file(cpath)
          f.display(ccp.pyret-to-js-runnable())
          f.close-file()
        | err(_) => nothing
      end
    end,
    needs-compile(self, provides):
      # does not handle provides from dependencies currently
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
      cpath = self.path + ".js"
      if file-ops.file-exists(self.path) and file-ops.file-exists(cpath):
        stimes = file-ops.file-times(self.path)
        # open cpath and use methods on it to try to avoid the obvious race
        # conditions (though others surely remain)
        # otherwise we could just use needs-compile to decide whether to
        # fetch or not (since otherwise they're very similar)
        cfp = file-ops.input-file(cpath)
        ctimes = cfp.file-times(cpath)
        if ctimes.mtime > stimes.mtime:
          ret = some(JSP.ccp-string(cfp.read-file()))
          cfp.close-file()
          ret
        else:
          none
        end
      else:
        none
      end
    end,
    uri(self): "file://" + self.path end,
    name(self): self.path end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
  } end
end

file-locator = mockable-file-locator({
    input-file: F.input-file,
    output-file: F.output-file,
    file-exists: F.file-exists,
    file-times: F.file-times,
})
