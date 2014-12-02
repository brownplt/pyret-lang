import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS
import "compiler/js-of-pyret.arr" as JSP
import file as F
import pathlib as P

# Still unsure if just a path is the right input for this, especially
# given the use of compile contexts to basically mean "all but the base
# file", but we'll see.
data FileLocator:
  | file-locator(path :: String, cenv :: CS.CompileEnv)
    with:
    get-module(self):
      fpath = P.join(self.cenv, self.path)
      when not(F.file-exists(fpath)):
        raise("File " + fpath + " does not exist")
      end
      f = F.input-file(fpath)
      str = CL.pyret-string(f.read-file())
      f.close-file()
      str
    end,
    get-dependencies(self):
      CL.get-dependencies(self.get-module(), self.uri())
    end,
    get-provides(self):
      CL.get-provides(self.get-module(), self.uri())
    end,
    get-compile-env(self): self.cenv end,
    update-compile-context(self, oenv):
      dir = P.dirname(P.join(self.cenv, self.path))
      P.normalize(P.join(dir, oenv))
    end,
    set-compiled(self, cr, deps):
      cases(CS.CompileResult) cr:
        | ok(ccp) =>
          cpath = P.join(self.cenv, self.path) + ".js"
          f = F.output-file(cpath)
          f.display(ccp.pyret-to-js-runnable())
          f.close-file()
        | err(_) => nothing
      end
    end,
    get-compiled(self):
      cpath = P.join(self.cenv, self.path) + ".js"
      if F.file-exists(cpath):
        some(JSP.ccp-string(F.file-to-string(cpath)))
      else:
        none
      end
    end,
    uri(self): "file://" + P.join(self.cenv, self.path) end,
    name(self): self.path end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
end
