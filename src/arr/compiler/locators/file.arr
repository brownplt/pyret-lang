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
      spath = P.join(self.cenv, self.path)
      cpath = spath + ".js"
      if F.file-exists(spath) and F.file-exists(cpath):
        stimes = F.file-times(spath)
        # open cpath and use methods on it to try to avoid the obvious race
        # conditions (though others surely remain)
        cfp = F.input-file(cpath)
        ctimes = cfp.file-times(cpath)
        if ctimes.mtime > stimes.mtime:
          ret = some(JSP.ccp-string(cfp.read-file(cfp)))
          cfp.close-file()
          ret
        else:
          none
        end
      else:
        none
      end
    end,
    uri(self): "file://" + P.join(self.cenv, self.path) end,
    name(self): self.path end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
end
