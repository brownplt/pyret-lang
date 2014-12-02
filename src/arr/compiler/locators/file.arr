import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CS
import file as F

# Still unsure if just a path is the right input for this, especially
# given the use of compile contexts to basically mean "all but the base
# file", but we'll see.
data FileLocator:
  | file-locator(path :: String, cenv :: CS.CompileEnv)
    with:
    get-module(self):
      when not(F.file-exists(self.path)):
        raise("File " + self.path + " does not exist")
      end
      f = F.input-file(self.path)
      CL.pyret-string(f.read-file())
    end,
    get-dependencies(self):
      CL.get-dependencies(self.get-module(), self.uri())
    end,
    get-provides(self):
      CL.get-provides(self.get-module(), self.uri())
    end,
    get-compile-env(self): self.cenv end,
    update-compile-context(self, oenv):
      # need to have utils for taking relative paths and
      # combining them with absolute paths and getting a new absolute path
      raise("Not implemented yet: need relative path handling")
    end,
    set-compiled(self, cr, deps): nothing end,
    get-compiled(self): none end,
    uri(self): "file://" + self.path end,
    name(self): self.path end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
end
