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
    get-modified-time(self):
      file-ops.file-times(path).mtime
    end,
    get-options(self, options):
      options
    end,
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
      nothing
    end,
    needs-compile(self, provides):
        true
    end,
    get-compiled(self):
          none
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
