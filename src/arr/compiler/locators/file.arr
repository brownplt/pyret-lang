provide *

import builtin-modules as B
import parser-interface as PP
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
  lam(path, globals):
    var ast = nothing
    {
      path: path,
      globals: globals,
      method get-modified-time(self):
        file-ops.file-times(path).mtime
      end,
      method get-options(self, options):
        options
      end,
      method get-module(self) block:
        when ast == nothing block:
          when not(file-ops.file-exists(self.path)):
            raise("File " + self.path + " does not exist")
          end
          f = file-ops.input-file(self.path)
          str = f.read-file()
          f.close-file()
          ast := CL.pyret-ast(PP.surface-parse(str, self.uri()))
        end
        ast
      end,
      method get-dependencies(self):
        CL.get-standard-dependencies(self.get-module(), self.uri())
      end,
      method get-native-modules(self):
        [list:]
      end,
      method get-extra-imports(self):
        CS.standard-imports
      end,
      method get-globals(self): self.globals end,
      method set-compiled(self, cr, deps) block:
        ast := nothing
        nothing
      end,
      method needs-compile(self, provides):
        true
      end,
      method get-compiled(self):
        none
      end,
      method uri(self): "file://" + string-replace(file-ops.real-path(self.path), P.path-sep, "/") end,
      method name(self): P.basename(self.path, "") end,
      method _equals(self, other, eq): eq(self.uri(), other.uri()) end
    }
  end
end

file-locator = mockable-file-locator({
    input-file: F.input-file,
    output-file: F.output-file(_, false),
    file-exists: F.file-exists,
    file-times: F.file-times,
    real-path: F.real-path
})
