provide *

import builtin-modules as B
import js-file("../parse-pyret") as PP
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CS
import file("../js-of-pyret.arr") as JSP
import file("../file.arr") as F
import js-file("./../filelib") as FL
import pathlib as P

# Still unsure if just a path is the right input for this.
#data FileLocator:
#  | file-locator(path :: String, cenv :: CS.CompileEnv)
#    with:

fun mockable-file-locator(file-ops):
  lam(path, globals):
    var ast = nothing
    var mtime = nothing
    var real-path = nothing
    {
      path: path,
      globals: globals,
      method get-modified-time(self) block:
        when mtime == nothing:
          mtime := file-ops.mtimes(path).mtime
        end
        mtime
      end,
      method get-options(self, options):
        options
      end,
      method get-module(self) block:
        when ast == nothing block:
          when not(file-ops.file-exists(self.path)):
            raise("File " + self.path + " does not exist")
          end
          str = file-ops.read-file-path(self.path)
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
        CS.minimal-imports
      end,
      method get-globals(self): self.globals end,
      method set-compiled(self, cr, deps) block:
        ast := nothing
        nothing
      end,
      method needs-compile(self, provides):
        true
      end,
      method get-compiled(self, options):
        CL.arr-file(self.get-module(), self.get-extra-imports(), self.get-options(options))
      end,
      method uri(self) block:
        when real-path == nothing: real-path := file-ops.real-path(self.path) end
        "file://" + string-replace(real-path, P.path-sep, "/") end,
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
    mtimes: F.mtimes,
    real-path: F.real-path,
    read-file-path: FL.read-file-path
})
