provide *

import builtin-modules as B
import parse-pyret as PP
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CS
import file("../js-of-pyret.arr") as JSP
import file as F
import pathlib as P
import shared-gdrive as SGD

# Still unsure if just a path is the right input for this.
#data FileLocator:
#  | file-locator(path :: String, cenv :: CS.CompileEnv)
#    with:


shared-gdrive-file-locator = lam(filename, program-id, globals):
  var ast = nothing
  var modified-date = nothing
  {
    filename: filename,
    program-id: program-id,
    globals: globals,

    method get-modified-time(self) block:
      when modified-date == nothing:
        _ = self.get-module()
        modified-date
      end 
      
      modified-date
    end,
    
    method get-options(self, options):
      options
    end,
    
    method get-module(self) block:
      when ast == nothing block:

        program = SGD.get-shared-program(filename, program-id)
        
        ast := CL.pyret-ast(PP.surface-parse(program.src, self.uri()))
        modified-date := program.modifiedDate
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

    method uri(self): "shared-gdrive://" + filename end,

    method name(self): filename end,

    method _equals(self, other, eq): eq(self.uri(), other.uri()) end
  }
end
