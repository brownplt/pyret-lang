provide *
import namespace-lib as N
import builtin-modules as B
import string-dict as SD
import file as F
import pathlib as P
import file("../compile-lib.arr") as CL
import file("../compile-structs.arr") as CM
import file("../type-structs.arr") as T
import file("../js-of-pyret.arr") as JSP

mtd = [SD.string-dict:]

# NOTE(joe): These conversions are done in Pyret-land because
# "builtin-modules" in JS is as spartan as possible to make module load-order
# dependencies as painless as possible; if importing builtin modules required
# access to compile-lib and sets and so on, it would be very difficult to
# bootstrap things.  So make-dep and make-provides handle this transition

fun make-dep(raw-dep):
 if raw-dep.import-type == "builtin":
    CM.builtin(raw-dep.name)
  else:
    CM.dependency(raw-dep.protocol, raw-array-to-list(raw-dep.args))
  end
end

fun convert-provides(uri, provides):
  CM.provides-from-raw-provides(uri, provides)
end

fun const-dict<a>(strs :: List<String>, val :: a) -> SD.StringDict<a>:
  for fold(d from mtd, s from strs):
    d.set(s, val)
  end
end

var builtin-js-dir = "src/js/trove/"
var builtin-arr-dir = "src/arr/trove/"

fun set-builtin-js-dir(s :: String):
  builtin-js-dir := s
end

fun set-builtin-arr-dir(s :: String):
  builtin-arr-dir := s
end

fun make-builtin-js-locator(basedir, builtin-name):
  raw = B.builtin-raw-locator(P.join(basedir, builtin-name))
  {
    needs-compile(_, _): false end,
    get-modified-time(self):
      F.file-times(P.join(basedir, builtin-name + ".js")).mtime
    end,
    get-options(self, options):
      options.{ check-mode: false }
    end,
    get-module(_): 
      raise("Should never fetch source for builtin module " + builtin-name)
    end,
    get-extra-imports(self):
      CM.standard-imports
    end,
    get-dependencies(_): 
      deps = raw.get-raw-dependencies()
      raw-array-to-list(deps).map(make-dep)
    end,
    get-native-modules(_):
      natives = raw.get-raw-native-modules()
      raw-array-to-list(natives).map(CM.requirejs)
    end,
    get-globals(_):
      raise("Should never get compile-env for builtin module " + builtin-name)
    end,
    get-namespace(_, some-runtime):
      N.make-base-namespace(some-runtime)
    end,

    uri(_): "builtin://" + builtin-name end,
    name(_): builtin-name end,

    set-compiled(_, _): nothing end,
    get-compiled(self):
      provs = convert-provides(self.uri(), {
        uri: self.uri(),
        values: raw-array-to-list(raw.get-raw-value-provides()),
        aliases: raw-array-to-list(raw.get-raw-alias-provides()),
        datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
      })
      some(CL.module-as-string(provs, CM.minimal-builtins, CM.ok(JSP.ccp-string(raw.get-raw-compiled()))))
    end,

    _equals(self, other, req-eq):
      req-eq(self.uri(), other.uri())
    end
  }
end

fun make-builtin-arr-locator(basedir, builtin-name):
  path = P.join(basedir, builtin-name + ".arr")
  {
    get-modified-time(self):
      F.file-times(path).mtime
    end,
    get-options(self, options):
      options.{ check-mode: false }
    end,
    get-module(self):
      when not(F.file-exists(path)):
        raise("File " + path + " does not exist")
      end
      f = F.input-file(path)
      str = CL.pyret-string(f.read-file())
      f.close-file()
      str
    end,
    get-namespace(self, runtime): N.make-base-namespace(runtime) end,
    get-dependencies(self):
      CL.get-dependencies(self.get-module(), self.uri())
    end,
    get-native-modules(self):
      [list:]
    end,
    get-extra-imports(self):
      CM.minimal-imports
    end,
    get-globals(self):
      CM.standard-globals
    end,
    set-compiled(self, cr, deps):
      cases(CM.CompileResult) cr.result-printer:
        | ok(ccp) =>
          cpath = path + ".js"
          f = F.output-file(cpath, false)
          f.display(ccp.pyret-to-js-runnable())
          f.close-file()
        | err(_) => nothing
      end
    end,
    needs-compile(self, provides):
      # does not handle provides from dependencies currently
      # NOTE(joe): Until we serialize provides correctly, just return false here
      cpath = path + ".js"
      if F.file-exists(path) and F.file-exists(cpath):
        stimes = F.file-times(path)
        ctimes = F.file-times(cpath)
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
          provs = convert-provides(self.uri(), {
            uri: self.uri(),
            values: raw-array-to-list(raw.get-raw-value-provides()),
            aliases: raw-array-to-list(raw.get-raw-alias-provides()),
            datatypes: raw-array-to-list(raw.get-raw-datatype-provides())
          })
          some(CL.module-as-string(provs, CM.minimal-builtins, CM.ok(JSP.ccp-string(raw.get-raw-compiled()))))
        else:
          none
        end
      else:
        none
      end
    end,
    uri(self): "builtin://" + builtin-name end,
    name(self): builtin-name end,
    _equals(self, other, eq): eq(self.uri(), other.uri()) end
  }
end

fun make-builtin-locator(builtin-name :: String) -> CL.Locator:
  ask:
    | F.file-exists(P.join(builtin-arr-dir, builtin-name + ".arr")) then:
      make-builtin-arr-locator(builtin-arr-dir, builtin-name)
    | F.file-exists(P.join(builtin-js-dir, builtin-name + ".js")) then:
      make-builtin-js-locator(builtin-js-dir, builtin-name)
    | otherwise:
      raise("Could not find module " + builtin-name + " in either of " + builtin-js-dir + " or " + builtin-arr-dir)
  end
end

