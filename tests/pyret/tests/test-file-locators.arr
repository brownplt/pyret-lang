import lists as L
import string-dict as SD
import load-lib as LL
import runtime-lib as R
import "compiler/compile-lib.arr" as CL
import "compiler/compile-structs.arr" as CM
import "compiler/locators/file.arr" as FL
import "compiler/locators/builtin.arr" as BL

fun worklist-contains-checker(wlist :: List<CM.ToCompile>):
  locs = wlist.map(_.locator)
  lam(loc :: CL.Locator): locs.member(loc) end
end

data Box:
  | box(ref v)
end

counter =
  block:
    var count = 0
    lam():
      c = count
      count := count + 1
      c
    end
  end

fun file-object(time): {
    atime: box(time),
    ctime: time,
    mtime: box(time),
    contents: box(""),
    file-times(self):
      {
        mtime: self.mtime!v,
        atime: self.atime!v,
        ctime: self.ctime
      }
    end,
    read-file(self):
      self.atime!{v : counter()}
      self.contents!v
    end,
    display(self, str):
      self.mtime!{v : counter()}
      self.contents!{v : str}
    end,
    close-file(self): nothing end
} end

fun make-file-ops(): {
    file-map: SD.make-mutable-string-dict(),
    input-file(self, path): self.file-map.get-value-now(path) end,
    output-file(self, path):
      cases (Option) self.file-map.get-now(path):
        | none =>
          fp = file-object(counter())
          self.file-map.set-now(path, fp)
          fp
        | some(fp) => fp
      end
    end,
    file-exists(self, path): self.file-map.has-key-now(path) end,
    file-times(self, path): self.file-map.get-value-now(path).file-times() end
} end

check "File locators":
  fops = make-file-ops()
  file-loc = FL.mockable-file-locator(fops)

  foo = fops.output-file("foo")
  foo.display(
    ```
    provide { f: f } end
    import file("bar") as B

    fun f(x): B.g(x) end
    f(42) 
    ```)
  foo.close-file()
  bar = fops.output-file("bar")
  bar.display(
    ```
    provide { g: g } end

    fun g(x): x end
    ```)
  bar.close-file()

  fun dfind(ctxt, dep):
    l = cases(CM.Dependency) dep:
      | builtin(modname) =>
        BL.make-builtin-locator(modname)
      | else =>
        file-loc(dep.arguments.get(0), CM.standard-globals)
    end
    CL.located(l, nothing)
  end

  floc = file-loc("foo", CM.standard-globals)
  CL.get-dependencies(floc.get-module(), floc.uri()) is [list: CM.dependency("file", [list: "bar"])]
  wlist = CL.compile-worklist(dfind, floc, {})
  wlist.length() is 12
  wlist.get(11).locator is floc
  wlist.get(10).locator is file-loc("bar", CM.standard-globals)

  ans = CL.compile-and-run-worklist(wlist, R.make-runtime(), CM.default-compile-options)
  ans.v satisfies LL.is-success-result
end
