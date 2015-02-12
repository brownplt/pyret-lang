
import "compiler/compile.arr" as CM
import "compiler/compile-structs.arr" as CS

compile-str = lam(str):
  CM.compile-js(
          CM.start,
          "Pyret",
          str,
          "test",
          CS.standard-builtins,
          {
            check-mode : true,
            allow-shadowed : false,
            collect-all: false,
            type-check: false,
            show-steps: false,
            ignore-unbound: false
          }
          ).result
end

is-unbound-failure = lam(e): CS.is-err(e) and CS.is-unbound-id(e.problems.first) end
is-pointless-rec = lam(e): CS.is-err(e) and CS.is-pointless-rec(e.problems.first) end
is-ok = CS.is-ok

check "pointless":
  compile-str("rec _ = 5") satisfies is-pointless-rec
end

check "boundaries":
  no-propagate-through-let = ```
  rec x = lam(): y end
  z = 10
  rec y = lam(): x end
  ```
  compile-str(no-propagate-through-let) satisfies is-unbound-failure

  no-propagate-through-var = ```
  rec x = lam(): y end
  var z = 10
  rec y = lam(): x end
  ```
  compile-str(no-propagate-through-var) satisfies is-unbound-failure

  no-propagate-through-let-var = ```
  rec x = lam(): y end
  var z = 10
  y = lam(): x end
  ```
  compile-str(no-propagate-through-let-var) satisfies is-unbound-failure

  no-propagate-through-fun-let = ```
  rec x = lam(): y end
  fun f(): x end
  y = lam(): x end
  ```
  compile-str(no-propagate-through-fun-let) satisfies is-unbound-failure
end

check "recursive":
  propagate-through-data = ```
  rec x = lam() -> D: y + mt end
  data D: mt end
  rec y = lam(): x end
  ```
  compile-str(propagate-through-data) satisfies is-ok

  propagate-through-datas = ```
  rec x = lam() -> D2: y() + mt + mt2 end
  data D: mt end
  data D2: mt2 | mt3 end
  rec y = lam(): x end
  ```
  compile-str(propagate-through-data) satisfies is-ok

  propagate-through-fun = ```
  rec x = lam(): y end
  fun f(): y end
  rec y = lam(): x end
  ```
  compile-str(propagate-through-fun) satisfies is-ok

  propagate-through-funs = ```
  rec x = lam(): y end
  fun f(): y end
  fun g(): y end
  rec y = lam(): x end
  ```
  compile-str(propagate-through-fun) satisfies is-ok

  propagate-through-funs-and-data = ```
  rec x = lam() -> D: f() + mt + g() + y() end
  fun f(): y end
  data D: mt end
  fun g(): y end
  rec y = lam(): x end
  ```
  compile-str(propagate-through-fun) satisfies is-ok

end

