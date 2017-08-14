import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as H

is-ok = CS.is-ok

check "pointless":
  H.get-compile-errs("rec _ = 5").first satisfies CS.is-pointless-rec
end

check "boundaries":
  no-propagate-through-let = ```
  rec x = lam(): y end
  z = 10
  rec y = lam(): x end
  ```
  H.get-compile-errs(no-propagate-through-let).first satisfies CS.is-unbound-id

  no-propagate-through-var = ```
  rec x = lam(): y end
  var z = 10
  rec y = lam(): x end
  ```
  H.get-compile-errs(no-propagate-through-var).first satisfies CS.is-unbound-id

  no-propagate-through-let-var = ```
  rec x = lam(): y end
  var z = 10
  y = lam(): x end
  ```
  H.get-compile-errs(no-propagate-through-let-var).first satisfies CS.is-unbound-id

  no-propagate-through-fun-let = ```
  rec x = lam(): y end
  fun f(): x end
  y = lam(): x end
  ```
  H.get-compile-errs(no-propagate-through-fun-let).first satisfies CS.is-unbound-id
end

check "recursive":
  propagate-through-data = ```
  rec x = lam() -> D: y + mt end
  data D: mt end
  rec y = lam(): x end
  ```
  H.get-compile-errs(propagate-through-data) is empty

  propagate-through-datas = ```
  rec x = lam() -> D2: y() + mt + mt2 end
  data D: mt end
  data D2: mt2 | mt3 end
  rec y = lam(): x end
  ```
  H.get-compile-errs(propagate-through-data) is empty

  propagate-through-fun = ```
  rec x = lam(): y end
  fun f(): y end
  rec y = lam(): x end
  ```
  H.get-compile-errs(propagate-through-fun) is empty

  propagate-through-funs = ```
  rec x = lam(): y end
  fun f(): y end
  fun g(): y end
  rec y = lam(): x end
  ```
  H.get-compile-errs(propagate-through-fun) is empty

  propagate-through-funs-and-data = ```
  rec x = lam() -> D: f() + mt + g() + y() end
  fun f(): y end
  data D: mt end
  fun g(): y end
  rec y = lam(): x end
  ```
  H.get-compile-errs(propagate-through-fun) is empty

end

