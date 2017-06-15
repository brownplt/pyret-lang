provide *

import error as E
import either as Eth
import srcloc as S
import format as F
import parse-pyret as P
import string-dict as D
import filelib as FL
import system as SYS

data FreshData:
  | fresh-constr()
end

data Data:
  | var1
  | var2(x)
end


check:
  fun get-err(thunk):
    cases(Eth.Either) run-task(thunk):
      | left(v) => "The given test produced no error"
      | right(v) => exn-unwrap(v)
    end
  end

  e = get-err(lam(): {}.x end)
  e satisfies E.is-field-not-found
  when E.is-field-not-found(e) block:
    e.field is "x"
    e.obj is {}
  end

  e1 = get-err(lam(): 5.x end)
  e1 satisfies E.is-lookup-non-object
  when E.is-lookup-non-object(e1) block:
    e1.field is "x"
    e1.non-obj is 5
    e1.loc satisfies S.is-srcloc
  end


  e2 = get-err(lam(): "x".x end)
  e2 satisfies E.is-lookup-non-object
  when E.is-lookup-non-object(e2) block:
    e2.field is "x"
    e2.non-obj is "x"
    e2.loc satisfies S.is-srcloc
  end

  e3 = get-err(lam(): true.x end)
  e3 satisfies E.is-lookup-non-object
  when E.is-lookup-non-object(e3) block:
    e3.field is "x"
    e3.non-obj is true
    e3.loc satisfies S.is-srcloc
  end

  e4 = get-err(lam(): "x".{x : false} end)
  e4 satisfies E.is-extend-non-object
  when E.is-extend-non-object(e4) block:
    e4.non-obj is "x"
    e4.loc satisfies S.is-srcloc
  end

  e5 = get-err(lam(): true.{x : false} end)
  e5 satisfies E.is-extend-non-object
  when E.is-extend-non-object(e5) block:
    e5.non-obj is true
    e5.loc satisfies S.is-srcloc
  end

  e6 = get-err(lam(): "a" + 5 end)
  e6 satisfies E.is-num-string-binop-error
  when E.is-num-string-binop-error(e6) block:
    e6.val1 is "a"
    e6.val2 is 5
  end

  e7 = get-err(lam(): 5 + "a" end)
  e7 satisfies E.is-num-string-binop-error
  when E.is-num-string-binop-error(e7) block:
    e7.val1 is 5
    e7.val2 is "a"
  end

#    e4 = get-err(lam(): string-append(5, "a") end)
#    e4 satisfies E.is-type-mismatch
#    e4.val is 5
#    e4.typ is "String"

#    e5 = get-err(lam(): num-sqrt("b") end)
#    e5 satisfies E.is-type-mismatch
#    e5.val is "b"
#    e5.typ is "Number"

  e10 = get-err(lam(): 5() end)
  e10 satisfies E.is-non-function-app
  when E.is-non-function-app(e10) block:
    e10.non-fun-val is 5
    e10.loc satisfies S.is-srcloc
  end

  e11 = get-err(lam(): 5(6, 7 + 8) end)
  e11 satisfies E.is-non-function-app
  when E.is-non-function-app(e11) block:
    e11.non-fun-val is 5
    e11.loc satisfies S.is-srcloc
  end

  e12 = get-err(lam(): num-tostring("two", "arguments") end)
  e12 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e12) block:
    e12.fun-def-arity is 1
    e12.fun-app-args.length() is 2
    e12.fun-def-loc satisfies S.is-builtin
    e12.fun-def-loc.module-name is "num-tostring"
  end

  e13 = get-err(lam(): P.surface-parse("missing argument") end)
  e13 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e13) block:
    e13.fun-def-arity is 2
    e13.fun-app-args.length() is 1
    e13.fun-def-loc satisfies S.is-builtin
    e13.fun-def-loc.module-name is "surface-parse"
  end

  e14 = get-err(lam(): P.surface-parse("too", "many", "arguments") end)
  e14 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e14) block:
    e14.fun-def-arity is 2
    e14.fun-app-args.length() is 3
    e14.fun-def-loc satisfies S.is-builtin
    e14.fun-def-loc.module-name is "surface-parse"
  end

  e16 = get-err(lam(): F.format("too", "many", "arguments") end)
  e16 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e16) block:
    e16.fun-def-arity is 2
    e16.fun-app-args.length() is 3
    e16.fun-def-loc satisfies S.is-builtin
    e16.fun-def-loc.module-name is "format"
  end

  e20 = get-err(lam(): D.to-dict("too", "many", "arguments") end)
  e20 satisfies E.is-field-not-found
  when E.is-field-not-found(e20) block:
    e20.field is "to-dict"
    e20.obj is D
  end

  e21 = get-err(lam(): D.make-string-dict("too", "many", "arguments") end)
  e21 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e21) block:
    e21.fun-def-arity is 0
    e21.fun-app-args.length() is 3
    e21.fun-def-loc satisfies S.is-builtin
    e21.fun-def-loc.module-name is "make-string-dict"
  end

  e22 = get-err(lam(): D.make-mutable-string-dict("too", "many", "arguments") end)
  e22 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e22) block:
    e22.fun-def-arity is 0
    e22.fun-app-args.length() is 3
    e22.fun-def-loc satisfies S.is-builtin
    e22.fun-def-loc.module-name is "make-mutable-string-dict"
  end

  e23 = get-err(lam(): FL.open-input-file("too", "many", "arguments") end)
  e23 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e23) block:
    e23.fun-def-arity is 1
    e23.fun-app-args.length() is 3
    e23.fun-def-loc satisfies S.is-builtin
    e23.fun-def-loc.module-name is "open-input-file"
  end

  e24 = get-err(lam(): FL.open-output-file("too", "many", "arguments") end)
  e24 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e24) block:
    e24.fun-def-arity is 2
    e24.fun-app-args.length() is 3
    e24.fun-def-loc satisfies S.is-builtin
    e24.fun-def-loc.module-name is "open-output-file"
  end

  e25 = get-err(lam(): FL.read-file("too", "many", "arguments") end)
  e25 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e25) block:
    e25.fun-def-arity is 1
    e25.fun-app-args.length() is 3
    e25.fun-def-loc satisfies S.is-builtin
    e25.fun-def-loc.module-name is "read-file"
  end

  e26 = get-err(lam(): FL.display("too", "many", "arguments") end)
  e26 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e26) block:
    e26.fun-def-arity is 2
    e26.fun-app-args.length() is 3
    e26.fun-def-loc satisfies S.is-builtin
    e26.fun-def-loc.module-name is "display"
  end

  e27 = get-err(lam(): FL.close-output-file("too", "many", "arguments") end)
  e27 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e27) block:
    e27.fun-def-arity is 1
    e27.fun-app-args.length() is 3
    e27.fun-def-loc satisfies S.is-builtin
    e27.fun-def-loc.module-name is "close-output-file"
  end

  e28 = get-err(lam(): FL.close-input-file("too", "many", "arguments") end)
  e28 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e28) block:
    e28.fun-def-arity is 1
    e28.fun-app-args.length() is 3
    e28.fun-def-loc satisfies S.is-builtin
    e28.fun-def-loc.module-name is "close-input-file"
  end

  e29 = get-err(lam(): link(1) end)
  e29 satisfies E.is-constructor-arity-mismatch
  when E.is-constructor-arity-mismatch(e12) block:
    e29.fun-def-arity is 2
    e29.fun-app-args.length() is 1
    e29.constructor-name is "link"
    e29.fun-def-loc satisfies S.is-srcloc
  end

  data-pred-arity = get-err(lam(): is-Data(1, 2) end)
  data-pred-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-pred-arity) block:
    data-pred-arity.fun-def-arity is 1
    data-pred-arity.fun-app-args.length() is 2
    data-pred-arity.fun-def-loc satisfies S.is-srcloc
  end

  data-var1-arity = get-err(lam(): is-var1(1, 2) end)
  data-var1-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-var1-arity) block:
    data-var1-arity.fun-def-arity is 1
    data-var1-arity.fun-app-args.length() is 2
    data-var1-arity.fun-def-loc satisfies S.is-srcloc
  end

  data-var2-arity = get-err(lam(): is-var2(1, 2) end)
  data-var2-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-var2-arity) block:
    data-var2-arity.fun-def-arity is 1
    data-var2-arity.fun-app-args.length() is 2
    data-var2-arity.fun-def-loc satisfies S.is-srcloc
  end

  data-cases-var1-arity1 = get-err(
    lam():
      cases(Data) var1:
        | var1() => true
        | var2(_) => false
      end
    end)
  data-cases-var1-arity1 satisfies E.is-cases-singleton-mismatch
  when E.is-cases-singleton-mismatch(data-cases-var1-arity1) block:
    data-cases-var1-arity1.should-be-singleton is true
    data-cases-var1-arity1.branch-loc satisfies S.is-srcloc
  end

  data-cases-var2-arity1 = get-err(
    lam():
      cases(Data) var2(5):
        | var1 => true
        | var2 => false
      end
    end)
  data-cases-var2-arity1 satisfies E.is-cases-singleton-mismatch
  when E.is-cases-singleton-mismatch(data-cases-var2-arity1) block:
    data-cases-var2-arity1.should-be-singleton is false
    data-cases-var2-arity1.branch-loc satisfies S.is-srcloc
  end

  data-cases-var2-arity2 = get-err(
    lam():
      cases(Data) var2(5):
        | var1 => true
        | var2(_, _) => false
      end
    end)
  data-cases-var2-arity2 satisfies E.is-cases-arity-mismatch
  when E.is-cases-singleton-mismatch(data-cases-var2-arity2) block:
    data-cases-var2-arity2.num-args is 2
    data-cases-var2-arity2.actual-fields.length() is 1
    data-cases-var2-arity2.branch-loc satisfies S.is-srcloc
  end

  data-cases-var2-arity3 = get-err(
    lam():
      cases(Data) var2(5):
        | var1 => true
        | var2() => false
      end
    end)
  data-cases-var2-arity3 satisfies E.is-cases-arity-mismatch
  when E.is-cases-singleton-mismatch(data-cases-var2-arity3) block:
    data-cases-var2-arity3.num-args is 0
    data-cases-var2-arity3.actual-fields.length() is 1
    data-cases-var2-arity3.branch-loc satisfies S.is-srcloc
  end

  cases-miss = get-err(
    lam():
      cases(Data) var2(5):
        | var1 => true
      end
    end)
  cases-miss satisfies E.is-no-cases-matched
  when E.is-cases-singleton-mismatch(cases-miss) block:
    cases-miss.val is var2(5)
    cases-miss.loc satisfies S.is-srcloc
  end

  template-err = get-err(lam(): (lam(): ... end)() end)
  template-err satisfies E.is-template-not-finished
  when E.is-template-not-finished(template-err) block:
    template-err.loc satisfies S.is-srcloc
  end

  constr-err = get-err(lam(): fresh-constr.foo end)
  constr-err satisfies E.is-lookup-constructor-not-object

  exit-err-zero = get-err(lam(): SYS.exit(0) end)
  exit-err-zero satisfies E.is-exit

  exit-err-one = get-err(lam(): SYS.exit(1) end)
  exit-err-one satisfies E.is-exit

  exit-quiet-err-zero = get-err(lam(): SYS.exit-quiet(0) end)
  exit-quiet-err-zero satisfies E.is-exit-quiet

  exit-quiet-err-one = get-err(lam(): SYS.exit-quiet(1) end)
  exit-quiet-err-one satisfies E.is-exit-quiet

end
