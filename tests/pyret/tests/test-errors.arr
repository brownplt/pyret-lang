provide *

import error as E
import either as Eth
import srcloc as S
import format as F
import parse-pyret as P
import world as W
import string-dict as D
import filelib as FL

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

  e = get-err(lam(): {}.x;)
  e satisfies E.is-field-not-found
  when E.is-field-not-found(e):
    e.field is "x"
    e.obj is {}
  end

  e1 = get-err(lam(): 5.x;)
  e1 satisfies E.is-lookup-non-object
  when E.is-lookup-non-object(e1):
    e1.field is "x"
    e1.non-obj is 5
    e1.loc satisfies S.is-srcloc
  end


  e2 = get-err(lam(): "x".x;)
  e2 satisfies E.is-lookup-non-object
  when E.is-lookup-non-object(e2):
    e2.field is "x"
    e2.non-obj is "x"
    e2.loc satisfies S.is-srcloc
  end

  e3 = get-err(lam(): true.x;)
  e3 satisfies E.is-lookup-non-object
  when E.is-lookup-non-object(e3):
    e3.field is "x"
    e3.non-obj is true 
    e3.loc satisfies S.is-srcloc
  end
  
  e4 = get-err(lam(): "x".{x : false};)
  e4 satisfies E.is-extend-non-object
  when E.is-extend-non-object(e4):
    e4.non-obj is "x"
    e4.loc satisfies S.is-srcloc
  end

  e5 = get-err(lam(): true.{x : false};)
  e5 satisfies E.is-extend-non-object
  when E.is-extend-non-object(e5):
    e5.non-obj is true 
    e5.loc satisfies S.is-srcloc
  end

  e6 = get-err(lam(): "a" + 5;)
  e6 satisfies E.is-num-string-binop-error
  when E.is-num-string-binop-error(e6):
    e6.val1 is "a"
    e6.val2 is 5
  end

  e7 = get-err(lam(): 5 + "a";)
  e7 satisfies E.is-num-string-binop-error
  when E.is-num-string-binop-error(e7):
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
  when E.is-non-function-app(e10):
    e10.non-fun-val is 5
    e10.loc satisfies S.is-srcloc
  end

  e11 = get-err(lam(): 5(6, 7 + 8) end)
  e11 satisfies E.is-non-function-app
  when E.is-non-function-app(e11):
    e11.non-fun-val is 5
    e11.loc satisfies S.is-srcloc
  end

  e12 = get-err(lam(): num-tostring("two", "arguments") end)
  e12 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e12):
    e12.fun-def-arity is 1
    e12.fun-app-args.length() is 2
    e12.fun-def-loc satisfies S.is-builtin
    e12.fun-def-loc.module-name is "num-tostring"
  end
  
  e13 = get-err(lam(): P.surface-parse("missing argument") end)
  e13 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e13):
    e13.fun-def-arity is 2
    e13.fun-app-args.length() is 1
    e13.fun-def-loc satisfies S.is-builtin
    e13.fun-def-loc.module-name is "surface-parse"
  end
  
  e14 = get-err(lam(): P.surface-parse("too", "many", "arguments") end)
  e14 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e14):
    e14.fun-def-arity is 2
    e14.fun-app-args.length() is 3
    e14.fun-def-loc satisfies S.is-builtin
    e14.fun-def-loc.module-name is "surface-parse"
  end
  
  e16 = get-err(lam(): F.format("too", "many", "arguments") end)
  e16 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e16):
    e16.fun-def-arity is 2
    e16.fun-app-args.length() is 3
    e16.fun-def-loc satisfies S.is-builtin
    e16.fun-def-loc.module-name is "format"
  end
  
  e17 = get-err(lam(): W.big-bang("too", "many", "arguments") end)
  e17 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e17):
    e17.fun-def-arity is 2
    e17.fun-app-args.length() is 3
    e17.fun-def-loc satisfies S.is-builtin
    e17.fun-def-loc.module-name is "big-bang"
  end
  
  e18 = get-err(lam(): W.is-world-config("too", "many", "arguments") end)
  e18 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e18):
    e18.fun-def-arity is 1
    e18.fun-app-args.length() is 3
    e18.fun-def-loc satisfies S.is-builtin
    e18.fun-def-loc.module-name is "is-world-config"
  end
  
  e19 = get-err(lam(): W.is-key-equal("too", "many", "arguments") end)
  e19 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e19):
    e19.fun-def-arity is 2
    e19.fun-app-args.length() is 3
    e19.fun-def-loc satisfies S.is-builtin
    e19.fun-def-loc.module-name is "is-key-equal"
  end
  
  e20 = get-err(lam(): D.to-dict("too", "many", "arguments") end)
  e20 satisfies E.is-field-not-found
  when E.is-field-not-found(e20):
    e20.field is "to-dict"
    e20.obj is D
  end
  
  e21 = get-err(lam(): D.make-string-dict("too", "many", "arguments") end)
  e21 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e21):
    e21.fun-def-arity is 0
    e21.fun-app-args.length() is 3
    e21.fun-def-loc satisfies S.is-builtin
    e21.fun-def-loc.module-name is "make-string-dict"
  end
  
  e22 = get-err(lam(): D.make-mutable-string-dict("too", "many", "arguments") end)
  e22 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e22):
    e22.fun-def-arity is 0
    e22.fun-app-args.length() is 3
    e22.fun-def-loc satisfies S.is-builtin
    e22.fun-def-loc.module-name is "make-mutable-string-dict"
  end
  
  e23 = get-err(lam(): FL.open-input-file("too", "many", "arguments") end)
  e23 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e23):
    e23.fun-def-arity is 1
    e23.fun-app-args.length() is 3
    e23.fun-def-loc satisfies S.is-builtin
    e23.fun-def-loc.module-name is "open-input-file"
  end
  
  e24 = get-err(lam(): FL.open-output-file("too", "many", "arguments") end)
  e24 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e24):
    e24.fun-def-arity is 2
    e24.fun-app-args.length() is 3
    e24.fun-def-loc satisfies S.is-builtin
    e24.fun-def-loc.module-name is "open-output-file"
  end
  
  e25 = get-err(lam(): FL.read-file("too", "many", "arguments") end)
  e25 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e25):
    e25.fun-def-arity is 1
    e25.fun-app-args.length() is 3
    e25.fun-def-loc satisfies S.is-builtin
    e25.fun-def-loc.module-name is "read-file"
  end
  
  e26 = get-err(lam(): FL.display("too", "many", "arguments") end)
  e26 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e26):
    e26.fun-def-arity is 2
    e26.fun-app-args.length() is 3
    e26.fun-def-loc satisfies S.is-builtin
    e26.fun-def-loc.module-name is "display"
  end
  
  e27 = get-err(lam(): FL.close-output-file("too", "many", "arguments") end)
  e27 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e27):
    e27.fun-def-arity is 1
    e27.fun-app-args.length() is 3
    e27.fun-def-loc satisfies S.is-builtin
    e27.fun-def-loc.module-name is "close-output-file"
  end

  e28 = get-err(lam(): FL.close-input-file("too", "many", "arguments") end)
  e28 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e28):
    e28.fun-def-arity is 1
    e28.fun-app-args.length() is 3
    e28.fun-def-loc satisfies S.is-builtin
    e28.fun-def-loc.module-name is "close-input-file"
  end

  data-pred-arity = get-err(lam(): is-Data(1, 2) end)
  data-pred-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-pred-arity):
    data-pred-arity.fun-def-arity is 1
    data-pred-arity.fun-app-args.length() is 2
    data-pred-arity.fun-def-loc satisfies S.is-srcloc
  end
  
  data-var1-arity = get-err(lam(): is-var1(1, 2) end)
  data-var1-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-var1-arity):
    data-var1-arity.fun-def-arity is 1
    data-var1-arity.fun-app-args.length() is 2
    data-var1-arity.fun-def-loc satisfies S.is-srcloc
  end

  data-var2-arity = get-err(lam(): is-var2(1, 2) end)
  data-var2-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-var2-arity):
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
  data-cases-var1-arity1 satisfies E.is-cases-arity-mismatch
  when E.is-cases-arity-mismatch(data-cases-var1-arity1):
    data-cases-var1-arity1.branch-loc satisfies S.is-srcloc
  end

  data-cases-var2-arity1 = get-err(
    lam():
      cases(Data) var2(5):
        | var1 => true
        | var2 => false
      end
    end)
  data-cases-var2-arity1 satisfies E.is-cases-arity-mismatch
  when E.is-cases-arity-mismatch(data-cases-var2-arity1):
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
  when E.is-cases-arity-mismatch(data-cases-var2-arity2):
    data-cases-var2-arity2.num-args is 2
    data-cases-var2-arity2.actual-arity is 1
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
  when E.is-cases-arity-mismatch(data-cases-var2-arity3):
    data-cases-var2-arity3.num-args is 0
    data-cases-var2-arity3.actual-arity is 1
    data-cases-var2-arity3.branch-loc satisfies S.is-srcloc
  end

  cases-miss = get-err(
    lam():
      cases(Data) var2(5):
        | var1 => true
      end
    end)
  cases-miss satisfies E.is-no-cases-matched
  when E.is-cases-arity-mismatch(cases-miss):
    cases-miss.val is var2(5)
    cases-miss.loc satisfies S.is-srcloc
  end
end

