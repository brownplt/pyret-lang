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
  e6 satisfies E.is-generic-type-mismatch
  when E.is-generic-type-mismatch(e6):
    e6.val is 5
    e6.typ is "String"
  end

  e7 = get-err(lam(): 5 + "a";)
  e7 satisfies E.is-generic-type-mismatch
  when E.is-generic-type-mismatch(e7):
    e7.val is "a"
    e7.typ is "Number"
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
    e12.expected-arity is 1
    e12.args.length() is 2
    e12.fun-loc satisfies S.is-builtin
    e12.fun-loc.module-name is "num-tostring"
  end
  
  e13 = get-err(lam(): P.parse-dialect("missing", "argument") end)
  e13 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e13):
    e13.expected-arity is 3
    e13.args.length() is 2
    e13.fun-loc satisfies S.is-builtin
    e13.fun-loc.module-name is "parse-dialect"
  end
  
  e14 = get-err(lam(): P.surface-parse("too", "many", "arguments") end)
  e14 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e14):
    e14.expected-arity is 2
    e14.args.length() is 3
    e14.fun-loc satisfies S.is-builtin
    e14.fun-loc.module-name is "surface-parse"
  end
  
  e15 = get-err(lam(): P.parse-bootstrap("too", "many", "arguments") end)
  e15 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e15):
    e15.expected-arity is 2
    e15.args.length() is 3
    e15.fun-loc satisfies S.is-builtin
    e15.fun-loc.module-name is "parse-bootstrap"
  end
  
  e16 = get-err(lam(): F.format("too", "many", "arguments") end)
  e16 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e16):
    e16.expected-arity is 2
    e16.args.length() is 3
    e16.fun-loc satisfies S.is-builtin
    e16.fun-loc.module-name is "format"
  end
  
  e17 = get-err(lam(): W.big-bang("too", "many", "arguments") end)
  e17 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e17):
    e17.expected-arity is 2
    e17.args.length() is 3
    e17.fun-loc satisfies S.is-builtin
    e17.fun-loc.module-name is "big-bang"
  end
  
  e18 = get-err(lam(): W.is-world-config("too", "many", "arguments") end)
  e18 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e18):
    e18.expected-arity is 1
    e18.args.length() is 3
    e18.fun-loc satisfies S.is-builtin
    e18.fun-loc.module-name is "is-world-config"
  end
  
  e19 = get-err(lam(): W.is-key-equal("too", "many", "arguments") end)
  e19 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e19):
    e19.expected-arity is 2
    e19.args.length() is 3
    e19.fun-loc satisfies S.is-builtin
    e19.fun-loc.module-name is "is-key-equal"
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
    e21.expected-arity is 0
    e21.args.length() is 3
    e21.fun-loc satisfies S.is-builtin
    e21.fun-loc.module-name is "make-string-dict"
  end
  
  e22 = get-err(lam(): D.make-mutable-string-dict("too", "many", "arguments") end)
  e22 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e22):
    e22.expected-arity is 0
    e22.args.length() is 3
    e22.fun-loc satisfies S.is-builtin
    e22.fun-loc.module-name is "make-mutable-string-dict"
  end
  
  e23 = get-err(lam(): FL.open-input-file("too", "many", "arguments") end)
  e23 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e23):
    e23.expected-arity is 1
    e23.args.length() is 3
    e23.fun-loc satisfies S.is-builtin
    e23.fun-loc.module-name is "open-input-file"
  end
  
  e24 = get-err(lam(): FL.open-output-file("too", "many", "arguments") end)
  e24 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e24):
    e24.expected-arity is 2
    e24.args.length() is 3
    e24.fun-loc satisfies S.is-builtin
    e24.fun-loc.module-name is "open-output-file"
  end
  
  e25 = get-err(lam(): FL.read-file("too", "many", "arguments") end)
  e25 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e25):
    e25.expected-arity is 1
    e25.args.length() is 3
    e25.fun-loc satisfies S.is-builtin
    e25.fun-loc.module-name is "read-file"
  end
  
  e26 = get-err(lam(): FL.display("too", "many", "arguments") end)
  e26 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e26):
    e26.expected-arity is 2
    e26.args.length() is 3
    e26.fun-loc satisfies S.is-builtin
    e26.fun-loc.module-name is "display"
  end
  
  e27 = get-err(lam(): FL.close-output-file("too", "many", "arguments") end)
  e27 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e27):
    e27.expected-arity is 1
    e27.args.length() is 3
    e27.fun-loc satisfies S.is-builtin
    e27.fun-loc.module-name is "close-output-file"
  end

  e28 = get-err(lam(): FL.close-input-file("too", "many", "arguments") end)
  e28 satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(e28):
    e28.expected-arity is 1
    e28.args.length() is 3
    e28.fun-loc satisfies S.is-builtin
    e28.fun-loc.module-name is "close-input-file"
  end

  data-pred-arity = get-err(lam(): is-Data(1, 2) end)
  data-pred-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-pred-arity):
    data-pred-arity.expected-arity is 1
    data-pred-arity.args.length() is 2
    data-pred-arity.fun-loc satisfies S.is-srcloc
  end
  
  data-var1-arity = get-err(lam(): is-var1(1, 2) end)
  data-var1-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-var1-arity):
    data-var1-arity.expected-arity is 1
    data-var1-arity.args.length() is 2
    data-var1-arity.fun-loc satisfies S.is-srcloc
  end

  data-var2-arity = get-err(lam(): is-var2(1, 2) end)
  data-var2-arity satisfies E.is-arity-mismatch
  when E.is-arity-mismatch(data-var2-arity):
    data-var2-arity.expected-arity is 1
    data-var2-arity.args.length() is 2
    data-var2-arity.fun-loc satisfies S.is-srcloc
  end

  data-cases-var1-arity1 = get-err(
    lam():
      cases(Data) var1:
        | var1() => true
        | var2(_) => false
      end
    end)
  data-cases-var1-arity1 satisfies E.is-cases-singleton-mismatch
  when E.is-cases-singleton-mismatch(data-cases-var1-arity1):
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
  when E.is-cases-singleton-mismatch(data-cases-var2-arity1):
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
  when E.is-cases-singleton-mismatch(data-cases-var2-arity2):
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
  when E.is-cases-singleton-mismatch(data-cases-var2-arity3):
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
  when E.is-cases-singleton-mismatch(cases-miss):
    cases-miss.val is var2(5)
    cases-miss.loc satisfies S.is-srcloc
  end
end

