provide *

import error as E
import srcloc as S
import format as F
import parse-pyret as P
import world as W
import string-dict as D
import filelib as FL

check:
  fun get-err(thunk):
    cases(Either) run-task(thunk):
      | left(v) => raise("no error")
      | right(v) => v
    end
  end

  e = get-err(fun(): {}.x;)
  e satisfies E.is-field-not-found
  e.field is "x"
  e.obj is {}

  e1 = get-err(fun(): 5.x;)
  e1 satisfies E.is-lookup-non-object
  e1.field is "x"
  e1.non-obj is 5
  e1.loc satisfies S.is-srcloc


  e2 = get-err(fun(): "x".x;)
  e2 satisfies E.is-lookup-non-object
  e2.field is "x"
  e2.non-obj is "x"
  e2.loc satisfies S.is-srcloc

  e3= get-err(fun(): true.x;)
  e3 satisfies E.is-lookup-non-object
  e3.field is "x"
  e3.non-obj is true 
  e3.loc satisfies S.is-srcloc

  e4 = get-err(fun(): true:x;)
  e4 satisfies E.is-lookup-non-object
  e4.field is "x"
  e4.non-obj is true 

  e5 = get-err(fun(): {}:x;)
  e5 satisfies E.is-field-not-found
  e5.field is "x"
  e5.obj is {}

  e6 = get-err(fun(): "a" + 5;)
  e6 satisfies E.is-generic-type-mismatch
  e6.val is 5
  e6.typ is "String"

  e7 = get-err(fun(): 5 + "a";)
  e7 satisfies E.is-generic-type-mismatch
  e7.val is "a"
  e7.typ is "Number"

#    e4 = get-err(fun(): string-append(5, "a") end)
#    e4 satisfies E.is-type-mismatch
#    e4.val is 5
#    e4.typ is "String"

#    e5 = get-err(fun(): num-sqrt("b") end)
#    e5 satisfies E.is-type-mismatch
#    e5.val is "b"
#    e5.typ is "Number"

  e10 = get-err(fun(): 5() end)
  e10 satisfies E.is-non-function-app
  e10.non-fun-val is 5
  e10.loc satisfies S.is-srcloc
  e10.args is [list: ]

  e11 = get-err(fun(): 5(6, 7 + 8) end)
  e11 satisfies E.is-non-function-app
  e11.non-fun-val is 5
  e11.loc satisfies S.is-srcloc
  e11.args is [list: 6, 15]

  e12 = get-err(fun(): num-tostring("two", "arguments") end)
  e12 satisfies E.is-arity-mismatch
  e12.expected-arity is 1
  e12.args.length() is 2
  e12.fun-loc satisfies S.is-builtin
  e12.fun-loc.module-name is "num-tostring"
  
  e13 = get-err(fun(): P.parse-dialect("missing", "argument") end)
  e13 satisfies E.is-arity-mismatch
  e13.expected-arity is 3
  e13.args.length() is 2
  e13.fun-loc satisfies S.is-builtin
  e13.fun-loc.module-name is "parse-dialect"
  
  e14 = get-err(fun(): P.surface-parse("too", "many", "arguments") end)
  e14 satisfies E.is-arity-mismatch
  e14.expected-arity is 2
  e14.args.length() is 3
  e14.fun-loc satisfies S.is-builtin
  e14.fun-loc.module-name is "surface-parse"
  
  e15 = get-err(fun(): P.parse-bootstrap("too", "many", "arguments") end)
  e15 satisfies E.is-arity-mismatch
  e15.expected-arity is 2
  e15.args.length() is 3
  e15.fun-loc satisfies S.is-builtin
  e15.fun-loc.module-name is "parse-bootstrap"

  e16 = get-err(fun(): F.format("too", "many", "arguments") end)
  e16 satisfies E.is-arity-mismatch
  e16.expected-arity is 2
  e16.args.length() is 3
  e16.fun-loc satisfies S.is-builtin
  e16.fun-loc.module-name is "format"
  
  e17 = get-err(fun(): W.big-bang("too", "many", "arguments") end)
  e17 satisfies E.is-arity-mismatch
  e17.expected-arity is 2
  e17.args.length() is 3
  e17.fun-loc satisfies S.is-builtin
  e17.fun-loc.module-name is "big-bang"
  
  e18 = get-err(fun(): W.is-world-config("too", "many", "arguments") end)
  e18 satisfies E.is-arity-mismatch
  e18.expected-arity is 1
  e18.args.length() is 3
  e18.fun-loc satisfies S.is-builtin
  e18.fun-loc.module-name is "is-world-config"
  
  e19 = get-err(fun(): W.is-key-equal("too", "many", "arguments") end)
  e19 satisfies E.is-arity-mismatch
  e19.expected-arity is 2
  e19.args.length() is 3
  e19.fun-loc satisfies S.is-builtin
  e19.fun-loc.module-name is "is-key-equal"
  
  e20 = get-err(fun(): D.to-dict("too", "many", "arguments") end)
  e20 satisfies E.is-arity-mismatch
  e20.expected-arity is 1
  e20.args.length() is 3
  e20.fun-loc satisfies S.is-builtin
  e20.fun-loc.module-name is "to-dict"
  
  e21 = get-err(fun(): D.immutable-string-dict("too", "many", "arguments") end)
  e21 satisfies E.is-arity-mismatch
  e21.expected-arity is 0
  e21.args.length() is 3
  e21.fun-loc satisfies S.is-builtin
  e21.fun-loc.module-name is "immutable-string-dict"
  
  e22 = get-err(fun(): D.string-dict("too", "many", "arguments") end)
  e22 satisfies E.is-arity-mismatch
  e22.expected-arity is 0
  e22.args.length() is 3
  e22.fun-loc satisfies S.is-builtin
  e22.fun-loc.module-name is "string-dict"
  
  e23 = get-err(fun(): FL.open-input-file("too", "many", "arguments") end)
  e23 satisfies E.is-arity-mismatch
  e23.expected-arity is 1
  e23.args.length() is 3
  e23.fun-loc satisfies S.is-builtin
  e23.fun-loc.module-name is "open-input-file"
  
  e24 = get-err(fun(): FL.open-output-file("too", "many", "arguments") end)
  e24 satisfies E.is-arity-mismatch
  e24.expected-arity is 2
  e24.args.length() is 3
  e24.fun-loc satisfies S.is-builtin
  e24.fun-loc.module-name is "open-output-file"
  
  e25 = get-err(fun(): FL.read-file("too", "many", "arguments") end)
  e25 satisfies E.is-arity-mismatch
  e25.expected-arity is 1
  e25.args.length() is 3
  e25.fun-loc satisfies S.is-builtin
  e25.fun-loc.module-name is "read-file"
  
  e26 = get-err(fun(): FL.display("too", "many", "arguments") end)
  e26 satisfies E.is-arity-mismatch
  e26.expected-arity is 2
  e26.args.length() is 3
  e26.fun-loc satisfies S.is-builtin
  e26.fun-loc.module-name is "display"

  e27 = get-err(fun(): FL.close-output-file("too", "many", "arguments") end)
  e27 satisfies E.is-arity-mismatch
  e27.expected-arity is 1
  e27.args.length() is 3
  e27.fun-loc satisfies S.is-builtin
  e27.fun-loc.module-name is "close-output-file"

  e28 = get-err(fun(): FL.close-input-file("too", "many", "arguments") end)
  e28 satisfies E.is-arity-mismatch
  e28.expected-arity is 1
  e28.args.length() is 3
  e28.fun-loc satisfies S.is-builtin
  e28.fun-loc.module-name is "close-input-file"

end

