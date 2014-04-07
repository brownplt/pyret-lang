provide *

import error as E
import srcloc as S

check:
  fun get-err(thunk):
    cases(Either) run-task(thunk):
      | left(v) => "not-error"
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
  e6.type is "String"

  e7 = get-err(fun(): 5 + "a";)
  e7 satisfies E.is-generic-type-mismatch
  e7.val is "a"
  e7.type is "Number"

#    e4 = get-err(fun(): string-append(5, "a") end)
#    e4 satisfies E.is-type-mismatch
#    e4.val is 5
#    e4.type is "String"

#    e5 = get-err(fun(): num-sqrt("b") end)
#    e5 satisfies E.is-type-mismatch
#    e5.val is "b"
#    e5.type is "Number"

  e8 = get-err(fun(): letrec x-unbound = x-unbound(): 5 end end)
  e8 satisfies E.is-uninitialized-id
  e8.name is "x-unbound"
  e8.loc satisfies S.is-srcloc

  e9 = get-err(fun(): letrec x = y-unbound, y-unbound = 10: x end end)
  e9 satisfies E.is-uninitialized-id
  e9.name is "y-unbound"
  e9.loc satisfies S.is-srcloc

end
