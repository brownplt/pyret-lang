provide *

import error as E
import either as Eth

check:
  fun get-err(thunk):
    cases(Eth.Either) run-task(thunk):
      | left(v) => "not-error"
      | right(v) => exn-unwrap(v)
    end
  end

  "a" + "b" is "ab"
  4 + 5 is 9
  
  e1 = get-err(lam(): {} + "a" end)
  e1 satisfies E.is-num-string-binop-error
  e1.val1 is {}
  e1.val2 is "a"

  o = {
      arr: [list: 1,2,3],
      method _plus(self, other): for lists.map2(a1 from self.arr, a2 from other.arr): a1 + a2 end end
    }
  o2 = { arr: [list: 3,4,5] }
  o + o2 is [list: 4,6,8]

  e2 = get-err(lam(): true * 5 end)
  e2 satisfies E.is-numeric-binop-error
  e2.val1 is true
  e2.val2 is 5
  
  e3 = get-err(lam(): true - 5 end)
  e3 satisfies E.is-numeric-binop-error
  e3.val1 is true
  e3.val2 is 5
  
  e4 = get-err(lam(): true / 5 end)
  e4 satisfies E.is-numeric-binop-error
  e4.val1 is true
  e4.val2 is 5
  
end
