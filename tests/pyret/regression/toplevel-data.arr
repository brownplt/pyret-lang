import file("../../../src/arr/compiler/compile-structs.arr") as CS
import file("../test-compile-helper.arr") as CH
import contracts as C
import load-lib as L
import either as E

compile-str = CH.compile-str
run-to-result = CH.run-to-result

run-str = lam(str): 
  result = run-to-result(str)
  cases(E.Either) result block:
    | left(err) =>
      { program: str, success: false, runtime: false, errors: err }
    | right(ans) =>
      if L.is-success-result(ans):
        { program: str, success: true, runtime: true, ans: ans }
      else:
        { program: str, success: false, runtime: true, errors: ans }
      end
  end
end

fun output(act, exp):
  if exp.success:
    act.success
  else:
    (exp.runtime == act.runtime) and exp.check-errors(act.errors)
  end
end

fun compile-error(check-err):
  {
    success: false,
    runtime: false,
    check-errors:
      lam(errors):
        for lists.any(err from errors):
          lists.any(check-err, err.problems)
        end
      end
  }
end

check:
  run-str(
    ```
    fun foo() block:
      data Oops:
        | bar
        | baz
      end
      bar
    end
    ```
    ) is%(output) compile-error(CS.is-non-toplevel)
end
