import either as E
import error as Err

check:
  table = table: foo, bar
    row: 1, 2
    row: 3, 4
  end

  cases(E.Either) run-task(lam(): table.row(1) end) block:
    | left(_) => false is true
    | right(exn) =>
      unwrapped = exn-unwrap(exn)
      unwrapped satisfies Err.is-row-length-mismatch
      
      unwrapped.render-reason() does-not-raise
  end
end

