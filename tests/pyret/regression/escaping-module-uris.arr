import file("../test-compile-helper.arr") as C
import load-lib as L
import either as E

fun run(name):
  C.run-to-result-named(```
  data Foo:
    | foo()
  end
  foo()
    ```, name)
end

check "SingleQuoteStringMod":
  res = run("'")
  res satisfies E.is-right
  res.v satisfies L.is-success-result
end

check "DoubleQuoteStringMod":
  res2 = run('"')
  res2 satisfies E.is-right
  res2.v satisfies L.is-success-result
end

check "EscapedDoubleQuoteStringMod":
  res = run("\\\"")
  res satisfies E.is-right
  res.v satisfies L.is-success-result
end

check "EscapedSingleQuoteStringMod":
  res = run("\\'")
  res satisfies E.is-right
  res.v satisfies L.is-success-result
end

check "TwoDoubleQuotes":
  res = run('""')
  res satisfies E.is-right
  res.v satisfies L.is-success-result
end

check "TwoSingleQuotes":
  res = run("''")
  res satisfies E.is-right
  res.v satisfies L.is-success-result
end
