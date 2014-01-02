#lang pyret

import filelib as F
import compile as C

base = "../src/tests/pyret/"
files = F.read-dir(base)

fun split(s, l):
  ix = s.index-of(l)
  if ix == -1:
    [s]
  else:
    link(s.substring(0, ix), split(s.substring(ix + 1, s.length()), l))
  end
where:
  split("a,b,c", ",") is ["a","b","c"]
end

fun filter-map-k(f, lst-initial, k):
  fun map-k-help(lst, answer):
    cases(List) lst:
      | empty => k(answer.reverse())
      | link(fst, rst) =>
        f(fst, fun(maybe-v):
            cases(Option) maybe-v:
              | none => map-k-help(rst, answer)
              | some(v) => map-k-help(rst, link(v, answer))
            end
          end)
    end
  end
  map-k-help(lst-initial, [])
end

fun done(answers):
  checkers.clear-results()
  for each(a from answers):
    checkers.run-checks([{
        location: { file: "done-checker", line: "0", column: "0" },
        name: "done-checker",
        run: fun():
          eq = checkers.check-equals
          eq(a.file + " tests", {
              total: a.summary.total,
              passed: a.summary.passed,
              failed: a.summary.failed,
              test-errors: a.summary.test-errors,
              other-errors: a.summary.other-errors
            }, a.expected) 
        end
      }])
  end
  checkers.get-results(nothing).format()
end

fun is-test-file(f):
  F.is-file(f) and f.contains(".arr")
end

for filter-map-k(
    f from files.map(base + _).filter(is-test-file),
    k from done
    ):
  contents = F.read-file(f)

  C.compile(contents, "normal", fun(compiled):
    print("\n\nTesting " + f + "\n\n")
    answer = C.eval(compiled, C.mooringsNamespace)
    answer.format()
    summary = checkers.check-results-summary(answer.results)
    expected-line = split(contents, "\n").get(1)
    expected-list = split(expected-line.substring(1, expected-line.length()), ",")
      .map(_.tonumber())
    if (expected-list.length() <> 4):
      k(none)
    else:
      expected = {
          total: expected-list.get(0),
          passed: expected-list.get(1),
          failed: expected-list.get(2),
          test-errors: expected-list.get(3),
          other-errors: expected-list.get(4)
        }
      k(some({
          file: f,
          summary: summary,
          expected: expected
        }))
    end
  end)
end

