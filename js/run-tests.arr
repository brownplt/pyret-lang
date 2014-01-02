#lang pyret

import filelib as F
import compile as C

base = "../src/tests/pyret/errors/"
files = F.read-dir(base)

print(files)

fun split(s, l):
  ix = s.index-of(l)
  if ix == -1:
    [s]
  else:
    link(s.substring(0, ix), split(s.substring(ix + 1, s.length()), l))
  end
where:
  ",".substring(0, 0) is ","
  "a,b,c".index-of(",") is 1
end

fun map-k(f, lst-initial, k):
  fun map-k-help(lst, answer):
    print(lst)
    cases(List) lst:
      | empty => k(answer.reverse())
      | link(fst, rst) =>
        print(fst)
        f(fst, fun(v): map-k-help(rst, link(v, answer));)
    end
  end
  print(lst-initial)
  map-k-help(lst-initial, [])
end

fun done(answers):
  print("Done, answers are")
  checkers.clear-results()
  for each(a from answers):
    print(a.{ summary: a.summary.{message: ""}})
    checkers.run-checks([{
        location: { file: "done-checker", line: "0", column: "0" },
        name: "done-checker",
        run: fun():
          eq = checkers.check-equals
          eq(a.file + " total tests", {
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

for map-k(
    f from files.map(base + _).filter(F.is-file).filter(_.contains(".arr")),
    k from done
    ):
  print(f)
  contents = F.read-file(f)

  C.compile(contents, "normal", fun(compiled):
    print("compiled")
    answer = C.eval(compiled, C.mooringsNamespace)
    summary = checkers.check-results-summary(answer.results)
    print(contents)
    print(contents^split("\n"))
    expected-line = split(contents, "\n").rest.first
    expected-list = split(expected-line.substring(1, expected-line.length()), ",")
      .map(_.tonumber())
    print(expected-list)
    expected = {
        total: expected-list.get(0),
        passed: expected-list.get(1),
        failed: expected-list.get(2),
        test-errors: expected-list.get(3),
        other-errors: expected-list.get(4)
      }
    k({
        file: f,
        summary: summary,
        expected: expected
      })
  end)
end

