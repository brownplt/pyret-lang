provide *

import either as E
import option as O
import pprint as PP
import ast as A
import file as F
import string-dict as SD
import file("../test-parse-helper.arr") as TPH

fun map-files-in-dir-rec<A>(dir :: String, func :: (String -> Option<A>)) -> SD.StringDict<A> block:
  acc = [SD.mutable-string-dict: ]
  fun help(path):
    for each(f from F.list-files(path)):
      shadow path = path + "/" + f
      if F.is-file(path):
        cases(O.Option) func(path):
          | none => nothing
          | some(ans) => acc.set-now(path, ans)
        end
      else if F.is-dir(path):
        help(path)
      else:
        nothing
      end
    end
  end
  help(dir)
  acc.freeze()
end

fun keep-arr-files(path :: String) -> Option<String>:
  if string-ends-with(path, ".arr"): some(path) else: none end
end

files = map-files-in-dir-rec(".", keep-arr-files).keys-list()

fix-numbers = A.default-map-visitor.{
  method s-frac(self, l, num, den): A.s-num(l, num / den) end,
  method s-rfrac(self, l, num, den): A.s-num(l, num-to-roughnum(num / den)) end
}

check:
  for each(k from files) block:
    print(k + "...\n")
    src-str = F.file-to-string(k)
    cases(E.Either) TPH.maybe-parse(src-str) block:
      | left(_) => nothing
      | right(ast1) =>
        shadow ast1 = ast1.visit(fix-numbers)
        tosource = ast1.tosource()
        for each(len from [list: 40, 80, 160]):
          re-src-str = tosource.pretty(len).join-str("\n")
          cases(E.Either) TPH.maybe-parse(re-src-str) block:
            | left(err) =>
              err is ast1
            | right(ast2) =>
              shadow ast2 = ast2.visit(fix-numbers)
              ast1.visit(A.dummy-loc-visitor) is-roughly ast2.visit(A.dummy-loc-visitor)
          end
        end
    end
  end
end

