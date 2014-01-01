#lang pyret

import filelib as F
import compile as C

base = "../src/tests/pyret/"
files = F.read-dir(base)

print(files)

for each(f from files.map(base + _).filter(F.is-file).filter(_.contains(".arr"))):
  print(f)
  contents = F.read-file(f)

  C.compile(contents, "normal", fun(compiled):
    answer = C.eval(compiled, C.mooringsNamespace)
    print(answer.results)
  end)
end

