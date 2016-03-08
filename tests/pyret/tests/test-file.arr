import filelib as F
import pathlib as P

check:
  f = F.open-input-file("./tests/pyret/tests/test-file.arr")
  times = F.file-times(f)
  times.ctime is%(_ <= _) times.mtime
  times.ctime is%(_ <= _) times.atime
  p = F.real-path("./tests/../tests/pyret/tests/./test-file.arr")
  expected = [list: "", "tests", "pyret", "tests", "test-file.arr"].join-str(P.path-sep)
  l = string-split(p, expected)
  l.length() is 2
  l.get(1) is ""
  F.exists(p) is true
  p2 = "./non-existing-file"
  F.exists(p2) is false
end
