import filelib as F

check:
  f = F.open-input-file("./tests/pyret/tests/test-file.arr")
  times = F.file-times(f)
  times.ctime is%(_ <= _) times.mtime
  times.ctime is%(_ <= _) times.atime
  p = F.real-path("./tests/../tests/pyret/tests/./test-file.arr")
  l = string-split(p, "/tests/pyret/tests/test-file.arr")
  l.length() is 2
  l.get(1) is ""
end
