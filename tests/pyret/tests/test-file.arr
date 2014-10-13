import filelib as F

check:
  f = F.open-input-file("./tests/pyret/tests/test-file.arr")
  times = F.file-times(f)
  times.ctime is%(_ <= _) times.mtime
  times.ctime is%(_ <= _) times.atime
end
