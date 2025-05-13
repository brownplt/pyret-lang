import filesystem as FS

s = "fairly unique string to test this filesystem test"

check:
  contents = FS.read-file-string("./tests/pyret/tests/test-filesystem.arr")
  contents satisfies string-contains(_, s)
end

