import filesystem as FS

s = "fairly unique string to test this filesystem test"

check:
  contents = FS.read-file-string("./tests/pyret/tests/test-filesystem.arr")
  contents satisfies string-contains(_, s)

  p = FS.resolve("./tests/../tests/pyret/tests/./test-file.arr")
  expected = [list: "", "tests", "pyret", "tests", "test-file.arr"].join-str("/")
  l = string-split(p, expected)
  l.length() is 2
  l.get(1) is ""
  FS.exists(p) is true
  p2 = "./non-existing-file"
  FS.exists(p2) is false
end

