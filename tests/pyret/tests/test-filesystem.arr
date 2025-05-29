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

check:
    FS.relative("/a/b/c", "/d/e/f") is "../../../d/e/f"
    FS.relative("a/b/c", "d/e/f") is "../../../d/e/f"
    FS.relative(FS.resolve("a/b/c"), FS.resolve("d/e/f")) is "../../../d/e/f"
    FS.relative("a/b/c", "file.arr") is "../../../file.arr"
    FS.relative(".", "a/b/c/file.arr") is "a/b/c/file.arr"
    FS.relative("a", "a/b/c/file.arr") is "b/c/file.arr"
    FS.relative("a/b", "a/b/c/file.arr") is "c/file.arr"

    FS.relative("a/b/c", "a/b/file.arr") is "../file.arr"
end

check:
    "/" satisfies FS.is-absolute
    "/a/b/c" satisfies FS.is-absolute
    "/../a/b/c" satisfies FS.is-absolute
    "/a/../c/d" satisfies FS.is-absolute
    "../../../../../../../../.." violates FS.is-absolute
    "." violates FS.is-absolute
    ".." violates FS.is-absolute
    "a/b/c" violates FS.is-absolute
    "./a/b/c" violates FS.is-absolute
end

check:
    FS.basename("/a/b/c") is "c"
    FS.basename("/a/b/c.arr") is "c.arr"
    FS.basename("rel/dir/c.arr") is "c.arr"
    FS.basename("a") is "a"

    FS.dirname("a") is "."
    FS.dirname(".") is "."
    FS.dirname("./a/b/c/file.txt") is "./a/b/c"
end