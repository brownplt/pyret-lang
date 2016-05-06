import pathlib as P

# Assumes sep is '/' for right now
check:
  p = "./foo/../bar/./baz//quux"
  P.normalize(p) is [list: "bar", "baz", "quux"].join-str(P.path-sep)
  p1 = "foo"
  p2 = "bar"
  P.join(p1, p2) is [list: "foo", "bar"].join-str(P.path-sep)
  p3 = "/etc"
  p4 = "/etc/passwd"
  P.relative(p3, p4) is "passwd"
  p5 = "./foo/bar.arr"
  P.dirname(p5) is "./foo"
  P.extname(p5) is ".arr"
  P.basename(p5, ".arr") is "bar"
  P.basename(p5, "") is "bar.arr"
end
