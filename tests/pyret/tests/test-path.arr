import pathlib as P

# Assumes sep is '/' for right now
check:
  p = "./foo/../bar/./baz//quux"
  P.normalize(p) is "bar/baz/quux"
  p1 = "foo"
  p2 = "bar"
  P.join(p1, p2) is "foo/bar"
  p3 = "/etc"
  p4 = "/etc/passwd"
  P.relative(p3, p4) is "passwd"
end
