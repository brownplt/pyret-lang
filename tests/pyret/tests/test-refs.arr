data Box:
  | box(ref v)
end

check "basic boxes":
  b1 = box(5)
  b1!v is 5
  b1!{v : 10 }
  b1!v is 10

  b1 is b1

  b2 = box(5)
  (b2 == b1) is false
end

check "cyclic printing":
  torepr(box(5)) is "box(5)"
  b = box("dummy")
  b!{v : b}
  torepr(b) is "box(<cyclic-object-1>)"

  b2 = box("dummy")
  ref-set(b2.v, b2.v)
  torepr(b2) is "box(<cyclic-ref-1>)"
end

check "alternative lookups":
  b = box(5)
  ref-get(b.v) is 5

  b2 = { x: 5 }
  b2.x is 5
end

data MultiBox:
  | mbox(ref v1, ref v2, ref v3, v4)
end

check "cases":
  bx = box(5)
  contents = cases(Box) bx:
    | box(ref x) => x
  end
  contents is 5

  bx!{v:10}
  contents2 = cases(Box) bx:
    | box(ref x) => x
  end
  contents2 is 10

  mb = mbox(1, 2, 3, 4)
  contents3 = cases(MultiBox) mb:
    | mbox(ref a, ref b, ref c, d) => [list: a, b, c, d]
  end
  contents3 is [list: 1, 2, 3, 4]

  mb!{v1: "v1"}
  contents4 = cases(MultiBox) mb:
    | mbox(ref a, ref b, ref c, d) => [list: a, b, c, d]
  end
  contents4 is [list: "v1", 2, 3, 4]

end


check "update multiple":
  m = mbox(1, 2, 3, 4)
  m!v1 is 1
  m!v2 is 2
  m!v3 is 3
  m.v4 is 4

  m!{v1: "v1", v2: "v2", v3: "v3"}
  m!v1 is "v1"
  ref-get(m.v1) is "v1"
  m!v2 is "v2"
  m!v3 is "v3"
  m.v4 is 4
end

check "update errors":
  m = mbox(1, 2, 3, 4)
  m!{v4: 10} raises "non-ref"
  m!{v1: "v1", v4: 10} raises "non-ref"
  # Value should not have changed
  m!v1 is 1
  m!v2 is 2
  m!v3 is 3
  m.v4 is 4
end

check "extend errors":
  m = mbox("a", "b", "c", "d") 
  m.{ v1: "new" } raises "update"
end

check "lookup error":
  m = mbox(1, 2, 3, 4)
  m!v4 raises "non-ref"
end

