#lang pyret


check:

  o = { mutable x: 5 }
  o!{ x: 10 } is nothing
  o!x is 10
  o.x raises 'Cannot look up mutable field "x" using dot or bracket'

  o2 = o.{ x: 15 }
  o2.x is 15
  o2!x is 15

  mut-value = o:x
  o3 = o.{ x: mut-value }
  o3!{ x: "o3-set"} is nothing
  o3!x is "o3-set"
  o!x is "o3-set"

end


data D:
  | no-ann(mutable x)
  | with-ann(mutable y :: Number)
  | arrow-ann(mutable f :: (Number -> String))
  | multi-field(mutable a, mutable b, c)
where:
    my-d1 = no-ann(5)
    my-d1!x is 5

    cases(D) my-d1: | no-ann(x) => x; is 5

    my-d1!{x : 10}

    my-d1!x is 10

    my-d1!y raises "y was not found"

    Mutable(my-d1:x) is true

    with-ann("not-a-num") raises "expected Number"

    my-d2 = with-ann(5)
    my-d2!{ y : "not-a-num" } raises "expected Number"

    my-d2!{ y : 10 }
    my-d2!y is 10

    my-other-d2 = with-ann(10)

    (my-other-d2 == my-d2) is false
    my-d2 is my-d2
    my-other-d2 is my-other-d2

    my-d3 = arrow-ann(fun(n): "some-string" end)
    my-d3!f(5) is "some-string"
    my-d3!{ f : fun(n): 42 end }
    my-d3!f(5) raises "expected String"

    my-d4 = multi-field(1, 2, 3)
    cases(D) my-d4: | multi-field(a, b, c) => [a, b, c]; is [1,2,3]

    my-d4!{a : "new-a", b : "new-b" }
    my-d4!a is "new-a"
    my-d4!b is "new-b"

    cases(D) my-d4: | multi-field(a, b, c) => [a, b, c]; is ["new-a","new-b",3]

    my-d4!{c : 5} raises "Updating immutable field"
    my-d4!{a : 5, c : 10} raises "Updating immutable field"
    my-d4!a is "new-a" # doesn't get updated unless all updates work

    my-d4!c is 3
end

data Node deriving builtins.Eq:
  | node(mutable in :: list.List, n :: Number)
where:
  n1 = node([], 2)
  n2 = node([n1], 3)
  n3 = node([n2], 4)
  n1!{in : [n3]}
  n1!in.first.n is 4
  (((n1!in).first!in).first!in).first.eq(n1) is true
end

check:
  m1 = mk-simple-mutable(5)
  m2 = mk-simple-mutable("5")
  m3 = mk-simple-mutable(true)
  m4 = mk-simple-mutable(false)

  m1 is m1
  m2 is m2
  m3 is m3
  m4 is m4
  m1 satisfies _ <> m2
  m3 satisfies _ <> m4

  torepr(m1) is "mutable(5)"
  torepr(m2) is "mutable(\"5\")"
  torepr(m3) is "mutable(true)"
  torepr(m4) is "mutable(false)"
end

