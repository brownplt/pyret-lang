### shipshape

include global

that-old-thing = nothing

shadow nothing = 100

fun f():
  when 5 < 10: "hello" end
end

check:
  n :: Any = nothing
  o :: Any = that-old-thing
  n is-not o
  f() is that-old-thing
end
