import world as W

# This just checks basic well-formedness and type errors of the world handler
# functions, not the event behavior of world itself

check "on-tick":
  inc = fun(v): v + 1 end

  on-tick(inc) satisfies W.is-world-config
  on-tick(inc, 5) raises "arity"
  on-tick() raises "arity"
  on-tick(5) raises "Function"

  on-tick-n(inc, 5) satisfies W.is-world-config
  on-tick-n(inc) raises "arity"
  on-tick-n(inc, 5, 6) raises "arity"
  on-tick-n(5, 5) raises "Function"
  on-tick-n(inc, inc) raises "Number"
end

check "on-key":
  key = fun(v, k): v + 1 end

  on-key(key) satisfies W.is-world-config
  on-key("not-key-function") raises "Function"
  on-key() raises "arity"
  on-key(4, 5) raises "arity"
end

check "to-draw":
  draw = fun(v): v end

  to-draw(draw) satisfies W.is-world-config
  to-draw("a") raises "Function"
  to-draw() raises "arity"
  to-draw(draw, 5) raises "arity"
end

check "on-mouse":
  mouse = fun(v, x, y, type): v + 1 end

  on-mouse(mouse) satisfies W.is-world-config
  on-mouse("not-mouse-function") raises "Function"
  on-mouse() raises "arity"
  on-mouse(4, 5) raises "arity"
end

check "stop-when":
  stop = fun(v): v > 1 end

  stop-when(stop) satisfies W.is-world-config
  stop-when("not-mouse-function") raises "Function"
  stop-when() raises "arity"
  stop-when(4, 5) raises "arity"
end

check "misc":
  W.is-world-config(42) is false

  is-key-equal(4, 5) raises "String"
  is-key-equal("up", "up") is true
  is-key-equal("up", "down") is false
end
