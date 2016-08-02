import parse-pyret as P
import pprint as PP
import reactors as R

check "parse and print":
  t1 = P.surface-parse("reactor: init: 5 end", "test")
  t1.tosource().pretty(80) is [list: "reactor: init: 5 end"]
  t2 = P.surface-parse("reactor: init: 5, on-tick: ticker end", "test")
  t2.tosource().pretty(80) is [list: "reactor: init: 5, on-tick: ticker end"]
end

check "reactors":
  r1 = reactor:
    init: 5,
    on-tick: lam(x): x + 1 end
  end

  r1.get-value() is 5
  r2 = r1.react(R.time-tick)
  r2.get-value() is 6
  r2.get-trace() raises "Tried to get trace"

  r3 = r2.start-trace()
  r4 = r3.react(R.time-tick)
  r4.get-trace() is [list: 6, 7]

  r4.react(R.mouse(4, 5, "buttondown")) raises "No on-mouse handler defined"
  r4.react(R.keypress("left")) raises "No on-key handler defined"

  r4.interact() raises "No interaction set up"

  torepr(r1) is "reactor(5)"
  torepr(r4) is "reactor(7)"

end

check "reactor-functions":
  r1 :: R.Reactor<Number> = reactor:
    init: 5,
    on-mouse: lam(w, x, y, kind): w + x + y end
  end

  R.get-value(r1) is 5
  R.get-value(R.react(r1, R.mouse(7, 8, "buttondown"))) is 20
  r1.get-trace() raises "Tried to get trace"

  r2 = R.start-trace(r1)
  r3 = R.react(r2, R.mouse(11, 13, "buttondown"))

  R.get-trace(r3) is [list: 5, 29]

  R.interact(r3) raises "No interaction set up"
end


check "stop-when, manually":
  r = reactor:
    init: 10,
    on-tick: lam(w): w + 1 end,
    stop-when: lam(x): x > 10 end
  end
  r.react(R.time-tick).get-value() is 11
  r.react(R.time-tick).react(R.time-tick).get-value() is 11
  r.react(R.time-tick).react(R.time-tick).react(R.time-tick).get-value() is 11
end
