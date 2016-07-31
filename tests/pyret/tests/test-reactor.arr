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
  r2 = r1.react(R.tick)
  r2.get-value() is 6
  r2.get-trace() raises "Tried to get trace"

  r3 = r2.start-trace()
  r4 = r3.react(R.tick)
  r4.get-trace() is [list: 6, 7]

  r4.react(R.mouse(4, 5, "buttondown")) raises "No on-mouse handler defined"
  r4.react(R.keypress("left")) raises "No on-key handler defined"

  r4.interact() raises "No interaction set up"

end

