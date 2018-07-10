import reactors as R 

reactor:
  init: 10,
  on-tick: lam(w :: Number): w + 1 end,
  stop-when: lam(x :: Number): x > 10 end
end

fun tencrement(x :: Number): x + 10 end

r :: R.Reactor<Number> = reactor:
  seconds-per-tick: 0.1,
  title: "Count by 10",
  on-tick: tencrement,
  init: 10,
end

t :: Table = R.simulate-trace(r, 20) 

spy: t end
