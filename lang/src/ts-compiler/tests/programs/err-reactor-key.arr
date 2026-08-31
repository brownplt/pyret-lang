# A reactor with both on-key and on-raw-key. The .arr well-formed check called
# wf-error with a missing loc (arity bug -> crash); the port passed `undefined`
# -> a downstream `.format()` crash. Both now pass the reactor loc and report
# the error cleanly (and byte-identically).
import reactors as R
r = reactor:
  init: 0,
  on-key: lam(x, k): x end,
  on-raw-key: lam(x, k): x end
end
