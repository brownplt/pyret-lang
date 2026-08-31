# Several compile errors at once. Guards that `unique` (compile-lib) emits them
# in the SAME order as the .arr compiler: Pyret's sets.list-to-list-set(...).to-list()
# keeps first occurrences but REVERSES their order, so the TS port must reverse too
# (otherwise TS reports the exact reverse). The check-block error is collected last
# (deferred phase), so this also pins the reverse-of-collection order -- which is
# NOT a source-position sort.
w = aa + bb
check:
  cc is 1
end
z = dd
