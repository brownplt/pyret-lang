# Two cross-compiler canonicalizations meet here: the "Valid options" list must
# be in the same (sorted) order (cross-cutting #1), and the trailing cmcode loc
# must render identically -- tostring, source unquoted (the srcloc stringRepr
# finding). Both compilers now agree byte-for-byte.
r = reactor:
  init: 0,
  not-a-real-option: 5
end
print(r)
