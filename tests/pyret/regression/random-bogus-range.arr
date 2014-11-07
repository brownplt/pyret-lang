
check "https://github.com/brownplt/pyret-lang/issues/349":
  for each(_ from range(0, 100)):
    random(9000000000000001) is%(_ < _) 9000000000000001
  end
end
