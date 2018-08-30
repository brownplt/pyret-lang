check "https://github.com/brownplt/pyret-lang/issues/1342":
  # if this parses, the test is fine
  var x = 1
  spy: x end
  spy: some-name: x end
end
