fun double(n):
  n * 2
where:
  double(2) is 4
  double(0) is 0
end

check "string ops":
  string-length("abc") is 3
  string-append("a", "b") is "ab"
  string-to-upper("hi") is "HI"
end

check "lists":
  [list: 1, 2].length() is 2
  [list: 1, 2, 3].reverse() is [list: 3, 2, 1]
end
