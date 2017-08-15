import string-dict as S

s = [S.string-dict: "a", 5, "b", 7]
s1 = [S.string-dict: "a", 5, "b", 7, "c", 8]
s2 = [S.string-dict: "a", 5, "b", 7, "c", 8, "d", 9]
s3 = [S.string-dict: "a", 5, "b", 7, "c", 8, "d", 9, "e", 10]
s4 = [S.string-dict: "a", 5, "b", 7, "c", 8, "d", 9, "e", 10, "f", 11]

fun f(o :: Option<Number>):
  o
end

f(s.get("b"))