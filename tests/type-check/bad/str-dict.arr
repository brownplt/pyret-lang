import str-dict as S

s = [S.string-dict: {"a";5}, {"b";7}]

fun f(o :: Option<Boolean>):
  o
end

f(s.get("b"))
