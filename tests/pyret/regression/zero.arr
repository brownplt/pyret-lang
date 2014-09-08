import s-exp as S

check:
  string-tonumber("0") is 0
  S.read-s-exp("0") is S.s-num(0)
end
