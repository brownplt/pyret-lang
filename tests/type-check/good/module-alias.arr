import s-exp as S

a = S.s-sym("a")

b :: String = cases(S.S-Exp) a:
  | s-str(str) => str
  | s-sym(sym) => sym
  | else => "default"
end
