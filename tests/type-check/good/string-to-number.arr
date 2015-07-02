import option as O

fun tonum(s :: String) -> Number:
  cases(O.Option<Number>) string-to-number(s):
    | none => raise("Junk data: " + s)
    | some(v) => v
  end
end

