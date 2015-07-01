fun tonum(s :: String) -> Number:
  cases(Option<Number>) string-to-number(s):
    | none => raise("Junk data: " + s)
    | some(v) => v
  end
end

