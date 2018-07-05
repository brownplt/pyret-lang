fun f():
  if true:
    raise("a")
  else:
    "a"
  end
end

fun g():
  if true:
    "a"
  else:
    raise("a")
  end
end

app = lam<a, b>(x :: (a -> b), y :: a) -> b:
  x(y)
end

app(lam(l :: Option<Number>) -> String:
    cases (Option) l:
      | none => "foo"
      | some(x) => raise("bar")
    end
  end, none)

lam(l :: Option<Number>) -> String:
  cases (Option) l:
    | none => "foo"
    | some(x) => raise("bar")
  end
end