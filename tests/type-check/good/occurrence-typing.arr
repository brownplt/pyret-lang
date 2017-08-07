data Foo:
  | foo(a :: Number)
  | bar(b :: String)
end

fun f(x :: Foo) -> String:
  cases(Foo) x:
    | foo(_) =>
      tostring(x.a)
    | bar(_) =>
      x.b
  end
end

data Bar<A>:
  | foo2(a :: A)
  | bar2(b :: String)
end

fun g<A>(x :: Bar<A>) -> String:
  cases(Bar<A>) x:
    | foo2(_) =>
      "a"
    | bar2(_) =>
      x.b
  end
end