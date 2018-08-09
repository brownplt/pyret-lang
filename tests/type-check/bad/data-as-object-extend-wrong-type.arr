data Foo:
  | bar(a :: Number, ref b :: String)
end

# bar(1, "a").a has type Number not String
bar(1, "a").{a: "b"}