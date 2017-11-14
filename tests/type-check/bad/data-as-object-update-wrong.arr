data Foo:
  | bar(a :: Number, ref b :: String)
end

# bar(1, "a").b has type String not Number
bar(1, "a")!{b: 3}