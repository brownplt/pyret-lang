data Foo:
  | bar(a :: Number, ref b :: String)
end

# bar(1, "a").b is a ref String while "a" is a String
bar(1, "a").{b: "a"}