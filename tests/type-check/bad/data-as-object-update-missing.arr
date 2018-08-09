data Foo:
  | bar(a :: Number, ref b :: String)
end

# bar(1, "a") does not have a field c
bar(1, "a")!{c: 3}