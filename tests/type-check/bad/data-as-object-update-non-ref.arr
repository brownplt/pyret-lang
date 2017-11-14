data Foo:
  | bar(a :: Number, ref b :: String)
end

# bar(1, "a").a is a String but update is for refs 
bar(1, "a")!{a: 3}