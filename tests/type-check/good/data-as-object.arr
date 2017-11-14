data Foo:
  | bar(a :: Number, ref b :: String)
end

bar(1, "a").{a: 3}
bar(1, "b")!{b: "a"}