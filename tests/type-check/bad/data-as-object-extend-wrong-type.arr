data Foo:
  | bar(a :: Number, ref b :: String)
end

bar(1, "a").{b: "b"}