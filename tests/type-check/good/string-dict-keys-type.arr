include string-dict
check:
  msd1 = [mutable-string-dict: "a", 5, "b", 10]
  ks :: Set<String> = msd1.keys-now()
  ks2 :: List<String> = msd1.keys-list-now()
  ks.size() is ks2.length()
end
