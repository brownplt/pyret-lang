fun test(n :: Number):
  if n == 0:
    empty
  else:
    link(num-to-string(num-random(10000000)), test(n - 1))
  end
end

print(string-length(test(100000).join-str('-'))) 
