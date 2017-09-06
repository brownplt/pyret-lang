import string-dict as SD
m = [SD.mutable-string-dict:]
for map(i from range(0, 10000)):
  m.set-now(num-to-string(i), true)
end
# test growing mutable-string-dict
