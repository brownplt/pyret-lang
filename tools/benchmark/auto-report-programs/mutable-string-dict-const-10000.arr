import string-dict as SD
m = [SD.mutable-string-dict:]
for map(_ from range(0, 10000)):
  m.set-now("const", true)
end
# test repeatedly setting same key of a mutable-string-dict
