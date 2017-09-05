import string-dict as SD
for fold(s from [SD.string-dict:], i from range(0, 10000)):
  s.set(num-to-string(i), true)
end
# test growing string-dict
