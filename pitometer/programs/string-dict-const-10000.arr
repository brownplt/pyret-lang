import string-dict as SD
for fold(s from [SD.string-dict:], _ from range(0, 10000)):
  s.set("const", true)
end
# test repeatedly setting same key of a string-dict
