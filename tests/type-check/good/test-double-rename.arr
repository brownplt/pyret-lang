import file("double-rename.arr") as DR
import string-dict as SD

include from DR:
  string-dict,
  str-dict,
  type StringDict,
  type StrDict
end
include from SD:
  type StringDict,
  type StringDict as StrDict,
  string-dict,
  string-dict as str-dict
end

check:
  x :: StrDict = [SD.string-dict:]
  y :: StringDict = x
  z :: DR.StrDict = y

  string-dict is SD.string-dict
  string-dict is DR.string-dict
  str-dict is SD.string-dict
  str-dict is DR.string-dict
end
