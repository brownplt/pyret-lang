import global as G
include option
include string-dict
include lists
import string as STR
import sets as S

include from S:
  tree-set
end

# NOTE: Most tests were taken directly from the documentation
# https://www.pyret.org/docs/latest/string-dict.html#%28part._.String.Dict_.Methods%29

check "get":
  [string-dict: {"a"; 5}].get("a") is some(5)
  [string-dict: {"a"; 5}].get("b") is none
end

check "get-value":
  [string-dict: {"a"; 5}].get-value("a") is 5
  [string-dict: {"a"; 5}].get-value("b") raises "Key b not found"
end

check "set":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  sd1.get-value("a") is 5
  sd1.get-value("b") is 10
  sd2 = sd1.set("a", 15)
  sd2.get-value("a") is 15
  sd2.get-value("b") is 10
end

check "has-key":
  sd1 = [string-dict: {"a"; 5}]
  sd1.has-key("a") is true
  sd1.has-key("b") is false
end

check "keys":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  sd1.keys() is [tree-set: "a", "b"]
  sd1.keys() is [tree-set: "b", "a"]
end

# NOTE: no guarentees on key order
check "keys-list":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  keys = sd1.keys-list()
  keys.length() is 2
  keys.member("a") is true
  keys.member("b") is true
end

check "remove":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  sd1.has-key("a") is true
  sd1.has-key("b") is true
  sd2 = sd1.remove("b")
  sd1.has-key("a") is true
  sd1.has-key("b") is true
  sd2.has-key("a") is true
  sd2.has-key("b") is false
end

check "count":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  sd1.count() is 2
  sd2 = sd1.set("c", 15)
  sd2.count() is 3
  sd3 = sd1.remove("a")
  sd3.count() is 1
end

check "map-keys":
  sd1 = [string-dict: {"a"; 5}, {"BBB"; 10}]
  sd1.map-keys(lam(string):
    STR.string-length(string)
  end) is [list: 1, 3]
  map-keys(lam(string):
    STR.string-length(string)
  end, sd1) is [list: 1, 3]

end

check "fold-keys":
  sd1 = [string-dict: {"a"; 5}, {"BBB"; 10}]
  sd1.fold-keys(lam(string, acc):
    acc + STR.string-length(string)
  end, 0) is 4
  fold-keys(lam(acc, string):
    acc + STR.string-length(string)
  end, 0, sd1) is 4

end

check "merge":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  sd2 = [string-dict: {"b"; 200}, {"c"; 9000}]
  sd1.merge(sd2) is [string-dict: {"a"; 5}, {"b"; 200}, {"c"; 9000}]
end

check "is-string-dict":
  is-string-dict(5) is false
  is-string-dict([string-dict: ]) is true
  is-string-dict([string-dict: {"a"; 5}, {"b"; 10}]) is true
end

check "string-dict-of":
  string-dict-of([list: "foo", "bar"], 5) is [string-dict: {"foo"; 5}, {"bar"; 5}]
end

check "unfreeze":
  sd1 = [string-dict: {"a"; 5}, {"b"; 10}]
  msd1 = sd1.unfreeze()
  msd1.set-now("a", 0)
  msd1.get-value-now("a") is 0
end
