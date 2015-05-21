import string-dict as SD

sd1 = SD.make-mutable-string-dict()
sd2 = [SD.mutable-string-dict: "a", 15, "b", 10]

sd3 = [SD.mutable-string-dict: "a", 15, "b", 10]
sd4 = [SD.mutable-string-dict: "a", 15, "b", 20]
sd5 = [SD.mutable-string-dict: "a", 15, "b", 10, "c", 15]

isd2 = [SD.string-dict: "a", 15, "b", 10]

isd3 = [SD.string-dict: "a", 15, "b", 10]
isd4 = [SD.string-dict: "a", 15, "b", 20]
isd5 = [SD.string-dict: "a", 15, "b", 10, "c", 15]

check "basics":

  sd1.set-now("a", 5) is nothing
  sd1.get-value-now("a") is 5
  sd1.get-now("a").or-else(1) is 5
  sd1.set-now("a", 10) is nothing
  sd1.get-value-now("a") is 10
  sd1.get-now("a").or-else(1) is 10

  sd1.get-now("c").or-else(42) is 42
  sd1.get-value-now("c") raises "Key c not found"

  sd2.get-now("a").or-else(42) is 15
  sd2.get-value-now("b") is 10

  fun check-sdstr(s):
    (s == "[mutable-string-dict: \"a\", 15, \"b\", 10]") or
      (s == "[mutable-string-dict: \"b\", 10, \"a\", 15]")
  end
  torepr(sd2) satisfies check-sdstr

  var long-torepr = nothing
  long-torepr := {
    _torepr(self, tor):
      var str = ""
      for each(i from range(0, 10000)):
        str := tostring(i)
      end
      str
    end
  }

  torepr([SD.mutable-string-dict: "a", long-torepr]) is "[mutable-string-dict: \"a\", 9999]"

  sd2.keys-now() is [tree-set: "a", "b"]
  sd2.keys-now() is [tree-set: "b", "a"]

  sd2.has-key-now("a") is true
  sd2.has-key-now("z") is false

  sd2 is sd3
  sd2 is-not sd4
  sd2 is-not sd5
  sd2 is-not 2

  [SD.mutable-string-dict: "a", 5] is-not [SD.mutable-string-dict: "b", 5]
  [SD.mutable-string-dict: "a", 5, "b", 5]
    is-not [SD.mutable-string-dict: "b", 5, "c", 5]
  sd-many-as = [SD.mutable-string-dict:]
  sd-almost-many-as = [SD.mutable-string-dict: "a", 10]
  for each(i from range(0, 100)):
    sd-many-as.set-now("a" + tostring(i), i)
    when not(i == 54):
      sd-almost-many-as.set-now("a" + tostring(i), i)
    end
  end
  sd-many-as is-not sd-almost-many-as
  sd-almost-many-as is-not sd-many-as

  isd2.keys() is [tree-set: "a", "b"]

  isd2.has-key("a") is true
  isd2.has-key("z") is false
end

check "Immutable string dicts":

  isd2 is-not sd2
  isd3 is-not sd3
  isd4 is-not sd4
  isd5 is-not sd5

  isd2 is isd3
  isd2 is-not isd4
  isd2 is-not isd5
  isd2 is-not 2

  [SD.string-dict: "a", 5] is-not [SD.string-dict: "b", 5]
  [SD.string-dict: "a", 5, "b", 5]
    is-not [SD.string-dict: "b", 5, "c", 6]

  sd-many-as = for fold(ad from [SD.string-dict:], i from range(0, 100)):
    ad.set("a" + tostring(i), i)
  end
  sd-almost-many-as = for fold(ad from [SD.string-dict: "a", 5], i from range(0, 100)):
    if i == 54:
      ad
    else:
      ad.set("a" + tostring(i), i)
    end
  end
  sd-many-as is-not sd-almost-many-as
  sd-almost-many-as is-not sd-many-as

  isd6 = isd5.set("a", 7)

  isd5 is-not isd6
  isd5.get-value("a") is 15
  isd6.get-value("a") is 7
  isd6.get("b").or-else(42) is 10

  sd5.remove-now("a")
  sd5.get-value-now("a") raises "Key a not found"

  isd7 = isd6.remove("a")
  isd7.get-value("a") raises "Key a not found"
  isd6.get-value("a") is 7

  sd7 = [SD.mutable-string-dict: "a", false]
  sd7.has-key-now("a") is true
  sd7.has-key-now("b") is false

  sd5.count-now() is 2
  isd5.count() is 3

  isd8 = sd5.freeze()
  isd8.get-value("b") is 10
  sd5.set-now("b", 5) is nothing
  sd5.get-value-now("b") is 5
  isd8.get-value("b") is 10

  sd8 = isd5.unfreeze()
  sd8.get-value-now("a") is 15
  sd8.set-now("a", 23) is nothing
  sd8.get-value-now("a") is 23
  isd5.get-value("a") is 15

  sd9 = sd3.seal()
  sd9.get-value-now("a") is 15
  sd9.set-now("b", 20) raises "Cannot modify sealed string dict"
  sd3.set-now("b", 20) is nothing
  sd9.get-value-now("b") is 20
end

check "cyclic":
  s1 = [SD.mutable-string-dict: "a", nothing]
  s1.set-now("a", s1)
  torepr(s1) is "[mutable-string-dict: \"a\", <cyclic-object-1>]"
end

fun one-of(ans, elts):
  is-some(for find(elt from elts):
    ans == elt
  end)
end

check "merge":
  s1 = [SD.string-dict: "a", 5, "c", 4]
  s2 = [SD.string-dict: "a", 10, "b", 6]
  s3 = s1.merge(s2)
  s3 is [SD.string-dict: "a", 10, "b", 6, "c", 4]
  s2.merge(s1) is [SD.string-dict: "a", 5, "b", 6, "c", 4]
  s2.merge(s1).merge(s1) is s2.merge(s1)

  s1.keys-list() is%(one-of) [list: [list: "a", "c"], [list: "c", "a"]]
  s2.keys-list() is%(one-of) [list: [list: "a", "b"], [list: "c", "a"]]

  s4 = [SD.string-dict: "a", 5]
  s5 = [SD.string-dict:]
  s4.merge(s5) is s4
  s5.merge(s4) is s4
end

check "duplicate":
  [SD.string-dict: "x", 5, "x", 10] raises "duplicate key x"
  [SD.string-dict: "x", 6, "y", 10, "x", 22] raises "duplicate key x"
  [SD.string-dict: "x", 6, "y", 10, "z", 22] does-not-raise
end
