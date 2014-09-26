import string-dict2 as SD

torepr([SD.string-dict: "a", 15, "b", 10])

check "basics":

  sd1 = SD.make-string-dict()
  sd1.set("a", 5)
  sd1.get("a") is 5
  sd1.set("a", 10)
  sd1.get("a") is 10

  sd1.get("c") raises "Key c not found"

  sd2 = [SD.string-dict: "a", 15, "b", 10]
  sd2.get("a") is 15
  sd2.get("b") is 10

  fun check-sdstr(s):
    (s == "[string-dict: \"a\", 15, \"b\", 10]") or
      (s == "[string-dict: \"b\", 10, \"a\", 15]")
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

  torepr([SD.string-dict: "a", long-torepr]) is "[string-dict: \"a\", 9999]"

  sd2.keys() is [list: "a", "b"]

  sd2.has-key("a") is true
  sd2.has-key("z") is false

  sd3 = [SD.string-dict: "a", 15, "b", 10]
  sd4 = [SD.string-dict: "a", 15, "b", 20]
  sd5 = [SD.string-dict: "a", 15, "b", 10, "c", 15]

  sd2 is sd3
  sd2 is-not sd4
  sd2 is-not sd5
  sd2 is-not 2

  isd2 = [SD.immutable-string-dict: "a", 15, "b", 10]

  isd2.keys() is [list: "a", "b"]

  isd2.has-key("a") is true
  isd2.has-key("z") is false

  isd3 = [SD.immutable-string-dict: "a", 15, "b", 10]
  isd4 = [SD.immutable-string-dict: "a", 15, "b", 20]
  isd5 = [SD.immutable-string-dict: "a", 15, "b", 10, "c", 15]

  isd2 is-not sd2
  isd3 is-not sd3
  isd4 is-not sd4
  isd5 is-not sd5

  isd2 is isd3
  isd2 is-not isd4
  isd2 is-not isd5
  isd2 is-not 2

  isd6 = isd5.set("a", 7)

  isd5 is-not isd6
  isd5.get("a") is 15
  isd6.get("a") is 7

  sd5.remove("a")
  sd5.get("a") raises "Key a not found"

  isd7 = isd6.remove("a")
  isd7.get("a") raises "Key a not found"
  isd6.get("a") is 7

  sd7 = [SD.string-dict: "a", false]
  sd7.has-key("a") is true
  sd7.has-key("b") is false

  sd5.count() is 2
  isd5.count() is 3

  isd8 = sd5.freeze()
  isd8.get("b") is 10
  sd5.set("b", 5)
  sd5.get("b") is 5
  isd8.get("b") is 10

  sd8 = isd5.unfreeze()
  sd8.get("a") is 15
  sd8.set("a", 23)
  sd8.get("a") is 23
  isd5.get("a") is 15
end

#check "immutables":
#  SD.make-immutable-string-dict() raises "Not yet implemented"
#  [SD.immutable-string-dict: "a", 5, "b", 10] raises "Not yet implemented"
#end
