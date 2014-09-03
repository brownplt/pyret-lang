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

  sd2.keys() raises "Not yet implemented"
  sd2.has-key("a") is true
  sd2.has-key("z") is false

  (sd1 == sd2) raises "Not yet implemented"

end

check "immutables":
  SD.make-immutable-string-dict() raises "Not yet implemented"
  [SD.immutable-string-dict: "a", 5, "b", 10] raises "Not yet implemented"
end
