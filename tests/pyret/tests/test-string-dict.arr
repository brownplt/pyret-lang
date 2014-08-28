import string-dict2 as SD


check "basics":

  sd1 = SD.make-string-dict()
  sd1.set("a", 5)
  sd1.get("a") is 5
  sd1.set("a", 10)
  sd1.get("a") is 10

  sd1.get("c") raises "Key c not found"

  sd2 = [SD.string-dict: "a", 5, "b", 10]
  sd2.get("a") is 5
  sd2.get("b") is 10

  sd2.keys() raises "Not yet implemented"
end
