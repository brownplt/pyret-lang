import format as F

check:
  F.format("~a!", [list: "it works"]) is "it works!"
end
