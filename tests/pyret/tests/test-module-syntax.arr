
import file("provide-modules.arr") as PM
include from PM:
  sd, msd,
  type SD, type MSD,
  module S
end

check:
  PM.sd is<=> sd
  PM.msd is<=> msd
  PM.msd is<=> S.mutable-string-dict

  [sd: "a", 5].get-value("a") is 5
  x :: PM.SD = [PM.sd: "b", 6]
  y :: SD = [S.string-dict: "b", 6]
  x is y
end
