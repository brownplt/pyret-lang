tuply-contract :: {Number;Number} -> {Number;Number}
fun tuply-contract({x; y}):
  {x + y; x - y}
end

tup1 :: {Number; Number} -> Number
fun tup1(tup): 5 end

tup2 :: {Number; Number} -> Number
fun tup2({x; y}): 5 end

tup3 :: {{Number; Number} ; Number} -> Number
fun tup3(tup): 5 end

tup4 :: {{Number; Number} ; Number} -> Number
fun tup4({xy; z}): 5 end

tup5 :: {{Number; Number} ; Number} -> Number
fun tup5({{x; y}; z}): 5 end
  
check:
  "all good" satisfies is-string
end
