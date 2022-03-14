##! incorrect-type

include global

data D:
  | x(a :: Number)
end

x(9)!{a : 10}