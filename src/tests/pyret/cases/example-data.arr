#lang pyret

provide {
  MyData: MyData,
  single: single,
  multi: multi
}

data MyData:
  | single
  | multi(a, b, c)
end
