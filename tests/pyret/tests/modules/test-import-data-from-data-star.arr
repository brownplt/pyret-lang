import file("provide-data-star.arr") as Data

include from Data:
  data *
end

check:
  d :: D = x
  e :: E = y(1, 2)
  is-x satisfies is-function
  is-y satisfies is-function
end


#|

A defines data D

{ values: "is-x", types: "D", datatypes: "D" }

B imports A with data* and re-provides data D

{ values: "is-x", types: "D" }

C imports B with data *

|#

#|
  module M:
    provide: type Num end
    type Num = Number
  module C:
    import M as M
    include from M:
      data Num
    end
|#


#|
  module M:
    provide: type Num, f end
    data Num: n end
    fun f(x :: Num) -> Number: 0 end

  module C:
    import M as M
    include from M:
      data Num
    end
|#


