import is-field-not-found from error

fun func() -> Number: 0
where:
  fun pred-fun(a :: Number, b :: Number):
    a == 4
  end

  func() is 0
  func() is-roughly 0.0000000000001
  func() is-not 10
  func() is%(pred-fun) 4
  func() is-not%(pred-fun) 5
  func() is== 0
  func() is-not== 5
  func() is=~ 0
  func() is-not=~ 5
  func() is<=> 0
  func() is-not<=> 5
  func() satisfies {(x): true}
  func() violates {(x): false}
  1 / 0 raises "zero"
  1 / 0 raises-other-than "field"
  func() does-not-raise
  # o = {}
  # o.x raises-satisfies is-field-not-found
  # 1 / 0 raises-violates is-field-not-found
end