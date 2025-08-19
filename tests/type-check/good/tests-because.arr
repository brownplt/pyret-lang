check:
  fun less-than(l :: Number, r :: Number): l < r end
  fun is-even(n :: Number): num-modulo(n, 2) == 0 end

  67 is 67
  67 is 67 because 67

  ~0.0000001 is-roughly ~0.0000002
  ~0.0000001 is-roughly ~0.0000002 because ~0.0000001

  67 is-not 69
  67 is-not 69 because 67

  67 is%(less-than) 69
  67 is%(less-than) 69 because 67

  69 is-not%(less-than) 67
  69 is-not%(less-than) 67 because 69

  67 + 1 satisfies is-even
  67 + 1 satisfies is-even because 68

  70 - 1 violates is-even
  70 - 1 violates is-even because 69

  raise("err") raises "err"
  raise("err") raises "err" because raise("err")

  raise("foo") raises-other-than "bar"
  raise("foo") raises-other-than "bar" because raise("foo")

  67 does-not-raise
  67 does-not-raise because 67

  fun is-foo(x :: Any): x == "foo" end

  raise("foo") raises-satisfies is-foo
  raise("foo") raises-satisfies is-foo because raise("foo")

  raise("bar") raises-violates is-foo
  raise("bar") raises-violates is-foo because raise("bar")
end

