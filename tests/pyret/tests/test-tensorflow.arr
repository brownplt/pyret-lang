include tensorflow

######################
## Tensor Functions ##
######################

check "is-tensor":
  is-tensor([tensor: 1, 2, 3]) is true
  is-tensor(true) is false
  is-tensor(0) is false
  is-tensor([list: 1, 2, 3]) is false
end

check "list-to-tensor":
  is-tensor(list-to-tensor(empty)) is true
  is-tensor(list-to-tensor([list: 5, 3, 4, 7])) is true

  list-to-tensor(empty).data-now() is empty
  list-to-tensor([list: 9, 3, 2, 3]).data-now() is-roughly [list: 9, 3, 2, 3]
  list-to-tensor([list: 3, 2, 1, 0, 4, 9]).as-2d(2, 3).shape() is [list: 2, 3]
end

check "make-scalar":
  make-scalar(1).size() is 1
  make-scalar(~12.3).shape() is empty
  make-scalar(2.34).data-now() is-roughly [list: 2.34]
end

check "fill":
  fill([list: 0], 1).data-now()
    is-roughly [list: ]
  fill([list: 3], 5).data-now()
    is-roughly [list: 5, 5, 5]
  fill([list: 3, 2], -3).data-now()
    is-roughly [list: -3, -3, -3, -3, -3, -3]
end

check "linspace":
  linspace(0, 3, 1).data-now()
    is-roughly [list: 0]
  linspace(10, 11, 1).data-now()
    is-roughly [list: 10]
  linspace(5, 1, 5).data-now()
    is-roughly [list: 5, 4, 3, 2, 1]
  linspace(0, 9, 10).data-now()
    is-roughly [list: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  linspace(0, 4, 9).data-now()
    is-roughly [list: 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4]
end

check "ones":
  ones([list: 0]).data-now() is-roughly [list: ]
  ones([list: 4]).data-now() is-roughly [list: 1, 1, 1, 1]
  two-dim = ones([list: 3, 2])
  two-dim.shape() is [list: 3, 2]
  two-dim.data-now() is-roughly [list: 1, 1, 1, 1, 1, 1]
end

check "zeros":
  zeros([list: 0]).data-now() is-roughly [list: ]
  zeros([list: 4]).data-now() is-roughly [list: 0, 0, 0, 0]
  two-dim = zeros([list: 3, 2])
  two-dim.shape() is [list: 3, 2]
  two-dim.data-now() is-roughly [list: 0, 0, 0, 0, 0, 0]
end

check "multinomial":
  # Check that it raises an error if the input Tensor's dimension is too big:
  three-dim = [tensor: 1, 1, 1, 1, 1, 1, 1, 1].as-3d(2, 2, 2)
  multinomial(three-dim, 2, none, false)
    raises "must be a one-dimensional or two-dimensional Tensor"
  four-dim = [tensor: 0.5].as-4d(1, 1, 1, 1)
  multinomial(four-dim, 2, none, false)
    raises "must be a one-dimensional or two-dimensional Tensor"

  # Check that it raises an error if the input Tensor's size is too small:
  multinomial([tensor: ], 1, none, false)
    raises "must have at least two possible outcomes"
  multinomial([tensor: ], 3, none, false)
    raises "must have at least two possible outcomes"
  multinomial([tensor: 0.4], 1, none, false)
    raises "must have at least two possible outcomes"
  multinomial([tensor: 0.8], 7, none, false)
    raises "must have at least two possible outcomes"

  # Check that it returns the correct number of outcomes:
  multinomial([tensor: 1.0, 0.0], 1, none, true).shape() is [list: 1]
  multinomial([tensor: 1.0, 0.0], 3, none, true).shape() is [list: 3]
  multinomial([tensor: 0.3, 0.5, 0.7], 10, none, false).shape() is [list: 10]
  for each(num-outcomes from range(1, 20)):
    multinomial([tensor: 0.3, 0.4, 0.2], num-outcomes, none, false).shape()
      is [list: num-outcomes]
  end

  # Testing with a set random seed (the third argument):
  multinomial([tensor: 0.3, 0.4, 0.2], 4, some(1), false).data-now()
    is-roughly [list: ~1, ~1, ~0, ~2]
end

check "random-normal":
  random-normal(empty, none, none).size() is 1
  random-normal(empty, none, none).shape() is empty
  random-normal([list: 4, 3], none, none).shape() is [list: 4, 3]
  random-normal([list: 2, 5, 3], none, none).shape() is [list: 2, 5, 3]
end

check "random-uniform":
  random-uniform(empty, none, none).size() is 1
  random-uniform(empty, none, none).shape() is empty
  random-uniform([list: 1, 3], none, none).shape() is [list: 1, 3]
  random-uniform([list: 5, 4, 8], none, none).shape() is [list: 5, 4, 8]

  lower-bound = 1
  upper-bound = 10
  random-data = random-uniform([list: 20], some(lower-bound), some(upper-bound))
  for each(data-point from random-data.data-now()):
    data-point satisfies lam(x): (x >= lower-bound) and (x <= upper-bound) end
  end
end

###############################
## Variable Tensor Functions ##
###############################

check "make-variable":
  make-variable([tensor: ]).data-now() is-roughly empty
  make-variable([tensor: 1]).data-now() is-roughly [list: 1]

  # Check that we can perform normal Tensor operations on mutable Tensors:
  two-dim = [tensor: 4, 5, 3, 9].as-2d(2, 2)
  make-variable(two-dim).size() is 4
  make-variable(two-dim).shape() is [list: 2, 2]
  make-variable(two-dim).data-now() is-roughly [list: 4, 5, 3, 9]
  make-variable(two-dim).as-3d(4, 1, 1).shape() is [list: 4, 1, 1]
end

####################
## Tensor Methods ##
####################

check "Tensor .size":
  make-scalar(4.21).size() is 1
  [tensor: 6.32].size() is 1
  [tensor: 1, 2, 3].size() is 3
  [tensor: 1.4, 5.2, 0.4, 12.4, 14.3, 6].as-2d(3, 2).size() is 6
end

check "Tensor .shape":
  make-scalar(3).shape() is empty
  [tensor: 9].shape() is [list: 1]
  [tensor: 8, 3, 1].shape() is [list: 3]
  [tensor: 0, 0, 0, 0, 0, 0].as-2d(3, 2).shape() is [list: 3, 2]
end

check "Tensor .flatten":
  a = [tensor: 1, 2, 3, 4, 5, 6].as-2d(3, 2)
  a.shape() is [list: 3, 2]
  a.flatten().shape() is [list: 6]

  b = make-scalar(12)
  b.shape() is empty
  b.flatten().shape() is [list: 1]
end

check "Tensor .as-scalar":
  size-one = [tensor: 1]
  size-one.as-scalar().shape() is empty
  size-one.shape() is [list: 1] # doesn't modify shape of original tensor

  size-two = [tensor: 1, 2]
  size-two.as-scalar() raises
    "Tensor was size-2 but `as-scalar` requires the tensor to be size-1"
end

check "Tensor .as-1d":
  one-dim = [tensor: 1]
  two-dim = [tensor: 4, 3, 2, 1].as-2d(2, 2)
  three-dim = [tensor: 0, 1, 2, 3, 4, 5, 6, 7, 8].as-3d(3, 1, 3)

  one-dim.shape() is [list: 1]
  one-dim.as-1d().shape() is [list: 1]

  two-dim.shape() is [list: 2, 2]
  two-dim.as-1d().shape() is [list: 4]

  three-dim.shape() is [list: 3, 1, 3]
  three-dim.as-1d().shape() is [list: 9]
end

check "Tensor .as-2d":
  one-dim = [tensor: 1]
  two-dim = [tensor: 0, 1, 2, 3, 4, 5].as-2d(3, 2)
  three-dim = [tensor: 4, 3, 2, 1, 0, -1, -2, -3].as-3d(2, 2, 2)

  one-dim.shape() is [list: 1]
  one-dim.as-2d(1, 1).shape() is [list: 1, 1]

  two-dim.shape() is [list: 3, 2]
  two-dim.as-2d(2, 3).shape() is [list: 2, 3]

  three-dim.shape() is [list: 2, 2, 2]
  three-dim.as-2d(4, 2).shape() is [list: 4, 2]

  one-dim.as-2d(2, 1) raises "Cannot reshape"
  two-dim.as-2d(3, 3) raises "Cannot reshape"
  three-dim.as-2d(5, 4) raises "Cannot reshape"
end

check "Tensor .as-3d":
  one-dim = [tensor: 1]
  two-dim = [tensor: 0, 1, 2, 3, 4, 5, 6, 7].as-2d(4, 2)

  one-dim.shape() is [list: 1]
  one-dim.as-3d(1, 1, 1).shape() is [list: 1, 1, 1]

  two-dim.shape() is [list: 4, 2]
  two-dim.as-3d(2, 2, 2).shape() is [list: 2, 2, 2]

  one-dim.as-3d(2, 1, 1) raises "Cannot reshape"
  two-dim.as-3d(4, 3, 2) raises "Cannot reshape"
end

check "Tensor .as-4d":
  one-dim = [tensor: 1]
  two-dim = [tensor: 0, 1, 2, 3, 4, 5, 6, 7].as-2d(4, 2)

  one-dim.shape() is [list: 1]
  one-dim.as-4d(1, 1, 1, 1).shape() is [list: 1, 1, 1, 1]

  two-dim.shape() is [list: 4, 2]
  two-dim.as-4d(2, 2, 1, 2).shape() is [list: 2, 2, 1, 2]

  one-dim.as-4d(2, 1, 1, 1) raises "Cannot reshape"
  two-dim.as-4d(2, 2, 2, 2) raises "Cannot reshape"
end

check "Tensor .as-type":
  some-tensor = [tensor: 1, 3, 5, 8]

  some-tensor.as-type("float32") does-not-raise
  some-tensor.as-type("int32") does-not-raise
  some-tensor.as-type("bool") does-not-raise
  some-tensor.as-type("invalid")
    raises "Attempted to cast tensor to invalid type"
end

check "Tensor .to-float":
  [tensor: 0].to-float().data-now() is-roughly [list: 0]
  [tensor: 1].to-float().data-now() is-roughly [list: 1]
  [tensor: 0.42].to-float().data-now() is-roughly [list: 0.42]
  [tensor: 0.999999].to-float().data-now() is-roughly [list: 0.999999]
  [tensor: 1.52, 4.12, 5.99].to-float().data-now()
    is-roughly [list: 1.52, 4.12, 5.99]
  [tensor: 4, 0.32, 9.40, 8].to-float().data-now()
    is-roughly [list: 4, 0.32, 9.40, 8]
end

check "Tensor .to-int":
  [tensor: 0].to-int().data-now() is-roughly [list: 0]
  [tensor: 1].to-int().data-now() is-roughly [list: 1]
  [tensor: 0.42].to-int().data-now() is-roughly [list: 0]
  [tensor: 0.999999].to-int().data-now() is-roughly [list: 0]
  [tensor: 1.52, 4.12, 5.99].to-int().data-now()
    is-roughly [list: 1, 4, 5]
  [tensor: 4, 0.32, 9.40, 8].to-int().data-now()
    is-roughly [list: 4, 0, 9, 8]
end

check "Tensor .to-bool":
  [tensor: 0].to-bool().data-now() is-roughly [list: 0]
  [tensor: 1].to-bool().data-now() is-roughly [list: 1]
  [tensor: 0.42].to-bool().data-now() is-roughly [list: 1]
  [tensor: 1, 4, 5].to-bool().data-now() is-roughly [list: 1, 1, 1]
  [tensor: 4, 7, 0, 9].to-bool().data-now()
    is-roughly [list: 1, 1, 0, 1]
  [tensor: 0, 2, 3, 0, 0].to-bool().data-now()
    is-roughly [list: 0, 1, 1, 0, 0]
end

check "Tensor .to-buffer":
end

check "Tensor .to-variable":
end

check "Tensor .reshape":
end

check "Tensor .expand-dims":
end

check "Tensor .squeeze":
end

check "Tensor .clone":
end

check "Tensor .add":
end

check "Tensor .subtract":
end

check "Tensor .multiply":
end

check "Tensor .divide":
end

check "Tensor .floor-divide":
end

check "Tensor .max":
end

check "Tensor .min":
end

check "Tensor .modulo":
end

check "Tensor .expt":
end

check "Tensor .squared-difference":
end
