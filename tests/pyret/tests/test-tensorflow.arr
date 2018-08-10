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
  list-to-tensor(empty) satisfies is-tensor
  list-to-tensor([list: 5, 3, 4, 7]) satisfies is-tensor

  list-to-tensor(empty).data-now() is empty
  list-to-tensor([list: 9, 3, 2, 3]).data-now() is-roughly [list: 9, 3, 2, 3]
  list-to-tensor([list: 3, 2, 1, 0, 4, 9]).as-2d(2, 3).shape() is [list: 2, 3]
end

check "make-scalar":
  make-scalar(1).size() is 1
  make-scalar(12.3).shape() is empty
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
    is-roughly [list: 1, 1, 0, 2]
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

check "Tensor .data-now":
  [tensor: ].data-now() is-roughly [list: ]
  [tensor: 1].data-now() is-roughly [list: 1]
  [tensor: 1.43].data-now() is-roughly [list: 1.43]
  [tensor: -3.21, 9.4, 0.32].data-now() is-roughly [list: -3.21, 9.4, 0.32]
end

check "Tensor .to-float":
  [tensor: ].to-float().data-now() is-roughly [list: ]
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
  # NOTE(ZacharyEspiritu): The below test doesn't work since TensorFlow.js
  # raises a bizzare error due to what I think is a TensorFlow.js issue.
  # Leaving commented out for now.
  # [tensor: ].to-int().data-now() is-roughly [list: ]
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
  # NOTE(ZacharyEspiritu): The below test doesn't work since TensorFlow.js
  # raises a bizzare error due to what I think is a TensorFlow.js issue.
  # Leaving commented out for now.
  # [tensor: ].to-bool().data-now() is-roughly [list: ]
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
  empty-buffer = [tensor: ].to-buffer()
  empty-buffer satisfies is-tensor-buffer
  empty-buffer.get-all-now() is-roughly [list: ]

  some-shape  = [list: 2, 2]
  some-values = [list: 4, 5, 9, 3]
  some-tensor = list-to-tensor(some-values).reshape(some-shape)
  some-buffer = some-tensor.to-buffer()
  some-buffer satisfies is-tensor-buffer
  some-buffer.get-all-now() is-roughly some-values
  some-buffer.to-tensor().shape() is some-shape
end

check "Tensor .to-variable":
  [tensor: ].to-variable() does-not-raise
  [tensor: 4, 5, 1].to-variable() does-not-raise
  [tensor: 0, 5, 1, 9, 8, 4].as-2d(3, 2).to-variable() does-not-raise
end

check "Tensor .reshape":
  [tensor: ].reshape([list: ]) raises "Cannot reshape"
  [tensor: 3, 2].reshape([list: ]) raises "Cannot reshape"
  [tensor: 3, 2].reshape([list: 6]) raises "Cannot reshape"
  [tensor: 3, 2, 1].reshape([list: 2, 4]) raises "Cannot reshape"

  [tensor: 1].reshape([list: 1]).shape() is [list: 1]
  [tensor: 1].reshape([list: 1, 1, 1]).shape() is [list: 1, 1, 1]
  [tensor: 1].reshape([list: 1, 1, 1, 1, 1]).shape() is [list: 1, 1, 1, 1, 1]
  [tensor: 1, 4].reshape([list: 2, 1]).shape() is [list: 2, 1]
  [tensor: 1, 4, 4, 5, 9, 3].reshape([list: 3, 2]).shape() is [list: 3, 2]
end

check "Tensor .expand-dims":
  one-dim = [tensor: 1, 2, 3, 4]
  one-dim.shape() is [list: 4]
  one-dim.expand-dims(none).shape() is [list: 1, 4]
  one-dim.expand-dims(some(1)).shape() is [list: 4, 1]

  one-dim.expand-dims(some(2))
    raises "input axis must be less than or equal to the rank of the tensor"
end

check "Tensor .squeeze":
  multi-dim = [tensor: 1, 2, 3, 4].reshape([list: 1, 1, 1, 4, 1])
  multi-dim.shape() is [list: 1, 1, 1, 4, 1]
  multi-dim.squeeze(none).shape() is [list: 4]
  multi-dim.squeeze(some([list: 0])).shape() is [list: 1, 1, 4, 1]
  multi-dim.squeeze(some([list: 4])).shape() is [list: 1, 1, 1, 4]
  multi-dim.squeeze(some([list: 1, 2])).shape() is [list: 1, 4, 1]

  multi-dim.squeeze(some([list: 7]))
    raises "Cannot squeeze axis 7 since the axis does not exist"
  multi-dim.squeeze(some([list: 3]))
    raises "Cannot squeeze axis 3 since the dimension of that axis is 4, not 1"
end

check "Tensor .clone":
  some-tensor = [tensor: 1, 2, 3, 4]
  new-tensor  = some-tensor.clone()
  new-tensor.size() is some-tensor.size()
  new-tensor.shape() is some-tensor.shape()
  new-tensor.data-now() is-roughly some-tensor.data-now()
end

###########################
## Arithmetic Operations ##
###########################

check "add-tensors":
  # Check one-dimensional usages:
  add-tensors([tensor: 1], [tensor: 1]).data-now()
    is-roughly [list: 2]
  add-tensors([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 2, 4]
  add-tensors([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 6, 4]
  add-tensors([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 1]
  add-tensors(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 6, 2, 8, 4, 9, 5]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  add-tensors(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 6, 2, 5, 8, 4, 7, 9, 5, 8]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  add-tensors(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"
end

check "subtract-tensors":
  # Check one-dimensional usages:
  subtract-tensors([tensor: 1], [tensor: 1]).data-now()
    is-roughly [list: 0]
  subtract-tensors([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 0, 2]
  subtract-tensors([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: -4, 2]
  subtract-tensors([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check not commutative:
  subtract-tensors([tensor: 2], [tensor: 1]).data-now()
    is-roughly [list: 1]
  subtract-tensors([tensor: 1], [tensor: 2]).data-now()
    is-roughly [list: -1]

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 1]
  subtract-tensors(two-dim-1, one-dim-1).data-now()
    is-roughly [list: -4, 0, -2, 2, -1, 3]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  subtract-tensors(two-dim-1, two-dim-2).data-now()
    is-roughly [list: -4, 0, -3, -2, 2, -1, -1, 3, 0]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  subtract-tensors(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"
end

check "multiply-tensors":
  # Check one-dimensional usages:
  multiply-tensors([tensor: 1], [tensor: 1]).data-now()
    is-roughly [list: 1]
  multiply-tensors([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 1, 3]
  multiply-tensors([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 5, 3]
  multiply-tensors([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 1]
  multiply-tensors(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 5, 1, 15, 3, 20, 4]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  multiply-tensors(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 5, 1, 4, 15, 3, 12, 20, 4, 16]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  multiply-tensors(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"
end

check "divide-tensors":
  # Check one-dimensional usages:
  divide-tensors([tensor: 1], [tensor: 1]).data-now()
    is-roughly [list: 1]
  divide-tensors([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 1, 3]
  divide-tensors([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 0.2, 3]
  divide-tensors([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 1]
  divide-tensors(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 0.2, 1, 0.6, 3, 0.8, 4]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  divide-tensors(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 0.2, 1, 0.25, 0.6, 3, 0.75, 0.8, 4, 1]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  divide-tensors(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"

  # Check for divide-by-zero errors:
  divide-tensors([tensor: 1], [tensor: 0])
    raises "The second input Tensor cannot contain 0"
  divide-tensors([tensor: 1], [tensor: 1, 0])
    raises "The second input Tensor cannot contain 0"
  divide-tensors([tensor: 4.23], [tensor: 7.65, 1.43, 0, 2.31])
    raises "The second input Tensor cannot contain 0"
end

check "floor-divide-tensors":
  # Check one-dimensional usages:
  floor-divide-tensors([tensor: 1], [tensor: 1]).data-now()
    is-roughly [list: 1]
  floor-divide-tensors([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 1, 3]
  floor-divide-tensors([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 0, 3]
  floor-divide-tensors([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 1]
  floor-divide-tensors(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 0, 1, 0, 3, 0, 4]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  floor-divide-tensors(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 0, 1, 0, 0, 3, 0, 0, 4, 1]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  floor-divide-tensors(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"

  # Check for divide-by-zero errors:
  floor-divide-tensors([tensor: 1], [tensor: 0])
    raises "The second input Tensor cannot contain 0"
  floor-divide-tensors([tensor: 1], [tensor: 1, 0])
    raises "The second input Tensor cannot contain 0"
  floor-divide-tensors([tensor: 4.23], [tensor: 7.65, 1.43, 0])
    raises "The second input Tensor cannot contain 0"
end

check "tensor-max":
  # Check one-dimensional usages:
  tensor-max([tensor: 0], [tensor: 1]).data-now()
    is-roughly [list: 1]
  tensor-max([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 1, 3]
  tensor-max([tensor: 1, 3], [tensor: 200]).data-now()
    is-roughly [list: 200, 200]
  tensor-max([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 5, 3]
  tensor-max([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 0]
  tensor-max(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 5, 1, 5, 3, 5, 4]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  tensor-max(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 5, 1, 4, 5, 3, 4, 5, 4, 4]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  tensor-max(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"
end

check "tensor-min":
  # Check one-dimensional usages:
  tensor-min([tensor: 0], [tensor: 1]).data-now()
    is-roughly [list: 0]
  tensor-min([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 1, 1]
  tensor-min([tensor: 1, 3], [tensor: 200]).data-now()
    is-roughly [list: 1, 3]
  tensor-min([tensor: 1, 3], [tensor: 0]).data-now()
    is-roughly [list: 0, 0]
  tensor-min([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 1, 1]
  tensor-min([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 0]
  tensor-min(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 1, 0, 3, 0, 4, 0]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  tensor-min(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 1, 1, 1, 3, 1, 3, 4, 1, 4]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  tensor-min(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"
end

check "tensor-modulo":
  # Check one-dimensional usages:
  tensor-modulo([tensor: 0], [tensor: 1]).data-now()
    is-roughly [list: 0]
  tensor-modulo([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 0, 0]
  tensor-modulo([tensor: 1, 3], [tensor: 200]).data-now()
    is-roughly [list: 1, 3]
  tensor-modulo([tensor: 1, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 1, 0]
  tensor-modulo([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 1]
  tensor-modulo(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 1, 0, 3, 0, 4, 0]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  tensor-modulo(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 1, 0, 1, 3, 0, 3, 4, 0, 0]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  tensor-modulo(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"

  # Check for division-by-zero errors:
  tensor-modulo([tensor: 1], [tensor: 0])
    raises "The second input Tensor cannot contain 0"
  tensor-modulo([tensor: 1], [tensor: 1, 0])
    raises "The second input Tensor cannot contain 0"
  tensor-modulo([tensor: 4.23], [tensor: 7.65, 1.43, 0, 2.31])
    raises "The second input Tensor cannot contain 0"
end

check "tensor-expt":
  # Check one-dimensional usages:
  tensor-expt([tensor: 0], [tensor: 1]).data-now()
    is-roughly [list: 0]
  tensor-expt([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 1, 3]
  tensor-expt([tensor: 1, 3], [tensor: 4]).data-now()
    is-roughly [list: 1, 81]
  tensor-expt([tensor: 3, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 243, 3]
  tensor-expt([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 0]
  tensor-expt(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 1, 1, 243, 1, 1024, 1]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  tensor-expt(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 1, 1, 1, 243, 3, 81, 1024, 4, 256]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  tensor-expt(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"

  # Check raising to negative exponents:
  tensor-expt([tensor: 3], [tensor: -3]).data-now()
    is-roughly [list: 0.03703703]
end

check "squared-difference":
  # Check one-dimensional usages:
  squared-difference([tensor: 0], [tensor: 1]).data-now()
    is-roughly [list: 1]
  squared-difference([tensor: 3], [tensor: -3]).data-now()
    is-roughly [list: 36]
  squared-difference([tensor: 1, 3], [tensor: 1]).data-now()
    is-roughly [list: 0, 4]
  squared-difference([tensor: 1, 3], [tensor: 4]).data-now()
    is-roughly [list: 9, 1]
  squared-difference([tensor: 3, 3], [tensor: 5, 1]).data-now()
    is-roughly [list: 4, 4]
  squared-difference([tensor: 1, 3, 4], [tensor: 5, 1])
    raises "Tensors could not be applied as binary operation arguments"

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 1, 3, 4].as-2d(3, 1)
  one-dim-1 = [tensor: 5, 0]
  squared-difference(two-dim-1, one-dim-1).data-now()
    is-roughly [list: 16, 1, 4, 9, 1, 16]

  two-dim-2 = [tensor: 5, 1, 4].as-2d(1, 3)
  squared-difference(two-dim-1, two-dim-2).data-now()
    is-roughly [list: 16, 0, 9, 4, 4, 1, 1, 9, 0]

  two-dim-3 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-4 = [tensor: 9, 8, 7, 6].as-2d(4, 1)
  squared-difference(two-dim-3, two-dim-4).data-now()
    raises "Tensors could not be applied as binary operation arguments"
end

##################################
## Strict Arithmetic Operations ##
##################################

check "strict-add-tensors":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-add-tensors` to
         tensor-1 and tensor-2 produces the same result as applying
         `add-tensors` to the same Tensors.
         ```
    strict-result     = strict-add-tensors(tensor-1, tensor-2)
    non-strict-result = add-tensors(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 0, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-add-tensors(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-add-tensors(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-add-tensors(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-subtract-tensors":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-subtract-tensors` to
         tensor-1 and tensor-2 produces the same result as applying
         `subtract-tensors` to the same Tensors.
         ```
    strict-result     = strict-subtract-tensors(tensor-1, tensor-2)
    non-strict-result = subtract-tensors(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 0, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-subtract-tensors(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-subtract-tensors(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-subtract-tensors(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-multiply-tensors":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-multiply-tensors` to
         tensor-1 and tensor-2 produces the same result as applying
         `multiply-tensors` to the same Tensors.
         ```
    strict-result     = strict-multiply-tensors(tensor-1, tensor-2)
    non-strict-result = multiply-tensors(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 0, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-multiply-tensors(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-multiply-tensors(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-multiply-tensors(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-divide-tensors":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-divide-tensors` to
         tensor-1 and tensor-2 produces the same result as applying
         `divide-tensors` to the same Tensors.
         ```
    strict-result     = strict-divide-tensors(tensor-1, tensor-2)
    non-strict-result = divide-tensors(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 1, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for divide-by-zero errors:
  strict-divide-tensors([tensor: 1], [tensor: 0])
    raises "The second input Tensor cannot contain 0"
  strict-divide-tensors([tensor: 1, 1], [tensor: 1, 0])
    raises "The second input Tensor cannot contain 0"
  strict-divide-tensors([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])
    raises "The second input Tensor cannot contain 0"

  # Check for shape strictness:
  strict-divide-tensors(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-divide-tensors(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-divide-tensors(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-tensor-max":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-tensor-max` to
         tensor-1 and tensor-2 produces the same result as applying
         `tensor-max` to the same Tensors.
         ```
    strict-result     = strict-tensor-max(tensor-1, tensor-2)
    non-strict-result = tensor-max(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 0, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-tensor-max(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-max(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-max(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-tensor-min":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-tensor-min` to
         tensor-1 and tensor-2 produces the same result as applying
         `tensor-min` to the same Tensors.
         ```
    strict-result     = strict-tensor-min(tensor-1, tensor-2)
    non-strict-result = tensor-min(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 0, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-tensor-min(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-min(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-min(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-tensor-modulo":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-tensor-modulo` to
         tensor-1 and tensor-2 produces the same result as applying
         `tensor-modulo` to the same Tensors.
         ```
    strict-result     = strict-tensor-modulo(tensor-1, tensor-2)
    non-strict-result = tensor-modulo(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 1, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for divide-by-zero errors:
  strict-tensor-modulo([tensor: 1], [tensor: 0])
    raises "The second input Tensor cannot contain 0"
  strict-tensor-modulo([tensor: 1, 1], [tensor: 1, 0])
    raises "The second input Tensor cannot contain 0"
  strict-tensor-modulo([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])
    raises "The second input Tensor cannot contain 0"

  # Check for shape strictness:
  strict-tensor-modulo(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-modulo(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-modulo(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-tensor-expt":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-tensor-expt` to
         tensor-1 and tensor-2 produces the same result as applying
         `tensor-expt` to the same Tensors.
         ```
    strict-result     = strict-tensor-expt(tensor-1, tensor-2)
    non-strict-result = tensor-expt(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 1, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-tensor-expt(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-expt(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-tensor-expt(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

check "strict-squared-difference":
  fun check-strict-equals-non-strict(tensor-1 :: Tensor, tensor-2 :: Tensor):
    doc: ```
         Helper function to test if applying `strict-squared-difference` to
         tensor-1 and tensor-2 produces the same result as applying
         `squared-difference` to the same Tensors.
         ```
    strict-result     = strict-squared-difference(tensor-1, tensor-2)
    non-strict-result = squared-difference(tensor-1, tensor-2)
    strict-result.data-now() is-roughly non-strict-result.data-now()
  end

  # Check one-dimensional usages:
  check-strict-equals-non-strict([tensor: 1], [tensor: 1])
  check-strict-equals-non-strict([tensor: 1], [tensor: 0])
  check-strict-equals-non-strict([tensor: 1, 1], [tensor: 1, 0])
  check-strict-equals-non-strict([tensor: 1, 5], [tensor: 9, 8])
  check-strict-equals-non-strict([tensor: -4, -1], [tensor: -8, -2])
  check-strict-equals-non-strict([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])

  # Check multi-dimensional usages:
  two-dim-1 = [tensor: 9, 8, 7, 6].as-2d(2, 2)
  two-dim-2 = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  two-dim-3 = [tensor: 1, 2, 4, 6, 8, 10].as-2d(3, 2)
  check-strict-equals-non-strict(two-dim-1, two-dim-2)
  check-strict-equals-non-strict(two-dim-3, two-dim-3)

  # Check for shape strictness:
  strict-squared-difference(two-dim-1, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-squared-difference(two-dim-2, two-dim-3)
    raises "The first tensor does not have the same shape as the second tensor"
  strict-squared-difference(two-dim-3, two-dim-1)
    raises "The first tensor does not have the same shape as the second tensor"
end

###########################
## Basic Math Operations ##
###########################

#|
check "tensor-abs":

end

check "tensor-acos":

end

check "tensor-acosh":

end

check "tensor-asin":

end

check "tensor-asinh":

end

check "tensor-atan":

end

check "tensor-atan2":

end

check "tensor-atanh":

end

check "tensor-ceil":

end

check "clip-by-value":

end

check "tensor-cos":

end

check "tensor-cosh":

end

check "exponential-linear-units":

end

check "elu":

end

check "gauss-error":

end

check "erf":

end

check "tensor-exp":

end

check "tensor-exp-min1":

end

check "tensor-floor":

end

check "leaky-relu":

end

check "tensor-log":

end

check "tensor-log-plus1":

end

check "log-sigmoid":

end

check "tensor-negate":

end

check "parametric-relu":

end

check "tensor-reciprocal":

end

check "relu":

end

check "tensor-round":

end

check "reciprocal-sqrt":

end

check "scaled-elu":

end

check "sigmoid":

end

check "signed-ones":

end

check "tensor-sin":

end

check "tensor-sinh":

end

check "softplus":

end

check "tensor-sqrt":

end

check "tensor-square":

end

check "step":

end

check "tensor-tan":

end

check "tensor-tanh":

end
|#

##########################
## Reduction Operations ##
##########################

#|
check "all":

end

check "any":

end

check "arg-max":

end

check "arg-min":

end

check "log-sum-exp":

end

check "reduce-max":

end

check "reduce-min":

end

check "reduce-mean":

end

check "reduce-sum":

end
|#

####################################
## Slicing and Joining Operations ##
####################################

#|
check "concatenate":

end
|#

check "gather":
  input-1   = [tensor: 1, 2, 3, 4]
  indices-1 = [tensor: 1, 3, 3].to-int()
  gather(input-1, indices-1, none).data-now()
    is-roughly [list: 2, 4, 4]

  input-2   = [tensor: 1, 2, 3, 4].as-2d(2, 2)
  indices-2 = [tensor: 1, 1, 0].to-int()
  gather(input-2, indices-2, none).data-now()
    is-roughly [list: 3, 4, 3, 4, 1, 2]

  float-indices = [tensor: 1, 1, 0].to-float()
  gather(input-2, float-indices, none).data-now()
    raises "The `indices` argument to `gather` must have a data type of 'int32'"
end

#|
check "reverse":

end

check "slice":

end

check "split":

end

check "stack":

end

check "tile":

end

check "unstack":

end

check "strided-slice":

end
|#
