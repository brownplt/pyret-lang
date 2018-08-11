include tensorflow

######################
## Tensor Functions ##
######################

check "[tensor: ...] convenience constructor":
  [tensor: ] raises "A tensor must contain at least 1 value"

  [tensor: 1] does-not-raise
  [tensor: 1.32] does-not-raise
  [tensor: ~9.21] does-not-raise
  [tensor: -43] does-not-raise
  [tensor: ~9.21, 4, 0.12, -43] does-not-raise
end

check "is-tensor":
  is-tensor([tensor: 1, 2, 3]) is true
  is-tensor(true) is false
  is-tensor(0) is false
  is-tensor([list: 1, 2, 3]) is false
end

check "list-to-tensor":
  list-to-tensor(empty) raises "A tensor must contain at least 1 value"

  list-to-tensor([list: 1]) satisfies is-tensor
  list-to-tensor([list: 4, 5, 6]) satisfies is-tensor
  list-to-tensor([list: 5, 3, 4, 7]) satisfies is-tensor

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
  make-variable([tensor: 1]).data-now() is-roughly [list: 1]

  # Check that we can perform normal Tensor operations on mutable Tensors:
  two-dim = [tensor: 4, 5, 3, 9].as-2d(2, 2)
  make-variable(two-dim).size() is 4
  make-variable(two-dim).shape() is [list: 2, 2]
  make-variable(two-dim).data-now() is-roughly [list: 4, 5, 3, 9]
  make-variable(two-dim).as-3d(4, 1, 1).shape() is [list: 4, 1, 1]
end

###################
## TensorBuffers ##
###################

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
  [tensor: 1].data-now() is-roughly [list: 1]
  [tensor: 1.43].data-now() is-roughly [list: 1.43]
  [tensor: -3.21, 9.4, 0.32].data-now() is-roughly [list: -3.21, 9.4, 0.32]
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
  some-shape  = [list: 2, 2]
  some-values = [list: 4, 5, 9, 3]
  some-tensor = list-to-tensor(some-values).reshape(some-shape)
  some-buffer = some-tensor.to-buffer()
  some-buffer satisfies is-tensor-buffer
  some-buffer.get-all-now() is-roughly some-values
  some-buffer.to-tensor().shape() is some-shape
end

check "Tensor .to-variable":
  [tensor: 4, 5, 1].to-variable() does-not-raise
  [tensor: 0, 5, 1, 9, 8, 4].as-2d(3, 2).to-variable() does-not-raise
end

check "Tensor .reshape":
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
    raises "The argument Tensor cannot contain 0"
  divide-tensors([tensor: 1], [tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  divide-tensors([tensor: 4.23], [tensor: 7.65, 1.43, 0, 2.31])
    raises "The argument Tensor cannot contain 0"
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
    raises "The argument Tensor cannot contain 0"
  floor-divide-tensors([tensor: 1], [tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  floor-divide-tensors([tensor: 4.23], [tensor: 7.65, 1.43, 0])
    raises "The argument Tensor cannot contain 0"
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
    raises "The argument Tensor cannot contain 0"
  tensor-modulo([tensor: 1], [tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  tensor-modulo([tensor: 4.23], [tensor: 7.65, 1.43, 0, 2.31])
    raises "The argument Tensor cannot contain 0"
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
    raises "The argument Tensor cannot contain 0"
  strict-divide-tensors([tensor: 1, 1], [tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  strict-divide-tensors([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])
    raises "The argument Tensor cannot contain 0"

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
    raises "The argument Tensor cannot contain 0"
  strict-tensor-modulo([tensor: 1, 1], [tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  strict-tensor-modulo([tensor: 4.23, 8.29, 1.01], [tensor: 7.65, 0, 1.43])
    raises "The argument Tensor cannot contain 0"

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

check "tensor-abs":
  # Check one-dimensional usages:
  tensor-abs([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-abs([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-abs([tensor: -1]).data-now() is-roughly [list: 1]
  tensor-abs([tensor: -1, -2, -3]).data-now() is-roughly [list: 1, 2, 3]
  tensor-abs([tensor: 6, 2, -4]).data-now() is-roughly [list: 6, 2, 4]
  tensor-abs([tensor: 21, 0, 32, 2]).data-now() is-roughly [list: 21, 0, 32, 2]
  tensor-abs([tensor: -1, 0, 16, -4]).data-now() is-roughly [list: 1, 0, 16, 4]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-abs = tensor-abs([tensor: -4, 5, -6, -7, -8, 9].as-2d(3, 2))
  two-dim-abs.shape() is [list: 3, 2]
  two-dim-abs.data-now() is-roughly [list: 4, 5, 6, 7, 8, 9]

  three-dim-abs = tensor-abs([tensor: 0, 8, -7, 6, -5, -4, 3, 2].as-3d(2, 2, 2))
  three-dim-abs.shape() is [list: 2, 2, 2]
  three-dim-abs.data-now() is-roughly [list: 0, 8, 7, 6, 5, 4, 3, 2]
end

check "tensor-acos":
  # Check one-dimensional usages:
  tensor-acos([tensor: 1]).data-now() is-roughly [list: 0]
  tensor-acos([tensor: 0]).data-now() is-roughly [list: ~1.5707963]
  tensor-acos([tensor: -1]).data-now() is-roughly [list: ~3.1415927]
  tensor-acos([tensor: 0.5, 0.2, 0.6]).data-now()
    is-roughly [list: ~1.0471975, ~1.3694384, ~0.9272952]

  # Check bounding values:
  tensor-acos([tensor: 10])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"
  tensor-acos([tensor: -1, -2, -3])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"
  tensor-acos([tensor: 6, 2, -4])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"
  tensor-acos([tensor: -1, 0, 16, 1])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-acos = tensor-acos([tensor: 0.5, 0.2, 0.6, 0.6].as-2d(2, 2))
  two-dim-acos.shape() is [list: 2, 2]
  two-dim-acos.data-now()
    is-roughly [list: ~1.0471975, ~1.3694384, ~0.9272952, ~0.9272952]
end

check "tensor-acosh":
  # Check one-dimensional usages:
  tensor-acosh([tensor: 1]).data-now() is-roughly [list: 0]
  tensor-acosh([tensor: 2]).data-now() is-roughly [list: ~1.3169579]
  tensor-acosh([tensor: 4]).data-now() is-roughly [list: ~2.0634369]
  tensor-acosh([tensor: 1, 5, 10, 200]).data-now()
    is-roughly [list: ~0, ~2.2924315, ~2.9932229, ~5.9914584]
  tensor-acosh([tensor: 443, 20, 12, 34, 50]).data-now()
    is-roughly [list: ~6.7867159, ~3.6882536, ~3.1763131, ~4.2192912, ~4.6050701]

  # Check bounding values:
  tensor-acosh([tensor: 0])
    raises "Values in the input Tensor must be at least 1"
  tensor-acosh([tensor: -10])
    raises "Values in the input Tensor must be at least 1"
  tensor-acosh([tensor: 4, 1, 10, 32, -2, 82])
    raises "Values in the input Tensor must be at least 1"

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-acosh = tensor-acosh([tensor: 1, 2, 3, 4, 5, 6].as-2d(2, 3))
  two-dim-acosh.shape() is [list: 2, 3]
  two-dim-acosh.data-now()
    is-roughly [list: ~0, ~1.3169579, ~1.7627471, ~2.0634369, ~2.2924315, ~2.4778885]
end

check "tensor-asin":
  # Check one-dimensional usages:
  tensor-asin([tensor: 1]).data-now() is-roughly [list: ~1.5707963]
  tensor-asin([tensor: 0.5]).data-now() is-roughly [list: ~0.5235987]
  tensor-asin([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-asin([tensor: -0.5]).data-now() is-roughly [list: ~-0.5235987]
  tensor-asin([tensor: -1]).data-now() is-roughly [list: ~-1.5707963]
  tensor-asin([tensor: 0.5, 0.2, 0.6]).data-now()
    is-roughly [list: ~0.5235987, ~0.2013579, ~0.6435011]

  # Check bounding values:
  tensor-asin([tensor: 10])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"
  tensor-asin([tensor: -1, -2, -3])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"
  tensor-asin([tensor: 6, 2, -4])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"
  tensor-asin([tensor: -1, 0, 16, 1])
    raises "Values in the input Tensor must be between -1 and 1, inclusive"

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-asin = tensor-asin([tensor: 0.5, 0.2, 0.6, 0.6].as-2d(2, 2))
  two-dim-asin.shape() is [list: 2, 2]
  two-dim-asin.data-now()
    is-roughly [list: ~0.5235987, ~0.2013579, ~0.6435011, ~0.6435011]
end

check "tensor-asinh":
  # Check one-dimensional usages:
  tensor-asinh([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-asinh([tensor: 1]).data-now() is-roughly [list: ~0.8813736]
  tensor-asinh([tensor: -1]).data-now() is-roughly [list: ~-0.8813736]
  tensor-asinh([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-0.8813736, ~-1.4436353, ~-1.8184462]
  tensor-asinh([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~2.4917798, ~1.4436354, ~-2.0947132]
  tensor-asinh([tensor: 21, 0, 32, 2]).data-now()
    is-roughly [list: ~3.7382359, ~0, ~4.1591272, ~1.4436354]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-asinh = tensor-asinh([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-asinh.shape() is [list: 2, 2]
  two-dim-asinh.data-now()
    is-roughly [list: ~-2.0947132, ~2.3124384, ~-2.4917776, ~-2.6441206]
end

check "tensor-atan":
  # Check one-dimensional usages:
  tensor-atan([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-atan([tensor: 1]).data-now() is-roughly [list: ~0.7853981]
  tensor-atan([tensor: -1]).data-now() is-roughly [list: ~-0.7853981]
  tensor-atan([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-0.7853981, ~-1.1071487, ~-1.2490458]
  tensor-atan([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~1.4056477, ~1.1071487, ~-1.3258177]
  tensor-atan([tensor: 21, 0, 32, 2]).data-now()
    is-roughly [list: ~1.5232132, ~0, ~1.5395565, ~1.1071487]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-atan = tensor-atan([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-atan.shape() is [list: 2, 2]
  two-dim-atan.data-now()
    is-roughly [list: ~-1.3258177, ~1.3734008, ~-1.40564775, ~-1.4288992]
end

check "tensor-atan2":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-atanh":
  # Check one-dimensional usages:
  tensor-atanh([tensor: 0.5]).data-now() is-roughly [list: ~0.5493061]
  tensor-atanh([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-atanh([tensor: -0.5]).data-now() is-roughly [list: ~-0.5493061]
  tensor-atanh([tensor: -0.9]).data-now() is-roughly [list: ~-1.4722193]
  tensor-atanh([tensor: 0.5, 0.2, 0.6]).data-now()
    is-roughly [list: ~0.5493061, ~0.2027325, ~0.6931471]

  # Check bounding values:
  tensor-atanh([tensor: 1])
    raises "Values in the input Tensor must be between -1 and 1, exclusive"
  tensor-atanh([tensor: -1])
    raises "Values in the input Tensor must be between -1 and 1, exclusive"
  tensor-atanh([tensor: 10])
    raises "Values in the input Tensor must be between -1 and 1, exclusive"
  tensor-atanh([tensor: -1, -2, -3])
    raises "Values in the input Tensor must be between -1 and 1, exclusive"
  tensor-atanh([tensor: 6, 2, -4])
    raises "Values in the input Tensor must be between -1 and 1, exclusive"
  tensor-atanh([tensor: 0, 16, -1, 9, 1])
    raises "Values in the input Tensor must be between -1 and 1, exclusive"

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-atanh = tensor-atanh([tensor: 0.5, 0.2, 0.6, 0.6].as-2d(2, 2))
  two-dim-atanh.shape() is [list: 2, 2]
  two-dim-atanh.data-now()
    is-roughly [list:  ~0.5493061, ~0.2027325, ~0.6931471, ~0.6931471]
end

check "tensor-ceil":
  # Check one-dimensional usages on integer tensors:
  tensor-ceil([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-ceil([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-ceil([tensor: -1]).data-now() is-roughly [list: -1]
  tensor-ceil([tensor: -1, -2, -3]).data-now() is-roughly [list: -1, -2, -3]

  # Check one-dimensional usages on float tensors:
  tensor-ceil([tensor: 0.1]).data-now() is-roughly [list: 1]
  tensor-ceil([tensor: 0.3]).data-now() is-roughly [list: 1]
  tensor-ceil([tensor: 0.5]).data-now() is-roughly [list: 1]
  tensor-ceil([tensor: 0.8]).data-now() is-roughly [list: 1]
  tensor-ceil([tensor: 0.999]).data-now() is-roughly [list: 1]
  tensor-ceil([tensor: 1.1]).data-now() is-roughly [list: 2]
  tensor-ceil([tensor: -0.2]).data-now() is-roughly [list: 0]
  tensor-ceil([tensor: -0.5]).data-now() is-roughly [list: 0]
  tensor-ceil([tensor: -0.9]).data-now() is-roughly [list: 0]
  tensor-ceil([tensor: 3.5, 5.2, 1.6]).data-now() is-roughly [list: 4, 6, 2]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-ceil-1 = tensor-ceil([tensor: 0.2, 4.3, 9.3, 10.1].as-2d(2, 2))
  two-dim-ceil-1.shape() is [list: 2, 2]
  two-dim-ceil-1.data-now() is-roughly [list: 1, 5, 10, 11]

  two-dim-ceil-2 = tensor-ceil([tensor: -4, 5, -6, -7, -8, 9].as-2d(3, 2))
  two-dim-ceil-2.shape() is [list: 3, 2]
  two-dim-ceil-2.data-now() is-roughly [list: -4, 5, -6, -7, -8, 9]

  three-dim-ceil = tensor-ceil([tensor: 0, 8, -7, 6, -5, -4, 3, 2].as-3d(2, 2, 2))
  three-dim-ceil.shape() is [list: 2, 2, 2]
  three-dim-ceil.data-now() is-roughly [list: 0, 8, -7, 6, -5, -4, 3, 2]
end

check "clip-by-value":
  # Check one-dimensional usages:
  clip-by-value([tensor: 0], 0, 0).data-now() is-roughly [list: 0]
  clip-by-value([tensor: 0], -1, 1).data-now() is-roughly [list: 0]
  clip-by-value([tensor: 0], 1, 4).data-now() is-roughly [list: 1]
  clip-by-value([tensor: 0], -10, -5).data-now() is-roughly [list: -5]
  clip-by-value([tensor: 21, 0, 32, 2], 4, 9).data-now()
    is-roughly [list: 9, 4, 9, 4]
  clip-by-value([tensor: 3, 9, 10, 3.24], 4.5, 9.4).data-now()
    is-roughly [list: 4.5, 9, 9.4, 4.5]
  clip-by-value([tensor: 4, 9, 300, 21, 100, 78], 100, 100).data-now()
    is-roughly [list: 100, 100, 100, 100, 100, 100]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim = [tensor: -4, 5, -6, 8].as-2d(2, 2)
  clip-by-value(two-dim, 3, 6).shape() is [list: 2, 2]
  clip-by-value(two-dim, 3, 6).data-now() is-roughly [list: 3, 5, 3, 6]

  # Check error handling when min is greater than max:
  clip-by-value([tensor: 1], 10, 0)
    raises "minimum value to clip to must be less than or equal to the maximum"
  clip-by-value([tensor: 1], -10, -45)
    raises "minimum value to clip to must be less than or equal to the maximum"
end

check "tensor-cos":
  # Check one-dimensional usages:
  tensor-cos([tensor: 0]).data-now() is-roughly [list: 1]
  tensor-cos([tensor: 1]).data-now() is-roughly [list: ~0.5403115]
  tensor-cos([tensor: -1]).data-now() is-roughly [list: ~0.5403116]
  tensor-cos([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~0.5403116, ~-0.4161522, ~-0.9900057]
  tensor-cos([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~0.9601798, ~-0.4161523, ~-0.6536576]
  tensor-cos([tensor: 21, 0, 32, 2]).data-now()
    is-roughly [list: ~-0.5477288, ~1, ~0.8342252, ~-0.4161523]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-cos = tensor-cos([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-cos.shape() is [list: 2, 2]
  two-dim-cos.data-now()
    is-roughly [list: ~-0.6536576, ~0.2836650, ~0.9601799, ~0.7539221]
end

check "tensor-cosh":
  # Check one-dimensional usages:
  tensor-cosh([tensor: 0]).data-now() is-roughly [list: 1]
  tensor-cosh([tensor: 1]).data-now() is-roughly [list: ~1.5430805]
  tensor-cosh([tensor: -1]).data-now() is-roughly [list: ~1.5430805]
  tensor-cosh([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~1.5430805, ~3.7621955, ~10.0676612]
  tensor-cosh([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~201.7155914, ~3.7621958, ~27.3082313]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-cosh = tensor-cosh([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-cosh.shape() is [list: 2, 2]
  two-dim-cosh.data-now()
    is-roughly [list: ~27.3082313, ~74.2099533, ~201.7155914, ~548.3170776]
end

check "exponential-linear-units":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "elu":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "gauss-error":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "erf":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-exp":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-exp-min1":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-floor":
  # Check one-dimensional usages on integer tensors:
  tensor-floor([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-floor([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-floor([tensor: -1]).data-now() is-roughly [list: -1]
  tensor-floor([tensor: -1, -2, -3]).data-now() is-roughly [list: -1, -2, -3]

  # Check one-dimensional usages on float tensors:
  tensor-floor([tensor: 0.1]).data-now() is-roughly [list: 0]
  tensor-floor([tensor: 0.3]).data-now() is-roughly [list: 0]
  tensor-floor([tensor: 0.5]).data-now() is-roughly [list: 0]
  tensor-floor([tensor: 0.8]).data-now() is-roughly [list: 0]
  tensor-floor([tensor: 0.999]).data-now() is-roughly [list: 0]
  tensor-floor([tensor: 1.1]).data-now() is-roughly [list: 1]
  tensor-floor([tensor: -0.2]).data-now() is-roughly [list: -1]
  tensor-floor([tensor: -0.5]).data-now() is-roughly [list: -1]
  tensor-floor([tensor: -0.9]).data-now() is-roughly [list: -1]
  tensor-floor([tensor: 3.5, 5.2, 1.6]).data-now() is-roughly [list: 3, 5, 1]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-floor-1 = tensor-floor([tensor: 0.2, 4.3, 9.3, 10.1].as-2d(2, 2))
  two-dim-floor-1.shape() is [list: 2, 2]
  two-dim-floor-1.data-now() is-roughly [list: 0, 4, 9, 10]

  two-dim-floor-2 = tensor-floor([tensor: -4, 5, -6, -7, -8, 9].as-2d(3, 2))
  two-dim-floor-2.shape() is [list: 3, 2]
  two-dim-floor-2.data-now() is-roughly [list: -4, 5, -6, -7, -8, 9]

  three-dim-floor = tensor-floor([tensor: 0, 8, -7, 6, -5, -4, 3, 2].as-3d(2, 2, 2))
  three-dim-floor.shape() is [list: 2, 2, 2]
  three-dim-floor.data-now() is-roughly [list: 0, 8, -7, 6, -5, -4, 3, 2]
end

check "leaky-relu":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-log":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-log-plus1":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "log-sigmoid":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-negate":
  # Check one-dimensional usages on integer tensors:
  tensor-negate([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-negate([tensor: 1]).data-now() is-roughly [list: -1]
  tensor-negate([tensor: -1]).data-now() is-roughly [list: 1]
  tensor-negate([tensor: -1, 2, 3, -4, 5]).data-now()
    is-roughly [list: 1, -2, -3, 4, -5]
  tensor-negate([tensor: -1, -2, -3, -4, -5]).data-now()
    is-roughly [list: 1, 2, 3, 4, 5]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-negate = tensor-negate([tensor: 0.2, -4.3, 9.3, -10.1].as-2d(2, 2))
  two-dim-negate.shape() is [list: 2, 2]
  two-dim-negate.data-now() is-roughly [list: -0.2, 4.3, -9.3, 10.1]

  three-dim-negate = tensor-negate([tensor: 0, 8, -7, 6, -5, -4, 3, 2].as-3d(2, 2, 2))
  three-dim-negate.shape() is [list: 2, 2, 2]
  three-dim-negate.data-now() is-roughly [list: -0, -8, 7, -6, 5, 4, -3, -2]
end

check "parametric-relu":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-reciprocal":
  # Check one-dimensional usages on integer tensors:
  tensor-reciprocal([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-reciprocal([tensor: -1]).data-now() is-roughly [list: -1]
  tensor-reciprocal([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-1, ~-0.5, ~-0.3333333]
  tensor-reciprocal([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~0.1666666, ~0.5, ~-0.25]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-reciprocal = tensor-reciprocal([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-reciprocal.shape() is [list: 2, 2]
  two-dim-reciprocal.data-now()
    is-roughly [list: ~-0.25, ~0.2, ~-0.1666666, ~-0.1428571]

  # Check for division-by-zero errors:
  tensor-reciprocal([tensor: 0])
    raises "The argument Tensor cannot contain 0"
  tensor-reciprocal([tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  tensor-reciprocal([tensor: 7.65, 0, 1.43])
    raises "The argument Tensor cannot contain 0"
end

check "relu":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-round":
  # Check one-dimensional usages on integer tensors:
  tensor-round([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-round([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-round([tensor: -1]).data-now() is-roughly [list: -1]
  tensor-round([tensor: -1, -2, -3]).data-now() is-roughly [list: -1, -2, -3]

  # Check weird behavior with rounding on Roughnums (TensorFlow.js bug?):
  tensor-round([tensor: 0.5]).data-now() is-roughly [list: 0] # rounds down?
  tensor-round([tensor: 3.5]).data-now() is-roughly [list: 4] # rounds up?

  # Check one-dimensional usages on float tensors:
  tensor-round([tensor: 0.1]).data-now() is-roughly [list: 0]
  tensor-round([tensor: 0.3]).data-now() is-roughly [list: 0]
  tensor-round([tensor: 0.8]).data-now() is-roughly [list: 1]
  tensor-round([tensor: 0.999]).data-now() is-roughly [list: 1]
  tensor-round([tensor: 1.1]).data-now() is-roughly [list: 1]
  tensor-round([tensor: -0.2]).data-now() is-roughly [list: 0]
  tensor-round([tensor: -0.7]).data-now() is-roughly [list: -1]
  tensor-round([tensor: 3.5, 5.2, 1.6]).data-now() is-roughly [list: 4, 5, 2]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-round-1 = tensor-round([tensor: 0.2, 4.3, 9.3, 10.1].as-2d(2, 2))
  two-dim-round-1.shape() is [list: 2, 2]
  two-dim-round-1.data-now() is-roughly [list: 0, 4, 9, 10]

  two-dim-round-2 = tensor-round([tensor: -4, 5, -6, -7, -8, 9].as-2d(3, 2))
  two-dim-round-2.shape() is [list: 3, 2]
  two-dim-round-2.data-now() is-roughly [list: -4, 5, -6, -7, -8, 9]

  three-dim-round = tensor-round([tensor: 0, 8, -7, 6, -5, -4, 3, 2].as-3d(2, 2, 2))
  three-dim-round.shape() is [list: 2, 2, 2]
  three-dim-round.data-now() is-roughly [list: 0, 8, -7, 6, -5, -4, 3, 2]
end

check "reciprocal-sqrt":
  # Check one-dimensional usages on integer tensors:
  reciprocal-sqrt([tensor: 1]).data-now() is-roughly [list: 1]
  reciprocal-sqrt([tensor: -1]).data-now() is-roughly [list: 1]
  reciprocal-sqrt([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~1, ~0.7071067, ~0.5773502]
  reciprocal-sqrt([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~0.4082482, ~0.7071067, ~0.5]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-reciprocal = reciprocal-sqrt([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-reciprocal.shape() is [list: 2, 2]
  two-dim-reciprocal.data-now()
    is-roughly [list: ~0.5, ~0.4472135, ~0.4082482, ~0.3779644]

  # Check for division-by-zero errors:
  reciprocal-sqrt([tensor: 0])
    raises "The argument Tensor cannot contain 0"
  reciprocal-sqrt([tensor: 1, 0])
    raises "The argument Tensor cannot contain 0"
  reciprocal-sqrt([tensor: 7.65, 0, 1.43])
    raises "The argument Tensor cannot contain 0"
end

check "scaled-elu":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "sigmoid":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "signed-ones":
  # Check one-dimensional usages:
  signed-ones([tensor: 0]).data-now() is-roughly [list: 0]
  signed-ones([tensor: 1]).data-now() is-roughly [list: 1]
  signed-ones([tensor: 3]).data-now() is-roughly [list: 1]
  signed-ones([tensor: 5]).data-now() is-roughly [list: 1]
  signed-ones([tensor: -1]).data-now() is-roughly [list: -1]
  signed-ones([tensor: -3]).data-now() is-roughly [list: -1]
  signed-ones([tensor: -5]).data-now() is-roughly [list: -1]
  signed-ones([tensor: 9, -7, 5, -3, -1, 0]).data-now()
    is-roughly [list: 1, -1, 1, -1, -1, 0]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-signed = signed-ones([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-signed.shape() is [list: 2, 2]
  two-dim-signed.data-now()
    is-roughly [list: -1, 1, -1, -1]
end

check "tensor-sin":
  # Check one-dimensional usages:
  tensor-sin([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-sin([tensor: 1]).data-now() is-roughly [list: ~0.8414709]
  tensor-sin([tensor: -1]).data-now() is-roughly [list: ~-0.8415220]
  tensor-sin([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-0.8415220, ~-0.9092977, ~-0.1411204]
  tensor-sin([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~-0.2794162, ~0.9092976, ~0.7568427]
  tensor-sin([tensor: 21, 0, 32, 2]).data-now()
    is-roughly [list: ~0.8366656, ~0, ~0.5514304, ~0.9092976]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-sin = tensor-sin([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-sin.shape() is [list: 2, 2]
  two-dim-sin.data-now()
    is-roughly [list: ~0.7568427, ~-0.9589251, ~0.2794161, ~-0.6570168]
end

check "tensor-sinh":
  # Check one-dimensional usages:
  tensor-sinh([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-sinh([tensor: 1]).data-now() is-roughly [list: ~1.1752011]
  tensor-sinh([tensor: -1]).data-now() is-roughly [list: ~-1.1752011]
  tensor-sinh([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-1.1752011, ~-3.6268603, ~-10.0178737]
  tensor-sinh([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~201.7131195, ~3.6268601, ~-27.2899169]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-sinh = tensor-sinh([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-sinh.shape() is [list: 2, 2]
  two-dim-sinh.data-now()
    is-roughly [list: ~-27.2899169, ~74.2032089, ~-201.7131195, ~-548.3162231]
end

check "softplus":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tensor-sqrt":
  # Check one-dimensional usages:
  tensor-sqrt([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-sqrt([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-sqrt([tensor: 4]).data-now() is-roughly [list: 2]
  tensor-sqrt([tensor: 9]).data-now() is-roughly [list: 3]
  tensor-sqrt([tensor: 25]).data-now() is-roughly [list: 5]

  # Check handling of negative values:
  tensor-sqrt([tensor: -1]).data-now()
    raises "Values in the input Tensor must be at least 0"
  tensor-sqrt([tensor: -3]).data-now()
    raises "Values in the input Tensor must be at least 0"
  tensor-sqrt([tensor: -5]).data-now()
    raises "Values in the input Tensor must be at least 0"
  tensor-sqrt([tensor: 9, -7, 5, -3, -1, 0, 0.5]).data-now()
    raises "Values in the input Tensor must be at least 0"

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-sqrt = tensor-sqrt([tensor: 4, 16, 25, 0].as-2d(2, 2))
  two-dim-sqrt.shape() is [list: 2, 2]
  two-dim-sqrt.data-now()
    is-roughly [list: 2, 4, 5, 0]
end

check "tensor-square":
  # Check one-dimensional usages:
  tensor-square([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-square([tensor: 1]).data-now() is-roughly [list: 1]
  tensor-square([tensor: 3]).data-now() is-roughly [list: 9]
  tensor-square([tensor: 5]).data-now() is-roughly [list: 25]
  tensor-square([tensor: -1]).data-now() is-roughly [list: 1]
  tensor-square([tensor: -3]).data-now() is-roughly [list: 9]
  tensor-square([tensor: -5]).data-now() is-roughly [list: 25]
  tensor-square([tensor: 9, -7, 5, -3, -1, 0, 0.5]).data-now()
    is-roughly [list: 81, 49, 25, 9, 1, 0, 0.25]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-squared = tensor-square([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-squared.shape() is [list: 2, 2]
  two-dim-squared.data-now()
    is-roughly [list: 16, 25, 36, 49]
end

check "step":
  # Check one-dimensional usages:
  step([tensor: 0]).data-now() is-roughly [list: 0]
  step([tensor: 1]).data-now() is-roughly [list: 1]
  step([tensor: 3]).data-now() is-roughly [list: 1]
  step([tensor: 5]).data-now() is-roughly [list: 1]
  step([tensor: -1]).data-now() is-roughly [list: 0]
  step([tensor: -3]).data-now() is-roughly [list: 0]
  step([tensor: -5]).data-now() is-roughly [list: 0]
  step([tensor: 9, -7, 5, -3, -1, 0]).data-now()
    is-roughly [list: 1, 0, 1, 0, 0, 0]
  step([tensor: -1, 4, 0, 0, 15, -43, 0]).data-now()
    is-roughly [list: 0, 1, 0, 0, 1, 0, 0]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-stepped = step([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-stepped.shape() is [list: 2, 2]
  two-dim-stepped.data-now()
    is-roughly [list: 0, 1, 0, 0]
end

check "tensor-tan":
  # Check one-dimensional usages:
  tensor-tan([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-tan([tensor: 1]).data-now() is-roughly [list: ~1.5573809]
  tensor-tan([tensor: -1]).data-now() is-roughly [list: ~-1.5573809]
  tensor-tan([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-1.5573809, ~2.1850113, ~0.1425446]
  tensor-tan([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~-0.2910035, ~-2.1850113, ~-1.1578584]
  tensor-tan([tensor: 21, 0, 32, 2]).data-now()
    is-roughly [list: ~-1.5275151, ~0, ~0.6610110, ~-2.1850113]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-tan = tensor-tan([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-tan.shape() is [list: 2, 2]
  two-dim-tan.data-now()
    is-roughly [list: ~-1.1578584, ~-3.3804838, ~0.2910035, ~-0.8714656]
end

check "tensor-tanh":
  # Check one-dimensional usages:
  tensor-tanh([tensor: 0]).data-now() is-roughly [list: 0]
  tensor-tanh([tensor: 1]).data-now() is-roughly [list: ~0.7615941]
  tensor-tanh([tensor: -1]).data-now() is-roughly [list: ~-0.7615941]
  tensor-tanh([tensor: -1, -2, -3]).data-now()
    is-roughly [list: ~-0.7615941, ~-0.9640275, ~-0.9950547]
  tensor-tanh([tensor: 6, 2, -4]).data-now()
    is-roughly [list: ~0.9999876, ~0.9640275, ~-0.9993293]
  tensor-tanh([tensor: 21, 0, 32, 2]).data-now()
    is-roughly [list: ~1, ~0, ~1, ~0.9640275]

  # Check operation preserves shape of multi-dimensional tensors:
  two-dim-tanh = tensor-tanh([tensor: -4, 5, -6, -7].as-2d(2, 2))
  two-dim-tanh.shape() is [list: 2, 2]
  two-dim-tanh.data-now()
    is-roughly [list: ~-0.9993293, ~0.9999091, ~-0.9999876, ~-0.9999983]
end

##########################
## Reduction Operations ##
##########################

check "all":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "any":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "arg-max":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "arg-min":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "log-sum-exp":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "reduce-max":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "reduce-min":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "reduce-mean":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "reduce-sum":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

####################################
## Slicing and Joining Operations ##
####################################

check "concatenate":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

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

check "reverse":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "slice":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "split":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "stack":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "tile":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "unstack":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

check "strided-slice":
  # TODO(ZacharyEspiritu): Add tests for this function
  true is true
end

############################
## TensorBuffer Functions ##
############################

check "is-tensor-buffer":
  is-tensor-buffer(make-buffer([list: 1])) is true
  is-tensor-buffer(make-buffer([list: 9, 4])) is true
  is-tensor-buffer(make-buffer([list: 8, 4, 10])) is true
  is-tensor-buffer(43) is false
  is-tensor-buffer("not a buffer") is false
  is-tensor-buffer({some: "thing"}) is false
end

check "make-buffer":
  make-buffer([list: 1]).size() is 1
  make-buffer([list: 1]).shape() is [list: 1]
  make-buffer([list: 9, 5]).size() is 45
  make-buffer([list: 9, 5]).shape() is [list: 9, 5]

  # Check for error handling of rank-0 shapes:
  make-buffer(empty) raises "input shape List had zero elements"

  # Check for error handling of less than zero dimension sizes:
  make-buffer([list: 0]) raises "Cannot create TensorBuffer"
  make-buffer([list: -1]) raises "Cannot create TensorBuffer"
  make-buffer([list: 1, 0]) raises "Cannot create TensorBuffer"
  make-buffer([list: 4, 5, 0, 3]) raises "Cannot create TensorBuffer"
  make-buffer([list: 2, -5, -1, 4]) raises "Cannot create TensorBuffer"
  make-buffer([list: 4, 5, 0, 3, 0]) raises "Cannot create TensorBuffer"
end

##########################
## TensorBuffer Methods ##
##########################

check "TensorBuffer .size":
  make-buffer([list: 1]).size() is 1
  make-buffer([list: 4]).size() is 4
  make-buffer([list: 3, 2]).size() is 6
  make-buffer([list: 4, 4]).size() is 16
  make-buffer([list: 4, 3, 5]).size() is 60
end

check "TensorBuffer .shape":
  make-buffer([list: 1]).shape() is [list: 1]
  make-buffer([list: 4, 3]).shape() is [list: 4, 3]
  make-buffer([list: 2, 4, 1]).shape() is [list: 2, 4, 1]
  make-buffer([list: 4, 3, 5]).shape() is [list: 4, 3, 5]
end

check "TensorBuffer .set-now":
  # Check size-1 buffer usage:
  buffer-1 = make-buffer([list: 1])
  buffer-1.set-now(3, [list: 0])
  buffer-1.get-all-now() is-roughly [list: 3]

  # Check long, one-dimensional buffer usage:
  buffer-2 = make-buffer([list: 7])
  buffer-2.set-now(-45, [list: 0])
  buffer-2.set-now(9, [list: 2])
  buffer-2.set-now(0, [list: 4])
  buffer-2.set-now(-3.42, [list: 6])

  buffer-2.get-all-now() is-roughly [list: -45, 0, 9, 0, 0, 0, -3.42]
  buffer-2.to-tensor().shape() is [list: 7]
  buffer-2.to-tensor().data-now() is-roughly [list: -45, 0, 9, 0, 0, 0, -3.42]

  # Check out-of-bounds coordinates:
  buffer-2.set-now(10, [list: -1])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"
  buffer-2.set-now(10, [list: 8])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"
  buffer-2.set-now(10, [list: 10])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"

  # Check too little coordinates:
  buffer-2.set-now(10, [list:])
    raises "number of supplied coordinates must match the rank"

  # Check too many coordinates:
  buffer-2.set-now(10, [list: 9, 5])
    raises "number of supplied coordinates must match the rank"
  buffer-2.set-now(10, [list: 9, 5, 20])
    raises "number of supplied coordinates must match the rank"

  # Check multi-dimensional buffer usage:
  buffer-3 = make-buffer([list: 2, 2])
  buffer-3.set-now(4, [list: 0, 0])
  buffer-3.set-now(3, [list: 0, 1])
  buffer-3.set-now(2, [list: 1, 0])
  buffer-3.set-now(1, [list: 1, 1])

  buffer-3.get-all-now() is-roughly [list: 4, 3, 2, 1]
  buffer-3.to-tensor().shape() is [list: 2, 2]
  buffer-3.to-tensor().data-now() is-roughly [list: 4, 3, 2, 1]

  # Check out-of-bounds coordinates:
  buffer-3.set-now(10, [list: -1, 0])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"
  buffer-3.set-now(10, [list: 2, 2])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"

  # Check too many coordinates:
  buffer-3.set-now(10, [list: 1, 2, 3])
    raises "number of supplied coordinates must match the rank"
end

check "TensorBuffer .get-now":
  # Check one-dimensional buffer usage:
  one-dim-buffer = make-buffer([list: 7])
  one-dim-buffer.set-now(-45, [list: 0])
  one-dim-buffer.set-now(9, [list: 2])
  one-dim-buffer.set-now(0, [list: 4])
  one-dim-buffer.set-now((4 / 3), [list: 5])
  one-dim-buffer.set-now(-3.42, [list: 6])

  one-dim-buffer.get-now([list: 0]) is-roughly -45
  one-dim-buffer.get-now([list: 1]) is-roughly 0
  one-dim-buffer.get-now([list: 2]) is-roughly 9
  one-dim-buffer.get-now([list: 3]) is-roughly 0
  one-dim-buffer.get-now([list: 4]) is-roughly 0
  one-dim-buffer.get-now([list: 5]) is-roughly (4 / 3)
  one-dim-buffer.get-now([list: 6]) is-roughly -3.42

  # Check multi-dimensional buffer usage:
  two-dim-buffer = make-buffer([list: 2, 2])
  two-dim-buffer.set-now(4, [list: 0, 0])
  two-dim-buffer.set-now(3, [list: 0, 1])
  two-dim-buffer.set-now(2, [list: 1, 0])
  two-dim-buffer.set-now(1, [list: 1, 1])

  two-dim-buffer.get-now([list: 0, 0]) is-roughly 4
  two-dim-buffer.get-now([list: 0, 1]) is-roughly 3
  two-dim-buffer.get-now([list: 1, 0]) is-roughly 2
  two-dim-buffer.get-now([list: 1, 1]) is-roughly 1

  # Check out-of-bounds coordinates:
  two-dim-buffer.set-now(10, [list: -1, 0])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"
  two-dim-buffer.set-now(10, [list: 2, 2])
    raises "Coordinates must be within the bounds of the TensorBuffer's shape"

  # Check too many coordinates:
  two-dim-buffer.set-now(10, [list: 1, 2, 3])
    raises "number of supplied coordinates must match the rank"
end

check "TensorBuffer .get-all-now":
  # Check works on newly initialized buffer with no set values:
  no-set-buffer = make-buffer([list: 2, 4])
  no-set-buffer.get-all-now() is-roughly [list: 0, 0, 0, 0, 0, 0, 0, 0]

  # Check one-dimensional buffer usage:
  one-dim-buffer = make-buffer([list: 7])
  one-dim-buffer.set-now(-45, [list: 0])
  one-dim-buffer.set-now(9, [list: 2])
  one-dim-buffer.set-now(0, [list: 4])
  one-dim-buffer.set-now((4 / 3), [list: 5])
  one-dim-buffer.set-now(-3.42, [list: 6])

  one-dim-buffer.get-all-now() is-roughly [list: -45, 0, 9, 0, 0, (4 / 3), -3.42]

  # Check multi-dimensional buffer usage:
  two-dim-buffer = make-buffer([list: 2, 2])
  two-dim-buffer.set-now(4, [list: 0, 0])
  two-dim-buffer.set-now(3, [list: 0, 1])
  two-dim-buffer.set-now(2, [list: 1, 0])
  two-dim-buffer.set-now(1, [list: 1, 1])

  two-dim-buffer.get-all-now() is-roughly [list: 4, 3, 2, 1]
end

check "TensorBuffer .to-tensor":
  # Check size-1 buffer usage:
  buffer-1 = make-buffer([list: 1])
  buffer-1.set-now(3, [list: 0])

  buffer-1.to-tensor().shape() is [list: 1]
  buffer-1.to-tensor().data-now() is-roughly [list: 3]

  # Check one-dimensional buffer usage:
  buffer-2 = make-buffer([list: 7])
  buffer-2.set-now(-45, [list: 0])
  buffer-2.set-now(9, [list: 2])
  buffer-2.set-now(0, [list: 4])
  buffer-2.set-now(-3.42, [list: 6])

  buffer-2.to-tensor().shape() is [list: 7]
  buffer-2.to-tensor().data-now() is-roughly [list: -45, 0, 9, 0, 0, 0, -3.42]

  # Check multi-dimensional buffer usage:
  buffer-3 = make-buffer([list: 2, 2])
  buffer-3.set-now(4, [list: 0, 0])
  buffer-3.set-now(3, [list: 0, 1])
  buffer-3.set-now(2, [list: 1, 0])
  buffer-3.set-now(1, [list: 1, 1])

  buffer-3.to-tensor().shape() is [list: 2, 2]
  buffer-3.to-tensor().data-now() is-roughly [list: 4, 3, 2, 1]
end
