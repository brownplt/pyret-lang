import tensorflow as TF

# Tensor Functions

check "is-tensor":
  is-tensor([tensor: 1, 2, 3]) is true
  is-tensor(true) is false
  is-tensor(0) is false
  is-tensor([list: 1, 2, 3]) is false
end

check "Tensor .size":
  make-scalar(4.21).size() is 1
  [TF.tensor: 6.32].size() is 1
  [TF.tensor: 1, 2, 3].size() is 3
  [TF.tensor: 1.4, 5.2, 0.4, 12.4, 14.3, 6].as-2d(3, 2).size() is 6
end

check "Tensor .shape":
  make-scalar(3).shape() is empty
  [TF.tensor: 9].shape() is [list: 1]
  [TF.tensor: 8, 3, 1].shape() is [list: 3]
  [TF.tensor: 0, 0, 0, 0, 0, 0].as-2d(3, 2).shape() is [list: 3, 2]
end

check "Tensor .flatten":
  a = [TF.tensor: 1, 2, 3, 4, 5, 6].as-2d(3, 2)
  a.shape() is [list: 3, 2]
  a.flatten().shape() is [list: 6]

  b = make-scalar(12)
  b.shape() is empty
  b.flatten().shape() is [list: 1]
end

check "Tensor .as-scalar":
  size-one = [TF.tensor: 1]
  size-one.as-scalar().shape() is empty
  size-one.shape() is [list: 1] # doesn't modify shape of original tensor

  size-two = [TF.tensor: 1, 2]
  size-two.as-scalar() raises
    "Tensor was size-2 but `as-scalar` requires the tensor to be size-1"
end

check "Tensor .as-1d":
  one-dim = [TF.tensor: 1]
  two-dim = [TF.tensor: 4, 3, 2, 1].as-2d(2, 2)
  three-dim = [TF.tensor: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9].as-3d(3, 1, 3)

  one-dim.shape() is [list: 1]
  one-dim.as-1d().shape() is [list: 1]

  two-dim.shape() is [list: 2, 2]
  two-dim.as-1d().shape() is [list: 4]

  three-dim.shape() is [list: 3, 1, 3]
  three-dim.as-1d().shape() is [list: 9]
end

check "Tensor .as-2d":
  one-dim = [TF.tensor: 1]
  two-dim = [TF.tensor: 0, 1, 2, 3, 4, 5].as-2d(3, 2)
  three-dim = [TF.tensor: 4, 3, 2, 1, 0, -1, -2, -3].as-3d(2, 2, 2)

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
  one-dim = [TF.tensor: 1]
  two-dim = [TF.tensor: 0, 1, 2, 3, 4, 5, 6, 7].as-2d(4, 2)

  one-dim.shape() is [list: 1]
  one-dim.as-3d(1, 1, 1).shape() is [list: 1, 1, 1]

  two-dim.shape() is [list: 4, 2]
  two-dim.as-3d(2, 2, 2).shape() is [list: 2, 2, 2]

  one-dim.as-3d(2, 1, 1) raises "Cannot reshape"
  two-dim.as-3d(4, 3, 2) raises "Cannot reshape"
end

check "Tensor .as-4d":
  one-dim = [TF.tensor: 1]
  two-dim = [TF.tensor: 0, 1, 2, 3, 4, 5, 6, 7].as-2d(4, 2)

  one-dim.shape() is [list: 1]
  one-dim.as-4d(1, 1, 1, 1).shape() is [list: 1, 1, 1, 1]

  two-dim.shape() is [list: 4, 2]
  two-dim.as-4d(2, 2, 1, 2).shape() is [list: 2, 2, 1, 2]

  one-dim.as-4d(2, 1, 1, 1) raises "Cannot reshape"
  two-dim.as-4d(2, 2, 2, 2) raises "Cannot reshape"
end
