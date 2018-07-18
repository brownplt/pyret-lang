provide *
provide-types *

import global as _
import lists as lists
type List = lists.List

data Tensor<A>:
  | tensor(values :: List<A>)
end

data Model:
  | generic(inputs :: List<Tensor<Any>>, outputs :: List<Tensor<Any>>)
  | sequential
end

data Layer:
  | activation(activation :: ActivationFunction)
  | dense(units :: Number, activation :: ActivationFunction)
  | dropout(rate :: Number, noise-shape :: List<Number>)
  | embedding(input-dimension :: NumPositive, output-dimension :: NumNonNegative)
  | flatten
  | repeat-vector(num-repeats :: Number)
  | reshape(target-shape :: List<Number>)
end

# Represents an activation function for Layer.activation and Layer.dense
data ActivationFunction:
  | elu
  | hard-sigmoid
  | linear
  | relu
  | relu6
  | selu
  | sigmoid
  | softmax
  | softplus
  | softsign
  | tanh
  | other(identifier :: String)
sharing:
  method js-name(self):
    cases (ActivationFunction) self:
      | elu => "elu"
      | hard-sigmoid => "hardSigmoid"
      | linear => "linear"
      | relu => "relu"
      | relu6 => "relu6"
      | selu => "selu"
      | sigmoid => "sigmoid"
      | softmax => "softmax"
      | softplus => "softplus"
      | softsign => "softsign"
      | tanh => "tanh"
      | other(id) => id
    end
  end
end
