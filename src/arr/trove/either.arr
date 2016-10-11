#lang pyret

provide *
provide-types *

import global as _

data Either<a,b>:
  | left(v :: a)
  | right(v :: b)
end
