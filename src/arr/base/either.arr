#lang pyret

provide *
provide-types *

data Either<a,b>:
  | left(v :: a)
  | right(v :: b)
end



