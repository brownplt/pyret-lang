#lang pyret

provide *

data Either<a,b>:
  | left(v :: a)
  | right(v :: b)
end



