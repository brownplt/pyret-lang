#lang pyret

provide *
provide-types *
import global as _
import error as error

data EqualityResult:
  | Equal
  | NotEqual(value1, value2)
  | Unknown(value1, value2)
end

fun equal-and(er1 :: EqualityResult, er2 :: EqualityResult):
  ask:
    | is-NotEqual(er1) then: er1
    | is-NotEqual(er2) then: er2
    | is-Equal(er1) and is-Equal(er2) then: Equal
    | otherwise: er1 # i.e., the first Unknown
  end
end

fun equal-or(er1 :: EqualityResult, er2 :: EqualityResult):
  ask:
    | is-Equal(er1) then: er1
    | is-Equal(er2) then: er2
    | is-Unknown(er1) then: er1 # i.e., the first Unknown
    | otherwise: er2 # NotEqual or NotEqual/NotEqual or Unknown
  end
end

fun to-boolean(er :: EqualityResult):
  cases(EqualityResult) er:
    | Unknown(r, v1, v2) => raise(error.equality-failure(r, v1, v2))
    | Equal => true
    | NotEqual(_,_,_) => false
  end
end
