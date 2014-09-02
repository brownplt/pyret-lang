#lang pyret

provide *
provide-types *

data EqualityResult:
  | Equal
  | NotEqual
  | Unknown
  | NotEqualReason(reason :: String)
end

fun equal-and(er1 :: EqualityResult, er2 :: EqualityResult):
  ask:
    | is-NotEqual(er1) then: er1
    | is-NotEqual(er2) then: er2
    | is-Equal(er1) and is-Equal(er2) then: Equal
    | otherwise: Unknown
  end
end

fun equal-or(er1 :: EqualityResult, er2 :: EqualityResult):
  ask:
    | is-Equal(er1) then: er1
    | is-Equal(er2) then: er2
    | is-Unknown(er1) then: Unknown # Unknown or NotEqual/Unknown or Unknown
    | otherwise: er2 # NotEqual or NotEqual/NotEqual or Unknown
  end
end

fun to-boolean(er :: EqualityResult):
  cases(EqualityResult) er:
    | Unknown => raise("Equality check on two functions or two methods")
    | Equal => true
    | NotEqual(_) => false
  end
end

