#lang pyret

provide *
provide-types *

data EqualityResult:
  | Equal
  | NotEqual(reason :: String)
  | Unknown(reason :: String)
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
    | Unknown(_) => raise("Equality check on functions or methods or roughnums")
    | Equal => true
    | NotEqual(_) => false
  end
end

fun from-boolean(b :: Boolean):
  if b: Equal else: NotEqual("false") end
end
