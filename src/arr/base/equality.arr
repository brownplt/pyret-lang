#lang pyret

provide *
provide-types *

data EqualityResult:
  | Equal
  | NotEqual(reason :: String)
  | Unknown
end
