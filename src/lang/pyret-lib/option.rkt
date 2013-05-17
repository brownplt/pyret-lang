#lang pyret/library

provide
  {
    is-Option: is-Option,
    is-none: is-none,
    is-some: is-some,
    none: none,
    some: some,
  }
end

data Option:
  | none
  | some(value)
end
