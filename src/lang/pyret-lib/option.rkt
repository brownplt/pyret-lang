#lang pyret/library

provide
  {
    Option: Option,
    is-none: is-none,
    is-some: is-some,
    none: none,
    some: some,
  }
end

data Option:
  | none
  | some(value)
sharing
  tostring(self):
    case:
      | is-none(self) => "None"
      | is-some(self) => "Some(" + tostring(self.value) + ")"
    end
  end
end
