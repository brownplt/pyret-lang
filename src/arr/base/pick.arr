provide *
provide-types *

data Pick<a, b>:
  | pick-none
  | pick-some(elt :: a, rest :: b)
end
