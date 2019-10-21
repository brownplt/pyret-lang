data Element<T>:
  | elt(val :: T, ref parent :: Option<Element<T>>)
end

e :: Element<Number> = elt(0, none)
e!{parent: none}
