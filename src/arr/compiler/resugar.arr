#lang pyret

provide *
provide-types *

data Node:
  | node(name :: String, children :: List<Node>)
end
