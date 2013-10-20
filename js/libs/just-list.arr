#lang pyret


builtins = {}

data List:
  | empty with:
    length(self): 0 end
  | link(first, rest :: List) with:
    length(self): 1 + self.rest.length() end
end

list = {
  link: link,
  empty: empty
}

