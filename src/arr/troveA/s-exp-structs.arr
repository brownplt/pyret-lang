#lang pyret

provide *
provide-types *

import lists as lists
type List = lists.List

data S-Exp:
  | s-list(exps :: List<S-Exp>)
  | s-num(n :: Number)
  | s-str(s :: String)
  | s-sym(s :: String)
end

