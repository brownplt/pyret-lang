#lang pyret

provide *
provide-types *

import global as _
include lists

data S-Exp:
  | s-list(exps :: List<S-Exp>)
  | s-num(n :: Number)
  | s-str(s :: String)
  | s-sym(s :: String)
end

