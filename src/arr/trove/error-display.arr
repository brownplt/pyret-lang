#lang pyret

provide *
provide-types *

import srcloc as S

data ErrorDisplay:
  | v-sequence(contents :: List<ErrorDisplay>)
  | h-sequence(contents :: List<ErrorDisplay>)
  | embed(val :: Any)
  | text(str :: String)
  | loc(loc :: S.Srcloc)
sharing:
  to-string(self): display-to-string(self) end
end

fun display-to-string(e):
  cases(ErrorDisplay) e:
    | text(str) => str
    | embed(val) => torepr(val)
    | loc(l) => tostring(l)
    | h-sequence(contents) => contents.map(display-to-string).join-str(" ")
    | v-sequence(contents) => contents.map(display-to-string).join-str("\n")
  end
end

shadow error = {
  make: lam(arr): v-sequence(raw-array-to-list(arr)) end
}
para = {
  make: lam(arr): h-sequence(raw-array-to-list(arr)) end
}
