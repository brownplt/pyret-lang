#lang pyret

provide *
provide-types *

#import lists as L

data ErrorDisplay:
  | v-sequence(contents #|:: List<ErrorDisplay>|#)
  | bulleted-sequence(contents #|:: List<ErrorDisplay>|#)
  | numbered-sequence(contents #|:: List<ErrorDisplay>|#)
  | h-sequence(contents #|:: List<ErrorDisplay>|#, sep :: String)
  | embed(val :: Any)
  | text(str :: String)
  | loc(loc #|:: S.Srcloc|#)
  | maybe-stack-loc(n :: Number, user-frames-only :: Boolean,
      contents-with-loc #|:: S.Srcloc -> ErrorDisplay|#,
      contents-without-loc :: ErrorDisplay)
  | code(contents :: ErrorDisplay)
  | styled(contents :: ErrorDisplay, style :: String)
  | loc-display(loc #|:: S.Srcloc|#, style :: String, contents :: ErrorDisplay)
  | optional(contents :: ErrorDisplay)
end

shadow error = {
  make: lam(arr): v-sequence(raw-array-to-list(arr)) end
}
para = {
  make: lam(arr): h-sequence(raw-array-to-list(arr), " ") end
}
para-nospace = {
  make: lam(arr): h-sequence(raw-array-to-list(arr), "") end
}
bulleted = {
  make: lam(arr): bulleted-sequence(raw-array-to-list(arr)) end
}
numbered = {
  make: lam(arr): numbered-sequence(raw-array-to-list(arr)) end
}
opt = {
  make: lam(arr): optional(v-sequence(raw-array-to-list(arr))) end
}
list = {
  make: lam(arr): raw-array-to-list(arr) end
}
fun map3(f, la, lb, lc):
  if la.length() == 0:
    la
  else:
    map3(f, la.rest, lb.rest, lc.rest).push(f(la.first, lb.first, lc.first))
  end
end
