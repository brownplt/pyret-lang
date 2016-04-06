#lang pyret

provide *
provide-types *

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
  make:  lam(arr):           v-sequence(raw-array-to-list(arr)) end,
  make0: lam():              v-sequence(raw-array-to-list([raw-array: ])) end,
  make1: lam(a):             v-sequence(raw-array-to-list([raw-array: a])) end,
  make2: lam(a, b):          v-sequence(raw-array-to-list([raw-array: a, b])) end,
  make3: lam(a, b, c):       v-sequence(raw-array-to-list([raw-array: a, b, c])) end,
  make4: lam(a, b, c, d):    v-sequence(raw-array-to-list([raw-array: a, b, c, d])) end,
  make5: lam(a, b, c, d, e): v-sequence(raw-array-to-list([raw-array: a, b, c, d, e])) end
}
para = {
  make:  lam(arr):           h-sequence(raw-array-to-list(arr), " ") end,
  make0: lam():              h-sequence(raw-array-to-list([raw-array: ]), " ") end,
  make1: lam(a):             h-sequence(raw-array-to-list([raw-array: a]), " ") end,
  make2: lam(a, b):          h-sequence(raw-array-to-list([raw-array: a, b]), " ") end,
  make3: lam(a, b, c):       h-sequence(raw-array-to-list([raw-array: a, b, c]), " ") end,
  make4: lam(a, b, c, d):    h-sequence(raw-array-to-list([raw-array: a, b, c, d]), " ") end,
  make5: lam(a, b, c, d, e): h-sequence(raw-array-to-list([raw-array: a, b, c, d, e]), " ") end
}
para-nospace = {
  make:  lam(arr):           h-sequence(raw-array-to-list(arr), "") end,
  make0: lam():              h-sequence(raw-array-to-list([raw-array: ]), "") end,
  make1: lam(a):             h-sequence(raw-array-to-list([raw-array: a]), "") end,
  make2: lam(a, b):          h-sequence(raw-array-to-list([raw-array: a, b]), "") end,
  make3: lam(a, b, c):       h-sequence(raw-array-to-list([raw-array: a, b, c]), "") end,
  make4: lam(a, b, c, d):    h-sequence(raw-array-to-list([raw-array: a, b, c, d]), "") end,
  make5: lam(a, b, c, d, e): h-sequence(raw-array-to-list([raw-array: a, b, c, d, e]), "") end
}
bulleted = {
  make:  lam(arr):           bulleted-sequence(raw-array-to-list(arr)) end,
  make0: lam():              bulleted-sequence(raw-array-to-list([raw-array: ])) end,
  make1: lam(a):             bulleted-sequence(raw-array-to-list([raw-array: a])) end,
  make2: lam(a, b):          bulleted-sequence(raw-array-to-list([raw-array: a, b])) end,
  make3: lam(a, b, c):       bulleted-sequence(raw-array-to-list([raw-array: a, b, c])) end,
  make4: lam(a, b, c, d):    bulleted-sequence(raw-array-to-list([raw-array: a, b, c, d])) end,
  make5: lam(a, b, c, d, e): bulleted-sequence(raw-array-to-list([raw-array: a, b, c, d, e])) end
}
numbered = {
  make:  lam(arr):           numbered-sequence(raw-array-to-list(arr)) end,
  make0: lam():              numbered-sequence(raw-array-to-list([raw-array: ])) end,
  make1: lam(a):             numbered-sequence(raw-array-to-list([raw-array: a])) end,
  make2: lam(a, b):          numbered-sequence(raw-array-to-list([raw-array: a, b])) end,
  make3: lam(a, b, c):       numbered-sequence(raw-array-to-list([raw-array: a, b, c])) end,
  make4: lam(a, b, c, d):    numbered-sequence(raw-array-to-list([raw-array: a, b, c, d])) end,
  make5: lam(a, b, c, d, e): numbered-sequence(raw-array-to-list([raw-array: a, b, c, d, e])) end
}
opt = {
  make:  lam(arr):           optional(v-sequence(raw-array-to-list(arr))) end,
  make0: lam():              optional(v-sequence(raw-array-to-list([raw-array: ]))) end,
  make1: lam(a):             optional(v-sequence(raw-array-to-list([raw-array: a]))) end,
  make2: lam(a, b):          optional(v-sequence(raw-array-to-list([raw-array: a, b]))) end,
  make3: lam(a, b, c):       optional(v-sequence(raw-array-to-list([raw-array: a, b, c]))) end,
  make4: lam(a, b, c, d):    optional(v-sequence(raw-array-to-list([raw-array: a, b, c, d]))) end,
  make5: lam(a, b, c, d, e): optional(v-sequence(raw-array-to-list([raw-array: a, b, c, d, e]))) end
}
