#lang pyret

provide *
provide-types *

data ErrorDisplay:
  | paragraph(contents #|:: List<ErrorDisplay>|#)
  | bulleted-sequence(contents #|:: List<ErrorDisplay>|#)
  | numbered-sequence(contents #|:: List<ErrorDisplay>|#)
  | v-sequence(contents #|:: List<ErrorDisplay>|#)
  | h-sequence(contents #|:: List<ErrorDisplay>|#, sep :: String)
  | embed(val :: Any)
  | text(str :: String)
  | loc(loc #|:: S.Srcloc|#)
  | maybe-stack-loc(n :: Number, user-frames-only :: Boolean,
      contents-with-loc #|:: S.Srcloc -> ErrorDisplay|#,
      contents-without-loc :: ErrorDisplay)
  | code(contents :: ErrorDisplay)
  | cmcode(loc #|:: S.Srcloc|#)
  | styled(contents :: ErrorDisplay, style :: String)
  | loc-display(loc #|:: S.Srcloc|#, style :: String, contents :: ErrorDisplay)
  | optional(contents :: ErrorDisplay)
  | highlight(contents :: ErrorDisplay, locs #|:: List<S.Srcloc>|#, color :: Number)
end

locs = {
  make: lam(arr): raw-array-to-list(arr) end
}

shadow error = {
  make: lam(arr): v-sequence(raw-array-to-list(arr)) end
}
para = {
  make: lam(arr): paragraph(raw-array-to-list(arr)) end
}
sequence = {
  make: lam(arr): h-sequence(raw-array-to-list(arr), "") end
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

fun ed-args(n):
  [sequence:
    embed(n),
    text(if n == 1: " argument"
            else:      " arguments";)]
end

fun ed-fields(n):
  [sequence:
    embed(n),
    text(if n == 1: " field"
            else:      " fields";)]
end

fun ed-field-bindings(n):
  [sequence:
    embed(n),
    text(if n == 1: " field binding"
            else:      " field bindings";)]
end

fun ed-params(n):
  [sequence:
    embed(n),
    text(if n == 1: " parameter"
            else:      " parameters";)]
end
