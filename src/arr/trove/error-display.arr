#lang pyret

provide *
provide-types *

import global as _

data ErrorDisplay:
  | paragraph(contents #|:: List<ErrorDisplay>|#)
  | bulleted-sequence(contents #|:: List<ErrorDisplay>|#)
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
  make:  lam(arr):           raw-array-to-list(arr) end,
  make0: lam():              raw-array-to-list([raw-array: ]) end,
  make1: lam(a):             raw-array-to-list([raw-array: a]) end,
  make2: lam(a, b):          raw-array-to-list([raw-array: a, b]) end,
  make3: lam(a, b, c):       raw-array-to-list([raw-array: a, b, c]) end,
  make4: lam(a, b, c, d):    raw-array-to-list([raw-array: a, b, c, d]) end,
  make5: lam(a, b, c, d, e): raw-array-to-list([raw-array: a, b, c, d, e]) end
}

para = {
  make:  lam(arr):           paragraph(raw-array-to-list(arr)) end,
  make0: lam():              paragraph(raw-array-to-list([raw-array: ])) end,
  make1: lam(a):             paragraph(raw-array-to-list([raw-array: a])) end,
  make2: lam(a, b):          paragraph(raw-array-to-list([raw-array: a, b])) end,
  make3: lam(a, b, c):       paragraph(raw-array-to-list([raw-array: a, b, c])) end,
  make4: lam(a, b, c, d):    paragraph(raw-array-to-list([raw-array: a, b, c, d])) end,
  make5: lam(a, b, c, d, e): paragraph(raw-array-to-list([raw-array: a, b, c, d, e])) end
}

sequence = {
  make:  lam(arr):           h-sequence(raw-array-to-list(arr), " ") end,
  make0: lam():              h-sequence(raw-array-to-list([raw-array: ]), " ") end,
  make1: lam(a):             h-sequence(raw-array-to-list([raw-array: a]), " ") end,
  make2: lam(a, b):          h-sequence(raw-array-to-list([raw-array: a, b]), " ") end,
  make3: lam(a, b, c):       h-sequence(raw-array-to-list([raw-array: a, b, c]), " ") end,
  make4: lam(a, b, c, d):    h-sequence(raw-array-to-list([raw-array: a, b, c, d]), " ") end,
  make5: lam(a, b, c, d, e): h-sequence(raw-array-to-list([raw-array: a, b, c, d, e]), " ") end
}

shadow error = sequence

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

opt = {
  make:  lam(arr):           optional(v-sequence(raw-array-to-list(arr))) end,
  make0: lam():              optional(v-sequence(raw-array-to-list([raw-array: ]))) end,
  make1: lam(a):             optional(v-sequence(raw-array-to-list([raw-array: a]))) end,
  make2: lam(a, b):          optional(v-sequence(raw-array-to-list([raw-array: a, b]))) end,
  make3: lam(a, b, c):       optional(v-sequence(raw-array-to-list([raw-array: a, b, c]))) end,
  make4: lam(a, b, c, d):    optional(v-sequence(raw-array-to-list([raw-array: a, b, c, d]))) end,
  make5: lam(a, b, c, d, e): optional(v-sequence(raw-array-to-list([raw-array: a, b, c, d, e]))) end
}

fun ed-args(n):
  [sequence:
    embed(n),
    text(if n == 1: " argument" else: " arguments" end)]
end

fun ed-names(n):
  [sequence:
    embed(n),
    text(if n == 1: " name" else: " names" end)]
end

fun ed-fields(n):
  [sequence:
    embed(n),
    text(if n == 1: " field" else: " fields" end)]
end

fun ed-field-bindings(n):
  [sequence:
    embed(n),
    text(if n == 1: " field binding" else: " field bindings" end)]
end

fun ed-params(n):
  [sequence:
    embed(n),
    text(if n == 1: " parameter" else: " parameters" end)]
end

fun ed-components(n):
  [sequence:
    embed(n),
    text(if n == 1: " component" else: " components" end)]
end

fun ed-nth(n):
  text(
    let last-digit = num-modulo(n,10):
      num-to-string(n) +  ask:
        | last-digit == 1 then: "ˢᵗ"
        | last-digit == 2 then: "ⁿᵈ"
        | last-digit == 3 then: "ⁿᵈ"
        | otherwise:            "ᵗʰ"
      end
    end)
end
