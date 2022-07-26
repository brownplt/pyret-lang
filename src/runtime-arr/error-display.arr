provide: *, data * end
provide-types *

import global as G
import raw-array as R
import srcloc as S
import number as N
include from S: type Srcloc as SL end
include from R: raw-array end
include from N: num-modulo end

data ErrorDisplay:
  | paragraph(contents :: RawArray<ErrorDisplay>)
  | paragrap(contents :: RawArray<ErrorDisplay>)
  | bulleted-sequence(contents :: RawArray<ErrorDisplay>)
  | v-sequence(contents :: RawArray<ErrorDisplay>)
  | h-sequence(contents :: RawArray<ErrorDisplay>, sep :: String)
  | h-sequence-sep(contents :: RawArray<ErrorDisplay>, sep :: String, last :: String)
  | embed(val :: Any)
  | text(str :: String)
  | loc(loc :: S.Srcloc)
  | maybe-stack-loc(n :: Number, user-frames-only :: Boolean,
      contents-with-loc :: (S.Srcloc -> ErrorDisplay),
      contents-without-loc :: ErrorDisplay)
  | code(contents :: ErrorDisplay)
  | cmcode(loc :: S.Srcloc)
  | loc-display(loc :: S.Srcloc, style :: String, contents :: ErrorDisplay)
  | optional(contents :: ErrorDisplay)
  | highlight(contents :: ErrorDisplay, locs :: RawArray<S.Srcloc>, color :: Number)
end

type ED = ErrorDisplay

locs = raw-array

para = {
  make:  lam(arr :: RawArray<ED>):           paragraph(arr) end,
  make0: lam():              paragraph([raw-array: ]) end,
  make1: lam(a :: ED):             paragraph([raw-array: a]) end,
  make2: lam(a :: ED, b :: ED):          paragraph([raw-array: a, b]) end,
  make3: lam(a :: ED, b :: ED, c :: ED):       paragraph([raw-array: a, b, c]) end,
  make4: lam(a :: ED, b :: ED, c :: ED, d :: ED):    paragraph([raw-array: a, b, c, d]) end,
  make5: lam(a :: ED, b :: ED, c :: ED, d :: ED, e :: ED): paragraph([raw-array: a, b, c, d, e]) end
}

sequence = {
  make:  lam(arr :: RawArray<ED>):           h-sequence(arr, " ") end,
  make0: lam():              h-sequence([raw-array: ], " ") end,
  make1: lam(a :: ED):             h-sequence([raw-array: a], " ") end,
  make2: lam(a :: ED, b :: ED):          h-sequence([raw-array: a, b], " ") end,
  make3: lam(a :: ED, b :: ED, c :: ED):       h-sequence([raw-array: a, b, c], " ") end,
  make4: lam(a :: ED, b :: ED, c :: ED, d :: ED):    h-sequence([raw-array: a, b, c, d], " ") end,
  make5: lam(a :: ED, b :: ED, c :: ED, d :: ED, e :: ED): h-sequence([raw-array: a, b, c, d, e], " ") end
}

vert = {
  make:  lam(arr :: RawArray<ED>):           v-sequence(arr) end,
  make0: lam():              v-sequence([raw-array: ]) end,
  make1: lam(a :: ED):             v-sequence([raw-array: a]) end,
  make2: lam(a :: ED, b :: ED):          v-sequence([raw-array: a, b]) end,
  make3: lam(a :: ED, b :: ED, c :: ED):       v-sequence([raw-array: a, b, c]) end,
  make4: lam(a :: ED, b :: ED, c :: ED, d :: ED):    v-sequence([raw-array: a, b, c, d]) end,
  make5: lam(a :: ED, b :: ED, c :: ED, d :: ED, e :: ED): v-sequence([raw-array: a, b, c, d, e]) end
}

shadow error = sequence

para-nospace = {
  make:  lam(arr :: RawArray<ED>):           h-sequence(arr, "") end,
  make0: lam():              h-sequence([raw-array: ], "") end,
  make1: lam(a :: ED):             h-sequence([raw-array: a], "") end,
  make2: lam(a :: ED, b :: ED):          h-sequence([raw-array: a, b], "") end,
  make3: lam(a :: ED, b :: ED, c :: ED):       h-sequence([raw-array: a, b, c], "") end,
  make4: lam(a :: ED, b :: ED, c :: ED, d :: ED):    h-sequence([raw-array: a, b, c, d], "") end,
  make5: lam(a :: ED, b :: ED, c :: ED, d :: ED, e :: ED): h-sequence([raw-array: a, b, c, d, e], "") end
}

bulleted = {
  make:  lam(arr :: RawArray<ED>):           bulleted-sequence(arr) end,
  make0: lam():              bulleted-sequence([raw-array: ]) end,
  make1: lam(a :: ED):             bulleted-sequence([raw-array: a]) end,
  make2: lam(a :: ED, b :: ED):          bulleted-sequence([raw-array: a, b]) end,
  make3: lam(a :: ED, b :: ED, c :: ED):       bulleted-sequence([raw-array: a, b, c]) end,
  make4: lam(a :: ED, b :: ED, c :: ED, d :: ED):    bulleted-sequence([raw-array: a, b, c, d]) end,
  make5: lam(a :: ED, b :: ED, c :: ED, d :: ED, e :: ED): bulleted-sequence([raw-array: a, b, c, d, e]) end
}

opt = {
  make:  lam(arr :: RawArray<ED>):           optional(v-sequence(arr)) end,
  make0: lam():              optional(v-sequence([raw-array: ])) end,
  make1: lam(a :: ED):             optional(v-sequence([raw-array: a])) end,
  make2: lam(a :: ED, b :: ED):          optional(v-sequence([raw-array: a, b])) end,
  make3: lam(a :: ED, b :: ED, c :: ED):       optional(v-sequence([raw-array: a, b, c])) end,
  make4: lam(a :: ED, b :: ED, c :: ED, d :: ED):    optional(v-sequence([raw-array: a, b, c, d])) end,
  make5: lam(a :: ED, b :: ED, c :: ED, d :: ED, e :: ED): optional(v-sequence([raw-array: a, b, c, d, e])) end
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

fun ed-bindings(n):
  [sequence:
    embed(n),
    text(if n == 1: " binding" else: " bindings" end)]
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

fun ed-rows(n):
  [sequence:
    embed(n),
    text(if n == 1: " row" else: " rows" end)]
end

fun ed-values(n):
  [sequence:
    embed(n),
    text(if n == 1: " value" else: " values" end)]
end

fun ed-nth(n):
  text(
    let last-digit = num-modulo(n,10):
      G.to-repr(n) +  ask:
        | last-digit == 1 then: "ˢᵗ"
        | last-digit == 2 then: "ⁿᵈ"
        | last-digit == 3 then: "ⁿᵈ"
        | otherwise:            "ᵗʰ"
      end
    end)
end
