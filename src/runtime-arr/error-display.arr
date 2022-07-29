provide: *, data * end
provide-types *

import runtime-global as G
import raw-array as R
import srcloc as S
import option as O
import number as N
import either as E
include from G: not end
include from S: type Srcloc as SL end
include from R: * end
include from N: num-modulo end

data ErrorDisplay:
  | paragraph(contents :: RawArray<ErrorDisplay>)
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

fun nth-stack-frame(n :: Number, user-frames-only :: Boolean, stack :: RawArray<S.Srcloc>) -> O.Option<S.Srcloc>:
  usable-frames =
    if user-frames-only: raw-array-filter(S.is-srcloc, stack)
    else: stack
    end
  if raw-array-length(usable-frames) > n: O.some(raw-array-get(usable-frames, n))
  else: O.none
  end
end

fun display-to-string(e :: ErrorDisplay, embed-display :: (Any -> String), stack :: RawArray<S.Srcloc>) -> String:
  help = display-to-string(_, embed-display, stack)
  cases(ErrorDisplay) e:
    | paragraph(contents) => contents ^ raw-array-map(help, _) ^ raw-array-join(_, "")
    | text(str) => str
    | embed(val) => embed-display(val)
      #|
      cases(E.Either) run-task(lam(): exn-unwrap(val).render-reason() end):
        | left(v)  => help(v)
        | right(_) => embed-display(val)
      end
      |#
    | loc(l) => l.format(true)
    | maybe-stack-loc(n, user-frames-only, contents-with-loc, contents-without-loc) =>
      cases(O.Option) nth-stack-frame(n, user-frames-only, stack):
        | none => help(contents-without-loc)
        | some(l) => help(contents-with-loc(l))
      end
    | loc-display(l, _, contents) =>
      cases(ErrorDisplay) contents:
        | loc(l2) =>
          if l2 == l: help(contents)
          else: help(contents) + " (at " + l.format(true) + ")"
          end
        | else => help(contents) + " (at " + l.format(true) + ")"
      end
    | code(contents) => "`" + help(contents) + "`"
    | h-sequence(contents, sep) =>
      contents ^ raw-array-filter(lam(c): not(is-optional(c)) end, _)
               ^ raw-array-map(help, _)
               ^ raw-array-join(_, sep)
    | h-sequence-sep(contents, sep, last-sep) =>
      contents ^ raw-array-filter(lam(c): not(is-optional(c)) end, _)
               ^ raw-array-map(help, _)
               ^ raw-array-join-last(_, sep, last-sep)
    | v-sequence(contents) =>
      contents ^ raw-array-filter(lam(c): not(is-optional(c)) end, _)
               ^ raw-array-map(help, _)
               ^ raw-array-join(_, "\n")
    | bulleted-sequence(contents) =>
      contents ^ raw-array-map(lam(elt): "* " + help(elt) end, _)
               ^ raw-array-join(_, "\n")
    | optional(_) => ""
    | cmcode(shadow loc) => G.to-string(loc)
    | highlight(contents, shadow locs, _) => help(loc-display(raw-array-get(locs, 0), "", contents))
  end
end
