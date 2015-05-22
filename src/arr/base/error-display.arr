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
  | code(contents :: ErrorDisplay)
  | styled(contents :: ErrorDisplay, style :: String)
  | loc-display(loc #|:: S.Srcloc|#, style :: String, contents :: ErrorDisplay)
  | optional(contents :: ErrorDisplay)
sharing:
  to-string(self): display-to-string(self, torepr) end
end

fun display-to-string(e, embed-display):
  cases(ErrorDisplay) e:
    | text(str) => str
    | embed(val) => embed-display(val)
    | loc(l) => tostring(l)
    | loc-display(l, _, contents) =>
      cases(ErrorDisplay) contents:
        | loc(l2) =>
          if l2 == l: display-to-string(contents, embed-display)
          else: display-to-string(contents, embed-display) + " (at " + tostring(l) + ")"
          end
        | else => display-to-string(contents, embed-display) + " (at " + tostring(l) + ")"
      end
    | code(contents) => "`" + display-to-string(contents, embed-display) + "`"
    | styled(contents, style) => display-to-string(contents)
    | h-sequence(contents, sep) =>
      contents.filter(lam(c): not(is-optional(c)) end).map(display-to-string(_, embed-display)).join-str(sep)
    | v-sequence(contents) =>
      contents.filter(lam(c): not(is-optional(c)) end).map(display-to-string(_, embed-display)).join-str("\n")
    | bulleted-sequence(contents) =>
      contents.map(lam(elt): "* " + display-to-string(elt, embed-display) end).join-str("\n")
    | numbered-sequence(contents) =>
      # Cannot load lists, because it depends on equality, which depends on error, which depends on this
      # so we have to fake our own map_n function here
      var n = 0
      contents.map(lam(elt):
          n := n + 1
          tostring(n) + " " + display-to-string(elt, embed-display)
        end).join-str("\n")
    | optional(_) => ""
  end
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
