#lang pyret

provide *
provide-types *

data ErrorDisplay:
  | v-sequence(contents #|:: List<ErrorDisplay>|#)
  | h-sequence(contents #|:: List<ErrorDisplay>|#, sep :: String)
  | embed(val :: Any)
  | text(str :: String)
  | loc(loc #|:: S.Srcloc|#)
  | code(contents :: ErrorDisplay)
  | loc-display(loc #|:: S.Srcloc|#, style :: String, contents :: ErrorDisplay)
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
    | h-sequence(contents, sep) => contents.map(display-to-string(_, embed-display)).join-str(sep)
    | v-sequence(contents) => contents.map(display-to-string(_, embed-display)).join-str("\n")
  end
end

shadow error = {
  make: lam(arr): v-sequence(raw-array-to-list(arr)) end
}
para = {
  make: lam(arr): h-sequence(raw-array-to-list(arr), " ") end
}
