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
  to-string(self): display-to-string(self) end
end

fun display-to-string(e):
  cases(ErrorDisplay) e:
    | text(str) => str
    | embed(val) => torepr(val)
    | loc(l) => tostring(l)
    | loc-display(l, _, contents) =>
      cases(ErrorDisplay) contents:
        | loc(l2) =>
          if l2 == l: display-to-string(contents)
          else: display-to-string(contents) + " (at " + tostring(l) + ")"
          end
        | else => display-to-string(contents) + " (at " + tostring(l) + ")"
      end
    | code(contents) => "`" + display-to-string(contents) + "`"
    | h-sequence(contents, sep) => contents.map(display-to-string).join-str(sep)
    | v-sequence(contents) => contents.map(display-to-string).join-str("\n")
  end
end

shadow error = {
  make: lam(arr): v-sequence(raw-array-to-list(arr)) end
}
para = {
  make: lam(arr): h-sequence(raw-array-to-list(arr), " ") end
}
