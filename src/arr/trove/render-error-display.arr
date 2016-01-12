provide *

import error-display as ED
import srcloc as S

fun nth-stack-frame(n :: Number, user-frames-only :: Boolean, stack):
  usable-frames =
    if user-frames-only: stack.filter(S.is-srcloc)
    else: stack
    end
  if usable-frames.length() > n: some(usable-frames.get(n))
  else: none
  end
end

fun display-to-string(e, embed-display, stack):
  help = display-to-string(_, embed-display, stack)
  cases(ED.ErrorDisplay) e:
    | paragraph(contents) => help(contents) + "\n"
    | text(str) => str
    | embed(val) => embed-display(val)
    | loc(l) => tostring(l)
    | maybe-stack-loc(n, user-frames-only, contents-with-loc, contents-without-loc) =>
      cases(Option) nth-stack-frame(n, user-frames-only, stack):
        | none => help(contents-without-loc)
        | some(l) => help(contents-with-loc(l))
      end
    | loc-display(l, _, contents) =>
      cases(ED.ErrorDisplay) contents:
        | loc(l2) =>
          if l2 == l: help(contents)
          else: help(contents) + " (at " + tostring(l) + ")"
          end
        | else => help(contents) + " (at " + tostring(l) + ")"
      end
    | code(contents) => "`" + help(contents) + "`"
    | styled(contents, style) => help(contents)
    | h-sequence(contents, sep) =>
      contents.filter(lam(c): not(ED.is-optional(c)) end).map(help).join-str(sep)
    | v-sequence(contents) =>
      contents.filter(lam(c): not(ED.is-optional(c)) end).map(help).join-str("\n")
    | bulleted-sequence(contents) =>
      contents.map(lam(elt): "* " + help(elt) end).join-str("\n")
    | numbered-sequence(contents) =>
      for map_n(n from 1, elt from contents):
        tostring(n) + " " + help(elt)
      end.join-str("\n")
    | optional(_) => ""
    | highlight(contents, locs, _) => help(ED.loc-display(contents, "", locs.first))
  end
end
