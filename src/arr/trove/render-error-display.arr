provide *
import global as _
import either as E
import error-display as ED
import srcloc as S
import option as O

fun nth-stack-frame(n :: Number, user-frames-only :: Boolean, stack):
  usable-frames =
    if user-frames-only: stack.filter(S.is-srcloc)
    else: stack
    end
  if usable-frames.length() > n: O.some(usable-frames.get(n))
  else: O.none
  end
end

fun display-to-string(e :: ED.ErrorDisplay, embed-display, stack):
  help = display-to-string(_, embed-display, stack)
  cases(ED.ErrorDisplay) e:
    | paragraph(contents) => contents.map(help).join-str("")
    | text(str) => str
    | embed(val) => 
      cases(E.Either) run-task(lam(): exn-unwrap(val).render-reason() end):
        | left(v)  => help(v)
        | right(_) => embed-display(val)
      end
    | loc(l) => l.format(true)
    | maybe-stack-loc(n, user-frames-only, contents-with-loc, contents-without-loc) =>
      cases(O.Option) nth-stack-frame(n, user-frames-only, stack):
        | none => help(contents-without-loc)
        | some(l) => help(contents-with-loc(l))
      end
    | loc-display(l, _, contents) =>
      cases(ED.ErrorDisplay) contents:
        | loc(l2) =>
          if l2 == l: help(contents)
          else: help(contents) + " (at " + l.format(true) + ")"
          end
        | else => help(contents) + " (at " + l.format(true) + ")"
      end
    | code(contents) => "`" + help(contents) + "`"
    | h-sequence(contents, sep) =>
      contents.filter(lam(c): not(ED.is-optional(c)) end).map(help).join-str(sep)
    | h-sequence-sep(contents, sep, last-sep) =>
      contents.filter(lam(c): not(ED.is-optional(c)) end).map(help).join-str-last(sep, last-sep)
    | v-sequence(contents) =>
      contents.filter(lam(c): not(ED.is-optional(c)) end).map(help).join-str("\n")
    | bulleted-sequence(contents) =>
      contents.map(lam(elt): "* " + help(elt) end).join-str("\n")
    | optional(_) => ""
    | cmcode(loc) => tostring(loc)
    | highlight(contents, locs, _) => help(ED.loc-display(locs.first, "", contents))
  end
end
