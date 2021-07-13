provide *
import global as _
import error-display as ED
import js-file('ts-render-error-display') as TRED

fun display-to-string(e :: ED.ErrorDisplay, embed-display, stack):
  TRED.display-to-string(e, embed-display, stack)
end

fun display-to-json(e :: ED.ErrorDisplay, embed-display, stack):
  TRED.display-to-json(e, embed-display, stack)
end
