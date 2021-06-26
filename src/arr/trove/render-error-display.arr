provide *
import global as _
import error-display as ED
import jsfile('ts-render-error-display') as TRED

fun display-to-string(e :: ED.ErrorDisplay, embed-display, stack):
  TRED.display-to-string(e, embed-display, stack)
end
