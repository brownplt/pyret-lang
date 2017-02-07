provide *
provide-types *

import error as ERR
import global as _
import base as _

fun exit(code :: Number):
  raise(ERR.exit(code))
end

fun exit-quiet(code :: Number):
  raise(ERR.exit-quiet(code))
end
