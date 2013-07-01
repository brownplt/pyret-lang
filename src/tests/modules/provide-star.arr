#lang pyret

provide *

data Thing:
  | singleton
  | non-singleton(foo) with: something(self): end
sharing:
  something-else(self): end
check:
  fun do-not-include-this():
  end
end

fun foo():
  fun bar():
  end
check:
  fun do-not-include-this():
  end
end

x = "toplevel x"

var z = "do not include z"

