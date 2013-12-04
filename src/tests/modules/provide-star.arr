#lang pyret

provide *

data Thing:
  | singleton
  | non-singleton(foo) with: something(self): end
sharing:
  something-else(self): end
where:
  fun do-not-include-this():
  end
  nothing
end

fun foo():
  fun bar():
  end
  nothing
where:
  fun do-not-include-this():
  end
  nothing
end

x = "toplevel x"

var z = "changed to include z"
