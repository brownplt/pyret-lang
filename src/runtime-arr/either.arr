#
# Needs to be built with "--compile-mode builtin-stage-1" to avoid emitting code that depends
#  on the builtin global module.
#

provide *
provide-types *

import primitive-types as _

data Either<a, b>:
  | left(v :: a)
  | right(v :: b)
end
