provide: PI end
import primitive-types as _

#|
  Joe/Ben:

  This module was added to support incrementally adding constants to the global
  namespace (via extra-imports), while allowing for programs to also import
  from constants directly in the future (from this module) without
  name-clashing with the already-provided globals.
|#

PI = ~3.141592653589793
