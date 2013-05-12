(module cmdline (file
                 "/home/joe/src/pyret-lang/src/lang/pyret-lang-whalesong.rkt")
  #;(r:require "list-provider.rkt")
  (r:require (r:rename-in "list.rkt" (%PYRET-PROVIDE list)))
  (r:require (r:file "/home/joe/src/pyret-lang/src/lang/pyret-lib/list.rkt" (%PYRET-PROVIDE list)))
  (r:display list)
  )
