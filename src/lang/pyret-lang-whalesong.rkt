#lang whalesong

(require
  (except-in whalesong/lang/whalesong list error raise pi else)
  (prefix-in r: (only-in whalesong/lang/whalesong list error raise))
  "runtime.rkt"
  (rename-in pyret/lang/pyret-lib/list [%PYRET-PROVIDE list])
  (rename-in pyret/lang/pyret-lib/error [%PYRET-PROVIDE error])
  (rename-in pyret/lang/pyret-lib/builtins [%PYRET-PROVIDE builtins])
  (rename-in pyret/lang/pyret-lib/option [%PYRET-PROVIDE option])
  )

(provide
  #%module-begin
  #%top-interaction
  #%datum
  #%top
  #%app

  list
  error
  builtins

  [prefix-out r: (all-from-out whalesong/lang/whalesong)]
  r:list r:error r:raise
  (all-from-out "runtime.rkt")
  )
  
