#lang whalesong

(require
 (rename-in "list.arr" [%PYRET-PROVIDE list])
 (rename-in "builtins.arr" [%PYRET-PROVIDE builtins])
 (rename-in "error.arr" [%PYRET-PROVIDE error]))

(provide list builtins error)
