#lang racket/base

(require
  "lang/pyret-lang-whalesong.rkt"
  "lang/pyret.rkt")
(provide
  (all-from-out "lang/pyret-lang-whalesong.rkt")
  (rename-out [bare-read-syntax pyret-read-syntax]))
