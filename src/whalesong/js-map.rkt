#lang s-exp whalesong/lang/js/js
(require whalesong/resource)
(declare-implementation
  #:racket "js-map-racket-impl.rkt"
  #:javascript ("js-map.js")
  #:provided-values
    (make-js-map
     js-map-get
     js-map-set*
     js-map-set
     js-map-keys
     js-map-has-key?))

