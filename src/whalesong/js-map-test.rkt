#lang whalesong

(require "js-map.rkt")

(define m1 (make-js-map '()))
(define m2 (js-map-set m1 "foozle" 5))

(display m2)

(display (js-map-get m2 "foozle"))

(define m3 (js-map-set* m2 (list (cons "foozle" 4) (cons "bazzle" 9))))

(display "\nShould be 4:")
(display (js-map-get m3 "foozle"))
(display "\nShould be 9:")
(display (js-map-get m3 "bazzle"))

(display "\nShould be 5: \n")
(display (js-map-get m2 "foozle"))

(display "\nShould be true, then false")
(printf "~a\n" (js-map-has-key? m3 "bazzle"))
(printf "~a\n" (js-map-has-key? m3 "bedazzle"))

(display "\nShould be just 'bazzle', 'foozle'")
(display (js-map-keys m3))

(define m4 (js-map-set* m2 (list (cons "baz" 4) (cons "bazzle" 9))))

(display "\nShould be 4:")
(display (js-map-get m4 "baz"))
(display "\nShould be 5:")
(display (js-map-get m4 "foozle"))
(display "\nShould be 9:")
(display (js-map-get m4 "bazzle"))
