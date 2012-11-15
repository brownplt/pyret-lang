#lang racket

(require "../src/lang/reader.rkt" rackunit "../src/values.rkt")

(dynamic-require "../src/lang/pyret.rkt" 0)
(define ns (module->namespace "../src/lang/pyret.rkt"))

(define (pyret-eval str)
  (eval-syntax
   (read-syntax "cmdline" (open-input-string str))
   ns))

(define (check-pyret str expected)
  (check-equal? (pyret-eval str) expected))

(define (check-pyret-exn str message)
  (check-exn message (pyret-eval str)))

(check-pyret "{}" (p-object (none) (make-hash)))

(check-pyret "[]" (p-list (list) (none) (make-hash)))

(check-pyret "seal({}, [])" (p-object (set) (make-hash)))

(define (mk-num n)
  (p-num n (make-hash) (none)))

(define five (mk-num 5))
(define two (mk-num 2))
(define ten (mk-num 10))


(check-pyret "seal({x:5}, ['x'])" (p-object (set "x") (make-hash (list (cons "x" five)))) (set "x"))

(check-pyret "seal(seal({x:5, y:2}, ['y']), ['y'])"
             (p-object (set "y") '(("x" . five) ("y" . two))))

(check-pyret "seal(seal({x:5, y:2, z:10}, ['y', 'z']), ['y'])"
             (p-object (set "y") '(("x" . five) ("y" . two) ("z" . ten))))

(check-pyret "seal({x:5, y:2, z:10}, ['y', 'z'])"
             (p-object (set "y" "z") '(("x" . five) ("y" . two) ("z" . ten))))

(check-pyret "seal(seal({x:5, y:2}, ['y']), ['x'])" "seal")

(check-pyret-exn "seal({x:5}, ['y'])" "seal")

(check-pyret-exn "seal({x:5}, 'y')" "seal")

(check-pyret-exn "seal({}, ['y'])" "seal")

(check-pyret-exn "seal(5, ['y'])" "seal")


