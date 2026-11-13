#lang racket/base
;; Oracle harness: reads TSV lines "id \t op \t arg..." on stdin, prints
;; "id \t result" per line. Results: exact:n[/d] | rough:<16 hex> |
;; bool:true|false | string:... | none | div-by-zero | domain:... | skip
(require racket/string racket/math math/bigfloat math/number-theory)
(bf-precision 200)

(define (domain msg) (raise (exn:fail:contract (string-append "domain: " msg) (current-continuation-marks))))

;; Pyret literal grammar (js-numbers fromString) and makeBignum's grammar.
(define LIT #px"^~?[+-]?[0-9]+(?:/[0-9]+|(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?)$")
(define BIGNUM-LIT #px"^[+-]?[0-9]+(?:(?:\\.[0-9]+)?[eE]\\+?[0-9]+)?$")

(define (parse-num s)
  (cond [(string-prefix? s "~")
         (define r (substring s 1))
         (cond [(equal? r "inf") +inf.0] [(equal? r "-inf") -inf.0] [(equal? r "nan") +nan.0]
               [else (let ([v (string->number r 10 'number-or-false 'decimal-as-inexact)])
                       (unless (real? v) (error 'parse "bad rough ~a" s))
                       (real->double-flonum v))])]
        [else (let ([v (string->number s 10 'number-or-false 'decimal-as-exact)])
                (unless (and v (exact? v) (rational? v)) (error 'parse "bad exact ~a" s))
                v)]))

(define (bits x)
  (apply string-append
         (for/list ([b (in-bytes (real->floating-point-bytes x 8 #t))])
           (string-append (if (< b 16) "0" "") (number->string b 16)))))

(define (fmt v)
  (cond [(boolean? v) (if v "bool:true" "bool:false")]
        [(string? v) (string-append "string:" v)]
        [(eq? v 'none) "none"]
        [(and (number? v) (exact? v) (rational? v)) (string-append "exact:" (number->string v))]
        [(flonum? v) (string-append "rough:" (bits v))]
        [(number? v) "domain:complex"]
        [else (format "harness-error:unexpected ~a" v)]))

;; helpers
(define (int-only . xs) (for ([x xs]) (unless (and (exact? x) (integer? x)) (domain "not an exact integer"))))
(define (fl x) (if (exact? x) (exact->inexact x) x))
;; a double read back as the decimal it prints as (js-numbers' Roughnum.toRational)
(define (float-exact x) (string->number (number->string x) 10 'number-or-false 'decimal-as-exact))
(define (->exact x) (if (exact? x) x (float-exact x)))
;; evaluate at the double nearest x unless that loses the magnitude
(define (->bf x) (let ([d (fl x)]) (if (or (infinite? d) (and (zero? d) (not (zero? x)))) (bf x) (bf d))))
(define (via1 f x) (bigfloat->flonum (f (->bf x))))
(define (via2 f x y) (bigfloat->flonum (f (->bf x) (->bf y))))
(define (round-away x) (let ([e (->exact x)]) (* (if (< e 0) -1 1) (floor (+ (abs e) 1/2)))))
(define (exact-root n k) (let ([r (integer-root n k)]) (and (= (expt r k) n) r)))
(define (pyret-sqrt x)
  (cond [(and (exact? x) (>= x 0))
         (let ([rn (exact-root (numerator x) 2)] [rd (exact-root (denominator x) 2)])
           (if (and rn rd) (/ rn rd) (via1 bfsqrt x)))]
        [else (via1 bfsqrt x)]))
(define (pyret-expt x y)
  (cond [(and (exact? x) (exact? y) (integer? y)) (expt x y)]
        [(and (exact? x) (exact? y) (<= (denominator y) 8))
         (let ([p (numerator y)] [q (denominator y)] [neg (< x 0)])
           (when (and neg (even? q)) (domain "even root of negative"))
           (let ([rn (exact-root (abs (numerator x)) q)] [rd (exact-root (denominator x) q)])
             (if (and rn rd)
                 (* (if (and neg (odd? p)) -1 1) (expt (/ rn rd) p))
                 (* (if (and neg (odd? p)) -1.0 1.0) (via2 bfexpt (abs x) y)))))]
        [else (via2 bfexpt x y)]))
(define (pyret-atan2 y x)
  (when (and (zero? y) (zero? x)) (domain "atan2(0,0)"))
  (let ([r (bfatan2 (->bf y) (->bf x))])
    (bigfloat->flonum (if (bf< r (bf 0)) (bf+ r (bf* (bf 2) pi.bf)) r))))
(define (pyret-remainder a b)
  (when (zero? b) (domain "zero divisor"))
  (if (and (exact? a) (exact? b))
      (- a (* b (truncate (/ a b))))
      (let ([da (fl a)] [db (fl b)])   ; IEEE fmod: fmod(x, inf) = x, fmod(inf, y) undefined, zero keeps x's sign
        (cond [(infinite? da) (domain "non-finite")]
              [(infinite? db) da]
              [else (let* ([ea (inexact->exact da)] [eb (inexact->exact db)]
                           [r (- ea (* eb (truncate (/ ea eb))))])
                      (if (zero? r) (if (< da 0) -0.0 0.0) (exact->inexact r)))]))))
(define (pyret-rel cv tv delta smoothed)
  (set! cv (->exact cv)) (set! tv (->exact tv)) (set! delta (->exact delta))
  (when (< delta 0) (domain "negative tolerance"))
  (if (= cv tv) #t
      (let* ([err (abs (- cv tv))]
             [den (min (abs cv) (abs tv))]
             [den (if smoothed (+ den 1) den)])
        (if (<= delta 1) (<= err (* delta den)) (<= (/ err den) delta)))))
(define (from-string s)
  (if (regexp-match? LIT s)
      (if (string-prefix? s "~")
          (let* ([r (substring s 1)] [v (string->number r 10 'number-or-false 'decimal-as-inexact)])
            (cond [(not v) 'none]
                  [(and (exact? v) (zero? v) (string-prefix? r "-")) -0.0]
                  [else (exact->inexact v)]))
          (let ([v (string->number s 10 'number-or-false 'decimal-as-exact)]) (if v v 'none)))
      'none))
(define (make-bignum s)
  (unless (regexp-match? BIGNUM-LIT s) (domain "not an integer string"))
  (let ([v (string->number s 10 'number-or-false 'decimal-as-exact)])
    (unless (and v (integer? v)) (domain "not an integer string"))
    v))

;; op table: name -> (kinds . procedure). kinds: num double str bool
(define OPS
  (hash
   "add"                (cons '(num num) +)
   "subtract"           (cons '(num num) -)
   "multiply"           (cons '(num num) *)
   "divide"             (cons '(num num) /)
   "equals"             (cons '(num num) =)
   "eqv"                (cons '(num num) (lambda (x y) (and (eq? (exact? x) (exact? y)) (= x y))))
   "equalsAnyZero"      (cons '(num) zero?)
   "lessThan"           (cons '(num num) <)
   "lessThanOrEqual"    (cons '(num num) <=)
   "greaterThan"        (cons '(num num) >)
   "greaterThanOrEqual" (cons '(num num) >=)
   "expt"               (cons '(num num) pyret-expt)
   "exp"                (cons '(num) (lambda (x) (via1 bfexp x)))
   "log"                (cons '(num) (lambda (x) (via1 bflog x)))
   "sin"                (cons '(num) (lambda (x) (via1 bfsin x)))
   "cos"                (cons '(num) (lambda (x) (via1 bfcos x)))
   "tan"                (cons '(num) (lambda (x) (via1 bftan x)))
   "asin"               (cons '(num) (lambda (x) (via1 bfasin x)))
   "acos"               (cons '(num) (lambda (x) (via1 bfacos x)))
   "atan"               (cons '(num) (lambda (x) (via1 bfatan x)))
   "atan2"              (cons '(num num) pyret-atan2)
   "sqrt"               (cons '(num) pyret-sqrt)
   "integerSqrt"        (cons '(num) (lambda (x) (int-only x) (integer-sqrt x)))
   "sqr"                (cons '(num) (lambda (x) (* x x)))
   "abs"                (cons '(num) abs)
   "modulo"             (cons '(num num) (lambda (a b) (int-only a b) (modulo a b)))
   "quotient"           (cons '(num num) (lambda (a b) (int-only a b) (quotient a b)))
   "remainder"          (cons '(num num) pyret-remainder)
   "gcd"                (cons '(num num) (lambda (a b) (int-only a b) (gcd a b)))
   "lcm"                (cons '(num num) (lambda (a b) (int-only a b) (lcm a b)))
   "floor"              (cons '(num) (lambda (x) (floor (->exact x))))
   "ceiling"            (cons '(num) (lambda (x) (ceiling (->exact x))))
   "round"              (cons '(num) round-away)
   "roundEven"          (cons '(num) (lambda (x) (round (->exact x))))
   "numerator"          (cons '(num) (lambda (x) (numerator (->exact x))))
   "denominator"        (cons '(num) (lambda (x) (denominator (->exact x))))
   "toFixnum"           (cons '(num) exact->inexact)
   "toRational"         (cons '(num) ->exact)
   "toExact"            (cons '(num) ->exact)
   "toRoughnum"         (cons '(num) exact->inexact)
   "isInteger"          (cons '(num) (lambda (x) (and (exact? x) (integer? x))))
   "isRational"         (cons '(num) exact?)
   "isExact"            (cons '(num) exact?)
   "isReal"             (cons '(num) (lambda (x) #t))
   "isRoughnum"         (cons '(num) flonum?)
   "isPositive"         (cons '(num) (lambda (x) (> x 0)))
   "isNegative"         (cons '(num) (lambda (x) (< x 0)))
   "isNonPositive"      (cons '(num) (lambda (x) (<= x 0)))
   "isNonNegative"      (cons '(num) (lambda (x) (>= x 0)))
   "isPyretNumber"      (cons '(num) (lambda (x) #t))
   "fromString"         (cons '(str) from-string)
   "fromFixnum"         (cons '(double) (lambda (x) (if (or (infinite? x) (nan? x)) (domain "non-finite") (->exact x))))
   "makeBignum"         (cons '(str) make-bignum)
   "makeRational"       (cons '(num num) (lambda (n d) (int-only n d) (/ n d)))
   "makeRoughnum"       (cons '(double) (lambda (x) x))
   "roughlyEquals"      (cons '(num num num) (lambda (x y d) (when (< d 0) (domain "negative tolerance")) (<= (abs (- (->exact x) (->exact y))) (->exact d))))
   "roughlyEqualsRel"   (cons '(num num num bool) pyret-rel)
   "toRepeatingDecimal" (cons '(num num) (lambda (n d) (int-only n d) (when (<= d 0) (domain "d <= 0")) (/ n d)))
   "toStringDigits"     (cons '(num num) (lambda (n d) (int-only d) (let ([t (expt 10 d)]) (/ (round-away (* n t)) t))))))

(define (parse-arg kind s)
  (case kind
    [(num double) (parse-num s)]
    [(str) s]
    [(bool) (equal? s "true")]))

(define (run-line line)
  (define fields (string-split line "\t" #:trim? #f))
  (define id (car fields))
  (define op (cadr fields))
  (define row (hash-ref OPS op #f))
  (define result
    (cond
      [(not row) "skip"]
      [else
       (with-handlers ([exn:fail:contract:divide-by-zero? (lambda (e) "div-by-zero")]
                       [exn:fail? (lambda (e) (string-append "domain:" (car (string-split (exn-message e) "\n"))))])
         (fmt (apply (cdr row) (map parse-arg (car row) (cddr fields)))))]))
  (printf "~a\t~a\n" id result))

(module+ main
  (for ([line (in-lines)])
    (unless (string=? line "") (run-line line))))
