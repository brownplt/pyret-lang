#lang racket/base
(require ragg/rules/stx-types
         (for-syntax racket/base))

(provide flatten-rule
         flatten-rules
         prim-rule)



(define (make-fresh-name)
  (let ([n 0])
    (lambda ()
      (set! n (add1 n))
      (string->symbol (format "%rule~a" n)))))

(define default-fresh-name
  (make-fresh-name))


;; Translates rules to lists of primitive rules.


(define (flatten-rules rules #:fresh-name [fresh-name default-fresh-name])
  (define ht (make-hash))
  (apply append (map (lambda (a-rule) (flatten-rule a-rule
                                                    #:ht ht
                                                    #:fresh-name fresh-name))
                     rules)))


;; flatten-rule: rule -> (listof primitive-rule)
(define (flatten-rule a-rule
                      #:fresh-name [fresh-name default-fresh-name]

                      ;; ht: (hashtableof pattern-hash-key pat)
                      #:ht [ht (make-hash)])

  (let recur ([a-rule a-rule]
              [inferred? #f])

    ;; lift-nonprimitive-pattern: pattern -> (values (listof primitive-rule) pattern)
    ;; Turns non-primitive patterns into primitive patterns, and produces a set of
    ;; derived rules.
    (define (lift-nonprimitive-pattern a-pat)
      (cond
       [(primitive-pattern? a-pat)
        (values '() (linearize-primitive-pattern a-pat))]
       [(hash-has-key? ht (pattern->hash-key a-pat))
        (values '() (list (hash-ref ht (pattern->hash-key a-pat))))]
       [else
        (define head (syntax-case a-pat () [(head rest ...) #'head]))
        (define new-name (datum->syntax #f (fresh-name) a-pat))
        (define new-inferred-id (datum->syntax #f `(inferred-id  ,new-name ,head) a-pat))
        (hash-set! ht (pattern->hash-key a-pat) new-inferred-id)
        (values (recur #`(rule #,new-name #,a-pat) head)
                (list new-inferred-id))]))

    (define (lift-nonprimitive-patterns pats)
      (define-values (rules patterns)
        (for/fold ([inferred-ruless '()]
                   [patternss '()])
                  ([p (in-list pats)])
          (define-values (new-rules new-ps)
            (lift-nonprimitive-pattern p))
          (values (cons new-rules inferred-ruless)
                  (cons new-ps patternss))))
      (values (apply append (reverse rules))
              (apply append (reverse patterns))))
          
    (with-syntax ([head (if inferred? #'inferred-prim-rule #'prim-rule)]
                  [origin (syntax-case a-rule (rule) [(rule name (pat-head rest ...)) #'pat-head])])
      (syntax-case a-rule (rule)
        [(rule name pat)
         (syntax-case #'pat (id inferred-id lit token choice repeat maybe seq)

           ;; The primitive types stay as they are:
           [(id val)
            (list #'(head origin name [pat]))]
           [(inferred-id val reason)
            (list #'(head origin name [pat]))]
           [(lit val)
            (list #'(head origin name [pat]))]
           [(token val)
            (list #'(head origin name [pat]))]

           
           ;; Everything else might need lifting:
           [(choice sub-pat ...)
            (begin
              (define-values (inferred-ruless/rev new-sub-patss/rev)
                (for/fold ([rs '()] [ps '()])
                          ([p (syntax->list #'(sub-pat ...))])
                  (let-values ([(new-r new-p)
                                (lift-nonprimitive-pattern p)])
                    (values (cons new-r rs) (cons new-p ps)))))
              (with-syntax ([((sub-pat ...) ...) (reverse new-sub-patss/rev)])
                (append (list #'(head origin name [sub-pat ...] ...))
                        (apply append (reverse inferred-ruless/rev)))))]

           [(repeat min sub-pat)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-pattern #'sub-pat))
              (with-syntax ([(sub-pat ...) new-sub-pats])
                (cons (cond [(= (syntax-e #'min) 0)
                             #`(head origin name
                                          [(inferred-id name repeat) sub-pat ...]
                                          [])]
                            [(= (syntax-e #'min) 1)
                             #`(head origin name
                                          [(inferred-id name repeat) sub-pat ...]
                                          [sub-pat ...])])
                      inferred-rules)))]

           [(maybe sub-pat)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-pattern #'sub-pat))
              (with-syntax ([(sub-pat ...) new-sub-pats])
                (cons #'(head origin name
                              [sub-pat ...]
                              [])
                      inferred-rules)))]

           [(seq sub-pat ...)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-patterns (syntax->list #'(sub-pat ...))))
              (with-syntax ([(sub-pat ...) new-sub-pats])
                (cons #'(head origin name [sub-pat ...])
                      inferred-rules)))])]))))


;; Given a pattern, return a key appropriate for a hash.
(define (pattern->hash-key a-pat)
  (syntax->datum a-pat))


;; Returns true if the pattern looks primitive
(define (primitive-pattern? a-pat)
  (syntax-case a-pat (id lit token choice repeat maybe seq)
    [(id val)
     #t]
    [(lit val)
     #t]
    [(token val)
     #t]
    [(choice sub-pat ...)
     #f]
    [(repeat min val)
     #f]
    [(maybe sub-pat)
     #f]
    [(seq sub-pat ...)
     (andmap primitive-pattern? (syntax->list #'(sub-pat ...)))]))


;; Given a primitive pattern (id, lit, token, and seqs only containing
;; primitive patterns), returns a linear sequence of just id, lits,
;; and tokens.
(define (linearize-primitive-pattern a-pat)
  (define (traverse a-pat acc)
    (syntax-case a-pat (id inferred-id lit token seq)
      [(id val)
       (cons a-pat acc)]
      [(inferred-id val reason)
       (cons a-pat acc)]
      [(lit val)
       (cons a-pat acc)]
      [(token val)
       (cons a-pat acc)]
      [(seq vals ...)
       (foldl traverse acc (syntax->list #'(vals ...)))]))
  (reverse (traverse a-pat '())))



(define-syntax (prim-rule stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))

(define-syntax (inferred-prim-rule stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))

(define-syntax (inferred-id stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))
