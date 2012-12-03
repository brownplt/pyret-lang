#lang racket

(require
  redex/reduction-semantics
  "../lang/ast.rkt"
  "../lang/runtime.rkt"
  "pyret-core.rkt")

(provide
 redex-compile-pyret
 redex-compile-answer
 extract-pyret-val)

(define (redex-compile-pyret ast-node)
  (define rcp redex-compile-pyret)
  (define (redex-compile-member ast-node)
    (match ast-node
      [(s-data _ name value) (term (,name ,(rcp value)))]))
  (match ast-node
    [(s-num _ n) (term ,n)]
    [(s-str _ s) (term ,s)]
    [(s-block _ b)
     (term-let ([(e ...) (map rcp b)])
               (term (seq e ...)))]
    [(s-obj _ fields)
     (term (object ,(map redex-compile-member fields)))]
    [(s-dot _ object field)
     (term (get-field ,(rcp object) ,(symbol->string field)))]
    [_ (error (format "redex-compile: Haven't handled a case yet: ~a"
                      ast-node))]))

(define (redex-compile-answer val)
  (match val
    [(p-str _ _ _ _ s)
     (term-match πret
                 [(side-condition (σ Σ (vref ref))
                                  (redex-match? πret
                                                (side-condition (str-obj (()) string)
                                                                (equal? (term string)
                                                                        s))
                                                (term (obj-lookup ref Σ))))
                  (term (σ Σ (vref ref)))])]
    [(p-num _ _ _ _ n)
     (term-match πret
                 [(side-condition
                   (σ
                    (name Σ ((ref_1 any_1) ...
                             (ref (num-obj (()) number))
                             (ref_2 any_2) ...))
                    (vref ref))
                   (= n (term number)))
                  (term (σ Σ (vref ref)))])]
    
    [(p-object _ _ _ dict)
     ;; well-formedness assumptions:
     ;; - objects have only one of each field
     (term-match πret
                 [(side-condition
                   (σ
                    (name Σ ((ref_1 any_1) ...
                             (ref (obj-obj (((string_1 v_1) ...))))
                             (ref_2 any_2) ...))
                    (vref ref))
                   (let* ([dict-keys (hash-keys dict)]
                          [semantic-keys (term (string_1 ...))]
                          [semantic-dict (make-hash (map cons semantic-keys (term (v_1 ...))))])
                     (and (equal? (length dict-keys) (length semantic-keys))
                          (andmap
                           (lambda (d-key)
                             (define other-val (hash-ref semantic-dict d-key #f))
                             (if other-val
                                 (let [(result ((redex-compile-answer (hash-ref dict d-key))
                                                (term (σ Σ ,other-val))))]
                                   (= (length result) 1))
                                 #f))
                           dict-keys))))
                  (term (σ Σ (vref ref)))])]
     
    ))

(define extract-pyret-val
  (term-match πret
              [(σ (name Σ ((ref_1 any_1) ...
                             (ref (num-obj (()) number))
                             (ref_2 any_2) ...)) (vref ref))
               (mk-num (term number))]
              [(σ (name Σ ((ref_1 any_1) ...
                             (ref (str-obj (()) string))
                             (ref_2 any_2) ...)) (vref ref))
               (mk-str (term string))]
              [(σ (name Σ ((ref_1 any_1) ...
                             (ref (obj-obj (((string_1 v_1) ...))))
                             (ref_2 any_2) ...)) (vref ref))
               (mk-object (make-hash (map (lambda (s v) (cons s (first (extract-pyret-val (term (σ Σ ,v))))))
                                          (term (string_1 ...)) (term (v_1 ...)))))]))
               
