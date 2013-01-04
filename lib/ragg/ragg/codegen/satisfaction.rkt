#lang racket/base

(provide make-and make-or node? node-val node-yes? visit! add-child!)

(require racket/match)

;; I can't get no... satisfaction.
;;
;; A small module to make sure a small constraint system can be satisfied.
;;
;; Small variation on topological sort where we need both AND and OR nodes.


(struct node (type val yes? parents count-to-satisfy) #:mutable)
;; or nodes are satisfied if any of the children is satisfied.
;; and nodes are satisfied if all of the children are satisfied.


;; visit!: node -> void
;; Visit a node, and marking it if it's all satisfied.  Propagate
;; satisfaction to parents as appropriate.
(define visit!
  (let ()
    (define (dec! n)
      (set-node-count-to-satisfy! n (max 0 (sub1 (node-count-to-satisfy n))))
      (when (and (not (node-yes? n))
                 (= (node-count-to-satisfy n) 0))
        (sat! n)))
    
    (define (sat! n)
      (set-node-yes?! n #t)
      (for ([p (in-list (node-parents n))])
        (dec! p)))
    
    (lambda (n)
      (unless (node-yes? n)
        (when (= (node-count-to-satisfy n) 0)
          (sat! n))))))


;; make-or: X -> node
;; Create an or node
(define (make-or [val #f])
  (node 'or val #f '() 1))


;; make-and: X -> node
;; Create an and node
(define (make-and [val #f])
  (node 'and val #f '() 0))


;; add-child!: node node -> void
;; Attach a child c to the parent node p.
(define (add-child! p c)
  (set-node-parents! c (cons p (node-parents c)))
  (match p
    [(node 'or _ _ _ count-to-satisfy)
     (void)]
    [(node 'and _ _ _ count-to-satisfy)
     (set-node-count-to-satisfy! p (add1 count-to-satisfy))]))


(module* test racket
  (require (submod "..")
           racket/block
           rackunit)
  
  ;; a ::= a
  (block
   ;; Self-looping "a" and-node should not say yes after visiting.
   (define a (make-and 'a))
   (add-child! a a)
   (visit! a)
   (check-false (node-yes? a)))
  
  
  ;; a ::= a
  (block
   ;; Self-looping "a" or-node should not say yes after visiting.
   (define a (make-or 'a))
   (add-child! a a)
   (visit! a)
   (check-false (node-yes? a)))
  
  
  ;; This case should never happen in my situation, but we should check.
  (block
   ;; Empty "a" or-node should not say yes after visiting.
   (define a (make-or 'a))
   (visit! a)
   (check-false (node-yes? a)))
  
  
  ;; a : TOKEN
  (block
   ;; Empty "a" and-node SHOULD say yes after visiting.
   (define a (make-and 'a))
   (visit! a)
   (check-true (node-yes? a)))

  
  ;; a : a | b
  ;; b : TOKEN
  (block
   (define a (make-or 'a))
   (add-child! a a)
   (define b (make-and 'b))
   (add-child! a b)
   (visit! b)
   (check-true (node-yes? a))
   (check-true (node-yes? b)))
  
  ;; a : a b
  ;; b : TOKEN
  (block
   (define a (make-and 'a))
   (define b (make-and 'b))
   (define TOKEN (make-and 'TOKEN))
   (add-child! a a)
   (add-child! a b)
   (add-child! b TOKEN)
   (visit! TOKEN)
   (check-false (node-yes? a))
   (check-true (node-yes? b))
   (check-true (node-yes? TOKEN)))
  
  ;; a : b
  ;; b : a
  (block
   (define a (make-and 'a))
   (define b (make-and 'b))
   (add-child! a b)
   (add-child! b a)
   (check-false (node-yes? a))
   (check-false (node-yes? b)))
  
  ;; a : "a" b
  ;; b : a | b
  (block
   (define a (make-and 'a))
   (define b (make-or 'b))
   (define lit (make-and "a"))
   (add-child! a lit)
   (add-child! a b)
   (add-child! b a)
   (add-child! b b)
   (visit! lit)
   (check-false (node-yes? a))
   (check-false (node-yes? b))
   (check-true (node-yes? lit)))


  ;; x : x y
  ;; y : LIT
  (block
   (define x (make-and "x"))
   (define y (make-and "y"))
   (define lit (make-and "LIT"))
   (add-child! x x)
   (add-child! x y)
   (add-child! y lit)
   (visit! lit)
   (check-false (node-yes? x))
   (check-true (node-yes? y))
   (check-true (node-yes? lit)))


  ;; expr: LPAREN expr RPAREN | ATOM
  (block
   (define LPAREN (make-and))
   (define RPAREN (make-and))
   (define expr (make-or))
   (define expr-1 (make-and))
   (define expr-2 (make-and))
   (define ATOM (make-and))
   (add-child! expr expr-1)
   (add-child! expr expr-2)
   (add-child! expr-1 LPAREN)
   (add-child! expr-1 expr)
   (add-child! expr-1 RPAREN)
   (add-child! expr-2 ATOM)
   (visit! LPAREN)
   (visit! RPAREN)
   (visit! ATOM)
   (check-true (node-yes? expr)))



  ;; expr: LPAREN expr RPAREN
  (block
   (define LPAREN (make-and))
   (define RPAREN (make-and))
   (define expr (make-or))
   (define expr-1 (make-and))
   (define expr-2 (make-and))
   (define ATOM (make-and))
   (add-child! expr expr-1)
   (add-child! expr expr-2)
   (add-child! expr-1 LPAREN)
   (add-child! expr-1 expr)
   (add-child! expr-1 RPAREN)
   (visit! LPAREN)
   (visit! RPAREN)
   (check-false (node-yes? expr)))

  )
