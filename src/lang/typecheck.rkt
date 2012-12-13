#lang racket

(require "ast.rkt")
(provide typecheck-pyret)

(define (wrap-ann-check loc ann e)
  (s-app loc (ann-check loc ann) (list e)))

(define (mk-lam loc args result body)
  (s-lam loc args result (s-block loc (list body))))

(define (ann-check loc ann)
  (define (mk-flat-checker checker)
    (define argname (gensym))
    (mk-lam loc (list (s-bind loc argname (a-blank))) ann
            (s-app
             loc
             (s-id loc 'check-brand)
             (list checker
                   (s-id loc argname)))))
  (match ann
    [(a-name s id)
     (mk-flat-checker
      (s-id s (string->symbol
               (string-append
                (symbol->string id) "?"))))]
    [(a-blank)
     (mk-flat-checker (s-id loc 'Any?))]
    [(a-any)
     (mk-flat-checker (s-id loc 'Any?))]
    [(a-arrow s args result)
     (define funname (gensym))
     (define wrapargs (map (lambda (a) (s-bind s (gensym) a)) args))
     (define (check-arg bind)
       (match bind
         [(s-bind s id ann) (wrap-ann-check s ann (s-id s id))]))
     (mk-lam s (list (s-bind s funname ann)) ann
      (mk-lam s wrapargs result
       (wrap-ann-check s result 
        (s-app s (s-id s funname) (map check-arg wrapargs)))))]

#|    [(a-app s name params)
     (define (define-parameter-brands s params)
  (s-block s (map (λ (p) (s-def s (symbol-append '%t- p)
				(s-app s (s-id s 'brander) empty))))))

(define (parametrize-brand-fn s name params)
  (define fun-name (symbol-append name '-brand-data))
  (cond
   [(empty? params) empty]
   [else
    (list
     (s-fun s fun-name
	    (cons (s-bind s '%value (a-blank))
		  (map (λ (p) (s-bind s p (a-blank))) params))
	    (a-blank)
	    (s-cond
	     s
	     (append
	      (map
	      (λ (v)
		 (s-cond-branch
		  s (s-app s (s-id s (symbol-append
				      (s-variant-name v)
				      '?))
			   (list (s-id s '%value)))
		  (s-block
		   s
		   (append
		    (map
		    (λ (m)
		       (let ((ann (s-member-ann m)))
			 (if (and (a-app? ann)
				  (not (empty? (list-intersect
						(a-app-parameters ann)
						params))))
			     (list
			      (s-app
			       s
			       (s-dot
				(s-app
				 s
				 (s-dot
				  (s-id s name)
				  'combine)
				 (map
				  (λ (p)
				     (s-id s (symbol-append '%t- p)))
				  (filter
				   (λ (p)
				      (member
				       p
				       (a-app-parameters ann)))
				   params)))
				'brand)
			       (s-dot s (s-id s '%value)
				      (s-member-name m)))
			      ;; call recursively
			      (s-app
			       s
			       (s-id s fun-name)
			       (cons (s-dot s (s-id s '%value)
					    (s-member-name m))
				     params)))
			     empty)
		    (s-variant-members v)))))
		   variants)))
	      (list (s-cond-branch s
				   (s-bool s #t)
				   (s-block s empty))))))))])
		 
|#
    
    [else
     (error
      (format "typecheck: don't know how to check ann: ~a"
              ann))]))

(define (lookup env id)
  (define r (hash-ref env id #f))
  (when (not r) (error (format "Unbound id: ~a" id)))
  r)
(define (update bind env)
  (match bind
    [(s-bind _ id ann)
     (hash-set env id ann)]))


(define (tc-block-env stmts env)
  (foldr update env (map s-def-name (filter s-def? stmts))))

(define (get-arrow s args ann)
  (a-arrow s (map s-bind-ann args) ann))
  
(define (tc-env ast env)
  (define tc (curryr tc-env env))
  (define (tc-member ast env)
    (match ast
      [(s-data-field s name value) (s-data-field s name (tc-env value env))]))
  (match ast
    [(s-block s stmts)
     (define new-env (tc-block-env stmts env))
     (s-block s (map (curryr tc-env new-env) stmts))]
    [(s-def s bnd val)
     (s-def s bnd (wrap-ann-check s (s-bind-ann bnd) (tc val)))]

    [(s-lam s args ann body)
     (define body-env (foldr update env args))
     (wrap-ann-check s
                     (get-arrow s args ann)
                     (s-lam s args ann (tc-env body body-env)))]
    
    ;; TODO(joe): give methods an annotation position for result
    [(s-method s args body)
     (define body-env (foldr update env args))
     (s-method s args (tc-env body body-env))]
    
    [(s-cond s c-bs)
     (define (tc-branch branch)
       (match branch
         [(s-cond-branch s test expr)
          (s-cond-branch s (tc test) (tc expr))]))
     (s-cond s (map tc-branch c-bs))]
    
    [(s-assign s name expr)
     (s-assign s name (wrap-ann-check s (lookup env name) (tc expr)))]

    [(s-app s fun args)
     (s-app s (tc fun) (map tc args))]

    [(s-onion s super fields)
     (s-onion s (tc super) (map (curryr tc-member env) fields))]

    [(s-obj s fields)
     (s-obj s (map (curryr tc-member env) fields))]
    
    [(s-list s elts)
     (s-list s (map tc elts))]
    
    [(s-dot s val field)
     (s-dot s (tc val) field)]
    
    [(s-bracket s val field)
     (s-bracket s (tc val) (tc field))]
    
    [(s-dot-assign s obj field val)
     (s-dot-assign s (tc obj) field (tc val))]

    [(s-dot-method s obj field args)
     (s-dot-method s (tc obj) (tc field) (map tc args))]

    [(or (s-num _ _)
         (s-bool _ _)
         (s-str _ _)
         (s-id _ _)) ast]
    
    [else (error (format "Missed a case in type-checking: ~a" ast))]))

(define (typecheck-pyret ast)
  (tc-env ast (make-immutable-hash)))
  
