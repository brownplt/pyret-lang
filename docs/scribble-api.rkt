#lang at-exp racket/base

;; TODO
; - figure out dd/dt/dl
; - get path for loading module docs
; - pull in everything under generated for all-docs

;; Scribble extensions for creating pyret.code.org documentation

(require scribble/base
         scribble/core
         scribble/decode
         scribble/basic
         scribble/html-properties
         racket/list
         racket/dict
         "scribble-helpers.rkt"
         )

(provide docmodule
         function
         lod
         ignore
         ignoremodule
         )

;;;;;;;;;; Functions to sanity check generated documentation ;;;;;;;;;;;;;;;;;;

(define GEN-BASE (build-path 'up "generated" "trove"))
(define curr-doc-checks #f)

(define (init-doc-checker read-docs)
  (map (lambda (mod)
         (list (second mod)
               (make-hash
                (map (lambda (spec) 
                       (cons (get-defn-field 'name spec) #f)) 
                     (drop mod 3)))))
       read-docs))

(define (set-documented! modname name)
  (let ([mod (assoc modname curr-doc-checks)])
    (if mod
        (if (dict-has-key? (second mod) name)
            (if (dict-ref (second mod) name)
                (error 'set-documented! (format "~s is already documented in module ~s" name modname))
                (dict-set! (second mod) name #t))
            (error 'set-documented! (format "Unknown identifier ~s in module ~s" name modname)))
        (error 'set-documented! (format "Unknown module ~s" modname)))))

(define (load-gen-docs)
  (let ([all-docs (directory-list GEN-BASE)])
    (let ([read-docs
           (map (lambda (f) (with-input-from-file (build-path GEN-BASE f) read)) all-docs)])
      (set! curr-doc-checks (init-doc-checker read-docs))
      read-docs)))


;; finds module with given name within all files in docs/generated/arr/*
;; mname is string naming the module
(define (find-module mname)
  (let ([m (findf (lambda (mspec) (equal? (second mspec) mname)) ALL-GEN-DOCS)])
    (unless m
      (error 'find-module (format "WARNING: module not found ~a~n" mname)))
    m))

;; finds definition in defn spec list that has given value for designated field
;; by-field is symbol, indefns is list<specs>
(define (find-defn by-field field-val indefns)
  (let ([d (findf (lambda (d) (equal? field-val (second (assoc by-field (rest d))))) indefns)])
    (unless d
      (error 'find-defn (format "WARNING: no definition for ~a in module ~n" field-val)))
    d))

;; defn-spec is '(fun-spec <assoc>)
(define (get-defn-field field defn-spec)
  (let ([f (assoc field (rest defn-spec))])
    (if f (second f) #f)))

;; extracts the definition spec for the given function name
;; - will look in all modules to find the name
(define (find-doc mname fname)
  (let ([mdoc (find-module mname)])
    (find-defn 'name fname (drop mdoc 3))))

;;;;;;;;;; Styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define css-js-additions (list "foo.css"))

(define (div-style name)
  (make-style name (cons (make-alt-tag "div") css-js-additions)))
(define (pre-style name)
  (make-style name (cons (make-alt-tag "pre") css-js-additions)))

;;;;;;;;;; Scribble functions used in writing documentation ;;;;;;;;;;;;;;;;;;;

(define (ignoremodule name) "")

(define (ignore specnames)
  (for-each (lambda (n) (set-documented! (curr-module-name) n))
            specnames))
  

;; render documentation for all definitions in a module
@(define (docmodule name #:friendly-title (friendly-title #f) 
                    . defs)
  (list (title (or friendly-title name))
        (para "Usage:")
        (nested #:style (pre-style "code") "import " name " as ...")
        defs))

@(define (lod . assocLst)
   (let ([render-for "bs"])
     (second (assoc render-for assocLst))))

;; TODO: parameterize
(define (curr-module-name) "list")

(define dt elem)
(define dd elem)



;; render documentation for a function
@(define (function name 
                   #:contract (contract #f)
                   #:args (args #f)
                   #:alt-docstrings (alt-docstrings #f)
                   . contents
                   )
   (set-documented! (curr-module-name) name)
   (let ([spec (find-doc (curr-module-name) name)])
     ;; checklist
     ; - extract given args or lookup
     ; - get alt-docstrings or lookup doc
     ; - if contract, check arity against generated
     ; - make sure found funspec or unknown-item
     ;; render the scribble 
     (let* ([inputs (if (list? contract) (take contract (sub1 (length contract))) "OOPS")]
            [output (if (list? contract) (last contract) "OOPS")]
            [input-types (map (lambda (i) (if (pair? i) (first i) i)) inputs)]
            [input-descr (map (lambda (i) (if (pair? i) (second i) #f)) inputs)]
            [argnames (or args (get-defn-field 'args spec))]
            [doc (or alt-docstrings (get-defn-field 'doc spec))]
            [arity (get-defn-field 'arity spec)]
            )
       (if argnames
           (if (or (not arity) (eq? arity (length argnames)))
               (nested #:style (div-style "function")
                       (interleave-parbreaks/all
                        (list
                         (nested #:style (div-style "signature")
                                 (interleave-parbreaks/all
                                  (append
                                   (list
                                    (nested #:style (pre-style "code") name " :: " input-types " -> " output)
                                    (para "Returns " output)
                                    (itemlist (map (lambda (name type descr)
                                                     (cond [(and name type descr)
                                                            (item (dt name " :: " type)
                                                              (dd descr))]
                                                           [(and name type)
                                                            (item (dt name " :: " type)
                                                                  (dd ""))]
                                                           [(and name descr)
                                                            (item (dt name) (dd descr))]
                                                           [else (item (dt name) (dd ""))]))
                                                   argnames input-types input-descr))
                                    )
                                   (if doc (list doc) (list)))))
                         (nested #:style (div-style "description") contents)
                         (nested #:style (div-style "examples") 
                                 (para (bold "Examples:"))
                                 "empty for now"))))
               (error 'function (format "Provided argument names do not match expected arity ~a" arity)))
           (error 'function (format "Argument names not provided for name ~s" name))))))

(define ALL-GEN-DOCS (load-gen-docs))
