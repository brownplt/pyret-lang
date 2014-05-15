#lang at-exp racket/base

;; TODO
; - get path for loading module docs
; - pull in everything under generated for all-docs
; - turn set-documentation! errors into warnings that report at end of module
; - relax xrefs to support items beyond strings (such as method names)
;   idea here is xref["list" '("get" "to" "method")], with anchor formed
;   by string-join if itemspec is a list.

;; Scribble extensions for creating pyret.code.org documentation

(require scribble/base
         scribble/core
         scribble/decode
         scribble/basic
         scribble/html-properties
         (for-syntax racket/base racket/syntax)
         racket/list
         racket/dict
         "scribble-helpers.rkt"
         )

(provide docmodule
         function
         re-export from
         data-spec
         method-spec
         variants
         constr-spec
         singleton-spec
         with-members
         shared
         a-id
         a-arrow
         a-record
         a-field
         members
         member-spec
         lod
         ignore
         ignoremodule
         xref
         )

;;;;;;;;; Parameters and Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tracks the module currently being processed
(define curr-module-name (make-parameter #f))
(define EMPTY-XREF-TABLE (make-hash))

;;;;;;;;;; API for generated module information ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Each module specification has the form
;   (module name path (spec-type (field val) ...) ...)
; where 
;  - spec-type is one of path, fun-spec, unknown-item, data-spec, constr-spec
;  - each spec-type has a field called name

(define mod-name second)
(define mod-path third)
(define mod-specs cdddr)
(define spec-type first)
(define spec-fields rest)
(define field-name first)
(define field-val second)

;;;;;;;;;; Functions to sanity check generated documentation ;;;;;;;;;;;;;;;;;;

(define GEN-BASE (build-path 'up 'up "generated" "trove")) ;; THIS NEEDS HELP!
(define curr-doc-checks #f)

;; print a warning message, optionally with name of issuing function
(define (warning funname msg)
  (if funname
      (printf "WARNING in ~a: ~s~n" funname msg)
      (printf "WARNING: ~s~n" msg)))

(define (init-doc-checker read-docs)
  (map (lambda (mod)
         (list (mod-name mod)
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

(define (report-undocumented modname)
  (let ([mod (assoc modname curr-doc-checks)])
    (if mod
        (dict-for-each (second mod) (lambda (key val)
                                      (unless val
                                        (printf "WARNING: undocumented export ~s from module ~s~n"
                                                key modname))))
        (error 'report-undocumented (format "Unknown module ~s" modname)))))

(define (load-gen-docs)
  (let ([all-docs (directory-list GEN-BASE)])
    (let ([read-docs
           (map (lambda (f) (with-input-from-file (build-path GEN-BASE f) read)) all-docs)])
      (set! curr-doc-checks (init-doc-checker read-docs))
      read-docs)))

;;;;;;;;;;; Functions to extract information from generated documentation ;;;;;;;;;;;;;;

;; finds module with given name within all files in docs/generated/arr/*
;; mname is string naming the module
(define (find-module mname)
  (let ([m (findf (lambda (mspec) (equal? (mod-name mspec) mname)) ALL-GEN-DOCS)])
    (unless m
      (error 'find-module (format "WARNING: module not found ~a~n" mname)))
    m))

;; finds definition in defn spec list that has given value for designated field
;; by-field is symbol, indefns is list<specs>
(define (find-defn by-field for-val indefns)
  (let ([d (findf (lambda (d) (equal? for-val (field-val (assoc by-field (spec-fields d))))) indefns)])
    (unless d
      (error 'find-defn (format "WARNING: no definition for ~a in module ~n" field-val)))
    d))

;; defn-spec is '(fun-spec <assoc>)
(define (get-defn-field field defn-spec)
  (let ([f (assoc field (spec-fields defn-spec))])
    (if f (field-val f) #f)))

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

(define (span-style name)
  (make-style name (cons (make-alt-tag "span") css-js-additions)))

; style that drops html anchor -- use only with elems
(define (anchored-elem-style anchor)
  (make-style "anchor" (list (make-alt-tag "span") (url-anchor anchor))))

(define dl-style (make-style "dl" (list (make-alt-tag "dl"))))
(define dt-style (make-style "dt" (list (make-alt-tag "dt"))))
(define dd-style (make-style "dd" (list (make-alt-tag "dd"))))

;;;;;;;;;; Cross-Reference Infrastructure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Uses the xref table for error checking that
;   we aren't generating links to unknown targets  
; TODO: fix path to html file in "file" definition
(define (xref modname itemname)
  (let [(cur-mod (curr-module-name))]
    (traverse-element
     (lambda (get set!)
       (traverse-element
        (lambda (get set!)
          (let* ([xref-table (get 'doc-xrefs '())]
                 [entry (assoc itemname xref-table)])
            (when (string=? modname cur-mod)
                (unless (and entry (string=? (second entry) modname))
                  (error 'xref "No xref info for ~a in ~a~nxref-table = ~s~n" itemname modname xref-table)))
            (let* ([file (path->string 
                          (build-path (current-directory) 
                                      (string-append modname ".html#" itemname)))]) ; fix here if change anchor format
              (hyperlink file itemname)))))))))

; drops an "a name" anchor for cross-referencing
(define (drop-anchor name)
  (elem #:style (anchored-elem-style name) ""))

;;;;;;;;;; Scribble functions used in writing documentation ;;;;;;;;;;;;;;;;;;;

(define (ignoremodule name) "")

(define (ignore specnames)
  (for-each (lambda (n) (set-documented! (curr-module-name) n))
            specnames))
  
; generates dt for use in dl-style itemizations
(define (dt . args) 
  (elem #:style dt-style args))

; generates dd for use in dl-style itemizations
(define (dd . args) 
  (elem #:style dd-style args))

;; docmodule is a macro so that we can parameterize the
;; module name before processing functions defined within
;; the module.  Need this since module contents are nested
;; within the module specification in the scribble sources
@(define-syntax (docmodule stx)
   (syntax-case stx ()
     [(_ name args ...)
      (syntax/loc stx
        (parameterize ([curr-module-name name])
          (let ([contents (docmodule-internal name args ...)])
            (report-undocumented name)
            contents)))]))

;; render documentation for all definitions in a module
;; this function does the actual work after syntax expansion
@(define (docmodule-internal name 
                             #:friendly-title (friendly-title #f) 
                             . defs)
   (list (title (or friendly-title name))
         (para "Usage:")
         (nested #:style (pre-style "code") "import " name " as ...")
         (interleave-parbreaks/all defs)))

@(define (lod . assocLst)
   (let ([render-for "bs"])
     (second (assoc render-for assocLst))))

;; render re-exports
@(define (re-export name from . contents)
   (set-documented! (curr-module-name) name)
   (list "For " (elem #:style (span-style "code") name) ", see " from))

@(define (from where)
   (list where))


@(define (data-spec name . members)
   (set-documented! (curr-module-name) name)
   (let ([processing-module (curr-module-name)])
     (interleave-parbreaks/all
      (list (drop-anchor name)
            (section name)
            (traverse-block ; use this to build xrefs on an early pass through docs
             (lambda (get set!)
               (set! 'doc-xrefs (cons (list name processing-module)
                                      (get 'doc-xrefs '())))
               (nested #:style (div-style "data-defn")
                             "Nothing yet")))))))
@(define (method-spec name #:contract (contract #f) . body)
   (list @section[name] body))
@(define (member-spec name #:contract (contract #f) . body)
   (list @subsection[name] body))
@(define (singleton-spec name . body)
   (set-documented! (curr-module-name) name)
   (list @section[name] body))
@(define (constr-spec name . body)
   (set-documented! (curr-module-name) name)
   (list @section[name] name))
@(define (with-members . members)
   members)
@(define (members . mems)
   mems)
@(define (a-id name . args)
   (list
    (if (cons? args) (first args) name)))
@(define (a-arrow . typs)
   (append (list "(") (add-between typs ", " #:before-last " -> ") (list ")")))
@(define (a-record . fields)
   fields)
@(define (a-field name type . desc)
   desc)
@(define (variants . vars)
   vars)
@(define (shared . shares)
   shares)
  

;; render documentation for a function
@(define (function name 
                   #:contract (contract #f)
                   #:args (args #f)
                   #:alt-docstrings (alt-docstrings #f)
                   . contents
                   )
   (let ([spec (find-doc (curr-module-name) name)])
     (let* ([argnames (if (list? args) (map first args) (get-defn-field 'args spec))]
            [input-types (map (lambda(i) (first (drop contract (+ 1 (* 2 i))))) (range 0 (length argnames)))]
            [input-descr (if (list? args) (map second args) (map (lambda(i) #f) argnames))]
            [doc (or alt-docstrings (get-defn-field 'doc spec))]
            [arity (get-defn-field 'arity spec)]
            )
       ;; checklist
       ; - TODO: make sure found funspec or unknown-item
       ; confirm argnames provided
       (unless argnames
         (error 'function (format "Argument names not provided for name ~s" name)))
       ; if contract, check arity against generated
       (unless (or (not arity) (eq? arity (length argnames)))
         (error 'function (format "Provided argument names do not match expected arity ~a" arity))) 
       ; error checking complete, record name as documented
       (set-documented! (curr-module-name) name)
       ;; render the scribble 
       ; defining processing-module because raw ref to curr-module-name in traverse-block
       ;  wasn't getting bound properly -- don't know why
       (let ([processing-module (curr-module-name)])
         (interleave-parbreaks/all
          (list (drop-anchor name)
                (traverse-block ; use this to build xrefs on an early pass through docs
                 (lambda (get set!)
                   (set! 'doc-xrefs (cons (list name processing-module)
                                          (get 'doc-xrefs '())))
                   (nested #:style (div-style "function")
                           (interleave-parbreaks/all
                            (list
                             (nested #:style (div-style "signature")
                                     (interleave-parbreaks/all
                                      (append
                                       (list
                                        (nested #:style (pre-style "code") name " :: " contract)
                                        (para #:style dl-style
                                              (map (lambda (name type descr)
                                                     (cond [(and name type descr)
                                                            (list (dt name " :: " type)
                                                                  (dd descr))]
                                                           [(and name type)
                                                            (list (dt name " :: " type)
                                                                  (dd ""))]
                                                           [(and name descr)
                                                            (list (dt name) (dd descr))]
                                                           [else (list (dt name) (dd ""))]))
                                                   argnames input-types input-descr))
                                        )
                                       (if doc (list doc) (list)))))
                             (nested #:style (div-style "description") contents)
                             (nested #:style (div-style "examples") 
                                     (para (bold "Examples:"))
                                     "empty for now")))))))))
         )))

(define ALL-GEN-DOCS (load-gen-docs))
