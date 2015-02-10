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
         racket/bool
         racket/dict
         racket/path
         racket/runtime-path
         "scribble-helpers.rkt"
         )

(provide docmodule
         function
         render-fun-helper
         re-export from
         pyret pyret-id pyret-method pyret-block
         tag-name
         type-spec
         data-spec
         data-spec2
         method-doc
         method-spec
         variants
         collection-doc
         constructor-spec
         constructor-doc
         constr-spec
         singleton-spec
         singleton-doc
         singleton-spec2
         with-members
         shared
         examples
         a-compound
         a-id
         a-arrow
         a-named-arrow
         a-record
         a-field
         a-app
         a-pred
         a-dot
         members
         member-spec
         lod
         ignore
         ignoremodule
         xref
         init-doc-checker
         append-gen-docs
         curr-module-name
         make-header-elt-for
         )

;;;;;;;;; Parameters and Constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; tracks the module currently being processed
(define curr-module-name (make-parameter #f))
(define curr-data-spec (make-parameter #f))
(define curr-var-spec (make-parameter #f))
(define curr-method-location (make-parameter 'shared))
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
(define (spec-fields l) (if (cons? l) (rest l) #f))
(define field-name first)
(define field-val second)



;;;;;;;;;; Functions to sanity check generated documentation ;;;;;;;;;;;;;;;;;;

;; Maybe something like this could be put in the Racket standard library?
(define-runtime-path HERE ".")

(define GEN-BASE (build-path (path-only HERE) "generated" "trove"))
(define curr-doc-checks #f)

;; print a warning message, optionally with name of issuing function
(define (warning funname msg)
  (if funname
      (eprintf "WARNING in ~a: ~a~n" funname msg)
      (eprintf "WARNING: ~a~n" msg)))

(define (read-mod mod)
  (list (mod-name mod)
        (make-hash
         (map (lambda (spec)
                (cons (get-defn-field 'name spec) #f))
              (drop mod 3)))))
(define (init-doc-checker read-docs)
  (map read-mod read-docs))

(define (set-documented! modname name)
  (let ([mod (assoc modname curr-doc-checks)])
    (if mod
        (if (dict-has-key? (second mod) name)
            (if (dict-ref (second mod) name)
                (warning 'set-documented! (format "~s is already documented in module ~s" name modname))
                (dict-set! (second mod) name #t))
            (begin
              (warning 'set-documented! (format "Unknown identifier ~s in module ~s" name modname))))
        (warning 'set-documented! (format "Unknown module ~s" modname)))))

(define (report-undocumented modname)
  (let ([mod (assoc modname curr-doc-checks)])
    (if mod
        (dict-for-each
         (second mod)
         (lambda (key val)
           (unless val (warning 'report-undocumented
                                (format "Undocumented export ~s from module ~s"
                                        key modname)))))
        (warning 'report-undocumented (format "Unknown module ~s" modname)))))

(define (load-gen-docs)
  (let ([all-docs (filter (lambda(f)
                            (let ([str (path->string f)])
                              (and
                                (not (string=? (substring str (- (string-length str) 4)) ".bak"))
                                (not (string=? (substring str (- (string-length str) 1)) "~"))
                                (not (string=? (substring str 0 1) "."))
                                ))
                            ) (directory-list GEN-BASE))])
    (let ([read-docs
           (map (lambda (f) (with-input-from-file (build-path GEN-BASE f) read)) all-docs)])
      (set! curr-doc-checks (init-doc-checker read-docs))
      ;(printf "Modules are ~s~n" (map first curr-doc-checks))
      read-docs)))

;;;;;;;;;;; Functions to extract information from generated documentation ;;;;;;;;;;;;;;

;; finds module with given name within all files in docs/generated/arr/*
;; mname is string naming the module
(define (find-module mname)
  (let ([m (findf (lambda (mspec)
    (equal? (mod-name mspec) mname)) ALL-GEN-DOCS)])
    (unless m
      (error 'find-module (format "Module not found ~a~n" mname)))
    m))

;; finds definition in defn spec list that has given value for designated field
;; by-field is symbol, indefns is list<specs>
(define (find-defn/nowarn by-field for-val indefns)
  (if (or (empty? indefns) (not indefns))
      #f
      (let ([d (findf (lambda (d)
                        (and (list? (spec-fields d))
                             (equal? for-val (field-val (assoc by-field (spec-fields d)))))) indefns)])
        d)))


;; finds definition in defn spec list that has given value for designated field
;; by-field is symbol, indefns is list<specs>
(define (find-defn by-field for-val indefns)
  (if (or (empty? indefns) (not indefns))
      #f
      (let ([d (findf (lambda (d)
                        (and (list? (spec-fields d))
                             (equal? for-val (field-val (assoc by-field (spec-fields d)))))) indefns)])
        (unless d
          (warning 'find-defn (format "No definition for field '~a = \"~a\" in module ~s" by-field for-val indefns)))
        d)))

;; defn-spec is '(fun-spec <assoc>)
(define (get-defn-field field defn-spec)
  (if (or (empty? defn-spec) (not defn-spec)) #f
      (let ([f (assoc field (spec-fields defn-spec))])
        (if f (field-val f) #f))))

;; extracts the definition spec for the given function name
;; - will look in all modules to find the name
(define (find-doc mname fname)
  (let ([mdoc (find-module mname)])
    (find-defn 'name fname (drop mdoc 3))))
(define (find-doc/nowarn mname fname)
  (let ([mdoc (find-module mname)])
    (find-defn/nowarn 'name fname (drop mdoc 3))))

;;;;;;;;;; Styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define css-js-additions (list "style.css"))

(define (div-style name)
  (make-style name (cons (make-alt-tag "div") css-js-additions)))

(define (pre-style name)
  (make-style name (list (make-alt-tag "pre"))))

(define code-style (make-style "pyret-code" (cons (make-alt-tag "span") css-js-additions)))

(define (span-style name)
  (make-style name (cons (make-alt-tag "span") css-js-additions)))

; style that drops html anchor -- use only with elems
(define (anchored-elem-style anchor)
  (make-style "anchor" (list (make-alt-tag "span") (url-anchor anchor))))

(define (dl-style name) (make-style name (list (make-alt-tag "dl"))))
(define (dt-style name) (make-style name (list (make-alt-tag "dt"))))
(define (dd-style name) (make-style name (list (make-alt-tag "dd"))))

(define (pyret-block . body) (nested #:style (pre-style "pyret-highlight") (apply literal body)))
(define (pyret . body) (elem #:style (span-style "pyret-highlight") (apply tt body)))
(define (pyret-id id (mod (curr-module-name)))
  (seclink (xref mod id) (tt id)))
(define (pyret-method datatype id (mod (curr-module-name)))
  (seclink (xref mod datatype id) (tt (string-append "." id))))

;;;;;;;;;; Cross-Reference Infrastructure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Uses the xref table for error checking that
;   we aren't generating links to unknown targets
; TODO: fix path to html file in "file" definition
(define (xref modname itemname . subitems)
  (apply tag-name (cons modname (cons itemname subitems))))
;  (let [(cur-mod (curr-module-name))]
;    (traverse-element
;     (lambda (get set!)
;       (traverse-element
;        (lambda (get set!)
;          (let* ([xref-table (get 'doc-xrefs '())]
;                 [entry (assoc itemname xref-table)])
;            (when (string=? modname cur-mod)
;                (unless (and entry (string=? (second entry) modname))
;                  (error 'xref "No xref info for ~a in ~a~nxref-table = ~s~n" itemname modname xref-table)))
;            (let* ([file (path->string
;                          (build-path (current-directory)
;                                      (string-append modname ".html#" itemname)))]) ; fix here if change anchor format
;              (hyperlink file itemname)))))))))

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
  (elem #:style (dt-style "") args))
; generates dt for use in dl-style itemizations
(define (dt-indent . args)
  (elem #:style (dt-style "indent-arg") args))

; generates dd for use in dl-style itemizations
(define (dd . args)
  (elem #:style (dd-style "") args))

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
                             #:noimport (noimport #f)
                             . defs)
    (list (title #:tag (tag-name name) (or friendly-title name))
          (if noimport ""
                       (list (para "Usage:")
                             (nested #:style (pre-style "code") "import " name " as ...")))
          defs))

@(define (lod . assocLst)
   (let ([render-for "bs"])
     (second (assoc render-for assocLst))))

;; render re-exports
@(define (re-export name from . contents)
   (set-documented! (curr-module-name) name)
   (list "For " (elem #:style (span-style "code") name) ", see " from))

@(define (from where)
   (secref where))

@(define (tag-name . args)
   (apply string-append (add-between args "_")))

@(define (type-spec type-name tyvars)
  (set-documented! (curr-module-name) type-name)
  (define name-part (make-header-elt-for (seclink (xref (curr-module-name) type-name) (tt type-name)) type-name))
  (define vars (if (cons? tyvars) (append (list "<") (add-between tyvars ",") (list ">")) ""))
    (para #:style (div-style "boxed")
      (list name-part (tt vars))))

@(define-syntax (data-spec2 stx)
  (syntax-case stx()
    [(_ name tyvars args ...)
      (syntax/loc stx
        (parameterize ([curr-data-spec (find-doc (curr-module-name) name)])
         (let ([contents (data-spec-internal2 name tyvars args ...)])
           contents)))]))

@(define (make-header-elt-for elt name)
  (define tag (list 'part (tag-name (curr-module-name) name)))
  (toc-target-element code-style elt tag))

@(define (data-spec-internal2 data-name tyvars variants)
  (define name-part (make-header-elt-for (seclink (xref (curr-module-name) data-name) (tt data-name)) data-name))
  (define vars (if (cons? tyvars) (append (list "<") (add-between tyvars ",") (list ">")) ""))
    (nested #:style (div-style "boxed")
      (list
        (tt "data " name-part vars ":")
        (apply para #:style (dl-style "multiline-args") variants)
        (tt "end"))))

@(define (singleton-doc data-name variant-name return . body)
  (define name-elt (make-header-elt-for (seclink (xref (curr-module-name) variant-name) (tt variant-name)) variant-name))
  (define detector-name (string-append "is-" variant-name))
  (append
    (list 
      (para #:style (div-style "boxed") (tt name-elt " :: " return))
      #;(render-fun-helper
       '(fun) detector-name 
       (list 'part (tag-name (curr-module-name) detector-name))
       (a-arrow (a-id "Any") (a-id "Boolean" (xref "<global>" "Boolean")))
       (a-id "Boolean" (xref "<global>" "Boolean")) (list (list "value" #f)) '() '() '()))
    body))

@(define (constructor-doc data-name variant-name members return . body)
  (define name-part (make-header-elt-for variant-name variant-name))
  (define member-types (map (lambda (m) (cdr (assoc "contract" (rest m)))) members))
  (define members-as-args (map (lambda (m) `(,(first m) #f)) members))
  (define detector-name (string-append "is-" variant-name))
  (append
    (list
      (render-fun-helper
       '(fun) variant-name
       (list 'part (tag-name (curr-module-name) variant-name))
       (apply a-arrow (append member-types (list return)))
       return members-as-args '() '() '())
      #;(render-fun-helper
       '(fun) detector-name 
       (list 'part (tag-name (curr-module-name) detector-name))
       (a-arrow (a-id "Any") (a-id "Boolean" (xref "<global>" "Boolean")))
       (a-id "Boolean" (xref "<global>" "Boolean")) (list (list "value" #f)) '() '() '()))
    body))

@(define (singleton-spec2 data-name variant-name)
  (set-documented! (curr-module-name) variant-name)
  (define processing-module (curr-module-name))
  (define name (seclink (xref processing-module variant-name) (tt variant-name)))
  (list (dt-indent (tt "| " name))))

;; String String List<Member> -> Scribbly-thing
@(define (constructor-spec data-name variant-name members)
  (define processing-module (curr-module-name))
  (define args (map (lambda (m)
    (define name (first m))
    (define type (cdr (assoc "type" (rest m))))
    (define contract (cdr (assoc "contract" (rest m))))
    (define modifier (if (equal? type "mutable") "mutable " ""))
    (if contract (tt modifier name " :: " contract) (tt modifier name))) members))
  (define name (seclink (xref processing-module variant-name) (tt variant-name)))
  (list (dt-indent (tt "| " name "(" (add-between args ", ") ")"))))


;@(define (member-spec2 data-name variant-name name type contract)

@(define-syntax (data-spec stx)
   (syntax-case stx ()
     [(_ name args ...)
      (syntax/loc stx
        (parameterize ([curr-data-spec (find-doc (curr-module-name) name)])
         (let ([contents (data-spec-internal name args ...)])
           contents)))]))
@(define (data-spec-internal name #:params (params #f) . members)
   (set-documented! (curr-module-name) name)
   (let ([processing-module (curr-module-name)])
     (interleave-parbreaks/all
      (list (drop-anchor name)
            (subsubsub*section #:tag (tag-name (curr-module-name) name))
            (traverse-block ; use this to build xrefs on an early pass through docs
             (lambda (get set!)
               (set! 'doc-xrefs (cons (list name processing-module)
                                      (get 'doc-xrefs '())))
               @para{}))
            (interleave-parbreaks/all members)))))
@(define (method-doc data-name var-name name
                      #:params (params #f)
                      #:contract (contract #f)
                      #:return (return #f)
                      #:args (args #f)
                      #:alt-docstrings (alt-docstrings #f)
                      #:examples (examples '())
                      . body)
  (let* ([spec (or (find-defn/nowarn 'name name
                      (get-defn-field 'with-members (find-doc/nowarn (curr-module-name) var-name)))
                   (find-defn/nowarn 'name name
                      (get-defn-field 'shared (find-doc/nowarn (curr-module-name) data-name)))
                   (find-defn/nowarn 'name name
                      (get-defn-field 'with-members
                         (find-defn/nowarn 'name var-name
                            (drop (find-doc/nowarn (curr-module-name) data-name) 3)))))])
      (unless spec
        (warning 'method-doc
          (format "No definition for method ~a for data ~a and variant ~a in module ~s"
                  name data-name var-name (curr-module-name))))
      (render-fun-helper
        spec name
        (list 'part (tag-name (curr-module-name) data-name name))
        contract return args alt-docstrings examples body)))
@(define (method-spec name
                      #:params (params #f)
                      #:contract (contract #f)
                      #:return (return #f)
                      #:args (args #f)
                      #:alt-docstrings (alt-docstrings #f)
                      #:examples (examples '())
                      . body)
   (let* ([methods (get-defn-field (curr-method-location) (curr-var-spec))]
          [var-name (get-defn-field 'name (curr-var-spec))]
          [spec (find-defn 'name name methods)])
     (render-fun-helper
      spec name
      (list 'part (tag-name (curr-module-name) var-name name))
      contract return args alt-docstrings examples body)))
@(define (member-spec name #:type (type-in #f) #:contract (contract-in #f) . body)
   (let* ([members (get-defn-field 'members (curr-var-spec))]
          [member (if (list? members) (assoc name members) #f)]
          [contract (or contract-in (interp (get-defn-field 'contract member)))]
          [modifier (if (equal? type-in "mutable") "mutable " "")])
     (list (dt (if contract (tt modifier name " :: " contract) (tt modifier name)))
           (dd body))))

@(define-syntax (singleton-spec stx)
   (syntax-case stx ()
     [(_ name args ...)
      (syntax/loc stx
        (parameterize ([curr-var-spec (find-doc (curr-module-name) name)]
                       [curr-method-location 'with-members])
          (let ([contents (singleton-spec-internal name args ...)])
            contents)))]))
@(define (singleton-spec-internal name #:private (private #f) . body)
   (if private
       (list (subsubsub*section name) body)
       (begin
         (when (not private) (set-documented! (curr-module-name) name))
         (list (subsubsub*section #:tag (list (tag-name (curr-module-name) name) (tag-name (curr-module-name) (string-append "is-" name))) name) body))))

@(define-syntax (constr-spec stx)
   (syntax-case stx ()
     [(_ name args ...)
      (syntax/loc stx
        (parameterize ([curr-var-spec (find-doc (curr-module-name) name)]
                       [curr-method-location 'with-members])
          (let ([contents (constr-spec-internal name args ...)])
            contents)))]))
@(define (constr-spec-internal name #:params (params #f) #:private (private #f) . body)
   (if private
       (list (subsubsub*section name) body)
       (begin
         (when (not private) (set-documented! (curr-module-name) name))
         (list (subsubsub*section #:tag (list (tag-name (curr-module-name) name) (tag-name (curr-module-name) (string-append "is-" name))) name) body))))

@(define (with-members . members)
   (if (empty? members) 
       empty
       (list "Methods" members)))
@(define (members . mems)
   (if (empty? mems)
       empty
       (list "Fields" (para #:style (dl-style "fields") mems))))
@(define (a-id name . args)
   (if (cons? args) (seclink (first args) (tt name)) (tt name)))
@(define (a-compound typ . args)
   (if (cons? args) (seclink (first args) typ) typ))
@(define (a-app base . typs)
   (append (list base "<") (add-between typs ", ") (list ">")))
@(define (a-pred base refinement)
   (list base "%(" refinement ")"))
@(define (a-dot base field)
   (list base "." field))
@(define (a-arrow . typs)
   (append (list "(") (add-between typs ", " #:before-last " -> ") (list ")")))
@(define (every-other lst)
  (cond
    [(empty? lst) empty]
    [(cons? lst)
      (define r (rest lst))
      (cond
        [(empty? r) lst]
        [(cons? r) (cons (first lst) (every-other (rest (rest lst))))])]))
@(define (a-named-arrow . names-and-typs)
   (define all-but-last (take names-and-typs (- (length names-and-typs) 1)))
   (define last (first (drop names-and-typs (- (length names-and-typs) 1))))
   (define names (every-other all-but-last))
   (define typs (every-other (rest all-but-last)))
   (define pairs (map (λ (n t) (list n " :: " t)) names typs))
   (append (list "(") (add-between (append pairs (list last)) ", " #:before-last " -> ") (list ")")))
@(define (a-record . fields)
   (append (list "{") (add-between fields ", ") (list "}")))
@(define (a-field name type . desc)
   (append (list name ":") (list type)))
@(define (variants . vars)
   vars)
@(define-syntax (shared stx)
   (syntax-case stx ()
     [(_ args ...)
      (syntax/loc stx
        (parameterize ([curr-var-spec (curr-data-spec)]
                       [curr-method-location 'shared])
          (let ([contents (shared-internal args ...)])
            contents)))]))
@(define (shared-internal . shares)
   (if (empty? shares)
       empty
       (list "Shared Methods" shares)))

@(define (interp an-exp)
   (cond
     [(list? an-exp)
      (let* ([f (first an-exp)]
             [args (map interp (rest an-exp))])
        (cond
          [(symbol=? f 'a-record) (apply a-record args)]
          [(symbol=? f 'a-id) (apply a-id args)]
          [(symbol=? f 'a-compound) (apply a-compound args)]
          [(symbol=? f 'a-arrow) (apply a-arrow args)]
          [(symbol=? f 'a-field) (apply a-field args)]
          [(symbol=? f 'a-app) (apply a-app args)]
          [(symbol=? f 'a-pred) (apply a-pred args)]
          [(symbol=? f 'a-dot) (apply a-dot args)]
          [(symbol=? f 'xref) (apply xref args)]
          [#t an-exp]))]
     [#t an-exp]))


@(define (render-multiline-args names types descrs)
   (map (lambda (name type descr)
          (cond [(and name type descr)
                 (list (dt-indent (tt name " :: " type))
                       (dd descr))]
                [(and name type)
                 (list (dt-indent (tt name " :: " type))
                       (dd ""))]
                [(and name descr)
                 (list (dt-indent (tt name)) (dd descr))]
                [else (list (dt-indent (tt name)) (dd ""))]))
        names types descrs))

@(define (render-singleline-args names types)
  (define args
   (map (lambda (name type)
          (cond [(and name type)
                 (list (tt name " :: " type))]
                [else (list (tt name))]))
        names types))
  (add-between args ", "))

;; render documentation for a function
@(define (render-fun-helper spec name part-tag contract-in return-in args alt-docstrings examples contents)
   (define is-method (symbol=? (first spec) 'method-spec))
   (let* ([contract (or contract-in (interp (get-defn-field 'contract spec)))] 
          [return (or return-in (interp (get-defn-field 'return spec)))] 
          [orig-argnames (if (list? args) (map first args) (get-defn-field 'args spec))]
          [input-types (map (lambda(i) (first (drop contract (+ 1 (* 2 i))))) (range 0 (length orig-argnames)))]
          [argnames (if is-method (drop orig-argnames 1) orig-argnames)]
          [input-types (if is-method (drop input-types 1) input-types)]
          [input-descr (if (list? args) (map second args) (map (lambda(i) #f) orig-argnames))]
          [doc (or alt-docstrings (get-defn-field 'doc spec))]
          [arity (if args (length args) (get-defn-field 'arity spec))]
          [arity (if is-method (- arity 1) arity)]
;          [_ (printf "Input: ~a ~a ~a\n" name input-descr args)]
          [input-descr (if is-method (drop input-descr 1) input-descr)]
          )
     ;; checklist
     ; - TODO: make sure found funspec or unknown-item
     ; confirm argnames provided
     (unless argnames
       (error 'function (format "Argument names not provided for name ~s" name)))
     ; if contract, check arity against generated
     (unless (or (not arity) (eq? arity (length argnames)))
       (error 'function (format "Provided argument names do not match expected arity ~a ~a" arity name)))
     ;; render the scribble
     ; defining processing-module because raw ref to curr-module-name in traverse-block
     ;  wasn't getting bound properly -- don't know why
     (let ([processing-module (curr-module-name)])
       (define name-tt (if is-method (tt "." name) (seclink (xref processing-module name) (tt name))))
       (define name-elt (toc-target-element code-style (list name-tt) part-tag))
       (interleave-parbreaks/all
        (list ;;(drop-anchor name)
          (traverse-block ; use this to build xrefs on an early pass through docs
           (lambda (get set!)
             (set! 'doc-xrefs (cons (list name processing-module)
                                    (get 'doc-xrefs '())))
;             (define name-elt (seclink (xref processing-module name) name-target))
;             (printf "argnames: ~a, ~a\n" argnames (length argnames))
             (define no-descrs (or (empty? input-descr) (ormap (lambda (v) (false? v)) input-descr)))
             (define header-part
               (cond
                [(and (< (length argnames) 3) no-descrs)
                 (apply para #:style (div-style "boxed pyret-header")
                   (append
                    (list (tt name-elt " :: " "("))
                    (render-singleline-args argnames input-types)
                    (if return
                      (list (tt ")" " -> " return))
                      (list (tt ")")))))] 
                [else 
                 (nested #:style (div-style "boxed")
                 (apply para #:style (dl-style "multiline-args")
                   (append
                    (list (dt name-elt " :: " "("))
                    (render-multiline-args argnames input-types input-descr)
                    (if return
                      (list (dt (tt ")")) (dt (tt "-> " return)))
                      (list (dt (tt ")")))))))]))
             (nested #:style (div-style "function")
                     (cons
                       header-part
                       (interleave-parbreaks/all
                        (append
                          (if doc (list doc) (list))
                          (list (nested #:style (div-style "description") contents))
                          (list (if (andmap whitespace? examples)
                            (nested #:style (div-style "examples") "")
                            (nested #:style (div-style "examples")
                                    (para (bold "Examples:"))
                                    (apply pyret-block examples)))))))))))))))

@(define (collection-doc name arg-pattern return)
  (define name-part (make-header-elt-for (seclink (xref (curr-module-name) name) (tt name)) name))
  (define patterns (add-between (map (lambda (a) (list (car a) " :: " (cdr a))) arg-pattern) ","))
  (para #:style (div-style "boxed pyret-header")
    (tt "[" name-part ": " patterns ", ..." "] -> " return)))

@(define (examples . body)
  (nested #:style (div-style "examples")
          (para (bold "Examples:"))
          (apply pyret-block body)))

@(define (function name
                   #:contract (contract #f)
                   #:return (return #f)
                   #:args (args #f)
                   #:alt-docstrings (alt-docstrings #f)
                   #:examples (examples '())
                   . contents
                   )
   (let ([ans
          (render-fun-helper
           (find-doc (curr-module-name) name) name
           (list 'part (tag-name (curr-module-name) name))
           contract return args alt-docstrings examples contents)])
          ; error checking complete, record name as documented
     (set-documented! (curr-module-name) name)
     ans))

(define ALL-GEN-DOCS (load-gen-docs))

;; finds module with given name within all files in docs/generated/arr/*
;; mname is string naming the module
(define (check-module mname)
  (let ([m (findf (lambda (mspec)
    (equal? (mod-name mspec) mname)) ALL-GEN-DOCS)])
    m))


(define (append-gen-docs s-exp)
  (define mod (read-mod s-exp))
  (define modname (second s-exp))
  (define existing-mod (check-module modname))
  (if
    existing-mod
    (let ()
      (define new-elts (drop s-exp 3))
      (define without-orig (remove (lambda(m) (equal? (second m) (second s-exp))) ALL-GEN-DOCS))
      (set! ALL-GEN-DOCS (cons (append existing-mod new-elts) without-orig)))
    (set! ALL-GEN-DOCS (cons s-exp ALL-GEN-DOCS)))
  (set! curr-doc-checks (init-doc-checker ALL-GEN-DOCS))
  '())

