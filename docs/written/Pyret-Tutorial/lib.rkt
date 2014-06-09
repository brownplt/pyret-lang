#lang racket/base

(provide [all-defined-out]
	 [all-from-out scribble/core
		       scribble/base
		       scribble/manual
		       scriblib/footnote
		       "math-utilities.rkt"
		       "literate.rkt"])

(require [only-in scribble/manual code codeblock])
(require scriblib/footnote)
(require scribble/core)
(require scribble/base)
(require "math-utilities.rkt")
(require "literate.rkt")

;;;; <LOCAL ADDITIONS>

(define (show-url u) (hyperlink u u))

(define callout-style
  (make-style "Note" null))

(define (callout . t) 
  (list (para (bold "Note"))
	(nested #:style callout-style t)))

(define world-def-style
  (make-style "WorldDef" null))

(define (world-def . t) 
  (list (para (bold "World Definition"))
	(nested #:style world-def-style t)))

;;;; </LOCAL ADDITIONS>


(define (question . t) (list (bold "Q: ") (apply bold t)))
(define (answer . t) (list (bold "A: ") t))

(define $ math-in)
(define $$ math-disp)
(define math-lite italic)

(define (abbrev t) (smaller t))
(define (kwd t) (italic t))
(define (filename f) (tt f))
(define (email e) (tt e))

(define CPS (abbrev "CPS"))
(define GUI (abbrev "GUI"))
(define HTML (abbrev "HTML"))
(define REPL (abbrev "REPL"))
(define DRY (abbrev "DRY"))
(define NSA (abbrev "NSA"))
(define BST (abbrev "BST"))
(define BBST (abbrev "BBST"))
(define DAG (abbrev "DAG"))
(define GC (abbrev "GC"))
(define MST (abbrev "MST"))
(define ASCII (abbrev "ASCII"))
(define DNA (abbrev "DNA"))

(define pyin tt)
(define (pydisp . t)
  (nested #:style 'inset
    (apply verbatim t)))
(define verb tt)

; (tabular #:sep (hspace 4)

(define (cond-cost-table rows)
   (tabular #:sep (hspace 4)
    (cons (list (para (bold "|Q|"))
		(para (bold "#Q"))
		(para (bold "TotQ"))
		(para (bold "|A|"))
		(para (bold "#A"))
		(para (bold "TotA"))
		(para (bold "Total")))
	  rows)))

(define (glossary-ref name)
  (elemref (string-append "glossary-" name) (list "☛ " (italic name))))

(define (glossary-def name . stuff)
  (list (para (elemtag (string-append "glossary-" name) (list "☞ " (italic name))))
        (apply nested #:style 'inset stuff)))

; Name convention based on
; http://docs.racket-lang.org/scribble/config.html#%28part._extra-style%29

; Neither one has an accompanying style file, since the
; style extensions are folded into my-style-changes.css.
; Hence null instead of
; (list (make-css-addition <css-file-name>))

(define incercise-style
  (make-style "Incercise" null))

(define exercise-style
  (make-style "Exercise" null))

(define (incercise . t) 
  (list (para (bold "Do Now!"))
	(nested #:style incercise-style t)))

(define (exercise . t) 
  (list (para (bold "Exercise"))
	(nested #:style exercise-style t)))

(define (cite . e) (apply italic e))
(define (type . e) (apply tt e))
(define (quotation . e) (apply nested #:style 'inset e))

(define (inverse-taste-test . t)
  (list (bold "Which of these is the same?")
        (nested #:style 'inset
          (apply itemlist (map item t)))))

(define (itemize . t)
  (apply itemlist t))

(define (enumerate . t)
  (apply itemlist #:style 'ordered t))

;; Code originally from Matthew Flatt for judgments

(require scribble/html-properties
         scribble/latex-properties
         scribble/core)

(define spacer (hspace 1))

(define rule-style
  (list (table-cells (list (list (style "inferencetop" '(center)))
                           (list (style "inferencebottom" '(center)))))
        (make-css-addition "rule.css")
        (make-tex-addition "rule.tex")))

(define (margin-quote quote who)
  (margin-note* (list "``" quote "''" "---" who)))

(define (make-primitive-rule top bottom)
  (table (style #f rule-style)
         (list (list (paragraph plain `(,spacer ,top ,spacer)))
               (list (paragraph plain `(,spacer ,bottom ,spacer))))))

(define (rule top newline bottom)
  (unless (string=? newline "\n")
    (error 'rule "~a is not a newline" newline))
  (make-primitive-rule top bottom))
