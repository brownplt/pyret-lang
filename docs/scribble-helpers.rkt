#lang racket

;; various generic scribble helper functions

(require scribble/base
         scribble/core
         scribble/decode
         scribble/basic
         scribble/html-properties
         ;"checker.rkt"
         )

(provide append/itemization
         remdups/itemization
         fake-item
         list->itemization
         interleave-parbreaks/select
         interleave-parbreaks/all
         create-itemlist
         head-title-no-content
         ;(all-defined-out)  ;--> clashes with provide/contract
         )

(provide/contract [itemlist/splicing
                   (->* () 
                        (#:style (or/c style? string? symbol? #f)) 
                        #:rest list?
                        itemization?)]
                 )

;;;;;; page headings ;;;;;;;;;;;;;

;; used to generate title in head without scribble-generated title content
(define (head-title-no-content text)
  (title #:style 'hidden text))

;;;;; ITEMIZATIONS ;;;;;;;;;;;;

(define (create-itemlist #:style [style #f] contents)
  (apply itemlist/splicing #:style style contents))

;; append contents of two scribble itemizations, keeping style of the second
(define (append/itemization items1 items2)
  (cond [(empty? items2) items1]
        [(empty? items1) items2]
        [else
         (make-itemization (itemization-style items2)
                           (append (itemization-blockss items1) (itemization-blockss items2)))]))

;; remove duplicates in an itemlist
(define (remdups/itemization itemz)
  (let ([items (apply append (itemization-blockss itemz))])
    (apply itemlist (remove-duplicates items equal?))))

;; fake-item: (listof any) -> (listof any)
;; We try to make itemlist more pleasant to work with.  Itemlist/splicing automatically
;; wraps items around every argument, so there's no need to call item explicitly.
;; We provide a fake definition for fake-item that just returns the identity.
(define (fake-item . args)
  args)

;; itemlist/splicing is like itemlist, but also cooperates with the
;; splice form to absorb arguments.  
(define (itemlist/splicing #:style [style #f] . items)
  (define spliced-items
    (reverse
     (let loop ([items items]
                [acc '()])
       (foldl (lambda (i acc)
                (cond
                  [(splice? i)
                   (loop (splice-run i) acc)]
                  [(item? i)
                   (cons i acc)]
                  [else
                   (cons (item i) acc)]))
              acc
              items))))
  (apply itemlist spliced-items #:style style))

;; using this to override scribble's default to blockquotes in styles
;; similar to function with same name in form-elements.rkt, but didn't
;; have imports set up to reuse that defn here (and this one does not
;; include the css-js-additions, which aren't needed here anyway)
(define (make-div-style name)
  (make-style name (list (make-alt-tag "div"))))

;; used to distinguish simple values from lists or structs
;; not same as mzscheme atom? which just checks non-pairness
;; cheating a bit here -- using this in place where we really
;; want to check whether we have scribble content (of some form),
;; but thing string? is a sufficient approximation
(define (atomic? v)
  (or (string? v) (number? v) (symbol? v)))

;; converts a racket nested list (of strings) into a scribble
;; itemization.  Classnames is a list of strings,
;; one for each nesting level in the input list.
;; these strings get used as classnames in the generated
;; itemization
(define (list->itemization eltlist classnames)
  (let ([stylename (if (empty? classnames) #f (first classnames))]
        [remclassnames (if (empty? classnames) empty (rest classnames))])
    (apply itemlist/splicing #:style (make-div-style stylename)
           (map (lambda (elt)
                  (if (not (list? elt)) 
                      (if (atomic? elt) (elem elt) elt)
                      (nested #:style (make-div-style (format "~aItemContents" (or stylename "")))
                              (list "\n" "\n" 
                                    (if (atomic? (first elt)) (elem (first elt)) (first elt))
                                    "\n" "\n"
                                    (list->itemization (second elt) remclassnames)))))
                eltlist))))

;(list->itemization '("hi" ("mom" ("sis"))) (list "a" "b"))
  

;;;;;;;;;; MANAGING SINTRAPARAS ;;;;;;;;;;;;;;;;;;;

;; determine which elements need manual parbreaks to prevent surrounding SIntraparas
;; currently suppress around itemizations and student/teacher blocks
(define (suppress-intrapara-around? content)
  (or (itemization? content)
      (and (nested-flow? content) 
           (nested-flow-style content)
           (member (style-name (nested-flow-style content)) 
                   (list "student" "teacher" "activity")))
      ))

;; Avoid Sintraparas from being introduced by adding manual parbreaks between
;;   select items in a list.  Good for settings in which some items in list are 
;;   part of the same paragraph and others (like itemlists) introduce breaks
(define (interleave-parbreaks/select contentlist)
  (cond [(empty? contentlist) (list "\n" "\n")]
        [(cons? contentlist) 
         (cond [(suppress-intrapara-around? (first contentlist))
                (append (list "\n" "\n" (first contentlist) "\n" "\n")
                        (interleave-parbreaks/select (rest contentlist)))]
               [else (cons (first contentlist) (interleave-parbreaks/select (rest contentlist)))])]))

;; Avoid Sintraparas from being introduced by adding manual parbreaks between
;;   every pair of items in a list.  Good for settings in which items in list are
;;   never intended to be part of the same paragraph (like lesson segments)
(define (interleave-parbreaks/all contentlist)
  (cond [(empty? contentlist) (list "\n" "\n")]
        [(cons? contentlist) 
         (cons "\n" (cons "\n" (cons (first contentlist) 
                                     (interleave-parbreaks/all (rest contentlist)))))]))
  
