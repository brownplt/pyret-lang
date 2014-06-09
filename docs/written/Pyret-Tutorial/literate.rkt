#lang racket/base
(require scribble/base
         scribble/core
         racket/list)

(provide chunk-ref chunk-def)

;; Assumption: chunks are used only for Pyret

(define (chunk-ref name)
  (elemref name (list "<" (italic name) ">")))

(define (chunk-def name . stuff)
  (list (para (elemtag name (list "<" (italic name) "> ::=")))
        (apply verbatim2 #:indent 4 stuff)))

;; Code below from Matthew Flatt

(define (verbatim2 #:indent [i 0] s . more)
  (define lines 
    ;; Break input into a list of lists, where each inner
    ;; list is a single line. Break lines on "\n" in the
    ;; input strings, while non-string content is treated
    ;; as an element within a line.
    (let loop ([l (cons s more)] [strs null])
      (cond
       [(null? l) (if (null? strs)
                      null
                      (map
                       list
                       (regexp-split
                        #rx"\n"
                        (apply string-append (reverse strs)))))]
       [(string? (car l))
        (loop (cdr l) (cons (car l) strs))]
       [else
        (define post-lines (loop (cdr l) null))
        (define pre-lines (loop null strs))
        (define-values (post-line rest-lines)
          (if (null? post-lines)
              (values null null)
              (values (car post-lines) (cdr post-lines))))
        (define-values (first-lines pre-line)
          (if (null? pre-lines)
              (values null null)
              (values (drop-right pre-lines 1)
                      (last pre-lines))))
        (append first-lines
                (list (append pre-line (list (car l)) post-line))
                rest-lines)])))
  (define (str->elts str)
    ;; Convert a single string in a line to typewriter font,
    ;; and also convert multiple adjacent spaces to `hspace` so
    ;; that the space is preserved exactly:
    (let ([spaces (regexp-match-positions #rx"(?:^| ) +" str)])
      (if spaces
        (list* (substring str 0 (caar spaces))
               (hspace (- (cdar spaces) (caar spaces)))
               (str->elts (substring str (cdar spaces))))
        (list (make-element 'tt (list str))))))
  (define (strs->elts line)
    ;; Convert strings in the line:
    (apply append (map (lambda (e) 
                         (if (string? e) 
                             (str->elts e) 
                             (list e)))
                       line)))
  (define indent
    ;; Add indentation to a line:
    (if (zero? i)
      values
      (let ([hs (hspace i)]) (lambda (line) (cons hs line)))))
  (define (make-nonempty l)
    ;; If a line has no content, then add a single space:
    (if (let loop ([l l])
          (cond
           [(null? l) #t]
           [(equal? "" l) #t]
           [(list? l) (andmap loop l)]
           [(element? l) (loop (element-content l))]
           [(multiarg-element? l) (loop (multiarg-element-contents l))]
           [else #f]))
        (list l (hspace 1))
        l))
  (define (make-line line)
    ;; Convert a list of line elements --- a mixture of strings
    ;; and non-strings --- to a paragraph for the line:
    (let* ([line (indent (strs->elts line))])
      (list (make-paragraph omitable-style (make-nonempty line)))))
  (make-table plain (map make-line lines)))

(define omitable-style (make-style 'omitable null))

