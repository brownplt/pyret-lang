#lang at-exp racket/base

;; NOTE(joe): We've hacked the below, which started as dyoo's Python
;; tokenizer, to be specific to Pyret.  Danny's commentary follows

;; This is a translation of the tokenizer.py library from Python.
;;
;; Translation by Danny Yoo (dyoo@hashcollision.org)
;;
;; See:
;;
;;     http://hg.python.org/cpython/file/2.7/Lib/tokenize.py
;;
;; for the original Python sources.

;;
;; Comments and issues while translating the code:
;;
;; Racket considers characters different from length-1 strings, as a separate
;; character type.
;;
;; Regexps in Racket are different than in Python in a few particulars.
;; In character sets, particularly, control characters are significant
;; in Racket's regexp engine.
;;
;; Most uses of raw strings can be substituted with uses of the @-reader.
;; 
;; The 'in' operator in Python is extra-flexible.  It's hard to tell sometimes
;; what is being intended.
;;
;; Code that uses 'while' loops communicate values from one part of the code
;; to the other through mutation, often in wildly distant part of the code.
;;
;; variable declaration in Python is nonexistant, making it difficult to see
;; if some name is meant to be globally accessible within the tokenizer loop,
;; or is only for temporary use.  I've tried to determine where temporaries
;; are intended.


(require racket/generator
         racket/list
         racket/sequence
         racket/string
         racket/set
         data/gvector
         (for-syntax racket/base)
         (planet dyoo/while-loop))

(provide generate-tokens
         (struct-out exn:fail:token)
         (struct-out exn:fail:indentation))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper macros:
;;
;; We define a few macros here to help port the code over.
;;
;; The more significant one, the while loop macro, I've put in a
;; separate file "while-loop.rkt".

;; Here are a few miscellaneous macros:
;;

;; set! multiple identifiers at once.
(define-syntax (set!* stx)
  (syntax-case stx ()
    [(= id1 id-rest ... val) 
     (andmap identifier? (syntax->list #'(id1 id-rest ...)))
     (syntax/loc stx
       (let ([v val])
         (set! id1 v)
         (set! id-rest v) ...))]))

;; Since there's quite a bit of mutative variable
;; incrementing and decrementing, we can provide a small syntax for this.
(define-syntax (++ stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (syntax/loc stx
       (set! id (add1 id)))]))

(define-syntax (-- stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (syntax/loc stx
       (set! id (sub1 id)))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; Token errors will be exceptions:
(define-struct (exn:fail:token exn:fail) (loc))
;; As will indentation errors:
(define-struct (exn:fail:indentation exn:fail) (loc))


;; string-right-ref: string number -> char
;; Referencing characters from the right side.
(define (string-right-ref str n)
  (string-ref str (- (string-length str) n)))


;; slice-end: string number -> string
;; Slice from the end of a string.
(define (slice-end str n)
  (substring str
             (max (- (string-length str) n)
                  0)))

;; slice-front: string number -> string
;; Slice from the beginning of a string.
(define (slice-front str n)
  (substring str 0 (min n (string-length str))))


;; rstrip-newlines: string -> string
;; Trim off the newline characters off a string.
(define (rstrip-newlines s)
  (regexp-replace #px"[\r\n]+$" s ""))

;; my-gvector-pop!: (gvectorof X) -> X
;; Remove the last element of the gvector and return it.
(define (my-gvector-pop! gv)
  (define last-index (sub1 (gvector-count gv)))
  (define val (gvector-ref gv last-index))
  (gvector-remove! gv last-index)
  val)

;; gvector-last: (gvectorof X) -> X
(define (my-gvector-last gv)
  (define last-index (sub1 (gvector-count gv)))
  (gvector-ref gv last-index))


;; gvector-member: X (gvectorof X) -> boolean
(define (my-gvector-member x gv)
  (let/ec return
    (for ([elt (in-gvector gv)])
      (when (equal? x elt)
        (return #t)))
    (return #f)))


;; What are our token types?
;; In the original Python sources, they were integers
;; Here, we'll use symbols.
(define NAME 'NAME)
(define NUMBER 'NUMBER)
(define STRING 'STRING)
(define OP 'OP)
(define COMMENT 'COMMENT)
(define NL 'NL)
(define NEWLINE 'NEWLINE)
(define DEDENT 'DEDENT)
(define INDENT 'INDENT)
(define BACKSLASH 'BACKSLASH)
(define ERRORTOKEN 'ERRORTOKEN)
(define ENDMARKER 'ENDMARKER)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regular expression stuff.

(define (group . choices)
  (string-append "(" (string-join choices "|") ")"))

(define (any . choices)
  (string-append (apply group choices) "*"))

(define (maybe . choices)
  (string-append (apply group choices) "?"))



;; We'll use @-reader support to get equivalent functionality to
;; Python's raw strings.
;; See: http://jarnaldich.me/2011/08/07/raw-strings-in-racket.html
;; for a discussion.  Raw strings let us do the proper string escaping.
(define r string-append)



(define Whitespace "[ \f\t]*")
(define Comment "#[^\r\n]*")
(define Ignore (string-append Whitespace 
                              (any (string-append "\\\r?\n"  Whitespace))
                              (maybe Comment)))
(define Name @r{[a-zA-Z_][a-zA-Z0-9$_\-]*})

(define Hexnumber @r{0[xX][\da-fA-F]+[lL]?})
(define Octnumber @r{(0[oO][0-7]+)|(0[0-7]*)[lL]?})
(define Binnumber @r{0[bB][01]+[lL]?})
(define Decnumber @r{[1-9]\d*[lL]?})
(define Intnumber (group Hexnumber Binnumber Octnumber Decnumber))
(define Exponent  @r{[eE][-+]?\d+})
(define Pointfloat (string-append
                    (group @r{\d+\.\d+} @r{\.\d+}) 
                    (maybe Exponent)))
(define Expfloat (string-append @r{\d+} Exponent))
(define Floatnumber (group Pointfloat Expfloat))
(define Imagnumber (group @r{\d+[jJ]}
                          (string-append Floatnumber @r{[jJ]})))

(define Number (group Imagnumber Floatnumber Intnumber))


;; Tail end of ' string.
(define Single @r{[^'\\]*(?:\\.[^'\\]*)*'})
;; Tail end of " string.
(define Double @r{[^"\\]*(?:\\.[^"\\]*)*"})
;; Tail end of ''' string.
(define Single3 @r{[^'\\]*(?:(?:\\.|'(?!''))[^'\\]*)*'''})
;; Tail end of """ string.
(define Double3 @r{[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*"""})
(define Triple (group @r{[uU]?[rR]?'''} @r{[uU]?[rR]?"""}))
;; Single-line ' or " string.
;;
;; dyoo: Note that there's a little trickiness since the newline
;; has to really be in the string (and not its raw-string equivalent).
;; That's why the translation unescapes the newlines in the regexp group.
(define String (group @r{[uU]?[rR]?'[^@r["\n"]'\\]*(?:\\.[^@r["\n"]'\\]*)*'}
                      @r{[uU]?[rR]?"[^@r["\n"]"\\]*(?:\\.[^@r["\n"]"\\]*)*"}))


;; Because of leftmost-then-longest match semantics, be sure to put the
;; longest operators first (e.g., if = came before ==, == would get
;; recognized as two instances of =).
(define Operator (group @r{\*\*=?} @r{>>=?} @r{<<=?} @r{<>} @r{!=}
                        @r{\+\+} @r{--}
                        @r{//=?}
                        @r{:=}
                        @r{->}
                        @r{=>}
                        @r{[+\-*/%&|^=<>]=?}
                        @r{~}))

(define Bracket "[][(){}]")
(define Special (group @r{\\} "\r?\n" @r{::} @r|{[:;.,`@]}|))
(define Funny (group Operator Bracket Special))

(define PlainToken (group Number Funny String Name))
(define Token (string-append Ignore PlainToken))

;;  First (or only) line of ' or " string.
(define ContStr (group (string-append @r{[uU]?[rR]?'[^@r["\n"]'\\]*(?:\\.[^@r["\n"]'\\]*)*}
                                      (group "'" "\\\r?\n"))
                       (string-append @r{[uU]?[rR]?"[^@r["\n"]"\\]*(?:\\.[^@r["\n"]"\\]*)*}
                                      (group @r{"}  "\\\r?\n"))))

(define PseudoExtras (group "\\\r?\n" Comment Triple))

(define PseudoToken 
  (string-append Whitespace (group PseudoExtras Number Funny ContStr Name)))


(define-values (tokenprog pseudoprog single3prog double3prog)
  (apply values
         (map (lambda (x)
                ;; Slight change: explicitly adding the leading anchor
                ;; to force matches at the beginning
                (pregexp (string-append "^" x)))
              (list Token PseudoToken Single3 Double3))))

(define endprogs 
  (hash @r{'} (pregexp (string-append "^" Single))
        @r{"} (pregexp (string-append "^" Double))
        @r{'''} single3prog
        @r{"""} double3prog
        @r{r'''} single3prog
        @r{r"""} double3prog
        @r{u'''} single3prog
        @r{u"""} double3prog
        @r{ur'''} single3prog
        @r{ur"""} double3prog
        @r{R'''} single3prog
        @r{R"""} double3prog
        @r{U'''} single3prog
        @r{U"""} double3prog
        @r{uR'''} single3prog
        @r{uR"""} double3prog
        @r{Ur'''} single3prog
        @r{Ur"""} double3prog
        @r{UR'''} single3prog
        @r{UR"""} double3prog
        @r{b'''} single3prog
        @r{b"""} double3prog
        @r{br'''} single3prog
        @r{br"""} double3prog
        @r{B'''} single3prog
        @r{B"""} double3prog
        @r{bR'''} single3prog
        @r{bR"""} double3prog
        @r{Br'''} single3prog
        @r{Br"""} double3prog
        @r{BR'''} single3prog
        @r{BR"""} double3prog
        @r{r} #f
        @r{R} #f
        @r{u} #f
        @r{U} #f
        @r{b} #f
        @r{B} #f))


(define triple-quoted
  (set "'''" @r{"""}
       "r'''" @r{r"""} "R'''" @r{R"""}
       "u'''" @r{u"""} "U'''" @r{U"""}
       "ur'''" @r{ur"""} "Ur'''" @r{Ur"""}
       "uR'''" @r{uR"""} "UR'''" @r{UR"""}
       "b'''" @r{b"""} "B'''" @r{B"""}
       "br'''" @r{br"""} "Br'''" @r{Br"""}
       "bR'''" @r{bR"""} "BR'''" @r{BR"""}))

(define single-quoted
  (set "'" @r{"}
       "r'" @r{r"} "R'" @r{R"}
       "u'" @r{u"} "U'" @r{U"}
       "ur'" @r{ur"} "Ur'" @r{Ur"}
       "uR'" @r{uR"} "UR'" @r{UR"}
       "b'" @r{b"} "B'" @r{B"}
       "br'" @r{br"} "Br'" @r{Br"}
       "bR'" @r{bR"} "BR'" @r{BR"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define tabsize 8)


;; Notes during the translation of generate-tokens:
;;
;; One of the nasty things is to see how much state's involved in
;; tokenization.  In the original sources, it's a bit hard to tell 
;; what all the lexer's state is, since variables are function-scoped.
;;
;;


;; in-lines/preserve-newlines: input-port -> sequence
;; Like in-lines, but maintains the newline character, as in Python.
(define (in-lines/preserve-newlines ip)
  (in-generator
   (let loop ()
     (define a-match (regexp-match #px"^([^\r\n]*)(\r\n|\n|\r|$)"
                                   ip))
     (when (and a-match (not (bytes=? (first a-match) #"")))
       (yield (bytes->string/utf-8 (first a-match)))
       (loop)))))



;; generate-tokens: input-port -> sequence
(define (generate-tokens ip)
  #|
        The generate_tokens sequence requires one argument, ip, which
        must be an input port.
    
        The returned sequence produces 5-tuples with these members: the token type; the
        token string; a 2-tuple (srow, scol) of ints specifying the row and
        column where the token begins in the source; a 2-tuple (erow, ecol) of
        ints specifying the row and column where the token ends in the source;
        and the line on which the token was found. The line passed is the
        logical line; continuation lines are included.
  |#
  
  (in-generator
   
   ;; The idiom for reading from a sequence in Racket doesn't use
   ;; "it's easier to ask forgiveness than permission".
   (define-values (read-line-not-exhausted? read-line) 
     (sequence-generate (in-lines/preserve-newlines ip)))
   
   (define lnum 0)
   (define strstart (list 0 0))
   (define start 0)
   (define end 0)
   (define pos #f)
   (define max #f)
   (define column 0)
   (define parenlev 0)
   (define continued? #f)
   ;; Slight deviation: rather than represent namechars as a string, use a set.
   (define namechars (apply set (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_")))
   ;; Slight deviation: rather than represent numchars as a string, use a set.
   (define numchars (apply set (string->list "0123456789")))
   (define contstr "")
   (define needcont? #f)
   (define contline #f)
   (define indents (gvector 0))
   (define line "")
   (define endprog #px"")
   (define-values (pos-line pos-column pos-offset) (port-next-location ip))
   (let ([yield-list (lambda (type text start end reststr)
      (define len (string-length text))
      (define col (second start))
      (define posn (list (+ pos-offset col) len))
      (yield (list type text start end reststr posn)))])
     (while #t                          ;; loop over lines in stream
       (set!-values (pos-line pos-column pos-offset) (port-next-location ip))
       (if (read-line-not-exhausted?) 
           (set! line (read-line))
           (set! line ""))
       (++ lnum)
       (set! pos 0)
       (set! max (string-length line))
       
       (cond 
         [(> (string-length contstr) 0)                  ;; continued string
          (when (string=? line "")
            (raise (exn:fail:token "EOF in multi-line string")
                   (current-continuation-marks)
                   strstart))
          ;; Note: endprog must anchor the match with "^" or else
          ;; this does not have equivalent behavior to Python.
          (define endmatch (regexp-match-positions endprog line))
          (cond
            [endmatch
             (set!* pos end 
                    (cdr (first endmatch)))
             (yield-list STRING
                         (string-append contstr (substring line 0 end))
                         strstart
                         (list lnum end)
                         (string-append contline  line))
             (set! contstr "")
             (set! needcont? #f)
             (set! contline #f)]
            
            [(and needcont?
                  (not (string=? (slice-end line 2) "\\\n"))
                  (not (string=? (slice-end line 3) "\\\r\n")))
             (yield-list ERRORTOKEN
                         (string-append contstr line)
                         strstart
                         (list lnum (string-length line))
                         contline)
             (set! contstr "")
             (set! contline #f)
             (continue)]
            
            [else
             (set! contstr (string-append contstr line))
             (set! contline (string-append contline line))
             (continue)])]
         
         [(and (= parenlev 0)
               (not continued?))                    ;; new statement
          (when (string=? line "")
            (break))
          (set! column 0)
          (while (< pos max)                        ;; measure leading whitespace
            (cond
              [(char=? (string-ref line pos) #\space)
               (++ column)]
              [(char=? (string-ref line pos) #\tab)
               (set! column (* tabsize (add1 (quotient column tabsize))))]
              [(char=? (string-ref line pos) #\page)
               (set! column 0)]
              [else
               (break)])
            (++ pos))
          (when (= pos max)
            (break))

          (when (member (string-ref line pos) (list #\# #\return #\newline))
            (cond
              [(char=? (string-ref line pos) #\#)
               (define comment-token (rstrip-newlines (substring line pos)))
               (define nl-pos (+ pos (string-length comment-token)))
               (yield-list COMMENT
                           comment-token
                           (list lnum pos)
                           (list lnum (+ pos (string-length comment-token)))
                           line)
               (yield-list NL
                           (substring line nl-pos)
                           (list lnum nl-pos)
                           (list lnum (string-length line))
                           line)]
              [else
               (yield-list (if (char=? (string-ref line pos) #\#) COMMENT NL)
                           (string (string-ref line pos))
                           (list lnum pos)
                           (list lnum (string-length line))
                           line)])
            (continue))
          
          (when (> column (my-gvector-last indents))  ;; count indents or dedents
            (gvector-add! indents column)
            (yield-list INDENT
                        (substring line 0 pos)
                        (list lnum 0)
                        (list lnum pos)
                        line))
          (while (< column (my-gvector-last indents))
            ;; NOTE(joe): Removed for Pyret; should be used for
            ;; an implementation that wants to check indentations
            #;(unless (my-gvector-member column indents)
              (raise (exn:fail:indentation "unindent does not match any outer indentation level"
                                           (current-continuation-marks)
                                           (list "<tokenize>" lnum pos line))))
            (my-gvector-pop! indents)
            (yield-list DEDENT 
                        ""
                        (list lnum pos)
                        (list lnum pos)
                        line))]
         
         [else                                     ;; continued statement
          (if (= (string-length line) 0)
              (raise (exn:fail:token "EOF in multi-line statement" 
                                     (current-continuation-marks)
                                     (list lnum 0)))
              (set! continued? #f))])


       (while (< pos max)
         (define pseudomatch (regexp-match-positions pseudoprog line pos))
         (cond [pseudomatch                                  ;; scan for tokens
                (set! start (car (second pseudomatch)))
                (set! end (cdr (second pseudomatch)))
                (define spos (list lnum start))
                (define epos (list lnum end))
                (set! pos end)
                (define token (substring line start end))
                (define initial (string-ref line start))
                (cond
                  [(or (set-member? numchars initial)
                       (and (char=? initial #\.) (not (string=? token "."))))      ;; ordinary number
                   (yield-list NUMBER token spos epos line)]
                  [(or (char=? initial #\return) (char=? initial #\newline))
                   (yield-list (if (> parenlev 0) NL NEWLINE)
                               token spos epos line)]
                  [(char=? initial #\#)
                   (when (regexp-match #px"\n$" token)
                     (error 'generate-tokens "Assertion error: token ends with newline"))
                   (yield-list COMMENT token spos epos line)]
                  [(set-member? triple-quoted token)
                   (set! endprog (hash-ref endprogs token))
                   (define endmatch (regexp-match-positions endprog line pos))
                   (cond
                     [endmatch
                      (set! pos (cdr (first endmatch)))
                      (set! token (substring line start pos))
                      (yield-list STRING token spos (list lnum pos) line)]
                     [else
                      (set! strstart (list lnum start))              ;; multiple lines
                      (set! contstr (substring line start))
                      (set! contline line)
                      (break)])]
                  [(or (set-member? single-quoted (string initial))
                       (set-member? single-quoted (slice-front token 2))
                       (set-member? single-quoted (slice-front token 3)))
                   (cond
                     [(char=? (string-right-ref token 1) #\newline) ;; continued string
                      (set! strstart (list lnum start))
                      (set! endprog (or (hash-ref endprogs (string initial) #f)
                                        (hash-ref endprogs (string (string-ref token 1)) #f)
                                        (hash-ref endprogs (string (string-ref token 2)) #f)))
                      (set! contstr (substring line start))
                      (set! needcont? #t)
                      (set! contline line)
                      (break)]
                     [else                                          ;; ordinary string
                      (yield-list STRING token spos epos line)])]
                  [(set-member? namechars initial)                  ;; ordinary name
                   (yield-list NAME token spos epos line)]               
                  ;; NOTE(joe): altered for Pyret, which uses \\ for lambda
                  [(char=? initial #\\)                             ;; continued stmt
                   (yield-list BACKSLASH token spos epos line)]
                  [else
                   (cond [(or (char=? initial #\() (char=? initial #\[) (char=? initial #\{))
                          (++ parenlev)]
                         [(or (char=? initial #\)) (char=? initial #\]) (char=? initial #\}))
                          (-- parenlev)])
                   (yield-list OP token spos epos line)])]
               [else
                (yield-list ERRORTOKEN
                            (string-ref line pos)
                            (list lnum pos)
                            (list lnum (+ pos 1))
                            line)
                (++ pos)])))
     
     (for ([indent (sequence-tail indents 1)]) ;; pop remaining indent levels
          (yield-list DEDENT
                      ""
                      (list lnum 0)
                      (list lnum 0)
                      ""))
     (yield-list ENDMARKER
                 ""
                 (list lnum 0)
                 (list lnum 0)
                 ""))))



