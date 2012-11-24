#lang racket/base

;; A small test using code from Joe Politz

(require "pyret-tokenizer.rkt"
         rackunit
         racket/sequence
         racket/generator
         racket/match)

(define (dump-tokens s)
  (sequence->list
   (in-generator
    (let/ec break
      (for ([token (generate-tokens (open-input-string s))])
           (match token
             [(list 'ENDMARKER rest ...)
              (break)]
             [(list type text start end rest)
              (yield (list type text start end))]))))))


(define sample-text #<<EOF

deffun cons(elt, lst)
  {
    empty: lambda() false,
    first: lambda() elt,
    rest: lambda() lst
  }
in

defvar empty = { empty: lambda() true } in

deffun foldr(f, init, lst)
  if lst.empty()
  then init
  else f(foldr(f, init, lst.rest()), lst.first())
in

foldr(lambda(x, y) { +(x, y) }, 1, cons(5, cons(6, empty)))



EOF
)

(check-equal? (dump-tokens sample-text)
              '((NL "\n" (1 0) (1 1)) (NAME "deffun" (2 0) (2 6)) (NAME "cons" (2 7) (2 11)) (OP "(" (2 11) (2 12)) (NAME "elt" (2 12) (2 15)) (OP "," (2 15) (2 16)) (NAME "lst" (2 17) (2 20)) (OP ")" (2 20) (2 21)) (NEWLINE "\n" (2 21) (2 22)) (INDENT "  " (3 0) (3 2)) (OP "{" (3 2) (3 3)) (NL "\n" (3 3) (3 4)) (NAME "empty" (4 4) (4 9)) (OP ":" (4 9) (4 10)) (NAME "lambda" (4 11) (4 17)) (OP "(" (4 17) (4 18)) (OP ")" (4 18) (4 19)) (NAME "false" (4 20) (4 25)) (OP "," (4 25) (4 26)) (NL "\n" (4 26) (4 27)) (NAME "first" (5 4) (5 9)) (OP ":" (5 9) (5 10)) (NAME "lambda" (5 11) (5 17)) (OP "(" (5 17) (5 18)) (OP ")" (5 18) (5 19)) (NAME "elt" (5 20) (5 23)) (OP "," (5 23) (5 24)) (NL "\n" (5 24) (5 25)) (NAME "rest" (6 4) (6 8)) (OP ":" (6 8) (6 9)) (NAME "lambda" (6 10) (6 16)) (OP "(" (6 16) (6 17)) (OP ")" (6 17) (6 18)) (NAME "lst" (6 19) (6 22)) (NL "\n" (6 22) (6 23)) (OP "}" (7 2) (7 3)) (NEWLINE "\n" (7 3) (7 4)) (DEDENT "" (8 0) (8 0)) (NAME "in" (8 0) (8 2)) (NEWLINE "\n" (8 2) (8 3)) (NL "\n" (9 0) (9 1)) (NAME "defvar" (10 0) (10 6)) (NAME "empty" (10 7) (10 12)) (OP "=" (10 13) (10 14)) (OP "{" (10 15) (10 16)) (NAME "empty" (10 17) (10 22)) (OP ":" (10 22) (10 23)) (NAME "lambda" (10 24) (10 30)) (OP "(" (10 30) (10 31)) (OP ")" (10 31) (10 32)) (NAME "true" (10 33) (10 37)) (OP "}" (10 38) (10 39)) (NAME "in" (10 40) (10 42)) (NEWLINE "\n" (10 42) (10 43)) (NL "\n" (11 0) (11 1)) (NAME "deffun" (12 0) (12 6)) (NAME "foldr" (12 7) (12 12)) (OP "(" (12 12) (12 13)) (NAME "f" (12 13) (12 14)) (OP "," (12 14) (12 15)) (NAME "init" (12 16) (12 20)) (OP "," (12 20) (12 21)) (NAME "lst" (12 22) (12 25)) (OP ")" (12 25) (12 26)) (NEWLINE "\n" (12 26) (12 27)) (INDENT "  " (13 0) (13 2)) (NAME "if" (13 2) (13 4)) (NAME "lst" (13 5) (13 8)) (OP "." (13 8) (13 9)) (NAME "empty" (13 9) (13 14)) (OP "(" (13 14) (13 15)) (OP ")" (13 15) (13 16)) (NEWLINE "\n" (13 16) (13 17)) (NAME "then" (14 2) (14 6)) (NAME "init" (14 7) (14 11)) (NEWLINE "\n" (14 11) (14 12)) (NAME "else" (15 2) (15 6)) (NAME "f" (15 7) (15 8)) (OP "(" (15 8) (15 9)) (NAME "foldr" (15 9) (15 14)) (OP "(" (15 14) (15 15)) (NAME "f" (15 15) (15 16)) (OP "," (15 16) (15 17)) (NAME "init" (15 18) (15 22)) (OP "," (15 22) (15 23)) (NAME "lst" (15 24) (15 27)) (OP "." (15 27) (15 28)) (NAME "rest" (15 28) (15 32)) (OP "(" (15 32) (15 33)) (OP ")" (15 33) (15 34)) (OP ")" (15 34) (15 35)) (OP "," (15 35) (15 36)) (NAME "lst" (15 37) (15 40)) (OP "." (15 40) (15 41)) (NAME "first" (15 41) (15 46)) (OP "(" (15 46) (15 47)) (OP ")" (15 47) (15 48)) (OP ")" (15 48) (15 49)) (NEWLINE "\n" (15 49) (15 50)) (DEDENT "" (16 0) (16 0)) (NAME "in" (16 0) (16 2)) (NEWLINE "\n" (16 2) (16 3)) (NL "\n" (17 0) (17 1)) (NAME "foldr" (18 0) (18 5)) (OP "(" (18 5) (18 6)) (NAME "lambda" (18 6) (18 12)) (OP "(" (18 12) (18 13)) (NAME "x" (18 13) (18 14)) (OP "," (18 14) (18 15)) (NAME "y" (18 16) (18 17)) (OP ")" (18 17) (18 18)) (OP "{" (18 19) (18 20)) (OP "+" (18 21) (18 22)) (OP "(" (18 22) (18 23)) (NAME "x" (18 23) (18 24)) (OP "," (18 24) (18 25)) (NAME "y" (18 26) (18 27)) (OP ")" (18 27) (18 28)) (OP "}" (18 29) (18 30)) (OP "," (18 30) (18 31)) (NUMBER "1" (18 32) (18 33)) (OP "," (18 33) (18 34)) (NAME "cons" (18 35) (18 39)) (OP "(" (18 39) (18 40)) (NUMBER "5" (18 40) (18 41)) (OP "," (18 41) (18 42)) (NAME "cons" (18 43) (18 47)) (OP "(" (18 47) (18 48)) (NUMBER "6" (18 48) (18 49)) (OP "," (18 49) (18 50)) (NAME "empty" (18 51) (18 56)) (OP ")" (18 56) (18 57)) (OP ")" (18 57) (18 58)) (OP ")" (18 58) (18 59)) (NEWLINE "\n" (18 59) (18 60)) (NL "\n" (19 0) (19 1)) (NL "\n" (20 0) (20 1))))
