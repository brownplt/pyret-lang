#lang at-exp racket/base
(require "pyret-tokenizer.rkt"
         rackunit
         racket/match
         racket/sequence
         racket/generator)

;; Most of this is translated from:
;; http://hg.python.org/cpython/file/2.7/Lib/test/test_tokenize.py

;; This includes some of test cases extracted from Python's
;; test/test_tokenize.py file, though it doesn't currently
;; include the round-tripping tests that the original source included.
;;
;; One other difference is that we're using Racket rackunit rather
;; than Python's doctest.  It would be nice if someone ported over a
;; doctest equivalent.


;; The definition for f is just for raw strings (using at-exp).
(define r (lambda (x) x))

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


(check-equal? (dump-tokens "1 + 1")
              '((NUMBER "1" (1 0) (1 1))
                (OP "+" (1 2) (1 3))
                (NUMBER "1" (1 4) (1 5))))
                        

(check-equal? (dump-tokens (string-append "if False:\n"
                                          "    # NL\n"
                                          "    True = False # NEWLINE\n"))
              '((NAME       "if"          (1 0) (1 2))
                (NAME       "False"       (1 3) (1 8))
                (OP         ":"           (1 8) (1 9))
                (NEWLINE    "\n"         (1 9) (1 10))
                (COMMENT    "# NL"        (2 4) (2 8))
                (NL         "\n"         (2 8) (2 9))
                (INDENT     "    "        (3 0) (3 4))
                (NAME       "True"        (3 4) (3 8))
                (OP         "="           (3 9) (3 10))
                (NAME       "False"       (3 11) (3 16))
                (COMMENT    "# NEWLINE"   (3 17) (3 26))
                (NEWLINE    "\n"         (3 26) (3 27))
                (DEDENT     ""            (4 0) (4 0))))



(check-exn exn:fail:indentation?
           (lambda ()
             (dump-tokens #<<EOF
def k(x):
    x += 2
  x += 5
EOF
)))



;; Ordinary integers and binary operators
(check-equal? (dump-tokens "0xff <= 255")
              '((NUMBER     "0xff"        (1 0) (1 4))
                (OP         "<="          (1 5) (1 7))
                (NUMBER     "255"         (1 8) (1 11))))
(check-equal? (dump-tokens "0b10 <= 255")
              '((NUMBER     "0b10"        (1 0) (1 4))
                (OP         "<="          (1 5) (1 7))
                (NUMBER     "255"         (1 8) (1 11))))
(check-equal? (dump-tokens "0o123 <= 0123")
              '((NUMBER     "0o123"       (1 0) (1 5))
                (OP         "<="          (1 6) (1 8))
                (NUMBER     "0123"        (1 9) (1 13))))
(check-equal? (dump-tokens "01234567 > ~0x15")
              '((    NUMBER  "01234567"    (1 0) (1 8))
                (OP         ">"           (1 9) (1 10))
                (OP         "~"           (1 11) (1 12))
                (NUMBER     "0x15"        (1 12) (1 16))))
(check-equal? (dump-tokens "2134568 != 01231515")
              '((NUMBER     "2134568"     (1 0) (1 7))
                (OP         "!="          (1 8) (1 10))
                (NUMBER     "01231515"    (1 11) (1 19))))
(check-equal? (dump-tokens "(-124561-1) & 0200000000")
              '((OP         "("           (1 0) (1 1))
                (OP         "-"           (1 1) (1 2))
                (NUMBER     "124561"      (1 2) (1 8))
                (OP         "-"           (1 8) (1 9))
                (NUMBER     "1"           (1 9) (1 10))
                (OP         ")"           (1 10) (1 11))
                (OP         "&"           (1 12) (1 13))
                (NUMBER     "0200000000"  (1 14) (1 24))))
(check-equal? (dump-tokens "0xdeadbeef != -1")
              '((NUMBER     "0xdeadbeef"  (1 0) (1 10))
                (OP         "!="          (1 11) (1 13))
                (OP         "-"           (1 14) (1 15))
                (NUMBER     "1"           (1 15) (1 16))))
(check-equal? (dump-tokens "0xdeadc0de & 012345")
              '((NUMBER     "0xdeadc0de"  (1 0) (1 10))
                (OP         "&"           (1 11) (1 12))
                (NUMBER     "012345"      (1 13) (1 19))))
(check-equal? (dump-tokens "0xFF & 0x15 | 1234")
              '((NUMBER     "0xFF"        (1 0) (1 4))
                (OP         "&"           (1 5) (1 6))
                (NUMBER     "0x15"        (1 7) (1 11))
                (OP         "|"           (1 12) (1 13))
                (NUMBER     "1234"        (1 14) (1 18))))



;; Long integers
(check-equal? (dump-tokens "x = 0L")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "0L"          (1 4) (1 6))))

;; Note: the tests at this point in the original Python source look
;; somewhat corrupted.  I need to follow up and see if that's still
;; the case in the original Python sources, or if something really
;; weird has happened.
;;
;; Ah, ok.  the dump_tokens in Python's version of the tests truncate
;; the text of the token.  Strange.

(check-equal? (dump-tokens "x = 0xfffffffffff")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "0xfffffffffff" (1 4) (1 17))))

(check-equal? (dump-tokens "x = 123141242151251616110l")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "123141242151251616110l" (1 4) (1 26))))

(check-equal? (dump-tokens "x = -15921590215012591L")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (OP         "-"           (1 4) (1 5))
                (NUMBER     "15921590215012591L" (1 5) (1 23))))



;; Floating point numbers
(check-equal? (dump-tokens "x = 3.14159")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "3.14159"     (1 4) (1 11))))
(check-equal? (dump-tokens "x = 314159.")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "314159."     (1 4) (1 11))))
(check-equal? (dump-tokens "x = .314159")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     ".314159"     (1 4) (1 11))))
(check-equal? (dump-tokens "x = 3e14159")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "3e14159"     (1 4) (1 11))))
(check-equal? (dump-tokens "x = 3E123")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "3E123"       (1 4) (1 9))))
(check-equal? (dump-tokens "x+y = 3e-1230")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "+"           (1 1) (1 2))
                (NAME       "y"           (1 2) (1 3))
                (OP         "="           (1 4) (1 5))
                (NUMBER     "3e-1230"     (1 6) (1 13))))
(check-equal? (dump-tokens "x = 3.14e159")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "3.14e159"    (1 4) (1 12))))


;; String literals
;;
;; dyoo: the doctests involving quotes embedded in the string literals
;; are adding additional backslashes that I don't quite understand.
;; I've needed to hand-massage the test inputs and outputs to what I
;; think are the intention.
(check-equal? (dump-tokens "x = ''; y = \"\"")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (STRING     "''"          (1 4) (1 6))
                (OP         ";"           (1 6) (1 7))
                (NAME       "y"           (1 8) (1 9))
                (OP         "="           (1 10) (1 11))
                (STRING     "\"\""        (1 12) (1 14))))
(check-equal? (dump-tokens "x = '\"'; y = \"'\"")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (STRING     "'\"'"        (1 4) (1 7))
                (OP         ";"           (1 7) (1 8))
                (NAME       "y"           (1 9) (1 10))
                (OP         "="           (1 11) (1 12))
                (STRING     "\"'\""       (1 13) (1 16))))
(check-equal? (dump-tokens "x = \"doesn't \"shrink\", does it\"")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (STRING     "\"doesn't \"" (1 4) (1 14))
                (NAME       "shrink"      (1 14) (1 20))
                (STRING     "\", does it\"" (1 20) (1 31))))
(check-equal? (dump-tokens "x = u'abc' + U'ABC'")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (STRING     "u'abc'"      (1 4) (1 10))
                (OP         "+"           (1 11) (1 12))
                (STRING     "U'ABC'"      (1 13) (1 19))))
(check-equal? (dump-tokens "y = u\"ABC\" + U\"ABC\"")
              '((NAME       "y"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (STRING     "u\"ABC\""    (1 4) (1 10))
                (OP         "+"           (1 11) (1 12))
                (STRING     "U\"ABC\""    (1 13) (1 19))))
(check-equal?  (dump-tokens "x = ur'abc' + Ur'ABC' + uR'ABC' + UR'ABC'")
               '((NAME       "x"           (1 0) (1 1))
                 (OP         "="           (1 2) (1 3))
                 (STRING     "ur'abc'"     (1 4) (1 11))
                 (OP         "+"           (1 12) (1 13))
                 (STRING     "Ur'ABC'"     (1 14) (1 21))
                 (OP         "+"           (1 22) (1 23))
                 (STRING     "uR'ABC'"     (1 24) (1 31))
                 (OP         "+"           (1 32) (1 33))
                 (STRING     "UR'ABC'"     (1 34) (1 41))))
(check-equal? (dump-tokens @r{y = ur"abc" + Ur"ABC" + uR"ABC" + UR"ABC"})
              '((NAME       "y"            (1 0) (1 1))
                (OP         "="            (1 2) (1 3))
                (STRING     "ur\"abc\""    (1 4) (1 11))
                (OP         "+"            (1 12) (1 13))
                (STRING     "Ur\"ABC\""    (1 14) (1 21))
                (OP         "+"            (1 22) (1 23))
                (STRING     "uR\"ABC\""    (1 24) (1 31))
                (OP         "+"            (1 32) (1 33))
                (STRING     "UR\"ABC\""    (1 34) (1 41))))

;; Operators
(check-equal? (dump-tokens "def d22(a, b, c=2, d=2, *k): pass")
              '((NAME       "def"         (1 0) (1 3))
                (NAME       "d22"         (1 4) (1 7))
                (OP         "("           (1 7) (1 8))
                (NAME       "a"           (1 8) (1 9))
                (OP         ","           (1 9) (1 10))
                (NAME       "b"           (1 11) (1 12))
                (OP         ","           (1 12) (1 13))
                (NAME       "c"           (1 14) (1 15))
                (OP         "="           (1 15) (1 16))
                (NUMBER     "2"           (1 16) (1 17))
                (OP         ","           (1 17) (1 18))
                (NAME       "d"           (1 19) (1 20))
                (OP         "="           (1 20) (1 21))
                (NUMBER     "2"           (1 21) (1 22))
                (OP         ","           (1 22) (1 23))
                (OP         "*"           (1 24) (1 25))
                (NAME       "k"           (1 25) (1 26))
                (OP         ")"           (1 26) (1 27))
                (OP         ":"           (1 27) (1 28))
                (NAME       "pass"        (1 29) (1 33))))
(check-equal? (dump-tokens "def d01v_(a=1, *k, **w): pass")
              '((NAME       "def"         (1 0) (1 3))
                (NAME       "d01v_"       (1 4) (1 9))
                (OP         "("           (1 9) (1 10))
                (NAME       "a"           (1 10) (1 11))
                (OP         "="           (1 11) (1 12))
                (NUMBER     "1"           (1 12) (1 13))
                (OP         ","           (1 13) (1 14))
                (OP         "*"           (1 15) (1 16))
                (NAME       "k"           (1 16) (1 17))
                (OP         ","           (1 17) (1 18))
                (OP         "**"          (1 19) (1 21))
                (NAME       "w"           (1 21) (1 22))
                (OP         ")"           (1 22) (1 23))
                (OP         ":"           (1 23) (1 24))
                (NAME       "pass"        (1 25) (1 29))))


;; Comparison
(check-equal? (dump-tokens (string-append
                            "if 1 < 1 > 1 == 1 >= 5 <= 0x15 <= 0x12 != "
                            "1 and 5 in 1 not in 1 is 1 or 5 is not 1: pass"))
              '((NAME       "if"          (1 0) (1 2))
                (NUMBER     "1"           (1 3) (1 4))
                (OP         "<"           (1 5) (1 6))
                (NUMBER     "1"           (1 7) (1 8))
                (OP         ">"           (1 9) (1 10))
                (NUMBER     "1"           (1 11) (1 12))
                (OP         "=="          (1 13) (1 15))
                (NUMBER     "1"           (1 16) (1 17))
                (OP         ">="          (1 18) (1 20))
                (NUMBER     "5"           (1 21) (1 22))
                (OP         "<="          (1 23) (1 25))
                (NUMBER     "0x15"        (1 26) (1 30))
                (OP         "<="          (1 31) (1 33))
                (NUMBER     "0x12"        (1 34) (1 38))
                (OP         "!="          (1 39) (1 41))
                (NUMBER     "1"           (1 42) (1 43))
                (NAME       "and"         (1 44) (1 47))
                (NUMBER     "5"           (1 48) (1 49))
                (NAME       "in"          (1 50) (1 52))
                (NUMBER     "1"           (1 53) (1 54))
                (NAME       "not"         (1 55) (1 58))
                (NAME       "in"          (1 59) (1 61))
                (NUMBER     "1"           (1 62) (1 63))
                (NAME       "is"          (1 64) (1 66))
                (NUMBER     "1"           (1 67) (1 68))
                (NAME       "or"          (1 69) (1 71))
                (NUMBER     "5"           (1 72) (1 73))
                (NAME       "is"          (1 74) (1 76))
                (NAME       "not"         (1 77) (1 80))
                (NUMBER     "1"           (1 81) (1 82))
                (OP         ":"           (1 82) (1 83))
                (NAME       "pass"        (1 84) (1 88))))

              
;; Shift
(check-equal? (dump-tokens "x = 1 << 1 >> 5")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "1"           (1 4) (1 5))
                (OP         "<<"          (1 6) (1 8))
                (NUMBER     "1"           (1 9) (1 10))
                (OP         ">>"          (1 11) (1 13))
                (NUMBER     "5"           (1 14) (1 15))))


;; Additive
(check-equal? (dump-tokens "x = 1 - y + 15 - 01 + 0x124 + z + a[5]")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "1"           (1 4) (1 5))
                (OP         "-"           (1 6) (1 7))
                (NAME       "y"           (1 8) (1 9))
                (OP         "+"           (1 10) (1 11))
                (NUMBER     "15"          (1 12) (1 14))
                (OP         "-"           (1 15) (1 16))
                (NUMBER     "01"          (1 17) (1 19))
                (OP         "+"           (1 20) (1 21))
                (NUMBER     "0x124"       (1 22) (1 27))
                (OP         "+"           (1 28) (1 29))
                (NAME       "z"           (1 30) (1 31))
                (OP         "+"           (1 32) (1 33))
                (NAME       "a"           (1 34) (1 35))
                (OP         "["           (1 35) (1 36))
                (NUMBER     "5"           (1 36) (1 37))
                (OP         "]"           (1 37) (1 38))))

;; Multiplicative
(check-equal? (dump-tokens "x = 1//1*1/5*12%0x12")
              '((NAME       "x"           (1 0) (1 1))
                (OP         "="           (1 2) (1 3))
                (NUMBER     "1"           (1 4) (1 5))
                (OP         "//"          (1 5) (1 7))
                (NUMBER     "1"           (1 7) (1 8))
                (OP         "*"           (1 8) (1 9))
                (NUMBER     "1"           (1 9) (1 10))
                (OP         "/"           (1 10) (1 11))
                (NUMBER     "5"           (1 11) (1 12))
                (OP         "*"           (1 12) (1 13))
                (NUMBER     "12"          (1 13) (1 15))
                (OP         "%"           (1 15) (1 16))
                (NUMBER     "0x12"        (1 16) (1 20))))

;; Unary
(check-equal? (dump-tokens "~1 ^ 1 & 1 |1 ^ -1")
              '((OP         "~"           (1 0) (1 1))
                (NUMBER     "1"           (1 1) (1 2))
                (OP         "^"           (1 3) (1 4))
                (NUMBER     "1"           (1 5) (1 6))
                (OP         "&"           (1 7) (1 8))
                (NUMBER     "1"           (1 9) (1 10))
                (OP         "|"           (1 11) (1 12))
                (NUMBER     "1"           (1 12) (1 13))
                (OP         "^"           (1 14) (1 15))
                (OP         "-"           (1 16) (1 17))
                (NUMBER     "1"           (1 17) (1 18))))
(check-equal? (dump-tokens "-1*1/1+1*1//1 - ---1**1")
              '((OP         "-"           (1 0) (1 1))
                (NUMBER     "1"           (1 1) (1 2))
                (OP         "*"           (1 2) (1 3))
                (NUMBER     "1"           (1 3) (1 4))
                (OP         "/"           (1 4) (1 5))
                (NUMBER     "1"           (1 5) (1 6))
                (OP         "+"           (1 6) (1 7))
                (NUMBER     "1"           (1 7) (1 8))
                (OP         "*"           (1 8) (1 9))
                (NUMBER     "1"           (1 9) (1 10))
                (OP         "//"          (1 10) (1 12))
                (NUMBER     "1"           (1 12) (1 13))
                (OP         "-"           (1 14) (1 15))
                (OP         "-"           (1 16) (1 17))
                (OP         "-"           (1 17) (1 18))
                (OP         "-"           (1 18) (1 19))
                (NUMBER     "1"           (1 19) (1 20))
                (OP         "**"          (1 20) (1 22))
                (NUMBER     "1"           (1 22) (1 23))))

;; Selector
(check-equal? (dump-tokens "import sys, time\nx = sys.modules['time'].time()")
              '((NAME       "import"      (1 0) (1 6))
                (NAME       "sys"         (1 7) (1 10))
                (OP         ","           (1 10) (1 11))
                (NAME       "time"        (1 12) (1 16))
                (NEWLINE    "\n"         (1 16) (1 17))
                (NAME       "x"           (2 0) (2 1))
                (OP         "="           (2 2) (2 3))
                (NAME       "sys"         (2 4) (2 7))
                (OP         "."           (2 7) (2 8))
                (NAME       "modules"     (2 8) (2 15))
                (OP         "["           (2 15) (2 16))
                (STRING     "'time'"      (2 16) (2 22))
                (OP         "]"           (2 22) (2 23))
                (OP         "."           (2 23) (2 24))
                (NAME       "time"        (2 24) (2 28))
                (OP         "("           (2 28) (2 29))
                (OP         ")"           (2 29) (2 30))))

;; Methods
(check-equal? (dump-tokens "@staticmethod\ndef foo(x,y): pass")
              '((OP         "@"            (1 0) (1 1))
                (NAME       "staticmethod" (1 1) (1 13))
                (NEWLINE    "\n"           (1 13) (1 14))
                (NAME       "def"          (2 0) (2 3))
                (NAME       "foo"          (2 4) (2 7))
                (OP         "("            (2 7) (2 8))
                (NAME       "x"            (2 8) (2 9))
                (OP         ","            (2 9) (2 10))
                (NAME       "y"            (2 10) (2 11))
                (OP         ")"            (2 11) (2 12))
                (OP         ":"            (2 12) (2 13))
                (NAME       "pass"         (2 14) (2 18))))

;; Evil tabs
(check-equal? (dump-tokens "def f():\n\tif x\n        \tpass")
              '((NAME       "def"         (1 0) (1 3))
                (NAME       "f"           (1 4) (1 5))
                (OP         "("           (1 5) (1 6))
                (OP         ")"           (1 6) (1 7))
                (OP         ":"           (1 7) (1 8))
                (NEWLINE    "\n"          (1 8) (1 9))
                (INDENT     "\t"          (2 0) (2 1))
                (NAME       "if"          (2 1) (2 3))
                (NAME       "x"           (2 4) (2 5))
                (NEWLINE    "\n"          (2 5) (2 6))
                (INDENT     "        \t"  (3 0) (3 9))
                (NAME       "pass"        (3 9) (3 13))
                (DEDENT     ""            (4 0) (4 0))
                (DEDENT     ""            (4 0) (4 0))))
