#lang racket/base

(require
  rackunit
  "../lang/tokenizer.rkt")

(check-equal? (fix-escapes "\\\\") "\\")
(check-equal? (fix-escapes "\\n") "\n")
(check-equal? (fix-escapes "\\r") "\r")
(check-equal? (fix-escapes "\\t") "\t")
(check-equal? (fix-escapes "\\'") "'")
(check-equal? (fix-escapes "\\\"") "\"")
(check-equal? (fix-escapes "\\\\t") "\\t")
(check-equal? (fix-escapes "\\\\n") "\\n")
(check-equal? (fix-escapes "\\\\'") "\\'")

(check-equal? (fix-escapes "\\ n") "\\ n")
(check-equal? (fix-escapes "\\\\\\\\n") "\\\\n")
(check-equal? (fix-escapes "\n") "\n")

(check-equal? (fix-escapes "\\n\\r") "\n\r")

(check-equal? (fix-escapes "\\t\\\\n") "\t\\n")

(check-equal? (fix-escapes "This is an example
of a multi-line string \\\\
that someone put stuff at the \\r\\n ends of")
"This is an example\nof a multi-line string \\\nthat someone put stuff at the \r\n ends of")

(check-equal? (fix-escapes "(\\\\") "(\\")

(check-equal? (fix-escapes "\"(\\\\\"") "\"(\\\"")
