#lang pyret

import Racket as R

check:
  # embedded newline
  "newline
newline" is "newline\nnewline"
  # escaped newline gets elided
  "no \
linebreak" is "no linebreak"

  # check that octal escapes are actually translated
  "\141" is "a"
  R("racket/base")("char->integer", R("racket/base")("string-ref", "\344", 0)) is 228

  # check that hex escapes are actually translated
  "\u0061" is "a"
  "\x41" is "A"
  R("racket/base")("char->integer", R("racket/base")("string-ref", "\u5267", 0)) is 21095

  # escapes at end of strings are parsed and escaped properly
  "\"" is '"'
  '\'' is "'"
  "\\" is "\x5C"
  "\t" is "\011"
  "\n" is "\x0a"
  "\r" is "\x0d"

  "a".substring(0, 5) is "a"
  "a".substring(52, 0) raises "Start index is past"
  "a-str".substring(3, 1) raises "end to be greater than start"
  "a-str".substring(-1, 1) raises "non-negative"
  "abc".substring(0, 1) is "a"
  "abc".substring(1, 3) is "bc"

  "".char-at(4) raises "Index too large"
  "abc".char-at(2) is "c"
  "abc".char-at(3) raises "Index too large"

  "".to-lower() is ""
  "".to-upper() is ""
  "a".to-upper() is "A"
  "aBc".to-upper() is "ABC"
  "aB-c%a".to-upper() is "AB-C%A"
  "λ".to-upper() is "Λ"

  "λ".to-lower() is "λ"
  "Λ".to-lower() is "λ"
  "ß".to-upper() is "SS"
  "ß".to-lower() is "ß"

  "A".to-lower() is "a"
  "aBc".to-lower() is "abc"
  "aB-C%a".to-lower() is "ab-c%a"
end
