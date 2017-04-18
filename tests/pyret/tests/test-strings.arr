#lang pyret

provide *
import error as E

check:
  string-char-at("", 0) raises "index"
  string-char-at("a", -1) raises "index"
  string-char-at("a", 1.5) raises "integer"
end

check:
  string-substring("abc", 1.5, 3) raises "start"
  string-substring("abc", 1, 1.5) raises "end"
end

check:
  string-to-number("asdf").or-else("worked") is "worked"
  string-to-number("100").or-else("failed") is 100
end

check:
  string-contains("a", "") is true
  string-contains("a", "a") is true
  string-contains("aa", "a") is true

  string-contains("ab", "a") is true
  string-contains("a b a", "b") is true
  string-contains("a b a", "ba") is false
  string-contains("a ba a", "ba") is true

  string-contains("", "") is true

  string-append("", "") is ""
  string-append(" ", "") is " "
  string-append("", " ") is " "
  string-append("a", "a") is "aa"
  string-append("abc", "def") is "abcdef"

  string-length("") is 0
  # zero-width-space
  string-length(string-from-code-point(2083)) is 1
  string-length("a") is 1
  string-length("\b\n\r \t") is 5
  string-length("λjs") is 3

  string-tonumber("45") is 45
  # TODO(joe): some string-to-number parsing issue on the number below
  #string-tonumber("100000000000000000000000000000000000001")
  #  is 100000000000000000000000000000000000001
  string-tonumber("1/2") is 0.5

  # hyphenated version also bound
  string-to-number("1/2") is some(0.5)

  "1/2" satisfies string-is-number
  "abc" violates string-is-number
  "3+5i" violates string-is-number
  "1a2" violates string-is-number

  string-repeat("a", 4) is "aaaa"
  string-repeat("a", 0) is ""
  string-repeat("abc", 3) is "abcabcabc"

  string-substring("abc", 0, 1) is "a"
  string-substring("", 0, 0) is ""
  string-substring("abc", 0, 0) is ""
  string-substring("abc", 0, 3) is "abc"

  string-substring("", 0, 1) raises ""
  string-substring("abc", -1, 1) raises ""
  string-substring("abc", 0, 4) raises ""
  string-substring("abc", 0, 8) raises ""
  string-substring("abc", 8, 4) raises ""
  string-substring("abc", 2, 1) raises ""

  string-replace("abcd", "bc", "dd") is "addd"
  string-replace("abcbcd", "bc", "dd") is "addddd"
  string-replace("acccd", "cc", "dd") is "addcd"
  string-replace("acccd", "cc", "d") is "adcd"
  string-replace("accccd", "cc", "dd") is "addddd"
  string-replace("", "long-string", "replace") is ""
  string-replace("abcd", "abcd", "") is ""
  string-replace("abcdabcd", "abcd", "") is ""
  string-replace("abc", "", "empty") is "aemptybemptyc"

  string-split-all("abc", "") is [list: "a", "b", "c"]
  string-split-all("abc", "b") is [list: "a", "c"]
  string-split-all("sentence is a typical use case", " ") is [list:
      "sentence", "is", "a", "typical", "use", "case"
    ]
  string-split-all("aabcb", "b") is [list: "aa", "c", ""]

  string-split("abc", "") is [list: "", "abc"]
  string-split("abc", "b") is [list: "a", "c"]
  string-split("sentence is a typical use case", " ") is [list:
      "sentence", "is a typical use case"
    ]
  string-split("aabcb", "b") is [list: "aa", "cb"]

end

check:
  string-to-code-point("a", "b") raises-satisfies E.is-arity-mismatch
  string-to-code-point("ab") raises "length exactly one"
  string-to-code-point("") raises "length exactly one"
  string-to-code-point(5) raises "String"
  string-from-code-point(5, 6) raises-satisfies E.is-arity-mismatch
  string-from-code-point("a") raises "Natural Number"
  string-from-code-point(-1) raises "Natural Number"
  string-from-code-point(1000000000000000000000) raises "Invalid code point"
  string-from-code-point(1.1) raises "Natural Number"

  string-to-code-points(5) raises "String"
  string-from-code-points(5) raises "List"

  string-to-code-points("", 5) raises-satisfies E.is-arity-mismatch
  string-from-code-points([list:], 5) raises-satisfies E.is-arity-mismatch

  string-to-code-point("a") is 97
  string-to-code-point("\n") is 10
  string-to-code-point("\b") is 8

  string-to-code-point("λ") is 955
  string-from-code-point(955) is "λ"

  # zero-width space
  string-to-code-point("\u200b") is 8203
  string-from-code-point(8203) is "\u200b"

  # null
  string-to-code-point("\u0000") is 0
  string-from-code-point(0) is "\u0000"

  string-to-code-points("abcd") is [list: 97, 98, 99, 100]
  string-to-code-points("") is [list:]

  string-from-code-points([list: 955, 97, 10]) is "λa\n"
  string-from-code-points([list: 955, -1]) raises "Natural Number"
  string-from-code-points([list:]) is ""

  string-from-code-point(65535) is "\uFFFF"
  string-from-code-point(65534) is "\uFFFE"
  string-from-code-point(65536) raises "Invalid code point"
  string-from-code-point(65537) raises "Invalid code point"

  for each(i from range(0, 1000)):
    ix1 = random(65535)
    ix2 = random(65535)
    ix3 = random(65535)
    str = string-from-code-points([list: ix1, ix2, ix3])
    # this should always be true for non-astral plane characters
    codes = string-to-code-points(str)
    codes is [list: ix1, ix2, ix3]
  end

end

check "case":
  string-to-upper("a") is "A"
  string-to-upper("I'm not yelling!") is "I'M NOT YELLING!"
  string-to-upper("ß") is "SS"
  string-to-upper("λαμβδα") is "ΛΑΜΒΔΑ"

  string-to-lower("A") is "a"
  string-to-lower("I'M NOT YELLING!") is "i'm not yelling!"
  string-to-lower("SS") is "ss"
  string-to-lower("ΛΑΜΒΔΑ") is "λαμβδα"
end
