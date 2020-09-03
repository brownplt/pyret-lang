include primitive-types
include string
include option
import lists as L

include from L:
  list
end

# Tests lifted from the examples in the documentation
# https://www.pyret.org/docs/latest/strings.html

check "string-equal":
  string-equal("abc", "abc") is true
  "abc" is%(string-equal) "abc"
  "abc" == "abc" is true
  string-equal("ab", "abc") is false
  string-equal("abc     ", "abc") is false
end

check "string-contains":
  string-contains("Ahoy, world!", "world") is true
  string-contains("Ahoy, World!", "world") is false
  string-contains("world", "Ahoy world") is false
  string-contains("same string", "same string") is true
  string-contains("", "") is true
  string-contains("any string", "") is true
end

check "string-append":
  string-append("a", "b") is "ab"
  string-append("same", "same") is "samesame"
  string-append("", "") is ""
  string-append("", "a") is "a"
  string-append("a", "") is "a"
end

check "string-length":
  string-length("") is 0
  string-length("    ") is 4
  string-length("four") is 4
  string-length("🏏") is 2
end

check "string-to-number":
  string-to-number("100") is some(100)
  string-to-number("not-a-number") is none
  string-to-number(" 100") is none
  string-to-number("100abc") is none
  string-to-number("1,000") is none
  string-to-number("1-800-555-1212") is none
end

check "string-repeat":
  string-repeat("a", 5) is "aaaaa"
  string-repeat("", 1000000) is ""
  string-repeat("word ", 3) is "word word word "
  string-repeat("long string", 0) is ""
end

check "string-substring":
  string-substring("just the first", 0, 1) is "j"
  string-substring("same index", 4, 4) is ""

  tws = "length is 12"
  string-substring(tws, 4, 6) is "th"
  string-substring(tws, string-length(tws) - 1, string-length(tws)) is "2"
  string-substring(tws, 6, 4) raises "index"
  string-substring(tws, 6, 13) raises "index"
  string-substring(tws, 13, 6) raises "index"
  string-substring(tws, -1, 10) raises "index"
end

check "string-index-of":
  string-index-of("Pyret", "P") is 0
  string-index-of("012🤑45", "🤑") is 3
  string-index-of("🤔🤔🤔", "🤒") is -1
end

check "string-replace":
  string-replace("spaces to hyphens", " ", "-") is "spaces-to-hyphens"
  string-replace("remove: the: colons", ":", "") is "remove the colons"
  string-replace("😊😊🤕😊", "🤕", "😊") is "😊😊😊😊"
  string-replace("rinky dinky", "inky", "azzle") is "razzle dazzle"
  string-replace("a string", "not found", "not replaced") is "a string"
  string-replace("", "", "c") is ""
  string-replace("aaa", "", "b") is "ababa"
end

check "string-split":
  string-split("string", "not found") is [list: "string"]
  string-split("string", "g") is [list: "strin", ""]
  string-split("string", "") is [list: "", "string"]
  string-split("a-b-c", "-") is [list: "a", "b-c"]
end

check "string-split-all":
  string-split-all("string", "not found") is [list: "string"]
  string-split-all("a-b-c", "-") is [list: "a", "b", "c"]
  string-split-all("split on spaces", " ") is [list: "split", "on", "spaces"]
  string-split-all("explode", "") is [list: "e", "x", "p", "l", "o", "d", "e"]
  string-split-all("bananarama", "na") is [list: "ba", "", "rama"]
  string-split-all("bananarama", "a") is [list: "b", "n", "n", "r", "m", ""]
end

check "string-char-at":
  string-char-at("abc", 1) is "b"
  string-char-at("a", 0) is "a"
end

check "string-to-upper":
  string-to-upper("a") is "A"
  string-to-upper("I'm not yelling!") is "I'M NOT YELLING!"
  string-to-upper("ß") is "SS"
  string-to-upper("λαμβδα") is "ΛΑΜΒΔΑ"
  string-to-upper("😊") is "😊"
  string-to-upper(" ﷵ‎") is " ﷵ‎"
end

check "string-to-lower":
  string-to-lower("A") is "a"
  string-to-lower("I'M NOT YELLING!") is "i'm not yelling!"
  string-to-lower("SS") is "ss"
  string-to-lower("ΛΑΜΒΔΑ") is "λαμβδα"
end

check "string-to-code-point":
  string-to-code-point("a") is 97
  string-to-code-point("\n") is 10
  string-to-code-point("λ") is 955
end

check "string-to-code-points":
  string-to-code-points("") is [list:]
  string-to-code-points("abc") is [list: 97, 98, 99]
  string-to-code-points("😊") is [list: 55357, 56842]
  string-to-code-points("𝄞") is [list: 55348, 56606]
end

check "string-from-code-point":
  string-from-code-point(97) is "a"
  string-from-code-point(10) is "\n"
  string-from-code-point(955) is "λ"
end

check "string-from-code-points":
  string-from-code-points([list:]) is ""
  string-from-code-points([list: 97, 98, 99]) is "abc"
  string-from-code-points([list: 55348, 56606]) is "𝄞"
end
