#lang pyret

provide *

fun run-tests():
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
    string-length("a") is 1
    string-length("\n\r \t") is 4
    string-length("λjs") is 3
    
    string-tonumber("45") is 45
    # TODO(joe): some string-to-number parsing issue on the number below
    #string-tonumber("100000000000000000000000000000000000001")
    #  is 100000000000000000000000000000000000001
    string-tonumber("1/2") is 0.5

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
    string-replace("abc", "", "empty") is "emptyaemptybemptycempty"

    string-split("abc", "") is ["a", "b", "c"]
    string-split("abc", "b") is ["a", "c"]
    string-split("sentence is a typical use case", " ") is [
        "sentence", "is", "a", "typical", "use", "case"
      ]
    string-split("aabcb", "b") is ["aa", "c", ""]

  end
end

