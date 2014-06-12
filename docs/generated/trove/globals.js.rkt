(module
  "<global>"
  (path "src/js/base/runtime-anf.js")
  (data-spec
    (name "Boolean")
    (variants)
    (shared))
  (data-spec
    (name "Number")
    (variants)
    (shared))
  (data-spec
    (name "String")
    (variants)
    (shared))
  (data-spec
    (name "Nothing")
    (variants)
    (shared))
  (data-spec
    (name "Function")
    (variants)
    (shared))
  (data-spec
    (name "RawArray")
    (variants)
    (shared))
  (data-spec
    (name "Method")
    (variants)
    (shared))
  (data-spec
    (name "Object")
    (variants)
    (shared))
  (fun-spec
    (name "is-boolean")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a boolean"))
  (fun-spec
    (name "is-number")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a number"))
  (fun-spec
    (name "is-string")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a string"))
  (fun-spec
    (name "is-nothing")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact nothing"))
  (fun-spec
    (name "is-function")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact function"))

  (fun-spec
    (name "nums-equal")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-max")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-min")
    (arity 2)
    (args ("n1" "n2"))
    (doc ""))
  (fun-spec
    (name "num-abs")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sin")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-cos")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-tan")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-asin")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-acos")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-atan")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-modulo")
    (arity 2)
    (args ("n" "divisor"))
    (doc ""))
  (fun-spec
    (name "num-truncate")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sqrt")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-sqr")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-ceiling")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-floor")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-log")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-exp")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-expt")
    (arity 2)
    (args ("base" "exponent"))
    (doc ""))
  (fun-spec
    (name "num-exact")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-integer")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-is-fixnum")
    (arity 1)
    (args ("n"))
    (doc ""))
  (fun-spec
    (name "num-tostring")
    (arity 1)
    (args ("n"))
    (doc ""))


  (fun-spec
    (name "strings-equal")
    (arity 2)
    (args ("s1" "s2"))
    (doc ""))
  (fun-spec
    (name "string-contains")
    (arity 2)
    (args ("string-to-search" "string-to-find"))
    (doc ""))
  (fun-spec
    (name "string-append")
    (arity 2)
    (args ("beginning" "end"))
    (doc ""))
  (fun-spec
    (name "string-length")
    (arity 1)
    (args ("s"))
    (doc ""))
  (fun-spec
    (name "string-tonumber")
    (arity 1)
    (args ("s"))
    (doc ""))
  (fun-spec
    (name "string-repeat")
    (arity 2)
    (args ("s" "n"))
    (doc ""))
  (fun-spec
    (name "string-substring")
    (arity 3)
    (args ("s" "start" "end"))
    (doc ""))
  (fun-spec
    (name "string-replace")
    (arity 3)
    (args ("original-string" "string-to-find" "replacement-string"))
    (doc ""))
  (fun-spec
    (name "string-split")
    (arity 2)
    (args ("original-string" "string-to-split-on"))
    (doc ""))
  (fun-spec
    (name "string-char-at")
    (arity 2)
    (args ("s" "n"))
    (doc ""))
  (fun-spec
    (name "string-toupper")
    (arity 1)
    (args ("s"))
    (doc ""))
  (fun-spec
    (name "string-tolower")
    (arity 1)
    (args ("s"))
    (doc ""))
  (fun-spec
    (name "string-explode")
    (arity 1)
    (args ("s"))
    (doc ""))
  (fun-spec
    (name "string-index-of")
    (arity 2)
    (args ("original-string" "string-to-find"))
    (doc ""))

  (fun-spec
    (name "raw-array-of")
    (arity 2)
    (args ("value" "count"))
    (doc ""))
  (fun-spec
    (name "raw-array-get")
    (arity 2)
    (args ("array" "index"))
    (doc ""))
  (fun-spec
    (name "raw-array-set")
    (arity 3)
    (args ("array" "index" "new-value"))
    (doc ""))
  (fun-spec
    (name "raw-array-length")
    (arity 1)
    (args ("array"))
    (doc ""))
  (fun-spec
    (name "raw-array-to-list")
    (arity 1)
    (args ("array"))
    (doc ""))
  (fun-spec
    (name "raw-array-fold")
    (arity 4)
    (args ("f" "init" "array" "start-index"))
    (doc ""))
)
