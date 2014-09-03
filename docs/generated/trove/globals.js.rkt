(module
  "<global>"
  (path "src/js/base/runtime-anf.js")
  (data-spec
    (name "Any")
    (variants)
    (shared))
  (data-spec
    (name "Number")
    (variants)
    (shared))
  (data-spec
    (name "Boolean")
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
    (name "raise")
    (arity 1)
    (args ("val"))
    (return (a-id "Nothing" (xref "<global>" "Nothing")))
    (doc "Raises the value as an exception"))
  (fun-spec
    (name "torepr")
    (arity 1)
    (args ("val"))
    (return (a-id "String" (xref "<global>" "String")))
    (doc "Creates a string representation of the value"))
  (fun-spec
    (name "tostring")
    (arity 1)
    (args ("val"))
    (return (a-id "String" (xref "<global>" "String")))
    (doc "Creates a string representation of the value"))
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
    (name "is-object")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact object"))
  (fun-spec
    (name "is-method")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact method"))
  (fun-spec
    (name "is-raw-array")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact method"))

)
