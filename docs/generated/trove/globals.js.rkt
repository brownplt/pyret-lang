(module
  "<global>"
  (path "src/js/base/runtime-anf.js")
  (fun-spec
    (name "Number")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a number"))
  (fun-spec
    (name "Bool")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a boolean"))
  (fun-spec
    (name "String")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact a string"))
  (fun-spec
    (name "Nothing")
    (arity 1)
    (args ("val"))
    (doc "Checks whether the provided argument is in fact nothing"))
)
