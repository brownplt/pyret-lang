(module
  "sets"
  (path "src/arr/base/sets.arr")
  (re-export (name "List") (cross-ref "lists" "List"))
  (re-export (name "empty") (cross-ref "lists" "empty"))
  (re-export (name "link") (cross-ref "lists" "link"))
  (re-export (name "is-empty") (cross-ref "lists" "is-empty"))
  (re-export (name "fold") (cross-ref "lists" "fold"))

  (data-spec
   (name "Set")
   (variants ("list-set" "tree-set"))
   (shared
       ((method-spec
         (name "symmetric_difference")
         (arity 2)
         (args ("self" "other"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))))
         (doc "Compute the symmetric difference of this set and another set."))
        (method-spec
         (name "_equals")
         (arity 2)
         (args ("self" "other"))
         (contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any" "Any")))
        (method-spec
         (name "tostring")
         (arity 1)
         (args ("self"))
         (contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any")))
        (method-spec
         (name "_torepr")
         (arity 1)
         (args ("self"))
         (contract (a-arrow (a-id "Set" (xref "sets" "Set")) "Any")))
        (method-spec
         (name "member")
         (arity 2)
         (args ("self" "elem"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           "Any"
           (a-id "Boolean" (xref "<global>" "Boolean"))))
         (doc "Check to see if an element is in a set."))
        (method-spec
         (name "add")
         (arity 2)
         (args ("self" "elem"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           "Any"
           (a-id "Set" (xref "sets" "Set"))))
         (doc "Add an element to the set if it is not already present."))
        (method-spec
         (name "remove")
         (arity 2)
         (args ("self" "elem"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           "Any"
           (a-id "Set" (xref "sets" "Set"))))
         (doc "Remove an element from the set if it is present."))
        (method-spec
         (name "to-list")
         (arity 1)
         (args ("self"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           (a-compound (a-dot "lists" "List") (xref "lists" "List"))))
         (doc "Convert a set into a list of elements."))
        (method-spec
         (name "union")
         (arity 2)
         (args ("self" "other"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))))
         (doc "Compute the union of this set and another set."))
        (method-spec
         (name "intersect")
         (arity 2)
         (args ("self" "other"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))))
         (doc "Compute the intersection of this set and another set."))
        (method-spec
         (name "difference")
         (arity 2)
         (args ("self" "other"))
         (contract
          (a-arrow
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))
           (a-id "Set" (xref "sets" "Set"))))
         (doc "Compute the difference of this set and another set.")))))

  (fun-spec
   (name "is-list-set")
   (arity 1)
   (args ("val"))
   (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
   (doc "Checks whether the provided argument is in fact a list-set"))
  (fun-spec
   (name "is-tree-set")
   (arity 1)
   (args ("val"))
   (contract (a-arrow "Any" (a-id "Boolean" (xref "<global>" "Boolean"))))
   (doc "Checks whether the provided argument is in fact a tree-set"))
  (fun-spec
   (name "list-to-set")
   (arity 2)
   (args ("lst" "base-set"))
   (contract
    (a-arrow
     (a-compound (a-dot "lists" "List") (xref "lists" "List"))
     (a-id "Set" (xref "sets" "Set"))
     (a-id "Set" (xref "sets" "Set"))))
   (doc "Convert a list into a set."))
  (fun-spec
   (name "list-to-list-set")
   (arity 1)
   (args ("lst"))
   (contract
    (a-arrow
     (a-compound (a-dot "lists" "List") (xref "lists" "List"))
     (a-id "Set" (xref "sets" "Set"))))
   (doc "Convert a list into a list-based set."))
  (fun-spec
   (name "list-to-tree-set")
   (arity 1)
   (args ("lst"))
   (contract
    (a-arrow
     (a-compound (a-dot "lists" "List") (xref "lists" "List"))
     (a-id "Set" (xref "sets" "Set"))))
   (doc "Convert a list into a tree-based set."))
  (fun-spec
   (name "list-to-tree")
   (arity 1)
   (args ("lst"))
   (contract
    (a-arrow (a-compound (a-dot "lists" "List") (xref "lists" "List")) "Any")))
  (fun-spec
   (name "arr-to-list-set")
   (arity 1)
   (args ("arr"))
   (contract
    (a-arrow
     (a-id "RawArray" (xref "<global>" "RawArray"))
     (a-id "Set" (xref "sets" "Set")))))
  (fun-spec
   (name "arr-to-tree-set")
   (arity 1)
   (args ("arr"))
   (contract
    (a-arrow
     (a-id "RawArray" (xref "<global>" "RawArray"))
     (a-id "Set" (xref "sets" "Set"))))))
