(module
  "sets"
  (path "src/arr/base/sets.arr")
  (unknown-item
    (name "set")
    ;; { make: ~arr-to-list-set41 }
    )
  (unknown-item
    (name "list-set")
    ;; { make: ~arr-to-list-set41 }
    )
  (unknown-item
    (name "tree-set")
    ;; { make: ~arr-to-tree-set42 }
    )
  (unknown-item
    (name "empty-list-set")
    ;; ~list-set34(empty14)
    )
  (unknown-item
    (name "empty-tree-set")
    ;; ~tree-set36(~leaf22)
    )
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
    (doc "Convert a list into a tree-based set.")))