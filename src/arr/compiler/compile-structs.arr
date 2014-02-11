#lang pyret

provide *

data CompileEnvironment:
  | compile-env(modules :: List)
end

data CompileResult<C, P>:
  | ok(code :: C)
  | err(problems :: P)
end

no-builtins = compile-env([])

standard-builtins = compile-env(
    [{ name: "list",
       bindings: [
            "is-empty",
            "is-link",
            "empty",
            "link",
            "range",
            "repeat",
            "filter",
            "partition",
            "split-at",
            "any",
            "find",
            "map",
            "map2",
            "map3",
            "map4",
            "map_n",
            "map2_n",
            "map3_n",
            "map4_n",
            "each",
            "each2",
            "each3",
            "each4",
            "each_n",
            "each2_n",
            "each3_n",
            "each4_n",
            "fold",
            "fold2",
            "fold3",
            "fold4",
            "index"
          ]
        },
      { name: "option",
        bindings: [
            "Option",
            "is-none",
            "is-some",
            "none",
            "some"
          ]
        },
      { name: "error",
        bindings: []
        },
      { name: "sets",
        bindings: [
            "set",
            "tree-set",
            "list-set"
          ]
      }
    ])
