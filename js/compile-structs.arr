#lang pyret

provide *
import string-dict as D

data CompileEnvironment:
  | compile-env(modules :: D.StringDict)
end

data CompileResult:
  | ok(code :: String)
  | err(message :: String)
end

no-builtins = compile-env(D.immutable-string-dict())

standard-builtins = compile-env(
    D.to-dict({
        list: [
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
          ],
        option: [
            "Option",
            "is-none",
            "is-some",
            "none",
            "some"
          ],
        error: [],
        sets: [
            "set",
            "tree-set",
            "list-set"
          ]
      })
  )
