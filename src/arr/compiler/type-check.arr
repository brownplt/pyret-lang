#lang pyret

provide *
provide-types *
import ast as A
import "compiler/compile-structs.arr" as C
import "compiler/ast-util.arr" as U

# empty object
default-visitor = U.binding-env-map-visitor({
    bindings : [list: ],
    types    : [list: ]
  })

type-checker = default-visitor #.{...}
fun type-check(e): e.visit(type-checker) end
