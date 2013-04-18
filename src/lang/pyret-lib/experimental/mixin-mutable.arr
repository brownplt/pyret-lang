#lang pyret

import "src/lang/pyret-lib/experimental/check.arr" as Check

var todo1: {
  due: "25 January 2012",
  task: "Write mixin examples",
  done: false
}

fun doable(todo):
  todo.{ complete(self): self.{ done : true } end }
end

var todoable1: doable(todo1)
Check.equal(true, todoable1.complete().done, "todoable")

fun make-mutable(obj):
  var names: builtins.keys(obj)
  fun make-mutable-field(name, new-obj):
    var field-val: obj.[name]
    new-obj.{ [name](self, new-val):
      cond:
        | is-nothing(new-val) => field-val
        | else => field-val = new-val      
      end
    }
  end
  names.foldr(make-mutable-field, {})
end

var mutabletodo1: make-mutable(todo1)
mutabletodo1.done(true)
# need some way to distinguish b/w nothing and optional arguments
Check.equal(true, mutabletodo1.done(nothing), "mutable")

