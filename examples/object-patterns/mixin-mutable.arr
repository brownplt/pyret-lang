#lang pyret

fun make-mutable(obj):
  names = builtins.keys(obj)
  fun make-mutable-field(name, new-obj):
    var field-val = obj.[name]
    new-obj.{ [name](self, new-val):
        if is-nothing(new-val):
          field-val
        else:
          field-val := new-val      
        end
      end
    }
  end
  names.foldr(make-mutable-field, {})
check:
  todo1 = {
    due: "25 January 2012",
    task: "Write mixin examples",
    done: false
  }

  fun doable(todo):
    todo.{ complete(self): self.{ done : true } end }
  end

  todoable1 = doable(todo1)
  checkers.check-equals("todoable", true, todoable1.complete().done)


  mutabletodo1 = make-mutable(todo1)
  mutabletodo1.done(true)
  # need some way to distinguish b/w nothing and optional arguments
  checkers.check-equals("mutable", true, mutabletodo1.done(nothing))
end
