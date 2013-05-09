#lang pyret

import "src/lang/pyret-lib/experimental/check.arr" as Check

todo1 = {
  # Constructor should return an object to use as self
  constructor(_, self, spec):
    self.{
      due: spec.due,
      task: spec.task
    }
  end,
  due: "25 January 2012",
  task: "Write mixin examples",
  done: false,
  complete(self): self.{ done: true }
}

fun droppable(obj):
  obj.{
    drop(self, names):
      builtins.keys(self).foldr(\name, self-dropped: (
        cond:
          | names.member(name) => self-dropped
          | else => self-dropped.{ [name]: self.[name] }
        end
      ), {})
    end
  }
end

fun make-class(obj):
  { 
    instantiate(self, spec):
      instance = droppable(obj).drop(["constructor"])
      obj.constructor(instance, spec)
    end
  }
end
  
todo-class = make-class(todo1)
todo2 = todo-class.instantiate({ due: "today", task: "implement classes" })
Check.equal(todo2.due, "today", "due was instantiated")
Check.equal(todo2.done, false, "done used default")
todo2.constructor() # should err
