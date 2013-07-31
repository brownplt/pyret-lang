#lang pyret

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
  complete(self): self.{ done: true } end
}

fun droppable(obj):
  obj.{
    drop(self, names):
      for list.fold(self-dropped from {}, name from builtins.keys(self)):
        if names.member(name):
          self-dropped
        else:
          self-dropped.{ [name]: self.[name] }
        end
      end
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
check:
  todo-class = make-class(todo1)
  todo2 = todo-class.instantiate({ due: "today", task: "implement classes" })
  checkers.check-equals("due was instantiated", todo2.due, "today")
  checkers.check-equals("done used default", todo2.done, false)
  try:
    todo2.constructor() # should err
  except(e):
    checkers.check-equals("constructor not present on instance",
                          error.is-field-not-found(e),
                          true)
  end
end
