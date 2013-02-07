#lang pyret

import "src/lang/pyret-lib/experimental/check.arr" as Check

provide {
  class: class,
  instance-of: instance-of
} end

var todo-class-descr: {
  fields: {
    due: "String",
    task: "String",
    done: "Boolean"
  },
  methods: {
    complete(self): self.set("done", true) end
  },
  # Constructor should return an object to use as self
  # : Instance -> Object -> Instance
  constructor(self, spec):
    self.set("due", spec.due)
    self.set("task", spec.task)
    self.set("done", false)
    self
  end
}

# : ClassDescription -> Class
fun class(description):
  {
    _brander: brander(),
    new(self, spec): 
      var fields: description.fields
      var methods: description.methods
      var instance: {
        get(_, name):
          fields.[name]
        end,
        set(_, name, val):
          fields := fields.{ [name]: val }
        end,
        # For now, only support zero arg methods
        invoke(self, name):
          methods:[name]._fun(self)
        end
      }
      var branded: self._brander.brand(instance)
      description:constructor._fun(branded, spec)
    end
  }
end

fun instance-of(instance, class):
  class._brander.check(instance)
end

var Todo: class(todo-class-descr)
var todo1: Todo.new({ due: "Feb 2", task: "do that thing"})

Check.equal(todo1.get("task"), "do that thing", "get task")
todo1.set("task", "make some java")
Check.equal(todo1.get("task"), "make some java", "get task after set")

Check.equal(todo1.get("done"), false, "get done")
todo1.invoke("complete")
Check.equal(todo1.get("done"), true, "get done after invoke")

Check.tru(instance-of(todo1, Todo), "instance-of")

