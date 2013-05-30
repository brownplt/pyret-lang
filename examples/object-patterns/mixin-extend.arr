#lang pyret

import "check.arr" as Check

todo1 = {
  due: "25 January 2012",
  task: "Write mixin examples",
  done: false,
}

fun extendable(obj):
  obj.{
    ext(self, obj2):
      for list.fold(self-ext from self, name from builtins.keys(obj2)):
        self-ext.{ [name]: obj2.[name] }
      end
    end
  }
end

todo2 = extendable(todo1)
todo3 = todo2.ext({ complete(self): self.{ done: true } end })
Check.equal(todo3.done, false, "done is present after extend")
Check.equal(todo3.complete().done, true, "complete is present after extend")
