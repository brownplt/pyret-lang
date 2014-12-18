#lang pyret

fun sealable(obj):
  obj.{
    seal(self, names-to-expose):
      for list.fold(self-sealed from {}, name from names-to-expose):
        self-sealed.{ [name]: self.[name] }
      end
    end
  }
where:
  todo1 = {
    due: "25 January 2012",
    task: "Write mixin examples",
    done: false,
    complete(self): self.{ done: true } end
  }

  todo2 = sealable(todo1)
  todo3 = todo2.seal(["done", "complete"])
  checkers.check-equals("done is present after seal", todo3.done, false)
  checkers.check-equals("complete is present after seal", todo3.complete().done, true)
  todo2.task raises-satisfies error.is-field-not-found
end
