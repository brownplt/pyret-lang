#lang pyret

#import "../test.arr" as Test

provide { seal: seal, unseal: unseal } end

var todo1: {
  due: "25 January 2012",
  task: "Write mixin examples",
  done: false,
  complete(self): self.{ done: true }
}

fun copy(from, to, names):
  names.foldr(\name, to: (
    cond:
      | builtins.has-field(from, name) =>
          to.{ [name]: from.[name] }
      | else => to
    end
  ), to)
end

fun seal(obj, names-to-expose):
  copy(obj, {}, names-to-expose)
end

fun unseal(sealed, orig, names-to-expose):
  copy(sealed, orig, names-to-expose)
end

fun seal-kit(obj, names):
  {
    obj: seal(obj, names),
    unseal(_, sealed): unseal(sealed, obj, names),
  }
end

#var names: ["done", "complete", "task"]
#var todo-sealed: seal(todo1, names)

#Test.check-equals("field after seal", \(todo-sealed.done), false)
#Test.check-equals("method after seal", \(todo-sealed.complete().done), true)
#Test.check-exn("dropped field after seal", \(todo-sealed.due), "get-field: ")

#var todo-unsealed: unseal(todo-sealed.complete(), todo1, names)
#Test.check-equals("field is overwritten by unseal", \(todo-unsealed.done), true)

#Test.check-equals("unseal and seal compose to identity",
#  \(unseal(seal(todo1, names), todo1, names)),
#  todo1)

#Test.format-results()
