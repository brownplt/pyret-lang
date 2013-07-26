#lang pyret

provide { seal: seal, unseal: unseal } end

todo1 = {
  due: "25 January 2012",
  task: "Write mixin examples",
  done: false,
  complete(self): self.{ done: true } end
}

fun copy(fromobj, to, names):
  for list.fold(toobj from to, name from names):
    if builtins.has-field(fromobj, name):
      to.{ [name]: fromobj.[name] }
    else
      toobj
    end
  end
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
    unseal(_, sealed): unseal(sealed, obj, names) end
  }
end

#names = ["done", "complete", "task"]
#todo-sealed = seal(todo1, names)

#Test.check-equals("field after seal", \(todo-sealed.done), false)
#Test.check-equals("method after seal", \(todo-sealed.complete().done), true)
#Test.check-exn("dropped field after seal", \(todo-sealed.due), "get-field: ")

#todo-unsealed = unseal(todo-sealed.complete(), todo1, names)
#Test.check-equals("field is overwritten by unseal", \(todo-unsealed.done), true)

#Test.check-equals("unseal and seal compose to identity",
#  \(unseal(seal(todo1, names), todo1, names)),
#  todo1)

#Test.format-results()
