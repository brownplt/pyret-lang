#lang pyret

# Examples as a user
var todo1: {
  due: "25 January 2012",
  task: "Write mixin examples",
  done: false
}

fun check(val, expected, message):
  assert(\(val.equals(expected)), message)
end

fun assert(testfun, message):
  var result: testfun()
  cond:
    | result => print(message.append(" passed"))
    | else => print(message.append(" failed"))
  end
end

fun doable(todo):
  todo.{ complete(self): self.{ done : true } end }
end

var todoable1: doable(todo1)
check(true, todoable1.complete().done, "todoable worked")

fun make-mutable(obj):
  var names: builtins.keys(obj)
  fun make-mutable-field(name, new-obj):
    var field-val: obj.[name]
    fun field-method(self, new-val):
      cond:
        | is-nothing(new-val) => field-val
        | else => field-val = new-val      
      end
    end
    new-obj.[name] = field-method._method
  end
  names.foldr(make-mutable-field, {})
end

var mutabletodo1: doable(make-mutable(todo1))
mutabletodo1.complete()
mutabletodo1.done()

