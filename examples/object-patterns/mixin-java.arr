#lang pyret

import "check.arr" as Check

provide {
  class: class,
  Object: Object
} end

#fun drop-fields(obj, names):
#  builtins.keys(obj).foldr(fun(name, filtered-obj):
#    case:
#      | names.member(name) => filtered-obj
#      | else => filtered-obj.{ [name]: obj[name] }
#    end
#  end, {})
#end

# The base object for all hierarchies
object-brander = brander()
Object = object-brander.brand({
  #_brander: brander(),
  new(self, spec): object-brander.brand({
    get(_, name): raise("get: field not found: ".append(name)) end,
    set(_, name, v): raise("set: field not found: ".append(name)) end,
    invoke(_, name, a): raise("invoke: method not found: ".append(name)) end,
    instance-of(_, class): object-brander.check(class) end,
    view-as(inst, class):
      case:
        | object-brander.check(class) => inst
        | else => raise("Incompatible cast in view-as")
      end
    end
  }) end,
  ext(self, ext-descr): ext(self, ext-descr) end,
})

# : Class -> ClassDescription -> Class
fun ext(parent-class, description):
  class-brander = brander()
  class-brander.brand({

    # : (Class) -> Object -> Instance
    new(self, spec): 
      var fields = description.fields
      methods = description.methods
      var parent-inst = nothing # to be init'd by super from constructor

      instance = {

        # : (Instance) -> String -> Any
        get(_, name):
          case:
            | builtins.has-field(fields, name) => fields.[name]
            | else => parent-inst.get(name)
          end
        end,

        # : (Instance) -> String -> Any -> Any
        set(_, name, val):
          case:
            | builtins.has-field(fields, name) => 
                fields := fields.{ [name]: val }
            | else => parent-inst.set(name, val)
          end
        end,

        # : (Instance) -> String -> Any -> Any
        # For now, only support one arg methods
        invoke(inst, name, arg):

          inst-with-super = inst.{
            super(inst, arg):
              parent-inst:invoke._fun()(inst.view-as(parent-class), name, arg)
            end
          }

          case:
            | builtins.has-field(methods, name) =>
              methods:[name]._fun()(inst-with-super, arg)
            | else =>
              parent-inst:invoke._fun()(inst.view-as(parent-class), name, arg)
          end
        end,

        # : (Instance) -> Class -> Bool
        instance-of(_, class):
          class-brander.check(class).or(parent-inst.instance-of(class))
        end,
        
        view-as(inst, class):
          case:
            | class-brander.check(class) => inst
            | else => parent-inst:view-as._fun()(inst.{
                get: parent-inst:get,
                set: parent-inst:set,
                invoke(_, name, arg):
                  inst.invoke(name, arg)
                end
              }, class)
          end
        end
      }

      inst-with-super = instance.{
        super(inst, spec):
          parent-inst := parent-class.new(spec)
          inst
        end
      }

      inst-constructed = description:constructor._fun()(inst-with-super, spec)
      #drop-fields(inst-constructed, ["super"])
      inst-constructed
    end,

    # : (Class) -> ClassDescription -> Class
    ext(self, ext-descr): ext(self, ext-descr) end,
  })
end

# Don't really need this...
fun class(description): Object.ext(description) end





# Tests

todo-class-descr = {
  fields: {
    due: "String",
    task: "String",
    done: "Boolean"
  },
  methods: {
    is-completed(self, _): self.get("done") end,
    complete(self, _):
      self.set("done", true) end
  },
  # Constructor should return an object to use as self
  # : (Instance) -> Object -> Instance
  constructor(self, spec):
    self.set("due", spec.due)
    self.set("task", spec.task)
    self.set("done", false)
    self.super(spec)
  end
}

assignee-ext-descr = {
  fields: {
    assignee: "String"
  },
  methods: {

    assign(self, person):
      case:
        | self.get("done") => raise("Can't assign a completed task")
        | else => self.set("assignee", person)
      end
    end,

    complete(self, _):
      case:
        | is-nothing(self.get("assignee")) =>
            raise("Can't complete an unassigned task")
        | else => self.super(_)
      end
    end
  },
  constructor(self, spec):
    self.set("assignee", nothing)
    self.super(spec)
  end
}

Todo = class(todo-class-descr)
todo1 = Todo.new({ due: "Feb 2", task: "do that thing"})

Check.equal(todo1.get("task"), "do that thing", "get task")
todo1.set("task", "make some java")
Check.equal(todo1.get("task"), "make some java", "get task after set")

Check.equal(todo1.get("done"), false, "get done")
todo1.invoke("complete", nothing)
Check.equal(todo1.get("done"), true, "get done after invoke")

Check.tru(todo1.instance-of(Todo), "instance-of")

AssignableTodo = Todo.ext(assignee-ext-descr)
todo2 = AssignableTodo.new({ due: "Feb 8", task: "assign someone" })

Check.nothin(todo2.get("assignee"), "get child field")
Check.equal(todo2.get("due"), "Feb 8", "get parent field")

todo2.set("assignee", "Joe")
Check.equal(todo2.get("assignee"), "Joe", "set child field")
todo2.set("due", "Feb 9")
Check.equal(todo2.get("due"), "Feb 9", "set parent field")

Check.tru(todo2.instance-of(AssignableTodo), "instance-of child")
Check.tru(todo2.instance-of(Todo), "instance-of parent")
Check.tru(todo2.instance-of(Object), "instance-of Object")

todo2.invoke("assign", "Jonah")
Check.equal(todo2.get("assignee"), "Jonah", "invoke child method")
todo2.invoke("is-completed", nothing)
Check.fals(todo2.get("done"), "invoke parent method")

todo2.invoke("complete", nothing)
Check.tru(todo2.get("done"), "invoke overridden method")

