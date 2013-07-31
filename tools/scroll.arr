#lang pyret

import ast as A
import file as F
import directory as D

moorings = A.parse(F.file("../src/lang/pyret-lib/moorings.arr").read-file(),
                   "moorings.arr", {["check"]: false}).pre-desugar

fun render-ann-helper(prefix, ann):
  cases(A.Ann) ann:
   | a_name(_, id) => prefix + id
   | else => ""
   # | a_blank
   # | a_any
   # | a_arrow(_, args, ret)
   # | a_method(_, args, ret)
   # | a_record(_, fields)
   # | a_app(_, name, args)
   # | a_pred(_, ann, exp)
   # | a_dot(_, obj, field)
  end
end

fun render-ann(ann):
  render-ann-helper(" :: ", ann)
end

fun render-return(ann):
  render-ann-helper(" -> ", ann)
end

fun render-bind(bnd):
  bnd.id + render-ann(bnd.ann)
end

fun render-variant(variant):
  "  " + variant.name + 
  if A.is-s_variant(variant):
     "(" + variant.binds.map(render-bind).join-str(", ") + ")"
  else:
    ""
  end +
  if variant.with_members.length() <> 0:
    ":\n" +
    variant.with_members.map(render-member).join-str("\n")
  else:
    ""
  end
end

fun render-member(m):
  if A.is-s_method_field(m):
    var name = ""
    when A.is-s_str(m.name):
      name := m.name.s
    end
    "    " + name + "(" + m.args.map(render-bind).join-str(", ") + ")" + render-return(m.ann) + ": '" + m.doc + "' end"
  else if A.is-s_id(m.value) and A.is-s_str(m.name):
    m.name.s + ": " + m.value.id
  end
end

fun render-object(o):
  "{" + o.fields.map(render-member).join-str(",\n") + "}"
end

# Just print out some stuff

for list.each(stmt from moorings.imports):
  if A.is-s_provide(stmt) and A.is-s_obj(stmt.block):
    print("provide " + render-object(stmt.block))
  else:
    nothing
  end
end

print("\n")

for list.each(stmt from moorings.block.stmts):
  if A.is-s_fun(stmt):
    print("fun " + (if stmt.params.length() <> 0:
                     "<" + stmt.params.join-str(", ") + ">"
                    else:
                     ""
                    end) + stmt.name + "(" + stmt.args.map(render-bind).join-str(", ") + ")"
        + render-return(stmt.ann) + ":\n  '" + stmt.doc + "'\nend")
    print("\n")
  else if A.is-s_data(stmt):
    print("data " + stmt.name + ":\n" + stmt.variants.map(render-variant).join-str("\n") + "\nend")
    print("\n")
  else if A.is-s_let(stmt) and A.is-s_obj(stmt.value):
    print(render-bind(stmt.name) + " = " + render-object(stmt.value))
  else:
    nothing
  end
end