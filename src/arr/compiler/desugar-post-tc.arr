#lang pyret

provide *
provide-types *
import ast as A
import string-dict as SD
import file("list-aux.arr") as LA
import file("desugar.arr") as D
import file("compile-structs.arr") as C

names = A.global-names

no-branches-exn = D.no-branches-exn
is-s-method = A.is-s-method
flat-prim-app = A.prim-app-info-c(false)

fun no-cases-exn(l, val):
  A.s-prim-app(l, "throwNoCasesMatched", [list: A.s-srcloc(l, l), val], flat-prim-app)
end

desugar-visitor = A.default-map-visitor.{
  method s-cases-else(self, l, typ, val, branches, els, blocky):
    name = A.global-names.make-atom("cases")
    typ-compiled = typ.visit(self)
    val-exp = val.visit(self)
    val-id = A.s-id(l, name)
    A.s-let-expr(l, [list: A.s-let-bind(l, A.s-bind(l, false, name, typ-compiled), val-exp)],
      A.s-cases-else(l, A.a-blank, val-id, branches.map(_.visit(self)),
        els.visit(self), true), false)
  end,
  method s-cases(self, l, typ, val, branches, blocky):
    name = A.global-names.make-atom("cases")
    typ-compiled = typ.visit(self)
    val-exp = val.visit(self)
    val-id = A.s-id(l, name)
    A.s-let-expr(l, [list: A.s-let-bind(l, A.s-bind(l, false, name, typ-compiled), val-exp)],
      A.s-cases-else(l, A.a-blank, val-id, branches.map(_.visit(self)),
        A.s-block(l, [list: no-cases-exn(l, val-id)]), true), false)
  end,
  method s-check(self, l, name, body, keyword-check):
    A.s-id(l, A.s-global("nothing"))
  end
}

fun no-method-exn(l, obj, name):
  A.s-prim-app(l, "throwFieldNotFound", [list: A.s-srcloc(l, l), obj, name], flat-prim-app)
end

var generated-binds = SD.make-mutable-string-dict()
fun merge-methods(program :: A.Program):
  doc: ```
       Tries to merge methods on data definitions where possible
       Preconditions on program:
         - well-formed
         - has been type-checked
         - contains no s-data
       Requirements:
         - all variants have method of the same name
         - ... with the same arity,
         - ... and same argument annotations
       ```
  cases(A.Program) program block:
    | s-program(l, _provide, provided-types, imports, body) =>
      generated-binds := SD.make-mutable-string-dict()
      { ast: A.s-program(l, _provide, provided-types, imports,
            if false: body
            else:
            body.visit(A.default-map-visitor.{
                method s-data-expr(self, shadow l, name, name-type, name-ann, params, mixins, variants, shared, _check-loc, _check):
                  merge-data-methods(l, name, name-type, name-ann, params, mixins, variants, shared, _check-loc, _check)
                end
                })
            end),
        new-binds: generated-binds }
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end
fun same-ann(a1 :: A.Ann, a2 :: A.Ann) -> Boolean:
  a1.visit(A.dummy-loc-visitor) == a2.visit(A.dummy-loc-visitor)
end
fun same-sig(s1 :: A.Expr%(is-s-method), s2 :: A.Expr%(is-s-method)):
  (s1.params.length() == s2.params.length())
  and for LA.all2-strict(a1 from s1.args, a2 from s2.args):
    same-ann(a1.ann, a2.ann)
  end
  and same-ann(s1.ann, s2.ann)
end
fun mk-id-ann(loc, base, ann) block:
  a = names.make-atom(base)
  generated-binds.set-now(a.key(), C.value-bind(C.bo-local(loc), C.vb-let, a, ann))
  { id: a, id-b: A.s-bind(loc, false, a, ann), id-e: A.s-id(loc, a) }
end
fun make-renamer():
  renames = SD.make-mutable-string-dict()
  fields = SD.make-mutable-string-dict()
  { renames;
    fields;
    A.default-map-visitor.{
      method s-atom(self, base, serial):
        n = A.s-atom(base, serial)
        renames.get-now(n.key()).or-else(n)
      end,
      method s-dot(self, l, obj, field) block:
        cases(A.Expr) obj block:
          | s-id(_, name) =>
            # print("Trying to replace " + name.key() + "\n")
            cases(Option<SD.MutableStringDict<A.Expr>>) fields.get-now(name.key()) block:
              | some(field-ids) =>
                cases(Option<A.Expr>) field-ids.get-now(field) block:
                  | some(id) =>
                    # print("Replacing " + name.key() + "." + field + " with " + id.id.key() + "\n")
                    id
                  | none =>
                    # print("Couldn't find " + field + " in " + name.key() + ", so recurring\n")
                    # print(torepr(obj) + "\n")
                    # print(torepr(obj.visit(self)) + "\n")
                    # print(torepr(renames.keys-now()) + "\n")
                    A.s-dot(l, obj.visit(self), field)
                end
              | none =>
                # print("Couldn't find " + name.key() + " at all, so recurring\n")
                A.s-dot(l, obj.visit(self), field)
            end
          | else =>
            # print("Wasn't a simple s-dot: " + obj.tosource().pretty(10000).first + "." + field + "\n")
            A.s-dot(l, obj.visit(self), field)
        end
      end
    }
  }
end
fun merge-data-methods(l, name, name-type, name-ann, params, mixins, variants, shared, _check-loc, _check) block:
  # print("Merging for " + name + "\n")
  shared-names = SD.make-mutable-string-dict()
  for each(s from shared) block:
    shared-names.set-now(s.name, true)
  end
  method-sigs = SD.make-mutable-string-dict()
  method-bodies = SD.make-mutable-string-dict()
  {renames; fields; renamer} = make-renamer()
  variants-map = SD.make-mutable-string-dict()
  needed = variants.length()
  for each(v from variants) block:
    variants-map.set-now(v.name, v)
    for each(w from v.with-members) block:
      when A.is-s-data-field(w) and A.is-s-method(w.value) block:
        when not(method-sigs.has-key-now(w.name)) block:
          method-sigs.set-now(w.name, w.value)
          method-bodies.set-now(w.name, SD.make-mutable-string-dict())
        end
        when same-sig(w.value, method-sigs.get-value-now(w.name)) and not(shared-names.has-key-now(w.name)) block:
          #print("Candidate for merging type " + name + " : variant " + v.name + " : method " + w.name + "\n")
          method-bodies.get-value-now(w.name).set-now(v.name, w.value)
        end
      end
    end
  end
  shared-methods = SD.make-mutable-string-dict()
  for SD.each-key-now(m from method-bodies):
    sig = method-sigs.get-value-now(m)
    bodies = method-bodies.get-value-now(m)
    when bodies.count-now() == needed block:
      # print("Merging type " + name + " : method " + m + "\n")
      new-params = sig.params.map(lam(n): names.make-atom(n.toname()) end)
      new-args = sig.args.map(lam(b): mk-id-ann(l, b.id.toname(), b.ann) end)
      case-bodies = for SD.map-keys-now(vname from bodies) block:
        vmeth = bodies.get-value-now(vname)
        for each2(a from vmeth.args, na from new-args):
          renames.set-now(a.id.key(), na.id)
        end
        cases(A.Variant) variants-map.get-value-now(vname) block:
          | s-variant(lv, constr-loc, _, members, with-members) =>
            field-exps = SD.make-mutable-string-dict()
            # print("First arg name for " + vname + " is " + vmeth.args.first.id.key() + "\n")
            fields.set-now(vmeth.args.first.id.key(), field-exps)
            arg-fields = members.map(lam(member):
                cases(A.VariantMember) member block:
                  | s-variant-member(mloc, mt, b) =>
                    new-b = mk-id-ann(mloc, b.id.toname(), A.a-blank)
                    field-exps.set-now(b.id.toname(), new-b.id-e)
                    cases(A.VariantMemberType) mt:
                      | s-normal => A.s-cases-bind(mloc, A.s-cases-bind-normal, new-b.id-b)
                      | s-mutable => A.s-cases-bind(mloc, A.s-cases-bind-ref, new-b.id-b)
                    end
                end
              end)
            A.s-cases-branch(vmeth.l, constr-loc, vname, arg-fields, vmeth.body.visit(renamer))
          | s-singleton-variant(lv, _, with-members) =>
            A.s-singleton-cases-branch(vmeth.l, lv, vname, vmeth.body.visit(renamer))
        end
      end
      else-case = no-method-exn(l, new-args.first.id-e, A.s-str(l, m))
      ann-name = A.a-name(l, name-ann)
      shared-method = A.s-data-field(l, m, A.s-method(l, m, new-params, new-args.map(_.id-b), sig.ann, "",
        A.s-cases-else(l, ann-name, new-args.first.id-e, case-bodies, else-case, true),
        none, none, true))
      shared-methods.set-now(m, shared-method)
    end
  end
  new-variants = for map(v from variants):
    cases(A.Variant) v block:
      | s-variant(lv, constr-loc, vname, members, with-members) =>
        A.s-variant(lv, constr-loc, vname, members,
          with-members.filter(lam(m): not(shared-methods.has-key-now(m.name)) end))
      | s-singleton-variant(lv, vname, with-members) =>
        A.s-singleton-variant(lv, vname, with-members.filter(lam(m): not(shared-methods.has-key-now(m.name)) end))
    end
  end
  A.s-data-expr(l, name, name-type, name-ann, params, mixins,
    new-variants,
    shared-methods.map-keys-now(lam(mname): shared-methods.get-value-now(mname) end) + shared,
    _check-loc, _check)
end

fun desugar-post-tc(program :: A.Program, compile-env :: C.CompileEnvironment):
  doc: ```
        Desugar non-scope and non-check based constructs.
        Preconditions on program:
          - well-formed
          - has been type-checked
          - contains no s-var, s-fun, s-data, s-check, or s-check-test
          - contains no s-provide in headers
          - all where blocks are none
          - contains no s-name (e.g. call resolve-names first)
          - contains no s-for, s-if, s-op, s-method-field,
                        s-not, s-when, s-if-pipe, s-paren
          - contains no s-underscore in expression position (but it may
            appear in binding positions as in s-let-bind, s-letrec-bind)
        Postconditions on program:
          - in addition to preconditions,
            contains no s-cases, s-cases-else, s-instantiate
        ```
  cases(A.Program) program:
    | s-program(l, _provide, provided-types, imports, body) =>
      A.s-program(l, _provide, provided-types, imports, body.visit(desugar-visitor))
    | else => raise("Attempt to desugar non-program: " + torepr(program))
  end
end
