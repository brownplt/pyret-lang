provide *
provide-types *

import either as E
import pprint as PP
import srcloc as L
import string-dict as S
import ast as A
import "compiler/resolve-scope.arr" as R

fun break-if-needed(items):
  for fold(acc from PP.mt-doc, item from items.reverse()):
    if is-comment(item): item.tosource() + acc
    else if is-at-comment(item): item.tosource() + acc
    else if PP.is-mt-doc(acc): item.tosource()
    else: item.tosource() + PP.sbreak(1) + acc
    end
  end
end
data SExp:
  | snothing with: tosource(self): PP.mt-doc end
  | quote(e :: SExp) with: tosource(self): PP.str("'") + self.e.tosource() end
  | hashlang(str :: String) with: tosource(self): PP.str("#lang " + self.str) end
  | at-app(name :: String, kids :: List<SExp>) with:
    tosource(self):
      PP.str("@") + PP.parens(break-if-needed(leaf(self.name) ^ link(_, self.kids)))
    end
  | leaf(val :: String) with: tosource(self): PP.str(self.val) end
  | sexp(name :: String, kids :: List<SExp>) with:
    tosource(self):
      kids = break-if-needed(leaf(self.name) ^ link(_, self.kids))
      PP.parens(PP.nest(2, kids))
    end
  | comment(msg :: String) with: tosource(self): PP.str(";; " + self.msg) + PP.hardline end
  | at-comment(msg :: String) with: tosource(self): PP.str("@; " + self.msg) + PP.hardline end
  | toplevel(kids :: List<SExp>) with:
    tosource(self): break-if-needed(self.kids) end
  | slist(kids :: List<SExp>) with:
    tosource(self):
      PP.parens(break-if-needed(self.kids))
    end
  | hash-key(name :: String, val :: SExp) with:
    tosource(self):
      PP.group(PP.str("#:" + self.name) + PP.sbreak(1) + self.val.tosource())
    end
  | at-exp(name :: String, raw :: Option<List<SExp>>, processed :: Option<List<SExp>>) with:
    tosource(self):
      PP.str("@" + self.name) +
      cases(Option) self.raw:
        | none => PP.mt-doc
        | some(raw) => PP.brackets(PP.nest(2, PP.sbreak(0) + break-if-needed(raw)) + PP.sbreak(0))
      end +
      cases(Option) self.processed:
        | none => PP.mt-doc
        | some(kids) => PP.braces(PP.nest(2, PP.sbreak(0) + break-if-needed(kids)) + PP.sbreak(0))
      end
    end
end

fun spair(name, val):
  if SExp(val): sexp(name, [list: val])
  else: sexp(name, [list: leaf(tostring(val))])
  end
end


fun trim-path(path):
  var ret = path
  prefixes = [S.immutable-string-dict: 
      "trove", "",
      "js", "js/",
      "base", "",
      "compiler", "compiler/"
    ]
  ret := string-replace(ret, "\\", "/")
  for each(prefix from prefixes.keys().to-list()):
    i = string-index-of(ret, prefix + "/")
    when i >= 0:
      ret := string-replace(string-substring(ret, i, string-length(ret)), prefix + "/", prefixes.get-value(prefix))
      if string-index-of(prefixes.get-value(prefix), "/") >= 0:
        ret := '"' + ret + '"'
      else:
        ret := string-replace(ret, ".arr", "")
      end
    end
  end
  ret
end
fun relative-dir(path):
  prefixes = [S.immutable-string-dict:
      "trove", "../../",
      "js", "../../",
      "base", "../../",
      "compiler", "../../../"
    ]
  for fold(acc from "", prefix from prefixes.keys().to-list()):
    if string-index-of(path, prefix + "/") >= 0: prefixes.get-value(prefix)
    else: acc
    end
  end
end

data CrossRef:
  | crossref(modname :: String, field :: String) with:
    tosource(self): PP.str(torepr(self)) end
end

fun xref(modname, as-name):
  sexp("xref", [list: leaf(torepr(modname)), leaf(torepr(as-name))])
end


fun lookup-value(value, bindings):
  fun help(seen, item):
    to-examine =
      if A.Name(item):
        if seen.member(item): none
        else if bindings.has-key-now(item.key()):
          # print("Found " + item.key() + " in bindings")
          bindings.get-value-now(item.key()).expr
        else: none
        end
      else if A.Expr(item): some(item)
      else: none
      end
    cases(Option) to-examine:
      | none => value
      | some(v) =>
        # print("Further processing on " + v.tosource().pretty(80).first)
        cases(A.Expr) v:
          | s-import(_, _, _) => v
          | s-import-types(_, _, _, _) => v
          | s-import-fields(_, _, _) => v
          | s-id(_, id) => help(item ^ link(_, seen), id)
          | s-id-letrec(_, id, _) => help(item ^ link(_, seen), id)
          | s-id-var(_, id) => help(item ^ link(_, seen), id)
          | s-type(_, id, ann) => help(item ^ link(_, seen), id)
          | s-newtype(_, id, _) => help(item ^ link(_, seen), id)
          | s-block(_, stmts) => help(seen, stmts.last())
          | s-user-block(_, body) => help(seen, body)
          | s-let-expr(_, _, body) => help(seen, body)
          | s-letrec(_, _, body) => help(seen, body)
          | s-type-let-expr(_, _, body) => help(seen, body)
          | s-dot(_, obj, field) =>
            help-obj = help(seen, obj)
            # print("Looking for " + field + " on " + help-obj.tosource().pretty(80).join-str("\n"))
            cases(A.Expr) help-obj:
              | s-import(_, file, _) => crossref(file.tosource().pretty(1000).first, field)
              | s-import-types(_, file, _, _) => crossref(file.tosource().pretty(1000).first, field)
              | s-import-fields(_, _, file) => crossref(file.tosource().pretty(1000).first, field)
              | s-obj(_, obj-fields) =>
                cases(Option) obj-fields.find(lam(f): f.name == field end):
                  | none => v
                  | some(new-v) => help(seen, new-v)
                end
              | s-data-expr(_, name, _, _, _, variants, shared-members, _) =>
                if (name == field):
                  # print("Found " + field + " as a data-expr itself")
                  help-obj
                else:
                  cases(Option) variants.find(lam(f): f.name == field end):
                    | some(new-v) => new-v
                    | none =>
                      cases(Option) variants.find(lam(f): A.make-checker-name(f.name) == field end):
                        | some(new-v) =>
                          a-an =
                            if string-index-of("aeiou", string-substring(new-v.name, 0, 1)) >= 0: "an "
                            else: "a "
                            end
                          A.s-lam(new-v.l, empty,
                            [list: A.s-bind(new-v.l, false, A.s-name(new-v.l, "val"), A.a-any)],
                            A.a-name(new-v.l, A.s-global("Boolean")),
                            "Checks whether the provided argument is in fact " + a-an + new-v.name,
                            A.s-undefined(new-v.l), none)
                        | none =>
                          cases(Option) shared-members.find(lam(f): f.name == field end):
                            | some(new-v) => new-v
                            | none => v
                          end
                      end
                  end
                end
              | else => v
            end
          | else => v
        end
    end
  end
  help([list: ], value)
end


      # cases(A.Name) name:
      #   | s-global(_) =>
      #     sexp("a-id", [list: leaf(torepr(name.toname())), xref("<global>", name.toname())])
      #   | s-type-global(_) =>
      #     sexp("a-id", [list: leaf(torepr(name.toname())), xref("<global>", name.toname())])
      #   | else =>
      #     if fields.cross-refs.has-key(name.toname()):
      #       cases(CrossRef) fields.cross-refs.get(name.toname()):
      #         | crossref(modname, as-name) =>
      #           sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
      #       end
      #     else if bindings.has-key(name.key()):# and (bindings.get(name.toname()) == name.toname()):
      #       val = lookup-value(name, bindings)
      #       cases(Any) val:
      #         | crossref(modname, as-name) =>
      #           sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
      #         | s-data-expr(_, data-name, _, _, _, _, _, _) =>
      #           sexp("a-id", [list: leaf(torepr(name.toname())), xref(trim-path(file), data-name)])
      #         | else =>
      #           if (val == name):
      #             leaf(torepr(name.toname()))
      #           else:
      #             print("We found " + torepr(val))
      #             leaf(torepr(name.toname()))
      #           end
      #       end
      #     else if bindings.has-key(name.toname()):# and (bindings.get(name.toname()) == name.toname()):
      #       leaf(torepr(name.toname()))
      #     else if fields.data-vals.has-key(name.toname()):
      #       cases(Any) fields.data-vals.get(name.toname()):
      #         | crossref(modname, as-name) =>
      #           sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
      #         | else =>
      #           print("Found " + torepr(fields.data-vals.get(name.toname())))
      #           leaf(torepr(name.toname()))
      #       end
      #     else:



fun process-ann(ann, file, fields, bindings, type-bindings):
  cases(A.Ann) ann:
    | a-type-var(_, n) => leaf(torepr(n.toname()))
    | a-name(l, name) =>
      looked-up = lookup-ann(ann, type-bindings)
      cases(E.Either<A.Ann, CrossRef>) looked-up:
        | right(v) =>
          cases(CrossRef) v:
            | crossref(modname, as-name) =>
              sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
          end
        | left(v) =>
          cases(A.Ann) v:
            | a-name(_, name2) =>
              cases(A.Name) name2:
                | s-global(_) =>
                  sexp("a-id", [list: leaf(torepr(name.toname())), xref("<global>", name.toname())])
                | s-type-global(_) =>
                  sexp("a-id", [list: leaf(torepr(name.toname())), xref("<global>", name.toname())])
                | else =>
                  if fields.cross-refs.has-key(name.toname()):
                    cases(CrossRef) fields.cross-refs.get-value(name.toname()):
                      | crossref(modname, as-name) =>
                        sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
                    end
                  else if bindings.has-key-now(name.key()):# and (bindings.get(name.toname()) == name.toname()):
                    val = lookup-value(name, bindings)
                    cases(Any) val:
                      | crossref(modname, as-name) =>
                        sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
                      | s-data-expr(_, data-name, _, _, _, _, _, _) =>
                        sexp("a-id", [list: leaf(torepr(name.toname())), xref(trim-path(file), data-name)])
                      | else =>
                        if (val == name):
                          leaf(torepr(name.toname()))
                        else:
                          print("We found " + torepr(val))
                          leaf(torepr(name.toname()))
                        end
                    end
                  else if bindings.has-key-now(name.toname()):# and (bindings.get(name.toname()) == name.toname()):
                    leaf(torepr(name.toname()))
                  else if fields.data-vals.has-key(name.toname()):
                    cases(Any) fields.data-vals.get-value(name.toname()):
                      | crossref(modname, as-name) =>
                        sexp("a-id", [list: leaf(torepr(name.toname())), xref(modname, as-name)])
                      | else =>
                        print("Found " + torepr(fields.data-vals.get-value(name.toname())))
                        leaf(torepr(name.toname()))
                    end
                  else:
                    # print("Looking for " + torepr(name) + " in " + torepr(fields.cross-refs.keys()))
                    # print("Looking for " + torepr(name) + " in " + torepr(bindings.keys()))
                    print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Got " + torepr(name) + " ==> " + torepr(lookup-ann(ann, type-bindings).v) + " at " + tostring(l) + " and no crossref for it")
                    leaf(torepr(ann.tosource().pretty(1000).first))
                  end
              end
            | else => process-ann(v, file, fields, bindings, type-bindings)
          end
        | a-record(_, _) => leaf("Unexpected a-record: " + torepr(looked-up))
        | else => raise("Expected a crossref or a name, but got " + torepr(looked-up))
      end
    | a-app(l, base, args) =>
      sexp("a-app", process-ann(base, file, fields, bindings, type-bindings) ^ link(_, args.map(process-ann(_, file, fields, bindings, type-bindings))))
    | a-dot(l, obj, field) =>
      bound-as-import =
        if type-bindings.has-key-now(obj.key()): type-bindings.get-value-now(obj.key()).ann.value
        else: lookup-value(obj, bindings)
        end      
      if A.Import(bound-as-import):
        sexp("a-compound", [list: sexp("a-dot", [list: leaf(torepr(obj.toname())), leaf(torepr(field))]),
            xref(bound-as-import.file.tosource().pretty(1000).first, field)])
      else:
        sexp("a-dot", [list: process-ann(obj, file, fields, bindings, type-bindings), leaf(torepr(field))])
      end
    | a-arrow(l, args, ret, _) =>
      sexp("a-arrow", args.map(process-ann(_, file, fields, bindings, type-bindings)) + [list: process-ann(ret, file, fields, bindings, type-bindings)])
    | a-record(l, rec-fields) =>
      sexp("a-record", rec-fields.map(lam(f):
          sexp("a-field", [list: leaf(torepr(f.name)), process-ann(f.ann, file, fields, bindings, type-bindings)]) end))
    | else => leaf(torepr(ann.tosource().pretty(1000).first))
  end
end
fun tosource(val):
  if is-string(val): PP.str(val)
  else: val.tosource()
  end
end

fun lookup-ann(ann :: A.Ann, type-bindings :: S.StringDict) -> E.Either<A.Ann, CrossRef>:
  cases(A.Ann) ann:
    | a-dot(_, mod, name) =>
      cases(R.TypeBinding) type-bindings.get-value-now(mod.key()):
        | let-type-bind(_, _, opt-ann) =>
          cases(Option) opt-ann:
            | none =>
              E.left(ann)
            | some(new-ann) =>
              if A.is-Import(new-ann):
                E.right(crossref(new-ann.file.tosource().pretty(200).first, name))
              else:
                lookup-ann(new-ann, type-bindings)
              end
          end
        | global-type-bind(_, _, opt-ann) =>
          cases(Option) opt-ann:
            | none => E.right(crossref("<global>", ann.tosource().pretty(200).first))
            | some(new-ann) => E.right(crossref("<global>", new-ann.tosource().pretty(200).first))
          end
        | else =>
          print("Don't know what to do with " + torepr(ann))
          E.left(ann)
      end
    | a-name(_, id) =>
      if type-bindings.has-key-now(id.key()):
        cases(R.TypeBinding) type-bindings.get-value-now(id.key()):
          | let-type-bind(_, _, opt-ann) =>
            cases(Option) opt-ann:
              | none => E.left(ann)
              | some(new-ann) =>
                if A.is-Import(new-ann): E.left(ann)
                else: lookup-ann(new-ann, type-bindings)
                end
            end
          | global-type-bind(_, _, opt-ann) =>
            cases(Option) opt-ann:
              | none => E.right(crossref("<global>", ann.tosource().pretty(200).first))
              | some(new-ann) => E.right(crossref("<global>", new-ann.tosource().pretty(200).first))
            end
          | else =>
            print("Don't know what to do with " + torepr(ann))
            E.left(ann)
        end
      else:
        cases(A.Name) id:
          | s-type-global(name) => E.right(crossref("<global>", name))
          | else => E.left(ann)
        end
      end
    | else => E.left(ann)
  end
end

fun process-fields(module-name, fields, types, bindings, type-bindings):
  var looked-up-vals = S.make-immutable-string-dict()
  var looked-up-typs = S.make-immutable-string-dict()
  for each(field from fields):
    field-name = field.name
    value = lookup-value(field.value, bindings)
    # print("Binding " + torepr(field-name) + " to " + tosource(value).pretty(80).first)
    looked-up-vals := looked-up-vals.set(field-name, value)
  end
  for each(typ from type-bindings.keys-now().to-list()):
    cases(R.TypeBinding) type-bindings.get-value-now(typ):
      | let-type-bind(_, name, opt-ann) =>
        cases(Option) opt-ann:
          | none => nothing
          | some(ann) =>
            if A.is-Import(ann):
              looked-up-typs := looked-up-typs.set(typ, ann)
            else:
              looked-up-typs := looked-up-typs.set(typ, lookup-ann(ann, type-bindings).v)
            end
        end
      | global-type-bind(_, name, ann) =>
        looked-up-typs := looked-up-typs.set(typ, crossref("<global>", ann.tosource().pretty(200).first))
      | else => nothing
    end
  end
  for each(typ from types):
    new-typ = lookup-ann(typ.ann, type-bindings).v
    looked-up-typs := looked-up-typs.set(typ.name, new-typ)
  end
  var data-vals = S.make-immutable-string-dict()
  var fun-vals = S.make-immutable-string-dict()
  var ignored-vals = S.make-immutable-string-dict()
  var unknown-vals = S.make-immutable-string-dict()
  var imports = S.make-immutable-string-dict()
  var cross-refs = S.make-immutable-string-dict()
  var constructors = S.make-immutable-string-dict()
  for each(field from looked-up-typs.keys().to-list()):
    typ = looked-up-typs.get-value(field)
    cases(A.Ann) typ:
      | crossref(_, _) =>
        cross-refs := cross-refs.set(field, typ)
      | else =>
        cross-refs := cross-refs.set(field, crossref(module-name, field))
    end
  end
  for each(field from looked-up-vals.keys().to-list()):
    value = looked-up-vals.get-value(field)
    cases(A.Expr) value:
      | crossref(_, _) =>
        imports := imports.set(field, value)
        cross-refs := cross-refs.set(field, value)
      | s-lam(_, _, _, _, _, _, _) =>
        if (string-index-of(field, "is-") == 0):
          ignored-vals := ignored-vals.set(field, value)
          cross-refs := cross-refs.set(field,
            crossref(module-name, string-substring(field, 3, string-length(field))))
        else:
          fun-vals := fun-vals.set(field, value)
          cross-refs := cross-refs.set(field, crossref(module-name, field))
        end
      | s-variant(_, _, _, _, _) =>
        cross-refs := cross-refs.set(field, crossref(module-name, field))
      | s-singleton-variant(_, _, _) =>
        cross-refs := cross-refs.set(field, crossref(module-name, field))
      | s-data-expr(_, data-name, _, _, _, variants, _, _) =>
        for each(variant from variants):
          constructors := constructors.set(variant.name, field)
        end
        data-vals := data-vals.set(field, value)
        cross-refs := cross-refs.set(field, crossref(module-name, field))
      | else =>
        unknown-vals := unknown-vals.set(field, value)
        cross-refs := cross-refs.set(field, crossref(module-name, field))
    end
  end
  { data-vals: data-vals,
    constructors: constructors,
    fun-vals: fun-vals,
    ignored-vals: ignored-vals,
    unknown-vals: unknown-vals,
    imports: imports,
    cross-refs: cross-refs
  }
end
