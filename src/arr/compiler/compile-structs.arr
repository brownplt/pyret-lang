#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL
import error-display as ED
import string-dict as SD
import "compiler/type-structs.arr" as T

type URI = String
type StringDict = SD.StringDict
string-dict = SD.string-dict

type Loc = SL.Srcloc

data PyretDialect:
  | Pyret
  | Bootstrap
end

data Dependency:
  | dependency(protocol :: String, arguments :: List<String>)
    with:
    key(self): self.protocol + "(" + self.arguments.join-str(", ") + ")" end
  | builtin(modname :: String)
    with:
    key(self): "builtin(" + self.modname + ")" end
end

data NameResolution:
  | resolved(
      ast :: A.Program,
      errors :: List<CompileError>,
      bindings :: SD.MutableStringDict,
      type-bindings :: SD.MutableStringDict,
      datatypes :: SD.MutableStringDict)
end


# Used to describe when additional module imports should be added to a
# program.  See wrap-extra-imports
data ExtraImports:
  | extra-imports(imports :: List<ExtraImport>)
end

# Import this module, and bind the given value and type bindings from it
data ExtraImport:
  | extra-import(dependency :: Dependency, as-name :: String, values :: List<String>, types :: List<String>)
end

data CompileEnvironment:
  | compile-env(
        globals :: Globals,
        mods :: StringDict<Provides> # map from dependency key to info provided from module
      )
end

data Globals:
  | globals(values :: StringDict<T.Type>, types :: StringDict<T.Type>)
end

data Provides:
  | provides(
      from-uri :: URI,
      values :: StringDict<T.Type>,
      aliases :: StringDict<T.Type>,
      data-definitions :: StringDict<T.DataType>
    )
end

fun type-from-raw(uri, typ, tyvar-env :: SD.StringDict<T.TypeVariable>):
  tfr = type-from-raw(uri, _, tyvar-env)
  t = typ.tag
  ask:
    | t == "any" then: T.t-top
    | t == "record" then:
      T.t-record(for map(f from typ.fields): T.t-member(f.name, tfr(f.value)) end)
    | t == "name" then:
      modname = if typ.module == "LOCAL": uri else: typ.module end
      T.t-name(some(modname), A.s-type-global(typ.name))
    | t == "tyvar" then:
      cases(Option<T.TypeVariable>) tyvar-env.get(typ.name):
        | none => raise("Unbound type variable " + typ.name + " in provided type.")
        | some(tv) => T.t-var(tv)
      end
    | t == "forall" then:
      new-env = for fold(new-env from tyvar-env, a from typ.args):
        tvn = A.global-names.make-atom(a)
        new-env.set(a, tvn)
      end
      params = for map(k from new-env.keys-list()):
        T.t-variable(A.dummy-loc, new-env.get-value(k), T.t-top, T.invariant)
      end
      T.t-forall(params, type-from-raw(uri, typ.onto, new-env))
    | t == "tyapp" then:
      T.t-app(tfr(typ.onto), map(tfr, typ.args))
    | t == "arrow" then:
      T.t-arrow(map(tfr, typ.args), tfr(typ.ret))
    | otherwise: raise("Unkonwn raw tag for type: " + t)
  end
end

fun tvariant-from-raw(uri, tvariant, env):
  t = tvariant.tag
  ask:
    | t == "variant" then:
      members = for map(tm from tvariant.vmembers):
        # TODO(joe): Exporting ref fields?
        T.t-member(tm.name, type-from-raw(uri, tm.typ, env))
      end
      T.t-variant(A.dummy-loc, tvariant.name, members, empty)
    | t == "singleton-variant" then:
      T.t-singleton-variant(A.dummy-loc, tvariant.name, empty)
    | otherwise: raise("Unkonwn raw tag for variant: " + t)
  end
end

fun datatype-from-raw(uri, datatyp):
  pdict = for fold(pdict from SD.make-string-dict(), a from datatyp.params):
    tvn = A.global-names.make-atom(a)
    pdict.set(a, tvn)
  end
  params = for map(k from pdict.keys-list()):
    T.t-variable(A.dummy-loc, pdict.get-value(k), T.t-top, T.invariant)
  end
  variants = map(tvariant-from-raw(uri, _, pdict), datatyp.variants)
  members = for map(tm from datatyp.methods):
    # TODO(joe): Exporting ref fields?
    T.t-member(tm.name, type-from-raw(uri, tm.value, pdict))
  end
  T.t-datatype(datatyp.name, params, variants, members)
end

fun provides-from-raw-provides(uri, raw):
  values = raw.values
  vdict = for fold(vdict from SD.make-string-dict(), v from raw.values):
    if is-string(v):
      vdict.set(v, T.t-top)
    else:
      vdict.set(v.name, type-from-raw(uri, v.typ, SD.make-string-dict()))
    end
  end
  aliases = raw.aliases
  adict = for fold(adict from SD.make-string-dict(), a from raw.aliases):
    if is-string(a):
      adict.set(a, T.t-top)
    else:
      adict.set(a.name, type-from-raw(uri, a.typ, SD.make-string-dict()))
    end
  end
  datas = raw.datatypes
  ddict = for fold(ddict from SD.make-string-dict(), d from raw.datatypes):
    ddict.set(d.name, datatype-from-raw(uri, d.typ))
  end
  provides(uri, vdict, adict, ddict)
end


data CompileResult<C>:
  | ok(code :: C)
  | err(problems :: List<CompileError>)
end

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end

data CompileError:
  | wf-err(msg :: String, loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        draw-and-highlight(self.loc)]
    end
  | wf-empty-block(loc :: A.Loc) with:
    # semi-counterfactual loc on this error
    render-fancy-reason(self, make-pallet):
      [ED.error:
        [ED.para:
          ED.text("Pyret rejected your program because you have an "),
          ED.highlight(ED.text("empty block"),[list: self.loc], make-pallet(1).get(0)),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        draw-and-highlight(self.loc)]
    end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        ED.v-sequence(self.loc.map(lam(l): [ED.para: draw-and-highlight(l)] end))]
    end
  | reserved-name(loc :: Loc, id :: String) with:
    render-fancy-reason(self, make-pallet):
      [ED.error:
        [ED.para:
          ED.text("Pyret disallows the use of "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.loc], make-pallet(1).get(0))),
          ED.text(" as an identifier because it is reserved.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret disallows the use of "),
          ED.code(ED.text(self.id)),
          ED.text(" as an identifier at "),
          ED.loc(self.loc),
          ED.text(" because it is reserved.")]]
    end
  | zero-fraction(loc, numerator) with:
    render-fancy-reason(self, make-pallet):
      [ED.error:
        [ED.para:
          ED.text("Pyret disallows the fraction literal expression")],
        [ED.para:
          ED.code(ED.highlight([ED.sequence: 
                                  ED.embed(self.numerator),
                                  ED.text(" / 0")], 
                               [list: self.loc], make-pallet(1).get(0)))],
        [ED.para:
          ED.text("because its denominator is zero.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret disallows the fraction literal expression")],
        [ED.para:
          ED.code([ED.sequence: 
                    ED.embed(self.numerator),
                    ED.text(" / 0")])],
        [ED.para:
          ED.text("at "),
          ED.loc(self.loc),
          ED.text(" because its denominator is zero.")]]
    end
  | mixed-binops(op-a-name, op-a-loc, op-b-name, op-b-loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error: 
        [ED.para: 
          ED.text("Binary operators of different kinds cannot be mixed at the same level, but you use "),
          ED.code(ED.highlight(ED.text(self.op-a-name),[list: self.op-a-loc], pallet.get(0))),
          ED.text(" at the same level as "),
          ED.code(ED.highlight(ED.text(self.op-b-name),[list: self.op-b-loc], pallet.get(1))),
          ED.text(". Use parentheses to group the operations and to make their precedence unambiguous.")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("Binary operators of different kinds cannot be mixed at the same level, but you use "),
          ED.code(ED.text(self.op-a-name)),
          ED.text(" at "),
          ED.loc(self.op-a-loc),
          ED.text(" at the same level as "),
          ED.code(ED.text(self.op-b-name)),
          ED.text(" at "),
          ED.loc(self.op-b-loc),
          ED.text(". Use parentheses to group the operations and to make their precedence unambiguous.")]]
    end
  | block-ending(l :: Loc, kind) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      [ED.error: 
        [ED.para: 
          ED.text("Blocks should end with an expression, but you ended a block with a statement. You cannot end a block with a "),
          ED.highlight(ED.text(self.kind), [list: self.l], color),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("Blocks should end with an expression, but you ended a block with a statement. You cannot end a block with a "),
          ED.text(self.kind),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(".")]]
    end
  | single-branch-if(expr :: A.Expr) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error: 
        [ED.para: 
          ED.text("If-expressions must have more than one branch, but the if-expression")],
         ED.code(ED.highlight(ED.v-sequence(self.expr.tosource().pretty(80).map(ED.text)), [list: self.expr.l], pallet.get(0))),
        [ED.para:
          ED.text("only has "),
          ED.highlight(ED.text("one branch"), [list: self.expr.branches.first.l], pallet.get(1)),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("If-expressions may not only have one branch, but the if-expression at "),
          ED.loc(self.expr.l),
          ED.text(" does not have any other branches.")]]
    end
  | unwelcome-where(kind, loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      [ED.error: 
        [ED.para: 
          ED.code(ED.text("`where`")),
          ED.text(" blocks are only allowed on named function and declarations; a where block may not be added to a "),
          ED.code(ED.highlight(ED.text(self.kind), [list: self.loc], color)),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.code(ED.text("`where`")),
          ED.text(" blocks are only allowed on named function and declarations; a where block may not be added to a "),
          ED.loc(self.kind),
          ED.text(" at "),
          ED.loc(self.loc),
          ED.text(".")]]
    end
  | no-arguments(expr) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error: 
        [ED.para: 
          ED.text("Method declarations are expected to accept at least one argument, but the method declaration")],
         ED.code(ED.highlight(ED.v-sequence(self.expr.tosource().pretty(80).map(ED.text)), [list: self.expr.l], pallet.get(0))),
        [ED.para:
          ED.text("has no arguments. When a method is applied, the first argument is a reference to the object it belongs to.")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("Method declarations are expected to accept at least one argument, but the method declaration at "),
          ED.loc(self.expr.l),
          ED.text(" has no arguments. When a method is applied, the first argument is a reference to the object it belongs to.")]]
    end
  | underscore-as(l :: Loc, kind) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], color)),
          ED.text(" cannot be used as "),
          ED.text(self.kind),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used as "),
          ED.text(self.kind),
          ED.text(".")]]
    end
  | underscore-as-pattern(l :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], color)),
          ED.text(" cannot be used as a pattern in a cases expression. If you want to match all cases not matched by the previous branches, use the pattern "),
          ED.code(ED.text("else")),
          ED.text(" instead.")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used as a pattern in a cases expression. If you want to match all cases not matched by the previous branches, use the pattern "),
          ED.code(ED.text("else")),
          ED.text(" instead.")]]
    end
  | underscore-as-expr(l :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], color)),
          ED.text(" cannot be used where an expression is expected.")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used where an expression is expected.")]]
    end
  | underscore-as-ann(l :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], color)),
          ED.text("cannot be used where a type annotation is expected.")]]
    end,
    render-reason(self):
      [ED.error: 
        [ED.para: 
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used where a type annotation is expected.")]]
    end
  | unbound-id(id :: A.Expr) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      cases(SL.Srcloc) self.id.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id.id.toname()),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The identifier "), 
              ED.code(ED.highlight(ED.text(self.id.id.toname()), [ED.locs: self.id.l], color)),
              ED.text(" is unbound. It is "),
              ED.highlight(ED.text("used"), [ED.locs: self.id.l], color),
              ED.text(" but not previously defined.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.id.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id.id.toname()),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The identifier "), 
              ED.code(ED.text(self.id.id.toname())),
              ED.text(" at "),
              ED.loc(self.id.l),
              ED.text(" is unbound. It is "),
              ED.text("used but not previously defined.")]]
      end
    end
  | unbound-var(id :: String, loc :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The variable "), 
              ED.code(ED.highlight(ED.text(self.id), [ED.locs: self.loc], color)),
              ED.text(" is unbound. It is "),
              ED.highlight(ED.text("assigned to"), [ED.locs: self.loc], color),
              ED.text(" but not previously defined.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The variable "), 
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.loc),
              ED.text(" is unbound. It is "),
              ED.text("used but not previously defined.")]]
      end
    end
  | unbound-type-id(ann :: A.Ann) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      cases(SL.Srcloc) self.ann.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.ann.tosource().pretty(1000)),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name "),
              ED.code(ED.highlight(ED.text(self.ann.id.toname()), [ED.locs: self.ann.l], color)),
              ED.text(" is used to indicate a type, but a definition of a type named "),
              ED.code(ED.highlight(ED.text(self.ann.id.toname()), [ED.locs: self.ann.l], color)),
              ED.text(" could not be found.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.ann.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.ann.tosource().pretty(1000)),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name "),
              ED.code(ED.text(self.ann.id.toname())),
              ED.text(" at "),
              ED.loc(self.ann.l),
              ED.text(" is used to indicate a type, but a definition of a type named "),
              ED.code(ED.text(self.ann.id.toname())),
              ED.text(" could not be found.")]]
      end
    end
  | unexpected-type-var(loc :: Loc, name :: A.Name) with:
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      #### TODO ###
      ED.text("Identifier " + tostring(self.name) + " is used in a dot-annotation at " + tostring(self.loc) + ", but is bound as a type variable")
    end
  | pointless-var(loc :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining the anonymous variable "),
              ED.code(ED.highlight(ED.text("var _"), [ED.locs: self.loc], color)),
              ED.text(" is pointless since there is no name that can be used to mutate it later on.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining the anonymous variable "),
              ED.code(ED.text("var _")),
              ED.text(" at "),
              ED.loc(self.loc),
              ED.text(" is pointless since there is no name that can be used to mutate it later on.")]]
      end
    end
  | pointless-rec(loc :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining the anonymous recursive identifier "), 
              ED.code(ED.highlight(ED.text("rec _"), [ED.locs: self.loc], color)),
              ED.text(" is pointless since there is no name to call recursively.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining the anonymous recursive identifier "), 
              ED.code(ED.text("rec _")),
              ED.text(" at "),
              ED.loc(self.loc),
              ED.text(" is pointless since there is no name to call recursively.")]]
      end
    end
  | pointless-shadow(loc :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(1).get(0)
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The anonymous identifier "),
              ED.code(ED.highlight(ED.text("shadow _"), [ED.locs: self.loc], color)),
              ED.text(" cannot shadow anything: there is no name to shadow.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The anonymous identifier "),
              ED.code(ED.text("shadow _")),
              ED.text(" at "),
              ED.loc(self.loc),
              ED.text(" cannot shadow anything: there is no name to shadow.")]]
      end
    end
  | bad-assignment(iuse :: A.Expr, idef :: Loc) with:
    render-fancy-reason(self, make-pallet):
      color = make-pallet(3)
      use-loc-color = color.get(0)
      def-loc-color = color.get(1)
      [ED.error:
        [ED.para:
          ED.text("The variable assignment expression "),
          ED.code(ED.highlight(ED.text(self.iuse.tosource().pretty(1000).first), [ED.locs: self.iuse.l], use-loc-color)),
          ED.text(" expects the name "),
          ED.code(ED.highlight(ED.text(self.iuse.id.toname()), [ED.locs: self.iuse.l], use-loc-color)),
          ED.text(" to refer to a variable definition expression, but "),
          ED.code(ED.text(self.iuse.id.toname())),
          ED.text(" is declared by an "),
          ED.highlight(ED.text("identifier definition expression."), [ED.locs: self.idef], def-loc-color)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The variable assignment expression "),
          ED.code(ED.text(self.iuse.tosource().pretty(1000).first)),
          ED.text(" at "),
          ED.loc(self.iuse.l),
          ED.text(" expects the name "),
          ED.code(ED.text(self.iuse.id.toname())),
          ED.text(" to refer to a variable definition expression, but "),
          ED.code(ED.text(self.iuse.id.toname())),
          ED.text(" is declared by an identifier definition expression at "),
          ED.loc(self.idef)]]
    end
  | mixed-id-var(id :: String, var-loc :: Loc, id-loc :: Loc) with:
    #### TODO ###
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      ED.text(self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
          + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")")
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    # TODO: disambiguate what is doing the shadowing and what is being shadowed.
    # it's not necessarily a binding; could be a function definition.
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      old-loc-color = pallet.get(0)
      new-loc-color = pallet.get(1)
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.highlight(ED.text(self.id), [list: self.new-loc], new-loc-color),
              ED.text(" shadows the declaration of a built-in identifier also named "),
              ED.highlight(ED.text(self.id), [list: self.old-loc], old-loc-color)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.highlight(ED.text(self.id), [list: self.new-loc], new-loc-color),
              ED.text(" shadows a previous declaration of an identifier also named "),
              ED.highlight(ED.text(self.id), [list: self.old-loc], old-loc-color)]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.new-loc),
              ED.text(" shadows the declaration of a built-in identifier also named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.old-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.new-loc),
              ED.text(" shadows the declaration of a built-in identifier also named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.old-loc)]]
      end
    end
  | duplicate-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      old-loc-color = pallet.get(0)
      new-loc-color = pallet.get(1)
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.highlight(ED.text(self.id), [list: self.new-loc], new-loc-color),
              ED.text(" is preceeded in the same scope by a declaration of an identifier also named "),
              ED.highlight(ED.text(self.id), [list: self.old-loc], old-loc-color),
              ED.text(".")]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.highlight(ED.text(self.id), [list: self.new-loc], new-loc-color),
              ED.text(" is preceeded in the same scope by a declaration of an identifier also named "),
              ED.highlight(ED.text(self.id), [list: self.old-loc], old-loc-color),
              ED.text(".")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.new-loc),
              ED.text(" is preceeded in the same scope by a declaration of an identifier also named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.old-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.new-loc),
              ED.text(" is preceeded in the same scope by a declaration of an identifier also named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.old-loc)]]
      end
    end
  | duplicate-field(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      old-loc-color = pallet.get(0)
      new-loc-color = pallet.get(1)
      [ED.error:
        [ED.para:
          ED.text("The declaration of the field named "),
          ED.highlight(ED.text(self.id), [list: self.new-loc], new-loc-color),
          ED.text(" is preceeded by declaration of an field also named "),
          ED.highlight(ED.text(self.id), [list: self.old-loc], old-loc-color),
          ED.text(".")],
        [ED.para: ED.text("You need to pick a different name for one of them.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The declaration of the field named "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.new-loc),
          ED.text(" is preceeded in the same object by a field of an identifier also named "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.old-loc),
          ED.text(".")],
        [ED.para: ED.text("You need to pick a different name for one of them.")]]
    end
  | same-line(a :: Loc, b :: Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("Pyret expects each expression within a block to have its own line, but Pyret found "),
          ED.highlight(ED.text("an expression"), [list: self.a], pallet.get(0)),
          ED.text(" on the same line as "),
          ED.highlight(ED.text("another expression"), [list: self.b], pallet.get(1)),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret expects each expression within a block to have its own line, but the expression at "),
          ED.loc(self.a),
          ED.text(" is on the same line as the expression at "),
          ED.loc(self.b),
          ED.text(".")]]
    end
  | incorrect-type(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because it found a "), 
          ED.highlight(ED.text(self.bad-name), [list: self.bad-loc], pallet.get(0)),
          ED.text(" but it "),
          ED.highlight(ED.text("expected"), [list: self.expected-loc], pallet.get(1)),
          ED.text(" a "),
          ED.text(self.expected-name)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected to find "), ED.code(ED.text(self.expected-name)),
          ED.text(" at "), draw-and-highlight(self.bad-loc),
          ED.text(", required by "), draw-and-highlight(self.expected-loc),
          ED.text(", but instead found "), ED.code(ED.text(self.bad-name)), ED.text(".")]]
    end
  | incorrect-type-expression(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc, e :: A.Expr) with:
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected the expression")],
        [ED.para:
          ED.code(ED.v-sequence(self.e.tosource().pretty(80).map(ED.text)))],
        [ED.para:
          ED.text("because the expression at "),
          ED.embed(self.bad-loc),
          ED.text(" was of type " + self.bad-name),
          ED.text(" but it was expected to be of type "),
          ED.embed(self.expected-name),
          ED.text(" because "),
          draw-and-highlight(self.expected-loc)]]
    end
  | bad-type-instantiation(expected :: List<T.Type>, given :: List<T.Type>, ann :: A.Ann) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the type instantiation")],
       [ED.para:
          ED.code([ED.sequence:
            ED.highlight(ED.h-sequence(self.ann.ann.tosource().pretty(80).map(ED.text),""), [list: self.ann.ann.l], pallet.get(0)),
            ED.text("<"),
            ED.h-sequence(self.ann.args.map(lam(ann):
              ED.highlight(ED.h-sequence(ann.tosource().pretty(80).map(ED.text), ""), [list: ann.l], pallet.get(1));), ","),
            ED.text(">")])],
        [ED.para:
          ED.text("should give exactly the same number of parameters as the type accepts. However, the type instantiation is given "),
          ED.highlight(ED.ed-params(self.given.length()), self.ann.args.map(_.l), pallet.get(1)),
          ED.text(", but the type accepts "),
          ED.embed(self.expected.length()),
          ED.text(" parameters.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the type instantiation")],
       [ED.para:
          ED.code(ED.v-sequence(self.ann.tosource().pretty(80).map(ED.text)))],
        [ED.para:
          ED.text(" at "),
          ED.loc(self.ann.l),
          ED.text("should give exactly the same number of parameters as the type accepts. However, the type instantiation is given "),
          ED.ed-params(self.given.length()),
          ED.text(", but the type accepts "),
          ED.embed(self.expected.length()),
          ED.text(" parameters.")]]
    end
  | incorrect-number-of-args(app-expr, fun-typ) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      ed-applicant = ED.highlight(ED.text("applicant"), [list: self.app-expr._fun.l], pallet.get(0))
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the function application expression")],
        [ED.para:
          ED.code(ED.v-sequence(self.app-expr.tosource().pretty(80).map(ED.text)))],
        [ED.para:
          ED.text("expects the "), ed-applicant,
          ED.text(" to evaluate to a function accepting exactly the same number of arguments as given to it in application.")],
        [ED.para:
          ED.text("However, the "), 
          ed-applicant,
          ED.text(" is given "),
          ED.highlight(ED.ed-args(self.app-expr.args.length()), self.app-expr.args.map(_.l), pallet.get(1)), 
          ED.text(" and the type signature of the "),
          ed-applicant],
        [ED.para:
          ED.embed(self.fun-typ)],
        [ED.para:
          ED.text("indicates that it evaluates to a function accepting exactly "),
          ED.ed-args(self.fun-typ.args.length()),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the function application expression")],
        [ED.para:
          ED.code(ED.v-sequence(self.app-expr.tosource().pretty(80).map(ED.text)))],
        [ED.para:
          ED.text("expects the applicant at "),
          ED.loc(self.app-expr._fun.l),
          ED.text(" to evaluate to a function accepting exactly the same number of arguments as given to it in application.")],
        [ED.para:
          ED.text("However, the applicant is given "),
          ED.ed-args(self.app-expr.args.length()), 
          ED.text(" and the type signature of the applicant")],
        [ED.para:
          ED.embed(self.fun-typ)],
        [ED.para:
          ED.text("indicates that it evaluates to a function accepting exactly "),
          ED.ed-args(self.fun-typ.args.length()),
          ED.text(".")]]
    end
  | apply-non-function(app-expr :: A.Expr, typ) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(1)
      ed-applicant = ED.highlight(ED.text("applicant"), [list: self.app-expr._fun.l], pallet.get(0))
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the function application expression")],
        [ED.para:
          ED.code([ED.sequence:
            ED.highlight(ED.h-sequence(self.app-expr._fun.tosource().pretty(999).map(ED.text),""),[list: self.app-expr._fun.l],pallet.get(0)),
            ED.text("("),
            ED.h-sequence(
              self.app-expr.args.map(
                lam(arg):ED.h-sequence(arg.tosource().pretty(999).map(ED.text),"");), ", "),
            ED.text(")")])],
        [ED.para:
          ED.text("expects the "), ed-applicant,
          ED.text(" to evaluate to a function value. However, the type of the "), 
          ed-applicant,
          ED.text(" is "),
          ED.embed(self.typ)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the function application expression")],
        [ED.para:
          ED.code(ED.v-sequence(self.app-expr.tosource().pretty(80).map(ED.text)))],
        [ED.para:
          ED.text("at "),
          ED.loc(self.app-expr._fun.l),
          ED.text(" expects the applicant to evaluate to a function value. However, the type of the applicant is "), 
          ED.embed(self.typ)]]
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the object type")],
         ED.highlight(ED.embed(self.obj), [list: self.obj-loc], pallet.get(0)),
        [ED.para:
          ED.text("does not have a field named "),
          ED.code(ED.highlight(ED.text(self.field-name), [list: self.access-loc], pallet.get(1)))]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the object type ")],
          ED.embed(self.obj),
          ED.text(" at "),
          ED.loc(self.obj-loc),
          ED.text(" does not have a field named "),
          ED.code(ED.text(self.field-name)),
          ED.text(" as indicated by the access of that field at "),
          ED.loc(self.access-loc)]
    end
  | duplicate-variant(id :: String, found :: Loc, previous :: Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("A variant may not have the same name as any other variant in the type, but the declaration of a variant named "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.found], pallet.get(0))),
          ED.text(" is preceeded by a declaration of a variant also named "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.previous], pallet.get(1))),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("A variant may not have the same name as any other variant in the type, but the declaration of a variant "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.found),
          ED.text(" is preceeded by a declaration of a variant also named "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.previous),
          ED.text(".")]]
    end,
  | duplicate-branch(id :: String, found :: Loc, previous :: Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("A variant may not be matched more than once in a cases expression, but the branch matching the variant "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.found], pallet.get(0))),
          ED.text(" is preceeded by a branch also matching "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.previous], pallet.get(1))),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("A variant may not be matched more than once in a cases expression, but the branch matching the variant "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.found),
          ED.text(" is preceeded by a branch also matching "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.previous),
          ED.text(".")]]
    end,
  | unneccesary-branch(branch :: A.CasesBranch, data-type :: T.DataType, cases-loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(3)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the "),
          ED.highlight(ED.text("cases expression"),[list: self.cases-loc], pallet.get(0)),
          ED.text(" expects that all of its branches have a variant of the same name in the data-type "),
          ED.text(self.data-type.name), 
          ED.text(". However, no variant named "),
          ED.code(ED.highlight(ED.text(self.branch.name), [list: self.branch.pat-loc], pallet.get(1))),
          ED.text(" exists in "),
          ED.text(self.data-type.name), 
          ED.text("'s "),
          ED.highlight(ED.text("variants"),self.data-type.variants.map(_.l), pallet.get(2)),
          ED.text(":")],
         ED.bulleted-sequence(self.data-type.variants.map(lam(variant):
          ED.code(ED.highlight(ED.text(variant.name), [list: variant.l], pallet.get(2)));))]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the cases expression at "),
          ED.loc(self.cases-loc),
          ED.text(" expects that all of its branches have a variant of the same name in the data-type "),
          ED.text(self.data-type.name), 
          ED.text(". However, no variant named "),
          ED.code(ED.text(self.branch.name)),
          ED.text(" (mentioned in the branch at "),
          ED.loc(self.branch.pat-loc),
          ED.text(")"),
          ED.text(" exists in the type "),
          ED.text(self.data-type.name), 
          ED.text("'s variants:")],
         ED.bulleted-sequence(self.data-type.variants.map(_.name).map(ED.text))]
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(3)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the "),
          ED.highlight(ED.text("cases expression"),[list: self.loc], pallet.get(0)),
          ED.text(" has a branch for every variant of "),
          ED.code(ED.text(self.type-name)), 
          ED.text(". Therefore, the "),
          ED.code(ED.text("else")),
          ED.text(" branch is unreachable.")]]
    end,
    render-reason(self):
      ED.text("The else branch for the cases expression at " + tostring(self.loc)
        + " is not needed since all variants of " + self.type-name + " have been exhausted.")
    end
  | non-exhaustive-pattern(missing :: List<String>, type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(1)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the "),
          ED.highlight(ED.text("cases expression"),[list: self.loc], pallet.get(0)),
          ED.text(" is expected to be non-exhaustive, but there are variants in the type "),
          ED.code(ED.text(self.type-name)),
          ED.text(" which do not have a corresponding branch:")],
        ED.bulleted-sequence(self.missing.map(ED.text).map(ED.code))]
    end,
    render-reason(self):
      ED.text("The cases expression at " + tostring(self.loc)
        + " does not exhaust all variants of " + self.type-name
        + ". It is missing: " + self.missing.join-str(", ") + ".")
    end
  | cant-match-on(ann, type-name :: String, loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(2)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because a "),
          ED.code(ED.highlight(ED.text("cases expressions"), [list: self.loc], pallet.get(0))),
          ED.text(" can only branch on variants of "),
          ED.code(ED.text("data")),
          ED.text(" types. The type "),
          ED.code(ED.highlight(ED.text(self.type-name), [list: self.ann.l], pallet.get(1))),
          ED.text(" cannot be used in cases expressions.")]]
    end,
    render-reason(self):
      ED.text("The type specified " + self.type-name
        + " at " + tostring(self.loc)
        + " cannot be used in a cases expression.")
    end
  | incorrect-number-of-bindings(branch :: A.CasesBranch, variant :: T.TypeVariant) with:
    render-fancy-reason(self, make-pallet):
      fun ed-fields(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " field"
                  else:      " fields";)]
      end
      pallet = make-pallet(4)
      [ED.error:
        [ED.para:
          ED.text("The type checker expects that the "),
          ED.highlight(ED.text("pattern"), [list: self.branch.pat-loc], pallet.get(0)),
          ED.text(" in the cases branch has the same number of "),
          ED.highlight(ED.text("field bindings"), self.branch.args.map(_.l), pallet.get(1)),
          ED.text(" as the data variant "),
          ED.code(ED.highlight(ED.text(self.variant.name), [list: self.variant.l], pallet.get(2))),
          ED.text(" has "),
          ED.highlight(ED.text("fields"), [list: A.dummy-loc], pallet.get(3)),
          ED.text(". However, the branch pattern binds "),
          ED.highlight(ed-fields(self.branch.args.length()), self.branch.args.map(_.l), pallet.get(1)),
          ED.text(" and the variant is declared as having "),
          ED.highlight(ed-fields(self.variant.fields.length()), [list: A.dummy-loc], pallet.get(3))]]
    end,
    render-reason(self):
      fun ed-fields(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " field"
                  else:      " fields";)]
      end
      [ED.error:
        [ED.para:
          ED.text("The type checker expects that the pattern at "),
          ED.loc(self.branch.pat-loc),
          ED.text(" in the cases branch has the same number of field bindings as the data variant "),
          ED.code(ED.text(self.variant.name)),
          ED.text(" at "),
          ED.loc(self.variant.l),
          ED.text(" has fields. However, the branch pattern binds "),
          ed-fields(self.branch.args.length()),
          ED.text(" and the variant is declared as having "),
          ed-fields(self.variant.fields.length())]]
    end
  | cases-singleton-mismatch(name :: String, branch-loc :: A.Loc, should-be-singleton :: Boolean) with:
    render-fancy-reason(self, make-pallet):
      pallet = make-pallet(1)
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The type checker rejected your program because the cases branch named "), 
            ED.code(ED.highlight(ED.text(self.name), [list: self.branch-loc], pallet.get(0))),
            ED.text(" has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The type checker rejected your program because the cases branch named "), 
            ED.code(ED.highlight(ED.text(self.name), [list: self.branch-loc], pallet.get(0))),
            ED.text(" has an argument list, but the variant is not a singleton.")]]
      end
    end,
    render-reason(self):
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The type checker rejected your program because the cases branch named "), 
            ED.code(ED.text(self.name)),
            ED.text(" at "),
            ED.loc(self.branch-loc),
            ED.text(" has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The type checker rejected your program because the cases branch named "), 
            ED.code(ED.text(self.name)),
            ED.text(" at "),
            ED.loc(self.branch-loc),
            ED.text(" has an argument list, but the variant is not a singleton.")]]
      end
    end
  | given-parameters(data-type :: String, loc :: A.Loc) with:
    # duplicate of `bad-type-instantiation` ?
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The data type "),  ED.code(ED.text(self.data-type)),
          ED.text(" does not take any parameters, but is given some at "),
          draw-and-highlight(self.loc)]]
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("In the type at "), draw-and-highlight(self.loc),
          ED.text(" there was not enough information to instantiate the type, "
            + "or the given arguments are incompatible.")]]
    end
  | cant-typecheck(reason :: String) with:
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      ED.text("This program cannot be type-checked. Please send it to the developers. " +
        "The reason that it cannot be type-checked is: " + self.reason)
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    #### TODO ###
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      ED.text(self.message + " (found at " + tostring(self.blame-loc) + ")")
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    #### TODO ###
    render-fancy-reason(self, make-pallet):
      self.render-reason()
    end,
    render-reason(self):
      ED.text("There is no module imported with the name " + self.mod-name
        + " (used at " + tostring(self.loc) + ")")
    end
end

default-compile-options = {
  check-mode : true,
  type-check : false,
  allow-shadowed : false,
  collect-all: false,
  ignore-unbound: false,
  proper-tail-calls: true
}

t-nothing = T.t-nothing
t-str = T.t-string
t-bool = T.t-boolean
t-boolean = T.t-boolean
t-number = T.t-number

t-pred = T.t-arrow([list: T.t-top], t-bool)
t-pred2 = T.t-arrow([list: T.t-top, T.t-top], t-bool)
t-arrow = T.t-arrow
t-member = T.t-member
t-bot = T.t-bot
t-record = T.t-record

t-number-binop = T.t-arrow([list: t-number, t-number], t-number)
t-number-unop = T.t-arrow([list: t-number], t-number)
t-number-pred1 = T.t-arrow([list: t-number], t-boolean)
t-within-num = T.t-arrow([list: T.t-number], T.t-arrow([list: T.t-number, T.t-number], T.t-boolean))
t-within-any = T.t-arrow([list: T.t-number], T.t-arrow([list: T.t-top, T.t-top], T.t-boolean))

runtime-types = [string-dict:
  "Number", T.t-top,
  "String", t-str,
  "Function", T.t-top,
  "Boolean", T.t-top,
  "Object", T.t-top,
  "Method", T.t-top,
  "Nothing", T.t-top,
  "RawArray", T.t-top
]

fun t-forall1(f):
  n = A.global-names.make-atom("a")
  T.t-forall([list: T.t-variable(A.dummy-loc, n, T.t-top, T.invariant)], f(T.t-var(n)))
end

runtime-builtins = [string-dict: 
  "test-print", t-forall1(lam(a): T.t-arrow([list: a], a) end),
  "print", t-forall1(lam(a): T.t-arrow([list: a], a) end),
  "display", t-forall1(lam(a): T.t-arrow([list: a], a) end),
  "print-error", t-forall1(lam(a): T.t-arrow([list: a], a) end),
  "display-error", t-forall1(lam(a): T.t-arrow([list: a], a) end),
  "tostring", T.t-arrow([list: T.t-top], t-str),
  "torepr", T.t-arrow([list: T.t-top], t-str),
  "brander", T.t-top,
  "raise", T.t-arrow([list: T.t-top], T.t-bot),
  "nothing", t-nothing,
  "builtins", t-record([list:
      t-member("has-field", t-arrow([list: t-record(empty)], t-boolean)),
      t-member("current-checker", t-arrow([list: ], t-record([list: # Cheat on these types for now.
          t-member("run-checks", t-bot),
          t-member("check-is", t-bot),
          t-member("check-is-refinement", t-bot),
          t-member("check-is-not", t-bot),
          t-member("check-is-not-refinement", t-bot),
          t-member("check-is-refinement", t-bot),
          t-member("check-is-not-refinement", t-bot),
          t-member("check-satisfies", t-bot),
          t-member("check-satisfies-not", t-bot),
          t-member("check-raises-str", t-bot),
          t-member("check-raises-not", t-bot),
          t-member("check-raises-other-str", t-bot),
          t-member("check-raises-satisfies", t-bot),
          t-member("check-raises-violates" , t-bot)
      ])))
  ]),
  "not", T.t-arrow([list: t-bool], t-bool),
  "is-nothing", t-pred,
  "is-number", t-pred,
  "is-string", t-pred,
  "is-boolean", t-pred,
  "is-object", t-pred,
  "is-function", t-pred,
  "is-raw-array", t-pred,
  "gensym", T.t-top,
  "random", T.t-top,
  "run-task", T.t-top,
  "_plus", T.t-top,
  "_minus", T.t-top,
  "_times", T.t-top,
  "_divide", T.t-top,
  "_lessthan", T.t-top,
  "_lessequal", T.t-top,
  "_greaterthan", T.t-top,
  "_greaterequal", T.t-top,
  "string-equal", T.t-top,
  "string-contains", T.t-top,
  "string-append", T.t-top,
  "string-length", T.t-top,
  "string-tonumber", T.t-top,
  "string-to-number", T.t-arrow([list: T.t-string], T.t-option(T.t-number)),
  "string-repeat", T.t-top,
  "string-substring", T.t-top,
  "string-replace", T.t-top,
  "string-split", T.t-top,
  "string-split-all", T.t-top,
  "string-char-at", T.t-top,
  "string-toupper", T.t-top,
  "string-tolower", T.t-top,
  "string-explode", T.t-top,
  "string-index-of", T.t-top,
  "string-to-code-point", T.t-top,
  "string-from-code-point", T.t-top,
  "string-to-code-points", T.t-top,
  "string-from-code-points", T.t-top,
  "num-random", t-number-unop,
  "num-random-seed", T.t-arrow([list: T.t-number], T.t-nothing),
  "num-max", t-number-binop,
  "num-min", t-number-binop,
  "num-equal", T.t-arrow([list: T.t-number, T.t-number], T.t-boolean),
  "num-round", t-number-unop,
  "num-round-even", t-number-unop,
  "num-abs", t-number-unop,
  "num-sin", t-number-unop,
  "num-cos", t-number-unop,
  "num-tan", t-number-unop,
  "num-asin", t-number-unop,
  "num-acos", t-number-unop,
  "num-atan", t-number-unop,
  "num-modulo", t-number-binop,
  "num-truncate", t-number-unop,
  "num-sqrt", t-number-unop,
  "num-sqr", t-number-unop,
  "num-ceiling", t-number-unop,
  "num-floor", t-number-unop,
  "num-log", t-number-unop,
  "num-exp", t-number-unop,
  "num-exact", t-number-unop,
  "num-to-rational", t-number-unop,
  "num-to-roughnum", t-number-unop,
  "num-is-positive", t-number-pred1,
  "num-is-negative", t-number-pred1,
  "num-is-non-positive", t-number-pred1,
  "num-is-non-negative", t-number-pred1,
  "num-is-integer", t-number-pred1,
  "num-is-fixnum", t-number-pred1,
  "num-is-rational", t-number-pred1,
  "num-is-roughnum", t-number-pred1,
  "num-expt", t-number-binop,
  "num-tostring", T.t-arrow([list: T.t-number], T.t-string),
  "num-to-string", T.t-arrow([list: T.t-number], T.t-string),
  "num-to-string-digits", T.t-arrow([list: T.t-number, T.t-number], T.t-string),
  "num-within", t-within-num,
  "num-within-rel", t-within-num,
  "num-within-abs", t-within-num,
  "within-rel", t-within-any,
  "within-rel-now", t-within-any,
  "within-abs", t-within-any,
  "within-abs-now", t-within-any,
  "within", t-within-any,
  "raw-array-get", T.t-top,
  "raw-array-set", T.t-top,
  "raw-array-of", T.t-top,
  "raw-array-length", T.t-top,
  "raw-array-to-list", T.t-top,
  "raw-array-fold", T.t-top,
  "raw-array", T.t-record(
    [list:
      T.t-member("make", t-forall1(lam(a): T.t-arrow([list: T.t-array(a)], T.t-array(a)) end))
    ]),
  "ref-get", T.t-top,
  "ref-set", T.t-top,
  "ref-freeze", T.t-top,
  "equal-always", t-pred2,
  "equal-always3", T.t-top,
  "equal-now", t-pred2,
  "equal-now3", T.t-top,
  "identical", t-pred2,
  "identical3", T.t-top,
  "exn-unwrap", T.t-top,
  "_empty", T.t-top,
  "_link", T.t-top
]

no-builtins = compile-env(globals([string-dict: ], [string-dict: ]), [string-dict:])

minimal-builtins = compile-env(globals(runtime-builtins, runtime-types), [string-dict:])

standard-globals = globals(runtime-builtins, runtime-types)
standard-builtins = compile-env(globals(runtime-builtins, runtime-types), [string-dict:])

minimal-imports = extra-imports(empty)

standard-imports = extra-imports(
   [list: 
      extra-import(builtin("arrays"), "arrays", [list: 
          "array",
          "build-array",
          "array-from-list",
          "is-array",
          "array-of",
          "array-set-now",
          "array-get-now",
          "array-length",
          "array-to-list-now"
        ],
        [list: "Array"]),
      extra-import(builtin("lists"), "lists", [list: 
          "list",
          "is-empty",
          "is-link",
          "empty",
          "link",
          "range",
          "range-by",
          "repeat",
          "filter",
          "partition",
          "split-at",
          "any",
          "find",
          "map",
          "map2",
          "map3",
          "map4",
          "map_n",
          "map2_n",
          "map3_n",
          "map4_n",
          "each",
          "each2",
          "each3",
          "each4",
          "each_n",
          "each2_n",
          "each3_n",
          "each4_n",
          "fold",
          "fold2",
          "fold3",
          "fold4",
          "index"
        ],
        [list: "List"]),
      extra-import(builtin("option"), "option", [list: 
          "Option",
          "is-none",
          "is-some",
          "none",
          "some"
        ],
        [list: "Option"]),
      extra-import(builtin("error"), "error", [list: ], [list:]),
      extra-import(builtin("sets"), "sets", [list: 
          "set",
          "tree-set",
          "list-set"
        ],
        [list: "Set"])
    ])

