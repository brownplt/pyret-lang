#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL
import error-display as ED
import string-dict as SD
import file("concat-lists.arr") as CL
import file("type-structs.arr") as T
import file("js-ast.arr") as J

clist = CL.clist

t-nothing = T.t-nothing(A.dummy-loc)
t-str = T.t-string(A.dummy-loc)
t-boolean = T.t-boolean(A.dummy-loc)
t-number = T.t-number(A.dummy-loc)
t-arrow = T.t-arrow(_, _, A.dummy-loc)
t-top = T.t-top(A.dummy-loc)
t-member = T.t-member(_, _)
t-bot = T.t-bot(A.dummy-loc)
t-record = T.t-record(_, A.dummy-loc)
t-forall = T.t-forall(_, _, A.dummy-loc)
t-var = T.t-var(_, A.dummy-loc)
t-array = T.t-array(_, A.dummy-loc)
t-string = T.t-string(A.dummy-loc)
t-option = T.t-option(_, A.dummy-loc)
t-data = T.t-data(_, _, _, A.dummy-loc)
t-variant = T.t-variant(_, _, _)
t-singleton-variant = T.t-variant(_, _)
t-app = T.t-app(_, _, A.dummy-loc)
t-name = T.t-name(_, _, A.dummy-loc)



type URI = String
type StringDict = SD.StringDict
string-dict = SD.string-dict

type Loc = SL.Srcloc

data Dependency:
  | dependency(protocol :: String, arguments :: List<String>)
    with:
    key(self): self.protocol + "(" + self.arguments.join-str(", ") + ")" end
  | builtin(modname :: String)
    with:
    key(self): "builtin(" + self.modname + ")" end
end

data NativeModule:
  | requirejs(path :: String)
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

# The strings in globals should be the appropriate dependency (e.g. in mods)
data Globals:
  | globals(values :: StringDict<String>, types :: StringDict<String>)
end

data Provides:
  | provides(
      from-uri :: URI,
      values :: StringDict<T.Type>,
      aliases :: StringDict<T.Type>,
      data-definitions :: StringDict<T.Type>
    )
end

fun make-dep(raw-dep) -> Dependency:
 if raw-dep.import-type == "builtin":
    builtin(raw-dep.name)
  else:
    dependency(raw-dep.protocol, raw-array-to-list(raw-dep.args))
  end
end

rag = raw-array-get

fun type-from-raw(uri, typ, tyvar-env :: SD.StringDict<T.TypeVariable>):
  tfr = type-from-raw(uri, _, tyvar-env)
  # TODO(joe): Make this do something intelligent when location information
  # is available
  l = SL.builtin(uri)
  t = typ.tag
  ask:
    | t == "any" then: T.t-top(l)
    | t == "record" then:
      t-record(for map(f from typ.fields): T.t-member(f.name, tfr(f.value)) end, l)
    | t == "name" then:
      if typ.origin.import-type == "$ELF":
        T.t-name(T.local, A.s-type-global(typ.name), l)
      else if typ.origin.import-type == "uri":
        T.t-name(T.module-uri(typ.origin.uri), A.s-type-global(typ.name), l)
      else:
        T.t-name(T.dependency(make-dep(typ.origin)), A.s-type-global(typ.name), l)
      end
    | t == "tyvar" then:
      cases(Option<T.TypeVariable>) tyvar-env.get(typ.name):
        | none => raise("Unbound type variable " + typ.name + " in provided type.")
        | some(tv) => T.t-var(tv, l)
      end
    | t == "forall" then:
      new-env = for fold(new-env from tyvar-env, a from typ.args):
        tvn = A.global-names.make-atom(a)
        new-env.set(a, tvn)
      end
      params = for map(k from new-env.keys-list()):
        T.t-var(new-env.get-value(k), l)
      end
      T.t-forall(params, type-from-raw(uri, typ.onto, new-env), l)
    | t == "tyapp" then:
      T.t-app(tfr(typ.onto), map(tfr, typ.args), l)
    | t == "arrow" then:
      T.t-arrow(map(tfr, typ.args), tfr(typ.ret), l)
    | otherwise: raise("Unknown raw tag for type: " + t)
  end
end

fun tvariant-from-raw(uri, tvariant, env):
  l = SL.builtin(uri)
  t = tvariant.tag
  ask:
    | t == "variant" then:
      members = for map(tm from tvariant.vmembers):
        # TODO(joe): Exporting ref fields?
        T.t-member(tm.name, type-from-raw(uri, tm.typ, env))
      end
      T.t-variant(tvariant.name, members, empty, l)
    | t == "singleton-variant" then:
      T.t-singleton-variant(tvariant.name, empty, l)
    | otherwise: raise("Unkonwn raw tag for variant: " + t)
  end
end

fun datatype-from-raw(uri, datatyp):
  l = SL.builtin(uri)

  if datatyp.tag == "any":
    # TODO(joe): this will be replaced when datatypes have a settled format
    t-top
  else:
    pdict = for fold(pdict from SD.make-string-dict(), a from datatyp.params):
      tvn = A.global-names.make-atom(a)
      pdict.set(a, tvn)
    end
    params = for map(k from pdict.keys-list()):
      T.t-var(pdict.get-value(k), l)
    end
    variants = map(tvariant-from-raw(uri, _, pdict), datatyp.variants)
    members = for map(tm from datatyp.methods):
      # TODO(joe): Exporting ref fields?
      T.t-member(tm.name, type-from-raw(uri, tm.value, pdict))
    end
    temp-data = t-data(datatyp.name, variants, members)
    if is-empty(params):
      temp-data
    else:
      t-forall(params, temp-data)
    end
  end
end



fun provides-from-raw-provides(uri, raw):
  values = raw.values
  vdict = for fold(vdict from SD.make-string-dict(), v from raw.values):
    if is-string(v):
      vdict.set(v, t-top)
    else:
      vdict.set(v.name, type-from-raw(uri, v.typ, SD.make-string-dict()))
    end
  end
  aliases = raw.aliases
  adict = for fold(adict from SD.make-string-dict(), a from raw.aliases):
    if is-string(a):
      adict.set(a, t-top)
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




fun provides-to-raw-provides-ast(provs, env):
  cases(Provides) provs:
    | provides(uri, values, aliases, data-defs) =>
    #|
      value-fields = for CL.map_list(v from values.keys().to-list()):
        J.j-field(v, type-to-raw-ast(values.get-value(v), compile-env))
      end
      data-fields = for CL.map_list(d from data-defs.keys().to-list()):
        J.j-field(d, type-to-raw-ast(data-defs.get-value(d), compile-env))
      end
      alias-fields = for CL.map_list(a from aliases.keys().to-list()):
        J.j-field(a, type-to-raw-ast(aliases.get-value(a), compile-env))
      end
      |#
      J.j-obj([clist:
        #|J.j-field("values", J.j-obj(value-fields)),
        J.j-field("datatypes", J.j-obj(data-fields)),
        J.j-field("aliases", J.j-obj(alias-fields))|#
      ])
  end
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
    render-fancy-reason(self):
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret rejected your program because you have an "),
          ED.highlight(ED.text("empty block"),[list: self.loc], 0),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        [ED.para: draw-and-highlight(self.loc)]]
    end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    render-fancy-reason(self):
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret disallows the use of "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.loc], 0)),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret disallows the fraction literal expression")],
        [ED.para:
          ED.code(ED.highlight([ED.sequence:
                                  ED.embed(self.numerator),
                                  ED.text(" / 0")],
                               [list: self.loc], 0))],
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Binary operators of different kinds cannot be mixed at the same level, but you use "),
          ED.code(ED.highlight(ED.text(self.op-a-name),[list: self.op-a-loc], 0)),
          ED.text(" at the same level as "),
          ED.code(ED.highlight(ED.text(self.op-b-name),[list: self.op-b-loc], 1)),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Blocks should end with an expression, but you ended a block with a statement. You cannot end a block with a "),
          ED.highlight(ED.text(self.kind), [list: self.l], 0),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("If-expressions must have more than one branch, but the if-expression")],
         ED.code(ED.highlight(ED.v-sequence(self.expr.tosource().pretty(80).map(ED.text)), [list: self.expr.l], 0)),
        [ED.para:
          ED.text("only has "),
          ED.highlight(ED.text("one branch"), [list: self.expr.branches.first.l], 1),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.code(ED.text("`where`")),
          ED.text(" blocks are only allowed on named function and declarations; a where block may not be added to a "),
          ED.code(ED.highlight(ED.text(self.kind), [list: self.loc], 0)),
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
  | non-example(expr :: A.Expr) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.code(ED.text("`example`")),
          ED.text("blocks must only contain testing statements, but ")],
         ED.code(ED.highlight(ED.v-sequence(self.expr.tosource().pretty(80).map(ED.text)), [list: self.expr.l], 0)),
        [ED.para:
          ED.text(" isn't a testing statement.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.code(ED.text("`example`")),
          ED.text("blocks must only contain testing statements, but the statement at "),
          ED.loc(self.expr.l),
          ED.text(" isn't a testing statement.")]]
    end
  | no-arguments(expr) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Method declarations are expected to accept at least one argument, but the method declaration")],
         ED.code(ED.highlight(ED.v-sequence(self.expr.tosource().pretty(80).map(ED.text)), [list: self.expr.l], 0)),
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
  | non-toplevel(kind, l :: Loc) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("You may only define a "),
          ED.code(ED.highlight(ED.text(self.kind), [ED.locs: self.l], 0)),
          ED.text(" at the top-level.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("You may only define the "),
          ED.code(ED.text(self.kind)),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" at the top-level.")]]
    end
  | underscore-as(l :: Loc, kind) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
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
  | block-needed(expr-loc :: Loc, block-locs :: List<Loc>) with:
    render-reason(self):
      if self.block-locs.length() > 1:
        [ED.error:
          [ED.para: ED.text("The expression at"), draw-and-highlight(self.expr-loc),
            ED.text("contains several blocks that each contain multiple expressions:")],
          ED.v-sequence(self.block-locs.map(draw-and-highlight)),
          [ED.para:
            ED.text("Either simplify each of these blocks to a single expression, or mark the outer expression with"),
            ED.code(ED.text("block:")), ED.text("to indicate this is deliberate.")]]
      else:
        [ED.error:
          [ED.para: ED.text("The expression at"), draw-and-highlight(self.expr-loc),
            ED.text("contains a block that contains multiple expressions:")],
          ED.v-sequence(self.block-locs.map(draw-and-highlight)),
          [ED.para:
            ED.text("Either simplify this block to a single expression, or mark the outer expression with"),
            ED.code(ED.text("block:")), ED.text("to indicate this is deliberate.")]]
      end
    end
  | unbound-id(id :: A.Expr) with:
    render-fancy-reason(self):
      cases(SL.Srcloc) self.id.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id.id.toname()), ED.text("at"),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The identifier "),
              ED.code(ED.highlight(ED.text(self.id.id.toname()), [ED.locs: self.id.l], 0)),
              ED.text(" is unbound. It is "),
              ED.highlight(ED.text("used"), [ED.locs: self.id.l], 0),
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
    render-fancy-reason(self):
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
              ED.code(ED.highlight(ED.text(self.id), [ED.locs: self.loc], 0)),
              ED.text(" is unbound. It is "),
              ED.highlight(ED.text("assigned to"), [ED.locs: self.loc], 0),
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
    render-fancy-reason(self):
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
              ED.code(ED.highlight(ED.text(self.ann.id.toname()), [ED.locs: self.ann.l], 0)),
              ED.text(" is used to indicate a type, but a definition of a type named "),
              ED.code(ED.highlight(ED.text(self.ann.id.toname()), [ED.locs: self.ann.l], 0)),
              ED.text(" could not be found.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.ann.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.ann.tosource().pretty(1000)), ED.text("at"),
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
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      #### TODO ###
      [ED.error:
        [ED.para-nospace:
          ED.text("Identifier "),
          ED.text(tostring(self.name)),
          ED.text(" is used in a dot-annotation at "),
          draw-and-highlight(self.loc),
          ED.text(", but is bound as a type variable")]]
    end
  | pointless-var(loc :: Loc) with:
    render-fancy-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
              draw-and-highlight(self.loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining the anonymous variable "),
              ED.code(ED.highlight(ED.text("var _"), [ED.locs: self.loc], 0)),
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
    render-fancy-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
              draw-and-highlight(self.loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining the anonymous recursive identifier "),
              ED.code(ED.highlight(ED.text("rec _"), [ED.locs: self.loc], 0)),
              ED.text(" is pointless since there is no name to call recursively.")]]
      end
    end,
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
              draw-and-highlight(self.loc)]]
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
    render-fancy-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The anonymous identifier "),
              ED.code(ED.highlight(ED.text("shadow _"), [ED.locs: self.loc], 0)),
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
    render-fancy-reason(self):
      use-loc-color = 0
      def-loc-color = 1
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
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text(self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
              + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")")]]
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    # TODO: disambiguate what is doing the shadowing and what is being shadowed.
    # it's not necessarily a binding; could be a function definition.
    render-fancy-reason(self):
      old-loc-color = 0
      new-loc-color = 1
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.highlight(ED.text(self.id), [list: self.new-loc], new-loc-color),
              ED.text(" shadows the declaration of a built-in of the same name.")]]
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
    render-fancy-reason(self):
      old-loc-color = 0
      new-loc-color = 1
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
    render-fancy-reason(self):
      old-loc-color = 0
      new-loc-color = 1
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret expects each expression within a block to have its own line, but Pyret found "),
          ED.highlight(ED.text("an expression"), [list: self.a], 0),
          ED.text(" on the same line as "),
          ED.highlight(ED.text("another expression"), [list: self.b], 1),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because it found a "),
          ED.highlight(ED.text(self.bad-name), [list: self.bad-loc], 0),
          ED.text(" but it "),
          ED.highlight(ED.text("expected"), [list: self.expected-loc], 1),
          ED.text(" a "),
          ED.text(self.expected-name)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Expected to find "), ED.code(ED.text(self.expected-name)),
          ED.text(" at "), draw-and-highlight(self.bad-loc),
          ED.text(", required by "), draw-and-highlight(self.expected-loc),
          ED.text(", but instead found "), ED.code(ED.text(self.bad-name)), ED.text(".")]]
    end
  | incorrect-type-expression(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc, e :: A.Expr) with:
    render-fancy-reason(self):
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the type instantiation")],
       [ED.para:
          ED.code([ED.sequence:
              ED.highlight(ED.h-sequence(self.ann.ann.tosource().pretty(80).map(ED.text),""), [list: self.ann.ann.l], 0),
              ED.text("<"),
              ED.h-sequence(self.ann.args.map(lam(ann):
                  ED.highlight(ED.h-sequence(ann.tosource().pretty(80).map(ED.text), ""), [list: ann.l], 1) end), ","),
              ED.text(">")])],
        [ED.para:
          ED.text("should give exactly the same number of parameters as the type accepts. However, the type instantiation is given "),
          ED.highlight(ED.ed-params(self.given.length()), self.ann.args.map(_.l), 1),
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
    render-fancy-reason(self):
      ed-applicant = ED.highlight(ED.text("applicant"), [list: self.app-expr._fun.l], 0)
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
          ED.highlight(ED.ed-args(self.app-expr.args.length()), self.app-expr.args.map(_.l), 1),
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
  | method-missing-self(method-expr :: A.Expr) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the method expression")],
        [ED.para:
          ED.code(ED.v-sequence(self.method-expr.tosource().pretty(800).map(ED.text)))],
        [ED.para:
          ED.text("at "),
          ED.loc(self.method-expr.l),
          ED.text(" requires at least a 'self' argument.")]]
    end
  | apply-non-function(app-expr :: A.Expr, typ) with:
    render-fancy-reason(self):
      ed-applicant = ED.highlight(ED.text("applicant"), [list: self.app-expr._fun.l], 0)
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the function application expression")],
        [ED.para:
          ED.code([ED.sequence:
              ED.highlight(ED.h-sequence(self.app-expr._fun.tosource().pretty(999).map(ED.text),""),[list: self.app-expr._fun.l],0),
              ED.text("("),
              ED.h-sequence(
                self.app-expr.args.map(
                  lam(arg):ED.h-sequence(arg.tosource().pretty(999).map(ED.text),"") end), ", "),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the object type")],
         ED.highlight(ED.embed(self.obj), [list: self.obj-loc], 0),
        [ED.para:
          ED.text("does not have a field named "),
          ED.code(ED.highlight(ED.text(self.field-name), [list: self.access-loc], 1))]]
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("A variant may not have the same name as any other variant in the type, but the declaration of a variant named "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.found], 0)),
          ED.text(" is preceeded by a declaration of a variant also named "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.previous], 1)),
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("A variant may not be matched more than once in a cases expression, but the branch matching the variant "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.found], 0)),
          ED.text(" is preceeded by a branch also matching "),
          ED.code(ED.highlight(ED.text(self.id), [list: self.previous], 1)),
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
  | unneccesary-branch(branch :: A.CasesBranch, data-type :: T.Type, cases-loc :: A.Loc) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the "),
          ED.highlight(ED.text("cases expression"),[list: self.cases-loc], 0),
          ED.text(" expects that all of its branches have a variant of the same name in the data-type "),
          ED.text(self.data-type.name),
          ED.text(". However, no variant named "),
          ED.code(ED.highlight(ED.text(self.branch.name), [list: self.branch.pat-loc], 1)),
          ED.text(" exists in "),
          ED.text(self.data-type.name),
          ED.text("'s "),
          ED.highlight(ED.text("variants"),self.data-type.variants.map(_.l), 2),
          ED.text(":")],
        ED.bulleted-sequence(self.data-type.variants.map(lam(variant):
            ED.code(ED.highlight(ED.text(variant.name), [list: variant.l], 2)) end))]
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
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the "),
          ED.highlight(ED.text("cases expression"),[list: self.loc], 0),
          ED.text(" has a branch for every variant of "),
          ED.code(ED.text(self.type-name)),
          ED.text(". Therefore, the "),
          ED.code(ED.text("else")),
          ED.text(" branch is unreachable.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("The else branch for the cases expression at "),
          draw-and-highlight(self.loc),
          ED.text(" is not needed since all variants of " + self.type-name + " have been exhausted.")]]
    end
  | non-exhaustive-pattern(missing :: List<T.TypeVariant>, type-name :: String, loc :: A.Loc) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("cases expression"),[list: self.loc], 0),
          ED.text(" should be able to handle all possible values of "),
          ED.code(ED.text(self.type-name)),
          ED.text(", but its branches cannot handle "),
          ED.highlight(ED.text(
              if self.missing.length() > 1: "several variants"
              else: "a variant"
              end), self.missing.map(_.l), 1),
          ED.text(".")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The cases expression at"),
          draw-and-highlight(self.loc),
          ED.text("does not exhaust all variants of " + self.type-name
            + ". It is missing: " + self.missing.map(_.name).join-str(", ") + ".")]]
    end
  | cant-match-on(ann, type-name :: String, loc :: A.Loc) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("A "),
          ED.code(ED.highlight(ED.text("cases expressions"), [list: self.loc], 0)),
          ED.text(" can only branch on variants of "),
          ED.code(ED.text("data")),
          ED.text(" types. The type "),
          ED.code(ED.highlight(ED.text(self.type-name), [list: self.ann.l], 1)),
          ED.text(" cannot be used in cases expressions.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type specified " + self.type-name),
          ED.text("at"),
          draw-and-highlight(self.loc),
          ED.text("cannot be used in a cases expression.")]]
    end
  | different-branch-types(l, branch-types) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The branches of this expression evaluate to different types and no common type encompasses all of them:")],
        ED.bulleted-sequence(map_n(lam(n, branch):
            ED.highlight(ED.embed(branch), [list: branch.l], n) end,
            0, self.branch-types))]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The branches of this expression evaluate to different types and no common type encompasses all of them:")],
        ED.bulleted-sequence(map_n(lam(n, branch):
         [ED.sequence:
              ED.loc(branch.l), ED.text(" has type "), ED.embed(branch)] end,
            0, self.branch-types))]
    end
  | incorrect-number-of-bindings(branch :: A.CasesBranch, variant :: T.TypeVariant) with:
    render-fancy-reason(self):
      fun ed-fields(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " field" else: " fields" end)]
      end
      [ED.error:
        [ED.para:
          ED.text("The type checker expects that the "),
          ED.highlight(ED.text("pattern"), [list: self.branch.pat-loc], 0),
          ED.text(" in the cases branch has the same number of "),
          ED.highlight(ED.text("field bindings"), self.branch.args.map(_.l), 1),
          ED.text(" as the data variant "),
          ED.code(ED.highlight(ED.text(self.variant.name), [list: self.variant.l], 2)),
          ED.text(" has "),
          ED.highlight(ED.text("fields"), [list: A.dummy-loc], 3),
          ED.text(". However, the branch pattern binds "),
          ED.highlight(ed-fields(self.branch.args.length()), self.branch.args.map(_.l), 1),
          ED.text(" and the variant is declared as having "),
          ED.highlight(ed-fields(self.variant.fields.length()), [list: A.dummy-loc], 3)]]
    end,
    render-reason(self):
      fun ed-fields(n):
        [ED.sequence:
          ED.embed(n),
          ED.text(if n == 1: " field" else: " fields" end)]
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
    render-fancy-reason(self):
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The type checker rejected your program because the cases branch named "),
            ED.code(ED.highlight(ED.text(self.name), [list: self.branch-loc], 0)),
            ED.text(" has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The type checker rejected your program because the cases branch named "),
            ED.code(ED.highlight(ED.text(self.name), [list: self.branch-loc], 0)),
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
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The data type"),  ED.code(ED.text(self.data-type)),
          ED.text("does not take any parameters, but is given some at"),
          draw-and-highlight(self.loc)]]
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("In the type at"), draw-and-highlight(self.loc),
          ED.text("there was not enough information to instantiate the type, "
            + "or the given arguments are incompatible.")]]
    end
  | unable-to-infer(loc :: A.Loc) with:
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Unable to infer the type of "), draw-and-highlight(self.loc),
          ED.text(". Please add an annotation.")]]
    end
  | toplevel-unann(arg :: A.Bind) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("argument"), [list: self.arg.l], 0),
          ED.text(" at "),
          ED.cmcode(self.arg.l),
          ED.text(" needs a type annotation.")]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.text("argument at"), draw-and-highlight(self.arg.l),
          ED.text(" needs a type annotation.")]]
    end
  | binop-type-error(binop :: A.Expr, tl :: T.Type, tr :: T.Type, etl :: T.Type, etr :: T.Type) with:
    render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The typechecker thinks there's a problem with the "),
          ED.code(ED.highlight(ED.text(self.binop.op),[list: self.binop.op-l], 0)),
          ED.text(" binary operator expression:")],
         ED.cmcode(self.binop.l),
        [ED.para:
          ED.text("where the it thinks the "),
          ED.highlight(ED.text("left hand side"), [list: self.binop.left.l], 1),
          ED.text(" is a "), ED.embed(self.tl),
          ED.text(" and the "),
          ED.highlight(ED.text("right hand side"), [list: self.binop.right.l], 2),
          ED.text(" is a "), ED.embed(self.tr), ED.text(".")],
        [ED.para:
          ED.text("When the type checker sees a "),
          ED.highlight(ED.embed(self.etl), [list: self.binop.left.l], 1),
          ED.text("to the left of a "),
          ED.code(ED.highlight(ED.text(self.binop.op),[list: self.binop.op-l], 0)),
          ED.text(" it thinks that the "),
          ED.highlight(ED.text("right hand side"), [list: self.binop.right.l], 2),
          ED.text(" should be a "),
          ED.embed(self.etr)]]
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The typechecker thinks there's a problem with the "),
          ED.code(ED.highlight(ED.text(self.binop.op),[list: self.binop.op-l], 0)),
          ED.text(" binary operator expression at "), ED.loc(self.binop.op-l)],
        [ED.para:
          ED.text("where the it thinks the "),
          ED.highlight(ED.text("left hand side"), [list: self.binop.left.l], 1),
          ED.text(" is a "), ED.embed(self.tl),
          ED.text(" and the "),
          ED.highlight(ED.text("right hand side"), [list: self.binop.right.l], 2),
          ED.text(" is a "), ED.embed(self.tr), ED.text(".")],
        [ED.para:
          ED.text("When the type checker sees a "),
          ED.highlight(ED.embed(self.tl), [list: self.binop.left.l], 1),
          ED.text("to the left of a "),
          ED.code(ED.highlight(ED.text(self.binop.op),[list: self.binop.op-l], 0)),
          ED.text(" it thinks that the "),
          ED.highlight(ED.text("right hand side"), [list: self.binop.right.l], 2),
          ED.text(" should be a "),
          ED.embed(self.etr)]]
    end
  | cant-typecheck(reason :: String, loc :: A.Loc) with:
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This program cannot be type-checked. Please send it to the developers. " + "The reason that it cannot be type-checked is: " + self.reason +
        " at "), ED.cmcode(self.loc)]]
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    #### TODO ###
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text(self.message + " (found at "),
          draw-and-highlight(self.blame-loc),
          ED.text(")")]]
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    #### TODO ###
    render-fancy-reason(self):
      self.render-reason()
    end,
    render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("There is no module imported with the name " + self.mod-name),
          ED.text(" (used at "),
          draw-and-highlight(self.loc),
          ED.text(")")]]
    end
end

type CompileOptions = {
  check-mode :: Boolean,
  type-check :: Boolean,
  allow-shadowed :: Boolean,
  collect-all :: Boolean,
  ignore-unbound :: Boolean,
  proper-tail-calls :: Boolean,
  compile-module :: Boolean,
  compiled-cache :: String,
  standalone-file :: String,
  on-compile :: Function # NOTE: skipping types because the are in compile-lib
}

default-compile-options = {
  check-mode : true,
  type-check : false,
  allow-shadowed : false,
  collect-all: false,
  ignore-unbound: false,
  proper-tail-calls: true,
  compile-module: true,
  compiled-cache: "compiled",
  on-compile: lam(locator, loadable): nothing end,
  standalone-file: "src/js/base/handalone.js"
}

t-pred = t-arrow([list: t-top], t-boolean)
t-pred2 = t-arrow([list: t-top, t-top], t-boolean)

t-number-binop = t-arrow([list: t-number, t-number], t-number)
t-number-unop = t-arrow([list: t-number], t-number)
t-number-pred1 = t-arrow([list: t-number], t-boolean)
t-within-num = t-arrow([list: t-number], t-arrow([list: t-number, t-number], t-boolean))
t-within-any = t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean))

fun t-forall1(f):
  n = A.global-names.make-atom("a")
  t-forall([list: t-var(n)], f(t-var(n)))
end

runtime-provides = provides("builtin://global",
  [string-dict:
    "test-print", t-forall1(lam(a): t-arrow([list: a], a) end),
    "print", t-forall1(lam(a): t-arrow([list: a], a) end),
    "display", t-forall1(lam(a): t-arrow([list: a], a) end),
    "print-error", t-forall1(lam(a): t-arrow([list: a], a) end),
    "display-error", t-forall1(lam(a): t-arrow([list: a], a) end),
    "tostring", t-arrow([list: t-top], t-str),
    "torepr", t-arrow([list: t-top], t-str),
    "brander", t-top,
    "raise", t-arrow([list: t-top], t-bot),
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
    "not", t-arrow([list: t-boolean], t-boolean),
    "is-nothing", t-pred,
    "is-number", t-pred,
    "is-string", t-pred,
    "is-boolean", t-pred,
    "is-object", t-pred,
    "is-function", t-pred,
    "is-raw-array", t-pred,
    "gensym", t-top,
    "random", t-top,
    "run-task", t-top,
    "_plus", t-top,
    "_minus", t-top,
    "_times", t-top,
    "_divide", t-top,
    "_lessthan", t-top,
    "_lessequal", t-top,
    "_greaterthan", t-top,
    "_greaterequal", t-top,
    "string-equal", t-top,
    "string-contains", t-top,
    "string-append", t-top,
    "string-length", t-top,
    "string-tonumber", t-top,
    "string-to-number", t-arrow([list: t-string], t-option(t-number)),
    "string-repeat", t-top,
    "string-substring", t-top,
    "string-replace", t-top,
    "string-split", t-top,
    "string-split-all", t-top,
    "string-char-at", t-top,
    "string-toupper", t-top,
    "string-tolower", t-top,
    "string-explode", t-top,
    "string-index-of", t-top,
    "string-to-code-point", t-top,
    "string-from-code-point", t-top,
    "string-to-code-points", t-top,
    "string-from-code-points", t-top,
    "time-now", t-number-unop,
    "num-random", t-number-unop,
    "num-random-seed", t-arrow([list: t-number], t-nothing),
    "num-max", t-number-binop,
    "num-min", t-number-binop,
    "num-equal", t-arrow([list: t-number, t-number], t-boolean),
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
    "num-tostring", t-arrow([list: t-number], t-string),
    "num-to-string", t-arrow([list: t-number], t-string),
    "num-to-string-digits", t-arrow([list: t-number, t-number], t-string),
    "num-within", t-within-num,
    "num-within-rel", t-within-num,
    "num-within-abs", t-within-num,
    "within-rel", t-within-any,
    "within-rel-now", t-within-any,
    "within-abs", t-within-any,
    "within-abs-now", t-within-any,
    "within", t-within-any,
    "raw-array-get", t-top,
    "raw-array-set", t-top,
    "raw-array-of", t-top,
    "raw-array-length", t-top,
    "raw-array-to-list", t-top,
    "raw-array-fold", t-top,
    "raw-array", t-record(
      [list:
        t-member("make", t-forall1(lam(a): t-arrow([list: t-array(a)], t-array(a)) end)),
        t-member("make0", t-forall1(lam(a): t-arrow([list: ], t-array(a)) end)),
        t-member("make1", t-forall1(lam(a): t-arrow([list: a], t-array(a)) end)),
        t-member("make2", t-forall1(lam(a): t-arrow([list: a, a], t-array(a)) end)),
        t-member("make3", t-forall1(lam(a): t-arrow([list: a, a, a], t-array(a)) end)),
        t-member("make4", t-forall1(lam(a): t-arrow([list: a, a, a, a], t-array(a)) end)),
        t-member("make5", t-forall1(lam(a): t-arrow([list: a, a, a, a, a], t-array(a)) end))
      ]),
    "ref-get", t-top,
    "ref-set", t-top,
    "ref-freeze", t-top,
    "equal-always", t-pred2,
    "equal-always3", t-top,
    "equal-now", t-pred2,
    "equal-now3", t-top,
    "identical", t-pred2,
    "identical3", T.t-top,
    "exn-unwrap", T.t-top
  ],
  [string-dict:
     "Number", t-top,
     "Exactnum", t-top,
     "Roughnum", t-top,
     "NumInteger", t-top,
     "NumRational", t-top,
     "NumPositive", t-top,
     "NumNegative", t-top,
     "NumNonPositive", t-top,
     "NumNonNegative", t-top,
     "String", t-str,
     "Function", t-top,
     "Boolean", t-top,
     "Object", t-top,
     "Method", t-top,
     "Nothing", t-top,
     "RawArray", t-top  ],
  [string-dict:])

runtime-builtins = for fold(rb from [string-dict:], k from runtime-provides.values.keys().to-list()):
  rb.set(k, "builtin(global)")
end

runtime-types = for fold(rt from [string-dict:], k from runtime-provides.aliases.keys().to-list()):
  rt.set(k, "builtin(global)")
end
shadow runtime-types = for fold(rt from runtime-types, k from runtime-provides.data-definitions.keys().to-list()):
  rt.set(k, "builtin(global)")
end

no-builtins = compile-env(globals([string-dict: ], [string-dict: ]), [string-dict: "builtin(global)", runtime-provides])

minimal-builtins = compile-env(globals(runtime-builtins, runtime-types), [string-dict: "builtin(global)", runtime-provides])

standard-globals = globals(runtime-builtins, runtime-types)
standard-builtins = compile-env(globals(runtime-builtins, runtime-types), [string-dict: "builtin(global)", runtime-provides])

minimal-imports = extra-imports(empty)

standard-imports = extra-imports(
   [list:
      extra-import(builtin("global"), "$global", [list:], [list:]),
      extra-import(builtin("base"), "$base", [list:], [list:]),
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
          "is-List",
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
          "fold4"
        ],
        [list: "List"]),
      extra-import(builtin("option"), "option", [list:
          "Option",
          "is-Option",
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
          "list-set",
          "empty-set",
          "empty-list-set",
          "empty-tree-set",
          "list-to-set",
          "list-to-list-set",
          "list-to-tree-set"
        ],
        [list: "Set"])
    ])
