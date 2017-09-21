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
t-arrow = T.t-arrow(_, _, A.dummy-loc, false)
t-top = T.t-top(A.dummy-loc, false)
t-bot = T.t-bot(A.dummy-loc, false)
t-record = T.t-record(_, A.dummy-loc, false)
t-forall = T.t-forall(_, _, A.dummy-loc, false)
t-var = T.t-var(_, A.dummy-loc, false)
t-array = T.t-array(_, A.dummy-loc)
t-string = T.t-string(A.dummy-loc)
t-option = T.t-option(_, A.dummy-loc)
t-data = T.t-data(_, _, _, _, A.dummy-loc)
t-variant = T.t-variant(_, _, _, A.dummy-loc)
t-singleton-variant = T.t-singleton-variant(_, _, A.dummy-loc)
t-app = T.t-app(_, _, A.dummy-loc, false)
t-name = T.t-name(_, _, A.dummy-loc, false)

is-t-app = T.is-t-app

type URI = String
type StringDict = SD.StringDict
string-dict = SD.string-dict

is-s-block = A.is-s-block

type Loc = SL.Srcloc

data Dependency:
  | dependency(protocol :: String, arguments :: List<String>)
    with:
    method key(self): self.protocol + "(" + self.arguments.join-str(", ") + ")" end
  | builtin(modname :: String)
    with:
    method key(self): "builtin(" + self.modname + ")" end
end

data NativeModule:
  | requirejs(path :: String)
end

data BindOrigin:
  | bo-local(loc :: Loc)
  | bo-module(mod :: Option<A.ImportType>, uri :: URI)
end

data ValueBinder:
  | vb-letrec
  | vb-let
  | vb-var
  | vb-module(uri :: URI) # The A in import ast as A (with URI determined from compile env)
end

data ValueBind:
  | value-bind(
      origin :: BindOrigin,
      binder :: ValueBinder,
      atom :: A.Name,
      ann :: A.Ann,
      expr :: Option<A.Expr>)
end

data TypeBinder:
  | tb-type-let
  | tb-type-var
  | tb-module(uri :: URI)
end

data TypeBind:
  | type-bind(
      origin :: BindOrigin,
      binder :: TypeBinder,
      atom :: A.Name,
      ann :: Option<A.Ann>)
end

#|
data ScopeBinding:
  | letrec-bind(loc, atom :: A.Name, ann :: A.Ann, expr :: Option<A.Expr>)
  | let-bind(loc, atom :: A.Name, ann :: A.Ann, expr :: Option<A.Expr>)
  | var-bind(loc, atom :: A.Name, ann :: A.Ann, expr :: Option<A.Expr>)
  | global-bind(loc, atom :: A.Name, expr :: Option<A.Expr>)
  | module-bind(loc, atom :: A.Name, mod :: A.ImportType, expr :: Option<A.Expr>)
end

data TypeBinding:
  | let-type-bind(loc, atom :: A.Name, ann :: Option<A.Ann>)
  | type-var-bind(loc, atom :: A.Name, ann :: Option<A.Ann>)
  | global-type-bind(loc, atom :: A.Name, ann :: Option<A.Ann>)
  | module-type-bind(loc, atom :: A.Name, mod :: A.ImportType, ann :: Option<A.Ann>)
end
|#

data ScopeResolution:
  | resolved-scope(ast :: A.Program, errors :: List<CompileError>)
end

data NameResolution:
  | resolved-names(
      ast :: A.Program,
      errors :: List<CompileError>,
      bindings :: SD.MutableStringDict<ValueBind>,
      type-bindings :: SD.MutableStringDict<TypeBind>,
      datatypes :: SD.MutableStringDict<A.Expr>)
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
  | globals(values :: StringDict<URI>, types :: StringDict<URI>)
end

data ValueExport:
  | v-just-type(t :: T.Type)
  | v-var(t :: T.Type)
  | v-fun(t :: T.Type, name :: String, flatness :: Option<Number>)
end

data Provides:
  | provides(
      from-uri :: URI,
      values :: StringDict<ValueExport>,
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

fun value-export-from-raw(uri, val-export, tyvar-env :: SD.StringDict<T.Type>) -> ValueExport block:
  t = val-export.tag
  typ = type-from-raw(uri, val-export.typ, tyvar-env)
  if t == "v-fun":
    v-fun(typ, t, none)
  else:
    v-just-type(typ)
  end
end

fun type-from-raw(uri, typ, tyvar-env :: SD.StringDict<T.Type>) block:
  tfr = type-from-raw(uri, _, tyvar-env)
  # TODO(joe): Make this do something intelligent when location information
  # is available
  l = SL.builtin(uri)
  t = typ.tag
  #print("\n\ntyp: " + tostring(typ))
  ask:
    | t == "any" then: T.t-top(l, false)
    | t == "bot" then: T.t-bot(l, false)
    | t == "record" then:
      T.t-record(typ.fields.foldl(lam(f, fields): fields.set(f.name, tfr(f.value)) end, [string-dict: ]), l, false)
    | t == "tuple" then:
      T.t-tuple(for map(e from typ.elts): tfr(e) end, l, false)
    | t == "name" then:
      if typ.origin.import-type == "$ELF":
        T.t-name(T.local, A.s-type-global(typ.name), l, false)
      else if typ.origin.import-type == "uri":
        T.t-name(T.module-uri(typ.origin.uri), A.s-type-global(typ.name), l, false)
      else:
        T.t-name(T.dependency(make-dep(typ.origin)), A.s-type-global(typ.name), l, false)
      end
    | t == "tyvar" then:
      cases(Option<T.Type>) tyvar-env.get(typ.name):
        | none => raise("Unbound type variable " + typ.name + " in provided type.")
        | some(tv) => T.t-var(tv, l, false)
      end
    | t == "forall" then:
      new-env = for fold(new-env from tyvar-env, a from typ.args):
        tvn = A.global-names.make-atom(a)
        new-env.set(a, tvn)
      end
      params = for SD.map-keys(k from new-env):
        T.t-var(new-env.get-value(k), l, false)
      end
      T.t-forall(params, type-from-raw(uri, typ.onto, new-env), l, false)
    | t == "tyapp" then:
      T.t-app(tfr(typ.onto), map(tfr, typ.args), l, false)
    | t == "arrow" then:
      T.t-arrow(map(tfr, typ.args), tfr(typ.ret), l, false)
    | otherwise: raise("Unknown raw tag for type: " + t)
  end
end

fun tvariant-from-raw(uri, tvariant, env):
  l = SL.builtin(uri)
  t = tvariant.tag
  ask:
    | t == "variant" then:
      members = tvariant.vmembers.foldr(lam(tm, members):
        link({tm.name; type-from-raw(uri, tm.typ, env)}, members)
      end, empty)
      t-variant(tvariant.name, members, [string-dict: ])
    | t == "singleton-variant" then:
      t-singleton-variant(tvariant.name, [string-dict: ])
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
    params = for SD.map-keys(k from pdict):
      T.t-var(pdict.get-value(k), l, false)
    end
    variants = map(tvariant-from-raw(uri, _, pdict), datatyp.variants)
    members = datatyp.methods.foldl(lam(tm, members):
      members.set(tm.name, type-from-raw(uri, tm.value, pdict))
    end, [string-dict: ])
    t-data(datatyp.name, params, variants, members)
  end
end

fun provides-from-raw-provides(uri, raw):
  values = raw.values
  vdict = for fold(vdict from SD.make-string-dict(), v from raw.values):
    if is-string(v) block:
      vdict.set(v, v-just-type(t-top))
    else:
      if v.value.bind == "var":
        vdict.set(v.name, v-var(type-from-raw(uri, v.value.typ, SD.make-string-dict())))
      else if v.value.bind == "fun":
        flatness = if is-number(v.value.flatness):
          some(v.value.flatness)
        else:
          none
        end
        vdict.set(v.name, v-fun(type-from-raw(uri, v.value.typ, SD.make-string-dict()), v.value.name, flatness))
      else:
        vdict.set(v.name, v-just-type(type-from-raw(uri, v.value.typ, SD.make-string-dict())))
      end
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
    method render-fancy-reason(self):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        draw-and-highlight(self.loc)]
    end
  | wf-empty-block(loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("block"),[list: self.loc], 0),
          ED.text(" is empty:")],
        ED.cmcode(self.loc)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret rejected your program because there is an empty block at")],
        [ED.para: draw-and-highlight(self.loc)]]
    end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    method render-fancy-reason(self):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        ED.v-sequence(self.loc.map(lam(l): [ED.para: draw-and-highlight(l)] end))]
    end
  | reserved-name(loc :: Loc, id :: String) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Reading a "),
          ED.highlight(ED.text("name"), [list: self.loc], 0),
          ED.text(" errored:")],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text("This name is reserved is reserved by Pyret, and cannot be used as an identifier.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The name "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.loc),
          ED.text(" is reserved by Pyret, and cannot be used as an identifier.")]]
    end
  | contract-on-import(loc :: Loc, name :: String, import-type :: A.ImportType) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Contracts for functions can only be defined once, and the contract for "),
          ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0),
          ED.text(" is already defined in the "),
          ED.highlight(ED.code(ED.text(self.import-type.tosource().pretty(1000).join-str(""))),
            [list: self.import-type.l], 1),
          ED.text(" library.")],
        ED.cmcode(self.loc)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Contracts for functions can only be defined once, and the contract for "),
          ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
          ED.text(" is already defined in the "),
          ED.code(ED.text(self.import-type.tosource().pretty(1000).join-str(""))),
          ED.text(" library.")]]
    end
  | contract-redefined(loc :: Loc, name :: String, defn-loc :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Contracts for functions can only be defined once, and the contract for "),
          ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0),
          ED.text(" is "),
          ED.highlight(ED.text("already defined"), [list: self.defn-loc], -1),
          ED.text(": ")],
        ED.cmcode(self.defn-loc)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Contracts for functions can only be defined once, and the contract for "),
          ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
          ED.text(" is already defined at "), ED.loc(self.defn-loc)]]
    end
  | contract-non-function(loc :: Loc, name :: String, defn-loc :: Loc, defn-is-function :: Boolean) with:
    method render-fancy-reason(self):
      if self.defn-is-function:
        [ED.error:
          [ED.para:
            ED.text("The contract for "),
            ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0),
            ED.text(" is not a valid function contract, but "),
            ED.highlight(ED.code(ED.text(self.name)), [list: self.defn-loc], -1),
            ED.text(" is defined as a function.")],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("The contract and the "),
            ED.highlight(ED.text("definition"), [list: self.defn-loc], -1),
            ED.text(" must be consistent.")],
          ED.cmcode(self.defn-loc)]
      else:
        [ED.error:
          [ED.para:
            ED.text("The contract for "),
            ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0),
            ED.text(" is a function contract, but "),
            ED.highlight(ED.code(ED.text(self.name)), [list: self.defn-loc], -1),
            ED.text(" is not defined as a function.")],
          ED.cmcode(self.loc),
          [ED.para:
            ED.text("The contract and the "),
            ED.highlight(ED.text("definition"), [list: self.defn-loc], -1),
            ED.text(" must be consistent.")],
          ED.cmcode(self.defn-loc)]
      end
    end,
    method render-reason(self):
      if self.defn-is-function:
        [ED.error:
          [ED.para:
            ED.text("The contract for "),
            ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
            ED.text(" is not a valid function contract, but "),
            ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.defn-loc),
            ED.text(" is defined as a function.")],
          [ED.para: ED.text("The contract and the definition must be consistent.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The contract for "),
            ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
            ED.text(" is a function contract, but "),
            ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.defn-loc),
            ED.text(" is not defined as a function.")],
          [ED.para: ED.text("The contract and the definition must be consistent.")]]
      end
    end
  | contract-inconsistent-names(loc :: Loc, name :: String, defn-loc :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The contract for "),
          ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0)],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text("specifies arguments that are inconsistent with the "),
          ED.highlight(ED.text("associated definition"), [list: self.defn-loc], -1), ED.text(":")],
        ED.cmcode(self.defn-loc)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The contract for "),
          ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
          ED.text(" specifies arguments that are inconsistent with the definition at "), ED.loc(self.defn-loc)]]
    end
  | contract-unused(loc :: Loc, name :: String) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The contract for "),
          ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0)],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text(" does not match the name of any function definition.")],
        [ED.para:
          ED.text("Contracts must appear just before their function's definition (or just before the function's examples block).  Check the spelling of this contract's name, or move it closer to its function if necessary.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The contract for "), ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
          ED.text(" does not match the name of any function definition.")],
        [ED.para:
          ED.text("Contracts must appear just before their function's definition (or just before the function's examples block).  Check the spelling of this contract's name, or move it closer to its function if necessary.")]]
    end
  | contract-bad-loc(loc :: Loc, name :: String, defn-loc :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Contracts must appear just before their associated definition (or just before the function's examples block).  The contract for "),
          ED.highlight(ED.code(ED.text(self.name)), [list: self.loc], 0)],
        ED.cmcode(self.loc),
        [ED.para: ED.text(" comes after its "),
          ED.highlight(ED.text("associated definition"), [list: self.defn-loc], -1), ED.text(".")],
        ED.cmcode(self.defn-loc),
        [ED.para: ED.text("Move the contract just before its function.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Contracts must appear just before their associated definition (or just before the function's examples block).  The contract for "), ED.code(ED.text(self.name)), ED.text(" at "), ED.loc(self.loc),
          ED.text(" comes after its associated definition at "), ED.loc(self.defn-loc), ED.text(". Move the contract just before its function.")]]
    end
  | zero-fraction(loc, numerator) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Reading a "),
          ED.highlight(ED.text("fraction literal expression"), [ED.locs: self.loc], 0),
          ED.text(" errored:")],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text("Its denominator is zero.")]]
    end,
    method render-reason(self):
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
  | mixed-binops(exp-loc, op-a-name, op-a-loc, op-b-name, op-b-loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Reading this "),
          ED.highlight(ED.text("expression"), [ED.locs: self.exp-loc], -1),
          ED.text(" errored:")],
        ED.cmcode(self.exp-loc),
        [ED.para:
          ED.text("The "),
          ED.code(ED.highlight(ED.text(self.op-a-name),[list: self.op-a-loc], 0)),
          ED.text(" operation is at the same level as the "),
          ED.code(ED.highlight(ED.text(self.op-b-name),[list: self.op-b-loc], 1)),
          ED.text(" operation.")],
        [ED.para:
          ED.text("Use parentheses to group the operations and to make the order of operations clear.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Operators of different kinds cannot be mixed at the same level, but "),
          ED.code(ED.text(self.op-a-name)),
          ED.text(" is at "),
          ED.loc(self.op-a-loc),
          ED.text(" at the same level as "),
          ED.code(ED.text(self.op-b-name)),
          ED.text(" at "),
          ED.loc(self.op-b-loc),
          ED.text(". Use parentheses to group the operations and to make the order of operations clear.")]]
    end
  | block-ending(l :: Loc, block-loc :: Loc, kind) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("block"), [list: self.block-loc], -1),
          ED.text(" ends with a "),
          ED.highlight(ED.text(self.kind), [list: self.l], 0),
          ED.text(":")],
        ED.cmcode(self.l),
        [ED.para:
          ED.text("Blocks should end with an expression")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The block at "),
          ED.loc(self.block-loc),
          ED.text(" ends with a " + self.kind + " at "),
          ED.loc(self.l),
          ED.text(". Blocks should end with an expression.")]]
    end
  | single-branch-if(expr :: A.Expr) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("An "),
          ED.highlight(ED.text("if-expression"), [list: self.expr.l], -1),
          ED.text(" has only one "),
          ED.highlight(ED.text("branch"), [list: self.expr.branches.first.l], 0),
          ED.text(":")],
        ED.cmcode(self.expr.l)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("If-expressions may not only have one branch, but the if-expression at "),
          ED.loc(self.expr.l),
          ED.text(" does not have any other branches.")]]
    end
  | unwelcome-where(kind, loc, block-loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("A "),
          ED.highlight(ED.code(ED.text("where")), [list: self.block-loc], 0),
          ED.text(" can't be added to a "),
          ED.highlight(ED.text(self.kind), [list: self.loc], -1),
          ED.text(":")],
        ED.cmcode(self.block-loc),
        [ED.para:
          ED.text("A "),
          ED.code(ED.text("where")),
          ED.text(" block may only be added to named function declarations"),
          ED.text(".")]]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.highlight(ED.text("This"),[list: self.expr.l], 0),
          ED.text(" is not a testing statement:")],
        ED.cmcode(self.expr.l),
        [ED.para:
          ED.code(ED.text("example")),
          ED.text(" blocks must only contain testing statements.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.code(ED.text("example")),
          ED.text(" blocks must only contain testing statements, but the statement at "),
          ED.loc(self.expr.l),
          ED.text(" isn't a testing statement.")]]
    end
  | tuple-get-bad-index(l, tup, index, index-loc) with:
    method render-fancy-reason(self):
      if not(num-is-integer(self.index)):
        [ED.error:
          [ED.para:
            ED.text("This "),
            ED.highlight(ED.text("tuple indexing"), [list: self.l], -1),
            ED.text(" expression cannot extract a "),
            ED.highlight(ED.text("non-integer position"),[list: self.index-loc],0),
            ED.text(".")],
          ED.cmcode(self.l)]
      else if self.index < 0:
        [ED.error:
          [ED.para:
            ED.text("This "),
            ED.highlight(ED.text("tuple indexing"), [list: self.l], -1),
            ED.text(" expression cannot extract a "),
            ED.highlight(ED.text("negative position"),[list: self.index-loc],0),
            ED.text(".")],
          ED.cmcode(self.l)]
      else:
        [ED.error:
          [ED.para:
            ED.text("This "),
            ED.highlight(ED.text("tuple indexing"), [list: self.l], -1),
            ED.text(" expression cannot extract an "),
            ED.highlight(ED.text("index"),[list: self.index-loc],0),
            ED.text(" that large. There are no tuples that big.")],
          ED.cmcode(self.l)]
      end
    end,
    method render-reason(self):
      if not(num-is-integer(self.index)):
        [ED.error:
          [ED.para:
            ED.text("The tuple indexing expression at "),
            ED.loc(self.l),
            ED.text(" was given an invalid, non-integer index.")]]
      else if self.index < 0:
        [ED.error:
          [ED.para:
            ED.text("The tuple indexing expression at "),
            ED.loc(self.l),
            ED.text(" was given an invalid, negative index.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The tuple indexing expression at "),
            ED.loc(self.l),
            ED.text(" was given an index bigger than any tuple.")]]
      end
    end
  | import-arity-mismatch(l, kind, args, expected-arity, expected-args) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight([ED.sequence: ED.code(ED.text(self.kind)), ED.text(" import statement")],
                       [list: self.l], -1),
          ED.text(":")],
        ED.cmcode(self.l),
        [ED.para:
          ED.text("expects "),
          ED.ed-args(self.expected-arity),
          ED.text(":")],
         ED.bulleted-sequence(self.expected-args.map(ED.text))]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.code(ED.text(self.kind)),
          ED.text(" import statement at "),
          ED.loc(self.l),
          ED.text(" expects "),
          ED.ed-args(self.expected-arity),
          ED.text(":")],
         ED.bulleted-sequence(self.expected-args.map(ED.text))]
    end
  | no-arguments(expr) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("method declaration"), [list: self.expr.l], 0),
          ED.text(" should accept at least one argument:")],
        ED.cmcode(self.expr.l),
        [ED.para:
          ED.text("When a method is applied, the first argument is a reference to the object it belongs to.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Method declarations should accept at least one argument, but the method declaration at "),
          ED.loc(self.expr.l),
          ED.text(" has no arguments. When a method is applied, the first argument is a reference to the object it belongs to.")]]
    end
  | non-toplevel(kind, l :: Loc, parent-loc :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.code(ED.highlight(ED.text(self.kind), [ED.locs: self.l], 0)),
          ED.text(" is inside "),
          ED.highlight(ED.text("another block"), [list: self.parent-loc], -1),
          ED.text(":")],
        ED.cmcode(self.l),
        [ED.para:
          ED.text(self.kind),
          ED.text(" may only occur at the top-level of the program.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("You may only define the "),
          ED.code(ED.text(self.kind)),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" at the top-level.")]]
    end
  | unwelcome-test(loc :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("testing statement"),[list: self.loc], 0)],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text("is not inside a "),
          ED.code(ED.text("check")),
          ED.text(" or "),
          ED.code(ED.text("where")),
          ED.text(" block.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The testing statement at "),
          ED.loc(self.loc),
          ED.text(" is not inside a "),
          ED.code(ED.text("check")),
          ED.text(" or "),
          ED.code(ED.text("where")),
          ED.text(" block.")]]
    end
  | unwelcome-test-refinement(refinement, op) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("testing operator"),[list: self.op.l], 0),
          ED.text(" may not be used with a "),
          ED.highlight(ED.text("refinement"),[list: self.refinement.l], 1),
          ED.text(":")],
        ED.cmcode(self.op.l + self.refinement.l)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The testing operator at "),
          ED.loc(self.op.l),
          ED.text(" may not be used with the refinement syntax, "),
          ED.code(ED.text("%(...)"))]]
    end
  | underscore-as(l :: Loc, kind) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
          ED.text(" cannot be used as "),
          ED.text(self.kind),
          ED.text(".")]]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("An underscore cannot be used for this "),
          ED.highlight(ED.text("pattern"), [ED.locs: self.l], 0),
          ED.text(" in a cases expression:")],
        ED.cmcode(self.l),
        [ED.para:
          ED.text("To match all cases not matched by the other branches, use the pattern "),
          ED.code(ED.text("else")),
          ED.text(" instead.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used as a pattern in a cases expression. To match all cases not matched by the previous branches, use the pattern "),
          ED.code(ED.text("else")),
          ED.text(" instead.")]]
    end
  | underscore-as-expr(l :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
          ED.text(" cannot be used where an expression is expected.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used where an expression is expected.")]]
    end
  | underscore-as-ann(l :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.highlight(ED.text("_"), [ED.locs: self.l], 0)),
          ED.text(" cannot be used where a type annotation is expected.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The underscore "),
          ED.code(ED.text("_")),
          ED.text(" at "),
          ED.loc(self.l),
          ED.text(" cannot be used where a type annotation is expected.")]]
    end
  | block-needed(expr-loc :: Loc, blocks :: List<A.Expr % (is-s-block)>) with:
    method render-fancy-reason(self):
      if self.blocks.length() > 1:
        [ED.error:
          [ED.para:
            ED.text("This expression contains one or more "),
            ED.highlight(ED.text("blocks"), self.blocks.map(_.l), -1),
            ED.text(" that contain "),
           ED.highlight(ED.text("multiple expressions"), A.flatten(self.blocks.map(_.stmts)).filter({(e):not(A.is-binder(e))}).map(_.l), 0),
            ED.text(":")],
          ED.cmcode(self.expr-loc),
          [ED.para:
            ED.text("Either simplify each of these blocks to a single expression, or mark the outer expression with"),
            ED.code(ED.text("block:")), ED.text("to indicate this is deliberate.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("This expression contains a "),
            ED.highlight(ED.text("block"),[list: self.blocks.first.l],-1),
            ED.text(" that contains "),
            ED.highlight(ED.text("multiple expressions"), A.flatten(self.blocks.map(_.stmts)).filter({(e):not(A.is-binder(e))}).map(_.l), 0),
            ED.text(".")],
          ED.cmcode(self.expr-loc),
          [ED.para:
            ED.text("Either simplify this block to a single expression, or mark the outer expression with "),
            ED.code(ED.text("block:")), ED.text(" to indicate this is deliberate.")]]
      end
    end,
    method render-reason(self):
      if self.blocks.length() > 1:
        [ED.error:
          [ED.para: ED.text("The expression at "), draw-and-highlight(self.expr-loc),
            ED.text(" contains several blocks that each contain multiple expressions:")],
          ED.v-sequence(self.blocks.map(_.l).map(draw-and-highlight)),
          [ED.para:
            ED.text("Either simplify each of these blocks to a single expression, or mark the outer expression with "),
            ED.code(ED.text("block:")), ED.text(" to indicate this is deliberate.")]]
      else:
        [ED.error:
          [ED.para: ED.text("The expression at "), draw-and-highlight(self.expr-loc),
            ED.text(" contains a block that contains multiple expressions:")],
          ED.v-sequence(self.blocks.map(_.l).map(draw-and-highlight)),
          [ED.para:
            ED.text("Either simplify this block to a single expression, or mark the outer expression with "),
            ED.code(ED.text("block:")), ED.text(" to indicate this is deliberate.")]]
      end
    end
  | unbound-id(id :: A.Expr) with:
    method render-fancy-reason(self):
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
              ED.text(" is unbound:")],
             ED.cmcode(self.id.l),
            [ED.para:
              ED.text("It is "),
              ED.highlight(ED.text("used"), [ED.locs: self.id.l], 0),
              ED.text(" but not previously defined.")]]
      end
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
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
    method render-reason(self):
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
    method render-fancy-reason(self):
      cases(SL.Srcloc) self.ann.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.ann.tosource().pretty(1000).first),
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
    method render-reason(self):
      cases(SL.Srcloc) self.ann.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.ann.tosource().pretty(1000).first), ED.text("at"),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          ann-name = if A.is-a-name(self.ann): self.ann.id.toname() else: self.ann.obj.toname() + "." + self.ann.field end
          [ED.error:
            [ED.para:
              ED.text("The name "),
              ED.code(ED.text(ann-name)),
              ED.text(" at "),
              ED.loc(self.ann.l),
              ED.text(" is used to indicate a type, but a definition of a type named "),
              ED.code(ED.text(ann-name)),
              ED.text(" could not be found.")]]
      end
    end
  | type-id-used-in-dot-lookup(loc :: Loc, name :: A.Name) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("name"), [ED.locs: self.loc], 0),
          ED.text(" is being used with a dot accessor as if to access a type within another module.")],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text("but it does not refer to a module.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("The name "),
          ED.text(tostring(self.name)),
          ED.text(" is being used with a dot accessor as if to access a type within another module at "),
          draw-and-highlight(self.loc),
          ED.text(", but it does not refer to a module.")]]
    end
  | type-id-used-as-value(id :: A.Name, origin :: BindOrigin) with:
    method render-fancy-reason(self):
      intro =
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("name"), [ED.locs: self.id.l], 0),
          ED.text(" is being used as a value:")]
      usage = ED.cmcode(self.id.l)
      cases(BindOrigin) self.origin:
        | bo-local(loc) =>
          [ED.error: intro, usage,
            [ED.para:
              ED.text("But it is "),
              ED.highlight(ED.text("defined as a type"), [ED.locs: loc], 1),
              ED.text(":")],
            ED.cmcode(loc)]
        | bo-module(_, uri) =>
          [ED.error: intro, usage,
            [ED.para:
              ED.text("But it is defined as a type in "),
              ED.embed(uri),
              ED.text(".")]]
      end
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("The name "),
          ED.text(self.id.s),
          ED.text(" is used as a value at "),
          draw-and-highlight(self.id.l),
          ED.text(", but it is defined as a type.")]]
    end
  | unexpected-type-var(loc :: Loc, name :: A.Name) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("identifier"), [ED.locs: self.loc], self.loc),
          ED.text(" is used in a dot-annotation")],
        ED.cmcode(self.loc),
        [ED.para:
          ED.text("but is bound as a type variable.")]]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
              draw-and-highlight(self.loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("This "),
              ED.highlight(ED.text("variable binding"), [list: self.loc], 0),
              ED.text(" is pointless:")],
            ED.cmcode(self.loc),
            [ED.para:
              ED.text("There is no name that can be used to mutate it later on.")]]
      end
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
              draw-and-highlight(self.loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("This "),
              ED.highlight(ED.text("recursive binding"), [list: self.loc], 0),
              ED.text(" is pointless:")],
            ED.cmcode(self.loc),
            [ED.para:
              ED.text("There isn't a name that can be used to make a recursive call.")]]
      end
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("This "),
              ED.highlight(ED.text("shadowing binding"), [list: self.loc], 0),
              ED.text(" is pointless:")],
            ED.cmcode(self.loc),
            [ED.para:
              ED.text("There is no name to shadow.")]]
      end
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      use-loc-color = 0
      def-loc-color = 1
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("variable assignment statement"), [ED.locs: self.iuse.l], use-loc-color)],
        ED.cmcode(self.iuse.l),
        [ED.para:
          ED.text(" expects the name "),
          ED.code(ED.highlight(ED.text(self.iuse.id.toname()), [ED.locs: self.iuse.l], use-loc-color)),
          ED.text(" to refer to a variable definition statement, but "),
          ED.code(ED.text(self.iuse.id.toname())),
          ED.text(" is declared by an "),
          ED.highlight(ED.text("identifier definition statement."), [ED.locs: self.idef], def-loc-color)],
          ED.cmcode(self.idef)]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The name "),
          ED.code(ED.text(self.id)),
          ED.text(" is both "),
          ED.highlight(ED.text("declared as a variable"), [ED.locs: self.var-loc], 0)],
        ED.cmcode(self.var-loc),
        [ED.para:
          ED.text("and "),
          ED.highlight(ED.text("declared as an identifier"), [ED.locs: self.id-loc], 1)],
        ED.cmcode(self.id-loc)]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text(self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
              + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")")]]
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    # TODO: disambiguate what is doing the shadowing and what is being shadowed.
    # it's not necessarily a binding; could be a function definition.
    method render-fancy-reason(self):
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
    method render-reason(self):
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
              ED.text(" shadows the declaration of an identifier also named "),
              ED.code(ED.text(self.id)),
              ED.text(" at "),
              ED.loc(self.old-loc)]]
      end
    end
  | duplicate-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    method render-fancy-reason(self):
      old-loc-color = 0
      new-loc-color = 1
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The declaration of the identifier named "),
              ED.highlight(ED.code(ED.text(self.id)), [list: self.new-loc], new-loc-color),
              ED.text(" is preceeded in the same scope by a declaration of an identifier also named "),
              ED.highlight(ED.code(ED.text(self.id)), [list: self.old-loc], old-loc-color),
              ED.text(".")]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("This declaration of a "),
              ED.highlight(ED.text("name"), [list: self.new-loc], 0),
              ED.text(" conflicts with an earlier declaration of the "),
              ED.highlight(ED.text("same name"), [list: self.old-loc], 1),
              ED.text(":")],
            ED.cmcode(self.old-loc),
            ED.cmcode(self.new-loc)]
      end
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      fun adjust(l):
        n = string-length(self.id)
        SL.srcloc(l.source,
          l.start-line, l.start-column, l.start-char,
          l.start-line, l.start-column + n, l.start-char + n)
      end
      old-loc-color = 0
      new-loc-color = 1
      [ED.error:
        [ED.para:
          ED.text("The declaration of the field named "),
          ED.highlight(ED.code(ED.text(self.id)), [list: adjust(self.new-loc)], new-loc-color),
          ED.text(" is preceeded by declaration of an field also named "),
          ED.highlight(ED.code(ED.text(self.id)), [list: adjust(self.old-loc)], old-loc-color),
          ED.text(":")],
        ED.cmcode(self.old-loc + self.new-loc),
        [ED.para: ED.text("Pick a different name for one of them.")]]
    end,
    method render-reason(self):
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
        [ED.para: ED.text("Pick a different name for one of them.")]]
    end
  | same-line(a :: Loc, b :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.highlight(ED.text("This expression"), [list: self.a], 0),
          ED.text(" is on the same line as "),
          ED.highlight(ED.text("another expression"), [list: self.b], 1),
          ED.text(":")],
        ED.cmcode(self.a + self.b),
        [ED.para:
          ED.text("Each expression within a block should be on its own line.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Pyret expects each expression within a block to have its own line, but the expression at "),
          ED.loc(self.a),
          ED.text(" is on the same line as the expression at "),
          ED.loc(self.b),
          ED.text(".")]]
    end
  | template-same-line(a :: Loc, b :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("There are two "),
          ED.highlight(ED.text("unfinished template expressions"), [list: self.a, self.b], 0),
          ED.text(" on the same line.")],
        ED.cmcode(self.a + self.b),
        [ED.para:
          ED.text("Either remove one, or separate them.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("There are two unfinished template expressions on the same line at "),
          ED.loc(self.a + self.b),
          ED.text(". Either remove one, or separate them.")]]
    end
  | type-mismatch(type-1 :: T.Type, type-2 :: T.Type) with:
    method render-fancy-reason(self):
      {type-1; type-2} = if self.type-1.l.before(self.type-2.l): {self.type-1; self.type-2} else: {self.type-2; self.type-1} end
      [ED.error:
        [ED.para:
          ED.text("Type checking failed because of a type inconsistency.")],
        [ED.para:
          ED.text("The type constraint "),
          ED.highlight(ED.text(tostring(type-1)), [list: type-1.l], 0),
          ED.text(" was incompatible with the type constraint "),
          ED.highlight(ED.text(tostring(type-2)), [list: type-2.l], 1)]]
    end,
    method render-reason(self):
      {type-1; type-2} = if self.type-1.l.before(self.type-2.l): {self.type-1; self.type-2} else: {self.type-2; self.type-1} end
      [ED.error:
        [ED.para:
          ED.text("Type checking failed because of a type inconsistency.")],
        [ED.para:
          ED.text("The type constraint "),
          ED.code(ED.text(tostring(type-1))),
          ED.text(" at "), draw-and-highlight(type-1.l),
          ED.text(" was incompatible with the type constraint "),
          ED.code(ED.text(tostring(type-2))),
          ED.text(" at "), draw-and-highlight(type-2.l)]]
    end
  | incorrect-type(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because it found a "),
          ED.highlight(ED.text(self.bad-name), [list: self.bad-loc], 0),
          ED.text(" but it expected a "),
          ED.highlight(ED.text(self.expected-name), [list: self.expected-loc], 1)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Expected to find "), ED.code(ED.text(self.expected-name)),
          ED.text(" at "), draw-and-highlight(self.bad-loc),
          ED.text(", required by "), draw-and-highlight(self.expected-loc),
          ED.text(", but instead found "), ED.code(ED.text(self.bad-name)), ED.text(".")]]
    end
  | incorrect-type-expression(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc, e :: A.Expr) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected the expression")],
        [ED.para:
          ED.cmcode(self.e.l)],
        [ED.para:
          ED.text("because it found a "),
          ED.highlight(ED.text(self.bad-name), [list: self.bad-loc], 0),
          ED.text(" but it expected a "),
          ED.highlight(ED.text(self.expected-name), [list: self.expected-loc], 1)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected the expression")],
        [ED.para:
          ED.code(ED.v-sequence(self.e.tosource().pretty(80).map(ED.text)))],
        [ED.para:
          ED.text("because the expression at "), draw-and-highlight(self.bad-loc),
          ED.text(" was of type "), ED.code(ED.text(self.bad-name)),
          ED.text(" but it was expected to be of type "), ED.code(ED.text(self.expected-name)),
          ED.text(" because of "), draw-and-highlight(self.expected-loc)]]
    end
  | bad-type-instantiation(app-type :: T.Type%(is-t-app), expected-length :: Any) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the type application "),
          ED.highlight(ED.embed(self.app-type), [list: self.app-type.l], 0),
          ED.text(" expected " + tostring(self.expected-length) + " type arguments, "),
          ED.text("but it received " + tostring(self.app-type.args.length()))]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the type application "),
          ED.highlight(ED.embed(self.app-type), [list: self.app-type.l], 0),
          ED.text(" expected " + tostring(self.expected-length) + " type arguments, "),
          ED.text("but it received " + tostring(self.app-type.args.length()))]]
    end
  | incorrect-number-of-args(app-expr, fun-typ) with:
    method render-fancy-reason(self):
      ed-applicant = ED.highlight(ED.text("applicant"), [list: self.app-expr._fun.l], 0)
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("function application"), [ED.locs: self.app-expr.l], -1)],
        ED.cmcode(self.app-expr.l),
        [ED.para:
          ED.text("expects the "), ed-applicant,
          ED.text(" to evaluate to a function that accepts exactly the same number of arguments as are given to it.")],
        [ED.para:
          ED.highlight(ED.ed-args(self.app-expr.args.length()), self.app-expr.args.map(_.l), 1),
          ED.text(" " + if self.app-expr.args.length() == 1: "is " else: "are " end 
                + "given, but the type signature of the "),
          ed-applicant],
        [ED.para:
          ED.embed(self.fun-typ)],
        [ED.para:
          ED.text("indicates that it evaluates to a function accepting exactly "),
          ED.ed-args(self.fun-typ.args.length()),
          ED.text(".")]]
    end,
    method render-reason(self):
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
  | method-missing-self(expr :: A.Expr) with:
    # TODO: is this a duplicate of `no-arguments`???
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("method declaration"), [list: self.expr.l], 0)],
        ED.cmcode(self.expr.l),
        [ED.para:
          ED.text(" does not accept at least one argument. When a method is applied, the first argument is a reference to the object it belongs to.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Method declarations are expected to accept at least one argument, but the method declaration at "),
          ED.loc(self.expr.l),
          ED.text(" has no arguments. When a method is applied, the first argument is a reference to the object it belongs to.")]]
    end
  | apply-non-function(app-expr :: A.Expr, typ) with:
    method render-fancy-reason(self):
      ed-applicant = ED.highlight(ED.text("applicant"), [list: self.app-expr._fun.l], 0)
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("function application"), [ED.locs: self.app-expr.l], -1)],
        ED.cmcode(self.app-expr.l),
        [ED.para:
          ED.text("expects the "), ed-applicant,
          ED.text(" to evaluate to a function value.")],
        [ED.para:
          ED.text("The "),
          ed-applicant,
          ED.text(" is a ")],
        ED.embed(self.typ)]
    end,
    method render-reason(self):
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
  | tuple-too-small(index :: Number, tup-length :: Number, tup :: String, tup-loc :: A.Loc, access-loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the tuple type")],
         ED.highlight(ED.embed(self.tup), [list: self.tup-loc], 0),
        [ED.para:
          ED.text(" has only " + tostring(self.tup-length) + " elements, so the index"),
          ED.code(ED.highlight(ED.text(tostring(self.index)), [list: self.access-loc], 1)),
          ED.text(" is too large")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the tuple type ")],
          ED.embed(self.tup),
          ED.text(" at "),
          ED.loc(self.tup-loc),
          ED.text(" does not have a value at index "),
          ED.code(ED.text(self.index)),
          ED.text(" as indicated by the access of at "),
          ED.loc(self.access-loc)]
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the object type")],
         ED.highlight(ED.text(self.obj), [list: self.obj-loc], 0),
        [ED.para:
          ED.text("does not have a field named "),
          ED.code(ED.highlight(ED.text(self.field-name), [list: self.access-loc], 1))]]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("variant"), [list: self.found], 0),
          ED.text(" is preceeded by "),
          ED.highlight(ED.text("another variant"), [list: self.previous], 1),
          ED.text(" of the same name:")],
        ED.cmcode(self.previous),
        ED.cmcode(self.found),
        [ED.para:
          ED.text("A data declaration may not have two variants with the same names.")]]
    end,
    method render-reason(self):
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
  | data-variant-duplicate-name(id :: String, found :: Loc, data-loc :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("variant"), [list: self.found], 0),
          ED.text(" has the same name as its "),
          ED.highlight(ED.text("containing datatype"), [list: self.data-loc], 1), ED.text(".")],
        ED.cmcode(self.found),
        ED.cmcode(self.data-loc),
        [ED.para:
          ED.text("The "),
          ED.code(ED.text("is-" + self.id)),
          ED.text(" predicates will shadow each other.  Please rename either the variant or the datatype to avoid this.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The variant "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.found),
          ED.text(" has the same name as its containing datatype.  The "),
          ED.code(ED.text("is-" + self.id)),
          ED.text(" predicates will shadow each other.  Please rename either the variant or the datatype to avoid this.")]]
    end,
  | duplicate-is-variant(id :: String, is-found :: Loc, base-found :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("variant"), [list: self.base-found], 0),
          ED.text(" will create a predicate named "), ED.code(ED.text("is-" + self.id)),
          ED.text(", but "),
          ED.highlight(ED.text("another variant"), [list: self.is-found], 1),
          ED.text(" is defined with that name:")],
        ED.cmcode(self.base-found),
        ED.cmcode(self.is-found),
        [ED.para:
          ED.text("Please rename one of the variants so their names do not collide.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The variant "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.base-found),
          ED.text(" will create a predicate named "),
          ED.code(ED.text("is-" + self.id)),
          ED.text(", but another variant is defined with that name.  Please rename one of the variants so their names do not collide.")]]
    end,
  | duplicate-is-data(id :: String, is-found :: Loc, base-found :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("data definition"), [list: self.base-found], 0),
          ED.text(" will create a predicate named "), ED.code(ED.text("is-" + self.id)),
          ED.text(", but "),
          ED.highlight(ED.text("one of its variants"), [list: self.is-found], 1),
          ED.text(" is defined with that name:")],
        ED.cmcode(self.base-found),
        ED.cmcode(self.is-found),
        [ED.para:
          ED.text("Please rename either the variant or the data definition so their names do not collide.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The data definition "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.base-found),
          ED.text(" will create a predicate named "),
          ED.code(ED.text("is-" + self.id)),
          ED.text(", but one of its variants is defined with that name.  Please rename either the variant or the data definition so their names do not collide.")]]
    end,
  | duplicate-is-data-variant(id :: String, is-found :: Loc, base-found :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("variant"), [list: self.base-found], 0),
          ED.text(" will create a predicate named "), ED.code(ED.text("is-" + self.id)),
          ED.text(", but "),
          ED.highlight(ED.text("the data definition"), [list: self.is-found], 1),
          ED.text(" already uses that name:")],
        ED.cmcode(self.base-found),
        ED.cmcode(self.is-found),
        [ED.para:
          ED.text("Please rename either the variant or the data definition so their names do not collide.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The variant "),
          ED.code(ED.text(self.id)),
          ED.text(" at "),
          ED.loc(self.base-found),
          ED.text(" will create a predicate named "),
          ED.code(ED.text("is-" + self.id)),
          ED.text(", but its surrounding data definition already uses that name.  Please rename either the variant or the data definition so their names do not collide.")]]
    end,
  | duplicate-branch(id :: String, found :: Loc, previous :: Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This "),
          ED.highlight(ED.text("branch"), [list: self.found], 0),
          ED.text(" is preceeded by "),
          ED.highlight(ED.text("another branch"), [list: self.previous], 1),
          ED.text(" that matches the same name: ")],
        ED.cmcode(self.previous),
        ED.cmcode(self.found),
        [ED.para:
          ED.text("A variant may not be matched more than once in a cases expression.")]]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the "),
          ED.highlight(ED.text("cases expression"),[list: self.cases-loc], 0),
          ED.text(" expects that all of its branches have a variant of the same name in the data-type "),
          ED.code(ED.text(self.data-type.name)),
          ED.text(". However, no variant named "),
          ED.code(ED.highlight(ED.text(self.branch.name), [list: self.branch.pat-loc], 1)),
          ED.text(" exists in "),
          ED.code(ED.text(self.data-type.name)),
          ED.text("'s "),
          ED.highlight(ED.text("variants"),self.data-type.variants.map(_.l), 2),
          ED.text(":")],
        ED.bulleted-sequence(self.data-type.variants.map(lam(variant):
            ED.code(ED.highlight(ED.text(variant.name), [list: variant.l], 2)) end))]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker rejected your program because the cases expression at "),
          ED.loc(self.cases-loc),
          ED.text(" expects that all of its branches have a variant of the same name in the data-type "),
          ED.code(ED.text(self.data-type.name)),
          ED.text(". However, no variant named "),
          ED.code(ED.text(self.branch.name)),
          ED.text(" (mentioned in the branch at "),
          ED.loc(self.branch.pat-loc),
          ED.text(")"),
          ED.text(" exists in the type "),
          ED.code(ED.text(self.data-type.name)),
          ED.text("'s variants:")],
         ED.bulleted-sequence(self.data-type.variants.map(_.name).map(ED.text))]
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    method render-fancy-reason(self):
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
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("The else branch for the cases expression at "),
          draw-and-highlight(self.loc),
          ED.text(" is not needed since all variants of " + self.type-name + " have been exhausted.")]]
    end
  | non-exhaustive-pattern(missing :: List<T.TypeVariant>, type-name :: String, loc :: A.Loc) with:
    method render-fancy-reason(self):
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
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The cases expression at"),
          draw-and-highlight(self.loc),
          ED.text("does not exhaust all variants of " + self.type-name
            + ". It is missing: " + self.missing.map(_.name).join-str(", ") + ".")]]
    end
  | cant-match-on(ann, type-name :: String, loc :: A.Loc) with:
    method render-fancy-reason(self):
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
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type specified " + self.type-name),
          ED.text("at"),
          draw-and-highlight(self.loc),
          ED.text("cannot be used in a cases expression.")]]
    end
  | different-branch-types(l, branch-types) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The branches of this expression evaluate to different types and no common type encompasses all of them:")],
        ED.bulleted-sequence(map_n(lam(n, branch):
            ED.highlight(ED.embed(branch), [list: branch.l], n) end,
            0, self.branch-types))]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The branches of this expression evaluate to different types and no common type encompasses all of them:")],
        ED.bulleted-sequence(map_n(lam(n, branch):
         [ED.sequence:
              ED.loc(branch.l), ED.text(" has type "), ED.embed(branch)] end,
            0, self.branch-types))]
    end
  | incorrect-number-of-bindings(branch :: A.CasesBranch, variant :: T.TypeVariant) with:
    method render-fancy-reason(self):
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
          ED.highlight(ed-fields(self.variant.fields.count()), [list: A.dummy-loc], 3)]]
    end,
    method render-reason(self):
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
    method render-fancy-reason(self):
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
    method render-reason(self):
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
    method render-fancy-reason(self):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The data type"),  ED.code(ED.text(self.data-type)),
          ED.text("does not take any parameters, but is given some at"),
          draw-and-highlight(self.loc)]]
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    method render-fancy-reason(self):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("In the type at"), draw-and-highlight(self.loc),
          ED.text("there was not enough information to instantiate the type, "
            + "or the given arguments are incompatible.")]]
    end
  | unable-to-infer(loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Unable to infer the type of "), 
          ED.highlight(ED.text("the expression"), [list: self.loc], 0),
          ED.text(" at "),
          ED.cmcode(self.loc),
          ED.text("Please add an annotation.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Unable to infer the type of "), draw-and-highlight(self.loc),
          ED.text(". Please add an annotation.")]]
    end
  | unann-failed-test-inference(function-loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker could not infer the type of the "),
          ED.highlight(ED.text("function"), [list: self.function-loc], 0),
          ED.text(". Please add type annotations to the arguments.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type checker could not infer the type of the function at"),
          draw-and-highlight(self.function-loc),
          ED.text(". Please add type annotations to the arguments.")]]
    end
  | toplevel-unann(arg :: A.Bind) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("argument"), [list: self.arg.l], 0),
          ED.text(" at "),
          ED.cmcode(self.arg.l),
          ED.text(" needs a type annotation. Alternatively, provide a where: block with examples of the function's use.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.text("argument at"), draw-and-highlight(self.arg.l),
          ED.text(" needs a type annotation. Alternatively, provide a where: block with examples of the function's use.")]]
    end
  | polymorphic-return-type-unann(function-loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The "),
          ED.highlight(ED.text("function"), [list: self.function-loc], 0),
          ED.text(" is polymorphic. Please annotate its return type.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The function at "),
          draw-and-highlight(self.function-loc),
          ED.text(" is polymorphic. Please annotate its return type.")]]
    end
  | binop-type-error(binop :: A.Expr, tl :: T.Type, tr :: T.Type, etl :: T.Type, etr :: T.Type) with:
    method render-fancy-reason(self):
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
    method render-reason(self):
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
    method render-fancy-reason(self):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This program cannot be type-checked. " + "The reason that it cannot be type-checked is: " + self.reason +
        " at "), ED.cmcode(self.loc)]]
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    #### TODO ###
    method render-fancy-reason(self):
      self.render-reason()
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text(self.message + " (found at "),
          draw-and-highlight(self.blame-loc),
          ED.text(")")]]
    end
  | non-object-provide(loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Couldn't read the program because the provide statement must contain an object literal"),
          ED.cmcode(self.loc)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("Couldn't read the program because the provide statement must contain an object literal at "),
          draw-and-highlight(self.loc)]]
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("There is no module imported with the name "),
          ED.highlight(ED.text(self.mod-name), [list: self.loc], 0)]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para-nospace:
          ED.text("There is no module imported with the name " + self.mod-name),
          ED.text(" (used at "),
          draw-and-highlight(self.loc),
          ED.text(")")]]
    end
  | table-empty-header(loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.highlight(ED.text("This table"), [list: self.loc], 0),
          ED.text(" has no column names, but tables must have at least one column.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The table at "),
          ED.loc(self.loc),
          ED.text(" has no column names, but tables must have at least one column.")]]
    end
  | table-empty-row(loc :: A.Loc) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.highlight(ED.text("This table row"), [list: self.loc], 0),
          ED.text(" is empty, but table rows cannot be empty.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The table row at "),
          ED.loc(self.loc),
          ED.text(" is empty, but table rows cannot be empty.")]]
    end
  | table-row-wrong-size(header-loc :: A.Loc, header :: List<A.FieldName>, row :: A.TableRow) with:
    method render-fancy-reason(self):
      fun ed-cols(n, ls, c):
        ED.highlight([ED.sequence:
            ED.embed(n),
            if n <> 1:
              ED.text("columns")
            else:
              ED.text("column")
            end], ls, c)
      end
      [ED.error:
        [ED.para:
          ED.text("The table row")],
        ED.cmcode(self.row.l),
        [ED.para:
          ED.text("has "),
          ed-cols(self.row.elems.length(), self.row.elems.map(_.l), 0),
          ED.text(", but the table header")],
        ED.cmcode(self.header-loc),
        [ED.para:
          ED.text(" declares "),
          ed-cols(self.header.length(), self.header.map(_.l), 1),
          ED.text(".")]]
    end,
    method render-reason(self):
      fun ed-cols(n):
        [ED.sequence:
          ED.embed(n),
          if n <> 1:
            ED.text("columns")
          else:
            ED.text("column")
          end]
      end
      [ED.error:
        [ED.para:
          ED.text("The table row at "),
          ED.loc(self.row.l),
          ED.text(" has "),
          ed-cols(self.row.elems.length()),
          ED.text(", but the table header "),
          ED.loc(self.header-loc),
          ED.text(" declares "),
          ed-cols(self.header.length()),
          ED.text(".")]]
    end
  | table-duplicate-column-name(column1 :: A.FieldName, column2 :: A.FieldName) with:
    method render-fancy-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Column "),
          ED.highlight(ED.text(self.column1.name), [list: self.column1.l], 0),
          ED.text(" and column "),
          ED.highlight(ED.text(self.column2.name), [list: self.column2.l], 0),
          ED.text(" have the same name, but table columns must have different names.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The table columns at "),
          ED.loc(self.column1.l),
          ED.text(" and at "),
          ED.loc(self.column2.l),
          ED.text(" have the same name, but columns in a table must have different names.")]]
    end
  | table-reducer-bad-column(extension :: A.TableExtendField, col-defs :: A.Loc) with:
    method render-fancy-reason(self):
      bad-column = self.extension.col
      bad-column-name = bad-column.tosource().pretty(80).join-str("\n")
      reducer = self.extension.reducer
      reducer-name = reducer.tosource().pretty(80).join-str("\n")
      [ED.error:
        [ED.para:
          ED.text("The column "),
          ED.highlight(ED.text(bad-column-name), [list: bad-column.l], 0),
          ED.text(" is used with the reducer "),
          ED.highlight(ED.text(reducer-name), [list: reducer.l], 1),
          ED.text(", but it is not one of the "),
          ED.highlight(ED.text("used columns"), [list: self.col-defs], 2),
          ED.text(".")]]
    end,
    method render-reason(self):
      bad-column = self.extension.col
      reducer = self.extension.reducer
      [ED.error:
        [ED.para:
          ED.text("The column at "),
          ED.loc(bad-column.l),
          ED.text(" is used with the reducer at "),
          ED.loc(reducer.l),
          ED.text(", but it is not one of the used columns listed at "),
          ED.loc(self.col-defs),
          ED.text(".")]]
    end
  | table-sanitizer-bad-column(sanitize-expr :: A.LoadTableSpec, col-defs :: A.Loc) with:
    method render-fancy-reason(self):
      
      bad-column = self.sanitize-expr.name
      bad-column-name = bad-column.tosource().pretty(80)
      sanitizer = self.sanitize-expr.sanitizer
      sanitizer-name = sanitizer.tosource().pretty(80)
      [ED.error:
        [ED.para:
          ED.text("The column "),
          ED.highlight(ED.text(bad-column-name), [list: bad-column.l], 0),
          ED.text(" is used with the sanitizer "),
          ED.highlight(ED.text(sanitizer-name), [list: sanitizer.l], 1),
          ED.text(", but it is not one of the "),
          ED.highlight(ED.text("used columns"), [list: self.col-defs], 2),
          ED.text(".")]]
    end,
    method render-reason(self):
      bad-column = self.sanitize-expr.name
      sanitizer = self.sanitize-expr.sanitizer
      [ED.error:
        [ED.para:
          ED.text("The column at "),
          ED.loc(bad-column.l),
          ED.text(" is used with the sanitizer at "),
          ED.loc(sanitizer.l),
          ED.text(", but it is not one of the used columns listed at "),
          ED.loc(self.col-defs),
          ED.text(".")]]
    end
  | load-table-bad-number-srcs(lte :: A.Expr#|%(A.is-s-load-table)|#, num-found :: Number) with:
    method render-fancy-reason(self):
      load-table-expr = self.lte.tosource().pretty(80)
      [ED.error:
        [ED.para:
          ED.text("The table loader "),
          ED.highlight(ED.text(load-table-expr), [list: self.lte.l], 0),
          ED.text(" specifies "
              + num-to-string(self.num-found)
              + " sources, but it should only specify one.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The table loader at "),
          ED.loc(self.lte.l),
          ED.text(" specifies "
              + num-to-string(self.num-found)
              + " sources, but it should only specify one.")]]
    end
  | load-table-duplicate-sanitizer(original :: A.LoadTableSpec, col-name :: String, duplicate-exp :: A.LoadTableSpec) with:
    method render-fancy-reason(self):
      orig-pretty = self.original.tosource().pretty(80)
      dup-pretty = self.duplicate-exp.tosource().pretty(80)
      [ED.error:
        [ED.para:
          ED.text("The column "),
          ED.highlight(ED.text(self.col-name), [list: self.duplicate-exp.l], 0),
          ED.text(" is already sanitized by the sanitizer "),
          ED.highlight(ED.text(orig-pretty), [list: self.original.l], 1),
          ED.text(".")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The column at "),
          ED.loc(self.duplicate-exp.l),
          ED.text(" is already sanitized by the sanitizer at "),
          ED.loc(self.original.l),
          ED.text(".")]]
    end
  | load-table-no-body(load-table-exp :: A.Expr#|%(A.is-s-load-table)|#) with:
    method render-fancy-reason(self):
      pretty = self.load-table-exp.tosource().pretty(80)
      [ED.error:
        [ED.para:
          ED.text("The table loader "),
          ED.highlight(ED.text(pretty), [list: self.load-table-exp.l], 0),
          ED.text(" has no information about how to load the table. "
              + "It should at least contain a source.")]]
    end,
    method render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The table loader at "),
          ED.loc(self.load-table-exp.l),
          ED.text(" has no information about how to load the table. "
              + "It should at least contain a source.")]]
    end
end

type CompileOptions = {
  check-mode :: Boolean,
  check-all :: Boolean,
  type-check :: Boolean,
  allow-shadowed :: Boolean,
  collect-all :: Boolean,
  collect-times :: Boolean,
  ignore-unbound :: Boolean,
  proper-tail-calls :: Boolean,
  compiled-cache :: String,
  display-progress :: Boolean,
  standalone-file :: String,
  log :: (String -> Nothing),
  on-compile :: Function, # NOTE: skipping types because the are in compile-lib
  before-compile :: Function
}

default-compile-options = {
  this-pyret-dir: ".",
  check-mode : true,
  check-all : true,
  type-check : false,
  allow-shadowed : false,
  collect-all: false,
  collect-times: false,
  ignore-unbound: false,
  proper-tail-calls: true,
  inline-case-body-limit: 5,
  module-eval: true,
  compiled-cache: "compiled",
  display-progress: true,
  log: lam(s, to-clear):
    cases(Option) to-clear block:
      | none => print(s)
      | some(n) =>
        print("\r")
        print(string-repeat(" ", n))
        print("\r")
        print(s)
    end
  end,
  log-error: lam(s):
    print-error(s)
  end,
  method on-compile(_, locator, loadable, _): loadable end,
  method before-compile(_, _): nothing end,
  html-file: none,
  deps-file: "build/bundled-node-deps.js",
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
    "to-string", t-arrow([list: t-top], t-str),
    "torepr", t-arrow([list: t-top], t-str),
    "to-repr", t-arrow([list: t-top], t-str),
    "brander", t-top,
    "raise", t-arrow([list: t-top], t-bot),
    "nothing", t-nothing,
    "builtins", t-record([string-dict:
        "has-field", t-arrow([list: t-record([string-dict: ])], t-boolean),
        "trace-value", t-arrow([list: t-top, t-top], t-bot),
        "current-checker", t-arrow([list: ], t-record([string-dict: # Cheat on these types for now.
            "run-checks", t-bot,
            "check-is", t-bot,
            "check-is-not", t-bot,
            "check-is-not-refinement", t-bot,
            "check-is-refinement", t-bot,
            "check-satisfies", t-bot,
            "check-satisfies-not", t-bot,
            "check-raises-str", t-bot,
            "check-raises-not", t-bot,
            "check-raises-other-str", t-bot,
            "check-raises-satisfies", t-bot,
            "check-raises-violates" , t-bot
        ]))
    ]),
    "not", t-arrow([list: t-boolean], t-boolean),
    "is-nothing", t-pred,
    "is-number", t-pred,
    "is-string", t-pred,
    "is-boolean", t-pred,
    "is-object", t-pred,
    "is-function", t-pred,
    "is-raw-array", t-pred,
    "is-tuple", t-pred,
    "is-table", t-pred,
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
    "string-isnumber", t-top,
    "string-is-number", t-top,
    "string-tonumber", t-top,
    "string-to-number", t-arrow([list: t-string], t-option(t-number)),
    "string-repeat", t-top,
    "string-substring", t-top,
    "string-replace", t-top,
    "string-split", t-top,
    "string-split-all", t-top,
    "string-char-at", t-top,
    "string-toupper", t-top,
    "string-to-upper", t-top,
    "string-tolower", t-top,
    "string-to-lower", t-top,
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
    "num-atan2", t-number-binop,
    "num-modulo", t-number-binop,
    "num-remainder", t-number-binop,
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
    "raw-array-build", t-top,
    "raw-array-build-opt", t-top,
    "raw-array-length", t-top,
    "raw-array-to-list", t-top,
    "raw-array-fold", t-top,
    "raw-array-filter", t-top,
    "raw-array-and-mapi", t-top,
    "raw-array-or-mapi", t-top,
    "raw-array-map", t-top,
    "raw-array-map-1", t-top,
    "raw-array-join-str", t-top,
    "raw-array-from-list", t-top,
    "raw-array", t-record(
      [string-dict:
        "make", t-forall1(lam(a): t-arrow([list: t-array(a)], t-array(a)) end),
        "make0", t-forall1(lam(a): t-arrow([list: ], t-array(a)) end),
        "make1", t-forall1(lam(a): t-arrow([list: a], t-array(a)) end),
        "make2", t-forall1(lam(a): t-arrow([list: a, a], t-array(a)) end),
        "make3", t-forall1(lam(a): t-arrow([list: a, a, a], t-array(a)) end),
        "make4", t-forall1(lam(a): t-arrow([list: a, a, a, a], t-array(a)) end),
        "make5", t-forall1(lam(a): t-arrow([list: a, a, a, a, a], t-array(a)) end)
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
     "Table", t-top,
     "Function", t-top,
     "Boolean", t-top,
     "Object", t-top,
     "Method", t-top,
     "Nothing", t-top,
     "RawArray", t-top  ],
  [string-dict:])

runtime-builtins = for SD.fold-keys(rb from [string-dict:], k from runtime-provides.values):
  rb.set(k, "builtin(global)")
end

runtime-types = for SD.fold-keys(rt from [string-dict:], k from runtime-provides.aliases):
  rt.set(k, "builtin(global)")
end
shadow runtime-types = for SD.fold-keys(rt from runtime-types, k from runtime-provides.data-definitions):
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

reactor-optional-fields = [SD.string-dict:
  "last-image",       {(l): A.a-name(l, A.s-type-global("Function"))},
  "on-tick",          {(l): A.a-name(l, A.s-type-global("Function"))},
  "to-draw",          {(l): A.a-name(l, A.s-type-global("Function"))},
  "on-key",           {(l): A.a-name(l, A.s-type-global("Function"))},
  "on-mouse",         {(l): A.a-name(l, A.s-type-global("Function"))},
  "stop-when",        {(l): A.a-name(l, A.s-type-global("Function"))},
  "seconds-per-tick", {(l): A.a-name(l, A.s-type-global("NumPositive"))},
  "title",            {(l): A.a-name(l, A.s-type-global("String"))},
  "close-when-stop",  {(l): A.a-name(l, A.s-type-global("Boolean"))}
]

reactor-fields = reactor-optional-fields.set("init", {(l): A.a-any(l)})
