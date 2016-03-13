#lang pyret

provide *
provide-types *
import ast as A
import srcloc as SL
import error-display as ED
import string-dict as SD
import "compiler/type-structs.arr" as T

t-nothing = T.t-nothing(A.dummy-loc)
t-str = T.t-string(A.dummy-loc)
t-boolean = T.t-boolean(A.dummy-loc)
t-number = T.t-number(A.dummy-loc)
t-arrow = T.t-arrow(_, _, A.dummy-loc)
t-top = T.t-top(A.dummy-loc)
t-member = T.t-member(_, _, A.dummy-loc)
t-bot = T.t-bot(A.dummy-loc)
t-record = T.t-record(_, A.dummy-loc)
t-forall = T.t-forall(_, _, A.dummy-loc)
t-var = T.t-var(_, A.dummy-loc)
t-array = T.t-array(_, A.dummy-loc)
t-string = T.t-string(A.dummy-loc)
t-option = T.t-option(_, A.dummy-loc)
t-data = T.t-data(_, _, _, A.dummy-loc)
t-variant = T.t-variant(_, _, _, A.dummy-loc)
t-singleton-variant = T.t-variant(_, _, A.dummy-loc)
t-app = T.t-app(_, _, A.dummy-loc)
t-name = T.t-name(_, _, A.dummy-loc)

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
      data-definitions :: StringDict<T.Type>
    )
end

fun type-from-raw(uri, typ, tyvar-env :: SD.StringDict<T.TypeVariable>):
  tfr = type-from-raw(uri, _, tyvar-env)
  t = typ.tag
  ask:
    | t == "any" then: t-top
    | t == "record" then:
      t-record(for map(f from typ.fields): t-member(f.name, tfr(f.value)) end)
    | t == "name" then:
      modname = if typ.module == "LOCAL": uri else: typ.module end
      t-name(some(modname), A.s-type-global(typ.name))
    | t == "tyvar" then:
      cases(Option<T.TypeVariable>) tyvar-env.get(typ.name):
        | none => raise("Unbound type variable " + typ.name + " in provided type.")
        | some(tv) => t-var(tv)
      end
    | t == "forall" then:
      new-env = for fold(new-env from tyvar-env, a from typ.args):
        tvn = A.global-names.make-atom(a)
        new-env.set(a, tvn)
      end
      params = for map(k from new-env.keys-list()):
        t-var(new-env.get-value(k))
      end
      t-forall(params, type-from-raw(uri, typ.onto, new-env))
    | t == "tyapp" then:
      t-app(tfr(typ.onto), map(tfr, typ.args))
    | t == "arrow" then:
      t-arrow(map(tfr, typ.args), tfr(typ.ret))
    | otherwise: raise("Unkonwn raw tag for type: " + t)
  end
end

fun tvariant-from-raw(uri, tvariant, env):
  t = tvariant.tag
  ask:
    | t == "variant" then:
      members = for map(tm from tvariant.vmembers):
        # TODO(joe): Exporting ref fields?
        t-member(tm.name, type-from-raw(uri, tm.typ, env))
      end
      t-variant(tvariant.name, members, empty)
    | t == "singleton-variant" then:
      t-singleton-variant(tvariant.name, empty)
    | otherwise: raise("Unkonwn raw tag for variant: " + t)
  end
end

fun datatype-from-raw(uri, datatyp):
  pdict = for fold(pdict from SD.make-string-dict(), a from datatyp.params):
    tvn = A.global-names.make-atom(a)
    pdict.set(a, tvn)
  end
  params = for map(k from pdict.keys-list()):
    t-var(pdict.get-value(k))
  end
  variants = map(tvariant-from-raw(uri, _, pdict), datatyp.variants)
  members = for map(tm from datatyp.methods):
    # TODO(joe): Exporting ref fields?
    t-member(tm.name, type-from-raw(uri, tm.value, pdict))
  end
  t-data(params, variants, members)
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


data CompileResult<C>:
  | ok(code :: C)
  | err(problems :: List<CompileError>)
end

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end

data CompileError:
  | wf-err(msg :: String, loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        [ED.para: draw-and-highlight(self.loc)]]
    end
  | wf-err-split(msg :: String, loc :: List<A.Loc>) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        ED.v-sequence(self.loc.map(lam(l): [ED.para: draw-and-highlight(l)] end))]
    end
  | reserved-name(loc :: Loc, id :: String) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness: Pyret disallows the use of"),
          ED.code(ED.text(self.id)),
          ED.text("as an identifier")],
        [ED.para: draw-and-highlight(self.loc)]]
    end
  | zero-fraction(loc, numerator) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness: fraction literal with zero denominator (numerator was"),
          ED.val(self.numerator),
          ED.text(") at")],
        [ED.para: draw-and-highlight(self.loc)]]
    end
  | underscore-as-expr(l :: Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Underscore used as an expression, which is not allowed, at ")],
        [ED.para: draw-and-highlight(self.l)]]
    end
  | underscore-as-ann(l :: Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Underscore used as an annotation, which is not allowed at ")],
        [ED.para: draw-and-highlight(self.l)]]
    end
  | unbound-id(id :: A.Expr) with:
    render-reason(self):
      cases(SL.Srcloc) self.id.l:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id.id.toname()), ED.text("at"),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id.id.toname())), ED.text("is used but not defined at")],
            [ED.para: draw-and-highlight(self.id.l)]]
      end
    end
  | unbound-var(id :: String, loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id), ED.text("at"),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The variable"), ED.code(ED.text(self.id)), ED.text("is assigned to, but not defined, at")],
            [ED.para: draw-and-highlight(self.loc)]]
      end
    end
  | unbound-type-id(ann :: A.Ann) with:
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
              ED.text("The name"), ED.code(ED.text(self.ann.id.toname())),
              ED.text("is used as a type but not defined as one, at")],
            [ED.para: draw-and-highlight(self.ann.l)]]
      end
    end
  | unexpected-type-var(loc :: Loc, name :: A.Name) with:
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
              ED.text("Defining an anonymous variable is pointless: there is no name to modify."),
              ED.text("Either give this expression a name, or bind it to an identifier rather than a variable.")],
            [ED.para: draw-and-highlight(self.loc)]]
      end
    end
  | pointless-rec(loc :: Loc) with:
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
              ED.text("Defining an anonymous recursive identifier is pointless: there is no name to call recursively."),
              ED.text("Either give this expression a name, or remove the rec annotation.")],
            [ED.para: draw-and-highlight(self.loc)]]
      end
    end
  | pointless-shadow(loc :: Loc) with:
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
              ED.text("Anonymous identifier cannot shadow anything: there is no name to shadow."),
              ED.text("Either give this expression a name, or remove the shadow annotation.")],
            [ED.para: draw-and-highlight(self.loc)]]
      end
    end
  | bad-assignment(id :: String, loc :: Loc, prev-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.prev-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is defined as an identifier,"),
              ED.text("but it is assigned as if it were a variable at"),
              draw-and-highlight(self.loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is defined as an identifier,"),
              ED.text("but it is assigned as if it were a variable at"),
              draw-and-highlight(self.loc)],
            [ED.para:
              ED.text("One possible fix is to change the declaration of"), ED.code(ED.text(self.id)),
              ED.text("to use"), ED.code(ED.text("var")), ED.text("at"), draw-and-highlight(self.prev-loc)]]
      end
    end
  | mixed-id-var(id :: String, var-loc :: Loc, id-loc :: Loc) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text(self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
              + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")")]]
    end
  | shadow-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is already defined."),
              ED.text("You need to pick a different name for"), ED.code(ED.text(self.id)), ED.text("at"),
              draw-and-highlight(self.new-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("It looks like you've defined the name"), ED.code(ED.text(self.id)),
              ED.text("twice, at")],
            [ED.para: draw-and-highlight(self.old-loc), ED.text("and")],
            [ED.para-nospace: draw-and-highlight(self.new-loc), ED.text(".")],
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | duplicate-id(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The name"), ED.code(ED.text(self.id)), ED.text("is already defined."),
              ED.text("You need to pick a different name for"), ED.code(ED.text(self.id)), ED.text("at"),
              draw-and-highlight(self.new-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("It looks like you've defined the name"), ED.code(ED.text(self.id)),
              ED.text("twice, at")],
            [ED.para: draw-and-highlight(self.old-loc), ED.text("and")],
            [ED.para-nospace: draw-and-highlight(self.new-loc), ED.text(".")],
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | duplicate-field(id :: String, new-loc :: Loc, old-loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.old-loc:
        | builtin(_) =>
          [ED.error:
            [ED.para:
              ED.text("The field name"), ED.code(ED.text(self.id)), ED.text("is already defined."),
              ED.text("You need to pick a different name for"), ED.code(ED.text(self.id)), ED.text("at"),
              draw-and-highlight(self.new-loc)]]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("It looks like you've defined the field name"), ED.code(ED.text(self.id)),
              ED.text("twice, at")],
            [ED.para: draw-and-highlight(self.old-loc), ED.text("and")],
            [ED.para-nospace: draw-and-highlight(self.new-loc), ED.text(".")],
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | incorrect-type(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected to find "), ED.code(ED.text(self.expected-name)),
          ED.text(" at "), draw-and-highlight(self.bad-loc),
          ED.text(", required by "), draw-and-highlight(self.expected-loc),
          ED.text(", but instead found "), ED.code(ED.text(self.bad-name)), ED.text(".")]]
    end
  | bad-type-instantiation(wanted :: Number, given :: Number, loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected to receive "), ED.text(tostring(self.wanted)),
          ED.text(" arguments for type instantiation at "), draw-and-highlight(self.loc),
          ED.text(", but instead received "), ED.text(tostring(self.given))]]
    end
  | incorrect-number-of-args(loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Incorrect number of arguments given to function at "),
          draw-and-highlight(self.loc)]]
    end
  | apply-non-function(loc :: A.Loc, typ) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Tried to apply the non-function type "),
          ED.embed(self.typ),
          ED.text(" at "),
          draw-and-highlight(self.loc)]]
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The object type " + self.obj + " (at "),
          draw-and-highlight(self.obj-loc),
          ED.text(") does not have the field \"" + self.field-name + "\", accessed at "),
          draw-and-highlight(self.access-loc)]]
    end
  | unneccesary-branch(branch-name :: String, branch-loc :: A.Loc, type-name :: String, type-loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The branch "), ED.code(ED.text(self.branch-name)),
          ED.text(" at "), draw-and-highlight(self.branch-loc),
          ED.text(" is not a variant of "), ED.code(ED.text(self.type-name)),
          ED.text(" at "),
          draw-and-highlight(self.type-loc)]]
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The else branch for the cases expression at "),
          draw-and-highlight(self.loc),
          ED.text(" is not needed since all variants of " + self.type-name + " have been exhausted.")]]
    end
  | non-exhaustive-pattern(missing :: List<String>, type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The cases expression at "),
          draw-and-highlight(self.loc),
          ED.text(" does not exhaust all variants of " + self.type-name
            + ". It is missing: " + self.missing.join-str(", ") + ".")]]
    end
  | cant-match-on(type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The type specified " + self.type-name),
          ED.text(" at "),
          draw-and-highlight(self.loc),
          ED.text(" cannot be used in a cases expression.")]]
    end
  | incorrect-number-of-bindings(variant-name :: String, loc :: A.Loc, given :: Number, expected :: Number) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Incorrect number of bindings given to "),
          ED.text("the variant " + self.variant-name),
          ED.text(" at "),
          draw-and-highlight(self.loc),
          ED.text(". "
            + "Given " + num-tostring(self.given)
            + ", but expected " + num-tostring(self.expected)
            + ".")]]
    end
  | cases-singleton-mismatch(name :: String, branch-loc :: A.Loc, should-be-singleton :: Boolean) with:
    render-reason(self):
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The cases branch named"), ED.code(ED.text(self.name)),
            ED.text("at"), draw-and-highlight(self.branch-loc),
            ED.text("has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch named"), ED.code(ED.text(self.name)),
            ED.text("at"), draw-and-highlight(self.branch-loc),
            ED.text("doesn't have an argument list, but the variant is not a singleton.")]]
      end
    end
  | given-parameters(data-type :: String, loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("The data type "),  ED.code(ED.text(self.data-type)),
          ED.text(" does not take any parameters, but is given some at "),
          draw-and-highlight(self.loc)]]
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("In the type at "), draw-and-highlight(self.loc),
          ED.text(" there was not enough information to instantiate the type, "
            + "or the given arguments are incompatible.")]]
    end
  | unable-to-infer(loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Unable to infer the type of "), draw-and-highlight(self.loc),
          ED.text(". Please add an annotation.")]]
    end
  | cant-typecheck(reason :: String, loc :: A.Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("This program cannot be type-checked. Please send it to the developers. " + "The reason that it cannot be type-checked is: " + self.reason +
        " at "), draw-and-highlight(self.loc)]]
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text(self.message + " (found at "),
          draw-and-highlight(self.blame-loc),
          ED.text(")")]]
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("There is no module imported with the name " + self.mod-name),
          ED.text(" (used at "),
          draw-and-highlight(self.loc),
          ED.text(")")]]
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

t-pred = t-arrow([list: t-top], t-boolean)
t-pred2 = t-arrow([list: t-top, t-top], t-boolean)

t-number-binop = t-arrow([list: t-number, t-number], t-number)
t-number-unop = t-arrow([list: t-number], t-number)
t-number-pred1 = t-arrow([list: t-number], t-boolean)
t-within-num = t-arrow([list: t-number], t-arrow([list: t-number, t-number], t-boolean))
t-within-any = t-arrow([list: t-number], t-arrow([list: t-top, t-top], t-boolean))

runtime-types = [string-dict:
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
  "RawArray", t-top
]

fun t-forall1(f):
  n = A.global-names.make-atom("a")
  t-forall([list: t-var(n)], f(t-var(n)))
end

runtime-builtins = [string-dict:
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
  "identical3", t-top,
  "exn-unwrap", t-top# ,
  # "_empty", t-top,
  # "_link", t-top
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
