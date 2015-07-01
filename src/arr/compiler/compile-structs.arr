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
      T.t-name(some(modname), A.s-global(typ.name))
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
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness:"),
          ED.text(self.msg),
          ED.text("at")],
        draw-and-highlight(self.loc)]
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
        draw-and-highlight(self.loc)]
    end
  | zero-fraction(loc, numerator) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Well-formedness: fraction literal with zero denominator (numerator was"),
          ED.val(self.numerator),
          ED.text(") at")],
        draw-and-highlight(self.loc)]
    end
  | underscore-as-expr(l :: Loc) with:
    render-reason(self):
      [ED.error: 
        [ED.para: ED.text("Underscore used as an expression, which is not allowed, at ")],
        draw-and-highlight(self.l)]
    end
  | underscore-as-ann(l :: Loc) with:
    render-reason(self):
      [ED.error:
        [ED.para: ED.text("Underscore used as an annotation, which is not allowed at ")],
        draw-and-highlight(self.l)]
    end
  | unbound-id(id :: A.Expr) with:
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
              ED.text("The name"), ED.code(ED.text(self.id.id.toname())), ED.text("is used but not defined at")],
            draw-and-highlight(self.id.l)]
      end
    end
  | unbound-var(id :: String, loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's unbound:"),
            ED.text(self.id),
            draw-and-highlight(self.id.l)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("The variable"), ED.code(ED.text(self.id)), ED.text("is assigned to, but not defined, at")],
            draw-and-highlight(self.loc)]
      end
    end
  | unbound-type-id(ann :: A.Ann) with:
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
              ED.text("The name"), ED.code(ED.text(self.ann.id.toname())),
              ED.text("is used as a type but not defined as one, at")],
            draw-and-highlight(self.ann.l)]
      end
    end
  | unexpected-type-var(loc :: Loc, name :: A.Name) with:
    render-reason(self):
      #### TODO ###
      ED.text("Identifier " + tostring(self.name) + " is used in a dot-annotation at " + tostring(self.loc) + ", but is bound as a type variable")
    end
  | pointless-var(loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining an anonymous variable is pointless: there is no name to modify."),
              ED.text("Either give this expression a name, or bind it to an identifier rather than a variable.")],
            draw-and-highlight(self.loc)]
      end
    end
  | pointless-rec(loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Defining an anonymous recursive identifier is pointless: there is no name to call recursively."),
              ED.text("Either give this expression a name, or remove the rec annotation.")],
            draw-and-highlight(self.loc)]
      end
    end
  | pointless-shadow(loc :: Loc) with:
    render-reason(self):
      cases(SL.Srcloc) self.loc:
        | builtin(_) =>
          [ED.para:
            ED.text("ERROR: should not be allowed to have a builtin that's anonymous:"),
            draw-and-highlight(self.loc)]
        | srcloc(_, _, _, _, _, _, _) =>
          [ED.error:
            [ED.para:
              ED.text("Anonymous identifier cannot shadow anything: there is no name to shadow."),
              ED.text("Either give this expression a name, or remove the shadow annotation.")],
            draw-and-highlight(self.loc)]
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
      ED.text(self.id + " is declared as both a variable (at " + tostring(self.var-loc) + ")"
          + " and an identifier (at " + self.id-loc.format(not(self.var-loc.same-file(self.id-loc))) + ")")
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
            draw-and-highlight(self.old-loc),
            draw-and-highlight(self.new-loc),
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
            draw-and-highlight(self.old-loc),
            draw-and-highlight(self.new-loc),
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
            draw-and-highlight(self.old-loc),
            draw-and-highlight(self.new-loc),
            [ED.para: ED.text("You need to pick a different name for one of them.")]]
      end
    end
  | incorrect-type(bad-name :: String, bad-loc :: A.Loc, expected-name :: String, expected-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected to find "), ED.code(ED.text(self.expected-name)),
          ED.text(" (declared at "), draw-and-highlight(self.expected-loc),
          ED.text(") at "), draw-and-highlight(self.bad-loc),
          ED.text(" but instead found "), ED.code(ED.text(self.bad-name))]]
    end
  | bad-type-instantiation(wanted :: Number, given :: Number, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("Expected to receive " + tostring(self.wanted) + " arguments for type instantiation "
        + " on line " + tostring(self.loc) + ", but instead received " + tostring(self.given) + ".")
    end
  | incorrect-number-of-args(loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("Incorrect number of arguments given to function at line " + tostring(self.loc) + ".")
    end
  | apply-non-function(loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The program tried to apply something that is not a function at line " + tostring(self.loc) + ".")
    end
  | object-missing-field(field-name :: String, obj :: String, obj-loc :: A.Loc, access-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The object type " + self.obj
        + " (defined at " + tostring(self.obj-loc)
        + ") does not have the field \"" + self.field-name
        + "\" (accessed at line " + tostring(self.access-loc) + ").")
    end
  | unneccesary-branch(branch-name :: String, branch-loc :: A.Loc, type-name :: String, type-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The branch " + self.branch-name
        + " (defined at " + tostring(self.branch-loc)
        + ") is not a variant of " + self.type-name
        + " (declared at " + tostring(self.type-loc) + ")")
    end
  | unneccesary-else-branch(type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The else branch for the cases expression at " + tostring(self.loc)
        + " is not needed since all variants of " + self.type-name + " have been exhausted.")
    end
  | non-exhaustive-pattern(missing :: List<String>, type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The cases expression at " + tostring(self.loc)
        + " does not exhaust all variants of " + self.type-name
        + ". It is missing: " + self.missing.join-str(", ") + ".")
    end
  | cant-match-on(type-name :: String, loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("The type specified " + self.type-name
        + " at " + tostring(self.loc)
        + " cannot be used in a cases expression.")
    end
  | incorrect-number-of-bindings(variant-name :: String, loc :: A.Loc, given :: Number, expected :: Number) with:
    #### TODO ###
    render-reason(self):
      ED.text("Incorrect number of bindings given to "
        + "the variant " + self.variant-name
        + " at " + tostring(self.loc) + ". "
        + "Given " + num-tostring(self.given)
        + ", but expected " + num-tostring(self.expected)
        + ".")
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
    #### TODO ###
    render-reason(self):
      ED.text("The data type " + self.data-type
        + " does not take any parameters,"
        + " but is given some at " + tostring(self.loc)
        + ".")
    end
  | unable-to-instantiate(loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text("There is not enough information to instantiate the type at " + tostring(self.loc)
         + ", or the arguments are incompatible. Please provide more information or do the type instantiation directly.")
    end
  | cant-typecheck(reason :: String) with:
    #### TODO ###
    render-reason(self):
      ED.text("This program cannot be type-checked. Please send it to the developers. " +
        "The reason that it cannot be type-checked is: " + self.reason)
    end
  | unsupported(message :: String, blame-loc :: A.Loc) with:
    #### TODO ###
    render-reason(self):
      ED.text(self.message + " (found at " + tostring(self.blame-loc) + ")")
    end
  | no-module(loc :: A.Loc, mod-name :: String) with:
    #### TODO ###
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

t-pred = T.t-arrow([list: T.t-top], t-bool)
t-pred2 = T.t-arrow([list: T.t-top, T.t-top], t-bool)
t-arrow = T.t-arrow
t-member = T.t-member
t-bot = T.t-bot
t-record = T.t-record

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
  "string-to-number", T.t-top,
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
  "num-random", T.t-top,
  "num-random-seed", T.t-top,
  "num-max", T.t-top,
  "num-min", T.t-top,
  "num-equal", T.t-top,
  "num-within", T.t-top,
  "num-round", T.t-top,
  "num-round-even", T.t-top,
  "num-abs", T.t-top,
  "num-sin", T.t-top,
  "num-cos", T.t-top,
  "num-tan", T.t-top,
  "num-asin", T.t-top,
  "num-acos", T.t-top,
  "num-atan", T.t-top,
  "num-modulo", T.t-top,
  "num-truncate", T.t-top,
  "num-sqrt", T.t-top,
  "num-sqr", T.t-top,
  "num-ceiling", T.t-top,
  "num-floor", T.t-top,
  "num-log", T.t-top,
  "num-exp", T.t-top,
  "num-exact", T.t-top,
  "num-to-rational", T.t-top,
  "num-to-roughnum", T.t-top,
  "num-is-positive", T.t-top,
  "num-is-negative", T.t-top,
  "num-is-non-positive", T.t-top,
  "num-is-non-negative", T.t-top,
  "num-is-integer", T.t-top,
  "num-is-fixnum", T.t-top,
  "num-is-rational", T.t-top,
  "num-is-roughnum", T.t-top,
  "num-expt", T.t-top,
  "num-tostring", T.t-top,
  "num-to-string", T.t-top,
  "num-to-string-digits", T.t-top,
  "num-within-rel", T.t-top,
  "num-within-abs", T.t-top,
  "within-rel", T.t-top,
  "within-rel-now", T.t-top,
  "within-abs", T.t-top,
  "within-abs-now", T.t-top,
  "within", T.t-top,
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

