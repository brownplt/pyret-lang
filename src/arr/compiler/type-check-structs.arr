provide *
provide-types *

import type-logger as LOG

import ast as A
import string-dict as SD
import valueskeleton as VS
import lists as LISTS
import file("type-structs.arr") as TS
import file("compile-structs.arr") as C
import file("type-defaults.arr") as TD

type StringDict = SD.StringDict
type Type = TS.Type
type ModuleType = TS.ModuleType
type DataType = TS.DataType
type Loc = A.Loc

all = LISTS.all
string-dict = SD.string-dict

t-name = TS.t-name
t-arrow = TS.t-arrow
t-app = TS.t-app
t-top = TS.t-top
t-bot = TS.t-bot
t-record = TS.t-record
t-tuple = TS.t-tuple
t-forall = TS.t-forall
t-ref = TS.t-ref
t-data-refinement = TS.t-data-refinement
t-var = TS.t-var
t-existential = TS.t-existential
t-data = TS.t-data
t-variant = TS.t-variant
t-singleton-variant = TS.t-singleton-variant

is-t-name = TS.is-t-name
is-t-app = TS.is-t-app
is-t-top = TS.is-t-top
is-t-bot = TS.is-t-bot
is-t-existential = TS.is-t-existential

new-existential = TS.new-existential
new-type-var = TS.new-type-var

type-member-map = TS.type-member-map

foldr2 = TS.foldr2

fun bind(f, a): a.bind(f) end
fun typing-bind(f, a): a.typing-bind(f) end
fun fold-bind(f, a): a.fold-bind(f) end

data PathElement:
  | arg-path(arg-num :: Number)
  | ret-path
  | app-path(type-num :: Number)
  | record-path(field-name :: String)
  | ref-path
  | tuple-path(tuple-index :: Number)
end

type Path = List<PathElement>

# The StringDict is a mapping from the path of the type
type Structure = SD.StringDict<Set<Path>>

# "exported" context after type checking
data TCInfo:
  | tc-info(types :: SD.StringDict<Type>,
            aliases :: SD.StringDict<Type>,
            data-types :: SD.StringDict<Type>)
end

data Context:
  | typing-context(global-types :: StringDict<Type>, # global name -> type
                   aliases :: StringDict<Type>, # t-name -> aliased type
                   data-types :: StringDict<DataType>, # t-name -> data type
                   modules :: StringDict<ModuleType>, # module name -> module type
                   module-names :: StringDict<String>, # imported name -> module name
                   binds :: StringDict<Type>, # local name -> type
                   constraints :: ConstraintSystem, # constraints should only be added with methods to ensure that they have the proper forms
                   info :: TCInfo,
                   misc :: StringDict<{List<Type>; String}>) # miscellaneous info that is used for logging. Keyed by the function name
sharing:
  method _output(self):
    VS.vs-constr("typing-context", [list: VS.vs-value(self.binds), VS.vs-value(self.constraints)])
  end,
  method get-data-type(self, typ :: Type%(is-t-name)) -> Option<DataType>:
    shadow typ = resolve-alias(typ, self)
    cases(Type) typ:
      | t-name(module-name, name, _, _) =>
        cases(TS.NameOrigin) module-name:
          | module-uri(mod) =>
            cases(Option<ModuleType>) self.modules.get(mod):
              | some(t-mod) =>
                cases(Option<DataType>) t-mod.types.get(name.toname()):
                  | some(shadow typ) => some(typ)
                  | none =>
                    raise("No type " + torepr(typ) + " available on '" + torepr(t-mod) + "'")
                end
              | none =>
                if mod == "builtin":
                  self.data-types.get(name.key())
                else:
                  raise("No module available with the name '" + mod + "'")
                end
            end
          | local =>
            id-key = name.key()
            self.data-types.get(id-key)
          | dependency(_) => TS.dep-error(typ)
        end
      | else => raise("get-data-type should only be called on t-names")
    end
  end,
  method set-global-types(self, global-types :: StringDict<Type>):
    typing-context(global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints, self.info, self.misc)
  end,
  method set-aliases(self, aliases :: StringDict<Type>):
    typing-context(self.global-types, aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints, self.info, self.misc)
  end,
  method set-data-types(self, data-types :: StringDict<DataType>):
    typing-context(self.global-types, self.aliases, data-types, self.modules, self.module-names, self.binds, self.constraints, self.info, self.misc)
  end,
  method set-modules(self, modules :: StringDict<ModuleType>):
    typing-context(self.global-types, self.aliases, self.data-types, modules, self.module-names, self.binds, self.constraints, self.info, self.misc)
  end,
  method set-module-names(self, module-names :: StringDict<String>):
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, module-names, self.binds, self.constraints, self.info, self.misc)
  end,
  method set-binds(self, binds :: StringDict<Type>):
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, binds, self.constraints, self.info, self.misc)
  end,
  method set-constraints(self, constraints :: ConstraintSystem):
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, constraints, self.info, self.misc)
  end,
  method set-info(self, info :: TCInfo):
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints, info, self.misc)
  end,
  method add-binding(self, term-key :: String, assigned-type :: Type) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds.set(term-key, assigned-type), self.constraints, self.info, self.misc)
  end,
  method remove-binding(self, term-key :: String) -> Context:
    current-type = self.binds.get-value(term-key)
    new-info = tc-info(self.info.types.set(term-key, current-type), self.info.aliases, self.info.data-types)
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds.remove(term-key), self.constraints, new-info, self.misc)
  end,
  method add-dict-to-bindings(self, dict :: SD.StringDict<Type>) -> Context:
    new-binds = dict.fold-keys(lam(key, bindings):
      bindings.set(key, dict.get-value(key))
    end, self.binds)
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, new-binds, self.constraints, self.info, self.misc)
  end,
  method add-variable(self, variable :: Type) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-variable(variable), self.info, self.misc)
  end,
  method add-variable-set(self, variables :: Set<Type>) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-variable-set(variables), self.info, self.misc)
  end,
  method add-constraint(self, subtype :: Type, supertype :: Type) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-constraint(subtype, supertype), self.info, self.misc)
  end,
  method add-field-constraint(self, typ :: Type, field-name :: String, field-type :: Type):
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-field-constraint(typ, field-name, field-type), self.info, self.misc)
  end,
  method add-example-variable(self, existential :: Type, arg-types :: List<Type>, ret-type :: Type, loc :: Loc, checking-fun :: (Type, Context -> TypingResult), fun-name :: String) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-example-variable(existential, arg-types, ret-type, loc, checking-fun, fun-name), self.info, self.misc)
  end,
  method add-example-type(self, existential :: Type, typ :: Type) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-example-type(existential, typ), self.info, self.misc)
  end,
  method add-level(self) -> Context:
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints.add-level(), self.info, self.misc)
  end,
  method solve-level(self) -> FoldResult<ConstraintSolution>:
    self.constraints.solve-level(self).bind(lam({new-system; solution}, context) block:
      fold-result(solution, context.set-constraints(new-system))
    end)
  end,
  # this method calls generalize as it will only ever be called on let-bound bindings
  method substitute-in-binds(self, solution :: ConstraintSolution):
    new-binds = self.binds.fold-keys(lam(key, binds):
      bound-type = binds.get-value(key)
      binds.set(key, solution.generalize(solution.apply(bound-type)))
    end, self.binds)
    self.set-binds(new-binds)
  end,
  method add-misc-example-variable(self, fun-key :: String, fun-name :: String) -> Context:
    misc = self.misc.set(fun-key, {[list: ]; fun-name})
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints, self.info, misc)
  end,
  method add-misc-example-type(self, fun-key :: String, typ :: Type) -> Context:
    cases(Option) self.misc.get(fun-key):
      | none =>
        self
      | some({typs; fun-name}) =>
        misc = self.misc.set(fun-key, {link(typ, typs); fun-name})
        typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints, self.info, misc)
    end
  end,
  method substitute-in-misc(self, solution :: ConstraintSolution):
    new-misc = self.misc.fold-keys(lam(key, new-misc):
      {types; name} = self.misc.get-value(key)
      new-types = types.map(lam(typ): solution.apply(typ) end)
      new-misc.set(key, {new-types; name})
    end, [string-dict: ])
    typing-context(self.global-types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.constraints, self.info, new-misc)
  end
end

data ConstraintSolution:
  | constraint-solution(variables :: Set<Type>, substitutions :: StringDict<{Type; Type}>) # existential => {assigned-type; existential}
sharing:
  method apply(self, typ :: Type) -> Type:
    app = lam(x): self.apply(x) end
    cases(ConstraintSolution) self:
      | constraint-solution(_, substitutions) =>
        cases(Type) typ:
          | t-name(_, _, _, _) =>
            typ
          | t-arrow(args, ret, l, inferred) =>
            t-arrow(args.map(app), app(ret), l, inferred)
          | t-app(onto, args, l, inferred) =>
            t-app(app(onto), args.map(app), l, inferred)
          | t-top(_, _) =>
            typ
          | t-bot(_, _) =>
            typ
          | t-record(fields, l, inferred) =>
            map-app = lam(xs): TS.type-member-map(xs, lam(_, x): app(x) end) end
            t-record(map-app(fields), l, inferred)
          | t-tuple(elts, l, inferred) =>
            t-tuple(elts.map(app), l, inferred)
          | t-forall(introduces, onto, l, inferred) =>
            t-forall(introduces, app(onto), l, inferred)
          | t-ref(ref-typ, l, inferred) =>
            t-ref(app(ref-typ), l, inferred)
          | t-data-refinement(data-type, variant-name, l, inferred) =>
            t-data-refinement(app(data-type), variant-name, l, inferred)
          | t-var(_, _, _) =>
            typ
          | t-existential(_, l, inferred) =>
            cases(Option<{Type; Type}>) substitutions.get(typ.key()):
              | none =>
                typ
              | some({assigned-type; exists}) =>
                app(assigned-type.set-loc(l).set-inferred(inferred or assigned-type.inferred))
            end
        end
    end
  end,
  method apply-data-type(self, data-type :: DataType) -> DataType:
    cases(DataType) data-type:
      | t-data(name, params, variants, fields, l) =>
        t-data(name,
              params,
              variants.map(self.apply-variant),
              TS.type-member-map(fields, lam(_, x):
                self.generalize(self.apply(x))
              end),
              l)
    end
  end,
  method apply-variant(self, variant-type :: TS.TypeVariant) -> TS.TypeVariant:
    cases(TS.TypeVariant) variant-type:
      | t-variant(name, fields, with-fields, l) =>
        t-variant(name, fields, TS.type-member-map(with-fields, lam(_, x):
          self.generalize(self.apply(x))
        end), l)
      | t-singleton-variant(name, with-fields, l) =>
        t-singleton-variant(name, TS.type-member-map(with-fields, lam(_, x):
          self.generalize(self.apply(x))
        end), l)
    end
  end,
  method generalize(self, typ :: Type) -> Type:
    fun collect-vars(shadow typ, var-mapping :: StringDict<Type>) -> {Type; StringDict<Type>}:
      cases(Type) typ:
        | t-name(_, _, _, _) =>
          {typ; var-mapping}
        | t-arrow(args, ret, l, inferred) =>
          {new-ret; ret-mapping} = collect-vars(ret, var-mapping)
          {new-args; args-mapping} = args.foldr(lam(arg, {new-args; args-mapping}):
            {new-arg; arg-mapping} = collect-vars(arg, args-mapping)
            {link(new-arg, new-args); arg-mapping}
          end, {empty; ret-mapping})
          {t-arrow(new-args, new-ret, l, inferred); args-mapping}
        | t-app(onto, args, l, inferred) =>
          {new-onto; onto-mapping} = collect-vars(onto, var-mapping)
          {new-args; args-mapping} = args.foldr(lam(arg, {new-args; args-mapping}):
            {new-arg; arg-mapping} = collect-vars(arg, args-mapping)
            {link(new-arg, new-args); arg-mapping}
          end, {empty; onto-mapping})
          {t-app(new-onto, new-args, l, inferred); args-mapping}
        | t-top(_, _) =>
          {typ; var-mapping}
        | t-bot(_, _) =>
          {typ; var-mapping}
        | t-record(fields, l, inferred) =>
          {new-fields; fields-mapping} = fields.fold-keys(lam(key, {new-fields; fields-mapping}):
            field-typ = fields.get-value(key)
            {new-typ; typ-mapping} = collect-vars(field-typ, fields-mapping)
            {new-fields.set(key, new-typ); typ-mapping}
          end, {[string-dict: ]; var-mapping})
          {t-record(new-fields, l, inferred); fields-mapping}
        | t-tuple(elts, l, inferred) =>
          {new-elts; elts-mapping} = elts.foldr(lam(elt, {new-elts; elts-mapping}):
            {new-elt; elt-mapping} = collect-vars(elt, elts-mapping)
            {link(new-elt, new-elts); elt-mapping}
          end, {empty; var-mapping})
          {t-tuple(new-elts, l, inferred); elts-mapping}
        | t-forall(introduces, onto, l, inferred) =>
          {new-onto; onto-mapping} = collect-vars(onto, var-mapping)
          {t-forall(introduces, new-onto, l, inferred); onto-mapping}
        | t-ref(onto, l, inferred) =>
          {new-onto; onto-mapping} = collect-vars(onto, var-mapping)
          {t-ref(new-onto, l, inferred); onto-mapping}
        | t-data-refinement(data-type, variant-name, l, inferred) =>
          {new-data-type; data-type-mapping} = collect-vars(data-type, var-mapping)
          {t-data-refinement(new-data-type, variant-name, l, inferred); data-type-mapping}
        | t-var(_, _, _) =>
          {typ; var-mapping}
        | t-existential(id, l, _) =>
          if self.variables.member(typ):
            cases(Option<Type>) var-mapping.get(typ.key()):
              | none =>
                new-var = new-type-var(l)
                {new-var; var-mapping.set(typ.key(), new-var)}
              | some(mapped-typ) =>
                {mapped-typ; var-mapping}
            end
          else:
            {typ; var-mapping}
          end
      end
    end
    {new-typ; vars-mapping} = collect-vars(typ, [string-dict: ])
    vars = vars-mapping.map-keys(lam(key): vars-mapping.get-value(key) end)
    if is-empty(vars): typ else: t-forall(vars, new-typ, typ.l, false) end
  end
end

data ConstraintSystem:
  | constraint-system(variables :: Set<Type>, # the constrained existentials
                      constraints :: List<{Type; Type}>, # {subtype; supertype}
                      refinement-constraints :: List<{Type; Type}>, # {existential; t-data-refinement}
                      field-constraints :: StringDict<{Type; StringDict<List<Type>>}>, # type -> {type; field labels -> field types (with the location of their use)}
                      example-types :: StringDict<{Type; {arg-types :: List<Type>, ret-type :: Type, loc :: Loc}; List<Type>; (Type, Context -> TypingResult); String}>, # existential type for function (from examples) -> {existential; annotation types; example types; check function; function name}
                      next-system :: ConstraintSystem)
  | no-constraints
sharing:
  method add-variable(self, variable :: Type):
    cases(ConstraintSystem) self:
      | no-constraints => raise("can't add variable to an uninitialized system")
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        if TS.is-t-existential(variable):
          constraint-system(variables.add(variable), constraints, refinement-constraints, field-constraints, example-types, next-system)
        else:
          self
        end
    end
  end,
  method add-variable-set(self, new-variables :: Set<Type>):
    cases(ConstraintSystem) self:
      | no-constraints => raise("can't add variables to an uninitialized system")
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        constraint-system(variables.union(new-variables), constraints, refinement-constraints, field-constraints, example-types, next-system)
    end
  end,
  method add-constraint(self, subtype :: Type, supertype :: Type):
    cases(ConstraintSystem) self:
      | no-constraints => raise("can't add constraints to an uninitialized system: " + tostring(subtype) + " = " + tostring(supertype) + "\n" + tostring(subtype.l) + "\n" + tostring(supertype.l))
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        fun add-refinement(exists, refinement):
          constraint-system(variables, constraints, link({exists; refinement}, refinement-constraints), field-constraints, example-types, next-system)
        end
        if TS.is-t-existential(subtype) and TS.is-t-data-refinement(supertype):
          add-refinement(subtype, supertype)
        else if TS.is-t-existential(supertype) and TS.is-t-data-refinement(subtype):
          add-refinement(supertype, subtype)
        else:
          constraint-system(variables, link({subtype; supertype}, constraints), refinement-constraints, field-constraints, example-types, next-system)
        end
    end
  end,
  method add-field-constraint(self, typ :: Type, field-name :: String, field-type :: Type):
    cases(ConstraintSystem) self:
      | no-constraints => raise("can't add constraints to an uninitialized system")
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        new-field-constraints = cases(Option) field-constraints.get(typ.key()):
          | some({shadow typ; label-mapping}) =>
            new-label-mapping = cases(Option) label-mapping.get(field-name):
              | some(current-types) =>
                label-mapping.set(field-name, link(field-type, current-types))
              | none =>
                label-mapping.set(field-name, [list: field-type])
            end
            field-constraints.set(typ.key(), {typ; new-label-mapping})
          | none =>
            field-constraints.set(typ.key(), {typ; [string-dict: field-name, [list: field-type]]})
        end
        constraint-system(variables, constraints, refinement-constraints, new-field-constraints, example-types, next-system)
    end
  end,
  method add-example-variable(self, existential :: Type, arg-types :: List<Type>, ret-type :: Type, loc :: Loc, checking-fun :: (Type, Context -> TypingResult), fun-name :: String):
    cases(ConstraintSystem) self:
      | no-constraints => raise("can't add constraints to an uninitialized system")
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types.set(existential.key(), {existential; {arg-types: arg-types, ret-type: ret-type, loc: loc}; [list: ]; checking-fun; fun-name}), next-system)
    end
  end,
  method add-example-type(self, existential :: Type, typ :: Type):
    cases(ConstraintSystem) self block:
      | no-constraints => raise("can't add constraints to an uninitialized system")
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        cases(Option) example-types.get(existential.key()):
          | none =>
            constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system.add-example-type(existential, typ))
          | some({_; inference-data; typs; checking-fun; fun-name}) =>
            new-example-types = example-types.set(existential.key(), {existential; inference-data; link(typ, typs); checking-fun; fun-name})
            constraint-system(variables, constraints, refinement-constraints, field-constraints, new-example-types, next-system)
        end
    end
  end,
  method add-level(self):
    constraint-system(empty-tree-set, empty, empty, SD.make-string-dict(), SD.make-string-dict(), self)
  end,
  method solve-level-helper(self, solution :: ConstraintSolution, context :: Context) -> FoldResult<{ConstraintSystem; ConstraintSolution}>:
    solve-helper-constraints(self, solution, context).bind(lam({system; shadow solution}, shadow context):
      solve-helper-refinements(system, solution, context).bind(lam({shadow system; shadow solution}, shadow context):
        solve-helper-examples(system, solution, context).bind(lam({shadow system; shadow solution}, shadow context):
          solve-helper-fields(system, solution, context)
        end)
      end)
    end)
  end,
  method solve-level(self, context :: Context) -> FoldResult<{ConstraintSystem; ConstraintSolution}>:
    cases(ConstraintSystem) self:
      | no-constraints =>
        fold-result({self; constraint-solution(empty-tree-set, [string-dict: ])}, context)
      | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
        # introduce a half level so any constraints depending on test inference can be solved after test inference
        shadow next-system = next-system.add-level()
        {shadow variables; shadow next-system} = example-types.fold-keys(lam(key, {shadow variables; shadow next-system}):
          {existential; _; _; _; _} = example-types.get-value(key)
          {variables.remove(existential); next-system.add-variable(existential)}
        end, {variables; next-system})
        system = constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system)
        system.solve-level-helper(constraint-solution(empty-tree-set, [string-dict: ]), context).bind(lam({shadow system; solution}, shadow context):
          # This is solving the level introduced above
          shadow system = system.next-system.add-variable-set(system.variables)
          system.solve-level-helper(solution, context).bind(lam({shadow system; shadow solution}, shadow context):
            shadow solution = constraint-solution(variables, solution.substitutions)
            cases(ConstraintSystem) system:
              | no-constraints =>
                fold-result({system; solution}, context)
              | constraint-system(shadow variables, _, _, _, _, shadow next-system) =>
                shadow solution = constraint-solution(variables, solution.substitutions)
                shadow next-system = if is-constraint-system(next-system):
                  next-system.add-variable-set(variables)
                else:
                  next-system
                end
                fold-result({next-system; solution}, context)
            end
          end)
        end)
    end
  end
end

fun substitute-in-constraints(new-type :: Type, type-var :: Type, constraints :: List<{Type; Type}>):
  constraints.map(lam({subtype; supertype}):
    {subtype.substitute(new-type, type-var); supertype.substitute(new-type, type-var)}
  end)
end

fun substitute-in-field-constraints(new-type :: Type, type-var :: Type, field-constraints :: StringDict<{Type; StringDict<List<Type>>}>):
  field-constraints.fold-keys(lam(key, new-constraints):
    {constraint-type; field-mappings} = field-constraints.get-value(key)
    new-constraint-type = constraint-type.substitute(new-type, type-var)
    new-field-mappings = field-mappings.fold-keys(lam(field-name, new-field-mappings):
      types = field-mappings.get-value(field-name)
      new-types = types.map(lam(typ): typ.substitute(new-type, type-var) end)
      new-field-mappings.set(field-name, new-types)
    end, [string-dict: ])
    new-constraints.set(key, {new-constraint-type; new-field-mappings})
  end, [string-dict: ])
end

fun substitute-in-example-types(new-type :: Type, type-var :: Type, example-types :: StringDict<{Type; {arg-types :: List<Type>, ret-type :: Type, loc :: Loc}; List<Type>; (Type, Context -> TypingResult); String}>):
  example-types.fold-keys(lam(key, new-example-types):
    {existential; info; typs; check-fun; fun-name} = example-types.get-value(key)
    new-example-types.set(key, {existential; info; typs.map(lam(typ): typ.substitute(new-type, type-var) end); check-fun; fun-name})
  end, [string-dict: ])
end

fun add-substitution(new-type :: Type, type-var :: Type, system :: ConstraintSystem, solution :: ConstraintSolution) -> {solution :: ConstraintSolution, system :: ConstraintSystem}:
  substitutions = solution.substitutions.set(type-var.key(), {new-type; type-var})
  constraints = substitute-in-constraints(new-type, type-var, system.constraints)
  refinement-constraints = substitute-in-constraints(new-type, type-var, system.refinement-constraints)
  field-constraints = substitute-in-field-constraints(new-type, type-var, system.field-constraints)
  example-types = substitute-in-example-types(new-type, type-var, system.example-types)

  {solution: constraint-solution(empty-tree-set, substitutions), system: constraint-system(system.variables, constraints, refinement-constraints, field-constraints, example-types, system.next-system)}
end

fun solve-helper-constraints(system :: ConstraintSystem, solution :: ConstraintSolution, context :: Context) -> FoldResult<{ConstraintSystem; ConstraintSolution}>:
  fun add-substitution-and-continue(new-type :: Type, type-var :: Type, shadow system :: ConstraintSystem, shadow solution :: ConstraintSolution, shadow context :: Context):
    new-solution-and-system = add-substitution(new-type, type-var, system, solution)
    solve-helper-constraints(new-solution-and-system.system, new-solution-and-system.solution, context)
  end

  cases(ConstraintSystem) system:
    | no-constraints => fold-result({system; solution}, context)
    | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
      cases(List<{Type; Type}>) constraints:
        | empty => fold-result({system; solution}, context)
        | link(first, rest) =>
          shadow system = constraint-system(variables, rest, refinement-constraints, field-constraints, example-types, next-system)
          {subtype; supertype} = first
          if is-t-top(supertype) or is-t-bot(subtype):
            solve-helper-constraints(system, solution, context)
          else:
            cases(Type) supertype:
              | t-existential(b-id, b-loc, _) =>
                cases(Type) subtype:
                  | t-existential(a-id, a-loc, _) =>
                    if a-id == b-id:
                      solve-helper-constraints(system, solution, context)
                    else:
                      if variables.member(subtype):
                        add-substitution-and-continue(supertype, subtype, system, solution, context)
                      else if variables.member(supertype):
                        add-substitution-and-continue(subtype, supertype, system, solution, context)
                      else:
                        solve-helper-constraints(
                          constraint-system(system.variables,
                                            system.constraints,
                                            system.refinement-constraints,
                                            system.field-constraints,
                                            system.example-types,
                                            system.next-system.add-constraint(subtype, supertype)),
                          solution,
                          context)
                      end
                    end
                  | else =>
                    if variables.member(supertype):
                      if subtype.has-variable-free(supertype):
                        add-substitution-and-continue(subtype, supertype, system, solution, context)
                      else:
                        fold-errors([list: C.recursive-type-constraints(supertype, subtype)])
                      end
                    else:
                      solve-helper-constraints(
                        constraint-system(system.variables,
                                          system.constraints,
                                          system.refinement-constraints,
                                          system.field-constraints,
                                          system.example-types,
                                          system.next-system.add-constraint(subtype, supertype)),
                        solution,
                        context)
                    end
                end
              | t-data-refinement(b-data-type, b-variant-name, b-loc, _) =>
                solve-helper-constraints(system.add-constraint(subtype, b-data-type), solution, context)
              | t-forall(b-introduces, b-onto, b-loc, _) =>
                new-existentials = b-introduces.map(lam(variable): new-existential(variable.l, false) end)
                shadow b-onto = foldr2(lam(shadow b-onto, variable, exists):
                  b-onto.substitute(exists, variable)
                end, b-onto, b-introduces, new-existentials)
                shadow system = system.add-variable-set(list-to-tree-set(new-existentials))
                solve-helper-constraints(system.add-constraint(subtype, b-onto), solution, context)
              | else =>
                cases(Type) subtype:
                  | t-name(a-module-name, a-id, a-loc, _) =>
                    cases(Type) supertype:
                      | t-name(b-module-name, b-id, b-loc, _) =>
                        if (a-module-name == b-module-name) and (a-id == b-id):
                          solve-helper-constraints(system, solution, context)
                        else:
                          fold-errors([list: C.type-mismatch(subtype, supertype)])
                        end
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-arrow(a-args, a-ret, a-loc, _) =>
                    cases(Type) supertype:
                      | t-arrow(b-args, b-ret, b-loc, _) =>
                        if not(a-args.length() == b-args.length()):
                          # TODO(MATT) mention argument length
                          fold-errors([list: C.type-mismatch(subtype, supertype)])
                        else:
                          shadow system = foldr2(lam(shadow system, a-arg, b-arg):
                            system.add-constraint(b-arg, a-arg)
                          end, system.add-constraint(a-ret, b-ret), a-args, b-args)
                          solve-helper-constraints(system, solution, context)
                        end
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-app(a-onto, a-args, a-loc, _) =>
                    cases(Type) supertype:
                      | t-app(b-onto, b-args, b-loc, _) =>
                        if not(a-args.length() == b-args.length()):
                          # TODO(MATT) mention argument length
                          fold-errors([list: C.type-mismatch(subtype, supertype)])
                        else:
                          shadow system = foldr2(lam(shadow system, a-arg, b-arg):
                            system.add-constraint(a-arg, b-arg)
                          end, system.add-constraint(a-onto, b-onto), a-args, b-args)
                          solve-helper-constraints(system, solution, context)
                        end
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-top(a-loc, _) =>
                    cases(Type) supertype:
                      | t-top(b-loc, _) =>
                        solve-helper-constraints(system, solution, context)
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-bot(a-loc, _) =>
                    cases(Type) supertype:
                      | t-bot(b-loc, _) =>
                        solve-helper-constraints(system, solution, context)
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-record(a-fields, a-loc, _) =>
                    cases(Type) supertype:
                      | t-record(b-fields, b-loc, _) =>
                        foldr-fold-result(lam(b-key, shadow context, shadow system):
                          cases(Option<Type>) a-fields.get(b-key):
                            | some(a-field) =>
                              b-field = b-fields.get-value(b-key)
                              fold-result(system.add-constraint(a-field, b-field), context)
                            | none =>
                              # TODO(MATT): field missing error
                              fold-errors([list: C.type-mismatch(subtype, supertype)])
                          end
                        end, b-fields.keys-list(), context, system).bind(lam(shadow system, shadow context):
                          solve-helper-constraints(system, solution, context)
                        end)
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-tuple(a-elts, a-loc, _) =>
                    cases(Type) supertype:
                      | t-tuple(b-elts, b-loc, _) =>
                        if not(a-elts.length() == b-elts.length()):
                          # TODO(MATT): more specific error
                          fold-errors([list: C.type-mismatch(subtype, supertype)])
                        else:
                          shadow system = foldr2(lam(shadow system, a-elt, b-elt):
                            system.add-constraint(a-elt, b-elt)
                          end, system, a-elts, b-elts)
                          solve-helper-constraints(system, solution, context)
                        end
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-forall(a-introduces, a-onto, a-loc, _) =>
                    new-existentials = a-introduces.map(lam(variable): new-existential(variable.l, false) end)
                    shadow a-onto = foldr2(lam(shadow a-onto, variable, exists):
                      a-onto.substitute(exists, variable)
                    end, a-onto, a-introduces, new-existentials)
                    shadow system = system.add-variable-set(list-to-tree-set(new-existentials))
                    solve-helper-constraints(system.add-constraint(a-onto, supertype), solution, context)
                  | t-ref(a-typ, a-loc, _) =>
                    cases(Type) supertype:
                      | t-ref(b-typ, b-loc, _) =>
                        solve-helper-constraints(system.add-constraint(a-typ, b-typ), solution, context)
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-data-refinement(a-data-type, a-variant-name, a-loc, _) =>
                    solve-helper-constraints(system.add-constraint(a-data-type, supertype), solution, context)
                  | t-var(a-id, a-loc, _) =>
                    cases(Type) supertype:
                      | t-var(b-id, b-loc, _) =>
                        if a-id == b-id:
                          solve-helper-constraints(system, solution, context)
                        else:
                          fold-errors([list: C.type-mismatch(subtype, supertype)])
                        end
                      | else =>
                        fold-errors([list: C.type-mismatch(subtype, supertype)])
                    end
                  | t-existential(a-id, a-loc, _) =>
                    shadow system = system.add-constraint(supertype, subtype)
                    solve-helper-constraints(system, solution, context)
                end
            end
          end
      end
  end
end

fun solve-helper-refinements(system :: ConstraintSystem, solution :: ConstraintSolution, context :: Context) -> FoldResult<{ConstraintSystem; ConstraintSolution}>:
  cases(ConstraintSystem) system:
    | no-constraints => fold-result({system; solution}, context)
    | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
      partitioned = partition(lam({lhs; _}): TS.is-t-existential(lhs) end, refinement-constraints)
      shadow refinement-constraints = partitioned.is-true
      normal-constraints = partitioned.is-false
      if is-link(normal-constraints):
        shadow system = constraint-system(variables, normal-constraints, refinement-constraints, field-constraints, example-types, next-system)
        solve-helper-constraints(system, solution, context).bind(lam({shadow system; shadow solution}, shadow context):
          solve-helper-refinements(system, solution, context)
        end)
      else:
        if is-empty(refinement-constraints):
          fold-result({system; solution}, context)
        else:
          refinement-partition = partition(lam({exists; _}): variables.member(exists) end, refinement-constraints)
          shadow refinement-constraints = refinement-partition.is-true
          next-level-refinements = refinement-partition.is-false
          if is-link(next-level-refinements):
            shadow next-system = next-level-refinements.foldl(lam({lhs; rhs}, shadow next-system):
              next-system.add-constraint(lhs, rhs)
            end, next-system)
            shadow system = constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system)
            solve-helper-refinements(system, solution, context)
          else:
            mappings :: StringDict<{Type; List<Type>}> = refinement-constraints.foldl(lam({exists; refinement}, mappings):
              cases(Option<{Type; List<Type>}>) mappings.get(exists.key()):
                | none =>
                  mappings.set(exists.key(), {exists; [list: refinement]})
                | some({_; others}) =>
                  mappings.set(exists.key(), {exists; link(refinement, others)})
              end
            end, [string-dict: ])

            {temp-system; temp-variables} = mappings.fold-keys(lam(key, {temp-system; temp-variables}):
              {existential; refinements} = mappings.get-value(key)
              temp-variable = new-existential(existential.l, false)
              shadow temp-system = temp-system.add-variable(temp-variable)
              shadow temp-system = refinements.foldl(lam(refinement, shadow temp-system):
                cases(Type) refinement:
                  | t-data-refinement(data-type, variant-name, l, _) =>
                    temp-system.add-constraint(temp-variable, data-type)
                  | else =>
                    temp-system.add-constraint(temp-variable, refinement)
                end
              end, temp-system)
              {temp-system; temp-variables.add(temp-variable.key())}
            end, {system; empty-tree-set})

            solve-helper-constraints(temp-system, constraint-solution(empty-tree-set, [string-dict: ]), context).bind(lam({shadow temp-system; temp-solution}, shadow context):
              cases(ConstraintSolution) temp-solution:
                | constraint-solution(_, temp-substitutions) =>
                  temp-keys-set = temp-substitutions.keys()
                  shadow temp-keys-set = temp-keys-set.difference(temp-variables)
                  # TODO(MATT): make this more robust
                  if (temp-keys-set.size() > 0): # or not(temp-system.refinement-constraints.length() == refinement-constraints.length()): # some change in refinement constraints
                    shadow solution = constraint-solution(empty-tree-set, temp-substitutions.fold-keys(lam(key, shadow substitutions):
                      substitutions.set(key, temp-substitutions.get-value(key))
                    end, solution.substitutions))
                    solve-helper-refinements(temp-system, solution, context)
                  else:
                    # merge all constraints for each existential variable
                    # same data-refinements get merged otherwise goes to the inner data type

                    new-system-and-solution = mappings.fold-keys(lam(key, system-and-solution):
                      {exists; refinements} = mappings.get-value(key)
                      merged-type = refinements.rest.foldl(lam(refinement, merged):
                        cases(Type) merged:
                          | t-data-refinement(a-data-type, a-variant-name, a-loc, _) =>
                            cases(Type) refinement:
                              | t-data-refinement(b-data-type, b-variant-name, b-loc, _) =>
                                if a-variant-name == b-variant-name:
                                  merged
                                else:
                                  a-data-type
                                end
                              | else => refinement
                            end
                          | else => merged
                        end
                      end, refinements.first)
                      add-substitution(merged-type, exists, system-and-solution.system, system-and-solution.solution)
                    end, {system: system, solution: solution})
                    new-system = new-system-and-solution.system
                    new-solution = new-system-and-solution.solution
                    fold-result({constraint-system(variables, empty, empty, new-system.field-constraints, new-system.example-types, next-system); constraint-solution(empty-tree-set, new-solution.substitutions)}, context)
                  end
              end
            end)
          end
        end
      end
  end
end

fun solve-helper-fields(system :: ConstraintSystem, solution :: ConstraintSolution, context :: Context) -> FoldResult<{ConstraintSystem; ConstraintSolution}>:
  cases(ConstraintSystem) system:
    | no-constraints => fold-result({system; solution}, context)
    | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
      cases(List<String>) field-constraints.keys-list():
        | empty => fold-result({system; solution}, context)
        | link(first, rest) =>
          {typ; field-mappings} = field-constraints.get-value(first)
          shadow system = constraint-system(system.variables, system.constraints, system.refinement-constraints, system.field-constraints.remove(first), system.example-types, system.next-system)
          instantiate-object-type(typ, context).bind(lam(shadow typ, shadow context):
            shadow system = cases(ConstraintSystem) context.constraints:
              | no-constraints =>
                system
              | constraint-system(possible-new-variables, _, _, _, _, _) =>
                constraint-system(system.variables.union(possible-new-variables), system.constraints, system.refinement-constraints, system.field-constraints, system.example-types, system.next-system)
            end

            cases(Type) typ:
              | t-record(fields, l, _) =>
                field-set = fields.keys()
                required-field-set = field-mappings.keys()
                intersection = field-set.intersect(required-field-set)
                remaining-fields = required-field-set.difference(intersection)
                if remaining-fields.size() > 0:
                  missing-field-errors = remaining-fields.to-list().map(lam(remaining-field-name):
                    C.object-missing-field(remaining-field-name, tostring(typ), typ.l, field-mappings.get-value(remaining-field-name).get(0).l)
                  end)
                  fold-errors(missing-field-errors)
                else:
                  shadow system = intersection.fold(lam(shadow system, field-name):
                    field-mappings.get-value(field-name).foldl(lam(field-type, shadow system):
                      object-field-type = fields.get-value(field-name)
                      system.add-constraint(object-field-type, field-type)
                    end, system)
                  end, system)
                  system.solve-level-helper(solution, context)
                end
              | t-existential(_, _, _) =>
                if variables.member(typ):
                  fold-errors([list: C.unable-to-infer(typ.l)])
                else:
                  shadow next-system = field-mappings.fold-keys(lam(field-name, shadow next-system):
                    field-types = field-mappings.get-value(field-name)
                    field-types.foldl(lam(field-type, shadow next-system):
                      next-system.add-field-constraint(typ, field-name, field-type)
                    end, next-system)
                  end, system.next-system)
                  shadow system = constraint-system(system.variables, system.constraints, system.refinement-constraints, system.field-constraints, system.example-types, next-system)
                  solve-helper-fields(system, solution, context)
                end
              | else =>
                instantiate-data-type(typ, context).bind(lam(data-type, shadow context):
                  data-fields = data-type.fields
                  foldr-fold-result(lam(field-name, shadow context, shadow system):
                    cases(Option<Type>) data-fields.get(field-name):
                      | none =>
                        fold-errors([list: C.object-missing-field(field-name, tostring(typ), typ.l, field-mappings.get-value(field-name).l)])
                      | some(data-field-type) =>
                        shadow system = field-mappings.get-value(field-name).foldl(lam(field-type, shadow system):
                          system.add-constraint(data-field-type, field-type)
                        end, system)
                        fold-result(system, context)
                    end
                  end, field-mappings.keys-list(), context, system).bind(lam(shadow system, shadow context):
                    system.solve-level-helper(solution, context)
                  end)
                end)
            end
          end)
      end
  end
end

# TODO(MATT): check something about incomplete-examples
# TODO(MATT): check the types of the resulting values
fun solve-helper-examples(system :: ConstraintSystem, solution :: ConstraintSolution, context :: Context) -> FoldResult<{ConstraintSystem; ConstraintSolution}>:
  cases(ConstraintSystem) system:
    | no-constraints => fold-result({system; solution}, context)
    | constraint-system(variables, constraints, refinement-constraints, field-constraints, example-types, next-system) =>
      foldr-fold-result(lam(existential-key, shadow context, {shadow system; shadow solution}):
        {existential; _; fun-examples; checking-fun; fun-name} = example-types.get-value(existential-key)
        shadow fun-examples = fun-examples.map(lam(example): remove-refinements-and-foralls(example) end)
        partitioned = partition(lam(typ): typ.free-variables().size() == 0 end, fun-examples)
        complete-examples = partitioned.is-true
        incomplete-examples = partitioned.is-false
        cases(List<Type>) complete-examples block:
          | link(first, rest) =>
            generalized = rest.foldr(generalize-type, first)
            first-structure = find-structure(first)
            common-structure = rest.foldr(find-common-structure, first-structure)
            new-type = maintain-common-structure(common-structure, generalized)

            log-payload = "{"
              + "'function-name': " + "'" + fun-name + "'" + ","
              + "'inferred-type': " + "'" + tostring(new-type) + "'" + ","
              + "}"
            LOG.log("test-inferred-type", log-payload)

            fold-result({system; constraint-solution(empty-tree-set, solution.substitutions.set(existential-key, {new-type; existential}))}, context)
          | empty => fold-errors([list: C.unann-failed-test-inference(existential.l)])
        end
      end, example-types.keys-list(), context, {system; solution})
  end
end

fun remove-refinements-and-foralls(typ :: Type) -> Type:
  rraf = remove-refinements-and-foralls
  cases(Type) typ:
    | t-name(_, _, _, _) =>
      typ
    | t-arrow(args, ret, l, inferred) =>
      new-args = args.map(rraf)
      new-ret = rraf(ret)
      t-arrow(new-args, new-ret, l, inferred)
    | t-app(onto, args, l, inferred) =>
      new-onto = rraf(onto)
      new-args = args.map(rraf)
      t-app(new-onto, new-args, l, inferred)
    | t-top(_, _) => typ
    | t-bot(_, _) => typ
    | t-record(fields, l, inferred) =>
      new-fields = type-member-map(fields, {(_, field-type): rraf(field-type)})
      t-record(new-fields, l, inferred)
    | t-tuple(elts, l, inferred) =>
      t-tuple(elts.map(rraf), l, inferred)
    | t-forall(introduces, onto, l, inferred) =>
      new-onto = introduces.foldr(lam(a-var, new-onto): new-onto.substitute(new-existential(a-var.l, false), a-var) end, onto)
      shadow new-onto = rraf(new-onto)
      new-onto
    | t-ref(ref-typ, l, inferred) =>
      t-ref(rraf(ref-typ), inferred)
    | t-data-refinement(data-type, variant-name, l, inferred) =>
      data-type
    | t-var(id, l, _) =>
      typ
    | t-existential(id, l, inferred) =>
      typ
  end
end

fun generalize-type(current-type :: Type, next-type :: Type):
  new-var = lam(): new-existential(current-type.l, false) end
  cases(Type) current-type:
    | t-name(a-module-name, a-id, _, _) =>
      cases(Type) next-type:
        | t-name(b-module-name, b-id, _, _) =>
          if (a-module-name == b-module-name) and (a-id == b-id): current-type
          else: new-var() end
        | else => new-var()
      end
    | t-arrow(a-args, a-ret, a-l, a-inferred) =>
      cases(Type) next-type:
        | t-arrow(b-args, b-ret, _, _) =>
          if a-args.length() <> b-args.length(): new-var()
          else:
            new-args = map2(generalize-type, a-args, b-args)
            new-ret = generalize-type(a-ret, b-ret)
            t-arrow(new-args, new-ret, a-l, a-inferred)
          end
        | else => new-var()
      end
    | t-app(a-onto, a-args, a-l, a-inferred) =>
      cases(Type) next-type:
        | t-app(b-onto, b-args, _, _) =>
          if (a-args.length() <> b-args.length()) or (a-onto <> b-onto): new-var()
          else:
            new-args = map2(generalize-type, a-args, b-args)
            t-app(a-onto, new-args, a-l, a-inferred)
          end
        | else => new-var()
      end
    | t-top(_, _) =>
      cases(Type) next-type:
        | t-top(_, _) => current-type
        | else => new-var()
      end
    | t-bot(_, _) =>
      cases(Type) next-type:
        | t-bot(_, _) => current-type
        | else => new-var()
      end
    | t-record(a-fields, a-l, a-inferred) =>
      cases(Type) next-type:
        | t-record(b-fields, _, _) =>
          keys-set = a-fields.keys().intersect(b-fields.keys())
          new-fields = keys-set.fold(lam(new-fields, key):
            new-fields.set(key, generalize-type(a-fields.get-value(key), b-fields.get-value(key)))
          end, [string-dict: ])
          t-record(new-fields, a-l, a-inferred)
        | else => new-var()
      end
    | t-tuple(a-elts, a-l, a-inferred) =>
      cases(Type) next-type:
        | t-tuple(b-elts, _, _) =>
          if a-elts.length() <> b-elts.length(): new-var()
          else:
            new-elts = map2(generalize-type, a-elts, b-elts)
            t-tuple(new-elts, a-l, a-inferred)
          end
        | else => new-var()
      end
    | t-forall(_, _, _, _) => raise("foralls should have been removed")
    | t-ref(a-typ, a-l, a-inferred) =>
      cases(Type) next-type:
        | t-ref(b-typ, _, _) =>
          t-ref(generalize-type(a-typ, b-typ), a-l, a-inferred)
        | else => new-var()
      end
    | t-data-refinement(_, _, _, _) => raise("refinements should have been removed")
    | t-var(a-id, a-l, a-inferred) =>
      cases(Type) next-type:
        | t-var(b-id, _, _) =>
          if a-id == b-id: current-type
          else: new-var()
          end
        | else => new-var()
      end
    | t-existential(_, _, _) => current-type
  end
end

fun find-common-structure(typ :: Type, structure :: Structure) -> Structure:
  new-structure = find-structure(typ)
  new-structure.fold-keys(lam(new-key, result-structure):
    cases(Option<Set<Path>>) structure.get(new-key):
      | none => result-structure
      | some(old-set) =>
        result-set = new-structure.get-value(new-key)
        result-structure.set(new-key, result-set.intersect(old-set))
    end
  end, [string-dict: ])
end

fun find-structure(typ :: Type) -> Structure:
  flattened-type = flatten-tree-with-paths(typ)

  fun gather-set(shadow typ :: Type, rest :: List<{Type; Path}>) -> Set<Path>:
    rest.foldr(lam({rest-typ; path}, current-set):
      if typ == rest-typ:
        current-set.add(path)
      else:
        current-set
      end
    end, [list-set: ])
  end

  fun _find-structure(_flattened-type):
    cases(List<{Type; Path}>) _flattened-type:
      | empty => [SD.string-dict: ]
      | link({first-type; first-path}, rest) =>
        temp-set = gather-set(first-type, flattened-type)
        first-set = temp-set.remove(first-path)
        _find-structure(rest).set(tostring(first-path), first-set)
    end
  end

  _find-structure(flattened-type)
end

fun flatten-list<X>(xs :: List<List<X>>) -> List<X>:
  xs.foldl(lam(a, b): b.append(a) end, empty)
end

fun flatten-tree-with-paths(typ :: Type) -> List<{Type; Path}>:
  fun _flatten-tree-with-paths(shadow typ, current-path :: Path) -> List<{Type; Path}>:
    cases(Type) typ:
      | t-name(_, _, _, _) =>
        [list: {typ; current-path}]
      | t-arrow(args, ret, _, _) =>
        arg-pairs = map_n(lam(idx, arg):
          _flatten-tree-with-paths(arg, current-path.append([list: arg-path(idx)]))
        end, 0, args)
        ret-pairs = _flatten-tree-with-paths(ret, current-path.append([list: ret-path]))
        flatten-list(arg-pairs)
          .append(ret-pairs)
          .append([list: {typ; current-path}])
      | t-app(onto, args, _, _) =>
        type-pairs = map_n(lam(idx, arg):
          _flatten-tree-with-paths(arg, current-path.append([list: app-path(idx)]))
        end, 0, args)
        #onto-pairs = _flatten-tree-with-paths(onto, current-path.append([list: app-onto-path]))
        flatten-list(type-pairs)
        #  .append(onto-pairs)
          .append([list: {typ; current-path}])
      | t-top(_, _) =>
        [list: {typ; current-path}]
      | t-bot(_, _) =>
        [list: {typ; current-path}]
      | t-record(fields, _, _) =>
        field-pairs = fields.fold-keys(lam(field-name, pairs):
          pairs.append(_flatten-tree-with-paths(fields.get-value(field-name), current-path.append([list: record-path(field-name)])))
        end, empty)
        field-pairs.append([list: {typ; current-path}])
      | t-tuple(elts, _, _) =>
        elt-pairs = map_n(lam(idx, elt):
          _flatten-tree-with-paths(elt, current-path.append([list: tuple-path(idx)]))
        end, 0, elts)
        flatten-list(elt-pairs)
          .append([list: {typ; current-path}])
      | t-forall(introduces, onto, l, _) =>
        raise("Foralls should have been removed")
      | t-ref(ref-type, _, _) =>
        _flatten-tree-with-paths(ref-type, current-path.append([list: ref-path]))
          .append([list: {typ; current-path}])
      | t-data-refinement(_, _, _, _) =>
        raise("Refinements should have been removed")
      | t-var(_, _, _) =>
        [list: {typ; current-path}]
      | t-existential(_, _, _) =>
        [list: {typ; current-path}]
    end
  end
  _flatten-tree-with-paths(typ, empty)
end

fun maintain-common-structure(struct :: Structure, typ :: Type) -> Type:
  # sets all common existentials to the same variable
  # new-paths is a map from path -> existential to place at that path
  fun _maintain-structure(shadow typ, current-path :: Path, new-paths :: SD.MutableStringDict<Type>):
    cases(Type) typ:
      | t-name(_, _, _, _) =>
        typ
      | t-arrow(args, ret, l, inferred) =>
        new-args = map_n(lam(idx, arg):
          _maintain-structure(arg, current-path.append([list: arg-path(idx)]), new-paths)
        end, 0, args)
        new-ret = _maintain-structure(ret, current-path.append([list: ret-path]), new-paths)
        t-arrow(new-args, new-ret, l, inferred)
      | t-app(onto, args, l, inferred) =>
        new-args = map_n(lam(idx, arg):
          _maintain-structure(arg, current-path.append([list: app-path(idx)]), new-paths)
        end, 0, args)
        t-app(onto, new-args, l, inferred)
      | t-top(_, _) =>
        typ
      | t-bot(_, _) =>
        typ
      | t-record(fields, l, inferred) =>
        new-fields = type-member-map(fields, lam(field-name, field-type):
          _maintain-structure(field-type, current-path.append([list: record-path(field-name)]), new-paths)
        end)
        t-record(new-fields, l, inferred)
      | t-tuple(elts, l, inferred) =>
        new-elts = map_n(lam(idx, elt):
          _maintain-structure(elt, current-path.append([list: tuple-path(idx)]), new-paths)
        end, 0, elts)
        t-tuple(new-elts, l, inferred)
      | t-forall(_, _, _, _) =>
        raise("Foralls should have been removed")
      | t-ref(ref-type, l, inferred) =>
        t-ref(_maintain-structure(ref-type, current-path.append([list: ref-path]), new-paths), l, inferred)
      | t-data-refinement(_, _, _, _) =>
        raise("Refinements should have been removed")
      | t-var(_, _, _) =>
        typ
      | t-existential(_, _, _) =>
        cases(Option<Type>) new-paths.get-now(tostring(current-path)):
          | some(new-exists) =>
            new-exists
          | none =>
            cases(Option<Set<Path>>) struct.get(tostring(current-path)) block:
              | some(paths) =>
                each(lam(path):
                  new-paths.set-now(tostring(path), typ)
                end, paths.to-list())
                typ
              | none =>
                typ
            end
        end
    end
  end

  maintained = _maintain-structure(typ, empty, [SD.mutable-string-dict: ])
  existentials = maintained.free-variables()
  temp-solution = constraint-solution(existentials, [string-dict: ])
  temp-solution.generalize(maintained)
end

# resolves the type down to either a t-record, an existential, or a data type
# data type shapes: t-name, t-app(t-name), t-data-refinement(data type shape)
fun instantiate-object-type(typ :: Type, context :: Context) -> FoldResult<Type>:
  shadow typ = resolve-alias(typ, context)
  cases(Type) typ:
    | t-name(_, _, _, _) =>
      fold-result(typ, context)
    | t-app(a-onto, a-args, a-l, inferred) =>
      shadow a-onto = resolve-alias(a-onto, context)
      cases(Type) a-onto:
        | t-name(_, _, _, _) =>
          fold-result(t-app(a-onto, a-args, a-l, inferred), context)
        | t-forall(b-introduces, b-onto, _, _) =>
          if not(a-args.length() == b-introduces.length()):
            fold-errors([list: C.bad-type-instantiation(typ, b-introduces.length())])
          else:
            new-onto = foldr2(lam(new-onto, arg, type-var):
              new-onto.substitute(arg, type-var)
            end, b-onto, a-args, b-introduces)
            fold-result(new-onto, context)
          end
        | t-app(b-onto, b-args, _, _) =>
          instantiate-object-type(b-onto, context).bind(lam(temp-result, shadow context):
            instantiate-object-type(t-app(temp-result, a-args, a-l, inferred))
          end)
        | t-existential(_, exists-l, _) =>
          typing-error([list: C.unable-to-infer(exists-l)])
        | else =>
          fold-errors([list: C.incorrect-type(tostring(a-onto), a-onto.l, "a polymorphic type", a-l)])
      end
    | t-record(_, _, _) =>
      fold-result(typ, context)
    | t-data-refinement(data-type, variant-name, a-l, inferred) =>
      instantiate-object-type(data-type, context).bind(lam(temp-result, shadow context):
        fold-result(t-data-refinement(temp-result, variant-name, a-l, inferred), context)
      end)
    | t-existential(_, _, _) =>
      fold-result(typ, context)
    | t-forall(_, _, _, _) =>
      instantiate-forall(typ, context).bind(lam(shadow typ, shadow context):
        instantiate-object-type(typ, context)
      end)
    | else =>
      fold-errors([list: C.incorrect-type(tostring(typ), typ.l, "an object type", typ.l)])
  end
end

fun instantiate-forall(typ :: Type, context :: Context) -> FoldResult<Type>:
  cases(Type) typ:
    | t-forall(introduces, onto, l, _) =>
      instantiate-forall(onto, context).bind(lam(shadow onto, shadow context):
        new-existentials = introduces.map(lam(a-var): new-existential(a-var.l, false) end)
        shadow onto = foldr2(lam(shadow onto, a-var, a-exists):
          onto.substitute(a-exists, a-var)
        end, onto, introduces, new-existentials)
        shadow context = context.add-variable-set(list-to-tree-set(new-existentials))
        fold-result(onto, context)
      end)
    | else =>
      fold-result(typ, context)
  end
end

fun introduce-onto(app-type :: Type%(is-t-app), context :: Context) -> FoldResult<Type>:
  args = app-type.args
  onto = app-type.onto
  shadow onto = resolve-alias(onto, context)
  cases(Type) onto:
    | t-forall(a-introduces, a-onto, a-l, _) =>
      if not(args.length() == a-introduces.length()):
        fold-errors([list: C.bad-type-instantiation(app-type, a-introduces.length())])
      else:
        new-onto = foldr2(lam(new-onto, arg, type-var):
          new-onto.substitute(arg, type-var)
        end, a-onto, args, a-introduces)
        fold-result(new-onto, context)
      end
    | t-app(a-onto, a-args, a-l) =>
      introduce-onto(onto, context).bind(lam(new-onto, shadow context):
        introduce-onto(t-app(new-onto, args, a-l, app-type.inferred), context)
      end)
    | else =>
      fold-errors([list: C.bad-type-instantiation(app-type, 0)])
  end
end

fun instantiate-data-type(typ :: Type, context :: Context) -> FoldResult<DataType>:
  fun helper(shadow typ, shadow context) -> FoldResult<DataType>:
    cases(Type) typ:
      | t-name(_, _, _, _) =>
        cases(Option<DataType>) context.get-data-type(typ):
          | none =>
            fold-errors([list: C.cant-typecheck("Expected a data type but got " + tostring(typ), typ.l)])
          | some(data-type) =>
            fold-result(data-type, context)
        end
      | t-app(onto, args, _, _) =>
        shadow onto = resolve-alias(onto, context)
        cases(Type) onto:
          | t-name(_, _, _, _) =>
            helper(onto, context).bind(lam(data-type, shadow context):
              if data-type.params.length() == args.length():
                new-data-type = foldr2(lam(new-data-type, arg, type-var):
                  new-data-type.substitute(arg, type-var)
                end, data-type, args, data-type.params)
                fold-result(t-data(new-data-type.name,
                                   [list: ],
                                   new-data-type.variants,
                                   new-data-type.fields,
                                   new-data-type.l),
                            context)
              else:
                fold-errors([list: C.bad-type-instantiation(typ, data-type.params.length())])
              end
            end)
          | else =>
            introduce-onto(typ, context).bind(lam(shadow typ, shadow context):
              instantiate-data-type(typ, context)
            end)
        end
      | t-data-refinement(data-type, variant-name, _, _) =>
        instantiate-data-type(data-type, context).bind(lam(shadow data-type, shadow context):
          variant = data-type.get-variant-value(variant-name)
          new-with-fields = variant.with-fields.fold-keys(lam(key, new-with-fields):
            new-with-fields.set(key, variant.with-fields.get-value(key))
          end, data-type.fields)
          new-fields = variant.fields.foldl(lam({field-name; field-type}, new-fields):
            new-fields.set(field-name, field-type)
          end, new-with-fields)
          new-data-type = t-data(data-type.name, data-type.params, data-type.variants, new-fields, data-type.l)
          fold-result(new-data-type, context)
        end)
      | t-forall(_, _, _, _) =>
        instantiate-forall(typ, context).bind(lam(shadow typ, shadow context):
          instantiate-data-type(typ, context)
        end)
      | t-existential(_, exists-l, _) =>
        fold-errors([list: C.unable-to-infer(exists-l)])
      | else =>
        fold-errors([list: C.cant-typecheck("Expected a data type but got " + tostring(typ), typ.l)])
    end
  end

  helper(typ, context).bind(lam(data-type, shadow context):
    if is-empty(data-type.params):
      fold-result(data-type, context)
    else:
      fold-errors([list: C.cant-typecheck(tostring(typ) + " expected " + tostring(data-type.params.length()) + " type arguments, but received none.", typ.l)])
    end
  end)
end

fun empty-context():
  typing-context(TD.make-default-types(),
                 TD.make-default-aliases(),
                 TD.make-default-data-exprs(),
                 TD.make-default-modules(),
                 SD.make-string-dict(),
                 SD.make-string-dict(),
                 no-constraints,
                 empty-info(),
                 [string-dict: ])
end

fun empty-info():
  tc-info(SD.make-string-dict(),
          SD.make-string-dict(),
          SD.make-string-dict())
end

fun resolve-alias(t :: Type, context :: Context) -> Type:
  cases(Type) t:
    | t-name(a-mod, a-id, l, inferred) =>
      cases(TS.NameOrigin) a-mod:
        | dependency(d) => TS.dep-error(a-mod)
        | local =>
          cases(Option) context.aliases.get(a-id.key()):
            | none => t
            | some(aliased) => resolve-alias(aliased, context).set-loc(l).set-inferred(inferred)
          end
        | module-uri(mod) =>
          if mod == "builtin":
            cases(Option<Type>) context.aliases.get(a-id.key()):
              | none => t
              | some(aliased) => aliased.set-loc(l).set-inferred(inferred)
            end
          else:
            modtyp = context.modules.get-value(mod)
            cases(Option<Type>) modtyp.types.get(a-id.toname()):
              | some(typ) => t
              | none =>
                cases(Option<Type>) modtyp.aliases.get(a-id.toname()):
                  | none => t
                  | some(aliased) => resolve-alias(aliased, context).set-loc(l).set-inferred(inferred)
                end
            end
          end
      end
    | else => t
  end
end

data TypingResult:
  | typing-result(ast :: A.Expr, typ :: Type, out-context :: Context) with:
    method bind(self, f :: (A.Expr, Type, Context -> TypingResult)) -> TypingResult:
      f(self.ast, self.typ, self.out-context)
    end,
    method fold-bind<V>(self, f :: (A.Expr, Type, Context -> FoldResult<V>)) -> FoldResult<V>:
      f(self.ast, self.typ, self.out-context)
    end,
    method map-expr(self, f :: (A.Expr -> A.Expr)) -> TypingResult:
      typing-result(f(self.ast), self.typ, self.out-context)
    end,
    method map-type(self, f :: (Type -> Type)) -> TypingResult:
      typing-result(self.ast, f(self.typ), self.out-context)
    end,
    method solve-bind(self) -> TypingResult:
      self.out-context.solve-level().typing-bind(lam(solution, context):
        shadow context = context.substitute-in-binds(solution)

        shadow context = context.substitute-in-misc(solution)

        typing-result(self.ast, solution.apply(self.typ), context)
      end)
    end
  | typing-error(errors :: List<C.CompileError>) with:
    method bind(self, f :: (A.Expr, Type, Context -> TypingResult)) -> TypingResult:
      self
    end,
    method fold-bind<V>(self, f :: (A.Expr, Type, Context -> FoldResult<V>)) -> FoldResult<V>:
      fold-errors(self.errors)
    end,
    method map-expr(self, f :: (A.Expr -> A.Expr)) -> TypingResult:
      self
    end,
    method map-type(self, f :: (Type -> Type)) -> TypingResult:
      self
    end,
    method solve-bind(self) -> TypingResult:
      self
    end
end

data FoldResult<V>:
  | fold-result(v :: V, context :: Context) with:
    method bind<Z>(self, f :: (V, Context -> FoldResult<Z>)) -> FoldResult<Z>:
      f(self.v, self.context)
    end,
    method typing-bind(self, f :: (V, Context -> TypingResult)) -> TypingResult:
      f(self.v, self.context)
    end
  | fold-errors(errors :: List<C.CompileError>) with:
    method bind<Z>(self, f :: (V, Context -> FoldResult<Z>)) -> FoldResult<Z>:
      fold-errors(self.errors)
    end,
    method typing-bind(self, f :: (V, Context -> TypingResult)) -> TypingResult:
      typing-error(self.errors)
    end
end

data Typed:
  | typed(ast :: A.Program, info :: TCInfo)
end

fun map-fold-result<X, Y>(f :: (X, Context -> FoldResult<Y>), lst :: List<X>, context :: Context) -> FoldResult<List<Y>>:
  cases(List<X>) lst:
    | empty => fold-result(empty, context)
    | link(first, rest) =>
      f(first, context).bind(lam(result, rest-context):
        map-fold-result(f, rest, rest-context).bind(lam(rest-results, out-context):
          fold-result(link(result, rest-results), out-context)
        end)
      end)
  end
end

fun foldr-fold-result<X, Y>(f :: (X, Context, Y -> FoldResult<Y>), lst :: List<X>, context :: Context, base :: Y) -> FoldResult<Y>:
  cases(List<X>) lst:
    | empty => fold-result(base, context)
    | link(first, rest) =>
      foldr-fold-result(f, rest, context, base).bind(lam(rest-result, rest-context):
        f(first, rest-context, rest-result)
      end)
  end
end

fun fold-typing<X>(f :: (X, Context -> TypingResult), lst :: List<X>, context :: Context) -> FoldResult<List<A.Expr>>:
  cases(List<X>) lst:
    | empty => fold-result(empty, context)
    | link(first, rest) =>
      cases(TypingResult) f(first, context):
        | typing-error(errors) => fold-errors(errors)
        | typing-result(ast, typ, out-context) =>
          fold-typing(f, rest, out-context).bind(lam(exprs, rest-context):
            fold-result(link(ast, exprs), rest-context)
          end)
      end
  end
end

fun misc-test-inference(fun-examples :: List<Type>, fun-name :: String) -> Nothing:
    shadow fun-examples = fun-examples.map(lam(example): remove-refinements-and-foralls(example) end)  
    cases(List<Type>) fun-examples block:
      | link(first, rest) =>
        generalized = rest.foldr(generalize-type, first)
        first-structure = find-structure(first)
        common-structure = rest.foldr(find-common-structure, first-structure)
        new-type = maintain-common-structure(common-structure, generalized)

        log-payload = "{"
          + "'function-name': " + "'" + fun-name + "'" + ","
          + "'inferred-type': " + "'" + tostring(new-type) + "'" + ","
          + "}"
        LOG.log("extra-test-inferred-type", log-payload)
      | empty => nothing
    end
end
