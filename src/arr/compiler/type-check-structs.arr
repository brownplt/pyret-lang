provide *
provide-types *

import ast as A
import string-dict as SD
import equality as EQ
import valueskeleton as VS
import "compiler/type-structs.arr" as TS
import "compiler/type-defaults.arr" as TD
import "compiler/compile-structs.arr" as C
import "compiler/list-aux.arr" as LA

fun bind(f, a): a.bind(f) end
fun typing-bind(f, a): a.typing-bind(f) end
fun fold-bind(f, a): a.fold-bind(f) end

type Type = TS.Type
type ModuleType = TS.ModuleType
type Pair = TS.Pair

pair = TS.pair

type Bindings = SD.StringDict<Type>
empty-bindings :: Bindings = SD.make-string-dict()

# local context used during type-checking
data Context:
  | typing-context(types :: SD.MutableStringDict<Type>,
                   aliases :: SD.MutableStringDict<Type>,
                   data-types :: SD.MutableStringDict<Type>,
                   modules :: SD.MutableStringDict<ModuleType>,
                   module-names :: SD.MutableStringDict<String>,
                   binds :: Bindings,
                   existentials :: SD.StringDict<Pair<Type, Type>>, # existential, assigned-type
                   info :: TCInfo)
sharing:
  _output(self):
    VS.vs-seq([list: VS.vs-str("{ BINDS: "), VS.vs-value(self.binds), VS.vs-str(" EXISTS: "), VS.vs-value(self.existentials), VS.vs-str(" }")])
  end,
  get-data-type(self, typ :: Type) -> Option<Type>:
    shadow typ = resolve-alias(typ, self)
    cases(Type) typ:
      | t-name(module-name, name, _) =>
        cases(Option<String>) module-name:
          | some(mod) =>
            cases(Option<ModuleType>) self.modules.get-now(mod):
              | some(t-mod) =>
                cases(Option<Type>) t-mod.types.get(name.toname()):
                  | some(shadow typ) => some(typ)
                  | none =>
                    raise("No type " + torepr(typ) + " available on '" + torepr(t-mod) + "'")
                end
              | none =>
                if mod == "builtin":
                  self.data-types.get-now(name.toname())
                else:
                  raise("No module available with the name '" + mod + "'")
                end
            end
          | none =>
            id-key = name.key()
            self.data-types.get-now(id-key)
        end
      | t-app(base-typ, args, l) =>
        base-data-typ = self.get-data-type(base-typ)
        base-data-typ.and-then(_.introduce(args))
      | else => none
    end
  end,
  apply(self, typ :: Type) -> Type:
    keys = self.existentials.keys()
    keys.fold(lam(current-type, key):
      existential-pair = self.existentials.get-value(key)
      existential = existential-pair.left
      assigned-type = existential-pair.right
      current-type.substitute(assigned-type, existential)
    end, typ)
  end,
  assign-existential(self, existential :: Type, assigned-type :: Type) -> Context:
    if self.existentials.has-key(existential.key()):
      self
    else:
      existential-keys = self.existentials.keys()
      new-existentials = existential-keys.fold(lam(existentials, key):
        existential-pair = existentials.get-value(key)
        existential-value = existential-pair.right
        existentials.set(key, pair(existential-pair.left, existential-value.substitute(assigned-type, existential)))
      end, self.existentials)
      updated-existentials = new-existentials.set(existential.key(), pair(existential, assigned-type))
      binds-keys = self.binds.keys()
      new-binds = binds-keys.fold(lam(binds, key):
        bound-value = binds.get-value(key)
        binds.set(key, bound-value.substitute(assigned-type, existential))
      end, self.binds)
      typing-context(self.types, self.aliases, self.data-types, self.modules, self.module-names, new-binds, updated-existentials, self.info)
    end
  end,
  add-binding(self, term-key :: String, assigned-type :: Type) -> Context:
    typing-context(self.types, self.aliases, self.data-types, self.modules, self.module-names, self.binds.set(term-key, assigned-type), self.existentials, self.info)
  end,
  add-dict-to-bindings(self, dict :: SD.StringDict<Type>):
    new-binds = dict.keys().fold(lam(bindings, key):
      bindings.set(key, dict.get-value(key))
    end, self.binds)
    typing-context(self.types, self.aliases, self.data-types, self.modules, self.module-names, new-binds, self.existentials, self.info)
  end,
  set-info(self, info :: TCInfo) -> Context:
    typing-context(self.types, self.aliases, self.data-types, self.modules, self.module-names, self.binds, self.existentials, info)
  end
end

# "exported" context after type checking
data TCInfo:
  | tc-info(types :: SD.StringDict<Type>,
            aliases :: SD.StringDict<Type>,
            data-types :: SD.StringDict<Type>)
end

data TypingResult:
  | typing-result(ast :: A.Expr, typ :: Type, out-context :: Context) with:
    bind(self, f :: (A.Expr, Type, Context -> TypingResult)) -> TypingResult:
      f(self.ast, self.typ, self.out-context)
    end,
    fold-bind<V>(self, f :: (A.Expr, Type, Context -> FoldResult<V>)) -> FoldResult<V>:
      f(self.ast, self.typ, self.out-context)
    end,
    map-expr(self, f :: (A.Expr -> A.Expr)) -> TypingResult:
      typing-result(f(self.ast), self.typ, self.out-context)
    end,
    map-type(self, f :: (Type -> Type)) -> TypingResult:
      typing-result(self.ast, f(self.typ), self.out-context)
    end
  | typing-error(errors :: List<C.CompileError>) with:
    bind(self, f :: (A.Expr, Type, Context -> TypingResult)) -> TypingResult:
      self
    end,
    fold-bind<V>(self, f :: (A.Expr, Type, Context -> FoldResult<V>)) -> FoldResult<V>:
      fold-errors(self.errors)
    end,
    map-expr(self, f :: (A.Expr -> A.Expr)) -> TypingResult:
      self
    end,
    map-type(self, f :: (Type -> Type)) -> TypingResult:
      self
    end
end

data FoldResult<V>:
  | fold-result(v :: V) with:
    bind<Z>(self, f :: (V -> FoldResult<Z>)) -> FoldResult<Z>:
      f(self.v)
    end,
    typing-bind(self, f :: (V -> TypingResult)) -> TypingResult:
      f(self.v)
    end
  | fold-errors(errors :: List<C.CompileError>) with:
    bind<Z>(self, f :: (V -> FoldResult<Z>)) -> FoldResult<Z>:
      fold-errors(self.errors)
    end,
    typing-bind(self, f :: (V -> TypingResult)) -> TypingResult:
      typing-error(self.errors)
    end
end

data Typed:
  | typed(ast :: A.Program, info :: TCInfo)
end

fun empty-tc-info() -> TCInfo:
  tc-info(SD.make-string-dict(), SD.make-string-dict(), SD.make-string-dict())
end

fun empty-context() -> Context:
  typing-context(TD.make-default-types(), TD.make-default-aliases(), TD.make-default-data-exprs(), TD.make-default-modules(), SD.make-mutable-string-dict(), empty-bindings, SD.make-string-dict(), empty-tc-info())
end

fun resolve-alias(t :: Type, context :: Context) -> Type:
  cases(Type) t:
    | t-name(a-mod, a-id, l) =>
      cases(Option<String>) a-mod:
        | none =>
          cases(Option<Type>) context.aliases.get-now(a-id.key()):
            | none => t
            | some(aliased) => resolve-alias(aliased, context).set-loc(l)
          end
        | some(mod) =>
          if mod == "builtin":
            cases(Option<Type>) context.aliases.get-now(a-id.key()):
              | none => t
              | some(aliased) => aliased.set-loc(l)
            end
          else:
            # TODO(MATT): make sure that a-id.key() is correct here
            cases(Option<Type>) context.modules.get-value-now(mod).aliases.get(a-id.key()):
              | none => t
              | some(aliased) => resolve-alias(aliased, context).set-loc(l)
            end
          end
      end
    | else => t
  end
end

fun map-fold-result<X, Y>(f :: (X -> FoldResult<Y>), lst :: List<X>) -> FoldResult<List<Y>>:
  cases(List<X>) lst:
    | empty => fold-result(empty)
    | link(first, rest) =>
      initial-result = f(first)
      for bind(result from initial-result):
        for bind(rest-results from map-fold-result(f, rest)):
          fold-result(link(result, rest-results))
        end
      end
  end
end

fun foldr-fold-result<X, Y>(f :: (X, Y -> FoldResult<Y>), lst :: List<X>, base :: Y) -> FoldResult<Y>:
  cases(List<X>) lst:
    | empty => fold-result(base)
    | link(first, rest) =>
      rest-result = foldr-fold-result(f, rest, base)
      for bind(result from rest-result):
        f(first, result)
      end
  end
end

# TODO(MATT): determine if it's okay that this is a foldl
fun fold-typing<X>(f :: (X, Context -> TypingResult), lst :: List<X>, context :: Context) -> FoldResult<Pair<Context, List<A.Expr>>>:
  cases(List<X>) lst:
    | empty => fold-result(pair(context, empty))
    | link(first, rest) =>
      cases(TypingResult) f(first, context):
        | typing-error(errors) => fold-errors(errors)
        | typing-result(ast, typ, out-context) =>
          fold-typing(f, rest, out-context).bind(lam(context-and-exprs):
            fold-result(pair(context-and-exprs.left, link(ast, context-and-exprs.right)))
          end)
      end
  end
end
