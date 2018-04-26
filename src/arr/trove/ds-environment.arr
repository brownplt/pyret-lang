provide *
provide-types *

import global as _
include string-dict
include lists
include option

include ds-structs
include ds-parse
include debugging

################################################################################
#  Environments
#

fun empty-env() -> Env:
  environment([string-dict: ], [string-dict: ], [string-dict: ])
end

fun get-pvar(env :: Env, pvar :: String) -> Option<Term>:
  env.pvar-map.get(pvar)
end

fun get-fresh(env :: Env, pvar :: String) -> Option<Variable>:
  env.fresh-map.get(pvar)
end

fun unify<a>(dict1 :: StringDict<a>, dict2 :: StringDict<a>, label :: String) -> StringDict<a>:
  for fold-keys(acc from dict1, key from dict2):
    val = dict2.get-value(key)
    cases (Option) acc.get(key) block:
      | none => acc.set(key, val)
      | some(val2) =>
        when val <> val2:
          fail("Fail to unify environment: " + tostring(dict1) + " and " + tostring(dict2) + " due to " + tostring(val) + " and " + tostring(val2) + " on splitting label " + label)
        end
        acc
    end
  end
end

fun unify-list(dict1 :: StringDict<List<Env>>, dict2 :: StringDict<List<Env>>, label :: String) -> StringDict<List<Env>>:
  for fold-keys(acc from dict1, key from dict2):
    val = dict2.get-value(key)
    cases (Option) acc.get(key) block:
      | none => acc.set(key, val)
      | some(val2) =>
        when val.length() <> val2.length():
          fail("Fail to unify environment: " + tostring(dict1) + " and " + tostring(dict2) + " due to " + tostring(val) + " and " + tostring(val2) + " having different length on splitting label " + label)
        end
        acc.set(key, map2(unify-env(_, _, label), val, val2))
    end
  end
end

fun unify-env(env :: Env, env-new :: Env, label :: String) -> Env:
  pvar-map = unify(env.pvar-map, env-new.pvar-map, label)
  fresh-map = unify(env.fresh-map, env-new.fresh-map, label)
  ellipsis-map = unify-list(env.ellipsis-map, env-new.ellipsis-map, label)
  environment(pvar-map, fresh-map, ellipsis-map)
end

fun get-ellipsis(env :: Env, label :: String) -> Option<List<Env>> block:
  cases (Option) env.ellipsis-map.get(label):
    | none => none
    | some(envs) =>
      for map(env-new from envs):
        unify-env(
          environment(env.pvar-map, env.fresh-map, env.ellipsis-map.remove(label)),
          env-new,
          label)
      end ^ some
  end
end

fun set-pvar(env :: Env, pvar :: String, e :: Term) -> Env:
  environment(
    env.pvar-map.set(pvar, e),
    env.fresh-map,
    env.ellipsis-map)
end

fun set-fresh(env :: Env, pv :: String, ev :: Variable) -> Env:
  environment(
    env.pvar-map,
    env.fresh-map.set(pv, ev),
    env.ellipsis-map)
end

fun set-ellipsis(env :: Env, label :: String, env-list :: List<Env>)
  -> Env:
  environment(
    env.pvar-map,
    env.fresh-map,
    env.ellipsis-map.set(label, env-list))
end

fun assign-new-names(env :: Env, items :: List<FreshItem>, fresh :: Boolean) -> Env block:
  fold(assign-new-name(_, _, fresh), env, items)
end

fun assign-new-name(env :: Env, item :: FreshItem, fresh :: Boolean) -> Env block:
  cases (FreshItem) item:
    | fresh-name(name) =>
      new-name = if fresh: gensym("_") else: name end
      set-fresh(env, name, naked-var(new-name)) # TODO: make atom
    | fresh-ellipsis(shadow item, label) =>
      cases (Option) env.ellipsis-map.get(label):
        | none => fail("Ellipsis label " + label + " not found.")
        | some(envs) =>
          shadow envs = map(assign-new-name(_, item, fresh), envs)
          set-ellipsis(env, label, envs)
      end
  end
end
