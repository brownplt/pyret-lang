provide *
provide-types *

include string-dict

include file("ds-structs.arr")
include file("ds-parse.arr")

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

fun unify<a>(dict1 :: StringDict<a>, dict2 :: StringDict<a>) -> StringDict<a>:
  for fold-keys(acc from dict1, key from dict2):
    val = dict2.get-value(key)
    cases (Option) acc.get(key) block:
      | none => acc.set(key, val)
      | some(val2) =>
        when val <> val2:
          fail("Fail to unify environment")
        end
        acc
    end
  end
end

fun get-ellipsis(env :: Env, label :: String) -> Option<List<Env>>:
  cases (Option) env.ellipsis-map.get(label):
    | none => none
    | some(envs) =>
      for map(env-new from envs):
        pvar-map = unify(env.pvar-map, env-new.pvar-map)
        fresh-map = unify(env.fresh-map, env-new.fresh-map)
        ellipsis-map = unify(env.ellipsis-map, env-new.ellipsis-map.remove(label))
        environment(pvar-map, fresh-map, ellipsis-map)
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

fun assign-fresh-names(env :: Env, fresh :: Set<String>) -> Env:
  fresh.fold(lam(shadow env, v):
      set-fresh(env, v, naked-var(gensym("_")))
    end, env)
end
