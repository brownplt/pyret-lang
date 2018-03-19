provide *
provide-types *

include string-dict

include file("ds-structs.arr")
include file("ds-parse.arr")

################################################################################
#  Environments
#

data Env:
  | environment(
      pvar-map :: StringDict<Term>,
      fresh-map :: StringDict<Variable>,
      ellipsis-map :: StringDict<List<Env>>)
end

fun empty-env() -> Env:
  environment([string-dict: ], [string-dict: ], [string-dict: ])
end

fun get-pvar(env :: Env, pvar :: String) -> Option<Term>:
  env.pvar-map.get(pvar)
end

fun get-fresh(env :: Env, pvar :: String) -> Option<Variable>:
  env.fresh-map.get(pvar)
end

fun get-ellipsis(env :: Env, label :: String) -> Option<List<Env>>:
  env.ellipsis-map.get(label)
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
