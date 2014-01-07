#lang pyret

provide *

data CompileEnvironment:
  | compile-env(builtins :: List<A.Program>, builtin-env :: Object)
end

data CompileResult:
  | ok(code :: String)
  | err(message :: String)
end

standard-builtins = [ ]
