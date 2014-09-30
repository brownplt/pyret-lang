import parse-pyret as P
import ast as A
import sets as S

type ToCompile = { locator: Locator, provide-map: StringDict<Provides>, path :: List<Locator> }

data Dependency:
  | dependency(protocol :: String, arguments :: List<String>)
end

type URI = String

type PyretCode = String

type Locator = {

  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (StringDict<Provides> -> Bool),

  get-module :: ( -> PyretCode),

  # e.g. create a new CompileContext that is at the base of the directory
  # this Locator is in.  The CC holds the current working directory
  update-compile-context :: (CompileContext -> CompileContext),

  uri :: (-> URI),
  name :: (-> String),

  # Note that CompileResults can contain both errors and successful
  # compilations
  set-compiled :: (CompileResult, StringDict<Provides> -> Nothing),
  get-compiled :: ( -> Option<CompileResult>),
}

fun get-dependencies(loc :: Locator) -> Set<Dependency>:
  parsed = P.surface-parse(loc.get-module(), loc.uri())
  special-imports = parsed.imports.map(_.file).filter(A.is-s-special-import)
  dependency-list = for map(s from special-imports):
    dependency(s.kind, s.args)
  end
  S.list-to-list-set(dependency-list)
end

fun get-provides(loc :: Locator) -> Provides:
end

fun<CompileContext> make-compile-lib(find :: (CompileContext, Dependency -> Locator)):

  fun compile-worklist(locator :: Locator, context :: CompileContext) -> List<ToCompile>

  end

  fun compile-program(worklist :: List<ToCompile>) -> List<Compiled>:

  end

  fun compile-module(locator :: Locator, dependencies :: StringDict<Provides>) -> CompileResult:

  end

end

