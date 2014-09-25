
type ToCompile = { locator: Locator, provide-map: StringDict<Provides>, path :: List<Locator> }

fun<CompileContext> make-compile-lib(find):

  fun compile-worklist(locator :: Locator, context :: CompileContext) -> List<ToCompile>

  end

  fun compile-program(worklist :: List<ToCompile>) -> List<Compiled>:

  end

  fun compile-module(locator :: Locator, dependencies :: StringDict<Provides>) -> CompileResult:

  end

end

