/*
type Locator = {
 
  # Could either have needs-provide be implicitly stateful, and cache
  # the most recent map, or use explicit interface below
  needs-compile :: (SD.MutableStringDict<Provides> -> Boolean),

  get-module :: ( -> PyretCode),
  get-dependencies :: ( -> Set<CS.Dependency>),
  get-provides :: ( -> Provides),
  get-compile-env :: ( -> CS.CompileEnv),
  get-namespace :: (R.Runtime -> N.Namespace),

  uri :: (-> URI),
  name :: (-> String),

  # Note that CompileResults can contain both errors and successful
  # compilations
  set-compiled :: (CS.CompileResult<JSP.CompiledCodePrinter>, SD.MutableStringDict<Provides> -> Nothing),
  get-compiled :: ( -> Option<CS.CompileResult<JSP.CompiledCodePrinter>>),

  # _equals should compare uris for locators
  _equals :: Method

*/
define([], function() {
  return function(runtime, ns) {
    console.log("Loading bim");
    var F = runtime.makeFunction;
    function getBuiltinLocator(name) {
      console.log("Builtin: ", name);
      runtime.pauseStack(function(restarter) {
        // NOTE(joe): This is a bit of requireJS hackery that assumes a
        // certain layout for builtin modules
        require(["trove/" + name], function(m) {
          console.log("Module is: ", m);
          restarter.resume(runtime.makeObject({
            "get-raw-dependencies":
              F(function() {
                return m.dependencies;
              }),
            "get-raw-provides":
              F(function() {
                return m.provides;
              }),
            "get-raw-compiled":
              F(function() {
                return runtime.makeOpaque(m.theModule);
              })
          }));
        });

      });
    }
    var O = runtime.makeObject;
    return O({
      "provide-plus-types": O({
        types: { },
        values: O({
          "builtin-raw-locator": runtime.makeFunction(getBuiltinLocator)
        })
      }),
      "answer": runtime.nothing
    });
  };
});
