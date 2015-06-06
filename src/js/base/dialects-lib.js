define(["js/runtime-util", 
        "js/pyret-tokenizer", "js/pyret-parser", "compiler/compile-structs.arr",
        "js/bootstrap-tokenizer", "js/bootstrap-parser", "js/bootstrap-un-tokenizer", "js/bootstrap-un-parser", "js/bootstrap-namespace"], 
function(util, 
         PT, PG, PCS,
         BT, BG, BUT, BUG, BN) {
  return util.memoModule("dialects-lib", function(RUNTIME, NAMESPACE) {
    function get(obj, fld) { return RUNTIME.getField(obj, fld); }
    return RUNTIME.loadModulesNew(NAMESPACE, [PCS], function(PCSLib) {
      var pcs = get(PCSLib, "values");
      var dialects = {
        "Pyret": { 
          Tokenizer: PT.Tokenizer, 
          Grammar: PG.PyretGrammar,
          UnTokenizer: PT.Tokenizer, 
          UnGrammar: PG.PyretGrammar,
          makeNamespace: function(rt) { return rt.namespace; },
          compileEnv: get(pcs, "standard-builtins")
        }
      }

      return {
        dialects: dialects,
        defaultDialect: "Pyret"
      };
    });
  });
});



