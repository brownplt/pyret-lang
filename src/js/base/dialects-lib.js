define(["js/runtime-util", 
        "js/pyret-tokenizer", "js/pyret-parser", "compiler/compile-structs.arr",
        "js/bootstrap-tokenizer", "js/bootstrap-parser", "js/bootstrap-namespace"], 
function(util, 
         PT, PG, PCS,
         BT, BG, BN) {
  return util.memoModule("dialects-lib", function(RUNTIME, NAMESPACE) {
    function get(obj, fld) { return RUNTIME.getField(obj, fld); }
    var pcs = get(PCS(RUNTIME, NAMESPACE), "provide");
    var dialects = {
      "Pyret": { 
        Tokenizer: PT.Tokenizer, 
        Grammar: PG.PyretGrammar,
        makeNamespace: function(rt) { return rt.namespace; },
        compileEnv: get(pcs, "standard-builtins")
      },
      "pyret": { 
        Tokenizer: PT.Tokenizer, 
        Grammar: PG.PyretGrammar,
        makeNamespace: function(rt) { return rt.namespace; },
        compileEnv: get(pcs, "standard-builtins")
      },
      "Bootstrap": { 
        Tokenizer: BT.Tokenizer, 
        Grammar: BG.BootstrapGrammar, 
        makeNamespace: BN.makeBootstrapNamespace,
        compileEnv: get(pcs, "standard-builtins")
      },
      "bootstrap": { 
        Tokenizer: BT.Tokenizer, 
        Grammar: BG.BootstrapGrammar, 
        makeNamespace: BN.makeBootstrapNamespace,
        compileEnv: get(pcs, "standard-builtins")
      }
    }
    

    return {
      dialects: dialects,
      defaultDialect: "Pyret"
    };
  });
});



