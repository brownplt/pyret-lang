({
  requires: [
    { "import-type": "builtin", name: "parse-pyret" }
  ],
  nativeRequires: [],
  provides: {},

  theModule: function(RUNTIME, NAMESPACE, uri, parser) {
    
    // Plug in parser error passes here
    function surfaceParse(data, filename) {
      // NOTE: relies on internal module representation. Is there a better way to do this?
      var parseFunction = parser.dict["defined-values"]["surface-parse"];

      return parseFunction.app(data, filename);
    }

    return RUNTIME.makeModuleReturn({
          'surface-parse': RUNTIME.makeFunction(surfaceParse, "surfaceParse"),
        }, {});
  }
})
