({
  requires: [
    { "import-type": "builtin", name: "global" },
    { "import-type": "builtin", name: "option" },
    { "import-type": "builtin", name: "either" },
    { "import-type": "builtin", name: "error-display" },
    { "import-type": "builtin", name: "error" },
    { "import-type": "builtin", name: "equality" },
    { "import-type": "builtin", name: "valueskeleton" },
    { "import-type": "builtin", name: "lists" },
    { "import-type": "builtin", name: "arrays" },
    { "import-type": "builtin", name: "pick" },
    { "import-type": "builtin", name: "sets" },
    { "import-type": "builtin", name: "contracts" },
    { "import-type": "builtin", name: "srcloc" },
    { "import-type": "builtin", name: "string-dict" },
    { "import-type": "builtin", name: "render-error-display" },
    { "import-type": "builtin", name: "checker" },
    { "import-type": "builtin", name: "ffi" },
    { "import-type": "builtin", name: "table" },
    { "import-type": "builtin", name: "tables" },
    { "import-type": "builtin", name: "reactors" },
    { "import-type": "builtin", name: "data-source" },
    { "import-type": "builtin", name: "constants" },
    { "import-type": "builtin", name: "s-exp" },
    { "import-type": "builtin", name: "timing" }
  ],
  provides: {},
  nativeRequires: [],
  theModule: function(runtime, namespace, uri /* intentionally blank */) {
    return runtime.makeObject({
      "defined-values": {},
      "defined-types": {},
      "provide-plus-types": runtime.makeObject({
        "values": runtime.makeObject({}),
        "types": {}
      })
    });
  }
})
