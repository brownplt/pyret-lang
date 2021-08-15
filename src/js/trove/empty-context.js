({
  provides: {},
  requires: [
    { "import-type": "builtin", name: "global" },
    { "import-type": "builtin", name: "option" },
    { "import-type": "builtin", name: "lists" },
    { "import-type": "builtin", name: "checker" },
    { "import-type": "builtin", name: "error-display" },
    { "import-type": "builtin", name: "error" },
    { "import-type": "builtin", name: "equality" },
    { "import-type": "builtin", name: "valueskeleton" }
  ],
  nativeRequires: [],
  theModule: function(runtime) {
    return runtime.makeModuleReturn({}, {}, {});
  }
})