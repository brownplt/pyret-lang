{
  provides: {},
  requires: [
    { "import-type": "builtin", name: "option" },
  ],
  nativeRequires: [],
  theModule: function(runtime, namespace, uri, option) {
    return {
      option: option
    }
  }
}
