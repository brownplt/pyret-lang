({
  requires: [ ],
  provides: {
    shorthands: {
        "Either": { tag: "name",
            origin: { "import-type": "uri", uri: "builtin://either" },
            name: "Either" },
    },
    values: {
        "fetch": ["arrow", ["String"], ["tyapp", "Either", ["String", "String"]]]
    },
    types: {}
  },
  nativeRequires: ["cross-fetch"],
  theModule: function(RUNTIME, NAMESPACE, uri, fetchLib) {
    const fetch = fetchLib;
    return RUNTIME.makeModuleReturn({
        "fetch": RUNTIME.makeFunction(function(url) {
            RUNTIME.ffi.checkArity(1, arguments, "fetch", false);
            RUNTIME.checkString(url);
            return RUNTIME.pauseStack(async restarter => {
                const result = await fetch(url);
                if(result.ok) {
                    const text = await result.text();
                    restarter.resume(RUNTIME.ffi.makeLeft(text));
                }
                else {
                    const err = await result.statusText;
                    const message = `Fetching ${url} failed with status ${result.status}: ${err}`;
                    restarter.error(RUNTIME.ffi.makeRight(message)); 
                }
            });
        })
    }, {});
  }
})