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
    const FETCH_TIMEOUT = 20000;
    const fetch = fetchLib;
    return RUNTIME.makeModuleReturn({
        "fetch": RUNTIME.makeFunction(function(url) {
            RUNTIME.ffi.checkArity(1, arguments, "fetch", false);
            RUNTIME.checkString(url);
            return RUNTIME.pauseStack(async restarter => {
                try {
                    const result = await fetch(url, { signal: AbortSignal.timeout(FETCH_TIMEOUT) });
                    if(result.ok) {
                        const text = await result.text();
                        restarter.resume(RUNTIME.ffi.makeLeft(text));
                    }
                    else {
                        const err = await result.statusText;
                        const message = `Fetching ${url} failed with status ${result.status}: ${err}`;
                        restarter.resume(RUNTIME.ffi.makeRight(message)); 
                    }
                }
                catch(e) {
                    const message = String(e);
                    const error = `Fetch of ${url} failed with an error. This may mean that the server you're fetching from does not support fetch requests from the browser, the URL has a formatting issue, or the request took longer than ${FETCH_TIMEOUT}ms. The system-level error was "${message}"`
                    restarter.resume(RUNTIME.ffi.makeRight(error));
                }
            });
        })
    }, {});
  }
})