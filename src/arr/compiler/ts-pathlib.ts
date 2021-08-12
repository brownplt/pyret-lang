import type { PFunction, Runtime, StringDict } from './ts-impl-types';

export type Exports = {
  dict: { 
    values: {
      dict: {
        "normalize": PFunction<(p: string) => string>,
        "join": PFunction<(p1: string, p2: string) => string>,
        "resolve": PFunction<(p: string) => string>,
        "relative": PFunction<(from: string, to: string) => string>,
        "dirname": PFunction<(p: string) => string>,
        "extname": PFunction<(p: string) => string>,
        "basename": PFunction<(p: string, ext: string) => string>,
        "path-sep": string,
        "is-absolute": PFunction<(p: string) => boolean>
      }
    }
  }
}

({
  requires: [],
  nativeRequires: ["path"],
  provides: {
    values: {
      "normalize": ["arrow", ["String"], "String"],
      "join": ["arrow", ["String", "String"], "String"],
      "resolve": ["arrow", ["String"], "String"],
      "relative": ["arrow", ["String", "String"], "String"],
      "dirname": ["arrow", ["String"], "String"],
      "extname": ["arrow", ["String"], "String"],
      "basename": ["arrow", ["String", "String"], "String"],
      "path-sep": "String",
      "is-absolute": ["arrow", ["String"], "Boolean"]
    }
  },
  theModule: function(RUNTIME: Runtime, NAMESPACE, uri, path) {
    const values: Exports['dict']['values']['dict'] = {
      "normalize": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "normalize", false);
        RUNTIME.checkString(p);
        const s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.normalize(s));
      }),
      "join": RUNTIME.makeFunction(function(p1, p2) {
        RUNTIME.ffi.checkArity(2, arguments, "join", false);
        RUNTIME.checkString(p1);
        RUNTIME.checkString(p2);
        const s1 = RUNTIME.unwrap(p1);
        const s2 = RUNTIME.unwrap(p2);
        return RUNTIME.makeString(path.join(s1, s2));
      }),
      "resolve": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "resolve", false);
        RUNTIME.checkString(p);
        const s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.resolve(s));
      }),
      "is-absolute": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "is-absolute", false);
        RUNTIME.checkString(p);
        const s = RUNTIME.unwrap(p);
        return path.isAbsolute(s);
      }),

      "relative": RUNTIME.makeFunction(function(from, to) {
        RUNTIME.ffi.checkArity(2, arguments, "relative", false);
        RUNTIME.checkString(from);
        RUNTIME.checkString(to);
        const sfrom = RUNTIME.unwrap(from);
        const sto = RUNTIME.unwrap(to);
        return RUNTIME.makeString(path.relative(sfrom, sto));
      }),
      "dirname": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "dirname", false);
        RUNTIME.checkString(p);
        const s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.dirname(s));
      }),
      "extname": RUNTIME.makeFunction(function(p) {
        RUNTIME.ffi.checkArity(1, arguments, "extname", false);
        RUNTIME.checkString(p);
        const s = RUNTIME.unwrap(p);
        return RUNTIME.makeString(path.extname(s));
      }),
      "basename": RUNTIME.makeFunction(function(p, ext) {
        RUNTIME.ffi.checkArity(2, arguments, "basename", false);
        RUNTIME.checkString(p);
        RUNTIME.checkString(ext);
        const s = RUNTIME.unwrap(p);
        const sext = RUNTIME.unwrap(ext);
        return RUNTIME.makeString(path.basename(s, sext));
      }),
      "path-sep": RUNTIME.makeString(path.sep),
    };
    return RUNTIME.makeModuleReturn(values, {});
  }
})

