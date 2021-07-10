import type { PFunction, } from './ts-impl-types';

export interface Exports {
  dict: { values: { dict: {
    'reset': PFunction<() => void>,
    'make-name': PFunction<(base: string) => string>,
  }}}
}

({
  requires: [],
  provides: {
    values: {
      'reset': "tany",
      'make-name': "tany"
    }
  },
  nativeRequires: [],
  theModule: function(runtime, _, __) {
    let gensymCounter = 0

    function reset() { 
      gensymCounter = 0;
    }

    function makeName(base: string): string {
      gensymCounter += 1;
      return `base${gensymCounter}`;
    }

    const exports : Exports['dict']['values']['dict'] = {
      'reset': runtime.makeFunction(reset),
      'make-name': runtime.makeFunction(makeName)
    };
    return runtime.makeModuleReturn(exports, {});
  }
})