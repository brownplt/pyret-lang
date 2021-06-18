import type * as TS from './ts-type-structs';
import type * as A from './ts-ast';
import type * as T from './ts-impl-types';
import type * as CS from './ts-compile-structs';
import type * as TJ from './ts-codegen-helpers';
import type * as TCS from './ts-type-check-structs';

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['ast.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-check-structs.arr']},
    { 'import-type': 'dependency', protocol: 'file', args: ['type-defaults.arr']},
 ],
  nativeRequires: ["escodegen", "path"],
  provides: {
    values: {
      "type-check": "tany"
    }
  },
  theModule: function(runtime, _, __, tj : TJ.Exports, TS : (TS.Exports), A : (A.Exports), CS : (CS.Exports), TCS : (TCS.Exports)) {
    const {ok} = CS.dict.values.dict;
    const {typed, ['empty-info']: emptyInfo } = TCS.dict.values.dict;
    function typeCheck(program, compileEnv, postCompileEnv, modules) {
      return ok.app(typed.app(program, emptyInfo.app()));
    }
    return runtime.makeModuleReturn({
      'type-check': runtime.makeFunction(typeCheck)
    }, {});
  }
})