/*
  Browser entry point for the TS compiler.

  Bundling this file (e.g. with browserify --standalone PyretTSCompiler)
  yields the full compile pipeline plus the repl library, with no live
  node dependencies:
    - sha256 is pure TS (src/sha256.ts)
    - the tokenizer/parser/js-numbers/type-util/jglr interop modules are
      loaded through interop/amd's registerModuleSource hook; the host
      bundle must register their sources before first parse (see
      code.pyret.org's ts-compiler entry generator)
    - fs/path/vm imports in transitively-included modules are never
      called on browser code paths (e.g. js-of-pyret's CCPFile)

  The node CLI layer (pyret.ts, cmdline.ts, cli-module-loader.ts,
  server.ts, locators/*) is intentionally NOT exported here.
*/

import * as repl from './repl';
import * as compileLib from './compile-lib';
import * as compileStructs from './compile-structs';
import * as compileErrors from './compile-errors';
import * as errorDisplay from './error-display';
import * as renderErrorDisplay from './render-error-display';
import * as parsePyret from './parse-pyret';
import * as builtinModules from './builtin-modules';
import * as jsOfPyret from './js-of-pyret';
import * as srcloc from './srcloc';
import * as astModule from './ast';
import * as shared from './shared';
import { sha256 } from './sha256';
import {
  registerModuleSource,
  registerModuleValue,
  setModulePath,
  setPyretLangRoot,
  amdRequire
} from './interop/amd';

export {
  repl,
  compileLib,
  compileStructs,
  compileErrors,
  errorDisplay,
  renderErrorDisplay,
  parsePyret,
  builtinModules,
  jsOfPyret,
  srcloc,
  astModule as ast,
  shared,
  sha256
};

export const amd = {
  registerModuleSource,
  registerModuleValue,
  setModulePath,
  setPyretLangRoot,
  amdRequire
};
