import * as TSAST from '../src/arr/compiler/ts-ast';
import * as lsp from './lsp.js';

const runtime = await (lsp as any).result;

console.log(Object.keys(runtime));
console.log(Object.keys(runtime.modules));
console.log(runtime.modules["builtin://parse-pyret"]);
