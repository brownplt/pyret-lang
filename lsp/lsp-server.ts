import * as TSAST from '../src/arr/compiler/ts-ast';
import * as lsp from './lsp.js';

const result = (lsp as any).result;

async function createApi() {
  const runtime : any = await result;
  console.log(Object.keys(runtime));
  console.log(Object.keys(runtime.modules));
  console.log(runtime.modules["builtin://parse-pyret"]);
  return runtime;
}

createApi().then((result) => {
  console.log("finished");
}).catch((err) => {
  console.log("err", err);
});
