export interface CompileOptions {
  program: string,
  baseDir: string,
  builtinJSDir: string,
  typeCheck: boolean,
  checks: string,
}

export function compileProgram(compilerWorker, options: CompileOptions) {
  let message = {
    _parley: true,
    options: {
      program: options.program,
      "base-dir": options.baseDir,
      "builtin-js-dir": options.builtinJSDir,
      checks: options.checks,
      'type-check': options.typeCheck,
    }
  };

  compilerWorker.postMessage(message);
}
