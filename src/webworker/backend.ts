
export interface LintOptions {
  program: string,
  programSource: string,
}
export interface CompileOptions {
  program: string,
  baseDir: string,
  builtinJSDir: string,
  typeCheck: boolean,
  checks: string,
  recompileBuiltins: boolean,
}

export enum RunKind {
  Sync = "SYNC",
  Async = "ASYNC",
}

export interface RunResult {
  time: number,
  result: any,
}

let compileStart = window.performance.now();

/*
 * Handles Pyret compiler messages ONLY.
 * Ignores all other messages (including BrowserFS messages)
 */
export const makeBackendMessageHandler = (
  echoLog: (l: string) => void,
  setupFinished: () => void,
  compileFailure: (e: string[]) => void,
  runtimeFailure: (e: string[]) => void,
  lintFailure: (data: { name: string, errors: string[]}) => void,
  lintSuccess: (data: { name: string }) => void,
  compileSuccess: () => void): ((e: MessageEvent) => null | void) => {
  const backendMessageHandler = (e: MessageEvent) => {
    if (e.data.browserfsMessage === true) {
      return null;
    }

    try {
      var msgObject: any = JSON.parse(e.data);

      var msgType = msgObject["type"];
      if (msgType === undefined) {
        return null;
      } else if (msgType === "echo-log") {
        echoLog(msgObject.contents);
      } else if (msgType === "lint-failure") {
        lintFailure(msgObject.data);
      } else if (msgType === "lint-success") {
        lintSuccess(msgObject.data);
      } else if (msgType === "setup-finished") {
        setupFinished();
      } else if (msgType === "compile-failure") {
        compileFailure(msgObject.data);
      } else if (msgType === "compile-success") {
        console.log("compile-time: ", window.performance.now() - compileStart);
        compileSuccess();
      } else {
        return null;
      }

    } catch(e) {
      console.log(e);
      runtimeFailure(e);
      return null;
    }
  };

  return backendMessageHandler;
};

export const lintProgram = (
  compilerWorker: Worker,
  options: LintOptions): void => {
  const message = {
    _parley: true,
    options: {
      program: options.program,
      "program-source": options.programSource,
      "lint": true
    }
  };

  compilerWorker.postMessage(message);
};

export const compileProgram = (
  compilerWorker: Worker,
  options: CompileOptions): void => {
  compileStart = window.performance.now();
  const message = {
    _parley: true,
    options: {
      program: options.program,
      "base-dir": options.baseDir,
      "builtin-js-dir": options.builtinJSDir,
      checks: options.checks,
      'type-check': options.typeCheck,
      'recompile-builtins': options.recompileBuiltins,
    }
  };

  compilerWorker.postMessage(message);
};

const assertNever = (_arg: never): never => {
  throw new Error('assertNever');
};

export const runProgram = (
  runner: any,
  baseDir: string,
  program: string,
  runKind: RunKind): Promise<RunResult> => {
  if (runKind === RunKind.Sync) {
    const start = window.performance.now();
    const result = runner.makeRequire(baseDir)(program);
    const end = window.performance.now();

    return Promise.resolve({
      time: end - start,
      result: result
    });
  } else if (runKind === RunKind.Async) {
    const entry = runner.makeRequireAsync(baseDir);
    const resultP = entry(program);

    let wrapper = async function() {
      const start = window.performance.now();
      let result = await resultP;
      const end = window.performance.now();

      return {
        time: end - start,
        result: result,
      };
    };

    return wrapper();
  } else {
    // NOTE(michael): type checking in Typescript on enums is not exhaustive (as of v3.5.3)
    return assertNever(runKind);
  }
};
