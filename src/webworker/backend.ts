export interface CompileOptions {
  program: string,
  baseDir: string,
  builtinJSDir: string,
  typeCheck: boolean,
  checks: string,
}

export enum RunKind {
  Sync = "SYNC",
  Async = "ASYNC",
}

export interface RunResult {
  time: number,
  result: any,
}

/*
 * Handles Pyret compiler messages ONLY.
 * Ignores all other messages (including BrowserFS messages)
 */
export const makeBackendMessageHandler = (
  echoLog: (l: string) => void,
  echoErr: (e: string) => void,
  compileFailure: () => void,
  compileSuccess: () => void): ((e: MessageEvent) => null | void) => {
  const backendMessageHandler = (e: MessageEvent) => {
    if (e.data.browserfsMessage === true) {
      return null;
    }

    try {
      var msgObject = JSON.parse(e.data);

      var msgType = msgObject["type"];
      if (msgType === undefined) {
        return null;
      } else if (msgType === "echo-log") {
        echoLog(msgObject.contents);
      } else if (msgType === "echo-err") {
        echoErr(msgObject.contents);
      } else if (msgType === "compile-failure") {
        compileFailure();
      } else if (msgType === "compile-success") {
        compileSuccess();
      } else {
        return null;
      }

    } catch(e) {
      console.log(e);
      return null;
    }
  };

  return backendMessageHandler;
};

export const compileProgram = (
  compilerWorker: Worker,
  options: CompileOptions): void => {
  const message = {
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

    console.log("resultP:", resultP);
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
