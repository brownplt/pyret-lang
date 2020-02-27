
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
  compileSuccess: () => void,
  createReplSuccess: () => void,
  compileInteractionSuccess: (data: { program: string }) => void,
  compileInteractionFailure: (data: { program: string }) => void): ((e: MessageEvent) => null | void) => {
  const backendMessageHandler = (e: MessageEvent) => {
    if (e.data.browserfsMessage === true) {
      return null;
    }

    try {
      var msgObject: any = JSON.parse(e.data);

      var msgType = msgObject["type"];

      if (msgObject.tag === "error") {
        console.error(JSON.parse(msgObject.data));
      }

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
      } else if (msgType === "create-repl-success") {
        createReplSuccess();
      } else if (msgType === "compile-interaction-success") {
        compileInteractionSuccess({program: msgObject.program});
      } else if (msgType === "compile-interaction-failure") {
        compileInteractionFailure({program: msgObject.program});
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
    "request": 'lint-program',
    program: options.program,
    "program-source": options.programSource,
    "lint": true
  };

  compilerWorker.postMessage(message);
};

export const compileProgram = (
  compilerWorker: Worker,
  options: CompileOptions): void => {
  compileStart = window.performance.now();
  const message = {
    "request": 'compile-program',
    program: options.program,
    "base-dir": options.baseDir,
    "builtin-js-dir": options.builtinJSDir,
    checks: options.checks,
    'type-check': options.typeCheck,
    'recompile-builtins': options.recompileBuiltins,
  };

  compilerWorker.postMessage(message);
};

export const createRepl = (
  compilerWorker: Worker): void => {
  const message = {
    "request": "create-repl"
  }

  compilerWorker.postMessage(message);
};

export const compileInteraction = (
  compilerWorker: Worker,
  interactionFullPath: string): void => {
  const message = {
    "request": "compile-interaction",
    "program": interactionFullPath
  };

  compilerWorker.postMessage(message);
}

const assertNever = (_arg: never): never => {
  throw new Error('assertNever');
};

export const runProgram2 = (
  runner: any,
  baseDir: string,
  program: string,
  runKind: RunKind): Promise<any> => {
  if (runKind === RunKind.Sync) {
    const start = window.performance.now();
    const result = runner.makeRequire(baseDir)(program);
    const end = window.performance.now();
    return Promise.resolve({
      time: end - start,
      result: result
    });
  } else if (runKind === RunKind.Async) {
    return new Promise<any>((resolve, _reject) => {
      const startRequire = window.performance.now();
      runner.makeRequireAsync(baseDir)(program).then((asyncRunner: any) => {
        const endRequire = window.performance.now();
        console.log("require time", endRequire - startRequire);
        resolve({
          run: (callback: (result: RunResult) => void): void => {
            const startRun = window.performance.now();
            asyncRunner.run.then((result: any) => {
              const endRun = window.performance.now();
              console.log("run time", endRun - startRun);
              callback({
                time: endRun - startRequire,
                result: result,
              });
            }).catch((result: any) => {
              const endRun = window.performance.now();
              callback({
                time: endRun - startRequire,
                result: {error: String(result.value), result},
              })
            });
          },
          pause: (callback: (line: number) => void): void => {
            asyncRunner.pause(callback);
          },
          resume: (): void => {
            asyncRunner.resume();
          },
        });
      });
    });
  } else {
    return assertNever(runKind);
  }
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

    const wrapper = new Promise<RunResult>((resolve, _reject) => {
      const startRequire = window.performance.now();
      resultP.then((asyncRunner: any) => {
        console.log("asyncRunner", asyncRunner);
        const endRequire = window.performance.now();

        const startRun = window.performance.now();
        asyncRunner.run((result: any) => {
          const endRun = window.performance.now();

          console.log("require time", endRequire - startRequire);
          console.log("run time", endRun - startRun);
          console.log("total time", endRun - startRequire);

          resolve({
            time: endRun - startRequire,
            result: result,
          })
        });
      });
    });

    return wrapper;
  } else {
    // NOTE(michael): type checking in Typescript on enums is not exhaustive (as of v3.5.3)
    return assertNever(runKind);
  }
};
