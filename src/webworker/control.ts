import * as bfsSetup from './browserfs-setup';
import load from './runtime-loader';
import * as runner from './runner';
import * as backend from './backend';
import * as path from './path';
import { RuntimeConfig } from './runner';
import { CompileOptions, RunKind, runProgram2 } from './backend';

const runtimeFiles = require('./runtime-files.json');

export { backend, path, bfsSetup };

let resolve : ((w: Worker) => void), reject;
export const worker = new Promise<Worker>((res, rej) => {
  resolve = res;
  reject = rej;
});

function instantiateWorker() : Promise<Worker> {
  resolve(new Worker(path.pyretJarr));
  return worker;
}


export async function installFileSystem() : Promise<void> {
  bfsSetup.install();
  return bfsSetup.configure(instantiateWorker /* , path.compileBase */);
};

export const loadBuiltins = (): void => {
  load(bfsSetup.fs, bfsSetup.path, path.compileBuiltinJS, path.uncompiled, runtimeFiles);
};

export const { compileProgram } = backend;
export const { fs } = bfsSetup;

export const createFile = (file: string): void => {
  bfsSetup.fs.writeFileSync(file, '');
};

export const createDirectory = (dir: string): void => {
  bfsSetup.fs.mkdirSync(dir);
};

export const removeFile = (filePath: string): void => {
  bfsSetup.fs.unlinkSync(filePath);
};

// Synchronous deleteDir
export const removeDirectory = (dir: string): void => {
  const files = bfsSetup.fs.readdirSync(dir);

  files.forEach((file: string) => {
    const filePath = bfsSetup.path.join(dir, file);

    const stats = bfsSetup.fs.statSync(filePath);

    if (stats.isDirectory()) {
      removeDirectory(filePath);
    } else {
      bfsSetup.fs.unlinkSync(filePath);
    }
  });

  bfsSetup.fs.rmdirSync(dir);
};

export const deleteDir = (dir: string): void => {
  bfsSetup.fs.readdir(dir, (err: any, files: any) => {
    if (err) {
      throw err;
    }

    files.forEach((file: string) => {
      const filePath = bfsSetup.path.join(dir, file);

      bfsSetup.fs.stat(filePath, (statErr: any, stats: any) => {
        if (err) {
          throw statErr;
        }

        if (stats.isDirectory()) {
          deleteDir(filePath);
        } else {
          bfsSetup.fs.unlink(filePath, (unlinkErr: any) => {
            if (unlinkErr) {
              throw unlinkErr;
            }
          });
        }
      });
    });
  });
};

export const removeRootDirectory = (): void => {
  deleteDir(path.root);
};

export const lint = async (
  programText: string,
  programName: string,
): Promise<void> => {
  const w = await worker;
  backend.lintProgram(
    w,
    {
      program: programText,
      programSource: programName,
    },
  );
};

export const compile = async (
  baseDirectory: string,
  programFileName: string,
  typeCheck: boolean,
): Promise<void> => {
  const w = await(worker);
  backend.compileProgram(
    w,
    {
      program: programFileName,
      baseDir: baseDirectory,
      builtinJSDir: path.compileBuiltinJS,
      checks: 'none',
      typeCheck,
      recompileBuiltins: false,
      session: 'empty',
    },
  );
};

export const run = (
  baseDirectory: string,
  programFileName: string,
  callback: (result: any) => void,
  runnerCallback: (runner: any) => void,
  runKind: backend.RunKind,
  rtCfg?: RuntimeConfig,
): void => {
  backend.runProgram2(
    runner,
    baseDirectory,
    programFileName,
    runKind,
    rtCfg,
  )
    .then((receivedRunner: any): void => {
      // the "runner" here is only a runner if RunKind is equal to Async
      if (runKind === backend.RunKind.Async) {
        runnerCallback(receivedRunner);
      }
      try {
        if (runKind === backend.RunKind.Async) {
          receivedRunner.run(callback);
        } else if (runKind === backend.RunKind.Sync) {
          callback(receivedRunner);
        }
      } catch (x) {
        console.error(x);
        callback({
          result: { error: String((x as any).value) },
        });
      }
    });
};

export const createRepl = async () => {
  const w = await worker;
  backend.createRepl(w);
};

export const setupWorkerMessageHandler = (
  onLog: (l: string) => void,
  setupFinished: () => void,
  onCompileFailure: (e: string[]) => void,
  onRuntimeFailure: (e: string[]) => void,
  lintFailure: (data: { name: string, errors: string[]}) => void,
  lintSuccess: (data: { name: string }) => void,
  onCompileSuccess: () => void,
  onCreateReplSuccess: () => void,
  onCompileInteractionSuccess: (data: { program: string }) => void,
  onCompileInteractionFailure: (data: { program: string }) => void,
): void => {
  /* worker.addEventListener('message', */backend.makeBackendMessageHandler(
    onLog,
    setupFinished,
    onCompileFailure,
    onRuntimeFailure,
    lintFailure,
    lintSuccess,
    onCompileSuccess,
    onCreateReplSuccess,
    onCompileInteractionSuccess,
    onCompileInteractionFailure,
  )/* ) */;
};

export const openOrCreateFile = (filePath: string): string => {
  if (bfsSetup.fs.existsSync(filePath)) {
    return bfsSetup.fs.readFileSync(filePath, 'utf-8');
  }
  bfsSetup.fs.writeFileSync(filePath, '');
  return '';
};

type CompileResult =
  | 'ok'
  | string[];

type LintResult =
  | {data: { name: string, errors: string[] } }
  | {data: { name: string } };

type ServerAPIEvent =
  | { type: 'compile', action: () => void, resolve: (result : CompileResult) => void}
  | { type: 'lint', action: () => void, resolve: (result : LintResult) => void}
  | { type: 'delete-session', action: () => void, resolve: (a : any) => void, reject: (message : string) => void}
  | { type: 'filter-session', action: () => void, resolve: (a : any) => void, reject: (message : string) => void};

export type CompileAndRunResult =
  | { type: 'compile-failure', errors: string[] }
  | { type: 'run-failure', error: string, errorVal: any }
  | { type: 'run-result', result: any };

export async function makeServerAPI(echoLog : (l : string) => void, setupFinished : () => void) {
  const w = await worker;
  const queue : ServerAPIEvent[] = [];
  let hasInit = false;
  type RunActions = {
    run: (callback: (value: any) => void) => void,
    pause: (callback: (line: number) => void) => void,
    resume: () => void,
    stop: () => Promise<number>
  };
  let runActions: undefined | RunActions;

  function finishAndProcessNext() {
    queue.shift();
    if (queue.length > 0) {
      queue[0].action();
    }
  }

  function addEvent(e : ServerAPIEvent) {
    queue.push(e);
    if (queue.length === 1 && hasInit) { e.action(); }
  }

  function serverAPIMessageHandler(e: MessageEvent) {
    if (e.data.browserfsMessage === true) {
      return null;
    }
    const msgObject: any = JSON.parse(e.data);
    const msgType = msgObject.type;

    if (msgObject.tag === 'error') {
      try {
        console.log('Server reported error!', msgObject.data, JSON.parse(msgObject.data));
      } catch (err) {
        console.log("Server reported error, and it couldn't be parsed as JSON!", msgObject.data);
      }
    }

    const currentEvent = queue[0];

    if (msgType === undefined) {
      return null;
    }
    if (msgType === 'echo-log') {
      echoLog(msgObject.contents);
    } else if (msgType === 'setup-finished') {
      setupFinished();
      hasInit = true;
      if (queue.length > 0) {
        queue[0].action();
      }
    } else if (queue.length === 0) {
      console.log('received with empty queue: ', JSON.stringify(msgObject));
    } else if (msgType === 'success') {
      currentEvent.resolve(true);
      finishAndProcessNext();
    } else if ('reject' in currentEvent && msgType === 'failure') {
      currentEvent.reject(msgObject.message);
      finishAndProcessNext();
    } else if (msgType === 'compile-failure') {
      if (currentEvent.type !== 'compile') {
        throw new Error(`Mismatched event and response ${msgType} ${currentEvent.type}`);
      }
      currentEvent.resolve(msgObject.data);
      finishAndProcessNext();
    } else if (msgType === 'compile-success') {
      if (currentEvent.type !== 'compile') {
        throw new Error(`Mismatched event and response ${msgType} ${currentEvent.type}`);
      }
      currentEvent.resolve('ok');
      finishAndProcessNext();
    } else {
      console.error(msgObject);
      throw new Error(`Unhandled message type: ${msgType}`);
    }
    return null;
  }

  w.addEventListener('message', serverAPIMessageHandler);

  function deleteSession(session : String) : Promise<any> {
    function deleteAction() {
      const message = {
        request: 'session-delete',
        session,
        'session-delete': true,
      };
      w.postMessage(message);
    }
    return new Promise((resolve, reject) => {
      addEvent({
        type: 'delete-session',
        resolve,
        reject,
        action: deleteAction,
      });
    });
  }

  function filterSession(session : String, pattern: string) : Promise<any> {
    function filterAction() {
      const message = {
        request: 'session-filter',
        session,
        'session-filter': pattern,
      };
      w.postMessage(message);
    }
    return new Promise((resolve, reject) => {
      addEvent({
        type: 'filter-session',
        resolve,
        reject,
        action: filterAction,
      });
    });
  }

  function apiCompile(options : CompileOptions) : Promise<CompileResult> {
    function compileAction() {
      const message = {
        request: 'compile-program',
        program: options.program,
        'base-dir': options.baseDir,
        'builtin-js-dir': options.builtinJSDir,
        checks: options.checks,
        'type-check': options.typeCheck,
        'recompile-builtins': options.recompileBuiltins,
        pipeline: 'anchor',
        session: options.session,
      };

      w.postMessage(message);
    }
    return new Promise((resolve) => {
      addEvent({
        type: 'compile',
        resolve,
        action: compileAction,
      });
    });
  }

  function apiRun(
    baseDir : string,
    program : string,
    runKind : RunKind,
    rtCfg?: RuntimeConfig,
  )
    : Promise<any> {
    return new Promise((resolve) => {
      runProgram2(runner, baseDir, program, runKind, rtCfg).then((r: RunActions) => {
        if ('result' in r) {
          resolve(r);
        } else {
          runActions = r;
          console.log(runActions);
          r.run((result) => {
            runActions = undefined;
            resolve(result);
          });
        }
      }).catch((e) => {
        resolve(e);
      });
    });
  }
  // Resolves to whether runner was in fact running before stopped
  function apiStop(): Promise<boolean> {
    return new Promise((resolve) => {
      console.log('in promise of apiStop');
      if (runActions === undefined) {
        console.log('resolve false (not running)');
        resolve(false);
      } else {
        runActions.stop().then(() => {
          runActions = undefined;
          resolve(true);
        });
      }
    });
  }

  async function compileAndRun(

    options: CompileOptions,
    runKind: RunKind,
    rtCfg? : RuntimeConfig,
  ) : Promise<CompileAndRunResult> {
    const compileResult = await apiCompile(options);
    if (compileResult === 'ok') {
      const result = await apiRun(path.runBase, `${options.program}.js`, runKind, rtCfg);
      if ('error' in result.result) {
        return { type: 'run-failure', error: result.result.error, errorVal: result.result.result.value };
      }
      return { type: 'run-result', result };
    }
    return { type: 'compile-failure', errors: compileResult };
  }

  return {
    compile: apiCompile,
    run: apiRun,
    stop: apiStop,
    compileAndRun,
    filterSession,
    deleteSession,
  };
}
