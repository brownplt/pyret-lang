import * as bfsSetup from './browserfs-setup';
import load from './runtime-loader';
import * as runner from './runner';
import * as backend from './backend';
import * as path from './path';

const runtimeFiles = require('./runtime-files.json');

export { backend, path, bfsSetup };

export const worker = new Worker(path.pyretJarr);

export const installFileSystem = () => {
  bfsSetup.install();
  bfsSetup.configure(worker, path.compileBase);
};

export const loadBuiltins = (): void => {
  load(bfsSetup.fs, path.compileBuiltinJS, path.uncompiled, runtimeFiles);
};

export const { runProgram } = backend;
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

export const lint = (
  programText: string,
  programName: string,
): void => {
  backend.lintProgram(
    worker,
    {
      program: programText,
      programSource: programName,
    },
  );
};

export const compile = (
  baseDirectory: string,
  programFileName: string,
  typeCheck: boolean,
): void => {
  backend.compileProgram(
    worker,
    {
      program: programFileName,
      baseDir: baseDirectory,
      builtinJSDir: path.compileBuiltinJS,
      checks: 'none',
      typeCheck,
      recompileBuiltins: false,
    },
  );
};

export const run = (
  baseDirectory: string,
  programFileName: string,
  callback: (result: any) => void,
  runnerCallback: (runner: any) => void,
  runKind: backend.RunKind,
): void => {
  backend.runProgram2(
    runner,
    baseDirectory,
    programFileName,
    runKind,
  )
    .then((receivedRunner: any): void => {
      // the "runner" here is only a runner if RunKind is equal to Async
      if (runKind === backend.RunKind.Async) {
        runnerCallback(receivedRunner);
      }
      try {
        if (runKind === backend.RunKind.Async) {
          receivedRunner.run(callback);
        }
      } catch (x) {
        console.error(x);
        callback({
          result: { error: String(x.value) },
        });
      }
    });
};

export const createRepl = () => {
  backend.createRepl(worker);
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
  worker.onmessage = backend.makeBackendMessageHandler(
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
  );
};

export const openOrCreateFile = (filePath: string): string => {
  if (bfsSetup.fs.existsSync(filePath)) {
    return bfsSetup.fs.readFileSync(filePath, 'utf-8');
  }
  bfsSetup.fs.writeFileSync(filePath, '');
  return '';
};
