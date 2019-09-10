import * as bfsSetup from './browserfs-setup';
import * as runtimeLoader from './runtime-loader';
import * as runner from './runner';
import * as backend from './backend';
import * as path from './path';

const runtimeFiles = require('./runtime-files.json');

export {backend, path, bfsSetup};

export const worker = new Worker(path.pyretJarr);

export const installFileSystem = () => {
  bfsSetup.install();
  bfsSetup.configure(worker, path.compileBase);
};

export const loadBuiltins = (): void => {
  runtimeLoader.load(bfsSetup.fs, path.compileBuiltinJS, path.uncompiled, runtimeFiles);
};

export const runProgram = backend.runProgram;
export const compileProgram = backend.compileProgram;
export const fs = bfsSetup.fs;

export const createFile = (file: string): void => {
  bfsSetup.fs.writeFileSync(file, "");
};

export const createDirectory = (dir: string): void => {
  bfsSetup.fs.mkdirSync(dir);
};

export const removeFile = (path: string): void => {
  bfsSetup.fs.unlinkSync(path);
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
  bfsSetup.fs.readdir(dir, function(err: any, files: any) {
    if (err) {
      throw err;
    }

    files.forEach(function(file: string) {
      let filePath = bfsSetup.path.join(dir, file);

      bfsSetup.fs.stat(filePath, function(err: any, stats: any) {
        if (err) {
          throw err;
        }

        if (stats.isDirectory()) {
          deleteDir(filePath);
        } else {
          bfsSetup.fs.unlink(filePath, function(err: any) {
            if (err) {
              throw err;
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
  programFileName: string,
  programText: string): void => {
  backend.lintProgram(
    worker,
    {
      "program": programFileName,
      "programSource": programText
    });
};


export const compile = (
  baseDirectory: string,
  programFileName: string,
  typeCheck: boolean): void => {
  backend.compileProgram(
    worker,
    {
      "program": programFileName,
      "baseDir": baseDirectory,
      "builtinJSDir": path.compileBuiltinJS,
      "checks": "none",
      "typeCheck": typeCheck,
      "recompileBuiltins": false
    });
};

export const run = (
  baseDirectory: string,
  programFileName: string,
  callback: (result: any) => void,
  runKind: backend.RunKind): void => {
  backend.runProgram(
    runner,
    baseDirectory,
    programFileName,
    runKind)
    .catch((x) => { console.error(x); return {result: {error: String(x.value)}} })
    .then(callback);
};

export const setupWorkerMessageHandler = (
  onLog: (l: string) => void,
  onCompileFailure: (e: string[]) => void,
  onRuntimeFailure: (e: string[]) => void,
  lintFailure: (data: { name: string, errors: string[]}) => void,
  lintSuccess: (data: { name: string }) => void,
  onCompileSuccess: () => void): void => {
  worker.onmessage = backend.makeBackendMessageHandler(
    onLog,
    onCompileFailure,
    onRuntimeFailure,
    lintFailure,
    lintSuccess,
    onCompileSuccess);
};

export const openOrCreateFile = (path: string): string => {
  if (bfsSetup.fs.existsSync(path)) {
    return bfsSetup.fs.readFileSync(path, "utf-8");
  } else {
    bfsSetup.fs.writeFileSync(path, "");
    return "";
  }
};
