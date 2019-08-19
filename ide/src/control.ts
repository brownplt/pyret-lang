import * as bfsSetup from './browserfs-setup';
import * as runtimeLoader from './runtime-loader';
import * as runner from './runner';
import * as backend from './backend';
import * as path from './path';

const runtimeFiles = require('./runtime-files.json');

export {backend, path};

const worker = new Worker(path.pyretJarr);

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
      "typeCheck": typeCheck
    });
};

export const run = (
  baseDirectory: string,
  programFileName: string,
  callback: (result: any) => void): void => {
  backend.runProgram(
    runner,
    baseDirectory,
    programFileName,
    backend.RunKind.Sync)
    .catch(callback) // TODO use something other than callback here
    .then(callback);
};

export const setupWorkerMessageHandler = (
  onLog: (l: string) => void,
  onError: (e: string) => void,
  onCompileFailure: () => void,
  onCompileSuccess: () => void): void => {
  worker.onmessage = backend.makeBackendMessageHandler(
    onLog,
    onError,
    onCompileFailure,
    onCompileSuccess);
};
