const bfsSetup = require('./browserfs-setup.ts');
const runtimeFiles = require('./runtime-files.json');
const runtimeLoader = require('./runtime-loader.ts');
export const runner = require('./runner.ts');
export const backend = require('./backend.ts');

export const worker = new Worker('pyret.jarr');

bfsSetup.install();
bfsSetup.configure(worker, './projects');

export const loadBuiltins = (): void => {
  runtimeLoader.load(bfsSetup.fs, '/prewritten', '/uncompiled', runtimeFiles);
};

loadBuiltins();

export const runProgram = backend.runProgram;
export const compileProgram = backend.compileProgram;
export const fs = bfsSetup.fs;

