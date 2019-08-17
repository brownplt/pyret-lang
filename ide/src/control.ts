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

export const deleteDir = (dir: string): void => {
  bfsSetup.fs.readdir(dir, function(err: any, files: any) {
    if (err) {
      throw err;
    }

    let count = files.length;
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
  deleteDir("/");
};
