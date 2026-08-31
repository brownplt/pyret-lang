/*
  Ported from: src/arr/compiler/locators/file.arr

  The file-ops record mirrors the mockable interface of the original; the
  real implementation is built on node's fs (the `file` trove's input-file /
  output-file / file-exists / file-times / real-path).
*/

import * as fs from 'fs';
import * as P from 'path';
import * as PP from '../parse-pyret';
import * as CL from '../compile-lib';
import * as CS from '../compile-structs';
import { raise } from '../shared';

export interface FileOps {
  inputFile(path: string): { readFile(): string; closeFile(): void };
  outputFile(path: string): { display(s: string): void; flush(): void; closeFile(): void };
  fileExists(path: string): boolean;
  fileTimes(path: string): { mtime: number; atime: number; ctime: number };
  realPath(path: string): string;
}

export type FileLocator = CL.Locator & { path: string; globals: CS.Globals };

export function mockableFileLocator(fileOps: FileOps): (path: string, globals: CS.Globals) => FileLocator {
  return (path: string, globals: CS.Globals): FileLocator => {
    let ast: CL.PyretCode | undefined = undefined;
    return {
      path: path,
      globals: globals,
      getUncached(): CL.Locator | undefined { return undefined; },
      getModifiedTime(this: any): number {
        return fileOps.fileTimes(path).mtime;
      },
      getOptions(options: CS.CompileOptions): CS.CompileOptions {
        return options;
      },
      getModule(this: any): CL.PyretCode {
        if (ast === undefined) {
          if (!fileOps.fileExists(this.path)) {
            raise("File " + this.path + " does not exist");
          }
          const f = fileOps.inputFile(this.path);
          const str = f.readFile();
          f.closeFile();
          ast = new CL.PyretAst(PP.surfaceParse(str, this.uri()));
        }
        return ast;
      },
      getDependencies(this: any): CS.AnyDependency[] {
        return CL.getStandardDependencies(this.getModule(), this.uri());
      },
      getNativeModules(): CS.NativeModule[] {
        return [];
      },
      getExtraImports(): CS.ExtraImports {
        return CS.standardImports;
      },
      getGlobals(this: any): CS.Globals { return this.globals; },
      setCompiled(_loadable: CS.Loadable, _provides: Map<string, CS.Provides>): void {
        ast = undefined;
      },
      needsCompile(_provides: Map<string, CS.Provides>): boolean {
        return true;
      },
      getCompiled(): CS.Loadable | undefined {
        return undefined;
      },
      uri(this: any): string {
        return "file://" + fileOps.realPath(this.path).split(P.sep).join("/");
      },
      name(this: any): string { return P.basename(this.path, ""); }
    };
  };
}

export const fileLocator = mockableFileLocator({
  inputFile: (path: string) => ({
    readFile: () => fs.readFileSync(path, 'utf8'),
    closeFile: () => { return; }
  }),
  // F.output-file(_, false): open for writing, truncating.
  outputFile: (path: string) => {
    const fd = fs.openSync(path, 'w');
    return {
      display: (s: string) => { fs.writeSync(fd, s); },
      flush: () => { fs.fsyncSync(fd); },
      closeFile: () => { fs.closeSync(fd); }
    };
  },
  fileExists: (path: string) => fs.existsSync(path),
  fileTimes: (path: string) => {
    const stats = fs.statSync(path);
    return { mtime: Number(stats.mtime), atime: Number(stats.atime), ctime: Number(stats.ctime) };
  },
  realPath: (path: string) => {
    try {
      return fs.realpathSync(path);
    } catch (e) {
      return path; // should this be an error instead?
    }
  }
});
