import type { List, PFunction, PObject, POpaque, PTuple, Runtime } from "./ts-impl-types";
import type * as TCH from './ts-codegen-helpers';
import type * as TDC from './ts-direct-codegen';
import type * as A from "./ts-ast";
import type * as TCS from "./ts-compile-structs";
import type { CompileOptions } from "./ts-compiler-lib-impl";
import type * as TF from './ts-filelib';


///////////////////////////// OLD Types ///////////////////////////
export type CCPDict = {
  provides : string,
  requires : string,
  nativeRequires : string,
  theModule : string,
  theMap : string
}
export type CompiledCodePrinter = POpaque<
    { $name: "ccp-dict", 'dict': CCPDict }
  | { $name: "ccp-string", 'compiled': string }
  | {
    $name: "ccp-two-files",
    staticPath: string, codePath: string
  }
  | { $name: "ccp-file", 'path': string }>

/////////////////////////// Exports //////////////////////////
export interface Exports {
dict: {values: {dict: {
'is-CompiledCodePrinter': PFunction<(val: any) => val is CompiledCodePrinter>

'make-compiled-pyret':
  PFunction<(programAst: A.Program, uri: string, env: TCS.CompileEnvironment, postEnv: TCS.ComputedEnvironment, provides: TCS.Provides, options: CompileOptions) =>
  PTuple<[TCS.Provides, TCS.CompileResult<CompiledCodePrinter>]>>,

'pyret-to-js-runnable': PFunction<(ccp : CompiledCodePrinter) => string>,

'pyret-to-js-static': PFunction<(ccp : CompiledCodePrinter) => string>,

'pyret-to-js-pretty': PFunction<(ccp : CompiledCodePrinter) => string>,

'ccp-has-static-info': PFunction<(ccp : CompiledCodePrinter) => boolean>,

'ccp-dict': PFunction<(dict: PObject<CCPDict>) => CompiledCodePrinter>,
'ccp-two-files': PFunction<(staticPath: string, codePath: string) => CompiledCodePrinter>,
'ccp-file': PFunction<(path: string) => CompiledCodePrinter>,
'ccp-string': PFunction<(compiled: string) => CompiledCodePrinter>,
}}}}

({
  requires: [
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-codegen-helpers'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-direct-codegen'] },
    { 'import-type': 'dependency', protocol: 'file', args: ['compile-structs.arr'] },
    { 'import-type': 'dependency', protocol: 'js-file', args: ['ts-filelib'] },
  ],
  nativeRequires: [],
  provides: {
    values: {
      'is-CompiledCodePrinter': 'tany',
      'make-compiled-pyret': 'tany',
      'ccp-two-files': 'tany',
      'ccp-file': 'tany',
      'ccp-string': 'tany',
      'ccp-dict': 'tany',
      'pyret-to-js-runnable': 'tany',
      'pyret-to-js-static': 'tany',
      'pyret-to-js-pretty': 'tany',
      'ccp-has-static-info': 'tany',
    }
  },
  theModule: function(runtime: Runtime, _, __, TCH: TCH.Exports, DCin: TDC.Exports, CSin: TCS.Exports, Fin: TF.Exports) {
    const DC = DCin.dict.values.dict;
    const CS = CSin.dict.values.dict;
    const F = Fin.dict.values.dict;

    function isCompiledCodePrinter(val: any): val is CompiledCodePrinter {
      const $name = val?.val?.$name;
      return ($name === 'ccp-dict' ||
              $name === 'ccp-string' ||
              $name === 'ccp-two-files' ||
              $name === 'ccp-file');
    }

    function ccpDict(compiled): CompiledCodePrinter {
      return runtime.makeOpaque({
        $name: 'ccp-dict',
        dict: compiled,
      });
    }
    function ccpFile(path: string): CompiledCodePrinter {
      return runtime.makeOpaque({
        $name: 'ccp-file',
        path,
      });
    }
    function ccpTwoFiles(staticPath: string, codePath: string): CompiledCodePrinter {
      return runtime.makeOpaque({
        $name: 'ccp-two-files',
        staticPath,
        codePath,
      });
    }
    function ccpString(compiled: string): CompiledCodePrinter {
      return runtime.makeOpaque({
        $name: 'ccp-string',
        compiled,
      });
    }


    function makeCompiledPyret(programAst: A.Program, uri: string, env: TCS.CompileEnvironment, postEnv: TCS.ComputedEnvironment, provides: TCS.Provides, options: CompileOptions) {
      const compiled = DC["compile-program"].app(programAst, uri, env, postEnv, provides, options);
      return runtime.makeTuple([provides, CS.ok.app(ccpDict(compiled))]);
    }

    function ccpHasStaticInfo(ccp: CompiledCodePrinter): boolean {
      return ccp.val.$name === 'ccp-dict' || ccp.val.$name === 'ccp-two-files';
    }

    function pyretToJsRunnable(ccpVal: CompiledCodePrinter): string {
      const ccp = ccpVal.val;
      switch(ccp.$name) {
        case 'ccp-dict': return ccp.dict.theModule;
        case 'ccp-string': return ccp.compiled;
        case 'ccp-two-files': return F["read-file-path"].app(ccp["codePath"]);
        case 'ccp-file': return F["read-file-path"].app(ccp.path);
        default: throw new TCH.ExhaustiveSwitchError(ccp);
      }
    }

    function pyretToJsStatic(ccpVal: CompiledCodePrinter): string {
      const ccp = ccpVal.val;
      switch(ccp.$name) {
        case 'ccp-dict': return [
          "{",
          `"provides": ${ccp.dict.provides},`,
          `"requires": ${ccp.dict.requires},`,
          `"nativeRequires": ${ccp.dict.nativeRequires},`,
          `"theMap": ${ccp.dict.theMap}`,
          "}"
        ].join("\n");
        case 'ccp-two-files': return F["read-file-path"].app(ccp["staticPath"]);
        case 'ccp-string':
        case 'ccp-file': throw new TCH.InternalCompilerError(`pyret-to-js-static not implemented for ${ccp.$name}`);
        default: throw new TCH.ExhaustiveSwitchError(ccp);
      }
    }

    function pyretToJsPretty(ccpVal: CompiledCodePrinter): string {
      const ccp = ccpVal.val;
      switch(ccp.$name) {
        case 'ccp-dict': return [
          "{",
          `  "provides": ${ccp.dict.provides},`,
          `  "requires": ${ccp.dict.requires},`,
          `  "nativeRequires": ${ccp.dict.nativeRequires},`,
          `  "theModule": ${ccp.dict.theModule}`,
          `  "theMap": ${ccp.dict.theMap}`,
          "}"
        ].join("\n");;
        case 'ccp-string': return ccp.compiled;
        case 'ccp-file':
        case 'ccp-two-files': throw new TCH.InternalCompilerError(`cannot generate pretty JS for ${ccp.$name}`);
        default: throw new TCH.ExhaustiveSwitchError(ccp);
      }
    }

    const exports: Exports['dict']['values']['dict'] = {
      'make-compiled-pyret': runtime.makeFunction(makeCompiledPyret),
      "is-CompiledCodePrinter": runtime.makeFunction(isCompiledCodePrinter),
      'ccp-has-static-info': runtime.makeFunction(ccpHasStaticInfo),
      'pyret-to-js-static': runtime.makeFunction(pyretToJsStatic),
      'pyret-to-js-runnable': runtime.makeFunction(pyretToJsRunnable),
      'pyret-to-js-pretty': runtime.makeFunction(pyretToJsPretty),
      'ccp-dict': runtime.makeFunction(ccpDict),
      'ccp-file': runtime.makeFunction(ccpFile),
      'ccp-two-files': runtime.makeFunction(ccpTwoFiles),
      'ccp-string': runtime.makeFunction(ccpString),
    };
    return runtime.makeModuleReturn(exports, {});
  }
})