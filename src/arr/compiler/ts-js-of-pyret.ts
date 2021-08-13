import type { PFunction, PTuple } from "./ts-impl-types";
import type * as TCH from './ts-codegen-helpers';
import type * as A from "./ts-ast";
import type * as CS from "./ts-compile-structs";
import type { CompileOptions } from "./ts-compiler-lib-impl";

///////////////////////////// OLD Types ///////////////////////////
export type CCPDict = {
  dict: {
    provides : string,
    requires : string,
    nativeRequires : string,
    theModule : string,
    theMap : string
  }
}
export type CompiledCodePrinter = 
  | { $name: "ccp-dict", dict: { 'dict': CCPDict } }
  | { $name: "ccp-string", dict: { 'compiled': string } }
  | {
    $name: "ccp-two-files",
    dict: { 'static-path': string, 'code-path': string }
  }
  | { $name: "ccp-file", dict: { 'path': string } }

/////////////////////////// Exports //////////////////////////
export interface Exports {
dict: {values: {dict: {
'is-CompiledCodePrinter': PFunction<(val: any) => val is CompiledCodePrinter>

'is-ccp-dict': 
  PFunction<(val: any) => val is TCH.Variant<CompiledCodePrinter, 'ccp-dict'>>

'ccp-dict': 
  PFunction< (dict: CCPDict) => TCH.Variant<CompiledCodePrinter, 'ccp-dict'> >

'is-ccp-string': 
  PFunction<(val: any) => val is TCH.Variant<CompiledCodePrinter, 'ccp-string'>>

'ccp-string': 
  PFunction<
    (compiled: string) => TCH.Variant<CompiledCodePrinter, 'ccp-string'>
  >

'is-ccp-two-files': 
  PFunction<(val: any) => val is TCH.Variant<CompiledCodePrinter, 'ccp-two-files'>>

'ccp-two-files': 
  PFunction<
    (static_path: string, code_path: string) => TCH.Variant<CompiledCodePrinter, 'ccp-two-files'>
  >

'is-ccp-file': 
  PFunction<(val: any) => val is TCH.Variant<CompiledCodePrinter, 'ccp-file'>>

'ccp-file': 
  PFunction< (path: string) => TCH.Variant<CompiledCodePrinter, 'ccp-file'> >


'make-compiled-pyret':
  PFunction<(programAst: A.Program, uri: string, env: CS.CompileEnvironment, postEnv: CS.ComputedEnvironment, provides: CS.Provides, options: CompileOptions) =>
  PTuple<[CS.Provides, CS.CompileResult<CompiledCodePrinter>]>>,

'pyret-to-js-runnable': PFunction<(ccp : CompiledCodePrinter) => string>,

}}}}