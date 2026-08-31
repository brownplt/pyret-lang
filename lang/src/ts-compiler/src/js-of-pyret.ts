/*
  Ported from: src/arr/compiler/js-of-pyret.arr
  Glue between the ANF/codegen back end and compile-lib: runs ANF,
  flatness analysis, and the splitting compiler, and wraps the resulting
  dict of JS expressions in a CompiledCodePrinter.
*/

import * as fs from 'fs';
import * as A from './ast';
import * as N from './anf';
import * as AL from './anf-loop-compiler';
import * as C from './compile-structs';
import * as CL from './concat-lists';
import * as FL from './flatness';
import * as J from './js-ast';
import * as PP from './pprint';

const clEmpty = CL.clEmpty;
const clCons = CL.clCons;

// Note: the Pyret original iterates a StringDict via cl-map-sd; here we use a
// Map (insertion order). This preserves byte-parity: the only dict passed here
// is the top-level module object, which has exactly 5 fixed keys (requires,
// provides, nativeRequires, theModule, theMap). A StringDict of <=8 keys is an
// insertion-ordered ArrayMapNode (it only reaches hash order past
// MAX_ARRAY_MAP_SIZE=8; string-dict.js:138,428), and both compilers set those
// keys in the same order, so cl-map-sd and this Map emit identical field order.
// Verified: every compiled module of a sample program is byte-identical across
// the two compilers. (JS object field order is also not semantically
// significant at runtime; the anf-loop-compiler.ts cl-map-sd copy, which does
// take large user/provides dicts, is separately canonicalized by co-sorts.)
function clMapSd<T>(f: (key: string) => T, sd: Map<string, J.JExprT>): CL.ConcatList<T> {
  let acc: CL.ConcatList<T> = clEmpty as any;
  for (const key of sd.keys()) {
    acc = clCons(f(key), acc);
  }
  return acc;
}

export abstract class CompiledCodePrinterBase {
  abstract pyretToJsRunnable(): string;
  abstract printJsRunnable(printer: (s: string) => void): void;
}

export class CCPDict extends CompiledCodePrinterBase {
  get $name(): 'ccp-dict' { return 'ccp-dict'; }
  constructor(public dict: Map<string, J.JExprT>) { super(); }

  toJExpr(d: Map<string, J.JExprT>): J.JExprT {
    return new J.JParens(new J.JObj(clMapSd((k: string) => new J.JField(k, d.get(k)!), d)));
  }
  private dictWithout(key: string): Map<string, J.JExprT> {
    const d2 = new Map(this.dict);
    d2.delete(key);
    return d2;
  }
  pyretToJsStatic(): string {
    return this.toJExpr(this.dictWithout('theModule')).toUglySource();
  }
  printJsStatic(printer: (s: string) => void): void {
    printer(this.pyretToJsStatic());
  }
  pyretToJsPretty(): PP.PPrintDoc {
    return this.toJExpr(this.dict).tosource();
  }
  pyretToJsRunnable(): string {
    return this.toJExpr(this.dict).toUglySource();
  }
  printJsRunnable(printer: (s: string) => void): void {
    printer(this.pyretToJsRunnable());
  }
}

export class CCP extends CompiledCodePrinterBase {
  get $name(): 'ccp' { return 'ccp'; }
  constructor(public compiled: J.JExprT) { super(); }
  pyretToJsPretty(): PP.PPrintDoc {
    return this.compiled.tosource();
  }
  pyretToJsRunnable(): string {
    return this.compiled.toUglySource();
  }
  printJsRunnable(printer: (s: string) => void): void {
    printer(this.pyretToJsRunnable());
  }
}

export class CCPString extends CompiledCodePrinterBase {
  get $name(): 'ccp-string' { return 'ccp-string'; }
  constructor(public compiled: string) { super(); }
  pyretToJsPretty(): PP.PPrintDoc {
    return PP.str(this.compiled);
  }
  pyretToJsRunnable(): string {
    return this.compiled;
  }
  printJsRunnable(printer: (s: string) => void): void {
    printer(this.compiled);
  }
}

export class CCPFile extends CompiledCodePrinterBase {
  get $name(): 'ccp-file' { return 'ccp-file'; }
  constructor(public path: string) { super(); }
  pyretToJsRunnable(): string {
    return fs.readFileSync(this.path, 'utf8');
  }
  printJsRunnable(printer: (s: string) => void): void {
    printer(this.pyretToJsRunnable());
  }
}

export type CompiledCodePrinter = CCPDict | CCP | CCPString | CCPFile;

export function isCCPDict(x: any): x is CCPDict { return x instanceof CCPDict; }
export function isCCPString(x: any): x is CCPString { return x instanceof CCPString; }
export function isCCPFile(x: any): x is CCPFile { return x instanceof CCPFile; }

export function traceMakeCompiledPyret(
  addPhase: (name: string, value: any) => any,
  programAst: A.Program,
  env: C.CompileEnvironment,
  postEnv: C.ComputedEnvironment,
  provides: C.Provides,
  options: C.CompileOptions
): [C.Provides, C.CompileResult<CompiledCodePrinter>] {
  const anfed = addPhase('ANFed', N.anfProgram(programAst));
  const flatnessEnv = addPhase('Build flatness env', FL.makeProgFlatnessEnv(anfed, postEnv, env));
  const flatProvides = addPhase('Get flat-provides', FL.getFlatProvides(provides, env, postEnv, flatnessEnv, anfed));
  const compiled = anfed.visit(AL.splittingCompiler(env, addPhase, flatnessEnv, flatProvides, postEnv, options));
  return [flatProvides, addPhase('Generated JS', C.ok(new CCPDict(compiled)))];
}

export function makeCompiledPyret(
  programAst: any,
  env: C.CompileEnvironment,
  postEnv: C.ComputedEnvironment,
  provides: C.Provides,
  options: C.CompileOptions
): [C.Provides, CompiledCodePrinter] {
  const anfed = N.anfProgram(programAst);
  const flatnessEnv = FL.makeProgFlatnessEnv(anfed, postEnv, env);
  const flatProvides = FL.getFlatProvides(provides, env, postEnv, flatnessEnv, anfed);
  const compiled = anfed.visit(AL.splittingCompiler(env, (_name: string, v: any) => v, flatnessEnv, flatProvides, postEnv, options));
  return [flatProvides, new CCPDict(compiled)];
}
