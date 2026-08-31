/*
  TypeScript port of src/arr/compiler/anf-loop-compiler.arr — the code
  generator. Produces, per module, the dictionary of JS expressions
  ("requires", "provides", "nativeRequires", "theModule", "theMap") that
  js-of-pyret packages into a compiled-code-printer.

  Representation notes:
  - The Pyret `compiler-visitor` object (extended repeatedly with `.{ }`)
    is the class `CompilerVisitor`; Pyret's functional extension `obj.{f: v}`
    is `ext(obj, {f: v})`, which clones the object (preserving its
    prototype, hence all visitor methods) and overrides the given fields.
  - `sha.sha256(uri)` is node crypto's sha256 hex digest of the UTF-8 uri
    (verified byte-identical against the existing compiled output:
    _96cdfee8...__N == sha256("file:///tmp/ptest/hello.arr")).
  - `js-ids`/`effective-ids` are module-level mutable caches in the Pyret
    source (they persist across compile-module calls within one process,
    while `js-names` IS reset per module); mirrored exactly here.
  - Key-ORDER canonicalization: every site where emitted code depends on
    dict/set iteration order sorts keys first, matching sorts added to the
    Pyret compiler (anf-loop-compiler.arr clMapSd / compileFunBody vars /
    compileModule free-ids; resolve-scope.arr defined-* and final
    provides). Output is byte-identical between the two compilers; keep
    the sorts in lockstep.
*/

import { sha256 } from './sha256';
import * as A from './ast';
import * as N from './ast-anf';
import * as J from './js-ast';
import * as CS from './compile-structs';
import * as CL from './concat-lists';
import * as FL from './flatness';
import * as DAG from './js-dag-utils';
import * as AU from './ast-util';
import * as T from './type-structs';
import * as SL from './srcloc';
import { jsnums } from './interop/js-numbers';
import { InternalCompilerError, raise, map2, mapGetValue, partition, field } from './shared';

export type Loc = SL.Loc;
export type CList<T> = CL.ConcatList<T>;
const clist = CL.clist;

const clEmpty: CList<any> = CL.clEmpty;
const clSing = CL.clSing;
const clAppend = CL.clAppend;
const clCons = CL.clCons;
const clSnoc = CL.clSnoc;

// CompileOptions by the time codegen runs has should-profile already
// applied to the locator (see compile-lib.arr), so it is treated as a
// boolean here (tested only for truthiness, as Pyret's `if` would).
export type SplitCompileOptions = Omit<CS.CompileOptions, 'shouldProfile'> & { shouldProfile: any };

export function getExp(o: any): any { return o.exp; }
export function getId(o: any): any { return o.id; }
export function getName(o: any): any { return o.name; }
export function getL(o: any): any { return o.l; }
export function getBind(o: any): any { return o.bind; }
export function oGetField(o: any): any { return o.field; }

export function clMapSd<T, U>(f: (key: string) => U, sd: Map<string, T>): CList<U> {
  // Sorted key order, in lockstep with cl-map-sd in anf-loop-compiler.arr
  let acc: CList<U> = clEmpty;
  for (const key of [...sd.keys()].sort()) {
    acc = clSnoc(acc, f(key));
  }
  return acc;
}

export function makeFunName(compiler: any, loc: Loc): string {
  return '_' + sha256(compiler.uri) + '__' + String(compiler.getLocId(loc));
}

export function typeName(str: string): string {
  return '$type$' + str;
}

// ---------- j-ast shorthands (mirroring the Pyret import aliases) ----------

const jFun = (id: string, name: string, args: CList<A.Name>, body: J.JBlockT): J.JFun => new J.JFun(id, name, args, body);
const jVar = (name: A.Name, rhs: J.JExprT): J.JVar => new J.JVar(name, rhs);
const jId = (id: A.Name): J.JId => new J.JId(id);
const jMethod = (obj: J.JExprT, meth: string, args: CList<J.JExprT>): J.JMethod => new J.JMethod(obj, meth, args);
const jBlock = (stmts: CList<J.JStmt>): J.JBlock => new J.JBlock(stmts);
const jBlock1 = (stmt: J.JStmt): J.JBlock1 => new J.JBlock1(stmt);
const jTrue = J.jTrue;
const jFalse = J.jFalse;
const jNum = (n: any): J.JNum => new J.JNum(n);
const jStr = (s: string): J.JStr => new J.JStr(s);
const jReturn = (expr: J.JExprT): J.JReturn => new J.JReturn(expr);
const jAssign = (name: A.Name, rhs: J.JExprT): J.JAssign => new J.JAssign(name, rhs);
const jIf = (cond: J.JExprT, consq: J.JBlockT, alt: J.JBlockT): J.JIf => new J.JIf(cond, consq, alt);
const jIf1 = (cond: J.JExprT, consq: J.JBlockT): J.JIf1 => new J.JIf1(cond, consq);
const jNew = (func: J.JExprT, args: CList<J.JExprT>): J.JNew => new J.JNew(func, args);
const jApp = (func: J.JExprT, args: CList<J.JExprT>): J.JApp => new J.JApp(func, args);
const jList = (multiLine: boolean, elts: CList<J.JExprT>): J.JList => new J.JList(multiLine, elts);
const jObj = (fields: CList<J.JFieldT>): J.JObj => new J.JObj(fields);
const jDot = (obj: J.JExprT, field: string): J.JDot => new J.JDot(obj, field);
const jBracket = (obj: J.JExprT, field: J.JExprT): J.JBracket => new J.JBracket(obj, field);
const jField = (name: string, value: J.JExprT): J.JField => new J.JField(name, value);
const jDotAssign = (obj: J.JExprT, name: string, rhs: J.JExprT): J.JDotAssign => new J.JDotAssign(obj, name, rhs);
const jBracketAssign = (obj: J.JExprT, field: J.JExprT, rhs: J.JExprT): J.JBracketAssign => new J.JBracketAssign(obj, field, rhs);
const jThrow = (exp: J.JExprT): J.JThrow => new J.JThrow(exp);
const jExpr = (expr: J.JExprT): J.JExpr => new J.JExpr(expr);
const jBinop = (left: J.JExprT, op: J.JBinopT, right: J.JExprT): J.JBinop => new J.JBinop(left, op, right);
const jAnd = J.jAnd;
const jOr = J.jOr;
const jLt = J.jLt;
const jEq = J.jEq;
const jNeq = J.jNeq;
const jGeq = J.jGeq;
const jUnop = (exp: J.JExprT, op: J.JUnopT): J.JUnop => new J.JUnop(exp, op);
const jDecr = J.jDecr;
const jIncr = J.jIncr;
const jNot = J.jNot;
const jTypeof = J.jTypeof;
const jInstanceof = J.jInstanceof;
const jTernary = (test: J.JExprT, consq: J.JExprT, altern: J.JExprT): J.JTernary => new J.JTernary(test, consq, altern);
const jNull = J.jNull;
const jParens = (exp: J.JExprT): J.JParens => new J.JParens(exp);
const jSwitch = (exp: J.JExprT, branches: CList<J.JCaseT>): J.JSwitch => new J.JSwitch(exp, branches);
const jCase = (exp: J.JExprT, body: J.JBlockT): J.JCase => new J.JCase(exp, body);
const jDefault = (body: J.JBlockT): J.JDefault => new J.JDefault(body);
const jBreak = J.jBreak;
const jContinue = J.jContinue;
const jWhile = (cond: J.JExprT, body: J.JBlockT): J.JWhile => new J.JWhile(cond, body);
const jFor = (createVar: boolean, init: J.JExprT, cond: J.JExprT, update: J.JExprT, body: J.JBlockT): J.JFor =>
  new J.JFor(createVar, init, cond, update, body);
const jRawCode = (s: string): J.JRawCode => new J.JRawCode(s);
const jUndefined = J.jUndefined;
const isJAssign = J.isJAssign;
const makeLabelSequence = J.makeLabelSequence;

export function consoleLog(lst: CList<J.JExprT>): J.JExprT {
  return jApp(jId(new A.SName(A.dummyLoc, 'console.log')), lst);
}
export function consoleLogStmt(lst: CList<J.JExprT>): J.JStmt {
  return jExpr(consoleLog(lst));
}

export const isTData = T.isTData;

// ---------- data BindType ----------

export abstract class BindTypeBase {
  abstract get $name(): string;
}

export class BLet extends BindTypeBase {
  get $name(): 'b-let' { return 'b-let'; }
  constructor(public value: N.ABind) { super(); }
}

export class BArray extends BindTypeBase {
  get $name(): 'b-array' { return 'b-array'; }
  constructor(public value: N.ABind, public idx: number) { super(); }
}

export type BindType = BLet | BArray;

export function isBLet(x: any): x is BLet { return x instanceof BLet; }
export function isBArray(x: any): x is BArray { return x instanceof BArray; }

// this structure stores bindings of case dispatch objects
// so that the objects can be allocated only once in the top level, avoiding
// multiple allocations which could affect performance, particularly in recursive
// functions.
export class DispatchesBox {
  get $name(): 'dispatches-box' { return 'dispatches-box'; }
  // ref dispatches
  constructor(public dispatches: CList<J.JStmt>) {}
}

export type Dispatches = DispatchesBox;
export function isDispatchesBox(x: any): x is DispatchesBox { return x instanceof DispatchesBox; }

// ---------- js-id allocation ----------

export const jsNames = A.MakeName(0);
export const jsIds: Map<string, A.Name> = new Map();
export const effectiveIds: Map<string, boolean> = new Map();

export function freshId(id: A.Name): A.Name {
  const baseName = A.isSTypeGlobal(id) ? id.tosourcestring() : id.toname();
  const noHyphens = baseName.split('-').join('$');
  let n = jsNames.makeAtom(noHyphens);
  while (effectiveIds.has(n.tosourcestring())) { // awkward name collision!
    n = jsNames.makeAtom(noHyphens);
  }
  effectiveIds.set(n.tosourcestring(), true);
  return n;
}

export function jsIdOf(id: A.Name): A.Name {
  const s = id.key();
  const cached = jsIds.get(s);
  if (cached !== undefined) {
    return cached;
  } else {
    const safeId = freshId(id);
    jsIds.set(s, safeId);
    return safeId;
  }
}

export function constId(name: string): A.SName {
  return new A.SName(A.dummyLoc, name);
}

export function compilerName(id: string): A.SName {
  return constId('$' + id);
}

export function formalShadowName(id: A.Name): A.Name {
  const jsId = jsIdOf(id);
  return new A.SName(A.dummyLoc, '$' + jsId.tosourcestring());
}

export const getFieldLoc: J.JExprT = jId(constId('G'));
export const throwUninitialized: J.JExprT = jId(constId('U'));
export const sourceName: J.JId = jId(constId('M'));
// the Pyret source calls this binding `undefined`
export const UNDEFINED: J.JExprT = jId(constId('D'));
export const RUNTIME: J.JId = jId(constId('R'));
export const NAMESPACE: J.JId = jId(constId('NAMESPACE'));
export const THIS: J.JExprT = jId(constId('this'));
export const ARGUMENTS: J.JExprT = jId(constId('arguments'));

export const rtNameMap: Map<string, string> = new Map([
  ['addModuleToNamespace', 'aMTN'],
  ['checkArityC', 'cAC'],
  ['checkRefAnns', 'cRA'],
  ['derefField', 'dF'],
  ['getColonFieldLoc', 'gCFL'],
  ['getDotAnn', 'gDA'],
  ['getField', 'gF'],
  ['getFieldRef', 'gFR'],
  ['getBracket', 'gB'],
  ['hasBrand', 'hB'],
  ['isActivationRecord', 'isAR'],
  ['isCont', 'isC'],
  ['isFunction', 'isF'],
  ['isMethod', 'isM'],
  ['isPyretException', 'isPE'],
  ['isPyretTrue', 'isPT'],
  ['makeActivationRecord', 'mAR'],
  ['makeBoolean', 'mB'],
  ['makeBranderAnn', 'mBA'],
  ['makeCont', 'mC'],
  ['makeDataValue', 'mDV'],
  ['makeFunction', 'mF'],
  ['makeGraphableRef', 'mGR'],
  ['makeMatch', 'mM'],
  ['makeMethod', 'mMet'],
  ['makeMethodN', 'mMN'],
  ['makeObject', 'mO'],
  ['makePredAnn', 'mPA'],
  ['makeRecordAnn', 'mRA'],
  ['makeTupleAnn', 'mTA'],
  ['makeVariantConstructor', 'mVC'],
  ['namedBrander', 'nB'],
  ['profileEnter', 'pEn'],
  ['profileExit', 'pEx'],
  ['traceEnter', 'tEn'],
  ['traceErrExit', 'tErEx'],
  ['traceExit', 'tEx'],
  ['_checkAnn', '_cA'],
]);

export const jBool = (b: boolean): J.JExprT => (b ? jTrue : jFalse);

export function objOfLoc(l: Loc): J.JExprT {
  switch (l.$name) {
    case 'builtin':
      return jList(false, clist(jStr(l.moduleName)));
    case 'srcloc':
      return jList(false, clist<J.JExprT>(
        jStr(l.source),
        jNum(l.startLine),
        jNum(l.startColumn),
        jNum(l.startChar),
        jNum(l.endLine),
        jNum(l.endColumn),
        jNum(l.endChar)
      ));
    default:
      throw new InternalCompilerError('Unknown Loc in obj-of-loc');
  }
}

export function wrapWithSrcnode(l: Loc, expr: J.JExprT): J.JExprT {
  switch (l.$name) {
    case 'builtin':
      return expr;
    case 'srcloc':
      return new J.JSourcenode(l, l.source, expr);
    default:
      throw new InternalCompilerError('Unknown Loc in wrap-with-srcnode');
  }
}

export function getDictField(obj: J.JExprT, field: J.JExprT): J.JExprT {
  return jBracket(jDot(obj, 'dict'), field);
}

// Use when we're sure the field will exist
export function getFieldUnsafe(obj: J.JExprT, field: J.JExprT, locExpr: J.JExprT): J.JExprT {
  return jApp(getFieldLoc, clist(obj, field, locExpr));
}

export function getBracketUnsafe(obj: J.JExprT, field: J.JExprT, locExpr: J.JExprT): J.JExprT {
  return rtMethod('getBracket', clist(obj, field, locExpr));
}

// When the field may not exist, add source mapping so if we can't find it
// we get a useful stacktrace
export function getFieldSafe(l: Loc, obj: J.JExprT, field: J.JExprT, locExpr: J.JExprT): J.JExprT {
  return wrapWithSrcnode(l, getFieldUnsafe(obj, field, locExpr));
}

export function getBracketSafe(l: Loc, obj: J.JExprT, field: J.JExprT, locExpr: J.JExprT): J.JExprT {
  return wrapWithSrcnode(l, getBracketUnsafe(obj, field, locExpr));
}

export function getFieldRef(obj: J.JExprT, field: J.JExprT, loc: J.JExprT): J.JExprT {
  return rtMethod('getFieldRef', clist(obj, field, loc));
}

export function raiseIdExn(loc: J.JExprT, name: string): J.JExprT {
  return jApp(throwUninitialized, clist(loc, jStr(name)));
}

export function addStackFrame(exnId: A.Name, loc: J.JExprT): J.JExprT {
  return jMethod(jDot(jId(exnId), 'pyretStack'), 'push', clist(loc));
}

export function rtField(name: string): J.JExprT {
  return jDot(RUNTIME, name);
}

export function rtMethod(name: string, args: CList<J.JExprT>): J.JExprT {
  const shortName = rtNameMap.get(name);
  const rtName = shortName === undefined ? name : shortName;
  return jMethod(RUNTIME, rtName, args);
}

export function logAnd(log: CList<J.JExprT>, ret: J.JExprT): J.JExprT {
  return jBracket(jList(true, clist(consoleLog(log), ret)), jNum(1));
}

export function getField(obj: J.JExprT, field: string): J.JExprT {
  return rtMethod('getField', clist(obj, jStr(field)));
}

export function getModuleField(uri: string, which: string, name: string): J.JExprT {
  return rtMethod('getModuleField', clist<J.JExprT>(jStr(uri), jStr(which), jStr(name)));
}

export function app(l: Loc, f: J.JExprT, args: CList<J.JExprT>): J.JExprT {
  switch (l.$name) {
    case 'builtin':
      return jMethod(f, 'app', args);
    default:
      return new J.JSourcenode(l, (l as SL.Srcloc).source, jMethod(f, 'app', args));
  }
}

export function checkFun(sourcemapLoc: Loc, variableLoc: J.JExprT, f: J.JExprT): J.JStmt {
  let call: J.JExprT;
  switch (sourcemapLoc.$name) {
    case 'builtin':
      call = jMethod(rtField('ffi'), 'throwNonFunApp', clist(variableLoc, f));
      break;
    case 'srcloc':
      call = new J.JSourcenode(sourcemapLoc, sourcemapLoc.source,
        jMethod(rtField('ffi'), 'throwNonFunApp', clist(variableLoc, f)));
      break;
    default:
      throw new InternalCompilerError('Unknown Loc in check-fun');
  }
  return jIf1(jBinop(jUnop(jParens(jDot(f, 'app')), jTypeof), jNeq, jStr('function')),
    jBlock1(jExpr(call)));
}

const cExp = (exp: J.JExprT, otherStmts: CList<J.JStmt>): DAG.CExp => new DAG.CExp(exp, otherStmts);
const cField = (field: J.JFieldT, otherStmts: CList<J.JStmt>): DAG.CField => new DAG.CField(field, otherStmts);
const cBlock = (block: J.JBlockT, newCases: CList<J.JCaseT>): DAG.CBlock => new DAG.CBlock(block, newCases);

export function annLoc(ann: A.Ann): Loc {
  if (A.isABlank(ann)) { return A.dummyLoc; }
  else { return (ann as any).l; }
}

export function isFlatEnough(flatness: FL.Flatness): boolean {
  if (flatness === undefined) { return false; }
  else { return flatness <= 5; }
}

export function isFunctionFlat(flatnessEnv: FL.FEnv, funName: string): boolean {
  const flatnessOpt = flatnessEnv.get(funName);
  return isFlatEnough(flatnessOpt);
}

// Pyret object extension `obj.{ f1: v1, ... }`: clone preserving prototype
// (visitor methods), overriding the given fields.
function ext<T extends object>(obj: T, fields: Record<string, any>): T {
  const out = Object.create(Object.getPrototypeOf(obj));
  Object.assign(out, obj, fields);
  return out as T;
}

export function compileAnn(ann: A.Ann, optName: string | undefined, visitor: CompilerVisitor): DAG.CExp {
  switch (ann.$name) {
    case 'a-name':
      return cExp(jId(jsIdOf(ann.id)), clEmpty);
    case 'a-type-var':
      return cExp(rtField('Any'), clEmpty);
    case 'a-arrow':
      return cExp(rtField('Function'), clEmpty);
    case 'a-arrow-argnames':
      return cExp(rtField('Function'), clEmpty);
    case 'a-method':
      return cExp(rtField('Method'), clEmpty);
    case 'a-app':
      return compileAnn(ann.ann, optName, visitor);
    case 'a-record': {
      let names: CList<J.JExprT> = clEmpty;
      let locs: CList<J.JExprT> = clEmpty;
      let fields: CList<J.JFieldT> = clEmpty;
      let others: CList<J.JStmt> = clEmpty;
      for (const field of ann.fields) {
        const compiled = compileAnn(field.ann, undefined, visitor);
        names = clSnoc(names, jStr(field.name));
        locs = clSnoc(locs, visitor.getLoc(field.l));
        fields = clSnoc(fields, jField(field.name, compiled.exp));
        others = clAppend(others, compiled.otherStmts);
      }
      return cExp(
        rtMethod('makeRecordAnn', clist<J.JExprT>(
          jList(false, names),
          jList(false, locs),
          jObj(fields),
          optName !== undefined ? jStr(optName) : jUndefined
        )),
        others
      );
    }
    case 'a-tuple': {
      let locs: CList<J.JExprT> = clEmpty;
      let fields: CList<J.JExprT> = clEmpty;
      let others: CList<J.JStmt> = clEmpty;
      for (const field of ann.fields) {
        const compiled = compileAnn(field, optName, visitor);
        locs = clSnoc(locs, visitor.getLoc(annLoc(field)));
        fields = clSnoc(fields, compiled.exp);
        others = clAppend(others, compiled.otherStmts);
      }
      return cExp(
        rtMethod('makeTupleAnn', clist<J.JExprT>(
          jList(false, locs),
          jList(false, fields),
          optName !== undefined ? jStr(optName) : jUndefined
        )),
        others
      );
    }
    case 'a-pred': {
      const exp = ann.exp;
      let name: string;
      switch (exp.$name) {
        case 's-id': name = exp.id.toname(); break;
        case 's-id-letrec': name = exp.id.toname(); break;
        default: throw new InternalCompilerError('Unknown name in a-pred in compile-ann: ' + exp.$name);
      }
      let exprToCompile: N.AVal;
      switch (exp.$name) {
        case 's-id': exprToCompile = new N.AId(exp.l, exp.id); break;
        case 's-id-letrec': exprToCompile = new N.AIdLetrec(exp.l, exp.id, exp.safe) as any; break;
        default: throw new InternalCompilerError('Unknown expr in a-pred in compile-ann: ' + (exp as any).$name);
      }
      const compiledBase = compileAnn(ann.ann, optName, visitor);
      const compiledExp = (exprToCompile as any).visit(visitor) as DAG.CExp;
      const isFlat =
        isFlatEnough(FL.annFlatness(ann.ann, visitor.flatnessEnv, visitor.typeFlatnessEnv, visitor.moduleBindings, visitor.env))
        && isFunctionFlat(visitor.flatnessEnv, (exp as any).id.key());
      const predMaker = isFlat ? 'makeFlatPredAnn' : 'makePredAnn';
      return cExp(
        rtMethod(predMaker, clist(compiledBase.exp, compiledExp.exp, jStr(name))),
        clAppend(compiledBase.otherStmts, compiledExp.otherStmts)
      );
    }
    case 'a-dot':
      return cExp(
        rtMethod('getDotAnn', clist(
          visitor.getLoc(ann.l),
          jStr(ann.obj.toname()),
          jDot(jDot(jId(jsIdOf(ann.obj)), 'dict'), 'types'),
          jStr(ann.field))),
        clEmpty);
    case 'a-blank':
      return cExp(rtField('Any'), clEmpty);
    case 'a-any':
      return cExp(rtField('Any'), clEmpty);
    default:
      throw new InternalCompilerError('Unknown ann in compile-ann: ' + (ann as any).$name);
  }
}

export function arityCheck(locExpr: J.JExprT, arity: number, isMethod: boolean): CList<J.JStmt> {
  const len = jId(compilerName('l'));
  const iter = jId(compilerName('i'));
  const t = jId(compilerName('t'));
  return clist<J.JStmt>(
    jVar(len.id, jDot(ARGUMENTS, 'length')),
    jIf1(jBinop(len, jNeq, jNum(arity)),
      jBlock(clist<J.JStmt>(
        jVar(t.id, jNew(jId(constId('Array')), clist<J.JExprT>(len))),
        jFor(true, jAssign(iter.id, jNum(0)), jBinop(iter, jLt, len), jUnop(iter, jIncr),
          jBlock1(jExpr(jBracketAssign(t, iter, jBracket(ARGUMENTS, iter))))),
        jExpr(rtMethod('checkArityC', clist(locExpr, jNum(arity), t, jBool(isMethod))))))));
}

export const noVars = (): Map<string, A.Name> => new Map();

export function localBoundVars(kase: J.JCaseT, vars: Map<string, A.Name>): Map<string, A.Name> {
  function e(expr: J.JExprT): void {
    switch (expr.$name) {
      case 'j-sourcenode': e(expr.expr); break;
      case 'j-parens': e(expr.exp); break;
      case 'j-raw-code': break;
      case 'j-unop': e(expr.exp); break;
      case 'j-binop':
        e(expr.left);
        e(expr.right);
        break;
      case 'j-fun':
        // the body of a function contributes no *locally* bound vars
        break;
      case 'j-new':
        e(expr.func);
        expr.args.each(e);
        break;
      case 'j-app':
        e(expr.func);
        expr.args.each(e);
        break;
      case 'j-method':
        // the body of a method contributes no *locally* bound vars
        break;
      case 'j-ternary':
        e(expr.test);
        e(expr.consq);
        e(expr.altern);
        break;
      case 'j-assign': e(expr.rhs); break;
      case 'j-bracket-assign':
        e(expr.obj);
        e(expr.field);
        e(expr.rhs);
        break;
      case 'j-dot-assign':
        e(expr.obj);
        e(expr.rhs);
        break;
      case 'j-dot': e(expr.obj); break;
      case 'j-bracket':
        e(expr.obj);
        e(expr.field);
        break;
      case 'j-list':
        expr.elts.each(e);
        break;
      case 'j-obj':
        expr.fields.each(f);
        break;
      case 'j-id': break;
      case 'j-str': break;
      case 'j-num': break;
      case 'j-true': break;
      case 'j-false': break;
      case 'j-null': break;
      case 'j-undefined': break;
      case 'j-label': break;
      default:
        throw new InternalCompilerError('Unknown JExpr in local-bound-vars: ' + (expr as any).$name);
    }
  }
  function c(kase2: J.JCaseT): void {
    switch (kase2.$name) {
      case 'j-case':
        e(kase2.exp);
        b(kase2.body);
        break;
      case 'j-default':
        b(kase2.body);
        break;
      default:
        throw new InternalCompilerError('Unknown JCase in local-bound-vars: ' + (kase2 as any).$name);
    }
  }
  function f(field: J.JFieldT): void {
    e(field.value);
  }
  function s(stmt: J.JStmt): void {
    switch (stmt.$name) {
      case 'j-var':
        // Ignore all variables named $underscore#####
        if (A.isSAtom(stmt.name) && (stmt.name.base === '$underscore')) {
          e(stmt.rhs);
        } else {
          e(stmt.rhs);
          vars.set(stmt.name.key(), stmt.name);
        }
        break;
      case 'j-if1':
        e(stmt.cond);
        b(stmt.consq);
        break;
      case 'j-if':
        e(stmt.cond);
        b(stmt.consq);
        b(stmt.alt);
        break;
      case 'j-return': e(stmt.expr); break;
      case 'j-try-catch':
        b(stmt.body);
        // ignoring the exn name, because it's not a Pyret variable
        b(stmt.catch);
        break;
      case 'j-throw': e(stmt.exp); break;
      case 'j-expr': e(stmt.expr); break;
      case 'j-break': break;
      case 'j-continue': break;
      case 'j-switch':
        e(stmt.exp);
        stmt.branches.each(c);
        break;
      case 'j-while':
        e(stmt.cond);
        b(stmt.body);
        break;
      case 'j-for':
        e(stmt.init);
        e(stmt.cond);
        e(stmt.update);
        b(stmt.body);
        break;
      default:
        throw new InternalCompilerError('Unknown JStmt in local-bound-vars: ' + (stmt as any).$name);
    }
  }
  function b(blk: J.JBlockT): void {
    switch (blk.$name) {
      case 'j-block1': s(blk.stmt); break;
      case 'j-block': blk.stmts.each(s); break;
      default:
        throw new InternalCompilerError('Unknown JBlock in local-bound-vars: ' + (blk as any).$name);
    }
  }
  c(kase);
  return vars;
}

export function copyMutableDict<E>(s: Map<string, E>): Map<string, E> {
  // NOTE(ordering): Pyret's freeze().unfreeze() round-trips through the
  // hash trie and re-orders keys; this copy keeps insertion order.
  // See file header, site 2.
  return new Map(s);
}

let totalTime = 0;

function timeNow(): number { return Date.now(); }

const showStackTrace = false;

export function compileFunBody(
  l: Loc,
  step: A.Name,
  funName: A.Name,
  compiler: CompilerVisitor,
  args: N.ABind[],
  optArity: number | undefined,
  body: N.AExpr,
  _shouldReportErrorFrame: boolean,
  isFlat: boolean,
  isMethod: boolean
): J.JBlockT {
  let argUsedInLambda = false;
  const argNames = args.map((a) => a.id);
  const pendingBodies: Array<{ body: N.AExpr; lam: boolean }> = [{ body, lam: false }];
  function scanVal(v: N.AVal, lam: boolean): void {
    if (lam && !argUsedInLambda && v.$name === 'a-id' && argNames.some((an) => an.key() === v.id.key())) {
      argUsedInLambda = true;
    }
  }
  function scanHead(h: N.AExprHead, lam: boolean): void {
    switch (h.$name) {
      case 'a-let':
      case 'a-arr-let':
      case 'a-var':
        scanLettable(h.e, lam);
        break;
      case 'a-seq':
        scanLettable(h.e1, lam);
        break;
      case 'a-type-let':
        break;
    }
  }
  function scanLettable(e: N.ALettable, lam: boolean): void {
    switch (e.$name) {
      case 'a-module':
        scanVal(e.answer, lam);
        scanVal(e.checks, lam);
        break;
      case 'a-cases':
        scanVal(e.val, lam);
        for (const b of e.branches) {
          pendingBodies.push({ body: b.body, lam });
        }
        pendingBodies.push({ body: e._else, lam });
        break;
      case 'a-if':
        for (const b of e.branches) {
          for (const h of b.heads) {
            scanHead(h, lam);
          }
          scanVal(b.test, lam);
          pendingBodies.push({ body: b.body, lam });
        }
        pendingBodies.push({ body: e.elseBody, lam });
        break;
      case 'a-assign':
        scanVal(e.value, lam);
        break;
      case 'a-app':
        scanVal(e._fun, lam);
        for (const a of e.args) { scanVal(a, lam); }
        break;
      case 'a-method-app':
        scanVal(e.obj, lam);
        for (const a of e.args) { scanVal(a, lam); }
        break;
      case 'a-prim-app':
        for (const a of e.args) { scanVal(a, lam); }
        break;
      case 'a-tuple':
        for (const f of e.fields) { scanVal(f, lam); }
        break;
      case 'a-tuple-get':
        scanVal(e.tup, lam);
        break;
      case 'a-obj':
        for (const f of e.fields) { scanVal(f.value, lam); }
        break;
      case 'a-update':
      case 'a-extend':
        scanVal(e.supe, lam);
        for (const f of e.fields) { scanVal(f.value, lam); }
        break;
      case 'a-dot':
      case 'a-colon':
      case 'a-get-bang':
        scanVal(e.obj, lam);
        break;
      case 'a-lam':
      case 'a-method':
        pendingBodies.push({ body: e.body, lam: true });
        break;
      case 'a-val':
        scanVal(e.v, lam);
        break;
      case 'a-data-expr':
        for (const v of e.variants) {
          for (const wm of v.withMembers) { scanVal(wm.value, lam); }
        }
        for (const s of e.shared) { scanVal(s.value, lam); }
        break;
      default:
        // a-ref, a-id-var, a-id-var-modref, a-id-letrec: no a-id values
        break;
    }
  }
  while (pendingBodies.length > 0 && !argUsedInLambda) {
    const item = pendingBodies.pop()!;
    for (const h of item.body.heads) {
      scanHead(h, item.lam);
    }
    scanLettable(item.body.e, item.lam);
  }
  if (argUsedInLambda) {
    compiler = ext(compiler, { allowTco: false });
  }
  const makeLabel = makeLabelSequence(0);
  const retLabel = makeLabel();
  const ans = freshId(compilerName('ans'));
  const apploc = freshId(compilerName('al'));
  const doloop = freshId(compilerName('skiploop'));
  const elidedFrames = freshId(compilerName('elidedFrames'));
  const localCompiler: CompilerVisitor = ext(compiler, {
    makeLabel: makeLabel,
    curTarget: retLabel,
    curStep: step,
    curAns: ans,
    curApploc: apploc,
    args: args.map((a) => a.id).map(jsIdOf),
    elidedFrames: elidedFrames,
  });
  void doloop;
  // To avoid penalty for assigning to formal parameters and also using the arguments object,
  // we create a shadow set of formal arguments, and immediately assign them to the "real" ones
  // in the normal entry case.  This expands the function preamble, but might enable JS optimizations,
  // so it should be worth it
  const formalArgs = args.map((arg) => new N.ABind(arg.l, formalShadowName(arg.id), arg.ann));
  const visitedBody: DAG.CBlock = compileAExpr(localCompiler, body);
  const noRealArgs = args[0].id.key() === (compiler.resumer as A.Name).key();
  const copyFormalsToArgs: CList<J.JStmt> =
    noRealArgs ? clEmpty
      : CL.map_list2((formalArg: N.ABind, arg: N.ABind) => jVar(jsIdOf(arg.id), jId(formalArg.id)) as J.JStmt, formalArgs, args);
  const annCases = compileAnns(localCompiler, step, args, localCompiler.makeLabel());
  let mainBodyCases: CList<J.JCaseT> = clEmpty;
  mainBodyCases = clAppend(mainBodyCases, annCases.newCases);
  mainBodyCases = clSnoc(mainBodyCases, jCase(annCases.newLabel, visitedBody.block));
  mainBodyCases = clAppend(mainBodyCases, visitedBody.newCases);
  // Initialize the case numbers, for more legible output...
  mainBodyCases.each((c: J.JCaseT) => {
    if (J.isJCase(c)) { ((c.exp as J.JLabel).label).get(); }
  });
  const start = timeNow();
  const mainBodyCasesAndDeadVars = DAG.simplify(compiler.addPhase, mainBodyCases, step, compiler.dispatches.dispatches);
  const finish = timeNow() - start;
  totalTime = totalTime + finish;
  mainBodyCases = mainBodyCasesAndDeadVars.body;
  const allVars: Map<string, A.Name> = new Map();
  mainBodyCases.each((caseExpr: J.JCaseT) => {
    localBoundVars(caseExpr, allVars);
  });
  const allNeededVars = copyMutableDict(allVars);
  for (const d of mainBodyCasesAndDeadVars.discardableVars.keys()) {
    allNeededVars.delete(d);
  }
  const vars: A.Name[] = [...allNeededVars.keys()].sort()
    .map((k) => mapGetValue(allNeededVars, k));

  const numVars = vars.length;

  let switchCases: CList<J.JCaseT> = mainBodyCases;
  switchCases = clSnoc(switchCases, jCase(localCompiler.curTarget, jBlock(
    (showStackTrace
      ? clist<J.JStmt>(jExpr(rtMethod('traceExit', clist<J.JExprT>(jStr(String(l)), jNum(numVars)))))
      : clEmpty)
      .append(isFlat
        ? clEmpty
        : clSing<J.JStmt>(jExpr(jUnop(rtField('GAS'), jIncr))))
      .append(localCompiler.options.shouldProfile
        ? clSing<J.JStmt>(jExpr(rtMethod('profileExit', clist(localCompiler.getLoc(l)))))
        : clEmpty)
      .append(clSing<J.JStmt>(jReturn(jId(localCompiler.curAns)))))));
  switchCases = clSnoc(switchCases, jDefault(jBlock1(
    jExpr(jMethod(rtField('ffi'), 'throwSpinnakerError', clist(localCompiler.getLoc(l), jId(step)))))));

  const actRecord = rtMethod('makeActivationRecord', clist<J.JExprT>(
    jId(apploc),
    jId(funName),
    jId(step),
    jList(false, noRealArgs ? clEmpty : CL.map_list((a: N.ABind) => jId(jsIdOf(a.id)) as J.JExprT, args)),
    jList(false, CL.map_list((v: A.Name) => jId(v) as J.JExprT, vars)),
    jId(elidedFrames)
  ));
  const e = freshId(compilerName('e'));
  void e;
  const firstArg = formalArgs[0].id;
  const entryExit = clist<J.JExprT>(
    jStr(String(l)),
    jNum(numVars)
  );

  const restorer =
    jBlock(
      clist<J.JStmt>(
        jExpr(jAssign(step, jDot(jId(firstArg), 'step'))),
        jExpr(jAssign(apploc, jDot(jId(firstArg), 'from'))),
        jExpr(jAssign(localCompiler.curAns, jDot(jId(firstArg), 'ans'))),
        jExpr(jAssign(elidedFrames, jDot(jId(firstArg), 'elidedFrames')))
      )
        .append(CL.map_list_n((i: number, arg: N.ABind) =>
          jExpr(jAssign(jsIdOf(arg.id), jBracket(jDot(jId(firstArg), 'args'), jNum(i)))) as J.JStmt, 0, args))
        .append(CL.map_list_n((i: number, v: A.Name) =>
          jExpr(jAssign(v, jBracket(jDot(jId(firstArg), 'vars'), jNum(i)))) as J.JStmt, 0, vars)));

  const traceEnter = rtMethod('traceEnter', entryExit);
  let firstEntryStmts: CList<J.JStmt>;
  if (optArity !== undefined) {
    const stmts = clAppend(arityCheck(localCompiler.getLoc(l), optArity, isMethod), copyFormalsToArgs);
    if (showStackTrace) {
      // NOTE: the Pyret source snocs the bare expression here (no j-expr);
      // preserved as written (this branch is dead: show-stack-trace=false).
      firstEntryStmts = clSnoc(stmts, traceEnter as unknown as J.JStmt);
    } else {
      firstEntryStmts = stmts;
    }
  } else {
    if (showStackTrace) {
      firstEntryStmts = clCons(traceEnter as unknown as J.JStmt, copyFormalsToArgs);
    } else if (noRealArgs) {
      firstEntryStmts = clEmpty;
    } else {
      firstEntryStmts = copyFormalsToArgs;
    }
  }

  const isActivationRecordCall = rtMethod('isActivationRecord', clist<J.JExprT>(jId(firstArg)));
  const preambleStmts: CList<J.JStmt> =
    (localCompiler.options.shouldProfile
      ? clSing<J.JStmt>(jExpr(rtMethod('profileEnter', clist(localCompiler.getLoc(l)))))
      : clEmpty)
      .append(isFlat
        ? firstEntryStmts
        : clSing<J.JStmt>(firstEntryStmts.isEmpty()
          ? jIf1(isActivationRecordCall, restorer)
          : jIf(isActivationRecordCall, restorer, jBlock(firstEntryStmts))));

  const stackAttachGuard: J.JExprT =
    compiler.options.properTailCalls
      ? jBinop(jId(step), jNeq, retLabel)
      : jTrue;

  // If we exit the while loop, we must have used "break" because of a continuation
  const afterLoop: CList<J.JStmt> =
    isFlat
      ? clist<J.JStmt>(jReturn(jId(localCompiler.curAns)))
      : clist<J.JStmt>(
        jIf1(stackAttachGuard,
          jBlock(clist<J.JStmt>(
            jExpr(jBracketAssign(jDot(jId(localCompiler.curAns), 'stack'),
              jUnop(rtField('EXN_STACKHEIGHT'), J.jPostincr), actRecord))
          ))),
        jReturn(jId(localCompiler.curAns)));

  const checkCont = jUnop(rtMethod('isContinuation', clist<J.JExprT>(jId(localCompiler.curAns))), jNot);
  const gasCheck = jIf1(
    jBinop(jBinop(jUnop(rtField('GAS'), jDecr), J.jLeq, jNum(0)),
      J.jOr,
      jBinop(jUnop(rtField('RUNGAS'), jDecr), J.jLeq, jNum(0))),
    jBlock(clist<J.JStmt>(jExpr(jDotAssign(RUNTIME, 'EXN_STACKHEIGHT', jNum(0))),
      jExpr(jAssign(localCompiler.curAns, rtMethod('makeCont', clEmpty))))));

  const gasCheckOrComment: CList<J.JStmt> = isFlat
    ? clSing<J.JStmt>(jExpr(jRawCode('// callee optimization')))
    : clSing<J.JStmt>(gasCheck);
  const funBody =
    clAppend(
      clAppend(preambleStmts, gasCheckOrComment),
      clSing<J.JStmt>(jWhile(checkCont,
        jBlock(clist<J.JStmt>(
          jSwitch(jId(step), switchCases))))));

  return jBlock(
    clAppend(
      clAppend(clist<J.JStmt>(
        jVar(step, jNum(0)),
        jVar(elidedFrames, jNum(0)),
        jVar(localCompiler.curAns, UNDEFINED),
        jVar(apploc, localCompiler.getLoc(l))),
      funBody),
      afterLoop));
}

export function compileAnns(
  visitor: CompilerVisitor,
  step: A.Name,
  binds: N.ABind[],
  entryLabel: J.JExprT
): { newCases: CList<J.JCaseT>; newLabel: J.JExprT } {
  let curTarget = entryLabel;
  let newCases: CList<J.JCaseT> = clEmpty;
  for (const b of binds) {
    if (A.isABlank(b.ann) || A.isAAny(b.ann)) {
      // acc unchanged
    } else if (A.isATuple(b.ann) && b.ann.fields.every((a) => A.isABlank(a) || A.isAAny(a))) {
      const newLabel = visitor.makeLabel();
      const newCase =
        jCase(curTarget,
          jBlock(
            clist<J.JStmt>(
              jExpr(jAssign(step, newLabel)),
              jExpr(jAssign(visitor.curApploc, visitor.getLoc(b.ann.l))),
              jExpr(rtMethod('checkTupleBind', clist(jId(jsIdOf(b.id)), jNum(b.ann.fields.length),
                visitor.getLoc(b.ann.l)))),
              jBreak
            )));
      curTarget = newLabel;
      newCases = clSnoc(newCases, newCase);
    } else if (isFlatEnough(FL.annFlatness(b.ann, visitor.flatnessEnv, visitor.typeFlatnessEnv, visitor.moduleBindings, visitor.env))) {
      const compiledAnn = compileAnn(b.ann, undefined, visitor);
      const newLabel = visitor.makeLabel();
      const newCase = jCase(curTarget,
        jBlock(clAppend(compiledAnn.otherStmts,
          clist<J.JStmt>(
            jExpr(jAssign(step, newLabel)),
            jExpr(jAssign(visitor.curApploc, visitor.getLoc((b.ann as any).l))),
            jExpr(rtMethod('_checkAnn',
              clist(visitor.getLoc((b.ann as any).l), compiledAnn.exp, jId(jsIdOf(b.id))))),
            jBreak))));
      curTarget = newLabel;
      newCases = clSnoc(newCases, newCase);
    } else {
      const annResult = freshId(compilerName('ann-check'));
      const compiledAnn = compileAnn(b.ann, undefined, visitor);
      const newLabel = visitor.makeLabel();
      const newCase = jCase(curTarget,
        jBlock(clAppend(compiledAnn.otherStmts,
          clist<J.JStmt>(
            jExpr(jAssign(step, newLabel)),
            jExpr(jAssign(visitor.curApploc, visitor.getLoc((b.ann as any).l))),
            jVar(annResult, rtMethod('_checkAnn',
              clist(visitor.getLoc((b.ann as any).l), compiledAnn.exp, jId(jsIdOf(b.id))))),
            jIf1(rtMethod('isContinuation', clist<J.JExprT>(jId(annResult))),
              jBlock(clist<J.JStmt>(
                jExpr(jAssign(visitor.curAns, jId(annResult)))))),
            jBreak))));
      curTarget = newLabel;
      newCases = clSnoc(newCases, newCase);
    }
  }
  return { newCases: newCases, newLabel: curTarget };
}

export function compileAnnotatedLet(
  visitor: CompilerVisitor,
  b: BindType,
  compiledE: DAG.CExp,
  compiledBody: DAG.CBlock
): DAG.CBlock {
  let idAssign: CList<J.JStmt>;
  if (isBLet(b)) {
    idAssign = clSing<J.JStmt>(jVar(jsIdOf(b.value.id), compiledE.exp));
  } else if (isBArray(b)) {
    idAssign = clSing<J.JStmt>(jExpr(jBracketAssign(jId(jsIdOf(b.value.id)), jNum(b.idx), compiledE.exp)));
  } else {
    return raise('Unknown ' + (b as any).value.label() + ' in compile-annotated-let');
  }
  const bind = b.value;
  if (A.isABlank(bind.ann) || A.isAAny(bind.ann)) {
    return cBlock(
      jBlock(
        clAppend(
          clAppend(
            compiledE.otherStmts,
            idAssign),
          DAG.stmtsOf(compiledBody.block))
      ),
      compiledBody.newCases
    );
  } else if (A.isATuple(bind.ann) && bind.ann.fields.every((a) => A.isABlank(a) || A.isAAny(a))) {
    const step = visitor.curStep;
    const afterAnn = visitor.makeLabel();
    const afterAnnCase = jCase(afterAnn, jBlock(DAG.stmtsOf(compiledBody.block)));
    return cBlock(
      jBlock(
        clAppend(compiledE.otherStmts,
          clAppend(idAssign,
            clist<J.JStmt>(
              jExpr(jAssign(step, afterAnn)),
              jExpr(jAssign(visitor.curApploc, visitor.getLoc(bind.ann.l))),
              jExpr(rtMethod('checkTupleBind', clist(jId(jsIdOf(bind.id)), jNum(bind.ann.fields.length),
                visitor.getLoc(bind.ann.l)))),
              jBreak
            )))),
      clCons(afterAnnCase, compiledBody.newCases));
  } else {
    const step = visitor.curStep;
    const afterAnn = visitor.makeLabel();
    const afterAnnCase = jCase(afterAnn, jBlock(DAG.stmtsOf(compiledBody.block)));
    const compiledAnn = compileAnn(bind.ann, undefined, visitor);
    const annResult = freshId(compilerName('ann-check'));
    return cBlock(
      jBlock(
        clAppend(
          clAppend(
            clAppend(compiledE.otherStmts, idAssign),
            compiledAnn.otherStmts),
          clist<J.JStmt>(
            jExpr(jAssign(step, afterAnn)),
            jExpr(jAssign(visitor.curApploc, visitor.getLoc((bind.ann as any).l))),
            jVar(annResult, rtMethod('_checkAnn',
              clist(visitor.getLoc((bind.ann as any).l), compiledAnn.exp, jId(jsIdOf(bind.id))))),
            jIf1(rtMethod('isContinuation', clist<J.JExprT>(jId(annResult))),
              jBlock(clist<J.JStmt>(
                jExpr(jAssign(visitor.curAns, jId(annResult)))))),
            jBreak
          ))),
      clCons(afterAnnCase, compiledBody.newCases));
  }
}

/*
  The original recursive implementation compiled each head by doing some
  work, compiling the entire rest of the chain, and then wrapping the
  compiled rest -- one stack frame per statement. Here each head's
  compilation is split at exactly that point: a "pre" phase (the work
  done before the rest -- visiting operand values, minting labels and
  fresh ids) run in a forward loop, and a "post" phase (the work done
  after -- wrapping the destination binding, assembling the block and
  case list) folded backward from the compiled tail. The effect order
  (jsIdOf/freshId minting, makeLabel, getLoc interning, dispatch-table
  pushes) is kept exactly the same, so the generated code is identical.
*/
type HeadPost = (rest: DAG.CBlock) => DAG.CBlock;

export function compileAExpr(compiler: CompilerVisitor, ae: N.AExpr): DAG.CBlock {
  const posts: HeadPost[] = [];
  for (const head of ae.heads) {
    posts.push(preHead(compiler, head));
  }
  let cur = compileTailLettable(compiler, ae.e);
  for (let i = posts.length - 1; i >= 0; i--) {
    cur = posts[i](cur);
  }
  return cur;
}

function preHead(compiler: CompilerVisitor, head: N.AExprHead): HeadPost {
  switch (head.$name) {
    case 'a-let':
      return preLettable(compiler, new BLet(head.bind), head.e);
    case 'a-arr-let':
      return preLettable(compiler, new BArray(head.bind, head.idx), head.e);
    case 'a-seq':
      return preLettable(compiler, undefined, head.e1);
    case 'a-var': {
      const node = head;
      // The value is visited after the rest of the chain is compiled,
      // as in the recursive formulation.
      return (compiledBody) => {
        const compiledE: DAG.CExp = node.e.visit(compiler);
        // TODO: annotations here?
        return cBlock(
          jBlock(
            clCons(
              jVar(jsIdOf(node.bind.id),
                jObj(clist<J.JFieldT>(jField('$var', compiledE.exp)
                  // NOTE(joe): This can be useful to turn on for debugging
                  //                     , j-field("$name", j-str(b.id.toname()))
                ))) as J.JStmt,
              DAG.stmtsOf(compiledBody.block))),
          compiledBody.newCases);
      };
    }
    case 'a-type-let': {
      const bind = head.bind;
      switch (bind.$name) {
        case 'a-type-bind':
          return (visitedBody) => {
            const compiledAnn = compileAnn(bind.ann, bind.name.toname(), compiler);
            return cBlock(
              jBlock(
                clAppend(
                  clSnoc(compiledAnn.otherStmts, jVar(jsIdOf(bind.name), compiledAnn.exp) as J.JStmt),
                  DAG.stmtsOf(visitedBody.block))),
              visitedBody.newCases);
          };
        case 'a-newtype-bind': {
          const branderId = jsIdOf(bind.namet);
          return (visitedBody) => cBlock(
            jBlock(
              clAppend(
                clist<J.JStmt>(
                  jVar(branderId, rtMethod('namedBrander', clist<J.JExprT>(jStr(bind.name.toname()), compiler.getLoc(bind.l)))),
                  jVar(jsIdOf(bind.name), rtMethod('makeBranderAnn', clist<J.JExprT>(jId(branderId), jStr(bind.name.toname()))))
                ),
                DAG.stmtsOf(visitedBody.block))),
            visitedBody.newCases);
        }
        default:
          throw new InternalCompilerError('Unknown ATypeBind in a-type-let');
      }
    }
    default:
      throw new InternalCompilerError('Unknown AExprHead in preHead: ' + (head as any).$name);
  }
}

// The "rest of the chain follows this head" bookkeeping the recursive
// formulation did in getNewCases/getRemainingCode: the pre-body label is
// minted in the head's pre phase; once the rest is compiled, the
// destination binding (if any) is wrapped around it and it becomes the
// case the head's code jumps to.
function finishRest(
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  preBodyLabel: J.JExprT,
  rest: DAG.CBlock
): [CList<J.JCaseT>, J.JExprT] {
  let compiledBody = rest;
  if (optDest !== undefined) {
    compiledBody = compileAnnotatedLet(compiler, optDest, cExp(jId(compiler.curAns), clEmpty), compiledBody);
  }
  return [clCons(jCase(preBodyLabel, compiledBody.block) as J.JCaseT, compiledBody.newCases), preBodyLabel];
}

function plainPost(compiler: CompilerVisitor, b: BindType | undefined, compiledE: DAG.CExp): HeadPost {
  if (b !== undefined) {
    return (rest) => compileAnnotatedLet(compiler, b, compiledE, rest);
  }
  return (rest) => {
    const firstStmt: J.JStmt = (compiledE.exp as any) instanceof J.JStmtBase ? (compiledE.exp as any as J.JStmt) : jExpr(compiledE.exp);
    return cBlock(
      jBlock(clAppend(compiledE.otherStmts, clCons(firstStmt, DAG.stmtsOf(rest.block)))),
      rest.newCases);
  };
}

function preLettable(compiler: CompilerVisitor, b: BindType | undefined, e: N.ALettable): HeadPost {
  switch (e.$name) {
    case 'a-prim-app':
      return e.appInfo.needsStep
        ? preSplitPrimApp(e.l, compiler, b, e.f, e.args)
        : preFlatPrimApp(e.l, compiler, b, e.f, e.args);
    case 'a-app': {
      const f = e._fun;
      const isSafeId = N.isAId(f) || N.isAIdSafeLetrec(f);
      if (isSafeId && isFunctionFlat(compiler.flatnessEnv, (f as any).id.key())) {
        return preFlatApp(e.l, compiler, b, f, e.args);
      }
      const isFn = isSafeId && isIdFnName(compiler.flatnessEnv, (f as any).id.key());
      return preSplitApp(e.l, compiler, b, f, e.args, e.appInfo, isFn);
    }
    case 'a-method-app':
      return preSplitMethodApp(e.l, compiler, b, e.obj, e.meth, e.args);
    case 'a-if':
      return preSplitIf(compiler, b, e);
    case 'a-cases':
      return preSplitCases(compiler, e.l, b, e.val, e.branches, e._else);
    case 'a-update':
      return preSplitUpdate(compiler, e.l, b, e.supe, e.fields);
    case 'a-lam': {
      const compiledE = compileALam(compiler, e.l, e.name, e.args, e.ret, e.body, b);
      return plainPost(compiler, b, compiledE);
    }
    default: {
      const compiledE: DAG.CExp = e.visit(compiler);
      return plainPost(compiler, b, compiledE);
    }
  }
}

function compileTailLettable(compiler: CompilerVisitor, e: N.ALettable): DAG.CBlock {
  switch (e.$name) {
    case 'a-prim-app': {
      if (e.appInfo.needsStep) {
        const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), e.args);
        return buildSplitPrimApp(e.l, compiler, e.f, compiledArgs, clEmpty, compiler.curTarget);
      }
      const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), e.args);
      return buildFlatPrimApp(e.l, compiler, e.f, compiledArgs, tailRemainingBlock(compiler), clEmpty);
    }
    case 'a-app': {
      const f = e._fun;
      const isSafeId = N.isAId(f) || N.isAIdSafeLetrec(f);
      if (isSafeId && isFunctionFlat(compiler.flatnessEnv, (f as any).id.key())) {
        const compiledF = field(f.visit(compiler), 'exp');
        const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), e.args);
        return buildFlatApp(e.l, compiler, compiledF, compiledArgs, tailRemainingBlock(compiler), clEmpty);
      }
      const isFn = isSafeId && isIdFnName(compiler.flatnessEnv, (f as any).id.key());
      const compiledF = field(f.visit(compiler), 'exp');
      const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), e.args);
      return buildSplitApp(e.l, compiler, compiledF, compiledArgs, e.appInfo, isFn, clEmpty, compiler.curTarget);
    }
    case 'a-method-app': {
      const state = methodAppState(e.l, compiler, e.obj, e.meth, e.args);
      return buildMethodApp(e.l, compiler, state, clEmpty, compiler.curTarget);
    }
    case 'a-if': {
      const consqLabel = compiler.makeLabel();
      const altLabel = compiler.makeLabel();
      return buildNaryIf(compiler, e, consqLabel, altLabel, clEmpty, compiler.curTarget);
    }
    case 'a-cases': {
      const compiledVal = field(e.val.visit(compiler), 'exp');
      return buildSplitCases(compiler, e.l, compiledVal, e.branches, e._else, clEmpty, compiler.curTarget);
    }
    case 'a-update': {
      const state = updateState(compiler, e.supe, e.fields);
      return buildSplitUpdate(compiler, e.l, state, clEmpty, compiler.curTarget);
    }
    case 'a-lam': {
      const compiledE = compileALam(compiler, e.l, e.name, e.args, e.ret, e.body, undefined);
      return tailPlain(compiler, compiledE);
    }
    default: {
      const compiledE: DAG.CExp = e.visit(compiler);
      return tailPlain(compiler, compiledE);
    }
  }
}

function tailPlain(compiler: CompilerVisitor, visitE: DAG.CExp): DAG.CBlock {
  return cBlock(
    jBlock(
      clAppend(
        clAppend(
          clSing<J.JStmt>(jExpr(jAssign(compiler.curStep, compiler.curTarget))),
          visitE.otherStmts),
        clist<J.JStmt>(
          jExpr(jAssign(compiler.curAns, visitE.exp)),
          jBreak))),
    clEmpty);
}

function tailRemainingBlock(compiler: CompilerVisitor): J.JBlockT {
  return jBlock(clist<J.JStmt>(
    jExpr(jAssign(compiler.curStep, compiler.curTarget)),
    jBreak
  ));
}

// ---------- a-app ----------

function preSplitApp(
  l: Loc,
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  f: N.AVal,
  args: N.AVal[],
  appInfo: A.AppInfo,
  isDefinitelyFn: boolean
): HeadPost {
  const compiledF = field(f.visit(compiler), 'exp');
  const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), args);
  const preBodyLabel = compiler.makeLabel();
  return (rest) => {
    const [newCases, afterAppLabel] = finishRest(compiler, optDest, preBodyLabel, rest);
    return buildSplitApp(l, compiler, compiledF, compiledArgs, appInfo, isDefinitelyFn, newCases, afterAppLabel);
  };
}

function buildSplitApp(
  l: Loc,
  compiler: CompilerVisitor,
  compiledF: J.JExprT,
  compiledArgs: CList<J.JExprT>,
  appInfo: A.AppInfo,
  isDefinitelyFn: boolean,
  newCases: CList<J.JCaseT>,
  afterAppLabel: J.JExprT
): DAG.CBlock {
  const ans = compiler.curAns;
  const step = compiler.curStep;
  if (appInfo.isRecursive &&
    appInfo.isTail &&
    compiler.allowTco &&
    compiler.options.properTailCalls &&
    (compiledArgs.length() === compiler.args.length)) {
    // if it's an arity mismatch, use non-TCO to handle the error
    const argsList = map2((name: A.Name, exp: J.JExprT) => jAssign(name, exp) as J.JExprT, compiler.args, compiledArgs.toList());
    const [pre, post] = getAssignments(argsList, argsList.length);
    return cBlock(
      jBlock(
        clist<J.JStmt>(
          // Update step before the call, so that if it runs out of gas,
          // the resumer goes to the right step
          jExpr(jAssign(step, jNum(0))),
          jExpr(jUnop(jId(compiler.elidedFrames), jIncr)),
          jIf1(jBinop(jUnop(rtField('RUNGAS'), jDecr), J.jLeq, jNum(0)),
            jBlock(clist<J.JStmt>(
              jExpr(jDotAssign(RUNTIME, 'EXN_STACKHEIGHT', jNum(0))),
              jExpr(jAssign(ans, rtMethod('makeCont', clEmpty)))))))
          .append(CL.from_list<J.JStmt>([...pre, ...post]))
          .append(clSing<J.JStmt>(jContinue))),
      newCases);
  } else {
    return cBlock(
      jBlock(
        // Update step before the call, so that if it runs out of gas,
        // the resumer goes to the right step
        clist<J.JStmt>(
          jExpr(jAssign(step, afterAppLabel)),
          jExpr(jAssign(compiler.curApploc, compiler.getLoc(l))))
          .append(!isDefinitelyFn
            ? clSing<J.JStmt>(checkFun(l, jId(compiler.curApploc), compiledF))
            : clSing<J.JStmt>(jExpr(jRawCode('// omitting isFunction check'))))
          .append(clist<J.JStmt>(
            jExpr(wrapWithSrcnode(l, jAssign(ans, app(l, compiledF, compiledArgs)))),
            jBreak))),
      newCases);
  }
}

export function jBlockToStmtList(b: J.JBlockT): CList<J.JStmt> {
  switch (b.$name) {
    case 'j-block': return b.stmts;
    case 'j-block1': return clSing(b.stmt);
    default:
      throw new InternalCompilerError('Unknown JBlock in j-block-to-stmt-list');
  }
}

function preFlatApp(
  l: Loc,
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  f: N.AVal,
  args: N.AVal[]
): HeadPost {
  const compiledF = field(f.visit(compiler), 'exp');
  const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), args);
  return (rest) => {
    let compiledBody = rest;
    if (optDest !== undefined) {
      compiledBody = compileAnnotatedLet(compiler, optDest, cExp(jId(compiler.curAns), clEmpty), compiledBody);
    }
    return buildFlatApp(l, compiler, compiledF, compiledArgs, compiledBody.block, compiledBody.newCases);
  };
}

function buildFlatApp(
  l: Loc,
  compiler: CompilerVisitor,
  compiledF: J.JExprT,
  compiledArgs: CList<J.JExprT>,
  remainingCode: J.JBlockT,
  newCases: CList<J.JCaseT>
): DAG.CBlock {
  const ans = compiler.curAns;
  // Generate the code for calling the function
  const callCode = clist<J.JStmt>(
    jExpr(jRawCode('// caller optimization')),
    jExpr(wrapWithSrcnode(l, jAssign(ans, app(l, compiledF, compiledArgs))))
  );
  // Merge the code for calling the function with the next block
  // (this is basically our optimization, since we're not starting a new case
  // for the next block)
  return cBlock(
    jBlock(clAppend(callCode, jBlockToStmtList(remainingCode))),
    newCases);
}

// ---------- a-prim-app ----------

function preSplitPrimApp(
  l: Loc,
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  f: string,
  args: N.AVal[]
): HeadPost {
  const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), args);
  const preBodyLabel = compiler.makeLabel();
  return (rest) => {
    const [newCases, afterAppLabel] = finishRest(compiler, optDest, preBodyLabel, rest);
    return buildSplitPrimApp(l, compiler, f, compiledArgs, newCases, afterAppLabel);
  };
}

function buildSplitPrimApp(
  l: Loc,
  compiler: CompilerVisitor,
  f: string,
  compiledArgs: CList<J.JExprT>,
  newCases: CList<J.JCaseT>,
  afterAppLabel: J.JExprT
): DAG.CBlock {
  const ans = compiler.curAns;
  const step = compiler.curStep;
  return cBlock(
    jBlock(
      // Update step before the call, so that if it runs out of gas,
      // the resumer goes to the right step
      clist<J.JStmt>(
        jExpr(jAssign(step, afterAppLabel)),
        jExpr(jAssign(compiler.curApploc, compiler.getLoc(l))))
        .append(clist<J.JStmt>(
          jExpr(wrapWithSrcnode(l, jAssign(ans, rtMethod(f, compiledArgs)))),
          jBreak))),
    newCases);
}

function preFlatPrimApp(
  l: Loc,
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  f: string,
  args: N.AVal[]
): HeadPost {
  const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), args);
  return (rest) => {
    let compiledBody = rest;
    if (optDest !== undefined) {
      compiledBody = compileAnnotatedLet(compiler, optDest, cExp(jId(compiler.curAns), clEmpty), compiledBody);
    }
    return buildFlatPrimApp(l, compiler, f, compiledArgs, compiledBody.block, compiledBody.newCases);
  };
}

function buildFlatPrimApp(
  l: Loc,
  compiler: CompilerVisitor,
  f: string,
  compiledArgs: CList<J.JExprT>,
  remainingCode: J.JBlockT,
  newCases: CList<J.JCaseT>
): DAG.CBlock {
  const ans = compiler.curAns;
  // Generate the code for calling the function
  const callCode = jExpr(wrapWithSrcnode(l, jAssign(ans, rtMethod(f, compiledArgs))));
  // Merge the code for calling the function with the next block
  return cBlock(
    jBlock(clCons(callCode as J.JStmt, jBlockToStmtList(remainingCode))),
    newCases);
}

// ---------- a-method-app ----------

interface MethodAppState {
  compiledObj: J.JExprT;
  compiledArgs: CList<J.JExprT>;
  // isJId(compiledObj) path: the whole call is built (interning its loc)
  // before the rest of the chain is compiled, as in the recursive
  // formulation.
  call: J.JExprT | undefined;
  objId: J.JId | undefined;
  colonField: J.JExprT | undefined;
  colonFieldId: J.JId | undefined;
  checkMethod: J.JExprT | undefined;
}

function methodAppState(
  l: Loc,
  compiler: CompilerVisitor,
  obj: N.AVal,
  methname: string,
  args: N.AVal[]
): MethodAppState {
  const compiledObj = field(obj.visit(compiler), 'exp');
  const compiledArgs = CL.map_list((a: N.AVal) => field(a.visit(compiler), 'exp'), args);
  const argcount = compiledArgs.length();
  const helperName = argcount <= 7 ? 'maybeMethodCall' + String(argcount) : 'maybeMethodCall';
  if (J.isJId(compiledObj)) {
    const call = wrapWithSrcnode(l,
      rtMethod(helperName,
        clAppend(clist<J.JExprT>(compiledObj,
          jStr(methname),
          compiler.getLoc(l)),
        compiledArgs)));
    return { compiledObj, compiledArgs, call, objId: undefined, colonField: undefined, colonFieldId: undefined, checkMethod: undefined };
  } else {
    const objId = jId(freshId(compilerName('obj')));
    const colonField = rtMethod('getColonFieldLoc', clist(objId, jStr(methname), compiler.getLoc(l)));
    const colonFieldId = jId(freshId(compilerName('field')));
    const checkMethod = rtMethod('isMethod', clist<J.JExprT>(colonFieldId));
    return { compiledObj, compiledArgs, call: undefined, objId, colonField, colonFieldId, checkMethod };
  }
}

function preSplitMethodApp(
  l: Loc,
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  obj: N.AVal,
  methname: string,
  args: N.AVal[]
): HeadPost {
  const state = methodAppState(l, compiler, obj, methname, args);
  const preBodyLabel = compiler.makeLabel();
  return (rest) => {
    const [newCases, afterAppLabel] = finishRest(compiler, optDest, preBodyLabel, rest);
    return buildMethodApp(l, compiler, state, newCases, afterAppLabel);
  };
}

function buildMethodApp(
  l: Loc,
  compiler: CompilerVisitor,
  state: MethodAppState,
  newCases: CList<J.JCaseT>,
  afterAppLabel: J.JExprT
): DAG.CBlock {
  const ans = compiler.curAns;
  const step = compiler.curStep;
  if (state.objId === undefined) {
    return cBlock(jBlock(clist<J.JStmt>(
      jExpr(jAssign(step, afterAppLabel)),
      jExpr(jAssign(ans, state.call!)),
      jBreak
    )), newCases);
  } else {
    const objId = state.objId;
    const colonFieldId = state.colonFieldId!;
    return cBlock(
      jBlock(clist<J.JStmt>(
        // Update step before the call, so that if it runs out of gas, the resumer goes to the right step
        jExpr(jAssign(step, afterAppLabel)),
        jExpr(jAssign(compiler.curApploc, compiler.getLoc(l))),
        jVar(objId.id, state.compiledObj),
        jVar(colonFieldId.id, state.colonField!),
        jIf(state.checkMethod!, jBlock(clist<J.JStmt>(
          jExpr(jAssign(ans, jApp(jDot(colonFieldId, 'full_meth'),
            clCons<J.JExprT>(objId, state.compiledArgs))))
        )),
        jBlock(clist<J.JStmt>(
          checkFun(l, compiler.getLoc(l), colonFieldId),
          jExpr(wrapWithSrcnode(l, jAssign(ans, app(l, colonFieldId, state.compiledArgs))))
        ))),
        // If the answer is a cont, jump to the end of the current function
        // rather than continuing normally
        jBreak)),
      newCases);
  }
}

export function isIdOccurs(target: A.Name, e: J.JExprT): boolean {
  // Returns true iff `target` occurs in `e`
  const dummyJsExpr = jNum(0);
  let found = false;
  const visitor = ext(J.defaultMapVisitor as any, {
    jId(node: J.JId): any {
      if (node.id.key() === target.key()) {
        found = true;
      }
      return dummyJsExpr;
    },
  });
  e.visit(visitor);
  return found;
}

export function getAssignments(lst: J.JExprT[], limit: number): [J.JStmt[], J.JStmt[]] {
  /*
     Find an order of assignment statements in `lst` that avoid new variables
     where `limit` is the number of round-robin attempts allowed.

     When the dependency graph is acyclic, this algorithm degenerates to
     finding a topological order.

     If the RHS of assignment statements have at most one identifier,
     it's possible that the corresponding dependency graph will have cycles,
     but there can be at most one cycle per connected component. Thus, this
     algorithm degenerates to finding topological order at most twice per
     component (one for ordering non-cycle parts, then we break the cycle
     and then another one for the ordering the rest). It guarantees that
     it will reach limit = 0 at most once per each component.

     The output is a pair of `pre` and `post` which are lists of
     assignments. The order of `post` doesn't matter.
  */
  if (lst.length === 0) {
    return [[], []];
  }
  const asgn = lst[0];
  const rest = lst.slice(1);
  if (!isJAssign(asgn)) {
    throw new InternalCompilerError('Non j-assign in get-assignments: ' + (asgn as any).$name);
  }
  if (limit === 0) {
    const tmpArg = freshId(compilerName('tmp_asgn'));
    const [pre, post] = getAssignments(rest, rest.length);
    return [
      [jVar(tmpArg, asgn.rhs), ...pre],
      [jExpr(jAssign(asgn.name, jId(tmpArg))), ...post],
    ];
  } else {
    const occursAny = rest.some((nextAsgn) => isIdOccurs(asgn.name, (nextAsgn as J.JAssign).rhs));
    if (occursAny) {
      return getAssignments([...rest, asgn], limit - 1);
    } else {
      const [pre, post] = getAssignments(rest, rest.length);
      return [[jExpr(asgn), ...pre], post];
    }
  }
}

// ---------- a-if ----------

function preSplitIf(
  compiler: CompilerVisitor,
  optDest: BindType | undefined,
  node: N.AIf
): HeadPost {
  const consqLabel = compiler.makeLabel();
  const altLabel = compiler.makeLabel();
  const preBodyLabel = compiler.makeLabel();
  return (rest) => {
    const [afterIfCases, afterIfLabel] = finishRest(compiler, optDest, preBodyLabel, rest);
    return buildNaryIf(compiler, node, consqLabel, altLabel, afterIfCases, afterIfLabel);
  };
}

/*
  The flattened n-ary a-if, compiled exactly as the nested binary chain
  was: arm 1's dispatch block tests c1 and jumps to its body's case or to
  the alt case holding arm 2's test-computation heads followed by arm 2's
  dispatch, and so on; the else is the innermost alt. Forward over the
  arms: each arm's test-head pre phases, its two labels, then its
  compiled body. Backward: each arm's condition value is visited (the
  recursive formulation visited it after compiling the whole alt chain --
  this is where anf_if temporaries get their JS names, so the reverse-arm
  minting order is preserved), the dispatch block is assembled, and the
  arm's test-head posts wrap it to become the next-outer arm's alt.
*/
function buildNaryIf(
  compiler: CompilerVisitor,
  node: N.AIf,
  consqLabel1: J.JExprT,
  altLabel1: J.JExprT,
  afterIfCases: CList<J.JCaseT>,
  afterIfLabel: J.JExprT
): DAG.CBlock {
  const compilerAfterIf = ext(compiler, { curTarget: afterIfLabel });
  const branches = node.branches;
  const n = branches.length;
  const consqLabels: J.JExprT[] = [consqLabel1];
  const altLabels: J.JExprT[] = [altLabel1];
  const compiledBodies: DAG.CBlock[] = [];
  const headPosts: HeadPost[][] = [];
  for (let i = 0; i < n; i++) {
    const b = branches[i];
    const posts: HeadPost[] = [];
    for (const h of b.heads) {
      posts.push(preHead(compilerAfterIf, h));
    }
    headPosts.push(posts);
    if (i > 0) {
      consqLabels.push(compiler.makeLabel());
      altLabels.push(compiler.makeLabel());
    }
    compiledBodies.push(compileAExpr(compilerAfterIf, b.body));
  }
  let cur = compileAExpr(compilerAfterIf, node.elseBody);
  for (let i = n - 1; i >= 0; i--) {
    // The nested chain visited arm 1's condition with the enclosing
    // compiler and inner arms' with the after-if one; they only differ
    // in curTarget, which value visits never read -- match it anyway.
    const condCompiler = i === 0 ? compiler : compilerAfterIf;
    const condExp = field(branches[i].test.visit(condCompiler), 'exp');
    const newCases =
      clAppend(
        clAppend(
          clCons(jCase(consqLabels[i], compiledBodies[i].block) as J.JCaseT, compiledBodies[i].newCases),
          clCons(jCase(altLabels[i], cur.block) as J.JCaseT, cur.newCases)),
        i === 0 ? afterIfCases : clEmpty);
    cur = cBlock(
      jBlock(clist<J.JStmt>(
        jExpr(jAssign(compiler.curStep,
          jTernary(rtMethod('checkPyretTrue', clist(condExp)),
            consqLabels[i], altLabels[i]))),
        jBreak
      )),
      newCases);
    const posts = headPosts[i];
    for (let j = posts.length - 1; j >= 0; j--) {
      cur = posts[j](cur);
    }
  }
  return cur;
}

// ---------- a-cases ----------

export function compileCasesBranch(
  compiler: CompilerVisitor,
  compiledVal: J.JExprT,
  branch: N.ACasesBranch,
  casesLoc: Loc
): DAG.CBlock {
  const compiledBody = compileAExpr(compiler, branch.body);
  if (compiledBody.newCases.length() < compiler.options.inlineCaseBodyLimit) {
    return compileInlineCasesBranch(compiler, compiledVal, branch, compiledBody, casesLoc);
  } else {
    const tempBranch = freshId(compilerName('temp_branch'));
    const branchArgs: N.ABind[] =
      N.isACasesBranch(branch) && branch.args.length > 0
        ? branch.args.map(getBind)
        : [new N.ABind(branch.body.l, compiler.resumer, A.aBlank)];
    const step = freshId(compilerName('step'));
    const refBindsMask: J.JExprT = N.isACasesBranch(branch)
      ? jList(false, CL.map_list((cb: N.ACasesBind) => jBool(A.isSCasesBindRef(cb.fieldType)), branch.args))
      : jList(false, clEmpty);
    const compiledBranchFun =
      compileFunBody(branch.body.l, step, tempBranch,
        ext(compiler, { allowTco: false, options: { ...compiler.options, shouldProfile: false } }),
        branchArgs, undefined, branch.body, true, false, false);
    const preamble = casesPreamble(compiler, compiledVal, branch, casesLoc);
    const derefFields = jExpr(jAssign(compiler.curAns, jMethod(compiledVal, '$app_fields', clist<J.JExprT>(jId(tempBranch), refBindsMask))));
    const actualApp = clist<J.JStmt>(
      jExpr(jAssign(compiler.curStep, compiler.curTarget)),
      jExpr(jAssign(compiler.curApploc, compiler.getLoc(branch.l))),
      jVar(tempBranch,
        jFun(J.nextJFunId(), makeFunName(compiler, casesLoc),
          CL.map_list((arg: N.ABind) => formalShadowName(arg.id), branchArgs), compiledBranchFun)),
      derefFields,
      jBreak);

    return cBlock(
      jBlock(clAppend(preamble, actualApp)),
      clEmpty);
  }
}

export function casesPreamble(
  compiler: CompilerVisitor,
  compiledVal: J.JExprT,
  branch: N.ACasesBranch,
  casesLoc: Loc
): CList<J.JStmt> {
  const constructorLoc = jDot(compiledVal, '$loc');
  switch (branch.$name) {
    case 'a-cases-branch': {
      const branchGivenArity = jNum(branch.args.length);
      const objExpectedArity = jDot(compiledVal, '$arity');
      const checker =
        jIf1(jBinop(objExpectedArity, jNeq, branchGivenArity),
          jBlock1(
            jIf(jBinop(objExpectedArity, jGeq, jNum(0)),
              jBlock1(
                jExpr(jMethod(rtField('ffi'), 'throwCasesArityErrorC',
                  clist(compiler.getLoc(branch.l), branchGivenArity,
                    objExpectedArity, compiler.getLoc(casesLoc), constructorLoc)))),
              jBlock1(
                jExpr(jMethod(rtField('ffi'), 'throwCasesSingletonErrorC',
                  clist(compiler.getLoc(branch.l), jTrue, compiler.getLoc(casesLoc), constructorLoc)))))));
      return clist<J.JStmt>(checker);
    }
    case 'a-singleton-cases-branch': {
      const checker =
        jIf1(jBinop(jDot(compiledVal, '$arity'), jNeq, jNum(-1)),
          jBlock1(
            jExpr(jMethod(rtField('ffi'), 'throwCasesSingletonErrorC',
              clist(compiler.getLoc(branch.l), jFalse, compiler.getLoc(casesLoc), constructorLoc)))));
      return clist<J.JStmt>(checker);
    }
    default:
      throw new InternalCompilerError('Unknown ACasesBranch in cases-preamble');
  }
}

export function compileInlineCasesBranch(
  compiler: CompilerVisitor,
  compiledVal: J.JExprT,
  branch: N.ACasesBranch,
  compiledBody: DAG.CBlock,
  casesLoc: Loc
): DAG.CBlock {
  const preamble = casesPreamble(compiler, compiledVal, branch, casesLoc);
  if (N.isACasesBranch(branch)) {
    const entryLabel = compiler.makeLabel();
    const annCases = compileAnns(compiler, compiler.curStep, branch.args.map(getBind), entryLabel);
    const fieldNames = jId(jsIdOf(freshId(compilerName('fn'))));
    const getFieldNames = jVar(fieldNames.id, jDot(jDot(compiledVal, '$constructor'), '$fieldNames'));
    const derefFields = CL.map_list_n((i: number, arg: N.ACasesBind) => {
      const mask = jBracket(jDot(compiledVal, '$mut_fields_mask'), jNum(i));
      const field = getDictField(compiledVal, jBracket(fieldNames, jNum(i)));
      return jVar(jsIdOf(arg.bind.id),
        rtMethod('derefField', clist(field, mask, jBool(A.isSCasesBindRef(arg.fieldType))))) as J.JStmt;
    }, 0, branch.args);
    if (annCases.newCases instanceof CL.ConcatEmpty) {
      return cBlock(jBlock(
        clAppend(
          clAppend(
            clSnoc(preamble, getFieldNames as J.JStmt),
            derefFields),
          DAG.stmtsOf(compiledBody.block))),
      compiledBody.newCases);
    } else {
      return cBlock(jBlock(
        clSnoc(
          clSnoc(
            clAppend(
              clSnoc(preamble, getFieldNames as J.JStmt),
              derefFields),
            jExpr(jAssign(compiler.curStep, entryLabel)) as J.JStmt),
          jBreak as J.JStmt)),
      clSnoc(
        clAppend(
          annCases.newCases,
          compiledBody.newCases),
        jCase(annCases.newLabel, compiledBody.block) as J.JCaseT));
    }
  } else {
    return cBlock(jBlock(clAppend(preamble, DAG.stmtsOf(compiledBody.block))), compiledBody.newCases);
  }
}

function preSplitCases(
  compiler: CompilerVisitor,
  casesLoc: Loc,
  optDest: BindType | undefined,
  val: N.AVal,
  branches: N.ACasesBranch[],
  _else: N.AExpr
): HeadPost {
  const compiledVal = field(val.visit(compiler), 'exp');
  const preBodyLabel = compiler.makeLabel();
  return (rest) => {
    const [afterCasesCases, afterCasesLabel] = finishRest(compiler, optDest, preBodyLabel, rest);
    return buildSplitCases(compiler, casesLoc, compiledVal, branches, _else, afterCasesCases, afterCasesLabel);
  };
}

function buildSplitCases(
  compiler: CompilerVisitor,
  casesLoc: Loc,
  compiledVal: J.JExprT,
  branches: N.ACasesBranch[],
  _else: N.AExpr,
  afterCasesCases: CList<J.JCaseT>,
  afterCasesLabel: J.JExprT
): DAG.CBlock {
  const compilerAfterCases = ext(compiler, { curTarget: afterCasesLabel });
  const compiledBranches: DAG.CBlock[] = [];
  for (const branch of branches) {
    compiledBranches.push(compileCasesBranch(compilerAfterCases, compiledVal, branch, casesLoc));
  }
  const compiledElse = compileAExpr(compilerAfterCases, _else);
  const branchLabels = branches.map(() => compiler.makeLabel());
  const elseLabel = compiler.makeLabel();
  let branchCases: CList<J.JCaseT> = clEmpty;
  for (let i = 0; i < branchLabels.length; i++) {
    branchCases = clAppend(
      clSnoc(branchCases, jCase(branchLabels[i], compiledBranches[i].block) as J.JCaseT),
      compiledBranches[i].newCases);
  }
  const branchElseCases =
    clAppend(
      clSnoc(branchCases, jCase(elseLabel, compiledElse.block) as J.JCaseT),
      compiledElse.newCases);
  const dispatchTable = jObj(CL.map_list2((branch: N.ACasesBranch, label: J.JExprT) => jField(branch.name, label) as J.JFieldT, branches, branchLabels));
  const dispatch = jId(freshId(compilerName('cases_dispatch')));
  compiler.dispatches.dispatches = clCons(jVar(dispatch.id, dispatchTable) as J.JStmt, compiler.dispatches.dispatches);
  // NOTE: Ignoring typ for the moment!
  const newCases = clAppend(branchElseCases, afterCasesCases);
  return cBlock(
    jBlock(clist<J.JStmt>(
      jExpr(jAssign(compiler.curApploc, compiler.getLoc(casesLoc))),
      jExpr(jAssign(compiler.curStep,
        jBinop(jBracket(dispatch, jDot(compiledVal, '$name')), J.jOr, elseLabel))),
      jBreak)),
    newCases);
}

// ---------- a-update ----------

interface UpdateState {
  compiledObj: J.JExprT;
  compiledFieldVals: CList<J.JExprT>;
  fieldNames: CList<J.JExprT>;
  fieldLocs: CList<J.JExprT>;
  objLoc: Loc;
}

function updateState(compiler: CompilerVisitor, obj: N.AVal, fields: N.AField[]): UpdateState {
  const compiledObj = field(obj.visit(compiler), 'exp');
  const compiledFieldVals = CL.map_list((a: N.AField) => field(a.value.visit(compiler), 'exp'), fields);
  const fieldNames = CL.map_list((f: N.AField) => jStr(f.name) as J.JExprT, fields);
  const fieldLocs = CL.map_list((f: N.AField) => compiler.getLoc(f.l), fields);
  return { compiledObj, compiledFieldVals, fieldNames, fieldLocs, objLoc: obj.l };
}

function preSplitUpdate(
  compiler: CompilerVisitor,
  loc: Loc,
  optDest: BindType | undefined,
  obj: N.AVal,
  fields: N.AField[]
): HeadPost {
  const state = updateState(compiler, obj, fields);
  const preBodyLabel = compiler.makeLabel();
  return (rest) => {
    const [newCases, afterUpdateLabel] = finishRest(compiler, optDest, preBodyLabel, rest);
    return buildSplitUpdate(compiler, loc, state, newCases, afterUpdateLabel);
  };
}

function buildSplitUpdate(
  compiler: CompilerVisitor,
  loc: Loc,
  state: UpdateState,
  newCases: CList<J.JCaseT>,
  afterUpdateLabel: J.JExprT
): DAG.CBlock {
  const ans = compiler.curAns;
  const step = compiler.curStep;
  return cBlock(
    jBlock(clist<J.JStmt>(
      // Update step before the call, so that if it runs out of gas, the resumer goes to the right step
      jExpr(jAssign(step, afterUpdateLabel)),
      jExpr(jAssign(ans, rtMethod('checkRefAnns',
        clist(
          state.compiledObj,
          jList(false, state.fieldNames),
          jList(false, state.compiledFieldVals),
          jList(false, state.fieldLocs),
          compiler.getLoc(loc),
          compiler.getLoc(state.objLoc))))),
      jBreak)),
    newCases);
}

export function isIdFnName(flatnessEnv: FL.FEnv, name: string): boolean {
  return flatnessEnv.has(name);
}

export function compileALam(
  compiler: CompilerVisitor,
  l: Loc,
  name: string,
  args: N.ABind[],
  _ret: A.Ann,
  body: N.AExpr,
  bindOpt: BindType | undefined
): DAG.CExp {
  let isFlat: boolean;
  if (bindOpt !== undefined && isBLet(bindOpt)) {
    const bind = bindOpt.value;
    isFlat = isFunctionFlat(compiler.flatnessEnv, bind.id.key());
  } else {
    isFlat = false;
  }
  const newStep = freshId(compilerName('step'));
  const temp = freshId(compilerName('temp_lam'));
  const len = args.length;
  // NOTE: args may be empty, so we need at least one name ("resumer") for the stack convention
  const effectiveArgs =
    len > 0 ? args : [new N.ABind(l, compiler.resumer, A.aBlank)];
  return cExp(
    rtMethod('makeFunction', clist<J.JExprT>(jId(temp), jStr(name))),
    clist<J.JStmt>(
      jVar(temp,
        jFun(J.nextJFunId(), makeFunName(compiler, l),
          CL.map_list((arg: N.ABind) => formalShadowName(arg.id), effectiveArgs),
          compileFunBody(l, newStep, temp, ext(compiler, { allowTco: true }), effectiveArgs, len, body, true, isFlat, false)))));
}

// ---------- the compiler visitor ----------

export class CompilerVisitor {
  // fields installed by splitting-compiler
  uri!: string;
  addPhase!: (phase: string, data: any) => any;
  options!: SplitCompileOptions;
  flatnessEnv!: FL.FEnv;
  typeFlatnessEnv!: FL.FEnv;
  bindings!: Map<string, CS.ValueBind>;
  typeBindings!: Map<string, CS.TypeBind>;
  moduleBindings!: Map<string, CS.ModuleBind>;
  env!: CS.CompileEnvironment;
  // fields installed by compile-module
  progProvides!: A.ProvideBlock;
  getLoc!: (l: Loc) => J.JExprT;
  getLocId!: (l: Loc) => number;
  curApploc!: A.Name;
  resumer!: A.Name;
  allowTco!: boolean;
  dispatches!: DispatchesBox;
  // fields installed by compile-fun-body
  makeLabel!: () => J.JExprT;
  curTarget!: J.JExprT;
  curStep!: A.Name;
  curAns!: A.Name;
  args!: A.Name[];
  elidedFrames!: A.Name;

  aModule(node: N.AModule): DAG.CExp {
    const l = node.l;
    const mpSpecs = this.progProvides.specs.filter(A.isSProvideModule);
    const vpSpecs = this.progProvides.specs.filter(A.isSProvideName);
    const tpSpecs = this.progProvides.specs.filter(A.isSProvideType);
    const dpSpecs = this.progProvides.specs.filter(A.isSProvideData);
    void dpSpecs;

    let aliasFields: CList<J.JFieldT> = clEmpty;
    let aliasStmts: CList<J.JStmt> = clEmpty;
    for (const tp of tpSpecs) {
      const ns = tp.nameSpec;
      switch (ns.$name) {
        case 's-local-ref': {
          const compiled = compileAnn(new A.AName(l, ns.name), undefined, this); // TODO(Ben): should be none, or name, or as-name?
          aliasFields = clSnoc(aliasFields, jField(ns.asName.toname(), compiled.exp) as J.JFieldT);
          aliasStmts = clAppend(aliasStmts, compiled.otherStmts);
          break;
        }
        case 's-remote-ref': {
          aliasFields = clSnoc(aliasFields,
            jField(ns.asName.toname(),
              getModuleField(ns.uri, 'types', ns.name.toname())) as J.JFieldT);
          break;
        }
        default:
          throw new InternalCompilerError('Unknown NameSpec in provide-type compilation: ' + (ns as any).$name);
      }
    }

    const compiledProvides = CL.map_list((pv: A.SProvideName) => {
      const ns = pv.nameSpec;
      switch (ns.$name) {
        case 's-local-ref': {
          const valBind = mapGetValue(this.bindings, ns.name.key());
          let valExp: J.JExprT;
          if (CS.isVbLetrec(valBind.binder)) {
            valExp = jDot(jId(jsIdOf(ns.name)), '$var');
          } else if (CS.isVbVar(valBind.binder)) {
            valExp = jId(jsIdOf(ns.name));
          } else if (CS.isVbLet(valBind.binder)) {
            valExp = jId(jsIdOf(ns.name));
          } else {
            throw new InternalCompilerError('Unknown ValueBinder in provide compilation');
          }
          return jField(ns.asName.toname(), valExp) as J.JFieldT;
        }
        case 's-remote-ref': {
          const valExp = getModuleField(ns.uri, 'values', ns.name.toname());
          return jField(ns.asName.toname(), valExp) as J.JFieldT;
        }
        default:
          throw new InternalCompilerError('Unknown NameSpec in provide compilation: ' + (ns as any).$name);
      }
    }, vpSpecs);

    const typesFields = aliasFields; // + data-fields;
    const typesStmts = aliasStmts; // + data-stmts

    const compiledModuleProvides = CL.map_list((pm: A.SProvideModule) => {
      const ns = pm.nameSpec;
      switch (ns.$name) {
        case 's-local-ref': {
          const compiled = jId(jsIdOf(ns.name));
          return jField(ns.asName.toname(), compiled) as J.JFieldT;
        }
        case 's-remote-ref':
          return jField(ns.asName.toname(), jBracket(rtField('modules'), jStr(ns.uri))) as J.JFieldT;
        default:
          throw new InternalCompilerError('Unknown NameSpec in provide-module compilation: ' + (ns as any).$name);
      }
    }, mpSpecs);

    const compiledAnswer: DAG.CExp = node.answer.visit(this);
    const compiledChecks: DAG.CExp = node.checks.visit(this);
    return cExp(
      rtMethod('makeObject', clist<J.JExprT>(
        jObj(clist<J.JFieldT>(
          jField('answer', compiledAnswer.exp),
          jField('namespace', NAMESPACE),
          jField('locations', jId(constId('L'))),
          jField('defined-modules',
            jObj(
              CL.map_list((dm: N.ADefinedModule) => jField(dm.name, jId(jsIdOf(dm.value))) as J.JFieldT, node.definedModules))),
          jField('defined-values',
            jObj(
              CL.map_list((dv: N.ADefinedValue) => {
                switch (dv.$name) {
                  case 'a-defined-value': {
                    const compiledVal = field(dv.value.visit(this), 'exp');
                    return jField(dv.name, compiledVal) as J.JFieldT;
                  }
                  case 'a-defined-var':
                    return jField(dv.name, jId(jsIdOf(dv.id))) as J.JFieldT;
                  default:
                    throw new InternalCompilerError('Unknown ADefinedValue in a-module');
                }
              }, node.definedValues))),
          jField('defined-types',
            jObj(
              CL.map_list((dt: N.ADefinedType) => {
                const compiledAnn = compileAnn(dt.typ, undefined, this).exp;
                return jField(dt.name, compiledAnn) as J.JFieldT;
              }, node.definedTypes))),
          jField('provide-plus-types',
            rtMethod('makeObject', clist<J.JExprT>(jObj(clist<J.JFieldT>(
              jField('values', rtMethod('makeObject', clist<J.JExprT>(jObj(compiledProvides)))),
              jField('types', jObj(typesFields)),
              jField('modules', jObj(compiledModuleProvides))
            ))))),
          jField('checks', compiledChecks.exp))))),

      clAppend(
        clAppend(typesStmts, compiledAnswer.otherStmts),
        compiledChecks.otherStmts));
  }

  // AExpr chains are not visited: compileAExpr drives them directly
  // (see the pre/post machinery above).
  aIf(_node: N.AIf): never {
    return raise('Impossible: a-if directly in compiler-visitor should never happen');
  }

  aCases(_node: N.ACases): never {
    return raise('Impossible: a-cases directly in compiler-visitor should never happen');
  }

  aUpdate(_node: N.AUpdate): never {
    return raise('Impossible: a-update directly in compiler-visitor should never happen');
  }

  aAssign(node: N.AAssign): DAG.CExp {
    const visitValue: DAG.CExp = node.value.visit(this);
    return cExp(rtField('nothing'), clSnoc(visitValue.otherStmts, jExpr(jDotAssign(jId(jsIdOf(node.id)), '$var', visitValue.exp)) as J.JStmt));
  }

  aApp(_node: N.AApp): never {
    return raise('Impossible: a-app directly in compiler-visitor should never happen');
  }

  aPrimApp(node: N.APrimApp): DAG.CExp {
    const visitArgs = node.args.map((a) => a.visit(this) as DAG.CExp);
    const setLoc = clist<J.JStmt>(
      jExpr(jAssign(this.curApploc, this.getLoc(node.l)))
    );
    return cExp(rtMethod(node.f, CL.map_list(getExp, visitArgs)), setLoc);
  }

  aRef(node: N.ARef): DAG.CExp {
    if (node.ann === undefined) {
      return cExp(rtMethod('makeGraphableRef', clEmpty), clEmpty);
    } else {
      return raise('Cannot handle annotations in refs yet');
    }
  }

  aObj(node: N.AObj): DAG.CExp {
    const visitFields = node.fields.map((f) => f.visit(this) as DAG.CField);
    return cExp(rtMethod('makeObject', clist<J.JExprT>(jObj(CL.map_list(oGetField, visitFields)))), clEmpty);
  }

  aGetBang(node: N.AGetBang): DAG.CExp {
    const visitObj: DAG.CExp = node.obj.visit(this);
    return cExp(rtMethod('getFieldRef', clist(visitObj.exp, jStr(node.field), this.getLoc(node.l))), visitObj.otherStmts);
  }

  aExtend(node: N.AExtend): DAG.CExp {
    const visitObj: DAG.CExp = node.supe.visit(this);
    const visitFields = node.fields.map((f) => f.visit(this) as DAG.CField);
    return cExp(rtMethod('extendObj', clist(this.getLoc(node.l), visitObj.exp, jObj(CL.map_list(oGetField, visitFields)))),
      clEmpty);
  }

  aDot(node: N.ADot): DAG.CExp {
    const visitObj: DAG.CExp = node.obj.visit(this);
    return cExp(getFieldSafe(node.l, visitObj.exp, jStr(node.field), this.getLoc(node.l)),
      clSnoc(visitObj.otherStmts, jExpr(jAssign(this.curApploc, this.getLoc(node.l))) as J.JStmt));
  }

  aColon(node: N.AColon): DAG.CExp {
    const visitObj: DAG.CExp = node.obj.visit(this);
    return cExp(rtMethod('getColonFieldLoc', clist(visitObj.exp, jStr(node.field), this.getLoc(node.l))),
      visitObj.otherStmts);
  }

  aMethod(node: N.AMethod): DAG.CExp {
    const step = freshId(compilerName('step'));
    const tempFull = freshId(compilerName('temp_full'));
    const len = node.args.length;
    const fullVar =
      jVar(tempFull,
        jFun(J.nextJFunId(), makeFunName(this, node.l),
          CL.map_list((a: N.ABind) => formalShadowName(a.id), node.args),
          compileFunBody(node.l, step, tempFull, ext(this, { allowTco: true }), node.args, len, node.body, true, false, true)
        ));
    const methodExpr = len < 9
      ? rtMethod('makeMethod' + String(len - 1), clist<J.JExprT>(jId(tempFull), jStr(node.name)))
      : rtMethod('makeMethodN', clist<J.JExprT>(jId(tempFull), jStr(node.name)));
    return cExp(methodExpr, clist<J.JStmt>(fullVar));
  }

  aVal(node: N.AVal$): DAG.CaseResults {
    return node.v.visit(this);
  }

  aField(node: N.AField): DAG.CField {
    const visitV: DAG.CExp = node.value.visit(this);
    return cField(jField(node.name, visitV.exp), visitV.otherStmts);
  }

  aTuple(node: N.ATuple): DAG.CExp {
    const visitVals = node.fields.map((v) => v.visit(this) as DAG.CExp);
    return cExp(rtMethod('makeTuple', clist<J.JExprT>(jList(false, CL.map_list(getExp, visitVals)))), clEmpty);
  }

  aTupleGet(node: N.ATupleGet): DAG.CExp {
    const visitName: DAG.CExp = node.tup.visit(this);
    return cExp(rtMethod('getTuple', clist(visitName.exp, jNum(node.index), this.getLoc(node.l))), clEmpty);
  }

  aArray(node: any): DAG.CExp {
    const visitVals: DAG.CExp[] = node.values.map((v: N.AVal) => v.visit(this) as DAG.CExp);
    const otherStmts = visitVals.reduceRight((acc: CList<J.JStmt>, v: DAG.CExp) => clAppend(v.otherStmts, acc), clEmpty as CList<J.JStmt>);
    return cExp(jList(false, CL.map_list(getExp, visitVals)), otherStmts);
  }

  aSrcloc(node: N.ASrcloc): DAG.CExp {
    return cExp(this.getLoc(node.loc), clEmpty);
  }

  aNum(node: N.ANum): DAG.CExp {
    if (typeof node.n === 'number') {
      return cExp(jParens(jNum(node.n)), clEmpty);
    } else {
      return cExp(rtMethod('makeNumberFromString', clist<J.JExprT>(jStr(String(node.n)))), clEmpty);
    }
  }

  aStr(node: N.AStr): DAG.CExp {
    return cExp(jParens(jStr(node.s)), clEmpty);
  }

  aBool(node: N.ABool): DAG.CExp {
    return cExp(jParens(node.b ? jTrue : jFalse), clEmpty);
  }

  aUndefined(_node: N.AUndefined): DAG.CExp {
    return cExp(UNDEFINED, clEmpty);
  }

  aPrimVal(node: N.APrimVal): DAG.CExp {
    return cExp(rtField(node.name), clEmpty);
  }

  aId(node: N.AId): DAG.CExp {
    return cExp(jId(jsIdOf(node.id)), clEmpty);
  }

  aIdModref(node: N.AIdModref): DAG.CExp {
    return cExp(
      jBracket(
        jDot(
          jDot(
            jDot(jId(jsIdOf(node.id)), 'dict'),
            'values'),
          'dict'),
        jStr(node.name)), clEmpty);
  }

  aIdVarModref(node: N.AIdVarModref): DAG.CExp {
    return cExp(
      jDot(jBracket(
        jDot(
          jDot(
            jDot(jId(jsIdOf(node.id)), 'dict'),
            'values'),
          'dict'),
        jStr(node.name)), '$var'), clEmpty);
  }

  aIdVar(node: N.AIdVar): DAG.CExp {
    return cExp(jDot(jId(jsIdOf(node.id)), '$var'), clEmpty);
  }

  aIdSafeLetrec(node: N.AIdSafeLetrec): DAG.CExp {
    const s = jId(jsIdOf(node.id));
    return cExp(jDot(s, '$var'), clEmpty);
  }

  aIdLetrec(node: N.AIdLetrec): DAG.CExp {
    const s = jId(jsIdOf(node.id));
    if (node.safe) {
      return cExp(jDot(s, '$var'), clEmpty);
    } else {
      return cExp(
        jTernary(
          jBinop(jDot(s, '$var'), jEq, UNDEFINED),
          raiseIdExn(this.getLoc(node.l), node.id.toname()),
          jDot(s, '$var')),
        clEmpty);
    }
  }

  aDataExpr(node: N.ADataExpr): DAG.CExp {
    const self = this;
    const l = node.l;
    const name = node.name;
    const namet = node.namet;
    const variants = node.variants;
    const shared = node.shared;

    function brandName(base: string): string {
      return jsIdOf(compilerName('brand-' + base)).toname();
    }
    void brandName;

    const visitSharedFields = CL.map_list((f: N.AField) => f.visit(self) as DAG.CField, shared);
    const sharedFields = visitSharedFields.map(oGetField);
    const externalBrand = jId(jsIdOf(namet));

    function makeBrandPredicate(loc: Loc, b: J.JExprT, predName: string): J.JFieldT {
      const val = freshId(compilerName('val'));
      return jField(
        predName,
        rtMethod('makeFunction', clist<J.JExprT>(
          jFun(J.nextJFunId(),
            makeFunName(self, l),
            clist(val),
            jBlock(
              clSnoc(
                arityCheck(self.getLoc(loc), 1, false),
                jReturn(rtMethod('makeBoolean', clist(rtMethod('hasBrand', clist<J.JExprT>(jId(val), b))))) as J.JStmt)
            )
          ),
          jStr(predName + '-Tester')
        ))
      );
    }
    void makeBrandPredicate;

    function makeVariantConstructor(
      l2: Loc,
      baseId: A.Name,
      brandsId: A.Name,
      members: N.AVariantMember[],
      reflName: J.JExprT,
      reflRefFieldsMask: J.JExprT,
      reflFields: J.JExprT,
      constructorId: J.JExprT
    ): DAG.CExp {
      const nonblankAnns = members.filter((m) => !A.isABlank(m.bind.ann) && !A.isAAny(m.bind.ann));
      let anns: CList<J.JExprT> = clEmpty;
      let others: CList<J.JStmt> = clEmpty;
      for (const m of nonblankAnns) {
        const compiled = compileAnn(m.bind.ann, undefined, self);
        anns = clSnoc(anns, compiled.exp);
        others = clAppend(others, compiled.otherStmts);
      }
      const compiledLocs = CL.map_list((m: N.AVariantMember) => self.getLoc((m.bind.ann as any).l), nonblankAnns);
      const compiledVals = CL.map_list((m: N.AVariantMember) => jStr(jsIdOf(m.bind.id).tosourcestring()) as J.JExprT, nonblankAnns);

      // NOTE(joe 6-14-2014): We cannot currently statically check for if an annotation
      // is a refinement because of type aliases.  So, we use checkAnnArgs, which takes
      // a continuation and manages all of the stack safety of annotation checking itself.

      // NOTE(joe 5-26-2015): This has been moved to a hybrid static/dynamic solution by
      // passing the check off to a runtime function that uses JavaScript's Function
      // to only do the refinement check once.
      return cExp(
        rtMethod('makeVariantConstructor', clist<J.JExprT>(
          self.getLoc(l2),
          // NOTE(joe): Thunked at the JS level because compiled-anns might contain
          // references to rec ids that should be resolved later
          jFun(J.nextJFunId(), '$synthesizedConstructor_' + baseId.toname(), clEmpty, jBlock1(jReturn(jList(false, anns)))),
          jList(false, compiledVals),
          jList(false, compiledLocs),
          jList(false, CL.map_list((m: N.AVariantMember) => jBool(N.isAMutable(m.memberType)), members)),
          jList(false, CL.map_list((m: N.AVariantMember) => jStr(jsIdOf(m.bind.id).tosourcestring()) as J.JExprT, members)),
          reflRefFieldsMask,
          jId(baseId),
          jId(brandsId),
          reflName,
          reflFields,
          constructorId
        )),
        clEmpty);
    }

    function compileVariant(v: N.AVariant): { stmts: CList<J.JStmt>; constructor: J.JFieldT; predicate: J.JFieldT } {
      const vname = v.name;
      const variantBaseId = jsIdOf(compilerName(vname + '-base'));
      const variantBrand = rtMethod('namedBrander', clist<J.JExprT>(jStr(vname), self.getLoc(v.l)));
      const variantBrandId = jsIdOf(compilerName(vname + '-brander'));
      const variantBrandObjId = jsIdOf(compilerName(vname + '-brands'));
      const variantBrands = jObj(clEmpty);
      const visitWithFields = v.withMembers.map((wm) => wm.visit(self) as DAG.CField);

      const reflBaseFields: CList<J.JFieldT> =
        N.isASingletonVariant(v)
          ? clEmpty
          : clist<J.JFieldT>(
            jField('$fieldNames',
              jList(false, CL.map_list((m: N.AVariantMember) => jStr(m.bind.id.toname()) as J.JExprT, (v as N.AVariant$).members))));

      const fId = constId('f');
      const reflName = jStr(vname);

      const reflRefFieldsMaskId = jsIdOf(compilerName(vname + '_mutablemask'));
      const reflRefFieldsMask: J.JExprT =
        N.isASingletonVariant(v)
          ? jList(false, clEmpty)
          : jList(false,
            CL.map_list((m: N.AVariantMember) => (N.isAMutable(m.memberType) ? jTrue : jFalse) as J.JExprT, (v as N.AVariant$).members));

      const reflFieldsId = jsIdOf(compilerName(vname + '_getfields'));
      const refmaskId = constId('refmask');
      // refmask says which fields the caller wants dereferenced: `cases`
      // passes the fields its branch bound with `ref`, and _match passes the
      // mutable ones.  derefField also raises if that disagrees with the
      // field itself.
      const reflFields: J.JExprT =
        N.isAVariant(v)
          ? jFun(J.nextJFunId(), 'variant',
            clist<A.Name>(fId, refmaskId), jBlock1(jReturn(jApp(jId(fId),
              CL.map_list_n((i: number, m: N.AVariantMember) => {
                const field = getDictField(THIS, jStr(m.bind.id.toname()));
                const mask = jBracket(jId(refmaskId), jNum(i));
                return rtMethod('derefField',
                  clist<J.JExprT>(field, jBool(N.isAMutable(m.memberType)), mask));
              }, 0, v.members)))))
          : jFun(J.nextJFunId(), 'variant',
            clist<A.Name>(constId('f')), jBlock1(jReturn(jApp(jId(fId), clEmpty))));

      function memberCount(v2: N.AVariant): number {
        switch (v2.$name) {
          case 'a-variant': return v2.members.length;
          case 'a-singleton-variant': return 0;
          default:
            throw new InternalCompilerError('Unknown AVariant in member-count');
        }
      }

      const matchField = jField('_match', rtMethod('makeMatch', clist<J.JExprT>(reflName, jNum(memberCount(v)))));

      const stmts =
        visitWithFields.reduceRight((acc: CList<J.JStmt>, vf: DAG.CField) => clAppend(vf.otherStmts, acc),
          clist<J.JStmt>(
            jVar(reflFieldsId, reflFields),
            jVar(reflRefFieldsMaskId, reflRefFieldsMask),
            jVar(variantBaseId, jObj(reflBaseFields
              .append(sharedFields)
              .append(CL.map_list(oGetField, visitWithFields))
              .append(clist<J.JFieldT>(matchField)))),
            jVar(variantBrandId, variantBrand),
            jVar(variantBrandObjId, variantBrands),
            jExpr(jBracketAssign(
              jId(variantBrandObjId),
              jDot(externalBrand, '_brand'),
              jTrue)),
            jExpr(jBracketAssign(
              jId(variantBrandObjId),
              jDot(jId(variantBrandId), '_brand'),
              jTrue))
          ));
      const predicate = jField(A.makeCheckerName(vname), getFieldUnsafe(jId(variantBrandId), jStr('test'), self.getLoc(v.l))); // make-brand-predicate(v.l, j-dot(j-id(variant-brand-id), "_brand"), A.make-checker-name(vname))

      switch (v.$name) {
        case 'a-variant': {
          const constrVname = jsIdOf(constId(vname));
          const compiledConstr =
            makeVariantConstructor(v.l, variantBaseId, variantBrandObjId, v.members,
              reflName, jId(reflRefFieldsMaskId), jId(reflFieldsId), jId(variantBaseId));
          return {
            stmts: clSnoc(
              clAppend(stmts, compiledConstr.otherStmts),
              jVar(constrVname, compiledConstr.exp) as J.JStmt),
            constructor: jField(vname, jId(constrVname)),
            predicate: predicate,
          };
        }
        case 'a-singleton-variant': {
          return {
            stmts: stmts,
            constructor: jField(vname, rtMethod('makeDataValue', clist<J.JExprT>(jId(variantBaseId), jId(variantBrandObjId), reflName, jId(reflFieldsId), jNum(-1), jId(reflRefFieldsMaskId), jId(variantBaseId), jFalse, self.getLoc(v.l)))),
            predicate: predicate,
          };
        }
        default:
          throw new InternalCompilerError('Unknown AVariant in compile-variant');
      }
    }

    const variantPieces = variants.map(compileVariant);

    let headerStmts: CList<J.JStmt> = clEmpty;
    for (const piece of variantPieces) {
      headerStmts = clAppend(headerStmts, piece.stmts);
    }
    let objFields: CList<J.JFieldT> = clEmpty;
    for (const piece of variantPieces) {
      objFields = clAppend(objFields, clist<J.JFieldT>(piece.predicate, piece.constructor));
    }

    const dataPredicate = jField(name, getFieldUnsafe(externalBrand, jStr('test'), this.getLoc(l))); // make-brand-predicate(l, j-dot(external-brand, "_brand"), name)

    const dataObject = rtMethod('makeObject', clist<J.JExprT>(jObj(clCons(dataPredicate as J.JFieldT, objFields))));

    return cExp(dataObject, headerStmts);
  }
}

export function mkAbbrevs(l: Loc): CList<J.JStmt> {
  const loc = constId('loc');
  const name = constId('name');
  return clist<J.JStmt>(
    jVar(constId('G'), rtField('getFieldLoc')),
    jVar(constId('U'), jFun(J.nextJFunId(), 'throw_error', clist<A.Name>(loc, name),
      jBlock1(jExpr(jMethod(rtField('ffi'), 'throwUninitializedIdMkLoc',
        clist<J.JExprT>(jId(loc), jId(name))))))),
    jVar(constId('M'), jStr((l as SL.Srcloc).source)),
    jVar(constId('D'), rtField('undefined'))
  );
}

export function importKey(i: A.ImportType): string {
  return AU.importToDep(i).key();
}

export function compileTypeVariant(variant: T.TypeVariant): J.JExprT {
  switch (variant.$name) {
    case 't-variant': {
      const compiledMembers = jList(false, CL.map_list(([memName, typ]: T.VariantField) => {
        if (T.isTRef(typ)) {
          return jList(true, clist<J.JExprT>(jStr('ref'), jStr(memName), compileProvidedType(typ.typ))) as J.JExprT;
        } else {
          return jList(true, clist<J.JExprT>(jStr(memName), compileProvidedType(typ))) as J.JExprT;
        }
      }, variant.fields));
      const compiledWithMembers = jObj(clMapSd((memName: string) =>
        compileTypeMember(memName, mapGetValue(variant.withFields, memName)) as J.JFieldT, variant.withFields));
      return jList(true, clist<J.JExprT>(jStr(variant.name), compiledMembers, compiledWithMembers));
    }
    case 't-singleton-variant': {
      const compiledWithMembers = jObj(clMapSd((memName: string) =>
        compileTypeMember(memName, mapGetValue(variant.withFields, memName)) as J.JFieldT, variant.withFields));
      return jList(true, clist<J.JExprT>(jStr(variant.name), compiledWithMembers));
    }
    default:
      throw new InternalCompilerError('Unknown TypeVariant in compile-type-variant');
  }
}

export function compileTypeMember(name: string, typ: T.Type): J.JFieldT {
  return jField(name, compileProvidedType(typ));
}

export function compileProvidedData(de: CS.DataExport): J.JExprT {
  switch (de.$name) {
    case 'd-alias':
      return jList(false,
        clist<J.JExprT>(jStr('data-alias'),
          compileOrigin(de.origin),
          jStr(de.name)));
    case 'd-type': {
      const typ = de.typ;
      switch (typ.$name) {
        case 't-data':
          return jList(false,
            clist<J.JExprT>(jStr('data'),
              compileOrigin(de.origin),
              jStr(typ.name),
              jList(false, CL.map_list((p: T.Type) => jStr((p as T.TVar).id.key()) as J.JExprT, typ.params)),
              jList(false, CL.map_list(compileTypeVariant, typ.variants)),
              jObj(clMapSd((memName: string) =>
                compileTypeMember(memName, mapGetValue(typ.fields, memName)) as J.JFieldT, typ.fields))));
        default:
          throw new InternalCompilerError('Unknown DataType in compile-provided-data');
      }
    }
    default:
      throw new InternalCompilerError('Unknown DataExport in compile-provided-data');
  }
}

export function compileProvidedType(typ: T.Type): J.JExprT {
  switch (typ.$name) {
    case 't-name': {
      const modName = typ.moduleName;
      switch (modName.$name) {
        case 'local':
          return jObj(clist<J.JFieldT>(
            jField('tag', jStr('name')),
            jField('origin', jObj(clist<J.JFieldT>(jField('import-type', jStr('$ELF'))))),
            jField('name', jStr(typ.id.toname())))); // TODO: toname or key?
        case 'module-uri':
          return jObj(clist<J.JFieldT>(
            jField('tag', jStr('name')),
            jField('origin', jObj(clist<J.JFieldT>(jField('import-type', jStr('uri')), jField('uri', jStr(modName.uri))))),
            jField('name', jStr(typ.id.toname())))); // TODO: toname or key?
        case 'dependency':
          return raise("Dependency-origin names in provided-types shouldn't be possible");
        default:
          throw new InternalCompilerError('Unknown NameOrigin in compile-provided-type');
      }
    }
    case 't-var':
      return jList(true, clist<J.JExprT>(jStr('tid'), jStr(typ.id.key()))); // NOTE(joe): changed to .key()
    case 't-arrow':
      return jList(true,
        clist<J.JExprT>(jStr('arrow'),
          jList(true, CL.map_list(compileProvidedType, typ.args)), compileProvidedType(typ.ret)));
    case 't-app':
      return jList(false,
        clist<J.JExprT>(jStr('tyapp'), compileProvidedType(typ.onto),
          jList(true, CL.map_list(compileProvidedType, typ.args))));
    case 't-top':
      return jStr('tany');
    case 't-bot':
      return jStr('tbot');
    case 't-record':
      return jList(false,
        clist<J.JExprT>(jStr('record'), jObj(clMapSd((key: string) =>
          compileTypeMember(key, mapGetValue(typ.fields, key)) as J.JFieldT, typ.fields))));
    case 't-tuple':
      return jList(false,
        clist<J.JExprT>(jStr('tuple'), jList(false, CL.map_list(compileProvidedType, typ.elts))));
    case 't-forall':
      return jList(true,
        clist<J.JExprT>(jStr('forall'),
          jList(false, CL.map_list((p: T.Type) => jStr((p as T.TVar).id.key()) as J.JExprT, typ.introduces)),
          compileProvidedType(typ.onto)));
      // | t-ref(_, _) =>
      // | t-existential(_, _) =>
    case 't-data-refinement':
      return jList(true,
        clist<J.JExprT>(jStr('data%'), compileProvidedType(typ.dataType), jStr(typ.variantName)));
    default:
      return jTernary(jFalse, jStr(String(typ)), jStr('tany'));
  }
}

export function srclocToRaw(l: Loc): J.JExprT {
  switch (l.$name) {
    case 'builtin':
      return jList(true, clist<J.JExprT>(jStr(l.moduleName)));
    case 'srcloc':
      return jList(true, clist<J.JExprT>(jStr(l.source), jNum(l.startLine), jNum(l.startColumn), jNum(l.startChar), jNum(l.endLine), jNum(l.endColumn), jNum(l.endChar)));
    default:
      throw new InternalCompilerError('Unknown Srcloc in srcloc-to-raw');
  }
}

export function compileOrigin(bo: CS.BindOrigin): J.JExprT {
  return jObj(clist<J.JFieldT>(
    jField('local-bind-site', srclocToRaw(bo.localBindSite)),
    jField('definition-bind-site', srclocToRaw(bo.definitionBindSite)),
    jField('new-definition', jBool(bo.newDefinition)),
    jField('uri-of-definition', jStr(bo.uriOfDefinition))
  ));
}

export function compileProvides(provides: CS.Provides): J.JExprT {
  const moduleFields = clMapSd((m: string) =>
    jField(m, jObj(clist<J.JFieldT>(jField('uri', jStr(mapGetValue(provides.modules, m)))))) as J.JFieldT,
  provides.modules);
  const valueFields = clMapSd((v: string) => {
    const ve = mapGetValue(provides.values, v);
    switch (ve.$name) {
      case 'v-alias':
        return jField(v, jObj(clist<J.JFieldT>(
          jField('bind', jStr('alias')),
          jField('origin', compileOrigin(ve.origin)),
          jField('original-name', jStr(ve.originalName)),
          jField('typ', jBool(false))
        ))) as J.JFieldT;
      case 'v-just-type':
        return jField(v, jObj(clist<J.JFieldT>(
          jField('bind', jStr('let')),
          jField('origin', compileOrigin(ve.origin)),
          jField('typ', compileProvidedType(ve.t))
        ))) as J.JFieldT;
      case 'v-var':
        return jField(v, jObj(clist<J.JFieldT>(
          jField('bind', jStr('var')),
          jField('origin', compileOrigin(ve.origin)),
          jField('typ', compileProvidedType(ve.t))
        ))) as J.JFieldT;
      case 'v-fun':
        return jField(v, jObj(clist<J.JFieldT>(
          jField('bind', jStr('fun')),
          jField('origin', compileOrigin(ve.origin)),
          jField('flatness', ve.flatness !== undefined ? jNum(ve.flatness) : jFalse),
          jField('name', jStr(ve.name)),
          jField('typ', compileProvidedType(ve.t))
        ))) as J.JFieldT;
      default:
        throw new InternalCompilerError('Unknown ValueExport in compile-provides');
    }
  }, provides.values);
  const dataFields = clMapSd((d: string) =>
    jField(d, compileProvidedData(mapGetValue(provides.dataDefinitions, d))) as J.JFieldT,
  provides.dataDefinitions);
  const aliasFields = clMapSd((a: string) =>
    jField(a, compileProvidedType(mapGetValue(provides.aliases, a))) as J.JFieldT,
  provides.aliases);
  return jObj(clist<J.JFieldT>(
    jField('modules', jObj(moduleFields)),
    jField('values', jObj(valueFields)),
    jField('datatypes', jObj(dataFields)),
    jField('aliases', jObj(aliasFields))
  ));
}

// Pyret lists.sort-by (the non-stable variant used by compile-module):
// quicksort on the first element; equal elements come out in reverse order
// of appearance (they are accumulated with cons and not re-sorted).
function sortBy<T>(lst: T[], cmp: (a: T, b: T) => boolean, eq: (a: T, b: T) => boolean): T[] {
  if (lst.length === 0) { return []; }
  const pivot = lst[0];
  // builds up three lists, split according to cmp and eq
  const areLt: T[] = [];
  const areEq: T[] = [];
  const areGt: T[] = [];
  for (const e of lst) {
    if (cmp(e, pivot)) { areLt.unshift(e); }
    else if (eq(e, pivot)) { areEq.unshift(e); }
    else { areGt.unshift(e); }
  }
  const less = sortBy(areLt, cmp, eq);
  const equal = areEq;
  const greater = sortBy(areGt, cmp, eq);
  return [...less, ...equal, ...greater];
}

interface ModuleSpec {
  id: A.Name;
  inputId: A.Name;
  imp: A.SImport;
}

export function compileModule(
  self: CompilerVisitor,
  l: Loc,
  progProvides: A.ProvideBlock,
  importsIn: A.Import[],
  prog: N.AExpr,
  freevarsIn: Map<string, A.Name>,
  provides: CS.Provides,
  env: CS.CompileEnvironment
): Map<string, J.JExprT> {
  jsNames.reset();
  // NOTE(ordering): Pyret's freevars.unfreeze() re-orders keys into hash
  // trie order; this copy keeps the Map's insertion order. See header, site 3.
  const freevars = new Map(freevarsIn);

  const imports = sortBy(
    importsIn.filter(A.isSImport) as A.SImport[],
    (i1, i2) => importKey(i1.file) < importKey(i2.file),
    (i1, i2) => importKey(i1.file) === importKey(i2.file)
  );

  for (const i of imports) {
    switch (i.$name) {
      case 's-import':
        freevars.delete(i.name.key());
        break;
      default:
        break;
    }
  }

  const freeIds = [...freevars.keys()].sort()
    .map((k) => mapGetValue(freevars, k));
  const moduleAndGlobalBinds = partition(A.isSAtom, freeIds);
  const globalBinds = CL.map_list((n: A.Name) => {
    let maybeOrigin: CS.BindOrigin | undefined;
    let which: string;
    switch (n.$name) {
      case 's-module-global':
        maybeOrigin = env.originByModuleName(n.toname());
        which = 'modules';
        break;
      case 's-global':
        maybeOrigin = env.originByValueName(n.toname());
        which = 'values';
        break;
      case 's-type-global':
        maybeOrigin = env.originByTypeName(n.toname());
        which = 'types';
        break;
      default:
        throw new InternalCompilerError('Unknown global name in compile-module: ' + n.$name);
    }

    let uri: string;
    let name: string;
    if (maybeOrigin !== undefined) {
      uri = maybeOrigin.uriOfDefinition;
      name = maybeOrigin.originalName.toname();
    } else {
      return raise(n.toname() + ' not found');
    }

    return jVar(jsIdOf(n), getModuleField(uri, which, name)) as J.JStmt;
  }, moduleAndGlobalBinds.isFalse);
  // MARK(joe): need to do something below for modules that come from
  // a context like "include"
  const moduleBinds = CL.map_list((n: A.Name) => {
    let which: string;
    let uri: string;
    let lookupName: A.Name;
    if (self.bindings.has(n.key())) {
      const valBind = mapGetValue(self.bindings, n.key());
      which = 'values';
      uri = valBind.origin.uriOfDefinition;
      lookupName = valBind.origin.originalName;
    } else if (self.typeBindings.has(n.key())) {
      const typBind = mapGetValue(self.typeBindings, n.key());
      which = 'types';
      uri = typBind.origin.uriOfDefinition;
      lookupName = typBind.origin.originalName;
    } else if (self.moduleBindings.has(n.key())) {
      const modBind = mapGetValue(self.moduleBindings, n.key());
      which = 'modules';
      uri = modBind.origin.uriOfDefinition;
      lookupName = modBind.origin.originalName;
    } else {
      throw new InternalCompilerError('No binding found for ' + n.key() + ' in compile-module');
    }
    return jVar(jsIdOf(n), getModuleField(uri, which, lookupName.toname())) as J.JStmt;
  }, moduleAndGlobalBinds.isTrue);
  function cleanImportName(name: A.Name): A.Name {
    return jsIdOf(name);
  }
  const modIds = imports.map((i) => cleanImportName(i.name));
  const moduleLocators = imports.map((i) => AU.importToDep(i.file));
  const filenames = imports.map((i) => {
    const file = i.file;
    switch (file.$name) {
      case 's-const-import':
        return 'trove/' + file.mod;
      case 's-special-import': {
        const typ = file.kind;
        const args = file.args;
        if (typ === 'my-gdrive') {
          return '@my-gdrive/' + args[0];
        } else if (typ === 'shared-gdrive') {
          return '@shared-gdrive/' + args[0] + '/' + args[1];
        } else if (typ === 'js-http') {
          return '@js-http/' + args[0];
        } else if (typ === 'gdrive-js') {
          return '@gdrive-js/' + args[0] + '/' + args[1];
        } else {
          // NOTE(joe): under new module loading, this doesn't actually matter
          // NOTE(joe): yes it does, this is how we get a serialized rep of
          // the dependencies for the next time we need to check it
          return new CS.Dependency(typ, args).key();
        }
      }
      default:
        throw new InternalCompilerError('Unknown ImportType in compile-module');
    }
  });
  void filenames;
  // this needs to be freshened to support multiple repl interactions with the "same" source
  const moduleId = freshId(compilerName((l as SL.Srcloc).source)).tosourcestring();
  const moduleRef = (name: string): J.JExprT => jBracket(rtField('modules'), jStr(name));
  void moduleRef;
  const inputIds = CL.map_list((i: A.Name) => {
    if (A.isSAtom(i) && (i.base === '$import')) { return jsNames.makeAtom('$$import'); }
    else { return jsIdOf(compilerName(i.toname())); }
  }, modIds);
  const casesDispatches = new DispatchesBox(clEmpty);
  function wrapModules(modules: ModuleSpec[], bodyName: A.Name, bodyFun: J.JExprT): J.JBlockT {
    const modInputNames = CL.map_list((m: ModuleSpec) => m.inputId, modules);
    const modInputIds = modInputNames.map(jId);
    const modInputIdsList = modInputIds.toList();
    void modInputIdsList;
    const modValIds = modules.map(getId);
    void modValIds;
    const moduleVal = constId('moduleVal');
    return jBlock(
      CL.map_list((m: ModuleSpec) => jVar(m.id, jId(m.inputId)) as J.JStmt, modules)
        .append(casesDispatches.dispatches)
        .append(moduleBinds)
        .append(clist<J.JStmt>(
          jVar(bodyName, bodyFun),
          jReturn(rtMethod(
            'safeCall', clist<J.JExprT>(
              jId(bodyName),
              jFun(J.nextJFunId(),
                'module_load',
                clist<A.Name>(moduleVal),
                jBlock(clist<J.JStmt>(
                  jExpr(jBracketAssign(rtField('modules'), jStr(moduleId), jId(moduleVal))),
                  jReturn(jId(moduleVal))
                ))),
              jStr('Evaluating ' + bodyName.toname())
            ))))));
  }
  const moduleSpecs: ModuleSpec[] = [];
  const inputIdsList = inputIds.toList();
  for (let i = 0; i < imports.length; i++) {
    moduleSpecs.push({ id: modIds[i], inputId: inputIdsList[i], imp: imports[i] });
  }
  let locations: CList<J.JExprT> = clEmpty;
  let locCount = 0;
  const locCache: Map<string, number> = new Map();
  const LOCS = constId('L');
  function getLocId(loc: Loc): number {
    const asStr = loc.key();
    const cached = locCache.get(asStr);
    if (cached !== undefined) {
      return cached;
    } else {
      const ans = locCount;
      locCache.set(asStr, ans);
      locCount = locCount + 1;
      locations = clSnoc(locations, objOfLoc(loc));
      return ans;
    }
  }
  function getLoc(loc: Loc): J.JExprT {
    return jBracket(jId(LOCS), jNum(getLocId(loc)));
  }

  function wrapNewModule(compiler: CompilerVisitor, moduleBody: J.JBlockT): Map<string, J.JExprT> {
    const moduleLocatorsAsJs = CL.map_list((m: CS.AnyDependency) => {
      switch (m.$name) {
        case 'builtin':
          return jObj(clist<J.JFieldT>(
            jField('import-type', jStr('builtin')),
            jField('name', jStr(m.modname)))) as J.JExprT;
        case 'dependency':
          return jObj(clist<J.JFieldT>(
            jField('import-type', jStr('dependency')),
            jField('protocol', jStr(m.protocol)),
            jField('args', jList(true, CL.map_list((s: string) => jStr(s) as J.JExprT, m.arguments))))) as J.JExprT;
        default:
          throw new InternalCompilerError('Unknown Dependency in wrap-new-module');
      }
    }, moduleLocators);
    const providesObj = compileProvides(provides);
    const theModule = jFun(J.nextJFunId(), makeFunName(compiler, l),
      clist<A.Name>(RUNTIME.id, NAMESPACE.id, sourceName.id).append(inputIds), moduleBody);
    const moduleAndMap = theModule.toUglySourcemap(provides.fromUri, 1, 1, provides.fromUri);
    const out = new Map<string, J.JExprT>();
    out.set('requires', jList(true, moduleLocatorsAsJs));
    out.set('provides', providesObj);
    out.set('nativeRequires', jList(true, clEmpty));
    out.set('theModule',
      compiler.options.collectAll
        ? theModule
        : (compiler.options.moduleEval === false
          ? jRawCode(moduleAndMap.code)
          : jStr(moduleAndMap.code)));
    out.set('theMap', jStr(moduleAndMap.map));
    return out;
  }

  const step = freshId(compilerName('step'));
  const toplevelName = freshId(compilerName('toplevel'));
  const apploc = freshId(compilerName('al'));
  const resumer = compilerName('resumer');
  const resumerBind = new N.ABind(l, resumer, A.aBlank);
  const bodyCompiler: CompilerVisitor = ext(self, {
    progProvides: progProvides,
    getLoc: getLoc,
    getLocId: getLocId,
    curApploc: apploc,
    resumer: resumer,
    allowTco: false,
    dispatches: casesDispatches,
  });
  const visitedBody = compileFunBody(l, step, toplevelName,
    bodyCompiler, // resumer gets js-id-of'ed in compile-fun-body
    [resumerBind], undefined, prog, true, false, false);
  const toplevelFun = jFun(J.nextJFunId(), makeFunName(bodyCompiler, l), clist<A.Name>(formalShadowName(resumer)), visitedBody);
  const defineLocations = jVar(LOCS, jList(true, locations));
  // NOTE: as in the Pyret source, wrap-modules' j-block sits directly in
  // statement position of the outer block; it prints as its statements.
  const moduleBody = jBlock(
    //                    [clist: j-expr(j-str("use strict"))] +
    clSnoc(
      clAppend(
        clSnoc(mkAbbrevs(l), defineLocations as J.JStmt),
        globalBinds),
      wrapModules(moduleSpecs, toplevelName, toplevelFun) as unknown as J.JStmt));
  return wrapNewModule(bodyCompiler, moduleBody);
}

// Eventually maybe we should have a more general "optimization-env" instead of
// flatness-env. For now, leave it since our design might change anyway.

export class SplittingCompiler extends CompilerVisitor {
  private $provides: CS.Provides;

  constructor(
    env: CS.CompileEnvironment,
    addPhase: (phase: string, data: any) => any,
    flatnessEnvs: [FL.FEnv, FL.FEnv],
    provides: CS.Provides,
    postEnv: CS.ComputedEnvironment,
    options: SplitCompileOptions
  ) {
    super();
    this.uri = provides.fromUri;
    this.addPhase = addPhase;
    this.options = options;
    this.flatnessEnv = flatnessEnvs[0];
    this.typeFlatnessEnv = flatnessEnvs[1];
    // Pyret accesses these fields directly; a computed-none here would be a
    // field-not-found error there too.
    this.bindings = (postEnv as CS.ComputedEnv).bindings;
    this.typeBindings = (postEnv as CS.ComputedEnv).typeBindings;
    this.moduleBindings = (postEnv as CS.ComputedEnv).moduleBindings;
    this.env = env;
    this.$provides = provides;
  }

  aProgram(node: N.AProgram): Map<string, J.JExprT> {
    totalTime = 0;
    // This achieves nothing with our current code-gen, so it's a waste of time
    // simplified = body.visit(remove-useless-if-visitor)
    // add-phase("Remove useless ifs", simplified)
    const freevars = N.freevarsProg(new N.AProgram(node.l, node.provides, node.imports, node.body));
    this.addPhase('Freevars-e', freevars);
    const ans = compileModule(this, node.l, node.provides, node.imports, node.body, freevars as Map<string, A.Name>, this.$provides, this.env);
    this.addPhase('Total simplification: ' + String(totalTime), undefined);
    return ans;
  }
}

export function splittingCompiler(
  env: CS.CompileEnvironment,
  addPhase: (phase: string, data: any) => any,
  flatnessEnvs: [FL.FEnv, FL.FEnv],
  provides: CS.Provides,
  postEnv: CS.ComputedEnvironment,
  options: SplitCompileOptions
): SplittingCompiler {
  return new SplittingCompiler(env, addPhase, flatnessEnvs, provides, postEnv, options);
}
