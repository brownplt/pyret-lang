/*
  Ported from: src/arr/compiler/desugar.arr
  See CONVENTIONS.md. The `where:` blocks (tests) are not ported.
*/

import * as A from './ast';
import * as C from './compile-structs';
import { Loc, dummyLoc } from './srcloc';
import { jsnums, throwingErrbacks } from './interop/js-numbers';
import { raise, partition, field, nonNull } from './shared';
import { eliminatedScopeForm } from './ast-visitors';

const names = A.globalNames;

// ---------- data DesugarEnv ----------

export class DEnv {
  get $name(): 'd-env' { return 'd-env'; }
  constructor(public ids: Set<string>, public vars: Set<string>, public letrecs: Set<string>) {}
}
export type DesugarEnv = DEnv;
export function isDEnv(x: any): x is DEnv { return x instanceof DEnv; }

// ---------- data Pair ----------

export class Pair<L, R> {
  get $name(): 'pair' { return 'pair'; }
  constructor(public left: L, public right: R) {}
}
export function pair<L, R>(left: L, right: R): Pair<L, R> { return new Pair(left, right); }
export function isPair(x: any): x is Pair<any, any> { return x instanceof Pair; }

export const mtDEnv = new DEnv(new Set(), new Set(), new Set());

let generatedBinds: Map<string, C.ValueBind> = new Map();

export function g(id: string): A.SGlobal { return new A.SGlobal(id); }
export function gid(l: Loc, id: string): A.SId { return new A.SId(l, g(id)); }
export function bid(l: Loc, name: string): A.SDot {
  return new A.SDot(l, new A.SPrimVal(l, 'builtins'), name);
}

export const flatPrimApp = new A.PrimAppInfoC(false);

export function checkBool<T>(l: Loc, e: A.Expr, cont: (e: A.Expr) => T): T {
  return cont(new A.SPrimApp(l, 'checkWrapBoolean', [e], flatPrimApp));
}

export function checkTable<T>(l: Loc, e: A.Expr, cont: (e: A.Expr) => T): T {
  return cont(new A.SPrimApp(l, 'checkWrapTable', [e], flatPrimApp));
}

export function checkAnn(l: Loc, expr: A.Expr, ann: A.Ann): A.Expr {
  const id = mkIdAnn(l, 'ann-check_', ann);
  return A.sScopeLetBlock(l, [new A.SLetBind(l, id.idB, expr)], id.idE);
}

export function getTableColumn(opL: Loc, l: Loc, e: A.Expr, column: { name: A.Expr; l: Loc }): A.Expr {
  return new A.SApp(l,
    new A.SDot(dummyLoc, e, '_column-index'),
    [
      new A.SSrcloc(dummyLoc, opL),
      new A.SSrcloc(dummyLoc, l),
      column.name,
      new A.SSrcloc(dummyLoc, column.l)
    ]);
}

export function checkHasColumn(tbl: A.Expr, tblL: Loc, col: any, colL: Loc): A.Expr {
  // (sic: desugar.arr passes tbl-l to A.s-str here)
  return new A.SApp(tblL,
    new A.SDot(dummyLoc, tbl, '_column-index'),
    [
      new A.SSrcloc(dummyLoc, tblL),
      new A.SStr(dummyLoc, tblL as any),
      new A.SSrcloc(dummyLoc, colL)
    ]);
}

export function checkNoColumn(opL: Loc, tbl: A.Expr, tblL: Loc, col: string, colL: Loc): A.Expr {
  return new A.SApp(tblL,
    new A.SDot(dummyLoc, tbl, '_no-column'),
    [
      new A.SSrcloc(dummyLoc, opL),
      new A.SSrcloc(dummyLoc, tblL),
      new A.SStr(dummyLoc, col),
      new A.SSrcloc(dummyLoc, colL)
    ]);
}

export function noBranchesExn(l: Loc, typ: string): A.Expr {
  return new A.SPrimApp(l, 'throwNoBranchesMatched', [new A.SSrcloc(l, l), new A.SStr(l, typ)], flatPrimApp);
}
export function boolExn(l: Loc, typ: string, val: A.Expr): A.Expr {
  return new A.SPrimApp(l, 'throwNonBooleanCondition', [new A.SSrcloc(l, l), new A.SStr(l, typ), val], flatPrimApp);
}
export function boolOpExn(l: Loc, position: string, typ: string, val: A.Expr): A.Expr {
  return new A.SPrimApp(l, 'throwNonBooleanOp', [new A.SSrcloc(l, l), new A.SStr(l, position), new A.SStr(l, typ), val], flatPrimApp);
}
export function templateExn(l: Loc): A.Expr {
  return new A.SPrimApp(l, 'throwUnfinishedTemplate', [new A.SSrcloc(l, l)], flatPrimApp);
}

export function desugarAfield(f: A.AField): A.AField {
  return new A.AField(f.l, f.name, desugarAnn(f.ann));
}
export function desugarAnn(a: A.Ann): A.Ann {
  switch (a.$name) {
    case 'a-blank': return a;
    case 'a-any': return a;
    case 'a-name': return a;
    case 'a-type-var': return a;
    case 'a-dot': return a;
    case 'a-arrow':
      return new A.AArrow(a.l, a.args.map(desugarAnn), desugarAnn(a.ret), a.useParens);
    case 'a-arrow-argnames':
      return new A.AArrowArgnames(a.l, a.args.map(desugarAfield), desugarAnn(a.ret), a.useParens);
    case 'a-method':
      return new A.AArrow(a.l, a.args.map(desugarAnn), desugarAnn(a.ret), true);
    case 'a-app':
      return new A.AApp(a.l, desugarAnn(a.ann), a.args.map(desugarAnn));
    case 'a-record':
      return new A.ARecord(a.l, a.fields.map(desugarAfield));
    case 'a-tuple':
      return new A.ATuple(a.l, a.fields.map(desugarAnn));
    case 'a-pred':
      return new A.APred(a.l, desugarAnn(a.ann), desugarExpr(a.exp));
    default:
      return raise('No cases matched in desugar-ann for ' + (a as any).$name);
  }
}

export function desugar(program: A.Program): { ast: A.Program; newBinds: Map<string, C.ValueBind> } {
  /*
    Desugar non-scope and non-check based constructs.
    Preconditions on program:
      - well-formed
      - contains no s-var, s-fun, s-data, s-check, or s-check-test
      - contains no s-provide in headers
      - all where blocks are none
      - contains no s-name (e.g. call resolve-names first)
    Postconditions on program:
      - in addition to preconditions,
        contains no s-for, s-if (will all be s-if-else), s-op, s-method-field,
                    s-cases (will all be s-cases-else), s-not, s-when, s-if-pipe, s-paren
      - contains no s-underscore in expression position (but it may
        appear in binding positions as in s-let-bind, s-letrec-bind)
  */
  generatedBinds = new Map();
  return {
    ast: new A.SProgram(program.l, program._use, program._provide, program.providedTypes,
      program.provides, program.imports, desugarExpr(program.block)),
    newBinds: generatedBinds
  };
}

export interface MkId { id: A.SAtom; idB: A.SBind; idE: A.SId }
export interface MkIdVar { id: A.SAtom; idB: A.SBind; idE: A.SIdVar }

export function mkIdAnn(loc: Loc, base: string, ann: A.Ann): MkId {
  const a = names.makeAtom(base);
  generatedBinds.set(a.key(), new C.ValueBind(C.boLocal(loc, a), C.vbLet, a, ann));
  return { id: a, idB: new A.SBind(loc, false, a, ann), idE: new A.SId(loc, a) };
}

export function mkIdVarAnn(loc: Loc, base: string, ann: A.Ann): MkIdVar {
  const a = names.makeAtom(base);
  generatedBinds.set(a.key(), new C.ValueBind(C.boLocal(loc, a), C.vbVar, a, ann));
  return { id: a, idB: new A.SBind(loc, false, a, ann), idE: new A.SIdVar(loc, a) };
}

export function mkId(loc: Loc, base: string): MkId { return mkIdAnn(loc, base, A.aBlank); }

export function mkIdVar(loc: Loc, base: string): MkIdVar { return mkIdVarAnn(loc, base, A.aBlank); }

export function getArithOp(str: string): string | undefined {
  if (str === 'op+') { return '_plus'; }
  else if (str === 'op-') { return '_minus'; }
  else if (str === 'op*') { return '_times'; }
  else if (str === 'op/') { return '_divide'; }
  else if (str === 'op<') { return '_lessthan'; }
  else if (str === 'op>') { return '_greaterthan'; }
  else if (str === 'op>=') { return '_greaterequal'; }
  else if (str === 'op<=') { return '_lessequal'; }
  else { return undefined; }
}

export function desugarIf(l: Loc, branches: (A.IfBranch | A.IfPipeBranch)[], _else: A.Expr, blocky: boolean): A.Expr {
  // for fold(acc from desugar-expr(_else), branch from branches.reverse())
  let acc = desugarExpr(_else);
  const reversed = [...branches].reverse();
  for (const branch of reversed) {
    acc = new A.SIfElse(l,
      [new A.SIfBranch(branch.l, desugarExpr(branch.test), desugarExpr(branch.body))],
      acc, blocky);
  }
  return acc;
}

export function desugarCasesBind(cb: A.CasesBind): A.CasesBind {
  return new A.SCasesBind(cb.l, cb.fieldType, desugarBind(cb.bind));
}

export function desugarCaseBranch(c: A.CasesBranch): A.CasesBranch {
  switch (c.$name) {
    case 's-cases-branch':
      return new A.SCasesBranch(c.l, c.patLoc, c.name, c.args.map(desugarCasesBind), desugarExpr(c.body));
    case 's-singleton-cases-branch':
      return new A.SSingletonCasesBranch(c.l, c.patLoc, c.name, desugarExpr(c.body));
  }
}

export function desugarVariantMember(m: A.VariantMember): A.VariantMember {
  return new A.SVariantMember(m.l, m.memberType, desugarBind(m.bind));
}

export function desugarMember(f: A.Member): A.Member {
  switch (f.$name) {
    case 's-method-field':
      return new A.SDataField(f.l, f.name,
        desugarExpr(new A.SMethod(f.l, f.name, f.params, f.args, f.ann, f.doc, f.body, f._checkLoc, f._check, f.blocky)));
    case 's-data-field':
      return new A.SDataField(f.l, f.name, desugarExpr(f.value));
    default:
      return raise('NYI(desugar-member): ' + (f as any).$name);
  }
}

export function isUnderscore(e: A.Expr): boolean {
  return A.isSId(e) && A.isSUnderscore(e.id);
}

export function dsCurryArgs(l: Loc, args: A.Expr[]): Pair<A.Bind[], A.Expr[]> {
  // Builds both lists front-to-back, matching the fold + reverse in the
  // Pyret source (mk-id calls happen in arg order).
  const params: A.Bind[] = [];
  const newArgs: A.Expr[] = [];
  for (const arg of args) {
    if (isUnderscore(arg)) {
      const argId = mkId(l, 'arg_');
      params.push(argId.idB);
      newArgs.push(argId.idE);
    } else {
      newArgs.push(arg);
    }
  }
  return pair(params, newArgs);
}

export function dsCurryNullary(rebuildNode: (l: Loc, obj: A.Expr, m: any) => A.Expr, l: Loc, obj: A.Expr, m: any): A.Expr {
  if (isUnderscore(obj)) {
    const curriedObj = mkId(l, 'recv_');
    return new A.SLam(l, '', [], [curriedObj.idB], A.aBlank, '', rebuildNode(l, curriedObj.idE, m), undefined, undefined, false);
  } else {
    return rebuildNode(l, desugarExpr(obj), m);
  }
}

export function dsCurryBinop(s: Loc, e1: A.Expr, e2: A.Expr, rebuild: (e1: A.Expr, e2: A.Expr) => A.Expr): A.Expr {
  const paramsAndArgs = dsCurryArgs(s, [e1, e2]);
  const params = paramsAndArgs.left;
  if (params.length === 0) {
    return rebuild(e1, e2);
  } else {
    const curryArgs = paramsAndArgs.right;
    return new A.SLam(s, '', [], params, A.aBlank, '', rebuild(curryArgs[0], curryArgs[1]), undefined, undefined, false);
  }
}

export function dsCurry(l: Loc, f: A.Expr, args: A.Expr[]): A.Expr {
  function fallthrough(): A.Expr {
    const paramsAndArgs = dsCurryArgs(l, args);
    const params = paramsAndArgs.left;
    if (isUnderscore(f)) {
      const fId = mkId(l, 'f_');
      return new A.SLam(l, '', [], [fId.idB, ...params], A.aBlank, '', new A.SApp(l, fId.idE, paramsAndArgs.right), undefined, undefined, false);
    } else {
      const dsF = desugarExpr(f);
      if (params.length === 0) { return new A.SApp(l, dsF, args); }
      else { return new A.SLam(l, '', [], params, A.aBlank, '', new A.SApp(l, dsF, paramsAndArgs.right), undefined, undefined, false); }
    }
  }
  if (A.isSDot(f)) {
    if (isUnderscore(f.obj)) {
      const curriedObj = mkId(l, 'recv_');
      const paramsAndArgs = dsCurryArgs(l, args);
      const params = paramsAndArgs.left;
      return new A.SLam(l, '', [], [curriedObj.idB, ...params], A.aBlank, '',
        new A.SApp(l, new A.SDot(l, curriedObj.idE, f.field), paramsAndArgs.right), undefined, undefined, false);
    } else {
      return fallthrough();
    }
  } else {
    return fallthrough();
  }
}

export function desugarOpt<T>(f: (v: T) => T, opt: T | undefined): T | undefined {
  if (opt === undefined) { return undefined; }
  else { return f(opt); }
}

export function desugarBind(b: A.Bind): A.Bind {
  if (A.isSBind(b)) {
    return new A.SBind(b.l, b.shadows, b.id, desugarAnn(b.ann));
  } else {
    return raise('Non-bind given to desugar-bind: ' + (b as any).$name);
  }
}

export function desugarLetBinds(binds: A.LetBind[]): A.LetBind[] {
  return binds.map((bind) => {
    switch (bind.$name) {
      case 's-let-bind':
        return new A.SLetBind(bind.l, desugarBind(bind.b), desugarExpr(bind.value));
      case 's-var-bind':
        return new A.SVarBind(bind.l, desugarBind(bind.b), desugarExpr(bind.value));
    }
  });
}

export function desugarLetrecBinds(binds: A.LetrecBind[]): A.LetrecBind[] {
  return binds.map((bind) =>
    new A.SLetrecBind(bind.l, desugarBind(bind.b), desugarExpr(bind.value)));
}

function desugarScopeEntry(entry: A.ScopeEntry): A.ScopeEntry {
  if (A.isSScopeLet(entry)) {
    return new A.SScopeLet(entry.l, desugarLetBinds(entry.binds));
  } else if (A.isSScopeTypeLet(entry)) {
    return new A.SScopeTypeLet(entry.l, entry.binds.map(desugarTypeLetBind));
  } else if (A.isSScopeLetrec(entry)) {
    return new A.SScopeLetrec(entry.l, desugarLetrecBinds(entry.binds));
  } else {
    return new A.SScopeStmt(entry.l, desugarExpr(entry.stmt));
  }
}

// The desugaring of a dot/app chain (`o.m(1).n(2)…`) recurses once per link
// through desugarExpr -> dsCurry -> dsCurryNullary, so chain LENGTH became
// stack depth. This walker consumes the whole receiver spine with an explicit
// frame stack instead: descend doing exactly the work the recursive path did
// before it descended (args, then dsCurryArgs, in that order -- mkId order is
// observable in the output), then rebuild bottom-up. Each arm mirrors a
// branch of dsCurry/dsCurryNullary; those stay as-is for their other callers.
type ChainFrame =
  | { kind: 'dot', l: Loc, field: string }
  | { kind: 'app', l: Loc, dsArgs: A.Expr[], params: A.Bind[], curryArgs: A.Expr[], dot?: { l: Loc, field: string } };

function desugarDotAppChain(expr: A.Expr): A.Expr {
  const frames: ChainFrame[] = [];
  let cur: A.Expr = expr;
  let acc: A.Expr;
  for (;;) {
    if (A.isSApp(cur)) {
      const l = cur.l;
      const dsArgs = cur.args.map(desugarExpr);
      const f = cur._fun;
      if (A.isSDot(f) && isUnderscore(f.obj)) {
        // dsCurry's curried-receiver branch: mkId, THEN dsCurryArgs
        const curriedObj = mkId(l, 'recv_');
        const pa = dsCurryArgs(l, dsArgs);
        acc = new A.SLam(l, '', [], [curriedObj.idB, ...pa.left], A.aBlank, '',
          new A.SApp(l, new A.SDot(l, curriedObj.idE, f.field), pa.right), undefined, undefined, false);
        break;
      }
      // dsCurry's fallthrough: dsCurryArgs before anything touches f
      const pa = dsCurryArgs(l, dsArgs);
      if (isUnderscore(f)) {
        const fId = mkId(l, 'f_');
        acc = new A.SLam(l, '', [], [fId.idB, ...pa.left], A.aBlank, '',
          new A.SApp(l, fId.idE, pa.right), undefined, undefined, false);
        break;
      }
      if (A.isSDot(f)) {
        frames.push({ kind: 'app', l, dsArgs, params: pa.left, curryArgs: pa.right, dot: { l: f.l, field: f.field } });
        cur = f.obj;
        continue;
      }
      frames.push({ kind: 'app', l, dsArgs, params: pa.left, curryArgs: pa.right });
      cur = f;
      continue;
    }
    if (A.isSDot(cur)) {
      if (isUnderscore(cur.obj)) {
        // dsCurryNullary's curried-receiver branch
        const curriedObj = mkId(cur.l, 'recv_');
        acc = new A.SLam(cur.l, '', [], [curriedObj.idB], A.aBlank, '',
          new A.SDot(cur.l, curriedObj.idE, cur.field), undefined, undefined, false);
        break;
      }
      frames.push({ kind: 'dot', l: cur.l, field: cur.field });
      cur = cur.obj;
      continue;
    }
    acc = desugarExpr(cur);
    break;
  }
  for (let i = frames.length - 1; i >= 0; i--) {
    const fr = frames[i];
    if (fr.kind === 'dot') {
      acc = new A.SDot(fr.l, acc, fr.field);
    } else {
      const dsF = fr.dot !== undefined ? new A.SDot(fr.dot.l, acc, fr.dot.field) : acc;
      acc = fr.params.length === 0
        ? new A.SApp(fr.l, dsF, fr.dsArgs)
        : new A.SLam(fr.l, '', [], fr.params, A.aBlank, '',
            new A.SApp(fr.l, dsF, fr.curryArgs), undefined, undefined, false);
    }
  }
  return acc;
}

function desugarTypeLetBind(tb: A.TypeLetBind): A.TypeLetBind {
  switch (tb.$name) {
    case 's-type-bind': return new A.STypeBind(tb.l, tb.name, tb.params, desugarAnn(tb.ann));
    case 's-newtype-bind': return tb;
  }
}

// Post-resolve-scope ASTs nest one s-let-expr/s-letrec level (through an
// s-block whose LAST statement carries the rest) per binding group, so a
// flat script's statement count became desugar's recursion depth. Walk that
// body spine iteratively: binds (and the block's leading statements) are
// desugared on the way down, in source order, exactly as the recursive arms
// evaluated them; the nodes are rebuilt on the way back up.
export function desugarExpr(expr: A.Expr): A.Expr {
  switch (expr.$name) {
    case 's-module': {
      return new A.SModule(expr.l, desugarExpr(expr.answer), expr.definedModules, expr.definedValues,
        expr.definedTypes, desugarExpr(expr.checks));
    }
    case 's-instantiate':
      return new A.SInstantiate(expr.l, desugarExpr(expr.expr), expr.params.map(desugarAnn));
    case 's-block':
      return new A.SBlock(expr.l, expr.stmts.map(desugarExpr));
    case 's-scope-block':
      return new A.SScopeBlock(expr.l, expr.entries.map(desugarScopeEntry), desugarExpr(expr.tail));
    case 's-user-block':
      return desugarExpr(expr.body);
    case 's-template': return expr; // template-exn(l)
    case 's-app':
      return desugarDotAppChain(expr);
    case 's-prim-app':
      return new A.SPrimApp(expr.l, expr._fun, expr.args.map(desugarExpr), expr.appInfo);
    case 's-lam':
      return new A.SLam(expr.l, expr.name, expr.params, expr.args.map(desugarBind), desugarAnn(expr.ann),
        expr.doc, desugarExpr(expr.body), expr._checkLoc, desugarOpt(desugarExpr, expr._check), expr.blocky);
    case 's-method':
      return new A.SMethod(expr.l, expr.name, expr.params, expr.args.map(desugarBind), desugarAnn(expr.ann),
        expr.doc, desugarExpr(expr.body), expr._checkLoc, desugarOpt(desugarExpr, expr._check), expr.blocky);
    case 's-type': return new A.SType(expr.l, expr.name, expr.params, desugarAnn(expr.ann));
    case 's-newtype': return expr;
    case 's-type-let-expr':
    case 's-let-expr':
    case 's-letrec':
      return eliminatedScopeForm(expr);
    case 's-data-expr': {
      const extendVariant = (v: A.Variant): A.Variant => {
        switch (v.$name) {
          case 's-variant':
            return new A.SVariant(
              v.l,
              v.constrLoc,
              v.name,
              v.members.map(desugarVariantMember),
              v.withMembers.map(desugarMember));
          case 's-singleton-variant':
            return new A.SSingletonVariant(
              v.l,
              v.name,
              v.withMembers.map(desugarMember));
        }
      };
      return new A.SDataExpr(expr.l, expr.name, expr.namet, expr.params, expr.mixins.map(desugarExpr),
        expr.variants.map(extendVariant), expr.sharedMembers.map(desugarMember), expr._checkLoc,
        desugarOpt(desugarExpr, expr._check));
    }
    case 's-when': {
      const l = expr.l;
      const dsTest = desugarExpr(expr.test);
      const gNothing = gid(l, 'nothing');
      const dsBody = desugarExpr(expr.block);
      return new A.SIfElse(l,
        [
          new A.SIfBranch(l, dsTest, A.isSBlock(expr.block)
            ? new A.SBlock(l, [...field<A.Expr[]>(dsBody, 'stmts'), gNothing])
            : new A.SBlock(l, [dsBody, gNothing]))
        ],
        new A.SBlock(l, [gNothing]),
        expr.blocky);
    }
    case 's-if':
      return desugarIf(expr.l, expr.branches, new A.SBlock(expr.l, [noBranchesExn(expr.l, 'if')]), expr.blocky);
    case 's-if-else':
      return desugarIf(expr.l, expr.branches, expr._else, expr.blocky);
    case 's-if-pipe':
      return desugarIf(expr.l, expr.branches, new A.SBlock(expr.l, [noBranchesExn(expr.l, 'ask')]), expr.blocky);
    case 's-if-pipe-else':
      return desugarIf(expr.l, expr.branches, expr._else, expr.blocky);
    case 's-cases':
      return new A.SCases(expr.l, desugarAnn(expr.typ), desugarExpr(expr.val),
        expr.branches.map(desugarCaseBranch), expr.blocky);
    case 's-cases-else':
      return new A.SCasesElse(expr.l, desugarAnn(expr.typ), desugarExpr(expr.val),
        expr.branches.map(desugarCaseBranch),
        desugarExpr(expr._else),
        expr.blocky);
    case 's-assign': return new A.SAssign(expr.l, expr.id, desugarExpr(expr.value));
    case 's-dot':
      return desugarDotAppChain(expr);
    case 's-bracket': {
      const l = expr.l;
      return dsCurryBinop(l, desugarExpr(expr.obj), desugarExpr(expr.key), (e1, e2) =>
        new A.SPrimApp(l, 'getBracket', [new A.SSrcloc(l, l), e1, e2], new A.PrimAppInfoC(true)));
    }
    case 's-get-bang':
      return dsCurryNullary((l2, obj, m) => new A.SGetBang(l2, obj, m), expr.l, expr.obj, expr.field);
    case 's-update':
      return dsCurryNullary((l2, obj, m) => new A.SUpdate(l2, obj, m), expr.l, expr.supe, expr.fields.map(desugarMember));
    case 's-extend':
      return dsCurryNullary((l2, obj, m) => new A.SExtend(l2, obj, m), expr.l, expr.supe, expr.fields.map(desugarMember));
    case 's-for': {
      const l = expr.l;
      const values = expr.bindings.map((b) => b.value).map(desugarExpr);
      const name = 'for-body<' + l.format(false) + '>';
      const theFunction = new A.SLam(l, name, [], expr.bindings.map((b) => b.bind).map(desugarBind),
        desugarAnn(expr.ann), '', desugarExpr(expr.body), undefined, undefined, expr.blocky);
      return new A.SApp(l, desugarExpr(expr.iterator), [theFunction, ...values]);
    }
    case 's-op': {
      const l = expr.l;
      const op = expr.op;
      const left = expr.left;
      const right = expr.right;
      // Unused in the .arr original too; kept voided for structural fidelity.
      const thunk = (e: A.Expr): A.Expr =>
        new A.SLam(l, '', [], [], A.aBlank, '',
          A.isSBlock(e) ? e : new A.SBlock(l, [e]),
          undefined, undefined, false);
      const opbool = (fld: string): A.Expr =>
        new A.SApp(l, new A.SDot(l, desugarExpr(left), fld), [thunk(desugarExpr(right))]);
      void opbool;
      // In-order flatten of the whole same-op region, iteratively -- a long
      // `a or b or ...` chain is list-shaped work, not stack depth.
      const collectOp = (opname: string): A.Expr[] => {
        const out: A.Expr[] = [];
        const todo: A.Expr[] = [expr];
        while (todo.length > 0) {
          const e = todo.pop() as A.Expr;
          if (A.isSOp(e) && e.op === opname) { todo.push(e.right, e.left); }
          else { out.push(e); }
        }
        return out;
      };
      if (op === 'opor' || op === 'opand') {
        // Desugar the operands left-to-right (the recursive helper's order),
        // then build the nested if from the last operand outward; only that
        // last operand is checkBool-wrapped.
        const operands = collectOp(op);
        const dsOperands = operands.map(desugarExpr);
        const lastIdx = operands.length - 1;
        let acc: A.Expr = checkBool(operands[lastIdx].l, dsOperands[lastIdx], (oper) => oper);
        for (let i = lastIdx - 1; i >= 0; i--) {
          acc = op === 'opor'
            ? new A.SIfElse(l, [new A.SIfBranch(l, dsOperands[i], new A.SBool(l, true))], acc, false)
            : new A.SIfElse(l, [new A.SIfBranch(l, dsOperands[i], acc)], new A.SBool(l, false), false);
        }
        return acc;
      } else if (op === 'op^') {
        const operands = collectOp('op^');
        let acc = desugarExpr(operands[0]);
        for (const f of operands.slice(1)) {
          acc = new A.SApp(l, desugarExpr(f), [acc]);
        }
        return acc;
      } else {
        // Binop-shaped ops (arithmetic, ==, =~, <=>, <>) parse
        // left-associated, so operand COUNT is left-spine depth. Walk the
        // spine iteratively: desugar the leftmost operand, then fold back up
        // one dsCurryBinop per level, desugaring each level's right operand
        // just before its rebuild -- the recursive evaluation order.
        const rebuildFor = (nl: Loc, nop: string): ((e1: A.Expr, e2: A.Expr) => A.Expr) | undefined => {
          const field = getArithOp(nop);
          if (field !== undefined) { return (e1, e2) => new A.SApp(nl, gid(nl, field), [e1, e2]); }
          switch (nop) {
            case 'op==': return (e1, e2) => new A.SApp(nl, gid(nl, 'equal-always'), [e1, e2]);
            case 'op=~': return (e1, e2) => new A.SApp(nl, gid(nl, 'equal-now'), [e1, e2]);
            case 'op<=>': return (e1, e2) => new A.SApp(nl, gid(nl, 'identical'), [e1, e2]);
            case 'op<>': return (e1, e2) =>
              new A.SPrimApp(nl, 'not', [new A.SApp(nl, gid(nl, 'equal-always'), [e1, e2])], flatPrimApp);
            default: return undefined;
          }
        };
        if (rebuildFor(l, op) === undefined) { return raise('No implementation for ' + op); }
        const spine: A.SOp[] = [];
        let cur: A.Expr = expr;
        while (A.isSOp(cur) && cur.op !== 'opor' && cur.op !== 'opand' && cur.op !== 'op^'
            && rebuildFor(cur.l, cur.op) !== undefined) {
          spine.push(cur);
          cur = cur.left;
        }
        let acc = desugarExpr(cur);
        for (let i = spine.length - 1; i >= 0; i--) {
          const node = spine[i];
          acc = dsCurryBinop(node.l, acc, desugarExpr(node.right),
            rebuildFor(node.l, node.op) as (e1: A.Expr, e2: A.Expr) => A.Expr);
        }
        return acc;
      }
    }
    case 's-id': return expr;
    case 's-id-modref': return expr;
    case 's-id-var-modref': return expr;
    case 's-id-var': return expr;
    case 's-id-letrec': return expr;
    case 's-srcloc': return expr;
    case 's-num': return expr;
    // num, den are exact ints, and s-frac desugars to the exact rational num/den
    case 's-frac':
      return new A.SNum(expr.l, jsnums.divide(expr.num, expr.den, throwingErrbacks)); // NOTE: Possibly must preserve further?
    // num, den are exact ints, and s-rfrac desugars to the roughnum fraction corresponding to num/den
    case 's-rfrac':
      return new A.SNum(expr.l, jsnums.toRoughnum(jsnums.divide(expr.num, expr.den, throwingErrbacks), throwingErrbacks)); // NOTE: Possibly must preserve further?
    case 's-str': return expr;
    case 's-bool': return expr;
    case 's-obj': return new A.SObj(expr.l, expr.fields.map(desugarMember));
    case 's-tuple': return new A.STuple(expr.l, expr.fields.map(desugarExpr));
    case 's-tuple-get': return new A.STupleGet(expr.l, desugarExpr(expr.tup), expr.index, expr.indexLoc);
    case 's-ref': return new A.SRef(expr.l, desugarAnn(nonNull(expr.ann)));
    case 's-construct': {
      const l = expr.l;
      const constructorVal = expr.constructorVal;
      const elts = expr.values;
      switch (expr.modifier.$name) {
        case 's-construct-normal': {
          const len = elts.length;
          const desugaredElts = elts.map(desugarExpr);
          if (len <= 5) {
            return new A.SApp(constructorVal.l,
              new A.SPrimApp(constructorVal.l, 'getMaker' + String(len),
                [desugarExpr(constructorVal), new A.SStr(dummyLoc, 'make' + String(len)),
                  new A.SSrcloc(dummyLoc, l), new A.SSrcloc(dummyLoc, constructorVal.l)], flatPrimApp),
              desugaredElts);
          } else {
            return new A.SApp(constructorVal.l,
              new A.SPrimApp(constructorVal.l, 'getMaker',
                [desugarExpr(constructorVal), new A.SStr(dummyLoc, 'make'),
                  new A.SSrcloc(dummyLoc, l), new A.SSrcloc(dummyLoc, constructorVal.l)], flatPrimApp),
              [new A.SArray(l, desugaredElts)]);
          }
        }
        case 's-construct-lazy':
          return new A.SApp(constructorVal.l,
            new A.SPrimApp(constructorVal.l, 'getLazyMaker',
              [desugarExpr(constructorVal), new A.SStr(dummyLoc, 'lazy-make'),
                new A.SSrcloc(dummyLoc, l), new A.SSrcloc(dummyLoc, constructorVal.l)], flatPrimApp),
            [new A.SArray(l,
              elts.map((elt) => desugarExpr(new A.SLam(elt.l, '', [], [], A.aBlank, '', elt, undefined, undefined, false))))]);
      }
    }
    case 's-reactor': {
      const l = expr.l;
      const fieldsByName = new Map<string, A.Expr>();
      const initAndNonInit = partition((f: A.Member) => {
        if (f.name !== 'init') { fieldsByName.set(f.name, field(f, 'value')); }
        return f.name === 'init';
      }, expr.fields);
      const init = field(initAndNonInit.isTrue[0], 'value');
      const nonInitFields = initAndNonInit.isFalse;
      void nonInitFields;
      const fieldNames = C.reactorOptionalFields;
      const optionFields: A.Member[] = [];
      for (const f of [...fieldNames.keys()].sort()) {
        if (fieldsByName.has(f)) {
          const thisField = fieldsByName.get(f)!;
          const thisFieldL = thisField.l;
          optionFields.push(new A.SDataField(thisFieldL, f, new A.SPrimApp(thisFieldL, 'makeSome',
            [new A.SCheckExpr(thisFieldL, desugarExpr(thisField), fieldNames.get(f)!(thisFieldL))],
            flatPrimApp)));
        } else {
          optionFields.push(new A.SDataField(l, f, new A.SPrimApp(l, 'makeNone', [], flatPrimApp)));
        }
      }
      return new A.SPrimApp(l, 'makeReactor', [desugarExpr(init), new A.SObj(l, optionFields)], flatPrimApp);
    }
    case 's-table': {
      const l = dummyLoc; // shadow l = A.dummy-loc
      const columnNames = expr.headers.map((header) => new A.SStr(header.l, header.name));
      const anns = expr.headers.map((header) => desugarAnn(header.ann));
      const rows = expr.rows.map((row) => {
        const elems = row.elems.map((elem, n) => checkAnn(elem.l, desugarExpr(elem), anns[n]));
        return new A.SArray(l, elems);
      });
      return new A.SPrimApp(l, 'makeTable',
        [new A.SArray(l, columnNames),
          new A.SArray(l, rows)], flatPrimApp);
    }
    case 's-paren': return desugarExpr(expr.expr);
    // NOTE(john): see preconditions; desugar-scope should have already happened
    case 's-let': return raise('s-let should have already been desugared');
    case 's-var': return raise('s-var should have already been desugared');
    // NOTE(joe): see preconditions; desugar-checks should have already happened
    case 's-check':
      return new A.SCheck(expr.l, expr.name, desugarExpr(expr.body), expr.keywordCheck);
    case 's-check-test':
      return new A.SCheckTest(expr.l, expr.op, desugarOpt(desugarExpr, expr.refinement),
        desugarExpr(expr.left), desugarOpt(desugarExpr, expr.right), desugarOpt(desugarExpr, expr.cause));
    case 's-load-table': {
      const l = expr.l;
      const dummy = dummyLoc;
      let src: A.Expr | undefined = undefined;
      let sanitizers: A.Expr[] = [];
      for (const s of expr.spec) {
        switch (s.$name) {
          case 's-sanitize': {
            // Convert to loader option
            const asOption = new A.SApp(l, bid(l, 'as-loader-option'),
              [
                new A.SStr(dummy, 'sanitizer'),
                new A.SStr(dummy, s.name.toname()),
                s.sanitizer
              ]);
            sanitizers = [asOption, ...sanitizers];
            break;
          }
          case 's-table-src':
            // Well-formedness ensures that this matches exactly once
            src = desugarExpr(s.src);
            break;
        }
      }

      if (src === undefined) {
        return raise('s-load-table missing source: Well-formedness should have failed');
      }

      const loaded = new A.SApp(l,
        new A.SDot(l, src, 'load'),
        [
          new A.SArray(dummy, expr.headers.map((h) => new A.SStr(l, h.name))),
          new A.SArray(dummy, sanitizers)
        ]);

      return new A.SApp(l, bid(l, 'open-table'), [loaded]);
    }
    case 's-table-extend': {
      // NOTE(philip): I am fairly certain that this will need to be moved
      //               to post-type-check desugaring, since the variables used
      //               by reducers is not well-typed
      const l = expr.l;
      const columnBinds = expr.columnBinds;
      const extensions = expr.extensions;
      const row = mkId(dummyLoc, 'row');
      const tbl = mkId(dummyLoc, 'table');

      const columns = columnBinds.binds.map((c) => ({
        name: new A.SStr(dummyLoc, field(field(c, 'id'), 'base')),
        l: c.l,
        idx: mkId(dummyLoc, field(field(c, 'id'), 'base')),
        val: { idB: c, idE: new A.SId(c.l, field(c, 'id')) }
      }));

      const splitExts = partition(A.isSTableExtendReducer, extensions);
      const simpleExts = splitExts.isFalse;
      void simpleExts;
      const reducerExts = splitExts.isTrue as A.STableExtendReducer[];

      const mkReducerAnn = (loc: Loc, retType: A.Ann): A.Ann => {
        const one = new A.AField(loc, 'one', new A.AArrow(loc, [new A.AAny(loc)], retType, true));
        const reduce = new A.AField(loc, 'reduce',
          new A.AArrow(loc, [retType, new A.AAny(loc)], retType, true));
        return new A.ARecord(loc, [one, reduce]);
      };

      const reducers = new Map<string, MkId>();
      const accs = new Map<string, MkIdVar>();
      for (const extension of reducerExts) {
        const reducerId = mkIdAnn(dummyLoc,
          'reducer' + extension.name,
          mkReducerAnn(extension.l, extension.ann));
        const accId = mkIdVar(dummyLoc, 'acc' + extension.name);
        reducers.set(extension.name, reducerId);
        accs.set(extension.name, accId);
      }

      let initializedReducers: A.LetBind[] | undefined;
      if (reducerExts.length === 0) {
        initializedReducers = undefined;
      } else {
        let reducersAcc: A.LetBind[] = [];
        for (const ext of reducerExts) {
          const l2 = ext.l;
          const reducer = nonNull(reducers.get(ext.name));
          const acc = nonNull(accs.get(ext.name));
          const nothingExpr = new A.SId(l2, new A.SGlobal('nothing'));
          reducersAcc = [
            new A.SLetBind(l2, reducer.idB, desugarExpr(ext.reducer)),
            new A.SVarBind(l2, acc.idB, nothingExpr),
            ...reducersAcc
          ];
        }
        initializedReducers = [...reducersAcc].reverse();
      }

      const withInitializedReducers = (body: A.Expr): A.Expr =>
        initializedReducers === undefined ? body : A.sScopeLetBlock(dummyLoc, initializedReducers, body);

      const processExtension = (isFirst: boolean) => (extension: A.TableExtendField): A.Expr => {
        switch (extension.$name) {
          case 's-table-extend-field': return desugarExpr(extension.value);
          case 's-table-extend-reducer': {
            const l2 = extension.l;
            const name = extension.name;
            const col = extension.col;
            const reducer = nonNull(reducers.get(name));
            const acc = nonNull(accs.get(name));
            // Dereferenced accumulator
            const accIdE = new A.SIdVar(acc.idE.l, acc.idE.id);
            const found = columns.find((x) => x.name.s === field(col, 's'));
            // Lift from Option monad
            const colId = found === undefined
              // Dummy values; will end up unbound
              // (TODO: Figure out how to make only one 'unbound' error show up
              // since the desugaring produces the unbound column twice)
              ? { id: col, idB: new A.SBind(l2, false, col, A.aBlank), idE: new A.SId(l2, col) }
              : found.val;
            if (isFirst) {
              return new A.SBlock(dummyLoc,
                [
                  new A.SAssign(l2, acc.id,
                    new A.SApp(l2, new A.SDot(l2, reducer.idE, 'one'), [colId.idE])),
                  new A.STupleGet(l2, accIdE, 1, l2)
                ]);
            } else {
              return new A.SBlock(dummyLoc,
                [
                  new A.SAssign(l2, acc.id,
                    new A.SApp(l2, new A.SDot(l2, reducer.idE, 'reduce'),
                      [new A.STupleGet(l2, accIdE, 0, l2), colId.idE])),
                  new A.STupleGet(l2, accIdE, 1, l2)
                ]);
            }
          }
        }
      };

      const dataPopMapfun = (first: boolean): A.Expr =>
        new A.SLam(dummyLoc, '', [], [row.idB], A.aBlank, '',
          A.sScopeLetBlock(dummyLoc,
            columns.map((column) =>
              new A.SLetBind(dummyLoc, column.val.idB,
                new A.SPrimApp(dummyLoc, 'raw_array_get',
                  [row.idE, column.idx.idE], flatPrimApp))),
            new A.SPrimApp(dummyLoc, 'raw_array_concat', [
              row.idE,
              new A.SArray(dummyLoc,
                extensions.map(processExtension(first)))], flatPrimApp)),
          undefined, undefined, true);

      const binds: A.LetBind[] = [
        new A.SLetBind(dummyLoc, tbl.idB,
          checkTable(columnBinds.table.l, desugarExpr(columnBinds.table), (t) => t)),
        // Column Index Bindings
        ...columns.map((column) =>
          new A.SLetBind(dummyLoc, column.idx.idB,
            getTableColumn(l, columnBinds.table.l, tbl.idE, column)))
      ];
      // Table Construction
      const body = new A.SBlock(dummyLoc, [
        new A.SBlock(dummyLoc, extensions.map((extension) =>
          checkNoColumn(l, tbl.idE, columnBinds.l, extension.name, extension.l))),
        new A.SPrimApp(dummyLoc, 'makeTable', [
          // Header
          new A.SPrimApp(dummyLoc, 'raw_array_concat', [
            new A.SDot(dummyLoc, tbl.idE, '_header-raw-array'),
            new A.SArray(dummyLoc, extensions.map((e) => new A.SStr(e.l, e.name)))],
          flatPrimApp),
          // Data
          withInitializedReducers(
            new A.SApp(l, new A.SId(l, new A.SGlobal('raw-array-map-1')), [
              dataPopMapfun(true),
              dataPopMapfun(false),
              new A.SDot(dummyLoc, tbl.idE, '_rows-raw-array')]))], flatPrimApp)]);
      return A.sScopeLetBlock(dummyLoc, binds, body);
    }
    case 's-table-update': {
      const l = expr.l;
      const columnBinds = expr.columnBinds;
      const row = mkId(dummyLoc, 'row');
      const newRow = mkId(dummyLoc, 'new-row-row');
      const tbl = mkId(l, 'table');

      const columns = columnBinds.binds.map((c) => ({
        name: new A.SStr(dummyLoc, field(field(c, 'id'), 'base')),
        l: c.l,
        idx: mkId(dummyLoc, field(field(c, 'id'), 'base')),
        val: { idB: c, idE: new A.SId(c.l, field(c, 'id')) }
      }));

      const updates = expr.updates.map((u) => ({
        name: new A.SStr(dummyLoc, u.name),
        l: u.l,
        idx: mkId(dummyLoc, u.name),
        val: desugarExpr(field(u, 'value'))
      }));

      const binds: A.LetBind[] = [
        new A.SLetBind(dummyLoc, tbl.idB,
          checkTable(columnBinds.table.l, desugarExpr(columnBinds.table), (t) => t)),
        // Column Index Bindings
        ...columns.map((column) =>
          new A.SLetBind(dummyLoc, column.idx.idB,
            getTableColumn(l, columnBinds.table.l, tbl.idE, column))),
        ...updates.map((update) =>
          new A.SLetBind(dummyLoc, update.idx.idB,
            getTableColumn(l, columnBinds.table.l, tbl.idE, update)))
      ];
      // Table Construction
      const body = new A.SPrimApp(dummyLoc, 'makeTable', [
        // Header
        new A.SDot(dummyLoc, tbl.idE, '_header-raw-array'),
        // Data
        new A.SApp(l, new A.SId(dummyLoc, g('raw-array-map')), [
          new A.SLam(dummyLoc, '', [], [row.idB], A.aBlank, '',
            A.sScopeLetBlock(dummyLoc,
              [
                new A.SLetBind(dummyLoc, newRow.idB,
                  new A.SPrimApp(dummyLoc, 'raw_array_concat', [
                    row.idE, new A.SArray(dummyLoc, [])], flatPrimApp)),
                ...columns.map((column) =>
                  new A.SLetBind(dummyLoc, column.val.idB,
                    new A.SPrimApp(dummyLoc, 'raw_array_get',
                      [newRow.idE, column.idx.idE], flatPrimApp)))
              ],
              A.sScopeLetBlock(dummyLoc,
                updates.map((update) =>
                  new A.SLetBind(dummyLoc, newRow.idB,
                    new A.SPrimApp(dummyLoc, 'raw_array_set', [
                      newRow.idE, update.idx.idE, update.val], flatPrimApp))),
                newRow.idE)), undefined, undefined, true),
          new A.SDot(dummyLoc, tbl.idE, '_rows-raw-array')])],
      flatPrimApp);
      return A.sScopeLetBlock(dummyLoc, binds, body);
    }
    case 's-table-select': {
      const l = expr.l;
      const row = mkId(dummyLoc, 'row');
      const tbl = mkId(l, 'table');
      const columns = expr.columns.map((c) => ({
        l: field(c, 'l'),
        idx: mkId(field(c, 'l'), field(c, 's')),
        name: new A.SStr(field(c, 'l'), field(c, 's'))
      }));
      const binds: A.LetBind[] = [
        new A.SLetBind(dummyLoc, tbl.idB,
          checkTable(expr.table.l, desugarExpr(expr.table), (t) => t)),
        // Column Index Bindings
        ...columns.map((column) =>
          new A.SLetBind(dummyLoc, column.idx.idB,
            getTableColumn(l, expr.table.l, tbl.idE, column)))
      ];
      // Table Construction
      const body = new A.SPrimApp(dummyLoc, 'makeTable', [
        // Header
        new A.SArray(dummyLoc, columns.map((c) => c.name)),
        // Data
        new A.SApp(l, new A.SId(dummyLoc, g('raw-array-map')), [
          new A.SLam(dummyLoc, '', [], [row.idB], A.aBlank, '',
            new A.SArray(dummyLoc,
              columns.map((c) =>
                new A.SPrimApp(dummyLoc, 'raw_array_get',
                  [row.idE, c.idx.idE], flatPrimApp))), undefined, undefined, true),
          new A.SDot(dummyLoc, tbl.idE, '_rows-raw-array')])], flatPrimApp);
      return A.sScopeLetBlock(dummyLoc, binds, body);
    }
    case 's-table-extract': {
      const l = expr.l;
      const column = expr.column;
      const table = expr.table;
      const tbl = mkId(table.l, 'table');
      const col = mkId(dummyLoc, field(column, 's'));
      const row = mkId(dummyLoc, field(column, 's'));
      return A.sScopeLetBlock(dummyLoc, [
        new A.SLetBind(dummyLoc, tbl.idB,
          checkTable(table.l, desugarExpr(table), (t) => t)),
        new A.SLetBind(dummyLoc, col.idB,
          getTableColumn(l, table.l, tbl.idE, { l: field(column, 'l'), name: new A.SStr(dummyLoc, field(column, 's')) }))],
        // Table Construction
        new A.SPrimApp(dummyLoc, 'raw_array_to_list', [
          new A.SApp(l, new A.SId(dummyLoc, g('raw-array-map')), [
            new A.SLam(dummyLoc, '', [], [row.idB], A.aBlank, '',
              new A.SPrimApp(dummyLoc, 'raw_array_get', [row.idE, col.idE], flatPrimApp), undefined, undefined, true),
            new A.SDot(dummyLoc, tbl.idE, '_rows-raw-array')])], flatPrimApp));
    }
    case 's-table-order': {
      const l = expr.l;
      const orderingRawArr = expr.ordering.map((o) =>
        new A.SArray(o.l, [new A.SBool(o.l, A.isASCENDING(o.direction)), new A.SStr(o.l, field(o.column, 's'))]));
      return new A.SApp(l,
        new A.SDot(dummyLoc, desugarExpr(expr.table), 'multi-order'),
        [new A.SArray(dummyLoc, orderingRawArr)]);
    }
    case 's-table-filter': {
      const l = expr.l;
      const columnBinds = expr.columnBinds;
      const predicate = expr.predicate;
      const row = mkId(dummyLoc, 'row');
      const tbl = mkId(l, 'table');
      const predRes = mkIdAnn(predicate.l, 'pred', new A.AName(predicate.l, new A.STypeGlobal('Boolean')));

      const columns = columnBinds.binds.map((c) => ({
        name: new A.SStr(dummyLoc, field(field(c, 'id'), 'base')),
        l: c.l,
        idx: mkId(dummyLoc, field(field(c, 'id'), 'base')),
        val: { idB: c, idE: new A.SId(c.l, field(c, 'id')) }
      }));

      const binds: A.LetBind[] = [
        new A.SLetBind(dummyLoc, tbl.idB,
          checkTable(columnBinds.table.l, desugarExpr(columnBinds.table), (t) => t)),
        // Column Index Bindings
        ...columns.map((column) =>
          new A.SLetBind(dummyLoc, column.idx.idB,
            getTableColumn(l, columnBinds.table.l, tbl.idE, column)))
      ];
      // Table Construction
      const body = new A.SPrimApp(dummyLoc, 'makeTable', [
        // Header
        new A.SDot(dummyLoc, tbl.idE, '_header-raw-array'),
        // Data
        new A.SApp(l, new A.SId(dummyLoc, g('raw-array-filter')), [
          new A.SLam(dummyLoc, '', [], [row.idB], A.aBlank, '',
            A.sScopeLetBlock(dummyLoc,
              columns.map((column) =>
                new A.SLetBind(dummyLoc, column.val.idB,
                  new A.SPrimApp(dummyLoc, 'raw_array_get',
                    [row.idE, column.idx.idE], flatPrimApp))),
              A.sScopeLetBlock(dummyLoc,
                [new A.SLetBind(predicate.l, predRes.idB, desugarExpr(predicate))],
                predRes.idE)), undefined, undefined, true),
          new A.SDot(dummyLoc, tbl.idE, '_rows-raw-array')])],
      flatPrimApp);
      return A.sScopeLetBlock(dummyLoc, binds, body);
    }
    case 's-spy-block': {
      const l = expr.l;
      const dsMessage = expr.message === undefined ? new A.SStr(l, '') : desugarExpr(expr.message);
      const dsContentsList: [A.Expr, A.Expr, A.Expr][] = expr.contents.map((spyExp) =>
        [new A.SSrcloc(spyExp.l, spyExp.l), new A.SStr(spyExp.l, spyExp.name), desugarExpr(spyExp.value)] as [A.Expr, A.Expr, A.Expr]);
      // foldr that conses each component onto the front == forward order
      const locs: A.Expr[] = [];
      const nms: A.Expr[] = [];
      const vals: A.Expr[] = [];
      for (const dsContent of dsContentsList) {
        locs.push(dsContent[0]);
        nms.push(dsContent[1]);
        vals.push(dsContent[2]);
      }
      return new A.SApp(l, bid(l, 'spy'),
        [new A.SSrcloc(l, l), dsMessage,
          new A.SArray(l, locs), new A.SArray(l, nms), new A.SArray(l, vals)]);
    }
    case 's-prim-val': return expr;
    case 's-array': return new A.SArray(expr.l, expr.values.map(desugarExpr));
    default:
      return raise('NYI (desugar): ' + (expr as any).$name);
  }
}
