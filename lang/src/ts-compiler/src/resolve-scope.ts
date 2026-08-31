/*
  Ported from: src/arr/compiler/resolve-scope.arr
*/

import * as A from './ast';
import * as SL from './srcloc';
import * as C from './compile-structs';
import * as CE from './compile-errors';
import type { CompileError } from './compile-errors';
import * as U from './ast-util';
import * as T from './type-structs';
import { DefaultMapVisitor, PostScopeMapVisitor, PostScopeIterVisitor } from './ast-visitors';
import { InternalCompilerError, mapSet, mapGetValue, toRepr, field, asVariant } from './shared';

export type ValueBind = C.ValueBind;
export type TypeBind = C.TypeBind;
export type NameResolution = C.NameResolution;

export const mtd: Map<string, any> = new Map();

export const names = A.globalNames;

// lists.take-while: splits a list at the first element failing the predicate.
function takeWhile<X>(pred: (x: X) => boolean, lst: X[]): [X[], X[]] {
  const taken: X[] = [];
  for (let i = 0; i < lst.length; i++) {
    if (pred(lst[i])) {
      taken.push(lst[i]);
    } else {
      return [taken, lst.slice(i)];
    }
  }
  return [taken, []];
}

export function mkBind(l: C.Loc, id: A.Name): A.Bind { return new A.SBind(l, false, id, A.aBlank); }

export function mkId(loc: C.Loc, base: string): { id: A.Name; idB: A.Bind; idE: A.Expr } {
  const t = new A.SName(loc, base);
  return { id: t, idB: mkBind(loc, t), idE: new A.SId(loc, t) };
}

export function desugarToplevelTypes(stmts: A.Expr[]): { typeBinds: A.TypeLetBind[]; stmts: A.Expr[] } {
  // Treating stmts as a toplevel block, hoist any newtype declarations
  // to the top, and generate newtypes for all data expressions.
  // NOTE: the Pyret original accumulates reversed lists and reverses them at the
  // end; here we just push in order (the reversed intermediates are unobserved).
  const typeBinds: A.TypeLetBind[] = [];
  const newStmts: A.Expr[] = [];
  for (const s of stmts) {
    if (A.isSType(s)) {
      newStmts.push(s);
    } else if (A.isSNewtype(s)) {
      typeBinds.push(new A.SNewtypeBind(s.l, s.name, s.namet));
    } else if (A.isSData(s)) {
      const namet = names.makeAtom(s.name);
      typeBinds.push(new A.SNewtypeBind(s.l, new A.SName(s.l, s.name), namet));
      newStmts.push(new A.SDataExpr(s.l, s.name, namet, s.params, s.mixins, s.variants,
        s.sharedMembers, s._checkLoc, s._check));
    } else {
      newStmts.push(s);
    }
  }
  return { typeBinds, stmts: newStmts };
}

export function prependScopeEntries(l: C.Loc, entries: A.ScopeEntry[], body: A.Expr): A.Expr {
  if (entries.length === 0) {
    return body;
  } else if (A.isSScopeBlock(body)) {
    return new A.SScopeBlock(l, [...entries, ...body.entries], body.tail);
  } else {
    return new A.SScopeBlock(l, entries, body);
  }
}

export const isSContract = A.isSContract;
export type Contract = A.SContract;

// Note: binds are maintained in reversed order, for efficiency in adding new items to them
// They get reversed back to correct order in bind-wrap.
export class LetBinds {
  get $name(): 'let-binds' { return 'let-binds'; }
  constructor(public contracts: Contract[], public binds: A.LetBind[]) {}
}
export class LetrecBinds {
  get $name(): 'letrec-binds' { return 'letrec-binds'; }
  constructor(public contracts: Contract[], public binds: A.LetrecBind[]) {}
}
export class TypeLetBinds {
  get $name(): 'type-let-binds' { return 'type-let-binds'; }
  constructor(public binds: A.TypeLetBind[]) {}
}
export type BindingGroup = LetBinds | LetrecBinds | TypeLetBinds;
export function isLetBinds(x: any): x is LetBinds { return x instanceof LetBinds; }
export function isLetrecBinds(x: any): x is LetrecBinds { return x instanceof LetrecBinds; }
export function isTypeLetBinds(x: any): x is TypeLetBinds { return x instanceof TypeLetBinds; }

export let errors: CompileError[] = []; // THE MUTABLE LIST OF ERRORS

export function weaveContracts(contracts: Contract[], revBinds: any[]): any[] {
  // When weaving contracts, ensure that the contract location is before the definition
  // or else give a well-formedness error.
  // NOTE(Ben): This code is polymorphic over LetBind and LetrecBind
  const contractsSd = new Map<string, Contract>();
  for (const c of contracts) {
    const name = c.name.toname();
    const c2 = contractsSd.get(name);
    if (c2 === undefined) {
      contractsSd.set(name, c);
    } else {
      errors = [new CE.ContractRedefined(c.l, name, c2.l), ...errors];
    }
  }
  function rebuildBind(bind: any, newB: A.Bind, newV: A.Expr): any {
    if (A.isSLetBind(bind)) { return new A.SLetBind(bind.l, newB, newV); }
    else if (A.isSVarBind(bind)) { return new A.SVarBind(bind.l, newB, newV); }
    else if (A.isSLetrecBind(bind)) { return new A.SLetrecBind(bind.l, newB, newV); }
    throw new InternalCompilerError("rebuild-bind: no branches matched");
  }
  function namesMatch(funargs: A.Bind[], annargs: A.AField[]): boolean {
    if (funargs.length === 0 && annargs.length === 0) { return true; }
    else if (funargs.length === 0 || annargs.length === 0) { return false; }
    else {
      return (field(funargs[0], 'id').toname() === annargs[0].name)
        && namesMatch(funargs.slice(1), annargs.slice(1));
    }
  }
  function paramsMatch(funargs: A.Name[], cargs: A.Name[]): boolean {
    if (funargs.length === 0 && cargs.length === 0) { return true; }
    else if (funargs.length === 0 || cargs.length === 0) { return false; }
    else {
      return (funargs[0].toname() === cargs[0].toname())
        && paramsMatch(funargs.slice(1), cargs.slice(1));
    }
  }
  function funToLam(bind: any): any {
    let newV: A.Expr;
    if (A.isSFun(bind.value)) {
      const f = bind.value as A.SFun;
      newV = new A.SLam(f.l, f.name, f.params, f.args, f.ann, f.doc, f.body, f._checkLoc, f._check, f.blocky);
    } else {
      newV = bind.value;
    }
    if (A.isSLetBind(bind)) { return new A.SLetBind(bind.l, bind.b, newV); }
    else if (A.isSVarBind(bind)) { return new A.SVarBind(bind.l, bind.b, newV); }
    else if (A.isSLetrecBind(bind)) { return new A.SLetrecBind(bind.l, bind.b, newV); }
    throw new InternalCompilerError("fun-to-lam: no branches matched");
  }

  function isBlankContract(a: A.Ann): boolean {
    if (A.isABlank(a)) { return true; }
    else if (A.isATuple(a)) {
      return a.fields.every((elt) => isBlankContract(elt));
    } else {
      return false;
    }
  }

  let ans: any[] = [];
  for (const bind of revBinds) {
    if (A.isSBind(bind.b)) {
      const sb = bind.b as A.SBind;
      const l = sb.l;
      const shadows = sb.shadows;
      const id = sb.id;
      const ann = sb.ann;
      const idName = id.toname();
      const c = contractsSd.get(idName);
      if (c === undefined) {
        ans = [funToLam(bind), ...ans];
      } else {
        contractsSd.delete(idName);
        if (A.isABlank(ann)) {
          if (!c.l.before(bind.value.l)) {
            errors = [new CE.ContractBadLoc(c.l, idName, bind.value.l), ...errors];
            ans = [funToLam(bind), ...ans];
          } else {
            if (A.isSFun(bind.value)) {
              const fv = bind.value as A.SFun;
              const lFun = fv.l;
              const name = fv.name;
              const params = fv.params;
              const args = fv.args;
              const ret = fv.ann;
              const doc = fv.doc;
              const body = fv.body;
              const _checkLoc = fv._checkLoc;
              const _check = fv._check;
              const blocky = fv.blocky;
              if (!(args.every((a) => isBlankContract(field(a, 'ann'))) && A.isABlank(ret))) {
                errors = [new CE.ContractRedefined(c.l, idName, lFun), ...errors];
                ans = [funToLam(bind), ...ans];
              } else if (A.isAArrow(c.ann)) {
                const cAnn = c.ann as A.AArrow;
                let okParams: boolean;
                if (params.length > 0 && !paramsMatch(c.params, params)) {
                  errors = [new CE.ContractInconsistentParams(c.l, idName, lFun), ...errors];
                  okParams = false;
                } else {
                  okParams = true;
                }
                let okArgs: boolean;
                if (cAnn.args.length !== args.length) {
                  errors = [new CE.ContractInconsistentNames(c.l, idName, lFun), ...errors];
                  okArgs = false;
                } else {
                  okArgs = true;
                }
                if (okParams && okArgs) {
                  const newArgs = args.map((a, i) => {
                    const ab = asVariant(a, A.SBind);
                    return new A.SBind(ab.l, ab.shadows, ab.id, cAnn.args[i]);
                  });
                  ans = [
                    rebuildBind(bind,
                      bind.b,
                      new A.SLam(l, name, c.params, newArgs, cAnn.ret, doc, body, _checkLoc, _check, blocky)),
                    ...ans];
                } else {
                  ans = [funToLam(bind), ...ans];
                }
              } else if (A.isAArrowArgnames(c.ann)) {
                const cAnn = c.ann as A.AArrowArgnames;
                let okParams: boolean;
                if (params.length > 0 && !paramsMatch(c.params, params)) {
                  errors = [new CE.ContractInconsistentParams(c.l, idName, lFun), ...errors];
                  okParams = false;
                } else {
                  okParams = true;
                }
                let okArgs: boolean;
                if (!namesMatch(args, cAnn.args)) {
                  errors = [new CE.ContractInconsistentNames(c.l, idName, l), ...errors];
                  okArgs = false;
                } else {
                  okArgs = true;
                }
                if (okParams && okArgs) {
                  const newArgs = args.map((a, i) => {
                    const ab = asVariant(a, A.SBind);
                    return new A.SBind(ab.l, ab.shadows, ab.id, cAnn.args[i].ann);
                  });
                  ans = [
                    rebuildBind(bind,
                      bind.b,
                      new A.SLam(l, name, c.params, newArgs, cAnn.ret, doc, body, _checkLoc, _check, blocky)),
                    ...ans];
                } else {
                  ans = [funToLam(bind), ...ans];
                }
              } else {
                errors = [new CE.ContractNonFunction(c.l, idName, l, true), ...errors];
                ans = [funToLam(bind), ...ans];
              }
            } else {
              if (A.isAArrow(c.ann) || A.isAArrowArgnames(c.ann)) {
                errors = [new CE.ContractNonFunction(c.l, idName, bind.value.l, false), ...errors];
                ans = [bind, ...ans];
              } else {
                ans = [rebuildBind(bind, new A.SBind(l, shadows, id, c.ann), bind.value), ...ans];
              }
            }
          }
        } else {
          errors = [new CE.ContractRedefined(c.l, idName, l), ...errors];
          ans = [funToLam(bind), ...ans];
        }
      }
    } else {
      ans = [bind, ...ans];
    }
  }
  for (const cName of [...contractsSd.keys()].sort()) {
    const c = mapGetValue(contractsSd, cName);
    errors = [new CE.ContractUnused(c.l, cName), ...errors];
  }
  return ans;
}

function addLetrecBind(bg: BindingGroup, lrb: A.LetrecBind, stmts: A.Expr[]): DsbStep {
  return addLetrecBinds(bg, [lrb], stmts);
}

function addLetrecBinds(bg: BindingGroup, lrbs: A.LetrecBind[], stmts: A.Expr[]): DsbStep {
  if (isLetrecBinds(bg)) {
    return dsbDefer(stmts, new LetrecBinds(bg.contracts, [...lrbs, ...bg.binds]));
  } else {
    return dsbDeferGroup(bg, stmts, new LetrecBinds([], lrbs));
  }
}

export function simplifyLetBind(
  rebuild: (l: C.Loc, b: A.Bind, e: A.Expr) => any,
  l: C.Loc, bind: A.Bind, expr: A.Expr, lbs: any[]
): any[] {
  if (A.isSBind(bind)) {
    return [rebuild(l, bind, expr), ...lbs];
  } else if (A.isSTupleBind(bind)) {
    const lb = bind.l;
    const fields = bind.fields;
    const asName = bind.asName;
    let boundExpr: A.Expr;
    let binding: any;
    if (asName === undefined) {
      const name = names.makeAtom("tup");
      const ann = new A.ATuple(lb, fields.map((f) => {
        if (A.isSBind(f)) { return f.ann; }
        else { return A.aBlank; }
      }));
      boundExpr = new A.SId(lb, name);
      binding = rebuild(lb, new A.SBind(lb, false, name, ann), expr);
    } else {
      const b = asVariant(asName, A.SBind);
      let asBinding: A.Bind;
      if (A.isABlank(b.ann)) {
        // just create a tuple of the correct arity; leave the field annotations to be checked later
        const ann = new A.ATuple(lb, fields.map(() => A.aBlank));
        asBinding = new A.SBind(b.l, b.shadows, b.id, ann);
      } else {
        asBinding = b;
      }
      boundExpr = new A.SId(b.l, b.id);
      binding = rebuild(l, asBinding, expr);
    }
    let acc = [binding, ...lbs];
    let n = 0;
    for (const f of fields) {
      acc = simplifyLetBind(rebuild, f.l, f, new A.STupleGet(f.l, boundExpr, n, f.l), acc);
      n = n + 1;
    }
    return acc;
  }
  throw new InternalCompilerError("simplify-let-bind: no cases matched");
}

const mkSLetBind = (l: C.Loc, b: A.Bind, v: A.Expr) => new A.SLetBind(l, b, v);
const mkSVarBind = (l: C.Loc, b: A.Bind, v: A.Expr) => new A.SVarBind(l, b, v);

function addLetBinds(bg: BindingGroup, lbs: A.LetBind[], stmts: A.Expr[]): DsbStep {
  let simplifiedLbs: any[] = [];
  for (const lb of lbs) {
    if (A.isSLetBind(lb)) {
      simplifiedLbs = simplifyLetBind(mkSLetBind, lb.l, lb.b, lb.value, simplifiedLbs);
    } else if (A.isSVarBind(lb)) {
      simplifiedLbs = simplifyLetBind(mkSVarBind, lb.l, lb.b, lb.value, simplifiedLbs);
    } else {
      throw new InternalCompilerError("add-let-binds: no cases matched");
    }
  }
  if (isLetBinds(bg)) {
    return dsbDefer(stmts, new LetBinds(bg.contracts, [...simplifiedLbs, ...bg.binds]));
  } else {
    return dsbDeferGroup(bg, stmts, new LetBinds([], simplifiedLbs));
  }
}

function addLetBind(bg: BindingGroup, lb: A.LetBind, stmts: A.Expr[]): DsbStep {
  return addLetBinds(bg, [lb], stmts);
}

function addTypeLetBind(bg: BindingGroup, tlb: A.TypeLetBind, stmts: A.Expr[]): DsbStep {
  if (isTypeLetBinds(bg)) {
    return dsbDefer(stmts, new TypeLetBinds([tlb, ...bg.binds]));
  } else {
    return dsbDeferGroup(bg, stmts, new TypeLetBinds([tlb]));
  }
}

function addContracts(bg: BindingGroup, cs: Contract[], stmts: A.Expr[]): DsbStep {
  // The type of the next statement determines which binding group this contract belongs in
  if (stmts.length === 0) {
    // NOTE(Ben): would rather raise an informative error than a "no cases matched" error,
    // if somehow this invariant is violated
    throw new InternalCompilerError("Impossible: well-formedness prohibits contracts being last in block (at " + String(cs[0].l) + ")");
  } else {
    const first = stmts[0];
    // Construct the appropriate binding group (containing just the initial contract)
    // depending on the next binding: mimics the cases logic in desugar-scope-block, but defers
    // all actual processing to that function
    if (A.isSRec(first) || A.isSFun(first) || A.isSDataExpr(first) || A.isSCheck(first)) {
      if (isLetrecBinds(bg)) { // keep the current binding group going
        return dsbDefer(stmts, new LetrecBinds([...cs, ...bg.contracts], bg.binds));
      } else {
        return dsbDeferGroup(bg, stmts, new LetrecBinds(cs, []));
      }
    } else {
      if (isLetBinds(bg)) { // keep the current binding group going
        return dsbDefer(stmts, new LetBinds([...cs, ...bg.contracts], bg.binds));
      } else {
        return dsbDeferGroup(bg, stmts, new LetBinds(cs, []));
      }
    }
  }
}

/*
  The per-statement recursion through desugarScopeBlock <-> add*Bind(s) is
  one full activation per block statement (all calls are tail calls or a
  single wrap around a tail call), which overflows fixed-size stacks
  (e.g. browsers) on long blocks. desugarScopeBlock drives the per-step
  worker iteratively, and assembles the FLAT s-scope-block directly.
  Side effects happen in the same order as the nested formulation did
  them: atom generation in the worker steps, forward; contract weaving
  and unused-contract errors when groups are materialized, in the nested
  unwind order (the tail's group first, then the rest right-to-left).
*/
// One flat-block output item, listed in source order: a completed
// binding group (binds nonempty), contracts that never met a binding
// (reported as ContractUnused when reached), or a plain statement.
type DsbEmit =
  | { kind: 'group'; bg: BindingGroup }
  | { kind: 'unusedContracts'; contracts: Contract[] }
  | { kind: 'stmt'; stmt: A.Expr };
type DsbStep =
  | { kind: 'step'; emits: DsbEmit[]; stmts: A.Expr[]; bg: BindingGroup }
  | { kind: 'tail'; emits: DsbEmit[]; tail: A.Expr };

// Close a binding group into its emission: a real group, leftover
// contracts, or nothing. (TypeLetBinds has no contracts field and is
// never constructed empty.)
function dsbGroupEmits(bg: BindingGroup): DsbEmit[] {
  if (bg.binds.length > 0) {
    return [{ kind: 'group', bg }];
  }
  if (isTypeLetBinds(bg) || bg.contracts.length === 0) {
    return [];
  }
  return [{ kind: 'unusedContracts', contracts: bg.contracts }];
}

function dsbDefer(stmts: A.Expr[], bg: BindingGroup): DsbStep {
  return { kind: 'step', emits: [], stmts, bg };
}

// The binding group `closedBg` is complete (the next statement starts a
// group of a different kind); it becomes one flat entry.
function dsbDeferGroup(closedBg: BindingGroup, stmts: A.Expr[], bg: BindingGroup): DsbStep {
  return { kind: 'step', emits: dsbGroupEmits(closedBg), stmts, bg };
}

// A plain (non-binding) statement: the pending group is closed and the
// statement itself becomes an entry.
function dsbDeferStmt(closedBg: BindingGroup, stmt: A.Expr, stmts: A.Expr[]): DsbStep {
  return { kind: 'step', emits: [...dsbGroupEmits(closedBg), { kind: 'stmt', stmt }],
           stmts, bg: new LetBinds([], []) };
}

function dsbFinish(closedBg: BindingGroup, tail: A.Expr): DsbStep {
  return { kind: 'tail', emits: dsbGroupEmits(closedBg), tail };
}

// The group half of the old bindWrap: weave the group's contracts and
// build its flat entry. Only nonempty groups reach here (dsbGroupEmits
// resolves empty ones into unusedContracts or nothing).
function materializeGroup(bg: BindingGroup): A.ScopeEntry {
  if (isLetBinds(bg)) {
    return new A.SScopeLet(bg.binds[0].l, weaveContracts(bg.contracts, bg.binds));
  } else if (isLetrecBinds(bg)) {
    return new A.SScopeLetrec(bg.binds[0].l, weaveContracts(bg.contracts, bg.binds));
  } else if (isTypeLetBinds(bg)) {
    return new A.SScopeTypeLet(bg.binds[0].l, [...bg.binds].reverse());
  }
  throw new InternalCompilerError('materialize-group: no cases matched');
}

export function desugarScopeBlock(l: C.Loc, stmts: A.Expr[], bindingGroup: BindingGroup): A.Expr {
  const items: DsbEmit[] = [];
  let curStmts = stmts;
  let curBg = bindingGroup;
  let tail: A.Expr;
  for (;;) {
    const pending = desugarScopeBlockStep(curStmts, curBg);
    items.push(...pending.emits);
    if (pending.kind === 'tail') {
      tail = pending.tail;
      break;
    }
    curStmts = pending.stmts;
    curBg = pending.bg;
  }
  // Materialize backwards: the nested formulation wove contracts and
  // reported unused-contract errors in unwind order (tail's group first).
  const rev: A.ScopeEntry[] = [];
  for (let i = items.length - 1; i >= 0; i--) {
    const it = items[i];
    if (it.kind === 'stmt') {
      rev.push(new A.SScopeStmt(it.stmt.l, it.stmt));
    } else if (it.kind === 'unusedContracts') {
      const mkErr = c => new CE.ContractUnused(c.l, c.name.toname());
      const contractErrs = it.contracts.map(mkErr).reverse();
      errors = [ ...contractErrs, ...errors ]
    } else {
      rev.push(materializeGroup(it.bg));
    }
  }
  if (rev.length === 0) {
    return tail;
  }
  return new A.SScopeBlock(l, rev.reverse(), tail);
}

function desugarScopeBlockStep(stmts: A.Expr[], bindingGroup: BindingGroup): DsbStep {
  // Treating stmts as a block, resolve scope.
  // There should be no blocks left after this stage of the compiler pipeline.
  if (stmts.length === 0) {
    throw new InternalCompilerError("Should not get an empty block in desugar-scope-block");
  }
  const f = stmts[0];
  const restStmts = stmts.slice(1);
  if (A.isSType(f)) {
    return addTypeLetBind(bindingGroup, new A.STypeBind(f.l, f.name, f.params, f.ann), restStmts);
  } else if (A.isSContract(f)) {
    const [contracts, newRestStmts] = takeWhile<A.Expr>(A.isSContract, restStmts);
    return addContracts(bindingGroup, [f, ...(contracts as Contract[])], newRestStmts);
  } else if (A.isSLet(f)) {
    return addLetBind(bindingGroup, new A.SLetBind(f.l, f.name, f.value), restStmts);
  } else if (A.isSVar(f)) {
    return addLetBind(bindingGroup, new A.SVarBind(f.l, f.name, f.value), restStmts);
  } else if (A.isSRec(f)) {
    return addLetrecBind(bindingGroup, new A.SLetrecBind(f.l, f.name, f.value), restStmts);
  } else if (A.isSFun(f)) {
    return addLetrecBind(bindingGroup, new A.SLetrecBind(
      f.l,
      new A.SBind(f.l, false, new A.SName(f.l, f.name), A.aBlank),
      // NOTE(Ben): deliberately keeping this as an s-fun;
      // it'll get turned into an s-lam in weave-contracts
      f
    ), restStmts);
  } else if (A.isSDataExpr(f)) {
    const l = f.l;
    const name = f.name;
    const namet = f.namet;
    const params = f.params;
    const mixins = f.mixins;
    const variants = f.variants;
    const shared = f.sharedMembers;
    const _checkLoc = f._checkLoc;
    const _check = f._check;
    const b = (loc: C.Loc, id: string): A.Bind => new A.SBind(loc, false, new A.SName(loc, id), A.aBlank);
    const bn = (loc: C.Loc, n: A.Name): A.Bind => new A.SBind(loc, false, n, A.aBlank);
    function variantBinds(dataBlobId: A.Expr, v: any): A.LetrecBind[] {
      const vname = v.name;
      const checkerName = A.makeCheckerName(vname);
      const getPart = (field: string) => new A.SDot(v.l, dataBlobId, field);
      return [
        new A.SLetrecBind(v.l, b(v.l, vname), getPart(vname)),
        new A.SLetrecBind(v.l, b(v.l, checkerName), getPart(checkerName)),
      ];
    }
    const blobId = names.makeAtom(name);
    const dataExpr = new A.SDataExpr(l, name, namet, params, mixins, variants, shared, _checkLoc, _check);
    const bindData = new A.SLetrecBind(l, bn(l, blobId), dataExpr);
    const bindDataPred = new A.SLetrecBind(l, b(l, A.makeCheckerName(name)),
      new A.SDot(l, new A.SIdLetrec(l, blobId, true), name));
    let allBinds: A.LetrecBind[] = [bindDataPred, bindData];
    for (const v of variants) {
      allBinds = [...variantBinds(new A.SIdLetrec(v.l, blobId, true), v), ...allBinds];
    }
    return addLetrecBinds(bindingGroup, allBinds, restStmts);
  } else if (A.isSCheck(f)) {
    const l = f.l;
    const b = (loc: C.Loc): A.Bind => new A.SBind(loc, false, new A.SUnderscore(l), A.aBlank);
    return addLetrecBinds(bindingGroup,
      [new A.SLetrecBind(l, b(l), new A.SCheck(l, f.name, f.body, f.keywordCheck))], restStmts);
  } else {
    if (restStmts.length === 0) {
      return dsbFinish(bindingGroup, f);
    } else {
      return dsbDeferStmt(bindingGroup, f, restStmts);
    }
  }
}

export function rebuildFun(
  rebuild: (l: C.Loc, name: any, params: A.Name[], args: A.Bind[], ann: A.Ann, doc: string,
    body: A.Expr, checkLoc: C.Loc | undefined, check: A.Expr | undefined, blocky: boolean) => any,
  visitor: any, l: C.Loc, name: any, params: A.Name[], args: A.Bind[], ann: A.Ann, doc: string,
  body: A.Expr, _checkLoc: C.Loc | undefined, _check: A.Expr | undefined, blocky: boolean
): any {
  const vParams = params.map((p) => p.visit(visitor));
  const vAnn = ann.visit(visitor);
  const vBody = body.visit(visitor);
  const vCheck = _check === undefined ? undefined : _check.visit(visitor);
  const placeholder = new A.SStr(l, "placeholder");
  let newBinds: A.Bind[] = [];
  // reverse arg order: the Pyret original nests these last arg outermost
  const tupleGroups: A.ScopeEntry[] = [];
  for (const a of args) {
    const lbs = simplifyLetBind(mkSLetBind, a.l, a.visit(visitor), placeholder, []).reverse();
    const argBind = lbs[0];
    newBinds = [argBind.b, ...newBinds];
    if (lbs.length > 1) {
      tupleGroups.unshift(new A.SScopeLet(a.l, lbs.slice(1)));
    }
  }
  const newBody = tupleGroups.length === 0 ? vBody : new A.SScopeBlock(tupleGroups[0].l, tupleGroups, vBody);
  return rebuild(l, name, vParams, [...newBinds].reverse(), vAnn, doc, newBody, _checkLoc, vCheck, blocky);
}

export class DesugarScopeVisitor extends DefaultMapVisitor {
  sBlock(node: A.SBlock): A.Expr {
    return desugarScopeBlock(node.l, node.stmts.map((s) => s.visit(this)), new LetBinds([], []));
  }
  sLetExpr(node: A.SLetExpr): A.Expr {
    const vBody = node.body.visit(this);
    let newBinds: any[] = [];
    for (const b of node.binds) {
      newBinds = simplifyLetBind(mkSLetBind, b.l, b.b.visit(this), b.value.visit(this), newBinds);
    }
    return A.sScopeLetBlock(node.l, [...newBinds].reverse(), vBody);
  }
  sLetrec(node: A.SLetrec): A.Expr {
    const vBinds = node.binds.map((b) => b.visit(this));
    const vBody = node.body.visit(this);
    return new A.SScopeBlock(node.l, [new A.SScopeLetrec(node.l, vBinds)], vBody);
  }
  sTypeLetExpr(node: A.STypeLetExpr): A.Expr {
    const vBinds = node.binds.map((b) => b.visit(this));
    const vBody = node.body.visit(this);
    return new A.SScopeBlock(node.l, [new A.SScopeTypeLet(node.l, vBinds)], vBody);
  }
  sFor(node: A.SFor): A.Expr {
    const vIterator = node.iterator.visit(this);
    const vAnn = node.ann.visit(this);
    const vBody = node.body.visit(this);
    let newBinds: A.ForBind[] = [];
    const tupleGroups: A.ScopeEntry[] = []; // reverse binding order, as in rebuildFun
    for (const b of node.bindings) {
      const lbs = simplifyLetBind(mkSLetBind, b.l, b.bind.visit(this), b.value.visit(this), []).reverse();
      const argBind = lbs[0];
      newBinds = [new A.SForBind(b.l, argBind.b, argBind.value), ...newBinds];
      if (lbs.length > 1) {
        tupleGroups.unshift(new A.SScopeLet(b.l, lbs.slice(1)));
      }
    }
    const newBody = tupleGroups.length === 0 ? vBody : new A.SScopeBlock(tupleGroups[0].l, tupleGroups, vBody);
    return new A.SFor(node.l, vIterator, [...newBinds].reverse(), vAnn, newBody, node.blocky);
  }
  sCasesBranch(node: A.SCasesBranch): A.CasesBranch {
    const vBody = node.body.visit(this);
    let newBinds: A.CasesBind[] = [];
    const tupleGroups: A.ScopeEntry[] = []; // reverse binding order, as in rebuildFun
    for (const b of node.args) {
      const lbs = simplifyLetBind(mkSLetBind, b.l, b.bind.visit(this), new A.SStr(b.l, "placeholder"), []).reverse();
      const argBind = lbs[0];
      newBinds = [new A.SCasesBind(b.l, b.fieldType, argBind.b), ...newBinds];
      if (lbs.length > 1) {
        tupleGroups.unshift(new A.SScopeLet(b.l, lbs.slice(1)));
      }
    }
    const newBody = tupleGroups.length === 0 ? vBody : new A.SScopeBlock(tupleGroups[0].l, tupleGroups, vBody);
    return new A.SCasesBranch(node.l, node.patLoc, node.name, [...newBinds].reverse(), newBody);
  }
  sFun(node: A.SFun): A.Expr {
    return rebuildFun(
      (l, name, params, args, ann, doc, body, checkLoc, check, blocky) =>
        new A.SFun(l, name, params, args, ann, doc, body, checkLoc, check, blocky),
      this, node.l, node.name, node.params, node.args, node.ann, node.doc, node.body,
      node._checkLoc, node._check, node.blocky);
  }
  sLam(node: A.SLam): A.Expr {
    return rebuildFun(
      (l, name, params, args, ann, doc, body, checkLoc, check, blocky) =>
        new A.SLam(l, name, params, args, ann, doc, body, checkLoc, check, blocky),
      this, node.l, node.name, node.params, node.args, node.ann, node.doc, node.body,
      node._checkLoc, node._check, node.blocky);
  }
  sMethod(node: A.SMethod): A.Expr {
    return rebuildFun(
      (l, name, params, args, ann, doc, body, checkLoc, check, blocky) =>
        new A.SMethod(l, name, params, args, ann, doc, body, checkLoc, check, blocky),
      this, node.l, node.name, node.params, node.args, node.ann, node.doc, node.body,
      node._checkLoc, node._check, node.blocky);
  }
  sMethodField(node: A.SMethodField): A.Member {
    return rebuildFun(
      (l, name, params, args, ann, doc, body, checkLoc, check, blocky) =>
        new A.SMethodField(l, name, params, args, ann, doc, body, checkLoc, check, blocky),
      this, node.l, node.name, node.params, node.args, node.ann, node.doc, node.body,
      node._checkLoc, node._check, node.blocky);
  }
}

export const desugarScopeVisitor = new DesugarScopeVisitor();

export function desugarScope(prog: A.Program, env: C.CompileEnvironment): C.ScopeResolution {
  // Remove x = e, var x = e, tuple bindings, and fun f(): e end
  // and turn them into explicit let and letrec expressions.
  // Do this recursively through the whole program.
  // Preconditions on prog:
  //   - well-formed
  // Postconditions on prog:
  //   - contains no s-provide in headers
  //   - contains no s-let, s-var, s-data, s-tuple-bind
  const l = prog.l;
  const _useRaw = prog._use;
  const _provideRaw = prog._provide;
  const provideTypesRaw = prog.providedTypes;
  const provides = prog.provides;
  const importsRaw = prog.imports;
  const body = prog.block;

  const bodyL = A.isSBlock(body) ? body.l : l;
  const { typeBinds, stmts } = desugarToplevelTypes(A.isSBlock(body) ? body.stmts : [body]);
  // the loc the Pyret original's type-let wrapper carries
  const blockL = typeBinds.length > 0 ? typeBinds[0].l : bodyL;
  function transformToplevelLast(l2: C.Loc, last: A.Expr): A.Expr {
    return new A.SModule(l2, last, [], [], [],
      new A.SApp(l2, new A.SDot(l2, U.checkers(l2), "results"), []));
  }
  const last = stmts[stmts.length - 1];
  const withProvides = new A.SBlock(blockL, [...stmts.slice(0, stmts.length - 1), transformToplevelLast(blockL, last)]);

  errors = [];
  const visited = withProvides.visit(desugarScopeVisitor);
  // The hoisted binds get the same visit the statements got: today their
  // anns can only carry ids (refinements are syntactically ids), but this
  // is where a block-bearing ann would need scope-desugaring.
  const visitedTypeBinds = typeBinds.map((b) => b.visit(desugarScopeVisitor));
  const block = typeBinds.length === 0
    ? visited
    : prependScopeEntries(blockL, [new A.SScopeTypeLet(blockL, visitedTypeBinds)], visited);
  return new C.ResolvedScope(
    new A.SProgram(l, _useRaw, _provideRaw, provideTypesRaw, provides, importsRaw, block),
    errors);
}

export function getOriginLoc(o: C.BindOrigin): C.Loc {
  return o.definitionBindSite;
}

export function getLocalLoc(o: C.BindOrigin): C.Loc {
  return o.localBindSite;
}

export function uriFrom(start: string, path: A.Name[], compileEnv: C.CompileEnvironment): string | undefined {
  if (path.length === 0) {
    return start;
  } else {
    const f = path[0];
    const r = path.slice(1);
    const modInfo = compileEnv.providesByUriValue(start);
    const uri = modInfo.modules.get(f.toname());
    if (uri === undefined) {
      throw new InternalCompilerError("Cannot find a a provided module named  " + toRepr(f) + " on module " + start);
    }
    return uriFrom(uri, r, compileEnv);
  }
}

export function maybeUriForPath(fullPath: A.Name[], compileEnv: C.CompileEnvironment, modEnv: Map<string, C.ModuleBind>): string | undefined {
  if (fullPath.length === 0) {
    return undefined;
  } else {
    const f = fullPath[0];
    const r = fullPath.slice(1);
    const modBind = modEnv.get(f.toname());
    if (modBind === undefined) {
      throw new InternalCompilerError("Cannot find a binding for module named " + toRepr(f));
    }
    return uriFrom(modBind.uri, r, compileEnv);
  }
}

export function pathUri(prePath: A.Name[], path: A.Name[], compileEnv: C.CompileEnvironment, modEnv: Map<string, C.ModuleBind>): string | undefined {
  return maybeUriForPath([...prePath, ...path.slice(0, path.length - 1)], compileEnv, modEnv);
}

type ScopeEnv = Map<string, C.ValueBind>;
type TypeEnv = Map<string, C.TypeBind>;
type ModuleEnv = Map<string, C.ModuleBind>;
type ImportAcc = [ScopeEnv, TypeEnv, ModuleEnv, A.Import[]];
type ProvideTriple = [C.Loc, string | undefined, A.Name];

export function resolveNames(p: A.Program, thismoduleUri: string, initialEnv: C.CompileEnvironment): C.NameResolution {
  // Turn all s-names into s-atom or s-global
  // Requires:
  //  1. desugar-scope
  // Preconditions on p:
  //  -  Contains no s-block, s-let, s-var, s-data, s-rec
  // Postconditions on p (in addition to preconditions):
  //  -  Contains no s-name in names
  let nameErrors: CompileError[] = [];

  // Maps from keys to ModuleBinds
  const moduleBindings = new Map<string, C.ModuleBind>();

  // Maps from keys to ValueBinds
  const bindings = new Map<string, C.ValueBind>();

  // Maps from keys to TypeBinds
  const typeBindings = new Map<string, C.TypeBind>();

  // Maps from keys to data expressions
  const datatypes = new Map<string, A.Expr>();

  function makeAnonImportFor<B>(l: C.Loc, s: string, env: Map<string, B>, bindingsDict: Map<string, B>, b: (atom: A.Name) => B): { atom: A.Name; env: Map<string, B> } {
    const atom = names.makeAtom(s);
    bindingsDict.set(atom.key(), b(atom));
    return { atom, env };
  }
  function makeAtomFor<B extends { origin: C.BindOrigin }>(name: A.Name, isShadowing: boolean, env: Map<string, B>, bindingsDict: Map<string, B>, makeBinding: (atom: A.Name) => B): { atom: A.Name; env: Map<string, B> } {
    if (A.isSName(name)) {
      const l = name.l;
      const s = name.s;
      if (env.has(s) && !isShadowing) {
        const oldLoc = getOriginLoc(mapGetValue(env, s).origin);
        const localLoc = getLocalLoc(mapGetValue(env, s).origin);
        const importLocOpt =
          (localLoc.equals(p.l) || localLoc.equals(A.dummyLoc))
            ? undefined
            : localLoc;
        nameErrors = [new CE.ShadowId(s, l, oldLoc, importLocOpt), ...nameErrors];
      }
      const atom = names.makeAtom(s);
      const binding = makeBinding(atom);
      bindingsDict.set(atom.key(), binding);
      return { atom, env: mapSet(env, s, binding) };
    } else if (A.isSUnderscore(name)) {
      const atom = names.makeAtom("$underscore");
      bindingsDict.set(atom.key(), makeBinding(atom));
      return { atom, env };
    } else if (A.isSAtom(name)) {
      // NOTE(joe): an s-atom is pre-resolved to all its uses, so no need to add
      // it or do any more work.
      const binding = makeBinding(name);
      bindingsDict.set(name.key(), binding);
      return { atom: name, env };
    } else {
      throw new InternalCompilerError("Unexpected atom type: " + toRepr(name));
    }
  }

  function makeImportAtomFor<B extends { origin: C.BindOrigin }>(name: A.Name, fromUri: string, env: Map<string, B>, bindingsDict: Map<string, B>, makeBinding: (atom: A.Name) => B): { atom: A.Name; env: Map<string, B> } {
    if (A.isSName(name)) {
      const b = env.get(name.toname());
      if (b === undefined) {
        return makeAtomFor(name, false, env, bindingsDict, makeBinding);
      } else {
        // If they are from the same URI, can import the same name multiple
        // times. If not, then they count as shadowing one another (e.g. two
        // values named list coming from two different libs)
        const shadowing = b.origin.uriOfDefinition === fromUri;
        return makeAtomFor(name, shadowing, env, bindingsDict, makeBinding);
      }
    } else {
      return makeAtomFor(name, false, env, bindingsDict, makeBinding);
    }
  }

  function scopeEnvFromEnv(initial: C.CompileEnvironment): ScopeEnv {
    const acc = new Map<string, C.ValueBind>();
    for (const name of initial.globals.values.keys()) {
      const origin = mapGetValue(initial.globals.values, name);
      const uriOfDefinition = origin.uriOfDefinition;
      const valInfo = initial.valueByOrigin(origin);
      if (valInfo === undefined) {
        throw new InternalCompilerError("The value is a global that doesn't exist in any module: " + name);
      }
      if (C.isVVar(valInfo)) {
        const b = new C.ValueBind(C.boGlobal(origin, uriOfDefinition, origin.originalName), C.vbVar, names.sGlobal(name), A.aBlank);
        bindings.set(names.sGlobal(name).key(), b);
        acc.set(name, b);
      } else {
        // TODO(joe): Good place to add _location_ to valueexport to report errs better
        const b = new C.ValueBind(C.boGlobal(origin, uriOfDefinition, origin.originalName), C.vbLet, names.sGlobal(name), A.aBlank);
        bindings.set(names.sGlobal(name).key(), b);
        acc.set(name, b);
      }
    }
    return acc;
  }

  function typeEnvFromEnv(initial: C.CompileEnvironment): TypeEnv {
    const acc = new Map<string, C.TypeBind>();
    for (const name of initial.globals.types.keys()) {
      const origin = mapGetValue(initial.globals.types, name);
      const typeInfo = initial.typeByOriginValue(origin);
      const b = new C.TypeBind(C.boGlobal(origin, origin.uriOfDefinition, origin.originalName), C.tbTypeLet, names.sTypeGlobal(name), new C.TbTyp(typeInfo));
      typeBindings.set(names.sTypeGlobal(name).key(), b);
      acc.set(name, b);
    }
    return acc;
  }

  function moduleEnvFromEnv(initial: C.CompileEnvironment): ModuleEnv {
    const acc = new Map<string, C.ModuleBind>();
    for (const name of initial.globals.modules.keys()) {
      const origin = mapGetValue(initial.globals.modules, name);
      const modInfo = initial.providesByOriginValue(origin);
      const b = new C.ModuleBind(C.boGlobal(origin, origin.uriOfDefinition, origin.originalName), names.sModuleGlobal(name), mapGetValue(modInfo.modules, name));
      moduleBindings.set(names.sModuleGlobal(name).key(), b);
      acc.set(name, b);
    }
    return acc;
  }

  function resolveLetrecBinds(visitor: NamesVisitor, binds: A.LetrecBind[]): [A.LetrecBind[], NamesVisitor] {
    let env = visitor.env;
    let atoms: A.Name[] = [];
    for (const b of binds) {
      // TODO(joe): I think that b.b.ann.visit below could be wrong if
      // a letrec'd ID is used in a refinement within the same letrec,
      // so state may be necessary here
      const bBind = asVariant(b.b, A.SBind);
      const visitedAnn = bBind.ann.visit(visitor);
      const atomEnv = makeAtomFor(bBind.id, bBind.shadows, env, bindings,
        (atom) => new C.ValueBind(C.boLocal(b.l, bBind.id), C.vbLetrec, atom, visitedAnn));
      env = atomEnv.env;
      atoms = [atomEnv.atom, ...atoms];
    }
    const newVisitor = visitor.extend({ env });
    const atomsInOrder = [...atoms].reverse();
    const visitBinds = binds.map((b, i) => {
      const a = atomsInOrder[i];
      const l2 = b.l;
      const bind = asVariant(b.b, A.SBind);
      const newBind = new A.SBind(l2, false, a, bind.ann.visit(visitor.extend({ env })));
      const visitExpr = b.value.visit(newVisitor);
      return new A.SLetrecBind(l2, newBind, visitExpr);
    });
    return [visitBinds, newVisitor];
  }
  function handleId(env: ScopeEnv, l: C.Loc, id: A.Name): A.Name {
    if (A.isSName(id)) {
      const s = id.s;
      if (env.has(s)) {
        return mapGetValue(env, s).atom;
      } else {
        return names.sGlobal(s);
      }
    } else if (A.isSAtom(id)) {
      return id;
    } else if (A.isSUnderscore(id)) {
      return id;
    } else {
      throw new InternalCompilerError("Wasn't expecting a non-s-name in resolve-names id: " + toRepr(id));
    }
  }
  function handleAnn(l: C.Loc, typeEnv: TypeEnv, id: A.Name): A.Ann {
    if (A.isSName(id)) {
      const s = id.s;
      if (typeEnv.has(s)) {
        const vb = mapGetValue(typeEnv, s);
        const name = vb.atom;
        if (C.isTbTypeLet(vb.binder)) {
          return new A.AName(l, name);
        } else if (C.isTbTypeVar(vb.binder)) {
          return new A.ATypeVar(l, name);
        } else {
          throw new InternalCompilerError("handle-ann: no cases matched");
        }
      } else {
        return new A.AName(l, names.sTypeGlobal(s));
      }
    } else {
      return new A.AName(l, id);
    }
  }

  function handleColumnBinds(columnBinds: A.ColumnBinds, visitor: NamesVisitor): { columnBinds: A.ColumnBinds; env: ScopeEnv } {
    let env = visitor.env;
    let cbs: A.Bind[] = [];
    for (const cbBind of columnBinds.binds) {
      const cb = asVariant(cbBind, A.SBind);
      const accEnv = env;
      const visitedAnn = cb.ann.visit(visitor);
      const atomEnv = makeAtomFor(cb.id, cb.shadows, accEnv, bindings,
        (atom) => new C.ValueBind(C.boLocal(cb.l, cb.id), C.vbLet, atom, visitedAnn));
      const newCb = new A.SBind(cb.l, cb.shadows, atomEnv.atom, cb.ann.visit(visitor.extend({ env: accEnv })));
      env = atomEnv.env;
      cbs = [newCb, ...cbs];
    }
    return {
      columnBinds: new A.SColumnBinds(columnBinds.l, cbs, columnBinds.table.visit(visitor)),
      env,
    };
  }

  let includeCounter = 0;
  function includeName(): string {
    includeCounter = includeCounter + 1;
    return "$included-" + String(includeCounter);
  }

  function addValueName(l: C.Loc, impLoc: C.Loc, env: ScopeEnv, vname: A.Name, asName: A.Name, modInfo: C.Provides): ScopeEnv {
    const maybeValueExport = modInfo.values.get(vname.toname());
    if (maybeValueExport === undefined) {
      nameErrors = [new CE.NameNotProvided(l, impLoc, vname, "value"), ...nameErrors];
      return env;
    } else {
      const valueExport = maybeValueExport;
      const vbinder = C.isVVar(valueExport) ? C.vbVar : C.vbLet;
      const atomEnv = makeImportAtomFor(asName, valueExport.origin.uriOfDefinition, env, bindings,
        (atom) => new C.ValueBind(
          C.boModule(field(asName, 'l'), valueExport.origin.definitionBindSite, valueExport.origin.uriOfDefinition, valueExport.origin.originalName),
          vbinder, atom, new A.AAny(field(vname, 'l'))));
      return atomEnv.env;
    }
  }

  function addTypeName(l: C.Loc, impLoc: C.Loc, typeEnv: TypeEnv, tname: A.Name, asName: A.Name, modInfo: C.Provides): TypeEnv {
    const maybeTypeExport = modInfo.aliases.get(tname.toname());
    if (maybeTypeExport === undefined) {
      nameErrors = [new CE.NameNotProvided(l, impLoc, tname, "type"), ...nameErrors];
      return typeEnv;
    } else {
      const t = maybeTypeExport;
      let origName: A.Name;
      let uriOfTyp: string;
      let locOfTyp: C.Loc;
      if (T.isTName(t)) {
        const moduleName = t.moduleName;
        if (T.isLocal(moduleName)) {
          origName = t.id; uriOfTyp = modInfo.fromUri; locOfTyp = t.l;
        } else if (T.isModuleUri(moduleName)) {
          origName = t.id; uriOfTyp = moduleName.uri; locOfTyp = t.l;
        } else if (T.isDependency(moduleName)) {
          origName = t.id; uriOfTyp = modInfo.fromUri; locOfTyp = t.l;
        } else {
          throw new InternalCompilerError("add-type-name: no cases matched");
        }
      } else {
        origName = tname; uriOfTyp = modInfo.fromUri; locOfTyp = t.l;
      }
      const atomEnv = makeImportAtomFor(asName, uriOfTyp, typeEnv, typeBindings,
        (atom) => new C.TypeBind(
          C.boModule(field(asName, 'l'), locOfTyp, uriOfTyp, origName),
          C.tbTypeLet, atom, new C.TbTyp(t)));
      return atomEnv.env;
    }
  }

  function addModuleName(l: C.Loc, impLoc: C.Loc, moduleEnv: ModuleEnv, mname: A.Name, asName: A.Name, modInfo: C.Provides): ModuleEnv {
    const maybeModuleExport = modInfo.modules.get(mname.toname());
    if (maybeModuleExport === undefined) {
      nameErrors = [new CE.NameNotProvided(l, impLoc, mname, "module"), ...nameErrors];
      return moduleEnv;
    } else {
      const uri = maybeModuleExport;
      const atomEnv = makeImportAtomFor(asName, modInfo.fromUri, moduleEnv, moduleBindings,
        (atom) => new C.ModuleBind(
          C.boModule(field(asName, 'l'), new SL.Builtin(uri), modInfo.fromUri, mname),
          atom, uri));
      return atomEnv.env;
    }
  }

  function starNames(l: C.Loc, nameList: string[], hidings: A.Name[]): string[] {
    for (const h of hidings) {
      if (!nameList.includes(h.toname())) {
        nameErrors = [new CE.WfErrSplit("The name " + h.toname() + " is listed as hidden but was not included.", [l]), ...nameErrors];
      }
    }
    const hidingNames = hidings.map((h2) => h2.toname());
    return nameList.filter((n) => !hidingNames.includes(n));
  }

  function addSpec(impLoc: C.Loc, acc: ImportAcc, modInfo: C.Provides, spec: A.IncludeSpec): ImportAcc {
    const [impE, impTe, impMe, impImps] = acc;

    // data will spread across many
    const sharedDataHidings = new Map<string, boolean>();

    function addNameSpec(nameSpec: A.NameSpec, dict: Map<string, any>, whichEnv: any, adder: (l: C.Loc, impLoc: C.Loc, env: any, name: A.Name, asName: A.Name, modInfo: C.Provides) => any): any {
      if (A.isSStar(nameSpec)) {
        const l = nameSpec.l;
        const hidings = nameSpec.hidden;
        const allNames = [...dict.keys()].sort();
        const importedNames = starNames(l, allNames, hidings);
        let env = whichEnv;
        for (const n of importedNames) {
          env = adder(l, impLoc, env, new A.SName(l, n), new A.SName(l, n), modInfo);
        }
        return env;
      } else if (A.isSModuleRef(nameSpec)) {
        const l = nameSpec.l;
        const path = nameSpec.path;
        const asName = nameSpec.asName;
        const maybeUri = uriFrom(modInfo.fromUri, path.slice(0, path.length - 1), initialEnv);
        let newModInfo: C.Provides;
        if (maybeUri === undefined) {
          throw new InternalCompilerError("Could not find module " + toRepr(path));
        } else {
          newModInfo = initialEnv.providesByUriValue(maybeUri);
        }
        const newAsName = asName === undefined ? path[path.length - 1] : asName;
        return adder(l, impLoc, whichEnv, path[path.length - 1], newAsName, newModInfo);
      } else {
        throw new InternalCompilerError("add-name-spec: no cases matched");
      }
    }

    function maybeAddNameSpec(nameSpec: A.NameSpec, dict: Map<string, any>, whichEnv: any, adder: any, name: string, hidings: A.Name[]): any {
      if (hidings.find((h) => h.toname() === name) !== undefined) {
        sharedDataHidings.delete(name);
        return whichEnv;
      } else {
        return addNameSpec(nameSpec, dict, whichEnv, adder);
      }
    }

    function addDataSpec(envs: [ScopeEnv, TypeEnv], nameSpec: A.NameSpec, hidings: A.Name[]): [ScopeEnv, TypeEnv] {
      if (A.isSStar(nameSpec)) {
        // NOTE(joe): s-star on data-spec never has hidings, they are on the include-data-spec
        const l = nameSpec.l;
        const datatypeNames = [...modInfo.dataDefinitions.keys()].sort();
        let curEnvs = envs;
        for (const dname of datatypeNames) {
          curEnvs = addDataSpec(curEnvs, new A.SModuleRef(l, [new A.SName(l, dname)], undefined), hidings);
        }
        return curEnvs;
      } else if (A.isSModuleRef(nameSpec)) {
        const l = nameSpec.l;
        const path = nameSpec.path;
        const asName = nameSpec.asName;
        const maybeUri = uriFrom(modInfo.fromUri, path.slice(0, path.length - 1), initialEnv);
        let newModInfo: C.Provides;
        if (maybeUri === undefined) {
          throw new InternalCompilerError("Could not find module " + toRepr(path));
        } else {
          newModInfo = initialEnv.providesByUriValue(maybeUri);
        }
        // NOTE(joe): the module-ref can't possibly have an as-name for data
        // based on how the grammar is defined; we simply re-use s-module-ref
        // rather than introduce a new variant.
        void (asName === undefined ? path[path.length - 1] : asName);
        const dname = path[path.length - 1].toname();
        const maybeDtExport = initialEnv.resolveDatatypeByUri(newModInfo.fromUri, dname);
        if (maybeDtExport === undefined) {
          throw new InternalCompilerError("Cannot find datatype name " + dname + " in " + newModInfo.fromUri);
        } else {
          const typ = maybeDtExport;
          let [impEDts, impTeDts] = envs;
          for (const v of typ.variants) {
            const constructorRef = new A.SModuleRef(l, [...path.slice(0, path.length - 1), new A.SName(l, v.name)], undefined);
            const checkerRef = new A.SModuleRef(l, [...path.slice(0, path.length - 1), new A.SName(l, A.makeCheckerName(v.name))], undefined);
            const env1 = maybeAddNameSpec(constructorRef, newModInfo.values, impEDts, addValueName, v.name, hidings);
            impEDts = maybeAddNameSpec(checkerRef, newModInfo.values, env1, addValueName, A.makeCheckerName(v.name), hidings);
          }
          const typAliasRef = new A.SModuleRef(l, [...path.slice(0, path.length - 1), new A.SName(l, dname)], undefined);
          impTeDts = maybeAddNameSpec(typAliasRef, newModInfo.aliases, impTeDts, addTypeName, dname, hidings);
          return [impEDts, impTeDts];
        }
      } else {
        throw new InternalCompilerError("add-data-spec: no cases matched");
      }
    }

    if (A.isSIncludeName(spec)) {
      const newEnv = addNameSpec(spec.nameSpec, modInfo.values, impE, addValueName);
      return [newEnv, impTe, impMe, impImps];
    } else if (A.isSIncludeType(spec)) {
      const newTypeEnv = addNameSpec(spec.nameSpec, modInfo.aliases, impTe, addTypeName);
      return [impE, newTypeEnv, impMe, impImps];
    } else if (A.isSIncludeModule(spec)) {
      const newModuleEnv = addNameSpec(spec.nameSpec, modInfo.modules, impMe, addModuleName);
      return [impE, impTe, newModuleEnv, impImps];
    } else if (A.isSIncludeData(spec)) {
      const l = spec.l;
      const hidings = spec.hidden;
      for (const h of hidings) {
        sharedDataHidings.set(h.toname(), true);
      }
      const [impEDts, impTeDts] = addDataSpec([impE, impTe], spec.nameSpec, hidings);
      for (const extraneousHiding of [...sharedDataHidings.keys()].sort()) {
        nameErrors = [new CE.WfErrSplit("The name " + extraneousHiding + " is listed as hidden but was not included.", [l]), ...nameErrors];
      }
      return [impEDts, impTeDts, impMe, impImps];
    } else {
      throw new InternalCompilerError("add-spec: no cases matched");
    }
  }

  function addImport(acc: ImportAcc, imp: A.Import): ImportAcc {
    const [impE, impTe, impMe, impImps] = acc;
    if (A.isSImport(imp)) {
      const l = imp.l;
      const file = imp.file;
      const localName = imp.name;
      const infoKey = U.importToDep(file).key();
      const modUri = initialEnv.uriByDepKey(infoKey);
      const atomEnvM = A.isSUnderscore(localName)
        ? makeAnonImportFor(localName.l, "$underscore_import", impMe, moduleBindings,
          (atom) => new C.ModuleBind(C.boLocal(l, localName), atom, modUri))
        : makeAtomFor(localName, false, impMe, moduleBindings,
          (atom) => new C.ModuleBind(C.boLocal(l, localName), atom, modUri));
      const newHeader = new A.SImport(l, file, atomEnvM.atom);
      return [impE, impTe, atomEnvM.env, [newHeader, ...impImps]];
    } else if (A.isSImportFields(imp)) {
      const l = imp.l;
      const fields = imp.fields;
      const file = imp.file;
      const synthIncludeName = names.makeAtom(includeName());
      const updated = addImport(acc, new A.SImport(l, file, synthIncludeName));
      return addImport(updated, new A.SIncludeFrom(l, [synthIncludeName],
        fields.map((f) =>
          new A.SIncludeName(l, new A.SModuleRef(l, [f], undefined)))));
    } else if (A.isSInclude(imp)) {
      const l = imp.l;
      const file = imp.mod;
      const synthIncludeName = names.makeAtom(includeName());
      const updated = addImport(acc, new A.SImport(l, file, synthIncludeName));
      return addImport(updated, new A.SIncludeFrom(l, [synthIncludeName],
        [
          new A.SIncludeName(l, new A.SStar(l, [])),
          new A.SIncludeType(l, new A.SStar(l, [])),
          new A.SIncludeModule(l, new A.SStar(l, [])),
          // new A.SIncludeData(l, new A.SStar(l, []), [])
        ]));
    } else if (A.isSIncludeFrom(imp)) {
      const l = imp.l;
      const name = imp.mod;
      const specs = imp.specs;
      // NOTE(joe): This few lines is a funky little pattern. It may be worth
      // extracting for generic use for values & types as well. The reason
      // it's necessary is that it's useful to use atoms to avoid putting
      // "real" names into the namespace. If this is a more general thing we
      // do across different pre-resolve-scope desugarings, then this pattern
      // becomes handy.
      const moduleInfo = A.isSAtom(name[0])
        ? moduleBindings.get(name[0].key())
        : impMe.get(name[0].toname());
      if (moduleInfo === undefined) {
        throw new InternalCompilerError("Could not find import: " + name[0].toname());
      }
      const firstModUri = moduleInfo.uri;
      const atom = moduleInfo.atom;
      const maybeDottedUri = uriFrom(firstModUri, name.slice(1), initialEnv);
      if (maybeDottedUri === undefined) {
        throw new InternalCompilerError("Could not find module " + toRepr(name));
      }
      const dottedUri = maybeDottedUri;
      const modInfo = initialEnv.providesByUriValue(dottedUri);
      let specAcc: ImportAcc = acc;
      for (const s of specs) {
        specAcc = addSpec(l, specAcc, modInfo, s);
      }
      const [specsE, specsTe, specsMe] = specAcc;
      return [specsE, specsTe, specsMe, [new A.SIncludeFrom(l, [atom], specs), ...impImps]];
    } else {
      throw new InternalCompilerError("add-import: no cases matched");
    }
  }

  function resolveImportNames(self: NamesVisitor, imports: A.Import[]): ImportAcc {
    let acc: ImportAcc = [self.env, self.typeEnv, self.moduleEnv, []];
    for (const i of imports) {
      acc = addImport(acc, i);
    }
    return acc;
  }

  let finalVisitor: NamesVisitor | undefined = undefined;

  class NamesVisitor extends PostScopeMapVisitor {
    constructor(public env: ScopeEnv, public typeEnv: TypeEnv, public moduleEnv: ModuleEnv) {
      super();
    }
    extend(fields: { env?: ScopeEnv; typeEnv?: TypeEnv; moduleEnv?: ModuleEnv }): NamesVisitor {
      return new NamesVisitor(
        fields.env !== undefined ? fields.env : this.env,
        fields.typeEnv !== undefined ? fields.typeEnv : this.typeEnv,
        fields.moduleEnv !== undefined ? fields.moduleEnv : this.moduleEnv);
    }

    sModule(node: A.SModule): A.Expr {
      const l = node.l;

      /*
        ```
        include from T: * end <-- defined here?
        include from T: a, b, c end <-- defined here?
        ```
      */

      const nonGlobals = [...this.env.keys()].sort().filter((k) => {
        const vb = mapGetValue(this.env, k);
        return vb.origin.newDefinition;
      });
      const definedVals: A.DefinedValue[] = nonGlobals.map((key) => {
        const vb = mapGetValue(this.env, key);
        const atom = vb.atom;
        if (C.isVbLet(vb.binder)) {
          return new A.SDefinedValue(key, new A.SId(l, atom));
        } else if (C.isVbLetrec(vb.binder)) {
          return new A.SDefinedValue(key, new A.SIdLetrec(l, atom, true));
        } else if (C.isVbVar(vb.binder)) {
          return new A.SDefinedVar(key, atom);
        } else {
          throw new InternalCompilerError("s-module defined-vals: no cases matched");
        }
      });

      const nonGlobalTypes = [...this.typeEnv.keys()].sort().filter((k) => {
        const tb = mapGetValue(this.typeEnv, k);
        return tb.origin.newDefinition;
      });
      const definedTypes: A.DefinedType[] = nonGlobalTypes.map((key) => {
        const atom = mapGetValue(this.typeEnv, key).atom;
        return new A.SDefinedType(key, new A.AName(l, atom));
      });

      const nonGlobalModules = [...this.moduleEnv.keys()].sort().filter((k) => {
        const mb = mapGetValue(this.moduleEnv, k);
        return mb.origin.newDefinition;
      });
      const definedModules: A.DefinedModule[] = nonGlobalModules.map((key) => {
        const bind = mapGetValue(this.moduleEnv, key);
        return new A.SDefinedModule(key, bind.atom, bind.uri);
      });

      finalVisitor = this;
      return new A.SModule(l, node.answer.visit(this), definedModules, definedVals, definedTypes, node.checks.visit(this));
    }

    sProgram(node: A.SProgram): A.Program {
      const l = node.l;
      const _use = node._use;
      const _provide = node._provide;
      const _provideTypes = node.providedTypes;
      const provides = node.provides;
      const imports = node.imports;
      const body = node.block;

      const [impE, impTe, impMe, impImps] = resolveImportNames(this, imports);

      const visitBody = body.visit(this.extend({ env: impE, typeEnv: impTe, moduleEnv: impMe }));

      let provideValsSpecs: A.ProvideBlock;
      if (A.isSProvide(_provide)) {
        const pl = _provide.l;
        const obj = asVariant(_provide.block, A.SObj);
        const specs: A.ProvideSpec[] = obj.fields.map((f) => {
          const fd = asVariant(f, A.SDataField);
          if (!A.isSId(fd.value)) {
            throw new InternalCompilerError("The rhs of an object provide was not an id: " + toRepr(f));
          }
          return new A.SProvideName(fd.l, new A.SModuleRef(fd.l, [field(fd.value, 'id')], new A.SName(fd.l, fd.name)));
        });
        provideValsSpecs = new A.SProvideBlock(pl, [],
          [...specs, new A.SProvideData(pl, new A.SStar(pl, []), [])]);
      } else if (A.isSProvideAll(_provide)) {
        const pl = _provide.l;
        provideValsSpecs = new A.SProvideBlock(pl, [], [
          new A.SProvideName(pl, new A.SStar(pl, [])),
          new A.SProvideData(pl, new A.SStar(pl, []), [])]);
      } else if (A.isSProvideNone(_provide)) {
        const pl = _provide.l;
        provideValsSpecs = new A.SProvideBlock(pl, [], []);
      } else {
        throw new InternalCompilerError("s-program provide-vals-specs: no cases matched");
      }

      let provideTypesSpecs: A.ProvideBlock;
      if (A.isSProvideTypes(_provideTypes)) {
        const pl = _provideTypes.l;
        const anns = _provideTypes.ann;
        provideTypesSpecs = new A.SProvideBlock(pl, [], [...anns.map((a) => {
          if (!A.isAName(a.ann)) {
            throw new InternalCompilerError("Cannot use a non-name as a provided type");
          }
          const aAnn = a.ann as A.AName;
          return new A.SProvideType(pl, new A.SModuleRef(aAnn.l, [aAnn.id], new A.SName(a.l, a.name)));
        }),
        new A.SProvideData(pl, new A.SStar(pl, []), [])]);
      } else if (A.isSProvideTypesNone(_provideTypes)) {
        const pl = _provideTypes.l;
        provideTypesSpecs = new A.SProvideBlock(pl, [], []);
      } else if (A.isSProvideTypesAll(_provideTypes)) {
        const pl = _provideTypes.l;
        provideTypesSpecs = new A.SProvideBlock(pl, [], [
          new A.SProvideData(pl, new A.SStar(pl, []), []),
          new A.SProvideType(pl, new A.SStar(pl, []))]);
      } else {
        throw new InternalCompilerError("s-program provide-types-specs: no cases matched");
      }

      const allProvides = [provideValsSpecs, provideTypesSpecs, ...provides];

      // Each of these dictionaries maps from plain names to atoms, for example
      //   link => atom("link", 42)
      // the goal is to create a single s-provide-block with all the necessary
      // names and atoms that will be exposed. The atoms will be used by code
      // generation and by the type-checker/cross-module scope resolution to
      // pick out information about the binding (e.g. flatness, etc)
      // The actual values in the dictionaries are triples of
      //
      //  {Srcloc; Option<URI>; Atom}
      //
      // The location is the location of the provide clause, the URI is none if
      // the name is provided from this module, and some if it is re-provided
      // from another module
      const providedModules = new Map<string, ProvideTriple>();
      const providedValues = new Map<string, ProvideTriple>();
      const providedTypes = new Map<string, ProvideTriple>();
      const providedDatatypes = new Map<string, ProvideTriple>();

      function isHidden(hidden: A.Name[], maybeHiddenName: string): boolean {
        return hidden.some((h) => h.toname() === maybeHiddenName);
      }

      function maybeAdd(hidden: A.Name[], whichDict: Map<string, ProvideTriple>, maybeAddName: string, toAdd: ProvideTriple): void {
        if (!isHidden(hidden, maybeAddName)) {
          whichDict.set(maybeAddName, toAdd);
        }
      }

      function maybeAddRemoveIfHidden(hidden: A.Name[], hiddenTodo: Map<string, C.Loc | undefined>, whichDict: Map<string, ProvideTriple>, maybeAddName: string, toAdd: ProvideTriple): void {
        if (!isHidden(hidden, maybeAddName)) {
          whichDict.set(maybeAddName, toAdd);
        }
        if (hiddenTodo.has(maybeAddName)) {
          hiddenTodo.set(maybeAddName, undefined);
        }
      }

      function expandNameSpec(whichDict: Map<string, ProvideTriple>, whichBindings: Map<string, any>, whichEnv: Map<string, any>, getProvidedBindings: (p: C.Provides) => Map<string, any>, spec: A.NameSpec, prePath: A.Name[]): void {
        void whichBindings;
        if (A.isSStar(spec)) {
          const l = spec.l;
          const hidden = spec.hidden;
          const remoteReferenceUri = maybeUriForPath(prePath, initialEnv, finalVisitor!.moduleEnv);
          if (remoteReferenceUri === undefined) {
            const keys = [...whichEnv.keys()];
            for (const h of hidden) {
              if (!keys.includes(h.toname())) {
                nameErrors = [new CE.WfErrSplit("The name " + h.toname() + " is listed as hidden but was not provided.", [l]), ...nameErrors];
              }
            }
            for (const k of keys) {
              const bind = mapGetValue(whichEnv, k);
              if (bind.origin.newDefinition) {
                maybeAdd(hidden, whichDict, bind.atom.toname(), [l, undefined, bind.atom]);
              }
            }
          } else {
            const bindingsFromModule = getProvidedBindings(initialEnv.providesByUriValue(remoteReferenceUri));
            const keys = [...bindingsFromModule.keys()];
            for (const h of hidden) {
              if (!keys.includes(h.toname())) {
                nameErrors = [new CE.WfErrSplit("The name " + h.toname() + " is listed as hidden but was not provided.", [l]), ...nameErrors];
              }
            }
            for (const k of keys) {
              // NOTE(joe): This is where we would do something like
              // "prefix-out" by doing `prefix + k` below. The k that's the
              // key in set-now is the name it's provided as, and the k in
              // the s-name is the name to look for in the original module
              maybeAdd(hidden, whichDict, k, [l, remoteReferenceUri, new A.SName(l, k)]);
            }
          }
        } else if (A.isSModuleRef(spec)) {
          const l = spec.l;
          const path = spec.path;
          const asName = spec.asName;
          const remoteReferenceUri = pathUri(prePath, path, initialEnv, finalVisitor!.moduleEnv);
          let maybeUri: string | undefined;
          let atom: A.Name;
          if (remoteReferenceUri === undefined) {
            const b = whichEnv.get(path[0].toname());
            if (b !== undefined) {
              if (b.origin.newDefinition) {
                maybeUri = undefined; atom = b.atom;
              } else {
                maybeUri = b.origin.uriOfDefinition; atom = b.origin.originalName;
              }
            } else {
              nameErrors = [new CE.UnboundId(new A.SId(l, new A.SName(l, path[path.length - 1].toname()))), ...nameErrors];
              maybeUri = undefined; atom = new A.SName(l, path[path.length - 1].toname());
            }
          } else {
            const bindingsFromModule = getProvidedBindings(initialEnv.providesByUriValue(remoteReferenceUri));
            const remoteName = path[path.length - 1].toname();
            if (!bindingsFromModule.has(remoteName)) {
              nameErrors = [new CE.UnboundId(new A.SId(l, new A.SName(l, remoteName))), ...nameErrors];
            }
            maybeUri = remoteReferenceUri; atom = new A.SName(l, remoteName);
          }
          if (asName === undefined) {
            whichDict.set(atom.toname(), [l, maybeUri, atom]);
          } else {
            whichDict.set(asName.toname(), [l, maybeUri, atom]);
          }
        } else {
          throw new InternalCompilerError("expand-name-spec: no cases matched");
        }
      }

      function expandDataSpec(valEnv: ScopeEnv, typeEnv: TypeEnv, spec: A.NameSpec, prePath: A.Name[], hidden: A.Name[], hiddenTodo: Map<string, C.Loc | undefined>): void {
        const maybeAdd2 = (whichDict: Map<string, ProvideTriple>, maybeAddName: string, toAdd: ProvideTriple) =>
          maybeAddRemoveIfHidden(hidden, hiddenTodo, whichDict, maybeAddName, toAdd);
        if (A.isSStar(spec)) {
          // NOTE(joe): Assumption is that this s-star's hiding is always empty for s-provide-data
          const l = spec.l;
          const remoteReferenceUri = maybeUriForPath(prePath, initialEnv, finalVisitor!.moduleEnv);
          if (remoteReferenceUri === undefined) {
            for (const k of [...datatypes.keys()].sort()) {
              const dataExpr = asVariant(mapGetValue(datatypes, k), A.SDataExpr);
              expandDataSpec(valEnv, typeEnv, new A.SModuleRef(l, [new A.SName(l, dataExpr.name)], undefined), prePath, hidden, hiddenTodo);
            }
          } else {
            const datatypsFromModule = initialEnv.providesByUriValue(remoteReferenceUri).dataDefinitions;
            for (const k of [...datatypsFromModule.keys()].sort()) {
              const de = mapGetValue(datatypsFromModule, k);
              let dataName: string;
              if (C.isDAlias(de)) {
                dataName = de.name;
              } else if (C.isDType(de)) {
                dataName = de.typ.name;
              } else {
                throw new InternalCompilerError("expand-data-spec: no cases matched");
              }
              expandDataSpec(valEnv, typeEnv, new A.SModuleRef(l, [new A.SName(l, dataName)], undefined), prePath, hidden, hiddenTodo);
            }
          }
        } else if (A.isSModuleRef(spec)) {
          const l = spec.l;
          const path = spec.path;
          const maybeUri = pathUri(prePath, path, initialEnv, finalVisitor!.moduleEnv);
          if (maybeUri === undefined) {
            // path must be a single element if there's no URI of a remote module
            // e.g. provide: D end   NOT    provide: M.D end
            const dataExpr = asVariant(mapGetValue(datatypes, path[0].toname()), A.SDataExpr);
            maybeAdd2(providedDatatypes, dataExpr.name, [l, undefined, dataExpr.namet]);
            const dataCheckerName = A.makeCheckerName(dataExpr.name);
            const dataCheckerVb = mapGetValue(valEnv, dataCheckerName);
            maybeAdd2(providedValues, dataCheckerName, [l, undefined, dataCheckerVb.atom]);
            const dataAliasTb = mapGetValue(typeEnv, dataExpr.name);
            maybeAdd2(providedTypes, dataExpr.name, [l, undefined, dataAliasTb.atom]);
            for (const v of dataExpr.variants) {
              const variantVb = mapGetValue(valEnv, v.name);
              const checkerName = A.makeCheckerName(v.name);
              const variantCheckerVb = mapGetValue(valEnv, checkerName);
              maybeAdd2(providedValues, v.name, [l, undefined, variantVb.atom]);
              maybeAdd2(providedValues, checkerName, [l, undefined, variantCheckerVb.atom]);
            }
          } else {
            const uri = maybeUri;
            const datatypeName = path[path.length - 1].toname();
            const providingModule = initialEnv.providesByUriValue(uri);
            const maybeDatatype = initialEnv.resolveDatatypeByUri(uri, datatypeName);
            let datatypeUri: string;
            let datatype: T.DataType;
            if (maybeDatatype === undefined) {
              const t = providingModule.aliases.get(datatypeName);
              if (t === undefined) {
                throw new InternalCompilerError("Name " + datatypeName + " not defined as a type or datatype on " + uri);
              }
              if (T.isTName(t)) {
                const moduleName = t.moduleName;
                if (!T.isModuleUri(moduleName)) {
                  throw new InternalCompilerError("Expected a remote reference: " + toRepr(moduleName));
                }
                const remoteDatatype = initialEnv.providesByUriValue(moduleName.uri).dataDefinitions.get(datatypeName);
                if (remoteDatatype !== undefined) {
                  datatypeUri = moduleName.uri;
                  datatype = field(remoteDatatype, 'typ');
                } else {
                  throw new InternalCompilerError("Cannot re-provide datatype " + datatypeName + " because it isn't a datatype in " + uri);
                }
              } else {
                throw new InternalCompilerError("expand-data-spec aliases: no cases matched");
              }
            } else {
              datatypeUri = uri;
              datatype = maybeDatatype;
            }
            const addValueIfDefined = (name: string) => {
              if (providingModule.values.has(name)) {
                maybeAdd2(providedValues, name, [l, datatypeUri, new A.SName(l, name)]);
              }
            };
            maybeAdd2(providedDatatypes, datatypeName, [l, uri, new A.SName(l, datatypeName)]);
            addValueIfDefined(A.makeCheckerName(datatypeName));
            if (providingModule.aliases.has(datatypeName)) {
              maybeAdd2(providedTypes, datatypeName, [l, datatypeUri, new A.SName(l, datatypeName)]);
            }
            for (const v of datatype.variants) {
              addValueIfDefined(v.name);
              addValueIfDefined(A.makeCheckerName(v.name));
            }
          }
        } else {
          throw new InternalCompilerError("expand-data-spec: no cases matched");
        }
      }

      function expand(provideSpec: A.ProvideSpec, path: A.Name[]): void {
        if (A.isSProvideName(provideSpec)) {
          expandNameSpec(providedValues, bindings, finalVisitor!.env, (pr) => pr.values, provideSpec.nameSpec, path);
        } else if (A.isSProvideType(provideSpec)) {
          expandNameSpec(providedTypes, typeBindings, finalVisitor!.typeEnv, (pr) => pr.aliases, provideSpec.nameSpec, path);
        } else if (A.isSProvideModule(provideSpec)) {
          expandNameSpec(providedModules, moduleBindings, finalVisitor!.moduleEnv, (pr) => pr.modules, provideSpec.nameSpec, path);
        } else if (A.isSProvideData(provideSpec)) {
          const hidden = provideSpec.hidden;
          const hiddenTodo = new Map<string, C.Loc | undefined>();
          for (const h of hidden) {
            hiddenTodo.set(h.toname(), field(h, 'l'));
          }
          expandDataSpec(finalVisitor!.env, finalVisitor!.typeEnv, provideSpec.nameSpec, path, hidden, hiddenTodo);
          for (const key of [...hiddenTodo.keys()].sort()) {
            const hl = hiddenTodo.get(key);
            if (hl !== undefined) {
              nameErrors = [new CE.WfErrSplit("The name " + key + " is listed as hidden but was not provided.", [hl]), ...nameErrors];
            }
          }
        } else {
          // | else => nothing
        }
      }

      for (const pb of allProvides) {
        for (const provideSpec of pb.specs) {
          expand(provideSpec, pb.path);
        }
      }

      function makeProvideSpec(triple: ProvideTriple, k: string, maker: (ns: A.NameSpec) => A.ProvideSpec): A.ProvideSpec {
        const [pl, maybeUri, atom] = triple;
        const nameSpec = maybeUri === undefined
          ? new A.SLocalRef(pl, atom, new A.SName(pl, k))
          : new A.SRemoteRef(pl, maybeUri, atom, new A.SName(pl, k));
        return maker(nameSpec);
      }

      for (const k of [...datatypes.keys()]) {
        const dt = asVariant(mapGetValue(datatypes, k), A.SDataExpr);
        providedDatatypes.set(k, [dt.l, undefined, dt.namet]);
      }

      const finalValProvides = [...providedValues.keys()].sort().map((k) =>
        makeProvideSpec(mapGetValue(providedValues, k), k, (ns) => new A.SProvideName(l, ns)));
      const finalTypeProvides = [...providedTypes.keys()].sort().map((k) =>
        makeProvideSpec(mapGetValue(providedTypes, k), k, (ns) => new A.SProvideType(l, ns)));
      const finalModuleProvides = [...providedModules.keys()].sort().map((k) =>
        makeProvideSpec(mapGetValue(providedModules, k), k, (ns) => new A.SProvideModule(l, ns)));
      const finalDatatypeProvides = [...providedDatatypes.keys()].sort().map((k) =>
        makeProvideSpec(mapGetValue(providedDatatypes, k), k, (ns) => new A.SProvideData(l, ns, [])));

      const oneTrueProvide = [new A.SProvideBlock(l, [],
        [...finalValProvides, ...finalTypeProvides, ...finalModuleProvides, ...finalDatatypeProvides])];

      return new A.SProgram(l, _use, new A.SProvideNone(l), new A.SProvideTypesNone(l), oneTrueProvide, [...impImps].reverse(), visitBody);
    }

    sScopeBlock(node: A.SScopeBlock): A.Expr {
      let cur: NamesVisitor = this;
      const newEntries: A.ScopeEntry[] = [];
      for (const entry of node.entries) {
        if (A.isSScopeLet(entry)) {
          let e = cur.env;
          let bs: A.LetBind[] = [];
          for (const b of entry.binds) {
            if (A.isSLetBind(b)) {
              const l2 = b.l;
              const bind = asVariant(b.b, A.SBind);
              const expr = b.value;
              const visitedAnn = bind.ann.visit(cur.extend({ env: e }));
              const atomEnv = makeAtomFor(bind.id, bind.shadows, e, bindings,
                (atom) => new C.ValueBind(C.boLocal(l2, bind.id), C.vbLet, atom, visitedAnn));
              const visitExpr = expr.visit(cur.extend({ env: e }));
              const newBind = new A.SLetBind(l2, new A.SBind(l2, bind.shadows, atomEnv.atom, visitedAnn), visitExpr);
              e = atomEnv.env;
              bs = [newBind, ...bs];
            } else if (A.isSVarBind(b)) {
              const l2 = b.l;
              const bind = asVariant(b.b, A.SBind);
              const expr = b.value;
              const visitedAnn = bind.ann.visit(cur.extend({ env: e }));
              const atomEnv = makeAtomFor(bind.id, bind.shadows, e, bindings,
                (atom) => new C.ValueBind(C.boLocal(l2, bind.id), C.vbVar, atom, visitedAnn));
              const visitExpr = expr.visit(cur.extend({ env: e }));
              const newBind = new A.SVarBind(l2, new A.SBind(l2, bind.shadows, atomEnv.atom, visitedAnn), visitExpr);
              e = atomEnv.env;
              bs = [newBind, ...bs];
            } else {
              throw new InternalCompilerError("s-scope-block let entry: no cases matched");
            }
          }
          newEntries.push(new A.SScopeLet(entry.l, [...bs].reverse()));
          cur = cur.extend({ env: e });
        } else if (A.isSScopeTypeLet(entry)) {
          let e = cur.env;
          let te = cur.typeEnv;
          let bs: A.TypeLetBind[] = [];
          for (const b of entry.binds) {
            if (A.isSTypeBind(b)) {
              const l2 = b.l;
              const name = b.name;
              const params = b.params;
              const ann = b.ann;
              const accTe = te;
              let newTypesEnv = accTe;
              let newTypesAtoms: A.Name[] = [];
              for (const param of params) {
                const atomEnv = makeAtomFor(param, false, newTypesEnv, typeBindings,
                  (atom) => new C.TypeBind(C.boLocal(l2, param), C.tbTypeVar, atom, C.tbNone));
                newTypesEnv = atomEnv.env;
                newTypesAtoms = [atomEnv.atom, ...newTypesAtoms];
              }
              const visitedAnn = ann.visit(cur.extend({ env: e, typeEnv: newTypesEnv }));
              let fullTyp: T.Type;
              if (params.length === 0) {
                fullTyp = U.annToTyp(visitedAnn, thismoduleUri, initialEnv);
              } else {
                const tbody = U.annToTyp(visitedAnn, thismoduleUri, initialEnv);
                const tparams = newTypesAtoms.map((id) => new T.TVar(id, l2, false));
                fullTyp = new T.TForall(tparams, tbody, entry.l, false);
              }
              const atomEnv = makeAtomFor(name, false, accTe, typeBindings,
                (atom) => new C.TypeBind(C.boLocal(l2, name), C.tbTypeLet, atom, new C.TbTyp(fullTyp)));
              const newBind = new A.STypeBind(l2, atomEnv.atom, [...newTypesAtoms].reverse(), visitedAnn);
              te = atomEnv.env;
              bs = [newBind, ...bs];
            } else if (A.isSNewtypeBind(b)) {
              const l2 = b.l;
              const name = b.name;
              const tname = b.namet;
              // TODO(joe): What should the TypeBindTyp be here?
              const atomEnvT = makeAtomFor(name, false, te, typeBindings,
                (atom) => new C.TypeBind(C.boLocal(l2, name), C.tbTypeLet, atom, C.tbNone));
              const atomEnv = makeAtomFor(tname, false, e, bindings,
                (atom) => new C.ValueBind(C.boLocal(l2, tname), C.vbLet, atom, A.aBlank));
              const newBind = new A.SNewtypeBind(l2, atomEnvT.atom, atomEnv.atom);
              e = atomEnv.env;
              te = atomEnvT.env;
              bs = [newBind, ...bs];
            } else {
              throw new InternalCompilerError("s-scope-block type-let entry: no cases matched");
            }
          }
          newEntries.push(new A.SScopeTypeLet(entry.l, [...bs].reverse()));
          cur = cur.extend({ env: e, typeEnv: te });
        } else if (A.isSScopeLetrec(entry)) {
          const [newBinds, newVisitor] = resolveLetrecBinds(cur, entry.binds);
          newEntries.push(new A.SScopeLetrec(entry.l, newBinds));
          cur = newVisitor;
        } else {
          newEntries.push(entry.visit(cur));
        }
      }
      return new A.SScopeBlock(node.l, newEntries, node.tail.visit(cur));
    }

    sFor(node: A.SFor): A.Expr {
      let env = this.env;
      let fbs: A.ForBind[] = [];
      for (const fb of node.bindings) {
        const l2 = fb.l;
        const bind = asVariant(fb.bind, A.SBind);
        const val = fb.value;
        const visitedAnn = bind.ann.visit(this);
        const atomEnv = makeAtomFor(bind.id, bind.shadows, env, bindings,
          (atom) => new C.ValueBind(C.boLocal(l2, bind.id), C.vbLet, atom, visitedAnn));
        const newBind = new A.SBind(bind.l, bind.shadows, atomEnv.atom, bind.ann.visit(this.extend({ env })));
        const visitVal = val.visit(this);
        const newFb = new A.SForBind(l2, newBind, visitVal);
        env = atomEnv.env;
        fbs = [newFb, ...fbs];
      }
      return new A.SFor(node.l, node.iterator.visit(this), [...fbs].reverse(), node.ann.visit(this),
        node.body.visit(this.extend({ env })), node.blocky);
    }

    sCasesBranch(node: A.SCasesBranch): A.CasesBranch {
      let env = this.env;
      let atoms: A.Name[] = [];
      for (const a of node.args.map((arg) => asVariant(arg.bind, A.SBind))) {
        const visitedAnn = a.ann.visit(this);
        const atomEnv = makeAtomFor(a.id, a.shadows, env, bindings,
          (atom) => new C.ValueBind(C.boLocal(a.l, a.id), C.vbLet, atom, visitedAnn));
        env = atomEnv.env;
        atoms = [atomEnv.atom, ...atoms];
      }
      const atomsInOrder = [...atoms].reverse();
      const newArgs = node.args.map((a, i) => {
        const at = atomsInOrder[i];
        const l2 = a.l;
        const typ = a.fieldType;
        const binding = asVariant(a.bind, A.SBind);
        return new A.SCasesBind(l2, typ, new A.SBind(binding.l, false, at, binding.ann.visit(this.extend({ env }))));
      });
      const newBody = node.body.visit(this.extend({ env }));
      return new A.SCasesBranch(node.l, node.patLoc, node.name, newArgs, newBody);
    }

    // s-singleton-cases-branch introduces no new bindings

    sDataExpr(node: A.SDataExpr): A.Expr {
      let env = this.typeEnv;
      let atoms: A.Name[] = [];
      for (const param of node.params) {
        const atomEnv = makeAtomFor(param, false, env, typeBindings,
          (atom) => new C.TypeBind(C.boLocal(node.l, param), C.tbTypeVar, atom, C.tbNone));
        env = atomEnv.env;
        atoms = [atomEnv.atom, ...atoms];
      }
      const withParams = this.extend({ typeEnv: env });
      const result = new A.SDataExpr(node.l, node.name, node.namet, [...atoms].reverse(),
        node.mixins.map((m) => m.visit(withParams)), node.variants.map((v) => v.visit(withParams)),
        node.sharedMembers.map((s) => s.visit(withParams)), node._checkLoc,
        node._check === undefined ? undefined : node._check.visit(withParams));
      datatypes.set(node.name, result);
      return result;
    }

    sLam(node: A.SLam): A.Expr {
      let tyEnv = this.typeEnv;
      let tyAtoms: A.Name[] = [];
      for (const param of node.params) {
        const atomEnv = makeAtomFor(param, false, tyEnv, typeBindings,
          (atom) => new C.TypeBind(C.boLocal(node.l, param), C.tbTypeVar, atom, C.tbNone));
        tyEnv = atomEnv.env;
        tyAtoms = [atomEnv.atom, ...tyAtoms];
      }
      const withParams = this.extend({ typeEnv: tyEnv });
      let env = withParams.env;
      let atoms: A.Name[] = [];
      for (const arg of node.args) {
        const a = asVariant(arg, A.SBind);
        const visitedAnn = a.ann.visit(withParams);
        const atomEnv = makeAtomFor(a.id, a.shadows, env, bindings,
          (atom) => new C.ValueBind(C.boLocal(a.l, a.id), C.vbLet, atom, visitedAnn));
        env = atomEnv.env;
        atoms = [atomEnv.atom, ...atoms];
      }
      const atomsInOrder = [...atoms].reverse();
      const newArgs = node.args.map((arg, i) => {
        const at = atomsInOrder[i];
        const a = asVariant(arg, A.SBind);
        return new A.SBind(a.l, false, at, a.ann.visit(withParams));
      });
      const withParamsAndArgs = withParams.extend({ env });
      const newBody = node.body.visit(withParamsAndArgs);
      const savedNameErrors = nameErrors;
      const newCheck = node._check === undefined ? undefined : node._check.visit(withParams); // Maybe should be self?  Are any type params visible here?
      // Restore the errors to what they were. (_check has already been desugared,
      // so the programmer will see those errors, not the ones from here.)
      nameErrors = savedNameErrors;
      return new A.SLam(node.l, node.name, [...tyAtoms].reverse(), newArgs, node.ann.visit(withParams),
        node.doc, newBody, node._checkLoc, newCheck, node.blocky);
    }

    sMethod(node: A.SMethod): A.Expr {
      let tyEnv = this.typeEnv;
      let tyAtoms: A.Name[] = [];
      for (const param of node.params) {
        const atomEnv = makeAtomFor(param, false, tyEnv, typeBindings,
          (atom) => new C.TypeBind(C.boLocal(field(param, 'l'), param), C.tbTypeVar, atom, C.tbNone));
        tyEnv = atomEnv.env;
        tyAtoms = [atomEnv.atom, ...tyAtoms];
      }
      const withParams = this.extend({ typeEnv: tyEnv });
      let env = withParams.env;
      let atoms: A.Name[] = [];
      for (const arg of node.args) {
        const a = asVariant(arg, A.SBind);
        const visitedAnn = a.ann.visit(withParams);
        const atomEnv = makeAtomFor(a.id, a.shadows, env, bindings,
          (atom) => new C.ValueBind(C.boLocal(a.l, a.id), C.vbLet, atom, visitedAnn));
        env = atomEnv.env;
        atoms = [atomEnv.atom, ...atoms];
      }
      const atomsInOrder = [...atoms].reverse();
      const newArgs = node.args.map((arg, i) => {
        const at = atomsInOrder[i];
        const a = asVariant(arg, A.SBind);
        return new A.SBind(a.l, a.shadows, at, a.ann.visit(withParams));
      });
      const newBody = node.body.visit(withParams.extend({ env }));
      const newCheck = node._check === undefined ? undefined : node._check.visit(withParams);
      return new A.SMethod(node.l, node.name, [...tyAtoms].reverse(), newArgs, node.ann.visit(withParams),
        node.doc, newBody, node._checkLoc, newCheck, node.blocky);
    }

    sMethodField(node: A.SMethodField): A.Member {
      let tyEnv = this.typeEnv;
      let tyAtoms: A.Name[] = [];
      for (const param of node.params) {
        const atomEnv = makeAtomFor(param, false, tyEnv, typeBindings,
          (atom) => new C.TypeBind(C.boLocal(node.l, param), C.tbTypeVar, atom, C.tbNone));
        tyEnv = atomEnv.env;
        tyAtoms = [atomEnv.atom, ...tyAtoms];
      }
      const withParams = this.extend({ typeEnv: tyEnv });
      let env = withParams.env;
      let atoms: A.Name[] = [];
      for (const arg of node.args) {
        const a = asVariant(arg, A.SBind);
        const visitedAnn = a.ann.visit(withParams);
        const atomEnv = makeAtomFor(a.id, a.shadows, env, bindings,
          (atom) => new C.ValueBind(C.boLocal(a.l, a.id), C.vbLet, atom, visitedAnn));
        env = atomEnv.env;
        atoms = [atomEnv.atom, ...atoms];
      }
      const atomsInOrder = [...atoms].reverse();
      const newArgs = node.args.map((arg, i) => {
        const at = atomsInOrder[i];
        const a = asVariant(arg, A.SBind);
        return new A.SBind(a.l, a.shadows, at, a.ann.visit(withParams));
      });
      const newBody = node.body.visit(withParams.extend({ env }));
      const newCheck = node._check === undefined ? undefined : node._check.visit(withParams);
      return new A.SMethodField(node.l, node.name, [...tyAtoms].reverse(), newArgs, node.ann.visit(withParams),
        node.doc, newBody, node._checkLoc, newCheck, node.blocky);
    }

    sAssign(node: A.SAssign): A.Expr {
      const id = node.id;
      if (A.isSName(id)) {
        const s = id.s;
        if (this.env.has(s)) {
          const bind = mapGetValue(this.env, s);
          return new A.SAssign(node.l, bind.atom, node.value.visit(this));
          // This used to examine bind in more detail, and raise an error if it wasn't a var-bind
          // but that's better suited for a later pass
        } else {
          return new A.SAssign(node.l, id, node.value.visit(this));
        }
      } else if (A.isSUnderscore(id)) {
        return new A.SAssign(node.l, id, node.value.visit(this));
      } else {
        throw new InternalCompilerError("Wasn't expecting a non-s-name in resolve-names for assignment: " + toRepr(id));
      }
    }

    sDot(node: A.SDot): A.Expr {
      const obj = node.obj;
      const name = node.field;
      if (A.isSId(obj)) {
        const l2 = obj.l;
        const id = obj.id;
        if (A.isSName(id)) {
          const s = id.s;
          // NOTE(joe): This gives an ordering to names. If somehow we end up with
          // import foo as C
          //
          // C = 5
          // C.x
          //
          // and we _don't_ count it as a shadowing error, then the above
          // would be field-not-found
          if (!this.env.has(s) && this.moduleEnv.has(s)) {
            const modBind = mapGetValue(this.moduleEnv, s);
            const ve = initialEnv.valueByUri(modBind.uri, name);
            if (ve === undefined) {
              nameErrors = [new CE.WfErrSplit("The module " + s + " (" + modBind.uri + ") has no provided member " + name, [node.l, l2]), ...nameErrors];
              return new A.SIdModref(node.l, modBind.atom, modBind.uri, name);
            } else {
              if (C.isVVar(ve)) {
                return new A.SIdVarModref(node.l, modBind.atom, modBind.uri, name);
              } else {
                return new A.SIdModref(node.l, modBind.atom, modBind.uri, name);
              }
            }
          } else {
            return new A.SDot(node.l, obj.visit(this), name);
          }
        } else {
          return new A.SDot(node.l, obj.visit(this), name);
        }
      } else {
        return new A.SDot(node.l, obj.visit(this), name);
      }
    }

    // NOTE(joe): Since there's no syntactic difference between _uses_ of letrec-,
    // let-, and var-bound names, this case disambiguates based on known binding
    // information
    sId(node: A.SId): A.Expr {
      const id = node.id;
      if (A.isSName(id)) {
        const l2 = id.l;
        const s = id.s;
        const vb = this.env.get(s);
        if (vb === undefined) {
          if (this.typeEnv.has(s)) {
            nameErrors = [new CE.TypeIdUsedAsValue(id, mapGetValue(this.typeEnv, s).origin), ...nameErrors];
          }
          return new A.SId(l2, names.sGlobal(s));
        } else {
          if (C.isVbLet(vb.binder)) {
            return new A.SId(l2, vb.atom);
          } else if (C.isVbLetrec(vb.binder)) {
            return new A.SIdLetrec(l2, vb.atom, false);
          } else if (C.isVbVar(vb.binder)) {
            return new A.SIdVar(l2, vb.atom);
          } else {
            throw new InternalCompilerError("s-id: no cases matched");
          }
        }
      } else if (A.isSAtom(id)) {
        return new A.SId(node.l, id);
      } else if (A.isSUnderscore(id)) {
        return new A.SId(node.l, id);
      } else {
        throw new InternalCompilerError("Wasn't expecting a non-s-name in resolve-names id: " + toRepr(id));
      }
    }

    sIdLetrec(node: A.SIdLetrec): A.Expr {
      return new A.SIdLetrec(node.l, handleId(this.env, node.l, node.id), false);
    }

    sIdVar(node: A.SIdVar): A.Expr {
      return new A.SIdVar(node.l, handleId(this.env, node.l, node.id));
    }

    sVariantMember(node: A.SVariantMember): A.VariantMember {
      const bind = node.bind;
      let newBind: A.Bind;
      if (A.isSBind(bind)) {
        const l2 = bind.l;
        const shadows = bind.shadows;
        const name = bind.id;
        const ann = bind.ann;
        const visitedAnn = ann.visit(this);
        const atomEnv = makeAtomFor(name, true, this.env, bindings,
          (atom) => new C.ValueBind(C.boLocal(l2, name), C.vbLet, atom, visitedAnn));
        newBind = new A.SBind(l2, shadows, atomEnv.atom, ann.visit(this));
      } else {
        throw new InternalCompilerError("s-variant-member: no cases matched");
      }
      return new A.SVariantMember(node.l, node.memberType, newBind);
    }

    sBind(node: A.SBind): A.Bind {
      const id = node.id;
      if (A.isSUnderscore(id)) {
        return new A.SBind(node.l, node.shadows, id, node.ann);
      } else {
        throw new InternalCompilerError("Should not reach non-underscore bindings in resolve-names: " + id.key() + " at " + String(node.l));
      }
    }

    aBlank(node: A.ABlank): A.Ann { return A.aBlank; }
    aAny(node: A.AAny): A.Ann { return new A.AAny(node.l); }
    aName(node: A.AName): A.Ann { return handleAnn(node.l, this.typeEnv, node.id); }
    aArrow(node: A.AArrow): A.Ann {
      return new A.AArrow(node.l, node.args.map((a) => a.visit(this)), node.ret.visit(this), node.useParens);
    }
    aArrowArgnames(node: A.AArrowArgnames): A.Ann {
      return new A.AArrowArgnames(node.l, node.args.map((a) => a.visit(this)), node.ret.visit(this), node.useParens);
    }
    aMethod(node: A.AMethod): A.Ann {
      return new A.AMethod(node.l, node.args.map((a) => a.visit(this)), node.ret.visit(this));
    }
    aRecord(node: A.ARecord): A.Ann {
      return new A.ARecord(node.l, node.fields.map((f) => f.visit(this)));
    }
    aApp(node: A.AApp): A.Ann {
      return new A.AApp(node.l, node.ann.visit(this), node.args.map((a) => a.visit(this)));
    }
    aPred(node: A.APred): A.Ann {
      return new A.APred(node.l, node.ann.visit(this), node.exp.visit(this));
    }
    aDot(node: A.ADot): A.Ann {
      const obj = node.obj;
      if (A.isSName(obj)) {
        const s = obj.s;
        const mb = this.moduleEnv.get(s);
        if (mb === undefined) {
          return new A.ADot(node.l, obj, node.field); // NOTE(joe): Should this be error?
        } else {
          return new A.ADot(node.l, mb.atom, node.field);
        }
      } else {
        nameErrors = [new CE.UnderscoreAsAnn(field(obj, 'l')), ...nameErrors];
        return new A.ADot(node.l, obj.visit(this), node.field);
      }
    }
    aField(node: A.AField): A.AField {
      return new A.AField(node.l, node.name, node.ann.visit(this));
    }

    sTableSelect(node: A.STableSelect): A.Expr {
      return new A.STableSelect(node.l, node.columns.map((c) => c.visit(this)), node.table.visit(this));
    }
    sTableExtend(node: A.STableExtend): A.Expr {
      const columnBindsAndEnv = handleColumnBinds(node.columnBinds, this);
      return new A.STableExtend(node.l, columnBindsAndEnv.columnBinds,
        node.extensions.map((e) => e.visit(this.extend({ env: columnBindsAndEnv.env }))));
    }
    sTableUpdate(node: A.STableUpdate): A.Expr {
      const columnBindsAndEnv = handleColumnBinds(node.columnBinds, this);
      return new A.STableUpdate(node.l, columnBindsAndEnv.columnBinds,
        node.updates.map((u) => u.visit(this.extend({ env: columnBindsAndEnv.env }))));
    }
    sTableFilter(node: A.STableFilter): A.Expr {
      const columnBindsAndEnv = handleColumnBinds(node.columnBinds, this);
      return new A.STableFilter(node.l, columnBindsAndEnv.columnBinds,
        node.predicate.visit(this.extend({ env: columnBindsAndEnv.env })));
    }
    sTableOrder(node: A.STableOrder): A.Expr {
      return new A.STableOrder(node.l, node.table.visit(this), node.ordering);
    }
  }

  const namesVisitor = new NamesVisitor(scopeEnvFromEnv(initialEnv), typeEnvFromEnv(initialEnv), moduleEnvFromEnv(initialEnv));

  const visited = p.visit(namesVisitor);
  return new C.ResolvedNames(visited, nameErrors,
    new C.ComputedEnv(moduleBindings, bindings, typeBindings, datatypes,
      finalVisitor!.moduleEnv, finalVisitor!.env, finalVisitor!.typeEnv));
}

export function checkUnboundIdsBadAssignments(ast: A.Program, resolved: C.NameResolution, initialEnv: C.CompileEnvironment): CompileError[] {
  let errors: CompileError[] = []; // THE MUTABLE LIST OF ERRORS
  const env = asVariant(resolved.env, C.ComputedEnv);
  const bindings = env.bindings;
  const typeBindings = env.typeBindings;
  const moduleBindings = env.moduleBindings;
  function addError(err: CompileError): void {
    errors = [err, ...errors];
  }
  function handleId(id: A.Name, loc: C.Loc): boolean {
    if (A.isSUnderscore(id)) {
      addError(new CE.UnderscoreAsExpr(loc));
      return false;
    } else if (A.isSGlobal(id) && initialEnv.globals.values.has(id.toname())) {
      return false;
    } else if (bindings.has(id.key())) {
      return false;
    } else {
      return true;
    }
  }
  class CheckUnboundVisitor extends PostScopeIterVisitor {
    sId(node: A.SId): boolean {
      if (handleId(node.id, node.l)) {
        addError(new CE.UnboundId(new A.SId(node.l, node.id)));
      }
      return true;
    }
    sIdVar(node: A.SIdVar): boolean {
      if (handleId(node.id, node.l)) {
        addError(new CE.UnboundId(new A.SIdVar(node.l, node.id)));
      }
      return true;
    }
    sIdLetrec(node: A.SIdLetrec): boolean {
      if (handleId(node.id, node.l)) {
        addError(new CE.UnboundId(new A.SIdLetrec(node.l, node.id, node.safe)));
      }
      return true;
    }
    sAssign(node: A.SAssign): boolean {
      const idK = node.id.key();
      if (bindings.has(idK)) {
        const binding = mapGetValue(bindings, idK);
        if (!C.isVbVar(binding.binder)) {
          const varLoc = getOriginLoc(binding.origin);
          addError(new CE.BadAssignment(new A.SAssign(node.l, node.id, node.value), varLoc));
        }
      } else {
        addError(new CE.UnboundVar(node.id.toname(), node.l));
      }
      return node.value.visit(this);
    }
    aName(node: A.AName): boolean {
      const id = node.id;
      if (A.isSUnderscore(id)) {
        addError(new CE.UnderscoreAsAnn(id.l));
      } else if (A.isSTypeGlobal(id) && initialEnv.globals.types.has(id.toname())) {
        // nothing
      } else if (typeBindings.has(id.key())) {
        // nothing
      } else {
        addError(new CE.UnboundTypeId(new A.AName(node.l, id)));
      }
      return true;
    }
    aDot(node: A.ADot): boolean {
      const name = node.obj;
      if (A.isSUnderscore(name)) {
        addError(new CE.UnderscoreAsAnn(name.l));
      } else if (A.isSTypeGlobal(name) && initialEnv.globals.types.has(name.toname())) {
        // nothing
      } else if (moduleBindings.has(name.key())) {
        // nothing
      } else {
        addError(new CE.UnboundTypeId(new A.AName(node.l, name)));
      }
      return true;
    }
  }
  ast.visit(new CheckUnboundVisitor());
  return errors;
}
