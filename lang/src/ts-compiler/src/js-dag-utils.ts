/*
  TypeScript port of src/arr/compiler/js-dag-utils.arr.

  Dead-code elimination / register-allocation-ish DAG analysis used by
  anf-loop-compiler.

  Representation notes:
  - NameSet (MutableStringDict<A.Name>) and FrozenNameSet (StringDict<A.Name>)
    both port to Map<string, A.Name> keyed by name.key(). Pyret's freeze()
    calls here all occur at points where the mutable dict is never mutated
    again, so no defensive copy is made (see each call site).
  - The timing instrumentation (time-now() bookkeeping into copy-overhead
    etc.) is preserved with Date.now(); it only feeds debugprint, which is
    disabled, just as in the Pyret source.
*/

import * as A from './ast';
import * as J from './js-ast';
import * as CL from './concat-lists';
import { InternalCompilerError, mapGetValue, mapMergeNow } from './shared';

export type ConcatList<T> = CL.ConcatList<T>;
export type NameSet = Map<string, A.Name>;
export type FrozenNameSet = Map<string, A.Name>;

const clSing = CL.clSing;
const clEmpty = CL.clEmpty;
const clSnoc = CL.clSnoc;

export function nsEmpty(): NameSet { return new Map(); }

let copyOverhead = 0;
let jFunDifference = 0;

function timeNow(): number { return Date.now(); }

function debugprint(_str: string): void {
  // `when false: print(str) end` in the Pyret source
}

export function difference(s1: FrozenNameSet, s2: FrozenNameSet): FrozenNameSet {
  const start = timeNow();
  const s1UnfrozenCopy: NameSet = new Map(s1);
  copyOverhead = copyOverhead + (timeNow() - start);

  removeOverlapNow(s1UnfrozenCopy, s2);

  // freeze(): no further mutation of s1UnfrozenCopy, so no copy needed
  return s1UnfrozenCopy;
}

export function copyNameset(s: NameSet): NameSet {
  const start = timeNow();
  const ans = new Map(s);
  copyOverhead = copyOverhead + (timeNow() - start);
  return ans;
}

// does NOT mutate s1 or s2
export function differenceNow(s1: NameSet, s2: NameSet): NameSet {
  const s1Copy = copyNameset(s1);
  removeOverlapNow(s1Copy, s2);
  return s1Copy;
}

// !mutates s1
export function removeOverlapNow(s1: NameSet, s2: NameSet): void {
  for (const k2 of s2.keys()) {
    s1.delete(k2);
  }
}

// ---------- data GraphNode ----------

export abstract class GraphNodeBase {
  abstract get $name(): string;
}

export class Node extends GraphNodeBase {
  get $name(): 'node' { return 'node'; }
  constructor(
    public _from: string,
    public _to: Map<string, J.Label>,
    public caseBody: J.JCaseT,
    // ref fields (mutable)
    public freeVars: NameSet,
    public usedVars: NameSet,
    public declVars: NameSet,
    public liveVars: NameSet | undefined,
    public liveAfterVars: NameSet | undefined,
    public deadVars: NameSet | undefined,
    public deadAfterVars: NameSet | undefined
  ) { super(); }
}

export type GraphNode = Node;

export function isNode(x: any): x is Node { return x instanceof Node; }

// ---------- data CaseResults ----------

export abstract class CaseResultsBase {
  abstract get $name(): string;
}

export class CExp extends CaseResultsBase {
  get $name(): 'c-exp' { return 'c-exp'; }
  constructor(public exp: J.JExprT, public otherStmts: ConcatList<J.JStmt>) { super(); }
}

export class CField extends CaseResultsBase {
  get $name(): 'c-field' { return 'c-field'; }
  constructor(public field: J.JFieldT, public otherStmts: ConcatList<J.JStmt>) { super(); }
}

export class CBlock extends CaseResultsBase {
  get $name(): 'c-block' { return 'c-block'; }
  constructor(public block: J.JBlockT, public newCases: ConcatList<J.JCaseT>) { super(); }
}

export type CaseResults = CExp | CField | CBlock;

export function isCExp(x: any): x is CExp { return x instanceof CExp; }
export function isCField(x: any): x is CField { return x instanceof CField; }
export function isCBlock(x: any): x is CBlock { return x instanceof CBlock; }

// ---------- data RegisterAllocation ----------

export abstract class RegisterAllocationBase {
  abstract get $name(): string;
}

export class Results extends RegisterAllocationBase {
  get $name(): 'results' { return 'results'; }
  constructor(public body: ConcatList<J.JCaseT>, public discardableVars: FrozenNameSet) { super(); }
}

export type RegisterAllocation = Results;

export function isResults(x: any): x is Results { return x instanceof Results; }

// ---------- used/declared variable analysis ----------

const funDeclVars: Map<string, NameSet> = new Map();
const funUsedVars: Map<string, NameSet> = new Map();
let fromHit = 0;
let fromMiss = 0;

export function usedVarsJblock(b: J.JBlockT, soFar: NameSet): NameSet {
  switch (b.$name) {
    case 'j-block1':
      return usedVarsJstmt(b.stmt, soFar);
    case 'j-block':
      b.stmts.each((s: J.JStmt) => { usedVarsJstmt(s, soFar); });
      return soFar;
    default:
      throw new InternalCompilerError(`usedVarsJblock: unknown block ${(b as any).$name}`);
  }
}

export function declaredVarsJblock(b: J.JBlockT, soFar: NameSet): NameSet {
  switch (b.$name) {
    case 'j-block1':
      return declaredVarsJstmt(b.stmt, soFar);
    case 'j-block':
      b.stmts.each((s: J.JStmt) => { declaredVarsJstmt(s, soFar); });
      return soFar;
    default:
      throw new InternalCompilerError(`declaredVarsJblock: unknown block ${(b as any).$name}`);
  }
}

export function declaredVarsJstmt(s: J.JStmt, soFar: NameSet): NameSet {
  switch (s.$name) {
    case 'j-var':
      soFar.set(s.name.key(), s.name);
      return soFar;
    case 'j-if1':
      return declaredVarsJblock(s.consq, soFar);
    case 'j-if':
      soFar = declaredVarsJblock(s.consq, soFar);
      return declaredVarsJblock(s.alt, soFar);
    case 'j-return':
      return soFar;
    case 'j-try-catch':
      soFar = declaredVarsJblock(s.body, soFar);
      return declaredVarsJblock(s.catch, soFar);
    case 'j-throw':
      return soFar;
    case 'j-expr':
      return soFar;
    case 'j-break':
      return soFar;
    case 'j-continue':
      return soFar;
    case 'j-switch':
      s.branches.each((b: J.JCaseT) => { declaredVarsJcase(b, soFar); });
      return soFar;
    case 'j-while':
      return declaredVarsJblock(s.body, soFar);
    case 'j-for':
      soFar = declaredVarsJblock(s.body, soFar);
      if (s.createVar && J.isJAssign(s.init)) {
        soFar.set(s.init.name.key(), s.init.name);
      }
      return soFar;
    default:
      throw new InternalCompilerError(`declaredVarsJstmt: unknown stmt ${(s as any).$name}`);
  }
}

export function usedVarsJstmt(s: J.JStmt, soFar: NameSet): NameSet {
  switch (s.$name) {
    case 'j-var':
      soFar = usedVarsJexpr(s.rhs, soFar);
      soFar.delete(s.name.key());
      return soFar;
    case 'j-if1':
      soFar = usedVarsJexpr(s.cond, soFar);
      return usedVarsJblock(s.consq, soFar);
    case 'j-if':
      soFar = usedVarsJexpr(s.cond, soFar);
      soFar = usedVarsJblock(s.consq, soFar);
      return usedVarsJblock(s.alt, soFar);
    case 'j-return':
      return usedVarsJexpr(s.expr, soFar);
    case 'j-try-catch':
      soFar = usedVarsJblock(s.catch, soFar);
      soFar.delete(s.exn.key());
      return usedVarsJblock(s.body, soFar);
    case 'j-throw':
      return usedVarsJexpr(s.exp, soFar);
    case 'j-expr':
      return usedVarsJexpr(s.expr, soFar);
    case 'j-break':
      return soFar;
    case 'j-continue':
      return soFar;
    case 'j-switch':
      soFar = usedVarsJexpr(s.exp, soFar);
      s.branches.each((b: J.JCaseT) => { usedVarsJcase(b, soFar); });
      return soFar;
    case 'j-while':
      soFar = usedVarsJexpr(s.cond, soFar);
      return usedVarsJblock(s.body, soFar);
    case 'j-for':
      soFar = usedVarsJexpr(s.init, soFar);
      soFar = usedVarsJexpr(s.update, soFar);
      soFar = usedVarsJblock(s.body, soFar);
      if (s.createVar && J.isJAssign(s.init)) {
        soFar.delete(s.init.name.key());
      }
      return soFar;
    default:
      throw new InternalCompilerError(`usedVarsJstmt: unknown stmt ${(s as any).$name}`);
  }
}

export function usedVarsJexpr(e: J.JExprT, soFar: NameSet): NameSet {
  switch (e.$name) {
    case 'j-sourcenode':
      return usedVarsJexpr(e.expr, soFar);
    case 'j-parens':
      return usedVarsJexpr(e.exp, soFar);
    case 'j-unop':
      return usedVarsJexpr(e.exp, soFar);
    case 'j-binop':
      soFar = usedVarsJexpr(e.left, soFar);
      return usedVarsJexpr(e.right, soFar);
    case 'j-fun': {
      const start = timeNow();
      const totalBefore = jFunDifference;
      let declared: NameSet;
      if (funDeclVars.has(e.id)) {
        declared = funDeclVars.get(e.id)!;
      } else {
        const ans = declaredVarsJblock(e.body, nsEmpty());
        funDeclVars.set(e.id, ans);
        declared = ans;
      }
      let fromBody: NameSet;
      if (funUsedVars.has(e.id)) {
        fromHit = fromHit + 1;
        mapMergeNow(soFar, funUsedVars.get(e.id)!);
        fromBody = soFar;
      } else {
        fromMiss = fromMiss + 1;
        const cleanFromBody = usedVarsJblock(e.body, nsEmpty());
        funUsedVars.set(e.id, cleanFromBody);
        mapMergeNow(soFar, cleanFromBody);
        fromBody = soFar;
      }
      for (const d of declared.keys()) {
        fromBody.delete(d);
      }
      jFunDifference = jFunDifference + (timeNow() - start - (jFunDifference - totalBefore));
      e.args.each((a: A.Name) => { fromBody.delete(a.key()); });
      return soFar;
    }
    case 'j-new':
      soFar = usedVarsJexpr(e.func, soFar);
      e.args.each((a: J.JExprT) => { usedVarsJexpr(a, soFar); });
      return soFar;
    case 'j-app':
      soFar = usedVarsJexpr(e.func, soFar);
      e.args.each((a: J.JExprT) => { usedVarsJexpr(a, soFar); });
      return soFar;
    case 'j-method':
      soFar = usedVarsJexpr(e.obj, soFar);
      e.args.each((a: J.JExprT) => { usedVarsJexpr(a, soFar); });
      return soFar;
    case 'j-ternary':
      soFar = usedVarsJexpr(e.test, soFar);
      soFar = usedVarsJexpr(e.consq, soFar);
      return usedVarsJexpr(e.altern, soFar);
    case 'j-assign':
      soFar = usedVarsJexpr(e.rhs, soFar);
      soFar.set(e.name.key(), e.name);
      return soFar;
    case 'j-bracket-assign':
      soFar = usedVarsJexpr(e.obj, soFar);
      soFar = usedVarsJexpr(e.field, soFar);
      return usedVarsJexpr(e.rhs, soFar);
    case 'j-dot-assign':
      soFar = usedVarsJexpr(e.obj, soFar);
      return usedVarsJexpr(e.rhs, soFar);
    case 'j-dot':
      return usedVarsJexpr(e.obj, soFar);
    case 'j-bracket':
      soFar = usedVarsJexpr(e.obj, soFar);
      return usedVarsJexpr(e.field, soFar);
    case 'j-list':
      e.elts.each((elt: J.JExprT) => { usedVarsJexpr(elt, soFar); });
      return soFar;
    case 'j-obj':
      e.fields.each((f: J.JFieldT) => { usedVarsJfield(f, soFar); });
      return soFar;
    case 'j-id':
      soFar.set(e.id.key(), e.id);
      return soFar;
    case 'j-str':
      return soFar;
    case 'j-num':
      return soFar;
    case 'j-true':
      return soFar;
    case 'j-false':
      return soFar;
    case 'j-null':
      return soFar;
    case 'j-undefined':
      return soFar;
    case 'j-label':
      return soFar;
    case 'j-raw-code':
      return soFar;
    default:
      throw new InternalCompilerError(`usedVarsJexpr: unknown expr ${(e as any).$name}`);
  }
}

export function declaredVarsJcase(c: J.JCaseT, soFar: NameSet): NameSet {
  switch (c.$name) {
    case 'j-case':
      return declaredVarsJblock(c.body, soFar);
    case 'j-default':
      return declaredVarsJblock(c.body, soFar);
    default:
      throw new InternalCompilerError(`declaredVarsJcase: unknown case ${(c as any).$name}`);
  }
}

export function usedVarsJcase(c: J.JCaseT, soFar: NameSet): NameSet {
  switch (c.$name) {
    case 'j-case':
      soFar = usedVarsJexpr(c.exp, soFar);
      return usedVarsJblock(c.body, soFar);
    case 'j-default':
      return usedVarsJblock(c.body, soFar);
    default:
      throw new InternalCompilerError(`usedVarsJcase: unknown case ${(c as any).$name}`);
  }
}

export function usedVarsJfield(f: J.JFieldT, soFar: NameSet): NameSet {
  return usedVarsJexpr(f.value, soFar);
}

// ---------- liveness ----------

export function computeLiveVars(nInit: GraphNode, dag: Map<string, GraphNode>): NameSet {
  // Memoized post-order over the case graph, done with an explicit stack:
  // the longest path grows with the number of split points (roughly,
  // statements), so plain recursion overflows fixed-size stacks (e.g.
  // browsers) on long functions. Successor results are merged in the
  // same n._to iteration order as the recursive formulation.
  const stack: GraphNode[] = [nInit];
  while (stack.length > 0) {
    const n = stack[stack.length - 1];
    if (n.liveVars !== undefined) {
      stack.pop();
      continue;
    }
    let allSuccessorsDone = true;
    for (const followKey of n._to.keys()) {
      // Note: this is false only for the exit block of the function
      // which isn't currently present in the DAG (todo: why not?)
      if (dag.has(followKey)) {
        const next = dag.get(followKey)!;
        if (next.liveVars === undefined) {
          stack.push(next);
          allSuccessorsDone = false;
        }
      }
    }
    if (!allSuccessorsDone) {
      continue;
    }
    const liveAfter = copyNameset(n.freeVars);
    for (const followKey of n._to.keys()) {
      if (dag.has(followKey)) {
        const next = dag.get(followKey)!;
        mapMergeNow(liveAfter, next.liveVars!);
      }
    }
    const decls = n.declVars;
    const live = differenceNow(liveAfter, decls);
    const deadAfter = differenceNow(decls, liveAfter);
    const dead = differenceNow(deadAfter, n.usedVars);

    n.liveAfterVars = liveAfter;
    n.liveVars = live;
    n.deadAfterVars = deadAfter;
    n.deadVars = dead;
    stack.pop();
  }
  return nInit.liveVars!;
}

export function stmtsOf(blk: J.JBlockT): ConcatList<J.JStmt> {
  switch (blk.$name) {
    case 'j-block1':
      return clSing(blk.stmt);
    case 'j-block':
      return blk.stmts;
    default:
      throw new InternalCompilerError(`stmtsOf: unknown block ${(blk as any).$name}`);
  }
}

export function findStepsTo(
  stmts: ConcatList<J.JStmt>,
  step: A.Name,
  acc: Map<string, J.Label>,
  casesDispatches: ConcatList<J.JStmt>
): Map<string, J.Label> {
  return stmts.foldr((acc2: Map<string, J.Label>, stmt: J.JStmt): Map<string, J.Label> => {
    switch (stmt.$name) {
      case 'j-var':
        return acc2;
      case 'j-if1':
        return findStepsTo(stmtsOf(stmt.consq), step, acc2, casesDispatches);
      case 'j-if': {
        const acc3 = findStepsTo(stmtsOf(stmt.consq), step, acc2, casesDispatches);
        return findStepsTo(stmtsOf(stmt.alt), step, acc3, casesDispatches);
      }
      case 'j-return':
        return acc2;
      case 'j-try-catch':
        return acc2; // ignoring for now, because we know we don't use these
      case 'j-throw':
        return acc2;
      case 'j-expr': {
        const expr = stmt.expr;
        if (J.isJAssign(expr) && expr.name.key() === step.key()) {
          if (J.isJLabel(expr.rhs)) {
            // simple assignment statement to $step
            acc2.set(String(expr.rhs.label.get()), expr.rhs.label);
            return acc2;
          } else if (J.isJBinop(expr.rhs) && J.isJOr(expr.rhs.op)) {
            // $step gets a cases dispatch
            // ASSUMES that the dispatch table is assigned before toplevel is defined.
            // (see cases-dispatches in anf-loop-compiler.arr)
            const right = expr.rhs.right as J.JLabel;
            acc2.set(String(right.label.get()), right.label);
            const dispatchId = ((expr.rhs.left as J.JDot).obj as J.JId).id;
            const found = casesDispatches.find(
              (elt: J.JStmt) => J.isJVar(elt) && elt.name.key() === dispatchId.key()
            );
            if (found === undefined) {
              throw new InternalCompilerError('findStepsTo: no cases dispatch found for ' + dispatchId.key());
            }
            const nowLooking = (found as J.JVar).rhs as J.JObj;
            nowLooking.fields.foldl((accF: Map<string, J.Label>, field: J.JFieldT) => {
              const fieldLabel = (field.value as J.JLabel).label;
              accF.set(String(fieldLabel.get()), fieldLabel);
              return accF;
            }, acc2);
            return acc2;
          } else if (J.isJNum(expr.rhs)) {
            return acc2;
          } else if (J.isJTernary(expr.rhs)) {
            // ASSUMES that the only current use of $step = ( ? : )
            // comes from compile-split-if
            const consq = expr.rhs.consq as J.JLabel;
            const altern = expr.rhs.altern as J.JLabel;
            acc2.set(String(consq.label.get()), consq.label);
            acc2.set(String(altern.label.get()), altern.label);
            return acc2;
          } else {
            throw new InternalCompilerError('Should not happen: ' + expr.rhs.$name);
          }
        } else {
          return acc2;
        }
      }
      case 'j-break':
        return acc2;
      case 'j-continue':
        return acc2;
      case 'j-switch':
        return acc2;
      case 'j-while':
        return acc2;
      case 'j-for':
        return acc2;
      default:
        throw new InternalCompilerError(`findStepsTo: unknown stmt ${(stmt as any).$name}`);
    }
  }, acc);
}

export function ignorable(rhs: J.JExprT): boolean {
  switch (rhs.$name) {
    case 'j-sourcenode':
      return ignorable(rhs.expr);
    case 'j-parens':
      return ignorable(rhs.exp);
    case 'j-ternary':
      return ignorable(rhs.test) && ignorable(rhs.consq) && ignorable(rhs.altern);
    case 'j-dot':
      return ignorable(rhs.obj);
    case 'j-bracket':
      return ignorable(rhs.obj) && ignorable(rhs.field);
    case 'j-list':
      return rhs.elts.all(ignorable);
    case 'j-obj':
      return rhs.fields.all((f: J.JFieldT) => ignorable(f.value));
    case 'j-id':
      return true;
    case 'j-str':
      return true;
    case 'j-num':
      return true;
    case 'j-true':
      return true;
    case 'j-false':
      return true;
    case 'j-undefined':
      return true;
    case 'j-null':
      return true;
    default:
      return false;
  }
}

// ---------- dead-variable elimination ----------

export function elimDeadVarsJblock(b: J.JBlockT, deadVars: FrozenNameSet): J.JBlockT {
  switch (b.$name) {
    case 'j-block1':
      if (isPointlessJVar(b.stmt, deadVars)) { return new J.JBlock(clEmpty); }
      else { return b; }
    case 'j-block':
      return new J.JBlock(elimDeadVarsJstmts(b.stmts, deadVars));
    default:
      throw new InternalCompilerError(`elimDeadVarsJblock: unknown block ${(b as any).$name}`);
  }
}

export function isPointlessJVar(s: J.JStmt, deadVars: FrozenNameSet): boolean {
  switch (s.$name) {
    case 'j-var':
      return deadVars.has(s.name.key()) && ignorable(s.rhs);
    default:
      return false;
  }
}

export function elimDeadVarsJstmts(stmts: ConcatList<J.JStmt>, deadVars: FrozenNameSet): ConcatList<J.JStmt> {
  return stmts.foldl((acc: ConcatList<J.JStmt>, s: J.JStmt): ConcatList<J.JStmt> => {
    switch (s.$name) {
      case 'j-var':
        if (deadVars.has(s.name.key())) {
          if (ignorable(s.rhs)) { return acc; }
          else { return clSnoc(acc, new J.JExpr(s.rhs)); }
        } else {
          return clSnoc(acc, s);
        }
      case 'j-if1':
        return clSnoc(acc, new J.JIf1(s.cond, elimDeadVarsJblock(s.consq, deadVars)));
      case 'j-if':
        return clSnoc(acc,
          new J.JIf(s.cond, elimDeadVarsJblock(s.consq, deadVars), elimDeadVarsJblock(s.alt, deadVars)));
      case 'j-return':
        return clSnoc(acc, s);
      case 'j-try-catch':
        return clSnoc(acc,
          new J.JTryCatch(elimDeadVarsJblock(s.body, deadVars), s.exn, elimDeadVarsJblock(s.catch, deadVars)));
      case 'j-throw':
        return clSnoc(acc, s);
      case 'j-expr':
        return clSnoc(acc, s);
      case 'j-break':
        return clSnoc(acc, s);
      case 'j-continue':
        return clSnoc(acc, s);
      case 'j-switch': {
        const newSwitchBranches = s.branches.map((b: J.JCaseT) => elimDeadVarsJcase(b, deadVars));
        return clSnoc(acc, new J.JSwitch(s.exp, newSwitchBranches));
      }
      case 'j-while':
        return clSnoc(acc, new J.JWhile(s.cond, elimDeadVarsJblock(s.body, deadVars)));
      case 'j-for':
        return clSnoc(acc, new J.JFor(s.createVar, s.init, s.cond, s.update, elimDeadVarsJblock(s.body, deadVars)));
      default:
        throw new InternalCompilerError(`elimDeadVarsJstmts: unknown stmt ${(s as any).$name}`);
    }
  }, clEmpty);
}

export function elimDeadVarsJcase(c: J.JCaseT, deadVars: FrozenNameSet): J.JCaseT {
  switch (c.$name) {
    case 'j-default':
      return new J.JDefault(elimDeadVarsJblock(c.body, deadVars));
    case 'j-case':
      return new J.JCase(c.exp, elimDeadVarsJblock(c.body, deadVars));
    default:
      throw new InternalCompilerError(`elimDeadVarsJcase: unknown case ${(c as any).$name}`);
  }
}

let step1Total = 0;
let step2Total = 0;
let step3Total = 0;
let step4Total = 0;

export function simplify(
  _addPhase: (phase: string, data: any) => any,
  bodyCases: ConcatList<J.JCaseT>,
  step: A.Name,
  casesDispatches: ConcatList<J.JStmt>
): RegisterAllocation {
  const start = timeNow();
  fromHit = 0;
  fromMiss = 0;
  const accDag = new Map<string, GraphNode>();
  bodyCases.each((bodyCase: J.JCaseT) => {
    if (J.isJCase(bodyCase)) {
      const label = String((bodyCase.exp as J.JLabel).label.get());
      accDag.set(label,
        new Node(
          label,
          J.isJBlock1(bodyCase.body)
            ? findStepsTo(clSing(bodyCase.body.stmt), step, new Map<string, J.Label>(), casesDispatches)
            : findStepsTo((bodyCase.body as J.JBlock).stmts, step, new Map<string, J.Label>(), casesDispatches),
          bodyCase,
          nsEmpty(), nsEmpty(), nsEmpty(), undefined, undefined, undefined, undefined));
    }
  });
  let startCopy = timeNow();
  // freeze(): accDag is never mutated below, so the dag aliases it directly
  const dag: Map<string, GraphNode> = accDag;
  copyOverhead = copyOverhead + (timeNow() - startCopy);
  const step1 = timeNow() - start;
  step1Total = step1Total + step1;

  // `labels` is unused (it fed the commented-out live-range code), but the
  // label.get() calls are preserved because Label allocation is memoized and
  // order-sensitive.
  const labels = bodyCases.foldr((acc: number[], bodyCase: J.JCaseT) => {
    if (J.isJCase(bodyCase)) { acc.push((bodyCase.exp as J.JLabel).label.get()); }
    return acc;
  }, [] as number[]);
  void labels;
  const step2 = timeNow() - step1 - start;
  step2Total = step2Total + step2;

  for (const lbl of dag.keys()) {
    const n = dag.get(lbl)!;
    n.declVars = declaredVarsJcase(n.caseBody, new Map());
    n.usedVars = usedVarsJcase(n.caseBody, new Map());
    n.freeVars = differenceNow(n.usedVars, n.declVars);
  }
  for (const lbl of dag.keys()) {
    const n = dag.get(lbl)!;
    computeLiveVars(n, dag);
  }
  const step3 = timeNow() - step2 - step1 - start;
  step3Total = step3Total + step3;

  const acc = nsEmpty();
  for (const lbl of dag.keys()) {
    const n = dag.get(lbl)!;
    if (n.deadAfterVars !== undefined) {
      mapMergeNow(acc, n.deadAfterVars);
    }
  }
  startCopy = timeNow();
  // freeze(): acc is never mutated again
  const discardableVars: FrozenNameSet = acc;
  copyOverhead = copyOverhead + (timeNow() - startCopy);

  const deadAssignmentEliminated = bodyCases.map((bodyCase: J.JCaseT): J.JCaseT => {
    const n = mapGetValue(dag, String(((bodyCase as J.JCase).exp as J.JLabel).label.get()));
    if (n.deadVars === undefined) {
      return bodyCase;
    } else {
      startCopy = timeNow();
      // freeze(): the node's dead-vars set is never mutated again
      const deadFrozen: FrozenNameSet = n.deadVars;
      copyOverhead = copyOverhead + (timeNow() - startCopy);
      return elimDeadVarsJcase(bodyCase, deadFrozen);
    }
  });
  const step4 = timeNow() - step3 - step2 - step1 - start;
  step4Total = step4Total + step4;

  debugprint('Cumulative overhead from copying string-dicts: ' + String(copyOverhead) + '\n');
  debugprint('Cumulative overhead from differencing function sets: ' + String(jFunDifference) + '\n');

  return new Results(deadAssignmentEliminated, discardableVars);
}
