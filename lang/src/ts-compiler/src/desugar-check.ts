/*
  Ported from: src/arr/compiler/desugar-check.arr
  See CONVENTIONS.md.
*/

import * as A from './ast';
import * as G from './gensym';
import * as U from './ast-util';
import { DefaultMapVisitor } from './ast-visitors';
import { Loc } from './srcloc';
import { raise, nonNull } from './shared';

// ---------- data CheckInfo ----------

export class CheckInfo {
  get $name(): 'check-info' { return 'check-info'; }
  constructor(public l: Loc, public name: string, public body: A.Expr,
      public keywordCheck: boolean) {}
}
export function checkInfo(l: Loc, name: string, body: A.Expr, keywordCheck: boolean): CheckInfo {
  return new CheckInfo(l, name, body, keywordCheck);
}
export function isCheckInfo(x: any): x is CheckInfo { return x instanceof CheckInfo; }

export function astPretty(ast: A.Expr): A.Expr {
  return new A.SStr(ast.l, (ast.tosource().pretty(80) as string[]).join('\n'));
}

export function astLam(ast: A.Expr): A.Expr {
  return new A.SLam(ast.l, '', [], [], A.aBlank, '', ast, undefined, undefined, true);
}

export const flatPrimApp = new A.PrimAppInfoC(false);

export function astSrcloc(l: Loc): A.Expr {
  return new A.SPrimApp(l, 'makeSrcloc', [new A.SSrcloc(l, l)], flatPrimApp);
}

class CheckStmtsVisitor extends DefaultMapVisitor {
  sCheckTest(node: A.SCheckTest): A.Expr {
    const l = node.l;
    const op = node.op;
    const refinement = node.refinement;
    const left = node.left;
    const right = node.right;
    const cause = node.cause;
    const checkOp = (fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [astLam(left), astLam(nonNull(right)), astSrcloc(l)]);
    const checkOpCause = (cause2: A.Expr, fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [astLam(left), astLam(nonNull(right)), astLam(cause2), astSrcloc(l)]);
    const checkRefinement = (refinement2: A.Expr, fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [refinement2, astLam(left), astLam(nonNull(right)), astSrcloc(l)]);
    const checkRefinementCause = (refinement2: A.Expr, cause2: A.Expr, fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [refinement2, astLam(left), astLam(nonNull(right)), astLam(cause2), astSrcloc(l)]);
    const checkRaises = (fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [astLam(left), nonNull(right), astSrcloc(l)]);
    const checkRaisesNot = (fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [astLam(left), astSrcloc(l)]);
    const checkRaisesCause = (cause2: A.Expr, fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [astLam(left), nonNull(right), astLam(cause2), astSrcloc(l)]);
    const checkRaisesNotCause = (cause2: A.Expr, fieldname: string): A.Expr =>
      new A.SApp(l, new A.SDot(l, U.checkers(l), fieldname),
        [astLam(left), astLam(cause2), astSrcloc(l)]);
    switch (op.$name) {
      case 's-op-is': {
        if (refinement === undefined) {
          if (cause === undefined) { return checkOp('check-is'); }
          else { return checkOpCause(cause, 'check-is-cause'); }
        } else {
          if (cause === undefined) { return checkRefinement(refinement, 'check-is-refinement'); }
          else { return checkRefinementCause(refinement, cause, 'check-is-refinement-cause'); }
        }
      }
      case 's-op-is-roughly': {
        if (cause === undefined) { return checkOp('check-is-roughly'); }
        else { return checkOpCause(cause, 'check-is-roughly-cause'); }
      }
      case 's-op-is-not': {
        if (refinement === undefined) {
          if (cause === undefined) { return checkOp('check-is-not'); }
          else { return checkOpCause(cause, 'check-is-not-cause'); }
        } else {
          if (cause === undefined) { return checkRefinement(refinement, 'check-is-not-refinement'); }
          else { return checkRefinementCause(refinement, cause, 'check-is-not-refinement-cause'); }
        }
      }
      case 's-op-is-not-roughly': {
        if (cause === undefined) { return checkOp('check-is-not-roughly'); }
        else { return checkOpCause(cause, 'check-is-not-roughly-cause'); }
      }
      case 's-op-is-op': {
        const refinement2 = new A.SId(l, new A.SName(l, A.getOpFunName(op.op)));
        if (cause === undefined) { return checkRefinement(refinement2, 'check-is-refinement'); }
        else { return checkRefinementCause(refinement2, cause, 'check-is-refinement-cause'); }
      }
      case 's-op-is-not-op': {
        const refinement2 = new A.SId(l, new A.SName(l, A.getOpFunName(op.op)));
        if (cause === undefined) { return checkRefinement(refinement2, 'check-is-not-refinement'); }
        else { return checkRefinementCause(refinement2, cause, 'check-is-not-refinement-cause'); }
      }
      case 's-op-satisfies': {
        if (cause === undefined) { return checkOp('check-satisfies-delayed'); }
        else { return checkOpCause(cause, 'check-satisfies-delayed-cause'); }
      }
      case 's-op-satisfies-not': {
        if (cause === undefined) { return checkOp('check-satisfies-not-delayed'); }
        else { return checkOpCause(cause, 'check-satisfies-not-delayed-cause'); }
      }
      case 's-op-raises': {
        if (cause === undefined) { return checkRaises('check-raises-str'); }
        else { return checkRaisesCause(cause, 'check-raises-str-cause'); }
      }
      case 's-op-raises-not': {
        if (cause === undefined) { return checkRaisesNot('check-raises-not'); }
        else { return checkRaisesNotCause(cause, 'check-raises-not-cause'); }
      }
      case 's-op-raises-other': {
        if (cause === undefined) { return checkRaises('check-raises-other-str'); }
        else { return checkRaisesCause(cause, 'check-raises-other-str-cause'); }
      }
      case 's-op-raises-satisfies': {
        if (cause === undefined) { return checkRaises('check-raises-satisfies'); }
        else { return checkRaisesCause(cause, 'check-raises-satisfies-cause'); }
      }
      case 's-op-raises-violates': {
        if (cause === undefined) { return checkRaises('check-raises-violates'); }
        else { return checkRaisesCause(cause, 'check-raises-violates-cause'); }
      }
      default:
        return raise('Check test operator ' + (op as A.CheckOp).label() + ' not yet implemented at ' + String(l));
    }
  }
  sCheck(node: A.SCheck): A.Expr {
    // collapse check blocks into top layer
    return node.body.visit(this);
  }
}

export const checkStmtsVisitor = new CheckStmtsVisitor();

export function getChecks(stmts: A.Expr[]): CheckInfo[] {
  let standaloneCounter = 0;
  // Note: manually writing this fold, rather than using existing functions
  // foldr produces numbers that are backwards, and
  // foldl would require an extra list allocation and reversal
  function addCheck(stmts2: A.Expr[]): CheckInfo[] {
    const result: CheckInfo[] = [];
    for (const stmt of stmts2) {
      switch (stmt.$name) {
        case 's-fun': {
          if (stmt._check !== undefined) {
            result.push(checkInfo(stmt.l, stmt.name, stmt._check.visit(checkStmtsVisitor), true));
          }
          break;
        }
        case 's-data': {
          if (stmt._check !== undefined) {
            result.push(checkInfo(stmt.l, stmt.name, stmt._check.visit(checkStmtsVisitor), true));
          }
          break;
        }
        case 's-check': {
          let checkName: string;
          if (stmt.name === undefined) {
            standaloneCounter = standaloneCounter + 1;
            checkName = (stmt.keywordCheck ? 'check-block-' : 'examples-block-')
              + String(standaloneCounter);
          } else {
            checkName = stmt.name;
          }
          result.push(checkInfo(stmt.l, checkName, stmt.body.visit(checkStmtsVisitor), stmt.keywordCheck));
          break;
        }
        default:
          break;
      }
    }
    return result;
  }
  return addCheck(stmts);
}

export function createCheckBlock(l: Loc, checks: CheckInfo[]): A.Expr {
  function createChecker(c: CheckInfo): A.Expr {
    const l2 = c.l;
    const checkFun = makeLam(l2, [], c.body);
    return new A.SObj(l2, [
      new A.SDataField(l2, 'name', new A.SStr(l2, c.name)),
      new A.SDataField(l2, 'run', checkFun),
      new A.SDataField(l2, 'keyword-check', new A.SBool(l2, c.keywordCheck)),
      new A.SDataField(l2, 'location', new A.SPrimApp(l2, 'makeSrcloc', [new A.SSrcloc(l2, l2)], flatPrimApp))
    ]);
  }
  const checkers = checks.map(createChecker);
  return new A.SBlock(l, [
    new A.SApp(l, new A.SDot(l, U.checkers(l), 'run-checks'), [
      new A.SStr(l, (l as any).source),
      // TODO(joe): need to make this a s-global somehow
      new A.SApp(l, new A.SDot(l, new A.SPrimVal(l, 'builtins'), 'raw-array-to-list'), [new A.SArray(l, checkers)])
    ])
  ]);
}

export function makeLam(l: Loc, args: A.Name[], body: A.Expr): A.Expr {
  return new A.SLam(l, '', [], args.map((sym) => new A.SBind(l, false, sym, A.aBlank)), A.aBlank, '', body, undefined, undefined, true);
}

class NoChecksVisitor extends DefaultMapVisitor {
  sBlock(node: A.SBlock): A.Expr {
    // for L.foldr(acc from empty, stmt from stmts)
    const newStmts: A.Expr[] = [];
    for (let i = node.stmts.length - 1; i >= 0; i--) {
      const newStmt = node.stmts[i].visit(this);
      if (A.isSId(newStmt) && A.isSName(newStmt.id) && (newStmt.id.s === '$elidedCheckBlock')) {
        // drop it
      } else {
        newStmts.unshift(newStmt);
      }
    }
    return new A.SBlock(node.l, newStmts);
  }
  sFun(node: A.SFun): A.Expr {
    return new A.SFun(node.l, node.name, node.params, node.args, node.ann, node.doc, node.body, undefined, undefined, node.blocky);
  }
  sData(node: A.SData): A.Expr {
    return new A.SData(node.l, node.name, node.params, node.mixins, node.variants, node.sharedMembers, undefined, undefined);
  }
  sLam(node: A.SLam): A.Expr {
    return new A.SLam(node.l, node.name, node.params, node.args, node.ann, node.doc, node.body, undefined, undefined, node.blocky);
  }
  sCheck(node: A.SCheck): A.Expr {
    // Because we now weave contracts in, and because examples blocks can go between
    // mutually-recursive functions, we need to change our desugaring of elided check blocks
    // to be completely removed, rather than be a nilpotent expression
    return new A.SId(node.l, new A.SName(node.l, '$elidedCheckBlock'));
  }
}

export const noChecksVisitor = new NoChecksVisitor();

class CheckVisitor extends DefaultMapVisitor {
  sBlock(node: A.SBlock): A.Expr {
    const l = node.l;
    const checksToPerform = getChecks(node.stmts);
    const dsStmts = node.stmts.map((s) => s.visit(this));
    const doChecks = createCheckBlock(l, checksToPerform);
    if (checksToPerform.length === 0) { return new A.SBlock(l, dsStmts); }
    else if (dsStmts.length === 0) { return raise('Empty block'); }
    else {
      const idResult = new A.SName(l, G.makeName('result-after-checks'));
      const lastExpr = dsStmts[dsStmts.length - 1];
      return new A.SBlock(
        l,
        [...dsStmts.slice(0, dsStmts.length - 1),
          new A.SUserBlock(
            l,
            new A.SBlock(l, [
              new A.SLet(l, new A.SBind(l, true, idResult, A.aBlank), lastExpr, false),
              doChecks,
              new A.SId(l, idResult)
            ]))
        ]
      );
    }
  }
}

export const checkVisitor = new CheckVisitor();

export function desugarCheck(prog: A.Program): A.Program {
  /*
    Desugars all check blocks to be calls to the current checker
    Preconditions on prog:
      - well-formed
    Postconditions on prog:
      - contains no s-check or s-check-test statements
      - all where blocks on s-lam, s-fun, s-data, s-method are none
  */
  return prog.visit(checkVisitor);
}

export function desugarNoChecks(prog: A.Program): A.Program {
  return prog.visit(noChecksVisitor);
}
