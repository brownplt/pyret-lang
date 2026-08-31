/*
  Ported from: src/arr/compiler/desugar-post-tc.arr
  See CONVENTIONS.md.
*/

import * as A from './ast';
import * as D from './desugar';
import * as C from './compile-structs';
import { PostScopeMapVisitor } from './ast-visitors';
import { Loc } from './srcloc';

export const mkId = D.mkId;
export const noBranchesExn = D.noBranchesExn;
export const flatPrimApp = new A.PrimAppInfoC(false);

export function noCasesExn(l: Loc, val: A.Expr): A.Expr {
  return new A.SPrimApp(l, 'throwNoCasesMatched', [new A.SSrcloc(l, l), val], flatPrimApp);
}

class DesugarVisitor extends PostScopeMapVisitor {
  sTemplate(node: A.STemplate): A.Expr {
    return new A.SPrimApp(node.l, 'throwUnfinishedTemplate', [new A.SSrcloc(node.l, node.l)], flatPrimApp);
  }
  sCasesElse(node: A.SCasesElse): A.Expr {
    const l = node.l;
    const name = A.globalNames.makeAtom('cases');
    const typCompiled = node.typ.visit(this);
    const valExp = node.val.visit(this);
    const valId = new A.SId(l, name);
    return A.sScopeLetBlock(l, [new A.SLetBind(l, new A.SBind(l, false, name, typCompiled), valExp)],
      new A.SCasesElse(l, A.aBlank, valId, node.branches.map((b) => b.visit(this)),
        node._else.visit(this), true));
  }
  sCases(node: A.SCases): A.Expr {
    const l = node.l;
    const name = A.globalNames.makeAtom('cases');
    const typCompiled = node.typ.visit(this);
    const valExp = node.val.visit(this);
    const valId = new A.SId(l, name);
    return A.sScopeLetBlock(l, [new A.SLetBind(l, new A.SBind(l, false, name, typCompiled), valExp)],
      new A.SCasesElse(l, A.aBlank, valId, node.branches.map((b) => b.visit(this)),
        new A.SBlock(l, [noCasesExn(l, valId)]), true));
  }
  sCheck(node: A.SCheck): A.Expr {
    return new A.SId(node.l, new A.SGlobal('nothing'));
  }
}

export const desugarVisitor = new DesugarVisitor();

export function desugarPostTc(program: A.Program, compileEnv: C.CompileEnvironment): A.Program {
  /*
    Desugar non-scope and non-check based constructs.
    Preconditions on program:
      - well-formed
      - has been type-checked
      - contains no s-var, s-fun, s-data, s-check, or s-check-test
      - contains no s-provide in headers
      - all where blocks are none
      - contains no s-name (e.g. call resolve-names first)
      - contains no s-for, s-if, s-op, s-method-field,
                    s-not, s-when, s-if-pipe, s-paren
      - contains no s-underscore in expression position (but it may
        appear in binding positions as in s-let-bind, s-letrec-bind)
    Postconditions on program:
      - in addition to preconditions,
        contains no s-cases, s-cases-else, s-instantiate
  */
  void compileEnv;
  return new A.SProgram(program.l, program._use, program._provide, program.providedTypes,
    program.provides, program.imports, program.block.visit(desugarVisitor));
}
