/*
  Ported from: src/arr/compiler/well-formed.arr
*/

import * as A from './ast';
import * as SL from './srcloc';
import * as ED from './error-display';
import * as C from './compile-structs';
import { DefaultIterVisitor } from './ast-visitors';
import { InternalCompilerError, raise, field, asVariant } from './shared';
import { jsnums, throwingErrbacks } from './interop/js-numbers';

type Loc = SL.Loc;

// TODO: Make this a mutable field when we have them...
let errors: C.CompileError[] = [];
let inCheckBlock = false;
let curShared: A.Bind[] = [];
let paramCurrentWhereEverywhere = false; // TODO: What does this mean? (used by ensureEmptyBlock)

const reservedNames: Map<string, boolean> = new Map([
  ["function", true],
  ["break", true],
  ["return", true],
  ["do", true],
  ["yield", true],
  ["throw", true],
  ["continue", true],
  ["while", true],
  ["class", true],
  ["interface", true],
  ["type", true],
  ["generator", true],
  ["alias", true],
  ["extends", true],
  ["implements", true],
  ["module", true],
  ["package", true],
  ["namespace", true],
//  ["use", true],
  ["public", true],
  ["private", true],
  ["protected", true],
  ["static", true],
  ["const", true],
  ["enum", true],
  ["super", true],
  ["export", true],
  ["new", true],
  ["try", true],
  ["finally", true],
  ["debug", true],
  ["spy", true],
  ["switch", true],
  ["this", true],
  ["match", true],
  ["case", true],
  ["with", true],
  ["__proto__", true]
  // TODO: refactor AST so this can be added
  // ["table", true]
]);


function addError(err: C.CompileError): void {
  errors.push(err);
}
function wfError(msg: ED.ErrorDisplay[], loc: Loc): void {
  addError(new C.WfErr(msg, loc));
}
function wfError2(msg: string, loc1: Loc, loc2: Loc): void {
  addError(new C.WfErrSplit(msg, [loc1, loc2]));
}
function duplicateId(id: string, loc1: Loc, loc2: Loc): void {
  addError(new C.DuplicateId(id, loc1, loc2));
}
function reservedName(loc: Loc, id: string): void {
  addError(new C.ReservedName(loc, id));
}

function wrapVisitCheck(self: any, target: A.Expr | undefined): boolean {
  const curInCheck = inCheckBlock;
  inCheckBlock = true;
  const ret = self.option(target);
  inCheckBlock = curInCheck;
  return ret;
}

function ensureEmptyBlock(loc: Loc, typ: string, block: A.SBlock): void {
  if (!paramCurrentWhereEverywhere) {
    if (block.stmts.length === 0) { /* nothing */ }
    else {
      addError(new C.UnwelcomeWhere(typ, loc, block.l));
    }
  }
}

function isBlockAllowed(expr: A.Expr): boolean {
  return A.isBinder(expr) || A.isSSpyBlock(expr);
}

function explicitlyBlockyBlock(block: A.SBlock): boolean {
  let seenNonLet = false;
  let isBlocky = false;
  let seenTemplate = false;
  for (const expr of block.stmts) {
    if (A.isSTemplate(expr)) { seenTemplate = true; }
    else if (seenNonLet) { isBlocky = true; } // even if expr is a binder, it's non-consecutive
    else if (!isBlockAllowed(expr)) { seenNonLet = true; }
  }
  // any template presence overrules blockiness presence
  return isBlocky && !seenTemplate;
}

function wfBlockyBlocks(l: Loc, blocks: A.SBlock[]): void {
  const explicitlyBlockyBlocks = blocks.filter(explicitlyBlockyBlock);
  if (explicitlyBlockyBlocks.length !== 0) {
    addError(new C.BlockNeeded(l, explicitlyBlockyBlocks));
  }
}

function ensureUniqueCases(_cases: A.CasesBranch[]): void {
  for (let i = 0; i < _cases.length; i++) {
    const f = _cases[i];
    // Both s-cases-branch and s-singleton-cases-branch are handled identically
    // in the Pyret original (both have name and pat-loc).
    const rest = _cases.slice(i + 1);
    const found = rest.find((b) => b.name === f.name);
    if (found !== undefined) {
      addError(new C.DuplicateBranch(f.name, found.patLoc, f.patLoc));
    }
  }
}

// This function checks that a set of bindings are distinct, regardless of shadowing
// (e.g. the parameters of a function should never have the same name)
function ensureUniqueIds(bindings: A.Bind[]): void {
  const ad = new Map<string, Loc>();
  function help(bind: A.Bind): void {
    switch (bind.$name) {
      case 's-bind': {
        const id = bind.id;
        switch (id.$name) {
          case 's-underscore': break;
          default:
            if (ad.has(id.toname())) {
              addError(new C.DuplicateId(id.tosourcestring(), bind.l, ad.get(id.toname())!));
            } else {
              ad.set(id.toname(), bind.l);
            }
        }
        break;
      }
      case 's-tuple-bind': {
        if (bind.asName !== undefined) { help(bind.asName); }
        bind.fields.forEach(help);
        break;
      }
      default: throw new InternalCompilerError('ensureUniqueIds: unknown bind ' + (bind as any).$name);
    }
  }
  bindings.forEach(help);
}

// This function checks that the bindings within a block are either distinct
// or explicitly shadowed -- even before scope-resolution has a chance to kick in
function ensureUniqueBindings(bindings: A.Bind[]): void {
  const ad = new Map<string, Loc>();
  function help(bind: A.Bind): void {
    switch (bind.$name) {
      case 's-bind': {
        const id = bind.id;
        switch (id.$name) {
          case 's-underscore': break;
          default:
            if (bind.shadows) { /* nothing */ }
            else if (ad.has(id.toname())) {
              addError(new C.DuplicateId(id.tosourcestring(), bind.l, ad.get(id.toname())!));
            } else {
              ad.set(id.toname(), bind.l);
            }
        }
        break;
      }
      case 's-tuple-bind': {
        if (bind.asName !== undefined) { help(bind.asName); }
        bind.fields.forEach(help);
        break;
      }
      default: throw new InternalCompilerError('ensureUniqueBindings: unknown bind ' + (bind as any).$name);
    }
  }
  bindings.forEach(help);
}


function ensureUniqueFields(revFields: Array<{ name: string; l: Loc }>): void {
  for (let i = 0; i < revFields.length; i++) {
    const f = revFields[i];
    const found = revFields.slice(i + 1).find((f2) => f2.name === f.name);
    if (found !== undefined) {
      addError(new C.DuplicateField(f.name, f.l, found.l));
    }
  }
}

function checkUnderscoreName(fields: Array<{ name: string; l: Loc }>, kindOfThing: string): boolean {
  const underscores = fields.filter((f) => f.name === '_');
  if (underscores.length !== 0) {
    addError(new C.UnderscoreAs(underscores[0].l, kindOfThing));
  }
  return underscores.length === 0;
}

function ensureDistinctLines(loc: Loc, prevIsTemplate: boolean, stmts: A.Expr[]): void {
  for (const first of stmts) {
    switch (loc.$name) {
      case 'builtin':
        loc = first.l;
        prevIsTemplate = A.isSTemplate(first);
        break;
      case 'srcloc': {
        const endLine1 = loc.endLine;
        switch (first.l.$name) {
          case 'builtin':
            // No need to preserve builtin() locs
            break;
          case 'srcloc': {
            const startLine2 = first.l.startLine;
            if (endLine1 === startLine2) {
              if (A.isSTemplate(first) && prevIsTemplate) {
                addError(new C.TemplateSameLine(loc, first.l));
              } else if (!A.isSTemplate(first) && !prevIsTemplate) {
                addError(new C.SameLine(loc, first.l, A.isSParen(first)));
              }
            }
            loc = first.l;
            prevIsTemplate = A.isSTemplate(first);
            break;
          }
        }
        break;
      }
    }
  }
}

function ensureUniqueVariantIds(variants: A.Variant[], name: string, dataLoc: Loc): void {
  for (let i = 0; i < variants.length; i++) {
    const f = variants[i];
    if (f.name === name) {
      addError(new C.DataVariantDuplicateName(f.name, f.l, dataLoc));
    } else if (f.name === ('is-' + name)) {
      addError(new C.DuplicateIsData(name, f.l, dataLoc));
    } else if (('is-' + f.name) === name) {
      addError(new C.DuplicateIsDataVariant(f.name, dataLoc, f.l));
    }
    for (const b of variants.slice(i + 1)) {
      if (b.name === f.name) {
        addError(new C.DuplicateVariant(f.name, b.l, f.l));
      } else if (b.name === ('is-' + f.name)) {
        addError(new C.DuplicateIsVariant(f.name, b.l, f.l));
      } else if (('is-' + b.name) === f.name) {
        addError(new C.DuplicateIsVariant(b.name, f.l, b.l));
      }
    }
  }
}


function wfLastStmt(blockLoc: Loc, stmt: A.Expr): void {
  switch (stmt.$name) {
    case 's-let': addError(new C.BlockEnding(stmt.l, blockLoc, 'let-binding')); break;
    case 's-var': addError(new C.BlockEnding(stmt.l, blockLoc, 'var-binding')); break;
    case 's-rec': addError(new C.BlockEnding(stmt.l, blockLoc, 'rec-binding')); break;
    case 's-fun': addError(new C.BlockEnding(stmt.l, blockLoc, 'fun-binding')); break;
    case 's-data': addError(new C.BlockEnding(stmt.l, blockLoc, 'data definition')); break;
    case 's-contract': addError(new C.BlockEnding(stmt.l, blockLoc, 'contract')); break;
    case 's-spy-block': addError(new C.BlockEnding(stmt.l, blockLoc, 'spy block')); break;
    default: break;
  }
}

function fieldsToBinds(members: Array<{ l: Loc; name: string }>): A.Bind[] {
  return members.map((mem) =>
    new A.SBind(mem.l, false, new A.SName(mem.l, mem.name), A.aBlank));
}

function opname(op: string): string { return op.substring(2); }

function reachableOps(self: any, l: Loc, opL: Loc, op: string, ast: A.Expr): boolean {
  if (A.isSOp(ast)) {
    const opL2 = ast.opL;
    const op2 = ast.op;
    if (op === op2) {
      reachableOps(self, l, opL, op, ast.left);
      reachableOps(self, l, opL, op, ast.right);
    } else {
      if (opL.before(opL2)) {
        addError(new C.MixedBinops(l, opname(op), opL, opname(op2), opL2));
      } else {
        addError(new C.MixedBinops(l, opname(op2), opL2, opname(op), opL));
      }
    }
    return true;
  } else {
    return ast.visit(self);
  }
}

function rejectStandaloneExprs(stmts: A.Expr[], ignoreLast: boolean): boolean {
  const toExamine = ignoreLast
    // Ignore the last statement, because it might well be an expression
    ? stmts.slice(0, stmts.length - 1)
    : stmts;
  function badStmt(l: Loc, stmt: A.Expr): void {
    switch (stmt.$name) {
      case 's-op': {
        if (stmt.op === 'op==') {
          wfError([
            ED.para(ED.text('A standalone '),
              ED.highlight(ED.code(ED.text('==')), [stmt.opL], 1),
              ED.text(" operator expression probably isn't intentional.")),
            inCheckBlock
              ? ED.para(
                  ED.text('To write an example or test case, use the '), ED.code(ED.text('is')), ED.text(' operator; '),
                  ED.text('to define a name, use the '), ED.code(ED.text('=')), ED.text(' operator instead.'))
              : ED.para(
                  ED.text('To define a name, use the '), ED.code(ED.text('=')), ED.text(' operator instead.'))
          ], l);
        } else {
          wfError([
            ED.para(ED.text('A standalone '),
              ED.highlight(ED.code(ED.text(stmt.op.substring(2))), [stmt.opL], 1),
              ED.text(" operator expression probably isn't intentional."))], l);
        }
        break;
      }
      case 's-id': wfError([ED.para(ED.text("A standalone variable name probably isn't intentional."))], l); break;
      case 's-num': wfError([ED.para(ED.text("A standalone value probably isn't intentional."))], l); break;
      case 's-frac': wfError([ED.para(ED.text("A standalone value probably isn't intentional."))], l); break;
      case 's-rfrac': wfError([ED.para(ED.text("A standalone value probably isn't intentional."))], l); break;
      case 's-bool': wfError([ED.para(ED.text("A standalone value probably isn't intentional."))], l); break;
      case 's-str': wfError([ED.para(ED.text("A standalone value probably isn't intentional."))], l); break;
      case 's-dot': wfError([ED.para(ED.text("A standalone field-lookup expression probably isn't intentional."))], l); break;
      case 's-lam': wfError([ED.para(ED.text("A standalone anonymous function expression probably isn't intentional."))], l); break;
      case 's-paren': badStmt(l, stmt.expr); break;
      default: break;
    }
  }
  if (!stmts.some(A.isSTemplate)) { // Need to check all the statements for ...
    for (const stmt of toExamine) { // but only check the non-final statements for standalone expressions
      badStmt(stmt.l, stmt);
    }
  }
  return true;
}

function wrapRejectStandalonesInCheck(target: A.Expr | undefined): boolean {
  const curInCheck = inCheckBlock;
  inCheckBlock = true;
  let ret: boolean;
  if (target === undefined) {
    ret = true;
  } else {
    const stmts = field(target, 'stmts');
    if (stmts.length > 0) {
      ret = rejectStandaloneExprs(stmts, false);
    } else {
      ret = true;
    }
  }
  inCheckBlock = curInCheck;
  return ret;
}


function wfBlockStmts(visitor: any, l: Loc, stmts: A.Expr[], toplevel: boolean): boolean {
  const bindStmts = stmts
    .filter((s) => A.isSVar(s) || A.isSLet(s) || A.isSRec(s))
    .map((s) => field(s, 'name'));
  ensureUniqueBindings(bindStmts);
  ensureDistinctLines(A.dummyLoc, false, stmts);
  if (!inCheckBlock && !toplevel) {
    rejectStandaloneExprs(stmts, true);
  }
  return stmts.every((s) => s.visit(visitor));
}

function wfExamplesBody(visitor: any, body: A.SBlock): boolean {
  return body.stmts.every((b) => {
    if (!(A.isSCheckTest(b) || A.isSTemplate(b))) {
      addError(new C.NonExample(b));
      return true;
    } else {
      return true;
    }
  });
}

function wfTableHeaders(loc: Loc, headers: A.FieldName[]): boolean {
  for (const h of headers) {
    if (h.name === '_') {
      addError(new C.UnderscoreAs(h.l, "as a table column's name in a table expression"));
    }
  }
  if (headers.length === 0) {
    addError(new C.TableEmptyHeader(loc));
    return true;
  } else {
    for (let i = 0; i < headers.length; i++) {
      const first = headers[i];
      if (reservedNames.has(first.name)) {
        reservedName(first.l, first.name);
      }
      for (const hname of headers.slice(i + 1)) {
        if (first.name === hname.name) {
          addError(new C.TableDuplicateColumnName(first, hname));
        }
      }
    }
    return true;
  }
}


function isUnderscore(e: A.Expr): boolean {
  return A.isSId(e) && A.isSUnderscore(e.id);
}

let parentBlockLoc: Loc | undefined = undefined;

class WellFormedVisitor extends DefaultIterVisitor {
  public override option<T extends { visit(v: any): any }>(x: T | undefined): boolean {
    return super.option(x);
  }
  sProgram(node: A.SProgram): boolean {
    return raise('Impossible');
  }
  sUse(node: A.SUse): boolean {
    if (!(node.n.toname() === 'context')) {
      wfError([ED.text('The only supported type of '), ED.code(ED.text('use')), ED.text(' is '), ED.code(ED.text('context')), ED.text(', but this program used '), ED.code(ED.text(node.n.toname()))], field(node.n, 'l'));
      return false;
    } else {
      return true;
    }
  }
  sSpecialImport(node: A.SSpecialImport): boolean {
    const { l, kind, args } = node;
    if (kind === 'my-gdrive') {
      if (!(args.length === 1)) {
        addError(new C.ImportArityMismatch(l, kind, args, 2, ['the name of the file']));
        return false;
      } else {
        return true;
      }
    } else if (kind === 'shared-gdrive') {
      if (!(args.length === 2)) {
        addError(new C.ImportArityMismatch(l, kind, args, 2, ['the name of the file', "the file's id, which you can get from the share URL"]));
        return false;
      } else {
        return true;
      }
    } else if (kind === 'js-http') {
      return true;
    } else if (kind === 'gdrive-js') {
      if (!(args.length === 2)) {
        // The Pyret original returns `nothing` here (the result of add-error)
        addError(new C.ImportArityMismatch(l, kind, args, 2, ['the name of the file', "the file's id"]));
        return false;
      } else {
        return true;
      }
    } else {
      return true;
      // wf-error("Unsupported import type " + kind + ".  Did you mean my-gdrive or shared-gdrive?", l)
    }
  }
  sData(node: A.SData): boolean {
    addError(new C.NonToplevel('data declaration', node.l, parentBlockLoc!));
    return true;
  }
  sDataExpr(node: A.SDataExpr): boolean {
    addError(new C.NonToplevel('data declaration', node.l, parentBlockLoc!));
    return true;
  }
  sType(node: A.SType): boolean {
    addError(new C.NonToplevel('type alias', node.l, parentBlockLoc!));
    return true;
  }
  sNewtype(node: A.SNewtype): boolean {
    addError(new C.NonToplevel('newtype', node.l, parentBlockLoc!));
    return true;
  }
  sLetExpr(node: A.SLetExpr): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    const ans = node.binds.every((b) => b.visit(this)) && node.body.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sContract(node: A.SContract): boolean {
    addError(new C.NonToplevel('contract declaration', node.l, parentBlockLoc!));
    return true;
  }
  sLetrecBind(node: A.SLetrecBind): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    switch (node.b.$name) {
      case 's-bind': break;
      case 's-tuple-bind':
        wfError([ED.text('Recursive bindings must be names and cannot be tuple bindings ')], node.b.l);
        break;
      default: throw new InternalCompilerError('sLetrecBind: unknown bind ' + (node.b as any).$name);
    }
    const ans = node.b.visit(this) && node.value.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sLetrec(node: A.SLetrec): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    const ans = node.binds.every((b) => b.visit(this)) && node.body.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sTypeLetExpr(node: A.STypeLetExpr): boolean {
    addError(new C.NonToplevel('type alias', node.l, parentBlockLoc!));
    return true;
  }
  sOp(node: A.SOp): boolean {
    return reachableOps(this, node.l, node.opL, node.op, node.left)
      && reachableOps(this, node.l, node.opL, node.op, node.right);
  }
  sCasesBranch(node: A.SCasesBranch): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (node.name === '_') {
      addError(new C.UnderscoreAsPattern(node.patLoc));
    }
    if (reservedNames.has(node.name)) {
      reservedName(node.patLoc, node.name);
    }
    ensureUniqueIds(node.args.map((a) => a.bind));
    const ans = node.args.every((a) => a.visit(this)) && node.body.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sSingletonCasesBranch(node: A.SSingletonCasesBranch): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (node.name === '_') {
      addError(new C.UnderscoreAsPattern(node.patLoc));
    }
    if (reservedNames.has(node.name)) {
      reservedName(node.patLoc, node.name);
    }
    const ans = node.body.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sVar(node: A.SVar): boolean {
    const bind = node.name;
    switch (bind.$name) {
      case 's-bind': {
        if (A.isSUnderscore(bind.id)) {
          addError(new C.PointlessVar((node.l as SL.Srcloc).atStart().plus(bind.l as SL.Srcloc)));
        }
        return bind.visit(this) && node.value.visit(this);
      }
      case 's-tuple-bind': {
        wfError([ED.text('Variable bindings must be names and cannot be tuple bindings ')], bind.l);
        return true;
      }
      default: throw new InternalCompilerError('sVar: unknown bind ' + (bind as any).$name);
    }
  }
  sRec(node: A.SRec): boolean {
    const bind = node.name;
    switch (bind.$name) {
      case 's-bind': {
        if (A.isSUnderscore(bind.id)) {
          addError(new C.PointlessRec((node.l as SL.Srcloc).atStart().plus(bind.l as SL.Srcloc)));
        }
        return bind.visit(this) && node.value.visit(this);
      }
      case 's-tuple-bind': {
        wfError([ED.text('Recursive bindings must be names and cannot be tuple bindings ')], bind.l);
        return true;
      }
      default: throw new InternalCompilerError('sRec: unknown bind ' + (bind as any).$name);
    }
  }
  sVarBind(node: A.SVarBind): boolean {
    const bind = node.b;
    switch (bind.$name) {
      case 's-bind': {
        if (A.isSUnderscore(bind.id)) {
          addError(new C.PointlessVar((node.l as SL.Srcloc).atStart().plus(bind.l as SL.Srcloc)));
        }
        return bind.visit(this) && node.value.visit(this);
      }
      case 's-tuple-bind': {
        wfError([ED.text('Variable bindings must be names and cannot be tuple bindings ')], bind.l);
        return true;
      }
      default: throw new InternalCompilerError('sVarBind: unknown bind ' + (bind as any).$name);
    }
  }
  sBlock(node: A.SBlock): boolean {
    if (node.stmts.length === 0) {
      addError(new C.WfEmptyBlock(parentBlockLoc!));
      return true;
    } else {
      wfLastStmt(parentBlockLoc!, node.stmts[node.stmts.length - 1]);
      wfBlockStmts(this, parentBlockLoc!, node.stmts, false);
      return true;
    }
  }
  sUserBlock(node: A.SUserBlock): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    const ans = node.body.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sTupleBind(node: A.STupleBind): boolean {
    return true;
  }
  sBind(node: A.SBind): boolean {
    if (reservedNames.has(node.id.tosourcestring())) {
      reservedName(node.l, node.id.tosourcestring());
    }
    if (node.shadows && A.isSUnderscore(node.id)) {
      addError(new C.PointlessShadow(node.l));
    }
    return node.id.visit(this) && node.ann.visit(this);
  }
  sCheckTest(node: A.SCheckTest): boolean {
    if (!inCheckBlock) {
      addError(new C.UnwelcomeTest(node.l));
    }
    if (node.refinement !== undefined) {
      switch (node.op.$name) {
        case 's-op-is': break;
        case 's-op-is-not': break;
        default:
          addError(new C.UnwelcomeTestRefinement(node.refinement, node.op));
      }
    }
    return node.left.visit(this) && this.option(node.right) && this.option(node.cause);
  }
  sMethodField(node: A.SMethodField): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node._checkLoc === undefined
      ? node.l
      : (node.l as SL.Srcloc).uptoEnd(node._checkLoc as SL.Srcloc);
    if (reservedNames.has(node.name)) {
      reservedName(node.l, node.name);
    }
    if (node.args.length === 0) {
      addError(new C.NoArguments(new A.SMethodField(node.l, node.name, node.params, node.args, node.ann, node.doc, node.body, node._checkLoc, node._check, node.blocky)));
    }
    ensureUniqueIds(node.args);
    if (node._check !== undefined) {
      ensureEmptyBlock(node.l, 'methods', asVariant(node._check, A.SBlock));
    }
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    let ans = node.args.every((a) => a.visit(this)) && node.ann.visit(this) && node.body.visit(this);
    if (node._checkLoc !== undefined) {
      parentBlockLoc = (node._checkLoc as SL.Srcloc).uptoEnd(node.l as SL.Srcloc);
    }
    wrapRejectStandalonesInCheck(node._check);
    ans = ans && wrapVisitCheck(this, node._check);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sDataField(node: A.SDataField): boolean {
    if (reservedNames.has(node.name)) {
      reservedName(node.l, node.name);
    }
    return node.value.visit(this);
  }
  sMutableField(node: A.SMutableField): boolean {
    if (reservedNames.has(node.name)) {
      reservedName(node.l, node.name);
    }
    return node.ann.visit(this) && node.value.visit(this);
  }
  sMethod(node: A.SMethod): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node._checkLoc === undefined
      ? node.l
      : (node.l as SL.Srcloc).uptoEnd(node._checkLoc as SL.Srcloc);
    if (node.args.length === 0) {
      addError(new C.NoArguments(new A.SMethod(node.l, node.name, node.params, node.args, node.ann, node.doc, node.body, node._checkLoc, node._check, node.blocky)));
    }
    ensureUniqueIds(node.args);
    if (node._check !== undefined) {
      ensureEmptyBlock(node.l, 'methods', asVariant(node._check, A.SBlock));
    }
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    let ans = node.args.every((a) => a.visit(this)) && node.ann.visit(this) && node.body.visit(this);
    if (node._checkLoc !== undefined) {
      parentBlockLoc = (node._checkLoc as SL.Srcloc).uptoEnd(node.l as SL.Srcloc);
    }
    wrapRejectStandalonesInCheck(node._check);
    ans = ans && wrapVisitCheck(this, node._check);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sLam(node: A.SLam): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node._checkLoc === undefined
      ? node.l
      : (node.l as SL.Srcloc).uptoEnd(node._checkLoc as SL.Srcloc);
    ensureUniqueIds(node.args);
    if (node._check !== undefined) {
      ensureEmptyBlock(node.l, 'anonymous functions', asVariant(node._check, A.SBlock));
    }
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    let ans = node.params.every((p) => p.visit(this))
      && node.args.every((a) => a.visit(this)) && node.ann.visit(this) && node.body.visit(this);
    if (node._checkLoc !== undefined) {
      parentBlockLoc = (node._checkLoc as SL.Srcloc).uptoEnd(node.l as SL.Srcloc);
    }
    wrapRejectStandalonesInCheck(node._check);
    ans = ans && wrapVisitCheck(this, node._check);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sFun(node: A.SFun): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node._checkLoc === undefined
      ? node.l
      : (node.l as SL.Srcloc).uptoEnd(node._checkLoc as SL.Srcloc);
    if (reservedNames.has(node.name)) {
      reservedName(node.l, node.name);
    }
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    ensureUniqueIds(node.args);
    let ans = node.params.every((p) => p.visit(this))
      && node.args.every((a) => a.visit(this)) && node.ann.visit(this) && node.body.visit(this);
    if (node._checkLoc !== undefined) {
      parentBlockLoc = (node._checkLoc as SL.Srcloc).uptoEnd(node.l as SL.Srcloc);
    }
    wrapRejectStandalonesInCheck(node._check);
    ans = ans && wrapVisitCheck(this, node._check);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sObj(node: A.SObj): boolean {
    ensureUniqueFields(node.fields.slice().reverse());
    checkUnderscoreName(node.fields, 'a field name');
    return node.fields.every((f) => f.visit(this));
  }
  sExtend(node: A.SExtend): boolean {
    ensureUniqueFields(node.fields.slice().reverse());
    checkUnderscoreName(node.fields, 'a field name');
    return node.fields.every((f) => f.visit(this));
  }
  sDot(node: A.SDot): boolean {
    if (node.field === '_') {
      addError(new C.UnderscoreAs(node.l, 'a field name'));
    }
    return node.obj.visit(this);
  }
  sTupleGet(node: A.STupleGet): boolean {
    if (!Number.isInteger(node.index) || (node.index < 0) || (node.index > 1000)) {
      addError(new C.TupleGetBadIndex(node.l, node.tup, node.index, node.indexLoc));
      return true;
    } else {
      return true;
    }
  }
  sCheck(node: A.SCheck): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    let ans: boolean;
    if (!node.keywordCheck) {
      wrapVisitCheck(this, node.body);
      ans = wfExamplesBody(this, asVariant(node.body, A.SBlock));
    } else {
      wrapVisitCheck(this, node.body);
      ans = wrapRejectStandalonesInCheck(node.body);
    }
    parentBlockLoc = oldPbl;
    return ans;
  }
  sWhen(node: A.SWhen): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.block, A.SBlock)]);
    }
    const ans = node.test.visit(this) && node.block.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sIf(node: A.SIf): boolean {
    if (node.branches.length === 1) {
      addError(new C.SingleBranchIf(new A.SIf(node.l, node.branches, node.blocky)));
    }
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, node.branches.map((b) => asVariant(b.body, A.SBlock)));
    }
    const ans = node.branches.every((b) => b.visit(this));
    parentBlockLoc = oldPbl;
    return ans;
  }
  sIfElse(node: A.SIfElse): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node._else, A.SBlock), ...node.branches.map((b) => asVariant(b.body, A.SBlock))]);
    }
    const ans = node.branches.every((b) => b.visit(this)) && node._else.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sIfPipe(node: A.SIfPipe): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, node.branches.map((b) => asVariant(b.body, A.SBlock)));
    }
    const ans = node.branches.every((b) => b.visit(this));
    parentBlockLoc = oldPbl;
    return ans;
  }
  sIfPipeElse(node: A.SIfPipeElse): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node._else, A.SBlock), ...node.branches.map((b) => asVariant(b.body, A.SBlock))]);
    }
    const ans = node.branches.every((b) => b.visit(this)) && node._else.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sCases(node: A.SCases): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    ensureUniqueCases(node.branches);
    if (!node.blocky) {
      wfBlockyBlocks(node.l, node.branches.map((b) => asVariant(b.body, A.SBlock)));
    }
    const ans = node.typ.visit(this) && node.val.visit(this) && node.branches.every((b) => b.visit(this));
    parentBlockLoc = oldPbl;
    return ans;
  }
  sCasesElse(node: A.SCasesElse): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    ensureUniqueCases(node.branches);
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node._else, A.SBlock), ...node.branches.map((b) => asVariant(b.body, A.SBlock))]);
    }
    const ans = node.typ.visit(this) && node.val.visit(this)
      && node.branches.every((b) => b.visit(this)) && node._else.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sFor(node: A.SFor): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node.l;
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    const ans = node.iterator.visit(this) && node.bindings.every((b) => b.visit(this))
      && node.ann.visit(this) && node.body.visit(this);
    parentBlockLoc = oldPbl;
    return ans;
  }
  sFrac(node: A.SFrac): boolean {
    if (jsnums.equals(node.den, 0, throwingErrbacks)) {
      addError(new C.ZeroFraction(node.l, node.num));
    }
    return true;
  }
  sRfrac(node: A.SRfrac): boolean {
    if (jsnums.equals(node.den, 0, throwingErrbacks)) {
      addError(new C.ZeroFraction(node.l, node.num));
    }
    return true;
  }
  sId(node: A.SId): boolean {
    if (reservedNames.has(node.id.tosourcestring())) {
      reservedName(node.l, node.id.tosourcestring());
    }
    return true;
  }
  sProvide(node: A.SProvide): boolean {
    if (!A.isSObj(node.block)) {
      addError(new C.NonObjectProvide(node.l));
    }
    return true;
  }
  sReactor(node: A.SReactor): boolean {
    const methodFields = node.fields.filter((f) => A.isSMethodField(f));
    const hasField = (name: string): boolean =>
      node.fields.find((f) => f.name === name) !== undefined;
    if (methodFields.length !== 0) {
      wfError([ED.text('A reactor cannot contain method fields ')], methodFields[0].l);
      return true;
    } else {
      if (!hasField('init')) {
        wfError([ED.text('A reactor must have a field named '), ED.code(ED.text('init')),
          ED.text(' for the initial value ')], node.l);
      }
      const fieldsDict = new Map<string, Loc>();
      const okFields = C.reactorFields;
      if (hasField('on-key') && hasField('on-raw-key')) {
        wfError([ED.text('A reactor can only specify one of on-key and on-raw-key')], node.l);
      }

      for (const f of node.fields) {
        if (!okFields.has(f.name)) {
          wfError([ED.text('Valid options for reactors are '),
            // Canonicalize with sort() so the message is deterministic and
            // byte-identical across compilers -- okFields is a Map (insertion
            // order) here and a StringDict (hash order) in the .arr original;
            // well-formed.arr sorts the same keys-list for this message.
            ED.hSequenceSep([...okFields.keys()].sort().map((ok) => ED.code(ED.text(ok))), ', ', ', or '),
            ED.text(', but found one named '),
            ED.code(ED.text(f.name)), ED.text(' ')], f.l);
        }
        const existing = fieldsDict.get(f.name);
        if (existing === undefined) {
          fieldsDict.set(f.name, f.l);
        } else {
          wfError2('Duplicate option in reactor: ' + f.name, f.l, existing);
        }
        f.visit(this);
      }
      return true;
    }
  }
  sTable(node: A.STable): boolean {
    wfTableHeaders(node.l, node.headers);
    if (node.headers.length === 0) {
      return true;
    } else {
      const expectedLen = node.headers.length;
      return node.rows.every((row) => {
        const actualLen = row.elems.length;
        if (actualLen === 0) {
          addError(new C.TableEmptyRow(row.l));
        }
        if ((actualLen !== 0) && (actualLen !== expectedLen)) {
          const headerLoc = (node.headers[0].l as SL.Srcloc)
            .plus(node.headers[node.headers.length - 1].l as SL.Srcloc);
          const rowLoc = (row.elems[0].l as SL.Srcloc)
            .plus(row.elems[row.elems.length - 1].l as SL.Srcloc);
          void rowLoc; // computed (but unused) in the Pyret original too
          addError(new C.TableRowWrongSize(headerLoc, node.headers, row));
        }
        return row.elems.every((elem) => elem.visit(this));
      });
    }
  }
  sTableExtend(node: A.STableExtend): boolean {
    const boundNames = new Set<string>(node.columnBinds.binds.map((b) => field(b, 'id').toname()));
    return node.extensions.every((extension) => {
      switch (extension.$name) {
        case 's-table-extend-field':
          return extension.value.visit(this) && extension.ann.visit(this);
        case 's-table-extend-reducer': {
          if (!boundNames.has(extension.col.toname())) {
            addError(new C.TableReducerBadColumn(extension, node.columnBinds.l));
          }
          return extension.reducer.visit(this) && extension.ann.visit(this);
        }
        default: throw new InternalCompilerError('sTableExtend: unknown extension ' + (extension as any).$name);
      }
    });
  }
  sLoadTable(node: A.SLoadTable): boolean {
    const thisExpr = new A.SLoadTable(node.l, node.headers, node.spec);
    wfTableHeaders(node.l, node.headers);
    if (node.spec.length === 0) {
      addError(new C.LoadTableNoBody(thisExpr));
      return false;
    } else {
      const boundNames = new Set<string>(node.headers.map((b) => b.name));
      let dupFound = false;
      const headerLoc = node.headers.length === 0
        ? (node.l as SL.Srcloc).upto(node.spec[0].l as SL.Srcloc)
        : (node.headers[0].l as SL.Srcloc).plus(node.headers[node.headers.length - 1].l as SL.Srcloc);
      let numSrcs = 0;
      const sanitizers = new Map<string, A.LoadTableSpec>();
      for (const s of node.spec) {
        switch (s.$name) {
          case 's-sanitize': {
            const namestr = s.name.toname();
            if (!boundNames.has(namestr)) {
              addError(new C.TableSanitizerBadColumn(s, headerLoc));
            }
            const existing = sanitizers.get(namestr);
            if (existing !== undefined) {
              addError(new C.LoadTableDuplicateSanitizer(existing, namestr, s));
              dupFound = true;
            } else {
              sanitizers.set(namestr, s);
            }
            break;
          }
          case 's-table-src':
            numSrcs = numSrcs + 1;
            break;
          default: throw new InternalCompilerError('sLoadTable: unknown spec ' + (s as any).$name);
        }
      }
      if (numSrcs !== 1) {
        addError(new C.LoadTableBadNumberSrcs(thisExpr, numSrcs));
      }
      return (numSrcs === 1) && !dupFound && node.spec.every((s) => s.visit(this));
    }
  }
  aName(node: A.AName): boolean {
    if (A.isSUnderscore(node.id)) {
      addError(new C.UnderscoreAsAnn(node.l));
    }
    return true;
  }
  aRecord(node: A.ARecord): boolean {
    ensureUniqueFields(node.fields.slice().reverse());
    return true;
  }
}

const wellFormedVisitor = new WellFormedVisitor();

class TopLevelVisitor extends DefaultIterVisitor {
  public override option<T extends { visit(v: any): any }>(x: T | undefined): boolean {
    return x === undefined ? true : x.visit(this);
  }
  sProgram(node: A.SProgram): boolean {
    const body = node.block;
    const okBody = A.isSBlock(body)
      ? wfBlockStmts(this, body.l, body.stmts, true)
      : body.visit(this);
    return okBody && this.option(node._use) && node._provide.visit(this)
      && node.providedTypes.visit(this) && node.imports.every((i) => i.visit(this));
  }
  sType(node: A.SType): boolean {
    return node.ann.visit(wellFormedVisitor);
  }
  sNewtype(node: A.SNewtype): boolean {
    return true;
  }
  sTypeLetExpr(node: A.STypeLetExpr): boolean {
    if (!node.blocky) {
      wfBlockyBlocks(node.l, [asVariant(node.body, A.SBlock)]);
    }
    return node.binds.every((b) => b.visit(this)) && node.body.visit(wellFormedVisitor);
  }
  sTypeBind(node: A.STypeBind): boolean {
    return node.ann.visit(wellFormedVisitor);
  }
  sNewtypeBind(node: A.SNewtypeBind): boolean {
    return true;
  }
  sVariant(node: A.SVariant): boolean {
    if (reservedNames.has(node.name)) {
      reservedName(node.constrLoc, node.name);
    }
    for (const oneBind of node.members.map((b) => b.bind)) {
      switch (oneBind.$name) {
        case 's-bind': break;
        case 's-tuple-bind':
          wfError([ED.text('Tuple binding not allowed as variant member ')], oneBind.l);
          break;
        default: throw new InternalCompilerError('sVariant: unknown bind ' + (oneBind as any).$name);
      }
    }
    const ids = [...fieldsToBinds(node.withMembers), ...node.members.map((b) => b.bind)];
    ensureUniqueIds(ids);
    const underscores = node.members.filter((b) => A.isSBind(b.bind) && A.isSUnderscore(b.bind.id));
    if (underscores.length !== 0) {
      addError(new C.UnderscoreAs(underscores[0].l, 'a data variant name'));
    }
    checkUnderscoreName(node.withMembers, 'a field name');
    node.members.forEach((b) => b.visit(wellFormedVisitor));
    node.withMembers.forEach((m) => m.visit(wellFormedVisitor));
    return true;
  }
  sSingletonVariant(node: A.SSingletonVariant): boolean {
    if (reservedNames.has(node.name)) {
      reservedName(node.l, node.name);
    }
    ensureUniqueIds(fieldsToBinds(node.withMembers));
    node.withMembers.forEach((m) => m.visit(wellFormedVisitor));
    return true;
  }
  sData(node: A.SData): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node._checkLoc === undefined
      ? node.l
      : (node.l as SL.Srcloc).uptoEnd(node._checkLoc as SL.Srcloc);
    ensureUniqueVariantIds(node.variants, node.name, node.l);
    checkUnderscoreName(node.variants, 'a data variant name');
    checkUnderscoreName(node.sharedMembers, 'a shared field name');
    checkUnderscoreName([{ l: node.l, name: node.name }], 'a datatype name');
    const theCurShared = curShared;
    curShared = fieldsToBinds(node.sharedMembers);
    node.params.forEach((p) => p.visit(wellFormedVisitor));
    node.mixins.forEach((m) => m.visit(wellFormedVisitor));
    node.variants.forEach((v) => v.visit(this));
    node.sharedMembers.forEach((s) => s.visit(wellFormedVisitor));
    curShared = theCurShared;
    if (node._checkLoc !== undefined) {
      parentBlockLoc = (node._checkLoc as SL.Srcloc).uptoEnd(node.l as SL.Srcloc);
    }
    wrapRejectStandalonesInCheck(node._check);
    wrapVisitCheck(wellFormedVisitor, node._check);
    parentBlockLoc = oldPbl;
    return true;
  }
  sDataExpr(node: A.SDataExpr): boolean {
    const oldPbl = parentBlockLoc;
    parentBlockLoc = node._checkLoc === undefined
      ? node.l
      : (node.l as SL.Srcloc).uptoEnd(node._checkLoc as SL.Srcloc);
    ensureUniqueVariantIds(node.variants, node.name, node.l);
    checkUnderscoreName(node.variants, 'a data variant name');
    checkUnderscoreName(node.sharedMembers, 'a shared field name');
    checkUnderscoreName([{ l: node.l, name: node.name }], 'a datatype name');
    const theCurShared = curShared;
    curShared = fieldsToBinds(node.sharedMembers);
    node.params.forEach((p) => p.visit(wellFormedVisitor));
    node.mixins.forEach((m) => m.visit(wellFormedVisitor));
    node.variants.forEach((v) => v.visit(wellFormedVisitor));
    node.sharedMembers.forEach((s) => s.visit(wellFormedVisitor));
    curShared = theCurShared;
    if (node._checkLoc !== undefined) {
      parentBlockLoc = (node._checkLoc as SL.Srcloc).uptoEnd(node.l as SL.Srcloc);
    }
    wrapRejectStandalonesInCheck(node._check);
    wrapVisitCheck(wellFormedVisitor, node._check);
    parentBlockLoc = oldPbl;
    return true;
  }

  // Everything else delegates to the non-toplevel visitor
  sUse(node: A.SUse): boolean {
    return wellFormedVisitor.sUse(node);
  }
  sImport(node: A.SImport): boolean {
    return wellFormedVisitor.sImport(node);
  }
  sInclude(node: A.SInclude): boolean {
    return wellFormedVisitor.sInclude(node);
  }
  sImportTypes(node: A.SImportTypes): boolean {
    return wellFormedVisitor.sImportTypes(node);
  }
  sImportFields(node: A.SImportFields): boolean {
    return wellFormedVisitor.sImportFields(node);
  }
  sProvide(node: A.SProvide): boolean {
    return wellFormedVisitor.sProvide(node);
  }
  sProvideTypes(node: A.SProvideTypes): boolean {
    return wellFormedVisitor.sProvideTypes(node);
  }
  sBind(node: A.SBind): boolean {
    return wellFormedVisitor.sBind(node);
  }
  sVarBind(node: A.SVarBind): boolean {
    return wellFormedVisitor.sVarBind(node);
  }
  sLetBind(node: A.SLetBind): boolean {
    return wellFormedVisitor.sLetBind(node);
  }
  sTemplate(node: A.STemplate): boolean {
    return wellFormedVisitor.sTemplate(node);
  }
  sLetExpr(node: A.SLetExpr): boolean {
    return wellFormedVisitor.sLetExpr(node);
  }
  sLetrecBind(node: A.SLetrecBind): boolean {
    return wellFormedVisitor.sLetrecBind(node);
  }
  sLetrec(node: A.SLetrec): boolean {
    return wellFormedVisitor.sLetrec(node);
  }
  sHintExp(node: A.SHintExp): boolean {
    return wellFormedVisitor.sHintExp(node);
  }
  sInstantiate(node: A.SInstantiate): boolean {
    return wellFormedVisitor.sInstantiate(node);
  }
  sBlock(node: A.SBlock): boolean {
    return wellFormedVisitor.sBlock(node);
  }
  sUserBlock(node: A.SUserBlock): boolean {
    return wellFormedVisitor.sUserBlock(node);
  }
  sFun(node: A.SFun): boolean {
    return wellFormedVisitor.sFun(node);
  }
  sVar(node: A.SVar): boolean {
    return wellFormedVisitor.sVar(node);
  }
  sRec(node: A.SRec): boolean {
    return wellFormedVisitor.sRec(node);
  }
  sLet(node: A.SLet): boolean {
    return wellFormedVisitor.sLet(node);
  }
  sRef(node: A.SRef): boolean {
    return wellFormedVisitor.sRef(node);
  }
  sWhen(node: A.SWhen): boolean {
    return wellFormedVisitor.sWhen(node);
  }
  sContract(node: A.SContract): boolean {
    // TODO
    return true;
  }
  sAssign(node: A.SAssign): boolean {
    return wellFormedVisitor.sAssign(node);
  }
  sIfBranch(node: A.SIfBranch): boolean {
    return wellFormedVisitor.sIfBranch(node);
  }
  sIfPipeBranch(node: A.SIfPipeBranch): boolean {
    return wellFormedVisitor.sIfPipeBranch(node);
  }
  sIf(node: A.SIf): boolean {
    return wellFormedVisitor.sIf(node);
  }
  sIfElse(node: A.SIfElse): boolean {
    return wellFormedVisitor.sIfElse(node);
  }
  sIfPipe(node: A.SIfPipe): boolean {
    return wellFormedVisitor.sIfPipe(node);
  }
  sIfPipeElse(node: A.SIfPipeElse): boolean {
    return wellFormedVisitor.sIfPipeElse(node);
  }
  sCasesBranch(node: A.SCasesBranch): boolean {
    return wellFormedVisitor.sCasesBranch(node);
  }
  sSingletonCasesBranch(node: A.SSingletonCasesBranch): boolean {
    return wellFormedVisitor.sSingletonCasesBranch(node);
  }
  sCases(node: A.SCases): boolean {
    return wellFormedVisitor.sCases(node);
  }
  sCasesElse(node: A.SCasesElse): boolean {
    return wellFormedVisitor.sCasesElse(node);
  }
  sOp(node: A.SOp): boolean {
    return wellFormedVisitor.sOp(node);
  }
  sCheckTest(node: A.SCheckTest): boolean {
    return wellFormedVisitor.sCheckTest(node);
  }
  sParen(node: A.SParen): boolean {
    return wellFormedVisitor.sParen(node);
  }
  sLam(node: A.SLam): boolean {
    return wellFormedVisitor.sLam(node);
  }
  sMethod(node: A.SMethod): boolean {
    return wellFormedVisitor.sMethod(node);
  }
  sExtend(node: A.SExtend): boolean {
    return wellFormedVisitor.sExtend(node);
  }
  sUpdate(node: A.SUpdate): boolean {
    return wellFormedVisitor.sUpdate(node);
  }
  sTupleGet(node: A.STupleGet): boolean {
    return wellFormedVisitor.sTupleGet(node);
  }
  sObj(node: A.SObj): boolean {
    return wellFormedVisitor.sObj(node);
  }
  sArray(node: A.SArray): boolean {
    return wellFormedVisitor.sArray(node);
  }
  sConstruct(node: A.SConstruct): boolean {
    return wellFormedVisitor.sConstruct(node);
  }
  sApp(node: A.SApp): boolean {
    const _fun = node._fun;
    if (A.isSDot(_fun) && A.isSId(_fun.obj)
        && (_fun.obj.id.toname() === 'builtins') && (_fun.field === 'trace-value')) {
      // this is effectively still a top-level expression, so don't penalize it
      // for being inside a desugaring-introduced function call
      return _fun.visit(this) && node.args.every((a) => a.visit(this));
    } else {
      return wellFormedVisitor.sApp(node);
    }
  }
  sPrimApp(node: A.SPrimApp): boolean {
    return wellFormedVisitor.sPrimApp(node);
  }
  sFrac(node: A.SFrac): boolean {
    return wellFormedVisitor.sFrac(node);
  }
  sReactor(node: A.SReactor): boolean {
    return wellFormedVisitor.sReactor(node);
  }
  sRfrac(node: A.SRfrac): boolean {
    return wellFormedVisitor.sRfrac(node);
  }
  sId(node: A.SId): boolean {
    return wellFormedVisitor.sId(node);
  }
  sIdVar(node: A.SIdVar): boolean {
    return wellFormedVisitor.sIdVar(node);
  }
  sIdLetrec(node: A.SIdLetrec): boolean {
    return wellFormedVisitor.sIdLetrec(node);
  }
  sDot(node: A.SDot): boolean {
    return wellFormedVisitor.sDot(node);
  }
  sGetBang(node: A.SGetBang): boolean {
    return wellFormedVisitor.sGetBang(node);
  }
  sBracket(node: A.SBracket): boolean {
    return wellFormedVisitor.sBracket(node);
  }
  sFor(node: A.SFor): boolean {
    return wellFormedVisitor.sFor(node);
  }
  sCheck(node: A.SCheck): boolean {
    return wellFormedVisitor.sCheck(node);
  }
  sDataField(node: A.SDataField): boolean {
    return wellFormedVisitor.sDataField(node);
  }
  sMutableField(node: A.SMutableField): boolean {
    return wellFormedVisitor.sMutableField(node);
  }
  sMethodField(node: A.SMethodField): boolean {
    return wellFormedVisitor.sMethodField(node);
  }
  sForBind(node: A.SForBind): boolean {
    return wellFormedVisitor.sForBind(node);
  }
  sVariantMember(node: A.SVariantMember): boolean {
    switch (node.bind.$name) {
      case 's-bind':
        return wellFormedVisitor.sVariantMember(node);
      case 's-tuple-bind': {
        // The Pyret original returns the result of wf-error (nothing) here
        wfError([ED.text('Tuple binding not allowed as variant member')], node.bind.l);
        return false;
      }
      default: throw new InternalCompilerError('sVariantMember: unknown bind ' + (node.bind as any).$name);
    }
  }
  sTable(node: A.STable): boolean {
    return wellFormedVisitor.sTable(node);
  }
  sLoadTable(node: A.SLoadTable): boolean {
    return wellFormedVisitor.sLoadTable(node);
  }
  sTableExtend(node: A.STableExtend): boolean {
    return wellFormedVisitor.sTableExtend(node);
  }
  aArrow(node: A.AArrow): boolean {
    return wellFormedVisitor.aArrow(node);
  }
  aArrowArgnames(node: A.AArrowArgnames): boolean {
    return wellFormedVisitor.aArrowArgnames(node);
  }
  aMethod(node: A.AMethod): boolean {
    return wellFormedVisitor.aMethod(node);
  }
  aRecord(node: A.ARecord): boolean {
    return wellFormedVisitor.aRecord(node);
  }
  aApp(node: A.AApp): boolean {
    return wellFormedVisitor.aApp(node);
  }
  aPred(node: A.APred): boolean {
    return wellFormedVisitor.aPred(node);
  }
  aDot(node: A.ADot): boolean {
    return wellFormedVisitor.aDot(node);
  }
  aField(node: A.AField): boolean {
    return wellFormedVisitor.aField(node);
  }
}

const topLevelVisitor = new TopLevelVisitor();

export function checkWellFormed(ast: A.Program): C.CompileResult<A.Program> {
  curShared = [];
  errors = [];
  inCheckBlock = false;
  // errors are accumulated with push (chronological); the Pyret original
  // prepends and reverses, which yields the same final order.
  const ans = (ast.visit(topLevelVisitor) && (errors.length === 0))
    ? C.ok(ast)
    : C.err(errors.slice());
  // cleanup
  curShared = [];
  errors = [];
  inCheckBlock = false;
  return ans;
}
