/*
  TypeScript port of src/arr/compiler/js-ast.arr.

  Naming note: several Pyret data declarations here have a variant whose
  class name collides with the data-declaration (union) name: j-block,
  j-case, j-expr (a JStmt variant), j-field, j-binop and j-unop (JExpr
  variants colliding with the operator data decls JBinop/JUnop). Following
  the precedent in srcloc.ts (variant class `Srcloc`, union `Loc`), the rule
  is uniform: the BARE name is always the variant class (JBlock, JCase,
  JExpr, JField, JBinop, JUnop), and the union type for a data declaration
  whose name was claimed by a variant is suffixed with `T`: JBlockT, JCaseT,
  JExprT, JFieldT, JBinopT (the operators), JUnopT (the operators).
  `XBase` abstract classes always belong to the data declaration X.
  The unions JStmt and SourceMapFlags keep their plain names (no clash).
*/

import * as PP from './pprint';
import * as CL from './concat-lists';
import * as A from './ast';
import { PyretNumber } from './interop/js-numbers';
import { raise } from './shared';

// The npm `source-map` package (v0.5.7, synchronous API), used directly
// instead of going through the Pyret runtime's source-map-lib wrapper.
// eslint-disable-next-line @typescript-eslint/no-var-requires
const sourceMap = require('source-map');
const SN = sourceMap.SourceNode;

export type CList<T> = CL.ConcatList<T>;

export const INDENT = 2;
export const breakOne = PP.sbreak(1);
export const blankOne = PP.blank(1);

export function strpos(uri: string, loc: any): string {
  return ['', loc.startLine, loc.startColumn, loc.startChar, loc.endLine, loc.endColumn, loc.endChar].join(',');
}

// ---------- data SourceMapFlags ----------

export abstract class SourceMapFlagsBase {
  abstract get $name(): string;
}

export class NodeStart extends SourceMapFlagsBase {
  get $name(): 'node-start' { return 'node-start'; }
  constructor(public uri: string, public line: number, public col: number, public name: string) { super(); }
}

export class NodeEnd extends SourceMapFlagsBase {
  get $name(): 'node-end' { return 'node-end'; }
}

export type SourceMapFlags = NodeStart | NodeEnd;

export function isNodeStart(x: any): x is NodeStart { return x instanceof NodeStart; }
export function isNodeEnd(x: any): x is NodeEnd { return x instanceof NodeEnd; }

export const nodeEnd: NodeEnd = new NodeEnd();

// A printer receives plain strings and (for the source-map printer)
// node-start/node-end flags.
export type UglyPrinter = (s: string | SourceMapFlags) => void;

// ---------- Pyret torepr on strings ----------
// Mirrors runtime.js's escapeString/replaceUnprintableStringChars exactly,
// which is what `torepr(self.s)` produces for a Pyret string.

function replaceUnprintableStringChars(s: string): string {
  const ret: string[] = [];
  for (let i = 0; i < s.length; i++) {
    const val = s.charCodeAt(i);
    switch (val) {
      case 9: ret.push('\\t'); break;
      case 10: ret.push('\\n'); break;
      case 13: ret.push('\\r'); break;
      case 34: ret.push('\\"'); break;
      case 92: ret.push('\\\\'); break;
      default:
        if (val >= 32 && val <= 126) {
          ret.push(s.charAt(i));
        } else {
          let numStr = val.toString(16).toUpperCase();
          while (numStr.length < 4) {
            numStr = '0' + numStr;
          }
          ret.push('\\u' + numStr);
        }
        break;
    }
  }
  return ret.join('');
}

export function torepr(s: string): string {
  return '"' + replaceUnprintableStringChars(s) + '"';
}

// ---------- source-map machinery ----------
// Reimplements src/js/trove/source-map-lib.js's new-map/start-node/end-node/
// string/get layer on top of the npm source-map package, so that the
// generated `theMap` in compiled files is identical to the Pyret compiler's.

interface SMNodeInProgress {
  line: number;
  col: number;
  file: string;
  name: string;
  elts: any[];
}

function newMap(line: number, col: number, file: string, name: string) {
  let cur: SMNodeInProgress = { line: line, col: col, file: file, name: name, elts: [] };
  const stack: SMNodeInProgress[] = [cur];
  function startNode(line2: number, col2: number, file2: string, name2: string): void {
    cur = { line: line2, col: col2, file: file2, name: name2, elts: [] };
    stack.push(cur);
  }
  function endNode(): void {
    const elt = stack.pop()!;
    cur = stack[stack.length - 1];
    cur.elts.push(new SN(elt.line, elt.col, elt.file, elt.elts, elt.name));
  }
  function string(s: string): void {
    const elt = stack[stack.length - 1];
    const lastix = elt.elts.length - 1;
    if (elt.elts.length === 0) {
      elt.elts.push(s);
    } else if (typeof elt.elts[lastix] === 'string') {
      elt.elts[lastix] += s;
    } else {
      elt.elts.push(s);
    }
  }
  function get(): any {
    return new SN(cur.line, cur.col, cur.file, cur.elts, cur.name);
  }
  return { startNode, endNode, string, get };
}

export interface CodeWithMap {
  code: string;
  map: string;
}

function toStringWithSourceMap(sn: any, name: string): CodeWithMap {
  const mapped = sn.toStringWithSourceMap({ file: name });
  return { code: mapped.code, map: mapped.map.toString() };
}

export function stringPrinter(): { append: UglyPrinter; get: () => string } {
  const strs: string[] = [];
  return {
    append: (s: string | SourceMapFlags): void => {
      if (typeof s === 'string') {
        strs.push(s);
      }
    },
    get: (): string => strs.join('')
  };
}

export function sourcemapPrinter(uri: string, line: number, col: number, name: string): { append: UglyPrinter; get: () => CodeWithMap } {
  const theMap = newMap(line, col, uri, name);
  return {
    append: (s: string | SourceMapFlags): void => {
      if (isNodeStart(s)) {
        theMap.startNode(s.line, s.col, s.uri, s.name);
      } else if (isNodeEnd(s)) {
        theMap.endNode();
      } else if (typeof s === 'string') {
        theMap.string(s);
      }
    },
    get: (): CodeWithMap => toStringWithSourceMap(theMap.get(), uri)
  };
}

export type Label = { get: () => number };

// Pyret's sharing `visit` dispatches through `_match`: call the visitor
// method named after the variant, or raise if it is missing. (The original
// message uses self.label(), which equals the variant tag wherever defined.)
function visitDispatch(visitor: any, methodName: string, node: any): any {
  const f = visitor[methodName];
  if (typeof f !== 'function') {
    raise('No visitor field for ' + node.$name);
  }
  return f.call(visitor, node);
}

// ---------- data JBlock ----------

export abstract class JBlockBase {
  abstract get $name(): string;
  abstract printUglySource(printer: UglyPrinter): void;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
  toUglySourcemap(uri: string, line: number, col: number, name: string): CodeWithMap {
    const printer = sourcemapPrinter(uri, line, col, name);
    this.printUglySource(printer.append);
    return printer.get();
  }
  toUglySource(): string {
    const strprint = stringPrinter();
    this.printUglySource(strprint.append);
    return strprint.get();
  }
}

export class JBlock extends JBlockBase {
  get $name(): 'j-block' { return 'j-block'; }
  constructor(public stmts: CList<JStmt>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jBlock', this); }
  label(): string { return 'j-block'; }
  printUglySource(printer: UglyPrinter): void {
    this.stmts.each(s => s.printUglySource(printer));
  }
  tosource(): PP.PPrintDoc {
    if (this.stmts.isEmpty()) {
      return PP.mtDoc;
    } else {
      return PP.vert(this.stmts.mapToList(s => s.tosource()));
    }
  }
}

export class JBlock1 extends JBlockBase {
  get $name(): 'j-block1' { return 'j-block1'; }
  constructor(public stmt: JStmt) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jBlock1', this); }
  label(): string { return 'j-block1'; }
  printUglySource(printer: UglyPrinter): void {
    this.stmt.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.vert([this.stmt.tosource()]);
  }
}

export type JBlockT = JBlock | JBlock1;

export function isJBlock(x: any): x is JBlock { return x instanceof JBlock; }
export function isJBlock1(x: any): x is JBlock1 { return x instanceof JBlock1; }

// ---------- data JStmt ----------

export abstract class JStmtBase {
  abstract get $name(): string;
  abstract printUglySource(printer: UglyPrinter): void;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
  toUglySourcemap(uri: string, line: number, col: number, name: string): CodeWithMap {
    const printer = sourcemapPrinter(uri, line, col, name);
    this.printUglySource(printer.append);
    return printer.get();
  }
  toUglySource(): string {
    const strprint = stringPrinter();
    this.printUglySource(strprint.append);
    return strprint.get();
  }
}

export class JVar extends JStmtBase {
  get $name(): 'j-var' { return 'j-var'; }
  constructor(public name: A.Name, public rhs: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jVar', this); }
  label(): string { return 'j-var'; }
  printUglySource(printer: UglyPrinter): void {
    printer('var ' + this.name.tosourcestring() + ' = ');
    this.rhs.printUglySource(printer);
    printer(';\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(
      PP.str('var ').append(PP.group(PP.nest(INDENT, this.name.toCompiledSource()
        .append(PP.str(' =')).append(PP.sbreak(1)).append(this.rhs.tosource())))).append(PP.str(';')));
  }
}

export class JIf1 extends JStmtBase {
  get $name(): 'j-if1' { return 'j-if1'; }
  constructor(public cond: JExprT, public consq: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jIf1', this); }
  label(): string { return 'j-if1'; }
  printUglySource(printer: UglyPrinter): void {
    printer('if(');
    this.cond.printUglySource(printer);
    printer(') {\n');
    this.consq.printUglySource(printer);
    printer('}\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.str('if').append(PP.parens(this.cond.tosource()))).append(PP.str(' '))
      .append(PP.surround(INDENT, 1, PP.lbrace, this.consq.tosource(), PP.rbrace));
  }
}

export class JIf extends JStmtBase {
  get $name(): 'j-if' { return 'j-if'; }
  constructor(public cond: JExprT, public consq: JBlockT, public alt: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jIf', this); }
  label(): string { return 'j-if'; }
  printUglySource(printer: UglyPrinter): void {
    printer('if(');
    this.cond.printUglySource(printer);
    printer(') {\n');
    this.consq.printUglySource(printer);
    printer('} else {\n');
    this.alt.printUglySource(printer);
    printer('}\n');
  }
  tosource(): PP.PPrintDoc {
    const altDoc = this.alt.tosource();
    const elseDoc =
      PP.isMtDoc(altDoc) ? PP.mtDoc
        : PP.str(' else ').append(PP.surround(INDENT, 1, PP.lbrace, altDoc, PP.rbrace));
    return PP.group(PP.str('if').append(PP.parens(this.cond.tosource()))).append(PP.str(' '))
      .append(PP.surround(INDENT, 1, PP.lbrace, this.consq.tosource(), PP.rbrace))
      .append(elseDoc);
  }
}

export class JReturn extends JStmtBase {
  get $name(): 'j-return' { return 'j-return'; }
  constructor(public expr: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jReturn', this); }
  label(): string { return 'j-return'; }
  printUglySource(printer: UglyPrinter): void {
    printer('return ');
    this.expr.printUglySource(printer);
    printer(';\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.str('return ').append(this.expr.tosource()).append(PP.str(';'));
  }
}

export class JTryCatch extends JStmtBase {
  get $name(): 'j-try-catch' { return 'j-try-catch'; }
  catch: JBlockT;
  constructor(public body: JBlockT, public exn: A.Name, catchBlock: JBlockT) {
    super();
    this.catch = catchBlock;
  }
  visit(visitor: any): any { return visitDispatch(visitor, 'jTryCatch', this); }
  label(): string { return 'j-try-catch'; }
  printUglySource(printer: UglyPrinter): void {
    printer('try {\n');
    this.body.printUglySource(printer);
    printer('} catch(' + this.exn.tosourcestring() + ') {\n');
    this.catch.printUglySource(printer);
    printer('}\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.surround(INDENT, 1, PP.str('try {'), this.body.tosource(), PP.rbrace)
      .append(PP.surround(INDENT, 1, PP.str(' catch(' + this.exn.tosourcestring() + ') {'), this.catch.tosource(), PP.rbrace));
  }
}

export class JThrow extends JStmtBase {
  get $name(): 'j-throw' { return 'j-throw'; }
  constructor(public exp: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jThrow', this); }
  label(): string { return 'j-throw'; }
  printUglySource(printer: UglyPrinter): void {
    printer('throw ');
    this.exp.printUglySource(printer);
    printer(';\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT, PP.str('throw ').append(this.exp.tosource()))).append(PP.str(';'));
  }
}

export class JExpr extends JStmtBase {
  get $name(): 'j-expr' { return 'j-expr'; }
  constructor(public expr: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jExpr', this); }
  label(): string { return 'j-expr'; }
  printUglySource(printer: UglyPrinter): void {
    // (BSL) I wish this weren't necessary
    if (isJObj(this.expr)) { printer('('); }
    this.expr.printUglySource(printer);
    if (isJObj(this.expr)) { printer(')'); }
    printer(';\n');
  }
  tosource(): PP.PPrintDoc {
    return (isJObj(this.expr) ? PP.parens(this.expr.tosource()) : this.expr.tosource())
      .append(PP.str(';'));
  }
}

export class JBreak extends JStmtBase {
  get $name(): 'j-break' { return 'j-break'; }
  visit(visitor: any): any { return visitDispatch(visitor, 'jBreak', this); }
  label(): string { return 'j-break'; }
  printUglySource(printer: UglyPrinter): void { printer('break;\n'); }
  tosource(): PP.PPrintDoc { return PP.str('break;'); }
}

export class JContinue extends JStmtBase {
  get $name(): 'j-continue' { return 'j-continue'; }
  visit(visitor: any): any { return visitDispatch(visitor, 'jContinue', this); }
  label(): string { return 'j-continue'; }
  printUglySource(printer: UglyPrinter): void { printer('continue;\n'); }
  tosource(): PP.PPrintDoc { return PP.str('continue;'); }
}

export class JSwitch extends JStmtBase {
  get $name(): 'j-switch' { return 'j-switch'; }
  constructor(public exp: JExprT, public branches: CList<JCaseT>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jSwitch', this); }
  label(): string { return 'j-switch'; }
  printUglySource(printer: UglyPrinter): void {
    printer('switch(');
    this.exp.printUglySource(printer);
    printer(') {\n');
    this.branches.each(b => b.printUglySource(printer));
    printer('}\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.surround(0, 1, PP.group(PP.str('switch').append(PP.parens(this.exp.tosource())).append(PP.sbreak(1)).append(PP.lbrace)),
      PP.flowMap(PP.hardline, (b: JCaseT) => b.tosource(), this.branches.toList()), PP.rbrace);
  }
}

export class JWhile extends JStmtBase {
  get $name(): 'j-while' { return 'j-while'; }
  constructor(public cond: JExprT, public body: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jWhile', this); }
  label(): string { return 'j-while'; }
  printUglySource(printer: UglyPrinter): void {
    printer('while(');
    this.cond.printUglySource(printer);
    printer(') {\n');
    this.body.printUglySource(printer);
    printer('}\n');
  }
  tosource(): PP.PPrintDoc {
    return PP.surround(INDENT, 1, PP.group(PP.str('while').append(PP.parens(this.cond.tosource())).append(PP.sbreak(1)).append(PP.lbrace)),
      this.body.tosource(), PP.rbrace);
  }
}

export class JFor extends JStmtBase {
  get $name(): 'j-for' { return 'j-for'; }
  constructor(public createVar: boolean, public init: JExprT, public cond: JExprT, public update: JExprT, public body: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jFor', this); }
  // NOTE: j-for has no label() method in the Pyret source.
  printUglySource(printer: UglyPrinter): void {
    printer('for(');
    if (this.createVar) {
      printer('var ');
    }
    this.init.printUglySource(printer);
    printer(';');
    this.cond.printUglySource(printer);
    printer(';');
    this.update.printUglySource(printer);
    printer(') {\n');
    this.body.printUglySource(printer);
    printer('}\n');
  }
  tosource(): PP.PPrintDoc {
    const semi = PP.str(';').append(PP.sbreak(1));
    const initSrc =
      this.createVar ? PP.str('var ').append(this.init.tosource()) : this.init.tosource();
    return PP.surround(INDENT, 1,
      PP.group(PP.str('for')
        .append(PP.parens(initSrc.append(semi).append(this.cond.tosource()).append(semi).append(this.update.tosource())))
        .append(PP.sbreak(1)).append(PP.lbrace)),
      this.body.tosource(),
      PP.rbrace);
  }
}

export type JStmt =
  JVar | JIf1 | JIf | JReturn | JTryCatch | JThrow | JExpr | JBreak | JContinue
  | JSwitch | JWhile | JFor;

export function isJVar(x: any): x is JVar { return x instanceof JVar; }
export function isJIf1(x: any): x is JIf1 { return x instanceof JIf1; }
export function isJIf(x: any): x is JIf { return x instanceof JIf; }
export function isJReturn(x: any): x is JReturn { return x instanceof JReturn; }
export function isJTryCatch(x: any): x is JTryCatch { return x instanceof JTryCatch; }
export function isJThrow(x: any): x is JThrow { return x instanceof JThrow; }
export function isJExpr(x: any): x is JExpr { return x instanceof JExpr; }
export function isJBreak(x: any): x is JBreak { return x instanceof JBreak; }
export function isJContinue(x: any): x is JContinue { return x instanceof JContinue; }
export function isJSwitch(x: any): x is JSwitch { return x instanceof JSwitch; }
export function isJWhile(x: any): x is JWhile { return x instanceof JWhile; }
export function isJFor(x: any): x is JFor { return x instanceof JFor; }

export const jBreak: JBreak = new JBreak();
export const jContinue: JContinue = new JContinue();

// ---------- data JCase ----------

export abstract class JCaseBase {
  abstract get $name(): string;
  abstract printUglySource(printer: UglyPrinter): void;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
  toUglySourcemap(uri: string, line: number, col: number, name: string): CodeWithMap {
    const printer = sourcemapPrinter(uri, line, col, name);
    this.printUglySource(printer.append);
    return printer.get();
  }
  toUglySource(): string {
    const strprint = stringPrinter();
    this.printUglySource(strprint.append);
    return strprint.get();
  }
}

export class JCase extends JCaseBase {
  get $name(): 'j-case' { return 'j-case'; }
  constructor(public exp: JExprT, public body: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jCase', this); }
  label(): string { return 'j-case'; }
  printUglySource(printer: UglyPrinter): void {
    printer('case ');
    this.exp.printUglySource(printer);
    printer(': ');
    this.body.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT,
      PP.group(PP.nest(INDENT, PP.str('case ').append(this.exp.tosource()).append(PP.str(':')))).append(PP.sbreak(1))
        .append(this.body.tosource())));
  }
}

export class JDefault extends JCaseBase {
  get $name(): 'j-default' { return 'j-default'; }
  constructor(public body: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jDefault', this); }
  label(): string { return 'j-default'; }
  printUglySource(printer: UglyPrinter): void {
    printer('default: ');
    this.body.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT, PP.str('default:').append(PP.sbreak(1)).append(this.body.tosource())));
  }
}

export type JCaseT = JCase | JDefault;

export function isJCase(x: any): x is JCase { return x instanceof JCase; }
export function isJDefault(x: any): x is JDefault { return x instanceof JDefault; }

// ---------- data JBinop ----------

export abstract class JBinopBase {
  abstract get $name(): string;
  abstract toUglySource(): string;
  printUglySource(printer: UglyPrinter): void {
    printer(this.toUglySource());
  }
  tosource(): PP.PPrintDoc {
    return PP.str(this.toUglySource());
  }
}

export class JPlus extends JBinopBase {
  get $name(): 'j-plus' { return 'j-plus'; }
  toUglySource(): string { return '+'; }
}
export class JMinus extends JBinopBase {
  get $name(): 'j-minus' { return 'j-minus'; }
  toUglySource(): string { return '-'; }
}
export class JTimes extends JBinopBase {
  get $name(): 'j-times' { return 'j-times'; }
  toUglySource(): string { return '*'; }
}
export class JDivide extends JBinopBase {
  get $name(): 'j-divide' { return 'j-divide'; }
  toUglySource(): string { return '/'; }
}
export class JAnd extends JBinopBase {
  get $name(): 'j-and' { return 'j-and'; }
  toUglySource(): string { return '&&'; }
}
export class JOr extends JBinopBase {
  get $name(): 'j-or' { return 'j-or'; }
  toUglySource(): string { return '||'; }
}
export class JLt extends JBinopBase {
  get $name(): 'j-lt' { return 'j-lt'; }
  toUglySource(): string { return '<'; }
}
export class JLeq extends JBinopBase {
  get $name(): 'j-leq' { return 'j-leq'; }
  toUglySource(): string { return '<='; }
}
export class JGt extends JBinopBase {
  get $name(): 'j-gt' { return 'j-gt'; }
  toUglySource(): string { return '>'; }
}
export class JGeq extends JBinopBase {
  get $name(): 'j-geq' { return 'j-geq'; }
  toUglySource(): string { return '>='; }
}
export class JEq extends JBinopBase {
  get $name(): 'j-eq' { return 'j-eq'; }
  toUglySource(): string { return '==='; }
}
export class JEquals extends JBinopBase {
  get $name(): 'j-equals' { return 'j-equals'; }
  toUglySource(): string { return '=='; }
}
export class JNeq extends JBinopBase {
  get $name(): 'j-neq' { return 'j-neq'; }
  toUglySource(): string { return '!=='; }
}
export class JNequals extends JBinopBase {
  get $name(): 'j-nequals' { return 'j-nequals'; }
  toUglySource(): string { return '!='; }
}
export class JIn extends JBinopBase {
  get $name(): 'j-in' { return 'j-in'; }
  toUglySource(): string { return 'in'; }
}
export class JInstanceof extends JBinopBase {
  get $name(): 'j-instanceof' { return 'j-instanceof'; }
  toUglySource(): string { return 'instanceof'; }
}

export type JBinopT =
  JPlus | JMinus | JTimes | JDivide | JAnd | JOr | JLt | JLeq | JGt | JGeq
  | JEq | JEquals | JNeq | JNequals | JIn | JInstanceof;

export function isJPlus(x: any): x is JPlus { return x instanceof JPlus; }
export function isJMinus(x: any): x is JMinus { return x instanceof JMinus; }
export function isJTimes(x: any): x is JTimes { return x instanceof JTimes; }
export function isJDivide(x: any): x is JDivide { return x instanceof JDivide; }
export function isJAnd(x: any): x is JAnd { return x instanceof JAnd; }
export function isJOr(x: any): x is JOr { return x instanceof JOr; }
export function isJLt(x: any): x is JLt { return x instanceof JLt; }
export function isJLeq(x: any): x is JLeq { return x instanceof JLeq; }
export function isJGt(x: any): x is JGt { return x instanceof JGt; }
export function isJGeq(x: any): x is JGeq { return x instanceof JGeq; }
export function isJEq(x: any): x is JEq { return x instanceof JEq; }
export function isJEquals(x: any): x is JEquals { return x instanceof JEquals; }
export function isJNeq(x: any): x is JNeq { return x instanceof JNeq; }
export function isJNequals(x: any): x is JNequals { return x instanceof JNequals; }
export function isJIn(x: any): x is JIn { return x instanceof JIn; }
export function isJInstanceof(x: any): x is JInstanceof { return x instanceof JInstanceof; }

export const jPlus: JPlus = new JPlus();
export const jMinus: JMinus = new JMinus();
export const jTimes: JTimes = new JTimes();
export const jDivide: JDivide = new JDivide();
export const jAnd: JAnd = new JAnd();
export const jOr: JOr = new JOr();
export const jLt: JLt = new JLt();
export const jLeq: JLeq = new JLeq();
export const jGt: JGt = new JGt();
export const jGeq: JGeq = new JGeq();
export const jEq: JEq = new JEq();
export const jEquals: JEquals = new JEquals();
export const jNeq: JNeq = new JNeq();
export const jNequals: JNequals = new JNequals();
export const jIn: JIn = new JIn();
export const jInstanceof: JInstanceof = new JInstanceof();

// ---------- data JUnop ----------

export abstract class JUnopBase {
  abstract get $name(): string;
  abstract toUglySource(): string;
  printUglySource(printer: UglyPrinter): void {
    printer(this.toUglySource());
  }
  tosource(): PP.PPrintDoc {
    return PP.str(this.toUglySource());
  }
}

export class JIncr extends JUnopBase {
  get $name(): 'j-incr' { return 'j-incr'; }
  toUglySource(): string { return '++'; }
}
export class JDecr extends JUnopBase {
  get $name(): 'j-decr' { return 'j-decr'; }
  toUglySource(): string { return '--'; }
}
export class JPostincr extends JUnopBase {
  get $name(): 'j-postincr' { return 'j-postincr'; }
  toUglySource(): string { return '++'; }
}
export class JPostdecr extends JUnopBase {
  get $name(): 'j-postdecr' { return 'j-postdecr'; }
  toUglySource(): string { return '--'; }
}
export class JNot extends JUnopBase {
  get $name(): 'j-not' { return 'j-not'; }
  toUglySource(): string { return '!'; }
}
export class JTypeof extends JUnopBase {
  get $name(): 'j-typeof' { return 'j-typeof'; }
  toUglySource(): string { return 'typeof'; }
}

export type JUnopT = JIncr | JDecr | JPostincr | JPostdecr | JNot | JTypeof;

export function isJIncr(x: any): x is JIncr { return x instanceof JIncr; }
export function isJDecr(x: any): x is JDecr { return x instanceof JDecr; }
export function isJPostincr(x: any): x is JPostincr { return x instanceof JPostincr; }
export function isJPostdecr(x: any): x is JPostdecr { return x instanceof JPostdecr; }
export function isJNot(x: any): x is JNot { return x instanceof JNot; }
export function isJTypeof(x: any): x is JTypeof { return x instanceof JTypeof; }

export const jIncr: JIncr = new JIncr();
export const jDecr: JDecr = new JDecr();
export const jPostincr: JPostincr = new JPostincr();
export const jPostdecr: JPostdecr = new JPostdecr();
export const jNot: JNot = new JNot();
export const jTypeof: JTypeof = new JTypeof();

// ---------- data JExpr ----------

export abstract class JExprBase {
  abstract get $name(): string;
  abstract printUglySource(printer: UglyPrinter): void;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
  toUglySourcemap(uri: string, line: number, col: number, name: string): CodeWithMap {
    const printer = sourcemapPrinter(uri, line, col, name);
    this.printUglySource(printer.append);
    const node = printer.get();
    return node;
  }
  toUglySource(): string {
    const strprint = stringPrinter();
    this.printUglySource(strprint.append);
    return strprint.get();
  }
}

export class JSourcenode extends JExprBase {
  get $name(): 'j-sourcenode' { return 'j-sourcenode'; }
  // loc is untyped in the Pyret source; in practice it is a Srcloc.
  constructor(public loc: any, public uri: string, public expr: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jSourcenode', this); }
  label(): string { return 'j-sourcenode'; }
  printUglySource(printer: UglyPrinter): void {
    printer(new NodeStart(this.uri, this.loc.startLine, this.loc.startColumn, strpos(this.uri, this.loc)));
    this.expr.printUglySource(printer);
    printer(nodeEnd);
  }
  tosource(): PP.PPrintDoc {
    return PP.surround(INDENT, 1, PP.str('('), this.expr.tosource(), PP.str(')'));
  }
}

export class JParens extends JExprBase {
  get $name(): 'j-parens' { return 'j-parens'; }
  constructor(public exp: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jParens', this); }
  label(): string { return 'j-parens'; }
  printUglySource(printer: UglyPrinter): void {
    printer('(');
    this.exp.printUglySource(printer);
    printer(')');
  }
  tosource(): PP.PPrintDoc {
    return PP.surround(INDENT, 1, PP.str('('), this.exp.tosource(), PP.str(')'));
  }
}

export class JRawCode extends JExprBase {
  get $name(): 'j-raw-code' { return 'j-raw-code'; }
  constructor(public s: string) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jRawCode', this); }
  // NOTE: j-raw-code has no label() method in the Pyret source.
  printUglySource(printer: UglyPrinter): void {
    printer(this.s);
  }
  tosource(): PP.PPrintDoc {
    return PP.str(this.s);
  }
}

export class JUnop extends JExprBase {
  get $name(): 'j-unop' { return 'j-unop'; }
  constructor(public exp: JExprT, public op: JUnopT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jUnop', this); }
  label(): string { return 'j-unop'; }
  printUglySource(printer: UglyPrinter): void {
    // NOTE: the Pyret source's cases-branch for the postfix decrement is
    // misspelled `j-postdeccr`, so it never matches; j-postdecr therefore
    // falls into the else branch and prints PREFIX. Replicated exactly.
    switch (this.op.$name) {
      case 'j-postincr':
        this.exp.printUglySource(printer);
        this.op.printUglySource(printer);
        break;
      default:
        this.op.printUglySource(printer);
        this.exp.printUglySource(printer);
        break;
    }
  }
  tosource(): PP.PPrintDoc {
    // Same `j-postdeccr` typo as above: j-postdecr renders prefix.
    switch (this.op.$name) {
      case 'j-postincr':
        return this.exp.tosource().append(this.op.tosource());
      default:
        return this.op.tosource().append(this.exp.tosource());
    }
  }
}

export class JBinop extends JExprBase {
  get $name(): 'j-binop' { return 'j-binop'; }
  constructor(public left: JExprT, public op: JBinopT, public right: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jBinop', this); }
  label(): string { return 'j-binop'; }
  printUglySource(printer: UglyPrinter): void {
    this.left.printUglySource(printer);
    printer(' ');
    this.op.printUglySource(printer);
    printer(' ');
    this.right.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.flow([this.left.tosource(), this.op.tosource(), this.right.tosource()]);
  }
}

export class JFun extends JExprBase {
  get $name(): 'j-fun' { return 'j-fun'; }
  constructor(public id: string, public name: string, public args: CList<A.Name>, public body: JBlockT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jFun', this); }
  label(): string { return 'j-fun'; }
  printUglySource(printer: UglyPrinter): void {
    printer('function ');
    printer(this.name);
    printer('(');
    let n = 0;
    this.args.each((arg: A.Name) => {
      if (n > 0) { printer(','); }
      printer(arg.tosourcestring());
      n = n + 1;
    });
    printer(') {\n');
    this.body.printUglySource(printer);
    printer('}');
  }
  tosource(): PP.PPrintDoc {
    const arglist = PP.nest(INDENT, PP.surroundSeparate(INDENT, 0, PP.lparen.append(PP.rparen), PP.lparen, PP.commabreak, PP.rparen, this.args.mapToList((a: A.Name) => a.toCompiledSource())));
    const header = PP.group(PP.str('function').append(arglist));
    return PP.surround(INDENT, 1, header.append(PP.str(' {')), this.body.tosource(), PP.str('}'));
  }
}

export class JNew extends JExprBase {
  get $name(): 'j-new' { return 'j-new'; }
  constructor(public func: JExprT, public args: CList<JExprT>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jNew', this); }
  label(): string { return 'j-new'; }
  printUglySource(printer: UglyPrinter): void {
    printer('new ');
    this.func.printUglySource(printer);
    printer('(');
    let n = 0;
    this.args.each((arg: JExprT) => {
      if (n > 0) { printer(','); }
      arg.printUglySource(printer);
      n = n + 1;
    });
    printer(')');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.str('new ').append(this.func.tosource())
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.mapToList((a: JExprT) => a.tosource()))))));
  }
}

export class JApp extends JExprBase {
  get $name(): 'j-app' { return 'j-app'; }
  constructor(public func: JExprT, public args: CList<JExprT>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jApp', this); }
  label(): string { return 'j-app'; }
  printUglySource(printer: UglyPrinter): void {
    this.func.printUglySource(printer);
    printer('(');
    let n = 0;
    this.args.each((arg: JExprT) => {
      if (n > 0) { printer(','); }
      arg.printUglySource(printer);
      n = n + 1;
    });
    printer(')');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(this.func.tosource()
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.mapToList((a: JExprT) => a.tosource()))))));
  }
}

export class JMethod extends JExprBase {
  get $name(): 'j-method' { return 'j-method'; }
  constructor(public obj: JExprT, public meth: string, public args: CList<JExprT>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jMethod', this); }
  label(): string { return 'j-method'; }
  printUglySource(printer: UglyPrinter): void {
    this.obj.printUglySource(printer);
    printer('.');
    printer(this.meth);
    printer('(');
    let n = 0;
    this.args.each((arg: JExprT) => {
      if (n > 0) { printer(','); }
      arg.printUglySource(printer);
      n = n + 1;
    });
    printer(')');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.infix(INDENT, 0, PP.str('.'), this.obj.tosource(), PP.str(this.meth))
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.mapToList((a: JExprT) => a.tosource()))))));
  }
}

export class JTernary extends JExprBase {
  get $name(): 'j-ternary' { return 'j-ternary'; }
  constructor(public test: JExprT, public consq: JExprT, public altern: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jTernary', this); }
  label(): string { return 'j-ternary'; }
  printUglySource(printer: UglyPrinter): void {
    this.test.printUglySource(printer);
    printer('?');
    this.consq.printUglySource(printer);
    printer(':');
    this.altern.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.parens(
      this.test.tosource()
        .append(PP.nest(INDENT, breakOne.append(PP.str('?')).append(blankOne).append(PP.group(PP.nest(INDENT, this.consq.tosource())))))
        .append(PP.nest(INDENT, breakOne.append(PP.str(':')).append(blankOne).append(PP.group(PP.nest(INDENT, this.altern.tosource()))))));
  }
}

export class JAssign extends JExprBase {
  get $name(): 'j-assign' { return 'j-assign'; }
  constructor(public name: A.Name, public rhs: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jAssign', this); }
  label(): string { return 'j-assign'; }
  printUglySource(printer: UglyPrinter): void {
    printer(this.name.tosourcestring());
    printer(' = ');
    this.rhs.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT, this.name.toCompiledSource().append(PP.str(' =')).append(breakOne).append(this.rhs.tosource())));
  }
}

export class JBracketAssign extends JExprBase {
  get $name(): 'j-bracket-assign' { return 'j-bracket-assign'; }
  constructor(public obj: JExprT, public field: JExprT, public rhs: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jBracketAssign', this); }
  label(): string { return 'j-bracket-assign'; }
  printUglySource(printer: UglyPrinter): void {
    this.obj.printUglySource(printer);
    printer('[');
    this.field.printUglySource(printer);
    printer(']');
    printer(' = ');
    this.rhs.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT, this.obj.tosource().append(PP.lbrack).append(this.field.tosource()).append(PP.rbrack).append(PP.str(' ='))
      .append(breakOne).append(this.rhs.tosource())));
  }
}

export class JDotAssign extends JExprBase {
  get $name(): 'j-dot-assign' { return 'j-dot-assign'; }
  constructor(public obj: JExprT, public name: string, public rhs: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jDotAssign', this); }
  label(): string { return 'j-dot-assign'; }
  printUglySource(printer: UglyPrinter): void {
    this.obj.printUglySource(printer);
    printer('.');
    printer(this.name);
    printer(' = ');
    this.rhs.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT, PP.infix(INDENT, 0, PP.str('.'), this.obj.tosource(), PP.str(this.name)).append(PP.str(' =')).append(breakOne).append(this.rhs.tosource())));
  }
}

export class JDot extends JExprBase {
  get $name(): 'j-dot' { return 'j-dot'; }
  constructor(public obj: JExprT, public field: string) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jDot', this); }
  label(): string { return 'j-dot'; }
  printUglySource(printer: UglyPrinter): void {
    this.obj.printUglySource(printer);
    printer('.');
    printer(this.field);
  }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 0, PP.str('.'), this.obj.tosource(), PP.str(this.field));
  }
}

export class JBracket extends JExprBase {
  get $name(): 'j-bracket' { return 'j-bracket'; }
  constructor(public obj: JExprT, public field: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jBracket', this); }
  label(): string { return 'j-bracket'; }
  printUglySource(printer: UglyPrinter): void {
    this.obj.printUglySource(printer);
    printer('[');
    this.field.printUglySource(printer);
    printer(']');
  }
  tosource(): PP.PPrintDoc {
    return PP.group(this.obj.tosource().append(
      PP.surround(INDENT, 0, PP.lbrack, this.field.tosource(), PP.rbrack)));
  }
}

export class JList extends JExprBase {
  get $name(): 'j-list' { return 'j-list'; }
  constructor(public multiLine: boolean, public elts: CList<JExprT>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jList', this); }
  label(): string { return 'j-list'; }
  printUglySource(printer: UglyPrinter): void {
    printer('[');
    const sep = this.multiLine ? ',\n' : ',';
    let n = 0;
    this.elts.each((elt: JExprT) => {
      if (n > 0) { printer(sep); }
      elt.printUglySource(printer);
      n = n + 1;
    });
    printer(']');
  }
  tosource(): PP.PPrintDoc {
    return PP.surroundSeparate(INDENT, 1, PP.lbrack.append(PP.rbrack),
      PP.lbrack, PP.commabreak, PP.rbrack, this.elts.mapToList((e: JExprT) => e.tosource()));
  }
}

export class JObj extends JExprBase {
  get $name(): 'j-obj' { return 'j-obj'; }
  constructor(public fields: CList<JFieldT>) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jObj', this); }
  label(): string { return 'j-obj'; }
  printUglySource(printer: UglyPrinter): void {
    printer('{');
    let n = 0;
    this.fields.each((field: JFieldT) => {
      if (n > 0) { printer(',\n'); }
      field.printUglySource(printer);
      n = n + 1;
    });
    printer('}');
  }
  tosource(): PP.PPrintDoc {
    return PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace),
      PP.lbrace, PP.commabreak, PP.rbrace, this.fields.mapToList((f: JFieldT) => f.tosource()));
  }
}

export class JId extends JExprBase {
  get $name(): 'j-id' { return 'j-id'; }
  constructor(public id: A.Name) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jId', this); }
  label(): string { return 'j-id'; }
  printUglySource(printer: UglyPrinter): void {
    printer(this.id.tosourcestring());
  }
  tosource(): PP.PPrintDoc { return this.id.toCompiledSource(); }
}

export class JStr extends JExprBase {
  get $name(): 'j-str' { return 'j-str'; }
  constructor(public s: string) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jStr', this); }
  label(): string { return 'j-str'; }
  printUglySource(printer: UglyPrinter): void {
    printer(torepr(this.s));
  }
  tosource(): PP.PPrintDoc { return PP.str(torepr(this.s)); }
}

export class JNum extends JExprBase {
  get $name(): 'j-num' { return 'j-num'; }
  constructor(public n: PyretNumber) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jNum', this); }
  label(): string { return 'j-num'; }
  printUglySource(printer: UglyPrinter): void {
    // Pyret tostring on a number is String(val) (runtime.js ReprMethods);
    // boxed js-numbers values render via their own toString().
    printer(String(this.n));
  }
  tosource(): PP.PPrintDoc { return PP.number(this.n); }
}

export class JTrue extends JExprBase {
  get $name(): 'j-true' { return 'j-true'; }
  visit(visitor: any): any { return visitDispatch(visitor, 'jTrue', this); }
  label(): string { return 'j-true'; }
  printUglySource(printer: UglyPrinter): void {
    printer('true');
  }
  tosource(): PP.PPrintDoc { return PP.str('true'); }
}

export class JFalse extends JExprBase {
  get $name(): 'j-false' { return 'j-false'; }
  visit(visitor: any): any { return visitDispatch(visitor, 'jFalse', this); }
  label(): string { return 'j-false'; }
  printUglySource(printer: UglyPrinter): void {
    printer('false');
  }
  tosource(): PP.PPrintDoc { return PP.str('false'); }
}

export class JNull extends JExprBase {
  get $name(): 'j-null' { return 'j-null'; }
  visit(visitor: any): any { return visitDispatch(visitor, 'jNull', this); }
  label(): string { return 'j-null'; }
  printUglySource(printer: UglyPrinter): void {
    printer('null');
  }
  tosource(): PP.PPrintDoc { return PP.str('null'); }
}

export class JUndefined extends JExprBase {
  get $name(): 'j-undefined' { return 'j-undefined'; }
  visit(visitor: any): any { return visitDispatch(visitor, 'jUndefined', this); }
  label(): string { return 'j-undefined'; }
  printUglySource(printer: UglyPrinter): void {
    printer('undefined');
  }
  tosource(): PP.PPrintDoc { return PP.str('undefined'); }
}

export class JLabel extends JExprBase {
  get $name(): 'j-label' { return 'j-label'; }
  // TODO(joe): We don't really use label, so ignoring this clash for the moment
  // (in the Pyret source, j-label has no label() method; `label` is the field)
  constructor(public label: Label) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jLabel', this); }
  printUglySource(printer: UglyPrinter): void {
    printer(String(this.label.get()));
  }
  tosource(): PP.PPrintDoc { return PP.number(this.label.get()); }
}

export type JExprT =
  JSourcenode | JParens | JRawCode | JUnop | JBinop | JFun | JNew | JApp
  | JMethod | JTernary | JAssign | JBracketAssign | JDotAssign | JDot | JBracket
  | JList | JObj | JId | JStr | JNum | JTrue | JFalse | JNull | JUndefined | JLabel;

export function isJSourcenode(x: any): x is JSourcenode { return x instanceof JSourcenode; }
export function isJParens(x: any): x is JParens { return x instanceof JParens; }
export function isJRawCode(x: any): x is JRawCode { return x instanceof JRawCode; }
export function isJUnop(x: any): x is JUnop { return x instanceof JUnop; }
export function isJBinop(x: any): x is JBinop { return x instanceof JBinop; }
export function isJFun(x: any): x is JFun { return x instanceof JFun; }
export function isJNew(x: any): x is JNew { return x instanceof JNew; }
export function isJApp(x: any): x is JApp { return x instanceof JApp; }
export function isJMethod(x: any): x is JMethod { return x instanceof JMethod; }
export function isJTernary(x: any): x is JTernary { return x instanceof JTernary; }
export function isJAssign(x: any): x is JAssign { return x instanceof JAssign; }
export function isJBracketAssign(x: any): x is JBracketAssign { return x instanceof JBracketAssign; }
export function isJDotAssign(x: any): x is JDotAssign { return x instanceof JDotAssign; }
export function isJDot(x: any): x is JDot { return x instanceof JDot; }
export function isJBracket(x: any): x is JBracket { return x instanceof JBracket; }
export function isJList(x: any): x is JList { return x instanceof JList; }
export function isJObj(x: any): x is JObj { return x instanceof JObj; }
export function isJId(x: any): x is JId { return x instanceof JId; }
export function isJStr(x: any): x is JStr { return x instanceof JStr; }
export function isJNum(x: any): x is JNum { return x instanceof JNum; }
export function isJTrue(x: any): x is JTrue { return x instanceof JTrue; }
export function isJFalse(x: any): x is JFalse { return x instanceof JFalse; }
export function isJNull(x: any): x is JNull { return x instanceof JNull; }
export function isJUndefined(x: any): x is JUndefined { return x instanceof JUndefined; }
export function isJLabel(x: any): x is JLabel { return x instanceof JLabel; }

export const jTrue: JTrue = new JTrue();
export const jFalse: JFalse = new JFalse();
export const jNull: JNull = new JNull();
export const jUndefined: JUndefined = new JUndefined();

// ---------- next-j-fun-id / make-label-sequence ----------

export const nextJFunId: () => string = (() => {
  let n = 0;
  return (): string => {
    n = n + 1;
    return String(n);
  };
})();

export function makeLabelSequence(init: number): () => JExprT {
  let next = init;
  return (): JExprT => {
    let value: number | undefined = undefined;
    return new JLabel({
      get: (): number => {
        if (value === undefined) {
          value = next;
          next = next + 1;
          return value;
        } else {
          return value;
        }
      }
    });
  };
}

// ---------- data JField ----------

export abstract class JFieldBase {
  abstract get $name(): string;
  abstract printUglySource(printer: UglyPrinter): void;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
  toUglySourcemap(uri: string, line: number, col: number, name: string): CodeWithMap {
    const printer = sourcemapPrinter(uri, line, col, name);
    this.printUglySource(printer.append);
    return printer.get();
  }
  toUglySource(): string {
    const strprint = stringPrinter();
    this.printUglySource(strprint.append);
    return strprint.get();
  }
}

export class JField extends JFieldBase {
  get $name(): 'j-field' { return 'j-field'; }
  constructor(public name: string, public value: JExprT) { super(); }
  visit(visitor: any): any { return visitDispatch(visitor, 'jField', this); }
  label(): string { return 'j-field'; }
  printUglySource(printer: UglyPrinter): void {
    printer(torepr(this.name));
    printer(':');
    this.value.printUglySource(printer);
  }
  tosource(): PP.PPrintDoc {
    return PP.nest(INDENT, PP.dquote(PP.str(this.name)).append(PP.str(': ')).append(this.value.tosource()));
  }
}

export type JFieldT = JField;

export function isJField(x: any): x is JField { return x instanceof JField; }

// ---------- default-map-visitor ----------
// Visitor methods receive the node itself (see CONVENTIONS.md). The Pyret
// source omits methods for j-sourcenode, j-raw-code, and j-block1; this port
// does the same.

export class DefaultMapVisitor {
  // NOTE: the Pyret source reads `j-field(self, name, value.visit(self))`,
  // an arity error if ever invoked; the evident intent is preserved here.
  jField(node: JField): JField { return new JField(node.name, node.value.visit(this)); }
  jParens(node: JParens): JExprT { return new JParens(node.exp.visit(this)); }
  jUnop(node: JUnop): JExprT { return new JUnop(node.exp.visit(this), node.op); }
  jBinop(node: JBinop): JExprT { return new JBinop(node.left.visit(this), node.op, node.right.visit(this)); }
  jFun(node: JFun): JExprT { return new JFun(node.id, node.name, node.args, node.body.visit(this)); }
  jNew(node: JNew): JExprT { return new JNew(node.func.visit(this), node.args.map((a: JExprT) => a.visit(this))); }
  jApp(node: JApp): JExprT { return new JApp(node.func.visit(this), node.args.map((a: JExprT) => a.visit(this))); }
  jMethod(node: JMethod): JExprT { return new JMethod(node.obj.visit(this), node.meth, node.args.map((a: JExprT) => a.visit(this))); }
  jTernary(node: JTernary): JExprT { return new JTernary(node.test.visit(this), node.consq.visit(this), node.altern.visit(this)); }
  jAssign(node: JAssign): JExprT { return new JAssign(node.name, node.rhs.visit(this)); }
  jBracketAssign(node: JBracketAssign): JExprT { return new JBracketAssign(node.obj.visit(this), node.field.visit(this), node.rhs.visit(this)); }
  jDotAssign(node: JDotAssign): JExprT { return new JDotAssign(node.obj.visit(this), node.name, node.rhs.visit(this)); }
  jDot(node: JDot): JExprT { return new JDot(node.obj.visit(this), node.field); }
  jBracket(node: JBracket): JExprT { return new JBracket(node.obj.visit(this), node.field.visit(this)); }
  jList(node: JList): JExprT { return new JList(node.multiLine, node.elts.map((e: JExprT) => e.visit(this))); }
  jObj(node: JObj): JExprT { return new JObj(node.fields.map((f: JFieldT) => f.visit(this))); }
  jId(node: JId): JExprT { return new JId(node.id); }
  jStr(node: JStr): JExprT { return new JStr(node.s); }
  jNum(node: JNum): JExprT { return new JNum(node.n); }
  jTrue(_node: JTrue): JExprT { return jTrue; }
  jFalse(_node: JFalse): JExprT { return jFalse; }
  jNull(_node: JNull): JExprT { return jNull; }
  jUndefined(_node: JUndefined): JExprT { return jUndefined; }
  // NOTE: the Pyret source reads `j-label(label.visit(self))`, but Label
  // is a plain record with no visit method; replicated as written (it
  // fails if ever invoked, exactly as the original would).
  jLabel(node: JLabel): JExprT { return new JLabel((node.label as any).visit(this)); }
  jCase(node: JCase): JCaseT { return new JCase(node.exp.visit(this), node.body.visit(this)); }
  jDefault(node: JDefault): JCaseT { return new JDefault(node.body.visit(this)); }
  jBlock(node: JBlock): JBlockT { return new JBlock(node.stmts.map((s: JStmt) => s.visit(this))); }
  jVar(node: JVar): JStmt { return new JVar(node.name, node.rhs.visit(this)); }
  jIf1(node: JIf1): JStmt { return new JIf1(node.cond.visit(this), node.consq.visit(this)); }
  jIf(node: JIf): JStmt { return new JIf(node.cond.visit(this), node.consq.visit(this), node.alt.visit(this)); }
  jReturn(node: JReturn): JStmt { return new JReturn(node.expr.visit(this)); }
  jTryCatch(node: JTryCatch): JStmt { return new JTryCatch(node.body.visit(this), node.exn, node.catch.visit(this)); }
  jThrow(node: JThrow): JStmt { return new JThrow(node.exp.visit(this)); }
  jExpr(node: JExpr): JStmt { return new JExpr(node.expr.visit(this)); }
  jBreak(_node: JBreak): JStmt { return jBreak; }
  jContinue(_node: JContinue): JStmt { return jContinue; }
  jSwitch(node: JSwitch): JStmt { return new JSwitch(node.exp.visit(this), node.branches.map((b: JCaseT) => b.visit(this))); }
  jWhile(node: JWhile): JStmt { return new JWhile(node.cond.visit(this), node.body.visit(this)); }
  jFor(node: JFor): JStmt { return new JFor(node.createVar, node.init.visit(this), node.cond.visit(this), node.update.visit(this), node.body.visit(this)); }
}

export const defaultMapVisitor: DefaultMapVisitor = new DefaultMapVisitor();
