/*
  Ported from: src/arr/trove/pprint.arr

  A Wadler-style pretty printer. The Pyret `+` operator on docs (_plus)
  becomes the method `append(other)`. Widths/counts are plain JS numbers.
*/

import { InternalCompilerError } from './shared';
import { PyretNumber } from './interop/js-numbers';

export abstract class PPrintDocBase {
  constructor(public flatWidth: number, public hasHardline: boolean) {}
  abstract get $name(): string;
  abstract toString(): string;
  // sharing method _plus
  append(other: PPrintDoc): PPrintDoc {
    const self = this as unknown as PPrintDoc;
    if (isMtDoc(self)) { return other; }
    else if (isMtDoc(other)) { return self; }
    else {
      if (self.hasHardline || other.hasHardline) { return new Concat(self, other, 0, true); }
      else { return new Concat(self, other, self.flatWidth + other.flatWidth, false); }
    }
  }
  pretty(width: number): string[] {
    return format(width, this as unknown as PPrintDoc);
  }
}

export class MtDoc extends PPrintDocBase {
  get $name(): 'mt-doc' { return 'mt-doc'; }
  constructor(flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'EmptyDoc'; }
}

export class Str extends PPrintDocBase {
  get $name(): 'str' { return 'str'; }
  constructor(public s: string, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'Str(' + JSON.stringify(this.s) + ')'; }
}

export class Hardline extends PPrintDocBase {
  get $name(): 'hardline' { return 'hardline'; }
  constructor(flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'CRLF'; }
}

export class Blank extends PPrintDocBase {
  get $name(): 'blank' { return 'blank'; }
  constructor(public n: number, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'Blank(' + String(this.n) + ')'; }
}

export class Concat extends PPrintDocBase {
  get $name(): 'concat' { return 'concat'; }
  constructor(public fst: PPrintDoc, public snd: PPrintDoc, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'Concat(' + this.fst.toString() + ', ' + this.snd.toString() + ')'; }
}

export class Nest extends PPrintDocBase {
  get $name(): 'nest' { return 'nest'; }
  constructor(public indent: number, public d: PPrintDoc, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'Nest(' + String(this.indent) + ', ' + this.d.toString() + ')'; }
}

export class IfFlat extends PPrintDocBase {
  get $name(): 'if-flat' { return 'if-flat'; }
  constructor(public flat: PPrintDoc, public vert: PPrintDoc, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'IfFlat(' + this.flat.toString() + ', ' + this.vert.toString() + ')'; }
}

export class Align extends PPrintDocBase {
  get $name(): 'align' { return 'align'; }
  constructor(public d: PPrintDoc, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'Align(' + this.d.toString() + ')'; }
}

export class AlignSpaces extends PPrintDocBase {
  get $name(): 'align-spaces' { return 'align-spaces'; }
  constructor(public n: number, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'AlignSpaces(' + String(this.n) + ')'; }
}

export class Group extends PPrintDocBase {
  get $name(): 'group' { return 'group'; }
  constructor(public d: PPrintDoc, flatWidth: number, hasHardline: boolean) { super(flatWidth, hasHardline); }
  toString(): string { return 'Group(' + this.d.toString() + ')'; }
}

export type PPrintDoc =
  | MtDoc
  | Str
  | Hardline
  | Blank
  | Concat
  | Nest
  | IfFlat
  | Align
  | AlignSpaces
  | Group;

export function isMtDoc(x: any): x is MtDoc { return x instanceof MtDoc; }
export function isStr(x: any): x is Str { return x instanceof Str; }
export function isHardline(x: any): x is Hardline { return x instanceof Hardline; }
export function isBlank(x: any): x is Blank { return x instanceof Blank; }
export function isConcat(x: any): x is Concat { return x instanceof Concat; }
export function isNest(x: any): x is Nest { return x instanceof Nest; }
export function isIfFlat(x: any): x is IfFlat { return x instanceof IfFlat; }
export function isAlign(x: any): x is Align { return x instanceof Align; }
export function isAlignSpaces(x: any): x is AlignSpaces { return x instanceof AlignSpaces; }
export function isGroup(x: any): x is Group { return x instanceof Group; }

interface Item {
  indent: number;
  isFlat: boolean;
  d: PPrintDoc;
}

// The Pyret original is a tail-recursive `process` over a cons-list of
// items; here it is an explicit loop over a stack (top = head of the
// list), which preserves the exact processing order, including
// collect-concats' left-to-right flattening of concat nodes.
function format(width: number, doc: PPrintDoc): string[] {
  let curLine: string[] = [];
  const output: string[] = [];
  function emitText(s: string): void {
    curLine.push(s);
  }
  function emitSpaces(n: number): void {
    emitText(' '.repeat(n));
  }
  function emitNewline(i: number): void {
    output.push(curLine.join(''));
    curLine = [' '.repeat(i)];
  }

  let column = 0;
  const stack: Item[] = [{ indent: 0, isFlat: false, d: new Group(doc, doc.flatWidth, doc.hasHardline) }];
  while (stack.length > 0) {
    const first = stack.pop()!;
    const i = first.indent;
    const m = first.isFlat;
    const d = first.d;
    switch (d.$name) {
      case 'mt-doc':
        break;
      case 'concat':
        stack.push({ indent: i, isFlat: m, d: d.snd });
        stack.push({ indent: i, isFlat: m, d: d.fst });
        break;
      case 'str':
        emitText(d.s);
        column = column + d.flatWidth;
        break;
      case 'blank':
        emitSpaces(d.n);
        column = column + d.n;
        break;
      case 'align':
        stack.push({ indent: column, isFlat: m, d: d.d });
        break;
      case 'nest':
        stack.push({ indent: i + d.indent, isFlat: m, d: d.d });
        break;
      case 'hardline':
        if (m) {
          throw new InternalCompilerError('Impossible for HardLine to be flat');
        } else {
          emitNewline(i);
          column = i;
        }
        break;
      case 'if-flat':
        stack.push({ indent: i, isFlat: m, d: m ? d.flat : d.vert });
        break;
      case 'align-spaces':
        if (!m) {
          emitSpaces(d.n);
          column = column + d.n;
        }
        break;
      case 'group':
        if (m) { stack.push({ indent: i, isFlat: true, d: d.d }); }
        else if (d.hasHardline) { stack.push({ indent: i, isFlat: false, d: d.d }); }
        else if ((width - column) >= d.flatWidth) {
          // This used to check whether items.rest fits into the remaining space,
          // but that precludes implementing "flowing" text, which is more important.
          // If we need both behaviors, I guess I can add a flow-group...
          stack.push({ indent: i, isFlat: true, d: d.d });
        } else {
          stack.push({ indent: i, isFlat: false, d: d.d });
        }
        break;
      default:
        throw new InternalCompilerError(`format: unknown PPrintDoc ${(d as any).$name}`);
    }
  }
  output.push(curLine.join(''));
  return output;
}

export const mtDoc: PPrintDoc = new MtDoc(0, false);
export const hardline: PPrintDoc = new Hardline(0, true);
export function align(d: PPrintDoc): PPrintDoc { return new Align(d, d.flatWidth, d.hasHardline); }
export function group(d: PPrintDoc): PPrintDoc { return new Group(d, d.flatWidth, d.hasHardline); }
export function ifFlat(flat: PPrintDoc, vert: PPrintDoc): PPrintDoc {
  return new IfFlat(flat, vert, flat.flatWidth, flat.hasHardline);
}
export function nest(n: number, d: PPrintDoc): PPrintDoc { return new Nest(n, d, d.flatWidth, d.hasHardline); }
export function concat(fst: PPrintDoc, snd: PPrintDoc): PPrintDoc { return fst.append(snd); }
export function blank(n: number): PPrintDoc { return new Blank(n, n, false); }
export function str(s: string): PPrintDoc { return new Str(s, s.length, false); }

export function number(n: PyretNumber | number): PPrintDoc { return str(String(n)); }
export const lparen: PPrintDoc = str('(');
export const rparen: PPrintDoc = str(')');
export const lbrace: PPrintDoc = str('{');
export const rbrace: PPrintDoc = str('}');
export const lbrack: PPrintDoc = str('[');
export const rbrack: PPrintDoc = str(']');
export const langle: PPrintDoc = str('<');
export const rangle: PPrintDoc = str('>');
export const comma: PPrintDoc = str(',');
export const semi: PPrintDoc = str(';');

export function sbreak(n: number): PPrintDoc { return ifFlat(blank(n), hardline); }
export const commabreak: PPrintDoc = comma.append(sbreak(1));
export const semibreak: PPrintDoc = semi.append(sbreak(1));

export function flowMap<T>(sep: PPrintDoc, f: (x: T) => PPrintDoc, items: T[]): PPrintDoc {
  let acc: PPrintDoc = mtDoc;
  for (const item of items) {
    if (isMtDoc(acc)) { acc = f(item); }
    else { acc = acc.append(group(sep.append(f(item)))); }
  }
  return acc;
}
export function flow(items: PPrintDoc[]): PPrintDoc { return flowMap(sbreak(1), (x: PPrintDoc) => x, items); }
export function vert(items: PPrintDoc[]): PPrintDoc { return flowMap(hardline, (x: PPrintDoc) => x, items); }
export function parens(d: PPrintDoc): PPrintDoc { return group(lparen.append(d).append(rparen)); }
export function braces(d: PPrintDoc): PPrintDoc { return group(lbrace.append(d).append(rbrace)); }
export function brackets(d: PPrintDoc): PPrintDoc { return group(lbrack.append(d).append(rbrack)); }
const strSquote: PPrintDoc = str("'");
const strDquote: PPrintDoc = str('"');
export function dquote(s: PPrintDoc): PPrintDoc { return group(strDquote.append(s).append(strDquote)); }
export function squote(s: PPrintDoc): PPrintDoc { return group(strSquote.append(s).append(strSquote)); }

export function hang(i: number, d: PPrintDoc): PPrintDoc { return align(nest(i, d)); }
export function prefix(n: number, b: number, x: PPrintDoc, y: PPrintDoc): PPrintDoc {
  return group(x.append(nest(n, sbreak(b).append(y))));
}
export function infix(n: number, b: number, op: PPrintDoc, x: PPrintDoc, y: PPrintDoc): PPrintDoc {
  return prefix(n, b, x.append(blank(b)).append(op), y);
}
export function infixBreak(n: number, b: number, op: PPrintDoc, x: PPrintDoc, y: PPrintDoc): PPrintDoc {
  return prefix(n, b, x, op.append(blank(b)).append(y));
}
export function surround(n: number, b: number, open: PPrintDoc, contents: PPrintDoc, close: PPrintDoc): PPrintDoc {
  if (isMtDoc(close)) { return group(open.append(nest(n, sbreak(b).append(contents)))); }
  else { return group(open.append(nest(n, sbreak(b).append(contents))).append(sbreak(b)).append(close)); }
}
export function softSurround(n: number, b: number, open: PPrintDoc, contents: PPrintDoc, close: PPrintDoc): PPrintDoc {
  if (isMtDoc(close)) { return group(open.append(nest(n, group(sbreak(b).append(contents))))); }
  else { return group(open.append(nest(n, group(sbreak(b).append(contents)))).append(group(sbreak(b).append(close)))); }
}

export function separate(sep: PPrintDoc, docs: PPrintDoc[]): PPrintDoc {
  let acc: PPrintDoc = mtDoc;
  for (const d of docs) {
    if (isMtDoc(d)) { /* acc unchanged */ }
    else if (isMtDoc(acc)) { acc = d; }
    else { acc = acc.append(sep).append(d); }
  }
  return acc;
}
export function surroundSeparate(n: number, b: number, voidDoc: PPrintDoc, open: PPrintDoc, sep: PPrintDoc, close: PPrintDoc, docs: PPrintDoc[]): PPrintDoc {
  if (docs.length === 0) { return voidDoc; }
  else { return surround(n, b, open, separate(sep, docs), close); }
}

export function labelAlignSurround(label: PPrintDoc, open: PPrintDoc, sep: PPrintDoc, contents: PPrintDoc[], close: PPrintDoc): PPrintDoc {
  return group(label.append(align(open.append(align(separate(sep, contents))).append(group(sbreak(0).append(close))))));
}
