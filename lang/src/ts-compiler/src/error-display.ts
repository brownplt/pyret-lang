/*
  Ported from: src/arr/trove/error-display.arr
*/

import * as S from './srcloc';

export abstract class ErrorDisplayBase {
  abstract get $name(): string;
}

export class Paragraph extends ErrorDisplayBase {
  get $name(): 'paragraph' { return 'paragraph'; }
  constructor(public contents: ErrorDisplay[]) { super(); }
}

export class BulletedSequence extends ErrorDisplayBase {
  get $name(): 'bulleted-sequence' { return 'bulleted-sequence'; }
  constructor(public contents: ErrorDisplay[]) { super(); }
}

export class VSequence extends ErrorDisplayBase {
  get $name(): 'v-sequence' { return 'v-sequence'; }
  constructor(public contents: ErrorDisplay[]) { super(); }
}

export class HSequence extends ErrorDisplayBase {
  get $name(): 'h-sequence' { return 'h-sequence'; }
  constructor(public contents: ErrorDisplay[], public sep: string) { super(); }
}

export class HSequenceSep extends ErrorDisplayBase {
  get $name(): 'h-sequence-sep' { return 'h-sequence-sep'; }
  constructor(public contents: ErrorDisplay[], public sep: string, public last: string) { super(); }
}

export class Embed extends ErrorDisplayBase {
  get $name(): 'embed' { return 'embed'; }
  constructor(public val: any) { super(); }
}

export class Text extends ErrorDisplayBase {
  get $name(): 'text' { return 'text'; }
  constructor(public str: string) { super(); }
}

export class Loc extends ErrorDisplayBase {
  get $name(): 'loc' { return 'loc'; }
  constructor(public loc: S.Loc) { super(); }
}

export class MaybeStackLoc extends ErrorDisplayBase {
  get $name(): 'maybe-stack-loc' { return 'maybe-stack-loc'; }
  constructor(
    public n: number,
    public userFramesOnly: boolean,
    public contentsWithLoc: (l: S.Loc) => ErrorDisplay,
    public contentsWithoutLoc: ErrorDisplay
  ) { super(); }
}

export class Code extends ErrorDisplayBase {
  get $name(): 'code' { return 'code'; }
  constructor(public contents: ErrorDisplay) { super(); }
}

export class Cmcode extends ErrorDisplayBase {
  get $name(): 'cmcode' { return 'cmcode'; }
  constructor(public loc: S.Loc) { super(); }
}

export class LocDisplay extends ErrorDisplayBase {
  get $name(): 'loc-display' { return 'loc-display'; }
  constructor(public loc: S.Loc, public style: string, public contents: ErrorDisplay) { super(); }
}

export class Optional extends ErrorDisplayBase {
  get $name(): 'optional' { return 'optional'; }
  constructor(public contents: ErrorDisplay) { super(); }
}

export class Highlight extends ErrorDisplayBase {
  get $name(): 'highlight' { return 'highlight'; }
  constructor(public contents: ErrorDisplay, public locs: S.Loc[], public color: number) { super(); }
}

export type ErrorDisplay =
  | Paragraph
  | BulletedSequence
  | VSequence
  | HSequence
  | HSequenceSep
  | Embed
  | Text
  | Loc
  | MaybeStackLoc
  | Code
  | Cmcode
  | LocDisplay
  | Optional
  | Highlight;

export function isParagraph(x: any): x is Paragraph { return x instanceof Paragraph; }
export function isBulletedSequence(x: any): x is BulletedSequence { return x instanceof BulletedSequence; }
export function isVSequence(x: any): x is VSequence { return x instanceof VSequence; }
export function isHSequence(x: any): x is HSequence { return x instanceof HSequence; }
export function isHSequenceSep(x: any): x is HSequenceSep { return x instanceof HSequenceSep; }
export function isEmbed(x: any): x is Embed { return x instanceof Embed; }
export function isText(x: any): x is Text { return x instanceof Text; }
export function isLoc(x: any): x is Loc { return x instanceof Loc; }
export function isMaybeStackLoc(x: any): x is MaybeStackLoc { return x instanceof MaybeStackLoc; }
export function isCode(x: any): x is Code { return x instanceof Code; }
export function isCmcode(x: any): x is Cmcode { return x instanceof Cmcode; }
export function isLocDisplay(x: any): x is LocDisplay { return x instanceof LocDisplay; }
export function isOptional(x: any): x is Optional { return x instanceof Optional; }
export function isHighlight(x: any): x is Highlight { return x instanceof Highlight; }

// Constructor-function forms of the variants (Pyret exposes the lowercase
// constructors directly).
export function paragraph(contents: ErrorDisplay[]): Paragraph { return new Paragraph(contents); }
export function bulletedSequence(contents: ErrorDisplay[]): BulletedSequence { return new BulletedSequence(contents); }
export function vSequence(contents: ErrorDisplay[]): VSequence { return new VSequence(contents); }
export function hSequence(contents: ErrorDisplay[], sep: string): HSequence { return new HSequence(contents, sep); }
export function hSequenceSep(contents: ErrorDisplay[], sep: string, last: string): HSequenceSep { return new HSequenceSep(contents, sep, last); }
export function embed(val: any): Embed { return new Embed(val); }
export function text(str: string): Text { return new Text(str); }
export function loc(l: S.Loc): Loc { return new Loc(l); }
export function maybeStackLoc(n: number, userFramesOnly: boolean, contentsWithLoc: (l: S.Loc) => ErrorDisplay, contentsWithoutLoc: ErrorDisplay): MaybeStackLoc {
  return new MaybeStackLoc(n, userFramesOnly, contentsWithLoc, contentsWithoutLoc);
}
export function code(contents: ErrorDisplay): Code { return new Code(contents); }
export function cmcode(loc: S.Loc): Cmcode { return new Cmcode(loc); }
export function locDisplay(loc: S.Loc, style: string, contents: ErrorDisplay): LocDisplay { return new LocDisplay(loc, style, contents); }
export function optional(contents: ErrorDisplay): Optional { return new Optional(contents); }
export function highlight(contents: ErrorDisplay, locs: S.Loc[], color: number): Highlight { return new Highlight(contents, locs, color); }

// Construction helpers: Pyret's `[locs: ...]`, `[para: ...]`, etc. become
// variadic functions here.
export function locs(...ls: S.Loc[]): S.Loc[] { return ls; }
export function para(...contents: ErrorDisplay[]): Paragraph { return new Paragraph(contents); }
export function sequence(...contents: ErrorDisplay[]): HSequence { return new HSequence(contents, ' '); }
export function vert(...contents: ErrorDisplay[]): VSequence { return new VSequence(contents); }
export const error = sequence;
export function paraNospace(...contents: ErrorDisplay[]): HSequence { return new HSequence(contents, ''); }
export function bulleted(...contents: ErrorDisplay[]): BulletedSequence { return new BulletedSequence(contents); }
export function opt(...contents: ErrorDisplay[]): Optional { return new Optional(new VSequence(contents)); }

export function edArgs(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' argument' : ' arguments'));
}

export function edNames(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' name' : ' names'));
}

export function edFields(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' field' : ' fields'));
}

export function edFieldBindings(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' field binding' : ' field bindings'));
}

export function edBindings(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' binding' : ' bindings'));
}

export function edParams(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' parameter' : ' parameters'));
}

export function edComponents(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' component' : ' components'));
}

export function edRows(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' row' : ' rows'));
}

export function edValues(n: number): ErrorDisplay {
  return sequence(
    new Embed(n),
    new Text(n === 1 ? ' value' : ' values'));
}

export function edNth(n: number): Text {
  // num-modulo is Euclidean modulo
  const lastDigit = ((n % 10) + 10) % 10;
  let suffix: string;
  if (lastDigit === 1) { suffix = 'ˢᵗ'; }
  else if (lastDigit === 2) { suffix = 'ⁿᵈ'; }
  else if (lastDigit === 3) { suffix = 'ⁿᵈ'; }
  else { suffix = 'ᵗʰ'; }
  return new Text(String(n) + suffix);
}
