/*
  Port of src/js/trove/parse-pyret.js.

  Tokenizes + parses Pyret source using the reused AMD modules
  `pyret-base/js/pyret-tokenizer` and `pyret-base/js/pyret-parser`
  (loaded through interop/amd.ts), then walks the parse tree building
  plain TS AST values (src/ts-compiler/src/ast.ts) instead of
  Pyret-runtime values.

  Parse errors that the JS module reported through
  RUNTIME.ffi.throwParseError* are thrown as PyretParseError subclasses
  carrying the same information, so the CLI can render the same
  messages later.
*/

import * as A from './ast';
import * as ED from './error-display';
import { Srcloc } from './srcloc';
import { jsnums, PyretNumber } from './interop/js-numbers';
import { amdRequire } from './interop/amd';
import { Either, left, right } from './shared';

// ---------- jglr token / parse-node shapes ----------

// SrcLoc from lib/jglr/rnglr.js
export interface TokenPos {
  startRow: number;
  startCol: number;
  startChar: number;
  endRow: number;
  endCol: number;
  endChar: number;
  combine(other: TokenPos): TokenPos;
  toString(showSpan?: boolean): string;
}

export interface ParseNode {
  name: string;
  kids: ParseNode[];
  value?: string;
  pos: TokenPos;
  toRepr?(showVal?: boolean): string;
  toString?(showVal?: boolean): string;
}

// The tokenizer/parser are loaded lazily so that importing this module
// does not fail before amd.setModulePath has had a chance to run (the
// pyret-parser is generated at build time).
let tokenizerMod: any = undefined;
let parserMod: any = undefined;
function getTokenizer(): any {
  if (tokenizerMod === undefined) tokenizerMod = amdRequire('pyret-base/js/pyret-tokenizer');
  return tokenizerMod;
}
function getParser(): any {
  if (parserMod === undefined) parserMod = amdRequire('pyret-base/js/pyret-parser');
  return parserMod;
}

// ---------- srcloc construction (RUNTIME.ffi.makePyretPos / combinePyretPos) ----------

function makePyretPos(fileName: string, p: TokenPos): Srcloc {
  return new Srcloc(fileName, p.startRow, p.startCol, p.startChar, p.endRow, p.endCol, p.endChar);
}

function combinePyretPos(fileName: string, p1: TokenPos, p2: TokenPos): Srcloc {
  return new Srcloc(fileName, p1.startRow, p1.startCol, p1.startChar, p2.endRow, p2.endCol, p2.endChar);
}

// ---------- parse errors (RUNTIME.ffi.throwParseError*) ----------

export type ParseErrorKind =
  | 'parse-error-next-token'
  | 'parse-error-eof'
  | 'parse-error-unterminated-string'
  | 'parse-error-bad-number'
  | 'parse-error-bad-operator'
  | 'parse-error-bad-check-operator'
  | 'parse-error-colon-colon'
  | 'parse-error-bad-app'
  | 'parse-error-bad-fun-header'
  | 'could-not-create-number';

export class PyretParseError extends Error {
  constructor(
    public readonly kind: ParseErrorKind,
    public readonly loc: Srcloc | undefined,
    message: string
  ) {
    super(message);
    this.name = 'PyretParseError';
  }
  // error.arr's render-reason for the corresponding ParseError variant.
  // The Pyret-hosted CLI renders uncaught parse errors through these (via
  // render-error-display), so the top-level handler in pyret.ts must too
  // for byte-identical diagnostics. Subclasses override; this fallback
  // covers any kind without a ported renderer.
  renderReason(): ED.ErrorDisplay {
    return ED.para(ED.text(this.message));
  }
}

function locStr(loc: Srcloc): string {
  return `${loc.source}, ${loc.startLine}:${loc.startColumn}-${loc.endLine}:${loc.endColumn}`;
}

// error.arr's draw-and-highlight
function drawAndHighlight(l: Srcloc): ED.ErrorDisplay {
  return ED.locDisplay(l, "error-highlight", ED.loc(l));
}

export class ParseErrorNextToken extends PyretParseError {
  constructor(loc: Srcloc, public readonly nextToken: string) {
    super('parse-error-next-token', loc, `parse error around ${JSON.stringify(nextToken)} at ${locStr(loc)}`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(
      ED.para(ED.text("Pyret didn't understand your program around "), drawAndHighlight(this.loc!)),
      ED.para(ED.text("You may need to add or remove some text to fix your program.")),
      ED.para(ED.text("Look carefully before the highlighted text.")),
      ED.para(ED.text("Is there a missing colon ("), ED.code(ED.text(":")),
        ED.text("), comma ("), ED.code(ED.text(",")),
        ED.text("), string marker ("), ED.code(ED.text("\"")),
        ED.text("), or keyword?")),
      ED.para(ED.text("Is there something there that shouldn’t be?")));
  }
}
export class ParseErrorEOF extends PyretParseError {
  constructor(loc: Srcloc) {
    super('parse-error-eof', loc, `parse error at end of file at ${locStr(loc)}`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(ED.para(
      ED.text("Pyret didn't understand the very end of your program."),
      ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end.")));
  }
}
export class ParseErrorUnterminatedString extends PyretParseError {
  constructor(loc: Srcloc) {
    super('parse-error-unterminated-string', loc, `unterminated string at ${locStr(loc)}`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(ED.paraNospace(
      ED.text("Pyret thinks your program has an incomplete string literal around "),
      drawAndHighlight(this.loc!),
      ED.text("; you may be missing closing punctuation.")));
  }
}
export class ParseErrorBadNumber extends PyretParseError {
  constructor(loc: Srcloc) {
    super('parse-error-bad-number', loc, `bad number at ${locStr(loc)}`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(ED.paraNospace(
      ED.text("Pyret thinks your program probably has a number at "),
      drawAndHighlight(this.loc!),
      ED.text("; number literals in Pyret require at least one digit before the decimal point.")));
  }
}
export class ParseErrorBadOper extends PyretParseError {
  constructor(loc: Srcloc) {
    super('parse-error-bad-operator', loc, `bad operator at ${locStr(loc)}`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(ED.paraNospace(
      ED.text("The operator at "),
      drawAndHighlight(this.loc!),
      ED.text(" has no surrounding whitespace.")));
  }
}
export class ParseErrorBadCheckOper extends PyretParseError {
  // The ffi passed the constructed CheckOp (which carries its loc).
  constructor(public readonly op: any) {
    super('parse-error-bad-check-operator', op && op.l, `bad check operator at ${op && op.l ? locStr(op.l) : '<unknown>'}`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(
      ED.paraNospace(
        ED.text("The testing operator at "),
        drawAndHighlight(this.op.l),
        ED.text(" must be used inside a"),
        ED.code(ED.text("check")), ED.text(" or "), ED.code(ED.text("where")), ED.text(" block.")),
      ED.para(
        ED.text("Did you mean to use one of the comparison operators instead?")));
  }
}
export class ParseErrorColonColon extends PyretParseError {
  // NOTE: the JS ffi signature accepts a nextToken argument but drops it.
  constructor(loc: Srcloc, public readonly nextToken?: string) {
    super('parse-error-colon-colon', loc, `unexpected :: at ${locStr(loc)}`);
  }
  renderReason(): ED.ErrorDisplay {
    // NOTE: the loc sits at the error's top level (between the paras), not
    // inside one -- error.arr's structure, preserved for byte parity.
    return ED.error(
      ED.para(ED.text("Pyret didn't understand your program around ")),
      drawAndHighlight(this.loc!),
      ED.para(ED.text(" If you were trying to write a type annotation (with "), ED.code(ED.text("::")),
        ED.text("), remember that annotations only apply directly to names.  "),
        ED.text("If you were not trying to write an annotation, perhaps use a single colon instead.")));
  }
}
export class ParseErrorBadApp extends PyretParseError {
  constructor(public readonly funLoc: Srcloc, public readonly argsLoc: Srcloc) {
    super('parse-error-bad-app', funLoc, `bad application at ${locStr(funLoc)} (arguments at ${locStr(argsLoc)})`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(ED.para(
      ED.text("Pyret thinks the code at "), ED.loc(this.funLoc.plus(this.argsLoc)),
      ED.text(" is probably a function call, but there should be no space"),
      ED.text(" between the function and its arguments.")));
  }
}
export class ParseErrorBadFunHeader extends PyretParseError {
  constructor(public readonly funLoc: Srcloc, public readonly argsLoc: Srcloc) {
    super('parse-error-bad-fun-header', funLoc, `bad function header at ${locStr(funLoc)} (arguments at ${locStr(argsLoc)})`);
  }
  renderReason(): ED.ErrorDisplay {
    return ED.error(ED.para(
      ED.text("Pyret thinks the code at "), ED.loc(this.funLoc.plus(this.argsLoc)),
      ED.text(" is probably a function header, but there should be no space"),
      ED.text(" between the arguments.")));
  }
}

function throwParseErrorNextToken(loc: Srcloc, nextToken: string): never {
  throw new ParseErrorNextToken(loc, nextToken);
}
function throwParseErrorEOF(loc: Srcloc): never {
  throw new ParseErrorEOF(loc);
}
function throwParseErrorUnterminatedString(loc: Srcloc): never {
  throw new ParseErrorUnterminatedString(loc);
}
function throwParseErrorBadNumber(loc: Srcloc): never {
  throw new ParseErrorBadNumber(loc);
}
function throwParseErrorBadOper(loc: Srcloc): never {
  throw new ParseErrorBadOper(loc);
}
function throwParseErrorBadCheckOper(op: any): never {
  throw new ParseErrorBadCheckOper(op);
}
function throwParseErrorColonColon(loc: Srcloc): never {
  throw new ParseErrorColonColon(loc);
}
function throwParseErrorBadApp(funLoc: Srcloc, argsLoc: Srcloc): never {
  throw new ParseErrorBadApp(funLoc, argsLoc);
}
function throwParseErrorBadFunHeader(funLoc: Srcloc, argsLoc: Srcloc): never {
  throw new ParseErrorBadFunHeader(funLoc, argsLoc);
}

// RUNTIME.makeNumberFromString: jsnums.fromString, throwing on failure
// (the runtime throws a message-exception with exactly this text).
function makeNumberFromString(s: string): PyretNumber {
  const result = jsnums.fromString(s);
  if (result === false) {
    throw new PyretParseError('could-not-create-number', undefined, 'Could not create number from: ' + s);
  }
  return result;
}

// ---------- signed-number-as-stmt whitespace check ----------

function isSignedNumberAsStmt(stmt: ParseNode): boolean {
  let node = stmt;
  if (node.name !== 'stmt') return false; node = node.kids[0];
  if (node.name !== 'check-test') return false; node = node.kids[0];
  if (node.name !== 'binop-expr') return false; node = node.kids[0];
  if (node.name !== 'expr') return false; node = node.kids[0];
  if (node.name !== 'prim-expr') return false; node = node.kids[0];
  if (node.name !== 'num-expr') return false; node = node.kids[0];
  if (node.name !== 'NUMBER') return false;
  return node.value![0] === '-' || node.value![0] === '+';
}

function detectAndComplainAboutOperatorWhitespace(stmts: ParseNode[], fileName: string): void {
  for (let i = 1; i < stmts.length; i++) {
    if (isSignedNumberAsStmt(stmts[i]) &&
        stmts[i].pos.startRow === stmts[i - 1].pos.endRow) {
      const pos = stmts[i].pos;
      throwParseErrorBadOper(
        new Srcloc(fileName,
                   pos.startRow, pos.startCol, pos.startChar,
                   pos.startRow, pos.startCol + 1, pos.startChar + 1));
    }
  }
}

// ---------- operator lookup ----------

const opLookup: { [op: string]: string | ((l: Srcloc) => any) } = {
  '+': 'op+',
  '-': 'op-',
  '*': 'op*',
  '/': 'op/',
  '$': 'op^',
  '^': 'op^',
  '<=': 'op<=',
  '<': 'op<',
  '>=': 'op>=',
  '>': 'op>',
  '==': 'op==',
  '=~': 'op=~',
  '<=>': 'op<=>',
  '<>': 'op<>',
  'and': 'opand',
  'or': 'opor',

  'is': (l: Srcloc) => new A.SOpIs(l),
  'is-roughly': (l: Srcloc) => new A.SOpIsRoughly(l),
  'is==': (l: Srcloc) => new A.SOpIsOp(l, 'op=='),
  'is=~': (l: Srcloc) => new A.SOpIsOp(l, 'op=~'),
  'is<=>': (l: Srcloc) => new A.SOpIsOp(l, 'op<=>'),
  'is-not': (l: Srcloc) => new A.SOpIsNot(l),
  'is-not-roughly': (l: Srcloc) => new A.SOpIsNotRoughly(l),
  'is-not==': (l: Srcloc) => new A.SOpIsNotOp(l, 'op=='),
  'is-not=~': (l: Srcloc) => new A.SOpIsNotOp(l, 'op=~'),
  'is-not<=>': (l: Srcloc) => new A.SOpIsNotOp(l, 'op<=>'),
  'satisfies': (l: Srcloc) => new A.SOpSatisfies(l),
  'violates': (l: Srcloc) => new A.SOpSatisfiesNot(l),
  'raises': (l: Srcloc) => new A.SOpRaises(l),
  'raises-other-than': (l: Srcloc) => new A.SOpRaisesOther(l),
  'does-not-raise': (l: Srcloc) => new A.SOpRaisesNot(l),
  'raises-satisfies': (l: Srcloc) => new A.SOpRaisesSatisfies(l),
  'raises-violates': (l: Srcloc) => new A.SOpRaisesViolates(l),
};

// ---------- the parse-tree -> AST translation ----------

function translate(node: ParseNode, fileName: string): A.Program {
  // NOTE: This translation could blow the stack for very deep ASTs
  // We might have to rewrite the whole algorithm
  // One possibility is to reuse a stack of {todo: [...], done: [...], doing: fn} nodes
  // where each AST kid that needs to be recursively processed pushes a new frame on the stack
  // (it can eagerly process any primitive values, and defer the rest),
  // and returns a function to be called when all the new todos are done (which gets put into doing)
  // if a todo item is a Pyret value, it just gets pushed across to done
  // if a todo item is an array, then doing = RUNTIME.makeList and it creates a stack frame
  function tr(node: ParseNode): any {
    if (translators[node.name] === undefined)
      throw new Error('Cannot find ' + node.name + ' in translators');
    return translators[node.name](node);
  }

  function nameSpec(node: ParseNode, Constructor: new (l: Srcloc, nameSpec: any) => any): any {
    return new Constructor(pos(node.pos), tr(node.kids[0]));
  }
  function typeSpec(node: ParseNode, Constructor: new (l: Srcloc, nameSpec: any) => any): any {
    return new Constructor(pos(node.pos), tr(node.kids[1]));
  }
  function dataSpec(node: ParseNode, Constructor: new (l: Srcloc, nameSpec: any, hidings: any[]) => any): any {
    let hidings;
    node.kids[1].name = 'name-spec';
    if (node.kids.length === 2) {
      hidings = makeListTr([]);
    }
    else {
      hidings = tr(node.kids[2]);
    }
    return new Constructor(pos(node.pos), tr(node.kids[1]), hidings);
  }
  function moduleSpec(node: ParseNode, Constructor: new (l: Srcloc, nameSpec: any) => any): any {
    return new Constructor(pos(node.pos), tr(node.kids[1]));
  }

  const pos = function(p: TokenPos): Srcloc { return makePyretPos(fileName, p); };
  const pos2 = function(p1: TokenPos, p2: TokenPos): Srcloc { return combinePyretPos(fileName, p1, p2); };
  function makeListTr(arr: ParseNode[], start?: number, end?: number, onto?: any[], f?: (n: ParseNode) => any): any[] {
    start = start || 0;
    end = end || arr.length;
    f = f || tr;
    // The JS version builds the list back-to-front; preserve that
    // evaluation order.
    const acc: any[] = [];
    for (let i = end - 1; i >= start; i--)
      acc.push(f(arr[i]));
    acc.reverse();
    return onto ? acc.concat(onto) : acc;
  }
  function makeListComma(arr: ParseNode[], start?: number, end?: number, f?: (n: ParseNode) => any): any[] {
    start = start || 0;
    end = end || arr.length;
    f = f || tr;
    const acc: any[] = [];
    for (let i = end - 1; i >= start; i -= 2)
      acc.push(f(arr[i]));
    acc.reverse();
    return acc;
  }
  function makeList(arr: any[], start?: number, end?: number, onto?: any[]): any[] {
    start = start || 0;
    end = end || arr.length;
    const acc = arr.slice(start, end);
    return onto ? acc.concat(onto) : acc;
  }
  function name(tok: ParseNode): any {
    if (tok.value === '_')
      return new A.SUnderscore(pos(tok.pos));
    else
      return new A.SName(pos(tok.pos), tok.value!);
  }
  function symbol(tok: ParseNode): string {
    return tok.value!;
  }
  function string(tok: ParseNode): string {
    if (tok.value!.substring(0, 3) === '```')
      return tok.value!.slice(3, -3).trim();
    else
      return tok.value!.slice(1, -1);
  }
  function number(tok: ParseNode): PyretNumber {
    return makeNumberFromString(tok.value!);
  }
  const translators: { [name: string]: (node: ParseNode) => any } = {
    'program': function(node) {
      const prelude = tr(node.kids[0]);
      const body = tr(node.kids[1]);
      return new A.SProgram(pos(node.pos), prelude.use, prelude.provides, prelude.provideTypes, prelude.allProvides, prelude.imports, body);
    },
    'prelude': function(node) {
      let use: any = undefined;
      let provides: any = undefined;
      let provideTypes: any = undefined;
      const allProvides: ParseNode[] = [];
      const imports: ParseNode[] = [];

      node.kids.forEach(function(kid) {
        if (kid.name === 'use-stmt') {
          use = tr(kid);
        }
        else if (provideTypes === undefined && kid.kids[0].name === 'provide-types-stmt') {
          provideTypes = tr(kid);
        }
        else if (provides === undefined && kid.kids[0].name === 'provide-vals-stmt') {
          provides = tr(kid);
        }
        else if (kid.kids[0].name === 'provide-block') {
          allProvides.push(kid);
        }
        else if (kid.kids[0].name === 'INCLUDE' || kid.kids[0].name === 'IMPORT') {
          imports.push(kid);
        }
      });
      if (provides === undefined) {
        provides = new A.SProvideNone(pos(node.pos));
      }
      if (provideTypes === undefined) {
        provideTypes = new A.SProvideTypesNone(pos(node.pos));
      }
      return {
        use: use,
        provides: provides,
        provideTypes: provideTypes,
        allProvides: makeListTr(allProvides),
        imports: makeListTr(imports)
      };
    },
    'use-stmt': function(node) {
      return new A.SUse(pos(node.pos), name(node.kids[1]), tr(node.kids[2]));
    },
    'include-spec': function(node) {
      return tr(node.kids[0]);
    },
    'include-data-spec': function(node) {
      return dataSpec(node, A.SIncludeData);
    },
    'include-type-spec': function(node) {
      return typeSpec(node, A.SIncludeType);
    },
    'include-name-spec': function(node) {
      return nameSpec(node, A.SIncludeName);
    },
    'include-module-spec': function(node) {
      return moduleSpec(node, A.SIncludeModule);
    },
    'hiding-spec': function(node) {
      return makeListComma(node.kids, 2, node.kids.length - 1, name);
    },
    'module-ref': function(node) {
      return makeListComma(node.kids, 0, node.kids.length, name);
    },
    'name-spec': function(node) {
      if (node.kids[0].name === 'STAR' || node.kids[0].name === 'TIMES') {
        if (node.kids.length === 1) {
          return new A.SStar(pos(node.pos), makeListTr([]));
        }
        else {
          return new A.SStar(pos(node.pos), tr(node.kids[1]));
        }
      }
      else if (node.kids.length === 1) {
        return new A.SModuleRef(pos(node.pos), tr(node.kids[0]), undefined);
      }
      else {
        return new A.SModuleRef(pos(node.pos), tr(node.kids[0]), name(node.kids[2]));
      }
    },
    'provide-spec': function(node) {
      return tr(node.kids[0]);
    },
    'provide-name-spec': function(node) {
      return nameSpec(node, A.SProvideName);
    },
    'provide-data-spec': function(node) {
      return dataSpec(node, A.SProvideData);
    },
    'provide-type-spec': function(node) {
      return typeSpec(node, A.SProvideType);
    },
    'provide-module-spec': function(node) {
      return moduleSpec(node, A.SProvideModule);
    },
    'provide-stmt': function(node) {
      return tr(node.kids[0]);
    },
    'provide-block': function(node) {
      let skippedLast = 1;
      if (node.kids[node.kids.length - 2].name === 'COMMA') skippedLast = 2;
      if (node.kids[0].name === 'PROVIDECOLON') {
        return new A.SProvideBlock(
          pos(node.pos),
          makeListTr([]),
          makeListComma(node.kids, 1, node.kids.length - skippedLast));
      }
      else {
        return new A.SProvideBlock(
          pos(node.pos),
          tr(node.kids[2]),
          makeListComma(node.kids, 4, node.kids.length - skippedLast));
      }
    },
    'provide-vals-stmt': function(node) {
      if (node.kids.length === 2) {
        // (provide-stmt PROVIDE STAR)
        return new A.SProvideAll(pos(node.pos));
      } else {
        // (provide-stmt PROVIDE stmt END)
        return new A.SProvide(pos(node.pos), tr(node.kids[1]));
      }
    },
    'provide-types-stmt': function(node) {
      if (node.kids[1].name === 'STAR' || node.kids[1].name === 'TIMES') {
        return new A.SProvideTypesAll(pos(node.pos));
      } else {
        // will produce record-ann
        const rec = tr(node.kids[1]);
        // Get the fields out of it
        return new A.SProvideTypes(pos(node.pos), rec.fields);
      }
    },
    'import-stmt': function(node) {
      if (node.kids[node.kids.length - 2].name === 'AS') {
        if (node.kids.length === 4) {
          // (import-stmt IMPORT import-source AS NAME)
          return new A.SImport(pos(node.pos), tr(node.kids[1]), name(node.kids[3]));
        } else {
          // (import-stmt IMPORT import-source AS NAME, TYPES)
          return new A.SImportTypes(pos(node.pos), tr(node.kids[1]), name(node.kids[3]), name(node.kids[5]));
        }
      } else if (node.kids[0].name === 'INCLUDE' && node.kids[1].name === 'FROM') {
        let skippedLast = 1;
        if (node.kids[node.kids.length - 2].name === 'COMMA') skippedLast++;
        return new A.SIncludeFrom(pos(node.pos),
          tr(node.kids[2]),
          makeListComma(node.kids, 4, node.kids.length - skippedLast));
      } else if (node.kids[0].name === 'INCLUDE' && node.kids[1].name !== 'FROM') {
        // (import-stmt INCLUDE import-source)
        return new A.SInclude(pos(node.pos), tr(node.kids[1]));
      } else {
        // (import-stmt IMPORT comma-names FROM mod)
        return new A.SImportFields(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
      }
    },
    'import-source': function(node) {
      return tr(node.kids[0]);
    },
    // (import-special NAME LPAREN STRING (COMMA STRING)* RPAREN)
    'import-special': function(node) {
      return new A.SSpecialImport(pos(node.pos), symbol(node.kids[0]),
        makeListComma(node.kids, 2, node.kids.length - 1, string));
    },
    'import-name': function(node) {
      // (import-name NAME)
      return new A.SConstImport(pos(node.pos), symbol(node.kids[0]));
    },
    'block': function(node) {
      // (block stmts ...)
      detectAndComplainAboutOperatorWhitespace(node.kids, fileName);
      return new A.SBlock(pos(node.pos), makeListTr(node.kids));
    },
    'stmt': function(node) {
      // (stmt s)
      return tr(node.kids[0]);
    },
    'spy-stmt': function(node) {
      // (spy [label] COLON contents END)
      let label, contents;
      if (node.kids[1].name === 'binop-expr') {
        label = tr(node.kids[1]);
      } else {
        label = undefined;
      }
      if (node.kids[node.kids.length - 2].name === 'COLON') {
        contents = [];
      } else {
        contents = tr(node.kids[node.kids.length - 2]);
      }
      return new A.SSpyBlock(pos(node.pos), label, contents);
    },
    'spy-contents': function(node) {
      return makeListComma(node.kids);
    },
    'spy-field': function(node) {
      if (node.kids.length === 1) {
        return new A.SSpyExpr(pos(node.pos), symbol(node.kids[0].kids[0]), tr(node.kids[0]), true);
      } else {
        return new A.SSpyExpr(pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]), false);
      }
    },
    'data-with': function(node) {
      if (node.kids.length === 0) {
        // (data-with)
        return makeList([]);
      } else {
        // (data-with WITH fields)
        return tr(node.kids[1]);
      }
    },
    'variant-constructor': function(node) {
      // (variant-constructor NAME variant-members)
      return {
        pos: pos(node.pos),
        name: symbol(node.kids[0]),
        args: tr(node.kids[1])
      };
    },
    'data-variant': function(node) {
      if (node.kids[1].value !== undefined) {
        // (data-variant PIPE NAME with)
        return new A.SSingletonVariant(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]));
      } else {
        // (data-variant PIPE variant-constructor with)
        const constr = tr(node.kids[1]);
        return new A.SVariant(pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[2]));
      }
    },
    'first-data-variant': function(node) {
      if (node.kids[0].value !== undefined) {
        // (first-data-variant NAME with)
        return new A.SSingletonVariant(pos(node.pos), symbol(node.kids[0]), tr(node.kids[1]));
      } else {
        // (first-data-variant variant-constructor with)
        const constr = tr(node.kids[0]);
        return new A.SVariant(pos(node.pos), constr.pos, constr.name, constr.args, tr(node.kids[1]));
      }
    },
    'data-sharing': function(node) {
      if (node.kids.length === 2) {
        // (data-sharing SHARING fields)
        return tr(node.kids[1]);
      } else {
        // (data-sharing)
        return makeList([]);
      }
    },
    'type-expr': function(node) {
      return new A.SType(pos(node.pos),
        name(node.kids[1]),
        tr(node.kids[2]),
        tr(node.kids[4]));
    },
    'newtype-expr': function(node) {
      return new A.SNewtype(pos(node.pos), name(node.kids[1]), name(node.kids[3]));
    },
    'var-expr': function(node) {
      // (var-expr VAR bind EQUALS e)
      return new A.SVar(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
    },
    'rec-expr': function(node) {
      // (rec-expr REC bind EQUALS e)
      return new A.SRec(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
    },
    'let-expr': function(node) {
      if (node.kids.length === 3) {
        // (let-expr bind EQUALS e)
        return new A.SLet(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]), false);
      } else {
        // (let-expr VAL bind EQUALS e)
        return new A.SLet(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), true);
      }
    },
    'newtype-bind': function(node) {
      return new A.SNewtypeBind(pos(node.pos), name(node.kids[1]), name(node.kids[3]));
    },
    'type-bind': function(node) {
      return new A.STypeBind(pos(node.pos),
        name(node.kids[0]),
        tr(node.kids[1]),
        tr(node.kids[3]));
    },
    'type-let-bind': function(node) {
      return tr(node.kids[0]);
    },
    'type-let-binds': function(node) {
      // (type-let-binds COMMA type-let-bind)
      return tr(node.kids[1]);
    },
    'type-let-expr': function(node) {
      // (type-let-expr TYPE-LET type-let-bind (COMMA type-let-bind)* (BLOCK|COLON) block end
      const isBlock = (node.kids[node.kids.length - 3].name === 'BLOCK');
      return new A.STypeLetExpr(pos(node.pos),
        makeListComma(node.kids, 1, node.kids.length - 3),
        tr(node.kids[node.kids.length - 2]), isBlock);
    },
    'multi-let-expr': function(node) {
      // (multi-let-expr LET let-binding (COMMA let-binding)* COLON block END)
      // Note that we override the normal name dispatch here, because we don't want
      // to create the default let-expr or var-expr constructions
      const isBlock = (node.kids[node.kids.length - 3].name === 'BLOCK');
      return new A.SLetExpr(pos(node.pos),
        makeListComma(node.kids, 1, node.kids.length - 3, translators['let-binding']),
        tr(node.kids[node.kids.length - 2]), isBlock);
    },
    'letrec-expr': function(node) {
      // (letrec-expr LETREC let-expr (COMMA let-expr)* (BLOCK|COLON0 block END)
      // Note that we override the normal name dispatch here, because we don't want
      // to create the default let-expr constructions
      const isBlock = (node.kids[node.kids.length - 3].name === 'BLOCK');
      return new A.SLetrec(pos(node.pos),
        makeListComma(node.kids, 1, node.kids.length - 3, translators['letrec-binding']),
        tr(node.kids[node.kids.length - 2]), isBlock);
    },
    'let-binding': function(node) {
      if (node.name === 'let-binding') {
        // (let-binding let-expr) or (let-binding var-expr)
        node = node.kids[0];
      }
      if (node.name === 'let-expr') {
        // (let-expr binding EQUALS binop-expr)
        return new A.SLetBind(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      } else if (node.name === 'var-expr') {
        // (var-expr VAR binding EQUALS binop-expr)
        return new A.SVarBind(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
      }
    },
    'letrec-binding': function(node) {
      // (let-expr binding EQUALS binop-expr)
      return new A.SLetrecBind(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
    },
    'contract-stmt': function(node) {
      // (contract-stmt NAME COLONCOLON ty-params ann)
      return new A.SContract(pos(node.pos), name(node.kids[0]), tr(node.kids[2]), tr(node.kids[3]));
    },
    'fun-header': function(node) {
      // (fun-header ty-params args return-ann)
      if (node.kids[1].name === 'bad-args') {
        return {
          lparenPos: pos(node.kids[1].kids[0].pos)
        };
      } else {
        return {
          tyParams: tr(node.kids[0]),
          args: tr(node.kids[1]),
          returnAnn: tr(node.kids[2])
        };
      }
    },
    'fun-expr': function(node) {
      // (fun-expr FUN fun-name fun-header COLON doc body check END)
      const isBlock = (node.kids[3].name === 'BLOCK');
      const header = tr(node.kids[2]);
      if (header.lparenPos) {
        throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[3].pos), header.lparenPos);
      }
      const checkRes = tr(node.kids[6]);
      return new A.SFun(pos(node.pos), symbol(node.kids[1]),
        header.tyParams,
        header.args,
        header.returnAnn,
        tr(node.kids[4]),
        tr(node.kids[5]),
        checkRes[0], checkRes[1],
        isBlock);
    },
    'data-expr': function(node) {
      // (data-expr DATA NAME params COLON variant ... sharing-part check END)
      const checkRes = tr(node.kids[node.kids.length - 2]);
      return new A.SData(pos(node.pos), symbol(node.kids[1]), tr(node.kids[2]), [],
        makeListTr(node.kids, 4, node.kids.length - 3),
        tr(node.kids[node.kids.length - 3]),
        checkRes[0], checkRes[1]);
    },
    'assign-expr': function(node) {
      // (assign-expr id COLONEQUAL e)
      return new A.SAssign(pos(node.pos), name(node.kids[0]), tr(node.kids[2]));
    },
    'when-expr': function(node) {
      // (when-expr WHEN test COLON body END)
      const isBlock = (node.kids[2].name === 'BLOCK');
      return new A.SWhen(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), isBlock);
    },
    'check-expr': function(node) {
      if (node.kids.length === 3) {
        // (check-expr CHECKCOLON body END)
        return new A.SCheck(pos(node.pos), undefined, tr(node.kids[1]),
          node.kids[0].name === 'CHECKCOLON');
      } else {
        // (check-expr CHECK STRING COLON body END)
        return new A.SCheck(pos(node.pos), string(node.kids[1]), tr(node.kids[3]),
          node.kids[0].name === 'CHECK');
      }
    },
    'check-test': function(node) {
      const kids = node.kids;
      if (kids.length === 1) {
        // (check-test e)
        return tr(kids[0]);
      } else if (kids.length === 2) {
        // (check-test left op)
        //             0    1
        return new A.SCheckTest(pos(node.pos), tr(kids[1]), undefined, tr(kids[0]), undefined, undefined);
      } else {
        let refinement, right, because;
        if (kids[2].name === 'PERCENT') {
          // (check-test left op PERCENT LPAREN refinement RPAREN right ...)
          //             0    1                 4                 6
          refinement = tr(kids[4]);
          right = tr(kids[6]);
        } else if (kids[2].name === 'BECAUSE') {
          // (check-test left does-not-raise because ...)
          refinement = undefined;
          right = undefined;
        } else {
          // (check-test left op right ...)
          //             0    1  2
          refinement = undefined;
          right = tr(kids[2]);
        }
        if (kids[kids.length - 2].name === 'BECAUSE') {
          // (check-test ... right BECAUSE cause)
          //                       len-2   len-1
          because = tr(kids[kids.length - 1]);
        } else {
          because = undefined;
        }
        return new A.SCheckTest(pos(node.pos), tr(kids[1]), refinement, tr(kids[0]), right, because);
      }
    },
    'binop-expr': function(node) {
      if (node.kids.length === 1) {
        // (binop-expr e)
        return tr(node.kids[0]);
      } else {
        const mkOp = function(l: Srcloc, opL: Srcloc, op: string, leftE: any, rightE: any): any {
          return new A.SOp(l, opL, op, leftE, rightE);
        };
        let expr = mkOp(pos2(node.kids[0].pos, node.kids[2].pos),
                        pos(node.kids[1].pos),
                        tr(node.kids[1]),
                        tr(node.kids[0]),
                        tr(node.kids[2]));
        for (let i = 4; i < node.kids.length; i += 2) {
          expr = mkOp(pos2(node.kids[0].pos, node.kids[i].pos),
                      pos(node.kids[i - 1].pos),
                      tr(node.kids[i - 1]),
                      expr,
                      tr(node.kids[i]));
        }
        return expr;
      }
    },
    'doc-string': function(node) {
      if (node.kids.length === 0) {
        // (doc-string)
        return '';
      } else {
        // (doc-string DOC str)
        return string(node.kids[1]);
      }
    },
    'where-clause': function(node) {
      if (node.kids.length === 0) {
        // (where-clause)
        return [undefined, undefined];
      } else {
        // (where-clause WHERE block)
        return [makePyretPos(fileName, node.kids[0].pos), tr(node.kids[1])];
      }
    },
    'check-op': function(node) {
      // (check-op str)
      const opname = String(node.kids[0].value).trim();
      if (opLookup[opname]) {
        return (opLookup[opname] as (l: Srcloc) => any)(pos(node.pos));
      }
      else {
        throw new Error('Unknown operator: ' + opname);
      }
    },
    'check-op-postfix': function(node) {
      // (check-op-postfix str)
      const opname = String(node.kids[0].value).trim();
      if (opLookup[opname]) {
        return (opLookup[opname] as (l: Srcloc) => any)(pos(node.pos));
      }
      else {
        throw new Error('Unknown operator: ' + opname);
      }
    },
    'expr': function(node) {
      // (expr e)
      return tr(node.kids[0]);
    },
    'template-expr': function(node) {
      return new A.STemplate(pos(node.pos));
    },
    'binop-expr-paren': function(node) {
      if (node.kids[0].name === 'paren-nospace-expr') {
        // (binop-expr-paren (paren-nospace-expr _ e _))
        return new A.SParen(pos(node.pos), tr(node.kids[0].kids[1]));
      } else {
        // (binop-expr-paren e)
        return tr(node.kids[0]);
      }
    },
    'binop': function(node) {
      // (binop str)
      const opname = String(node.kids[0].value).trim();
      if (opLookup[opname]) {
        return opLookup[opname];
      }
      else {
        throw new Error('Unknown operator: ' + opname);
      }
    },
    'return-ann': function(node) {
      if (node.kids.length === 0) {
        // (return-ann)
        return new A.ABlank();
      } else {
        // (return-ann THINARROW ann)
        return tr(node.kids[1]);
      }
    },

    'binding': function(node) {
      return tr(node.kids[0]);
    },

    'tuple-binding': function(node) {
      let lastBinding = node.kids.length - 1;
      let optAsBinding;
      if (node.kids[lastBinding - 1].name === 'AS') {
        optAsBinding = tr(node.kids[lastBinding]);
        lastBinding -= 2;
      } else {
        optAsBinding = undefined;
      }
      if (node.kids[lastBinding - 1].name === 'SEMI') {
        lastBinding--;
      }
      return new A.STupleBind(pos(node.pos), makeListComma(node.kids, 1, lastBinding), optAsBinding);
    },

    'name-binding': function(node) {
      if (node.kids.length === 1) {
        // (binding name)
        return new A.SBind(pos(node.pos), false, name(node.kids[0]), new A.ABlank());
      } else if (node.kids.length === 3) {
        // (binding name COLONCOLON ann)
        return new A.SBind(pos(node.pos), false, name(node.kids[0]), tr(node.kids[2]));
      } else if (node.kids.length === 2) {
        // (binding SHADOW name)
        return new A.SBind(pos(node.pos), true, name(node.kids[1]), new A.ABlank());
      } else {
        // (binding SHADOW name COLONCOLON ann)
        return new A.SBind(pos(node.pos), true, name(node.kids[1]), tr(node.kids[3]));
      }
    },
    'toplevel-binding': function(node) {
      if (node.kids.length === 1) {
        // is actually a binding
        return tr(node.kids[0]);
      } else if (node.kids.length === 4) {
        // (toplevel-binding SHADOW NAME COLONCOLON noparen-arrow-ann)
        return new A.SBind(pos(node.pos), true, name(node.kids[1]), tr(node.kids[3]));
      } else {
        // (toplevel-binding NAME COLONCOLON noparen-arrow-ann)
        return new A.SBind(pos(node.pos), false, name(node.kids[0]), tr(node.kids[2]));
      }
    },
    'args': function(node) {
      if (node.kids.length === 2) {
        // (args LPAREN RPAREN)
        return makeList([]);
      } else {
        // (args LPAREN binding (COMMA binding)* RPAREN)
        return makeListComma(node.kids, 1, node.kids.length - 1);
      }
    },
    'variant-member': function(node) {
      if (node.kids.length === 1) {
        // (variant-member b)
        return new A.SVariantMember(pos(node.pos), new A.SNormal(), tr(node.kids[0]));
      } else {
        return new A.SVariantMember(pos(node.pos), new A.SMutable(), tr(node.kids[1]));
      }
    },
    'variant-members': function(node) {
      if (node.kids.length === 2) {
        // (variant-members LPAREN RPAREN)
        return makeList([]);
      } else {
        // (variant-members LPAREN mem (COMMA mem)* RPAREN)
        return makeListComma(node.kids, 1, node.kids.length - 1);
      }
    },
    'key': function(node) {
      if (node.kids[0].name === 'NAME') {
        // (key name)
        return symbol(node.kids[0]);
      } else {
        // (key str)
        return string(node.kids[0]);
      }
    },
    'obj-field': function(node) {
      if (node.kids.length === 4) {
        // (obj-field MUTABLE key COLON value)
        return new A.SMutableField(pos(node.pos), tr(node.kids[1]), new A.ABlank(), tr(node.kids[3]));
      } else if (node.kids.length === 6) {
        // (obj-field MUTABLE key COLONCOLON ann COLON value)
        return new A.SMutableField(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]), tr(node.kids[5]));
      } else if (node.kids.length === 3) {
        // (obj-field key COLON value)
        return new A.SDataField(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      } else {
        // (obj-field METHOD key fun-header COLON doc body check END)
        const isBlock = (node.kids[3].name === 'BLOCK');
        const header = tr(node.kids[2]);
        if (header.lparenPos) {
          throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[3].pos), header.lparenPos);
        }
        const checkRes = tr(node.kids[6]);
        return new A.SMethodField(pos(node.pos), tr(node.kids[1]), header.tyParams, header.args, header.returnAnn,
          tr(node.kids[4]), tr(node.kids[5]), checkRes[0], checkRes[1], isBlock);
      }
    },
    'tuple-name-list': function(node) {
      if (node.kids[node.kids.length - 1].name !== 'binding') {
        // (obj-fields (list-tuple-field f1 SEMI) ... lastField SEMI)
        return makeListComma(node.kids, 0, node.kids.length - 1);
      } else {
        // (fields (list-tuple-field f1 SEMI) ... lastField)
        return makeListComma(node.kids);
      }
    },
    'tuple-fields': function(node) {
      if (node.kids[node.kids.length - 1].name === 'SEMI') {
        // (obj-fields (list-tuple-field f1 SEMI) ... lastField SEMI)
        return makeListComma(node.kids, 0, node.kids.length - 1);
      } else {
        // (fields (list-tuple-field f1 SEMI) ... lastField)
        return makeListComma(node.kids);
      }
    },
    'reactor-expr': function(node) {
      // (REACTOR COLON fields END)
      return new A.SReactor(pos(node.pos), tr(node.kids[2]));
    },
    'table-expr': function(node) {
      // (TABLE table-headers table-rows end)
      return new A.STable(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]));
    },
    'load-table-expr': function(node) {
      // (LOAD-TABLE COLON table-headers load-table-specs END)
      return new A.SLoadTable(pos(node.pos), tr(node.kids[2]),
        ((node.kids[3].name === 'END')
          ? makeList([]) : tr(node.kids[3])));
    },
    'table-headers': function(node) {
      // [list-table-header* table-header]
      return makeList(node.kids.map(tr));
    },
    'list-table-header': function(node) {
      // (table-header COMMA)
      return tr(node.kids[0]);
    },
    'table-header': function(node) {
      // NAME [:: ann]
      if (node.kids.length === 3) {
        return new A.SFieldName(pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]));
      } else {
        return new A.SFieldName(pos(node.pos), symbol(node.kids[0]), new A.ABlank());
      }
    },
    'table-rows': function(node) {
      // [table-row* table-row]
      return makeList(node.kids.map(tr));
    },
    'table-row': function(node) {
      // (ROW table-items)
      return new A.STableRow(pos(node.pos), tr(node.kids[1]));
    },
    'table-items': function(node) {
      // [list-table-item* binop-expr]
      return makeList(node.kids.map(tr));
    },
    'list-table-item': function(node) {
      // (binop-expr COMMA)
      return tr(node.kids[0]);
    },
    'table-extend-fields': function(node) {
      if (node.kids[node.kids.length - 1].name !== 'table-extend-field') {
        return makeList(node.kids.slice(0, -1).map(tr));
      } else {
        // [list-table-extend-field* table-extend-field COMMA]
        return makeList(node.kids.map(tr));
      }
    },
    'list-table-extend-field': function(node) {
      // (table-extend-field COMMA)
      return tr(node.kids[0]);
    },
    'table-extend-field': function(node) {
      if (node.kids.length === 3) {
        // (key COLON binop-expr)
        return new A.STableExtendField(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]),
          new A.ABlank());
      } else if ((node.kids.length === 5)
                 && (node.kids[1].name === 'COLONCOLON')) {
        // (key COLONCOLON ann COLON binop-expr)
        return new A.STableExtendField(pos(node.pos), tr(node.kids[0]), tr(node.kids[4]),
          tr(node.kids[2]));
      } else if (node.kids.length === 5) {
        // (key COLON expr OF NAME)
        return new A.STableExtendReducer(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]),
          name(node.kids[4]), new A.ABlank());
      } else if (node.kids.length === 7) {
        // (key COLONCOLON ann COLON expr OF NAME)
        return new A.STableExtendReducer(pos(node.pos), tr(node.kids[0]), tr(node.kids[4]),
          name(node.kids[6]), tr(node.kids[2]));
      }
    },
    'load-table-specs': function(node) {
      if (node.kids[node.kids.length - 1].name !== 'load-table-spec') {
        return makeList(node.kids.slice(0, -1).map(tr));
      } else {
        // [list-load-table-spec* load-table-spec COMMA]
        return makeList(node.kids.map(tr));
      }
    },
    'load-table-spec': function(node) {
      if (node.kids[0].name === 'SANITIZE') {
        // (SANITIZE NAME USING expr)
        return new A.SSanitize(pos(node.pos), name(node.kids[1]), tr(node.kids[3]));
      } else {
        // (SOURCECOLON expr)
        return new A.STableSrc(pos(node.pos), tr(node.kids[1]));
      }
    },
    // NOTE: 'sql-expr', 'do-expr', 'for-then', and 'else' are carried
    // over from parse-pyret.js, but they are dead code: the productions
    // are not in pyret-grammar.bnf and the constructors they reference
    // (s-sql, s-do, 5-argument s-for, s-else) do not exist in ast.arr.
    // In the JS original they would crash on a missing runtime field;
    // here they fail with an explicit error.
    'sql-expr': function(node) {
      throw new Error("parse-pyret: 'sql-expr' is not a live grammar production (s-sql is not in ast.arr)");
    },
    'do-expr': function(node) {
      throw new Error("parse-pyret: 'do-expr' is not a live grammar production (s-do is not in ast.arr)");
    },
    'for-then': function(node) {
      throw new Error("parse-pyret: 'for-then' is not a live grammar production");
    },
    'for-bind-elt': function(node) {
      // (for-bind-elt b COMMA)
      return tr(node.kids[0]);
    },
    'obj-fields': function(node) {
      if (node.kids[node.kids.length - 1].name !== 'obj-field') {
        // (obj-fields objField (COMMA obj-field)* lastField COMMA)
        return makeListComma(node.kids, 0, node.kids.length - 1);
      } else {
        // (obj-fields obj-field (COMMA obj-field)*)
        return makeListComma(node.kids);
      }
    },
    'field': function(node) {
      if (node.kids.length === 3) {
        // (field key COLON value)
        return new A.SDataField(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
      } else {
        // (field METHOD key fun-header (BLOCK|COLON) doc body check END)
        const isBlock = (node.kids[3].name === 'BLOCK');
        const header = tr(node.kids[2]);
        if (header.lparenPos) {
          throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[3].pos), header.lparenPos);
        }
        const checkRes = tr(node.kids[6]);
        return new A.SMethodField(pos(node.pos), tr(node.kids[1]), header.tyParams, header.args, header.returnAnn,
          tr(node.kids[4]), tr(node.kids[5]), checkRes[0], checkRes[1], isBlock);
      }
    },
    'fields': function(node) {
      if (node.kids[node.kids.length - 1].name !== 'field') {
        // (fields field (COMMA f1)* COMMA)
        return makeListComma(node.kids, 0, node.kids.length - 1);
      } else {
        // (fields field (COMMA f1)*)
        return makeListComma(node.kids);
      }
    },
    'data-mixins': function(node) {
      if (node.kids.length === 0) {
        // (data-mixins)
        return makeList([]);
      } else {
        // (data-mixins DERIVING mixins)
        return tr(node.kids[1]);
      }
    },
    'app-args': function(node) {
      // (app-args LPAREN opt-comma-binops RPAREN)
      return tr(node.kids[1]);
    },
    'opt-comma-binops': function(node) {
      if (node.kids.length === 0) {
        return [];
      } else {
        return tr(node.kids[0]);
      }
    },
    'comma-binops': function(node) {
      return makeListComma(node.kids);
    },
    'trailing-opt-comma-binops': function(node) {
      if (node.kids.length === 0) {
        return [];
      } else {
        return tr(node.kids[0]);
      }
    },
    'cases-args': function(node) {
      if (node.kids.length === 2) {
        // (cases-args LPAREN RPAREN)
        return makeList([]);
      } else {
        // (cases-args LPAREN cases-binding (COMMA arg)* RPAREN)
        return makeListComma(node.kids, 1, node.kids.length - 1);
      }
    },
    'cases-binding': function(node) {
      if (node.kids.length === 2) {
        return new A.SCasesBind(pos(node.pos), new A.SCasesBindRef(), tr(node.kids[1]));
      }
      else {
        return new A.SCasesBind(pos(node.pos), new A.SCasesBindNormal(), tr(node.kids[0]));
      }
    },
    'cases-branch': function(node) {
      if (node.kids.length === 4) {
        // (singleton-cases-branch PIPE NAME THICKARROW body)
        return new A.SSingletonCasesBranch(pos(node.pos), pos(node.kids[1].pos), symbol(node.kids[1]), tr(node.kids[3]));
      } else {
        // (cases-branch PIPE NAME args THICKARROW body)
        return new A.SCasesBranch(pos(node.pos), pos(node.kids[1].pos.combine(node.kids[2].pos)),
          symbol(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
      }
    },
    'if-pipe-branch': function(node) {
      // (if-pipe-branch BAR binop-expr THENCOLON block)
      return new A.SIfPipeBranch(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
    },
    'else-if': function(node) {
      // (else-if ELSEIF test COLON body)
      return new A.SIfBranch(pos(node.pos), tr(node.kids[1]), tr(node.kids[3]));
    },
    'else': function(node) {
      // (else ELSECOLON body) -- dead: 'else' is not a grammar
      // production and s-else is not in ast.arr.
      throw new Error("parse-pyret: 'else' is not a live grammar production (s-else is not in ast.arr)");
    },
    'ty-params': function(node) {
      if (node.kids.length === 0) {
        // (ty-params)
        return makeList([]);
      } else {
        // (ty-params LANGLE comma-names RANGLE)
        return tr(node.kids[1]);
      }
    },
    'for-bind': function(node) {
      // (for-bind name FROM e)
      return new A.SForBind(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
    },
    'prim-expr': function(node) {
      // (prim-expr e)
      return tr(node.kids[0]);
    },
    'tuple-expr': function(node) {
      return new A.STuple(pos(node.pos), tr(node.kids[1]));
    },
    'tuple-get': function(node) {
      // ast.ts types the index as a plain JS number (it is a count, per
      // CONVENTIONS.md); the NUMBER token is an integer literal.
      return new A.STupleGet(pos(node.pos), tr(node.kids[0]), jsnums.toFixnum(number(node.kids[3])), pos(node.kids[3].pos));
    },
    'obj-expr': function(node) {
      if (node.kids.length === 2) {
        // (obj-expr LBRACE RBRACE)
        return new A.SObj(pos(node.pos), makeList([]));
      } else {
        // (obj-expr LBRACE obj-fields RBRACE)
        return new A.SObj(pos(node.pos), tr(node.kids[1]));
      }
    },
    'construct-expr': function(node) {
      // LBRACK construct-modifier binop-expr COLON trailing-opt-comma-binops RBRACK
      return new A.SConstruct(pos(node.pos), tr(node.kids[1]), tr(node.kids[2]), tr(node.kids[4]));
    },
    'construct-modifier': function(node) {
      if (node.kids.length === 0) {
        return new A.SConstructNormal();
      } else if (node.kids.length === 1) {
        if (node.kids[0].name === 'LAZY') {
          return new A.SConstructLazy();
        }
      }
    },
    'app-expr': function(node) {
      if (node.kids.length > 2) {
        throwParseErrorBadApp(pos(node.kids[0].pos),
          pos2(node.kids[1].pos, node.kids[node.kids.length - 1].pos));
      } else {
        // (app-expr f args)
        return new A.SApp(pos(node.pos), tr(node.kids[0]), tr(node.kids[1]));
      }
    },
    'id-expr': function(node) {
      // (id-expr x)
      return new A.SId(pos(node.pos), name(node.kids[0]));
    },
    'dot-expr': function(node) {
      // (dot-expr obj PERIOD field)
      return new A.SDot(pos(node.pos), tr(node.kids[0]), symbol(node.kids[2]));
    },
    'get-bang-expr': function(node) {
      // (get-bang-expr obj BANG field)
      return new A.SGetBang(pos(node.pos), tr(node.kids[0]), symbol(node.kids[2]));
    },
    'bracket-expr': function(node) {
      // (bracket-expr obj LBRACK field RBRACK)
      return new A.SBracket(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
    },
    'cases-expr': function(node) {
      const isBlock = (node.kids[5].name === 'BLOCK');
      if (node.kids[node.kids.length - 4].name === 'ELSE') {
        // (cases-expr CASES LPAREN type RPAREN val COLON branch ... PIPE ELSE THICKARROW elseblock END)
        return new A.SCasesElse(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
          makeListTr(node.kids, 6, node.kids.length - 5), tr(node.kids[node.kids.length - 2]), isBlock);
      } else {
        // (cases-expr CASES LPAREN type RPAREN val COLON branch ... END)
        return new A.SCases(pos(node.pos), tr(node.kids[2]), tr(node.kids[4]),
          makeListTr(node.kids, 6, node.kids.length - 1), isBlock);
      }
    },
    'if-pipe-expr': function(node) {
      const isBlock = (node.kids[1].name === 'BLOCK');
      if (node.kids[node.kids.length - 3].name === 'OTHERWISECOLON') {
        // (if-pipe-expr ASK (BLOCK|COLON) branch ... BAR OTHERWISECOLON else END)
        return new A.SIfPipeElse(pos(node.pos), makeListTr(node.kids, 2, node.kids.length - 4),
          tr(node.kids[node.kids.length - 2]), isBlock);
      } else {
        // (if-pipe-expr ASK (BLOCK|COLON) branch ... END)
        return new A.SIfPipe(pos(node.pos), makeListTr(node.kids, 2, node.kids.length - 1), isBlock);
      }
    },
    'if-expr': function(node) {
      const isBlock = (node.kids[2].name === 'BLOCK');
      if (node.kids[node.kids.length - 3].name === 'ELSECOLON') {
        // (if-expr IF test (BLOCK|COLON) body branch ... ELSECOLON else END)
        return new A.SIfElse(pos(node.pos),
          makeList([new A.SIfBranch(pos2(node.kids[1].pos, node.kids[3].pos), tr(node.kids[1]), tr(node.kids[3]))],
                   0, 1,
                   makeListTr(node.kids, 4, node.kids.length - 3)),
          tr(node.kids[node.kids.length - 2]), isBlock);
      } else {
        // (if-expr IF test (BLOCK|COLON) body branch ... END)
        return new A.SIf(pos(node.pos),
          makeList([new A.SIfBranch(pos2(node.kids[1].pos, node.kids[3].pos), tr(node.kids[1]), tr(node.kids[3]))],
                   0, 1,
                   makeListTr(node.kids, 4, node.kids.length - 1)), isBlock);
      }
    },
    'for-expr': function(node) {
      // (for-expr FOR iter LPAREN for-bind (COMMA for-bind)* RPAREN return (BLOCK|COLON) body END)
      const isBlock = (node.kids[node.kids.length - 3].name === 'BLOCK');
      return new A.SFor(pos(node.pos), tr(node.kids[1]), makeListComma(node.kids, 3, node.kids.length - 5),
        tr(node.kids[node.kids.length - 4]), tr(node.kids[node.kids.length - 2]), isBlock);
    },
    'user-block-expr': function(node) {
      // (user-block-expr BLOCK body END)
      return new A.SUserBlock(pos(node.pos), tr(node.kids[1]));
    },
    'lambda-expr': function(node) {
      // (lambda-expr LAM fun-header COLON doc body check END)
      const isBlock = (node.kids[2].name === 'BLOCK');
      const header = tr(node.kids[1]);
      if (header.lparenPos) {
        throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[2].pos), header.lparenPos);
      }
      const checkRes = tr(node.kids[5]);
      return new A.SLam(pos(node.pos), '', header.tyParams, header.args, header.returnAnn,
        tr(node.kids[3]), tr(node.kids[4]), checkRes[0], checkRes[1], isBlock);
    },
    'method-expr': function(node) {
      // (method-expr METHOD fun-header COLON doc body check END)
      const isBlock = (node.kids[2].name === 'BLOCK');
      const header = tr(node.kids[1]);
      if (header.lparenPos) {
        throwParseErrorBadFunHeader(pos2(node.kids[0].pos, node.kids[2].pos), header.lparenPos);
      }
      const checkRes = tr(node.kids[5]);
      return new A.SMethod(pos(node.pos), '', header.tyParams, header.args, header.returnAnn,
        tr(node.kids[3]), tr(node.kids[4]), checkRes[0], checkRes[1], isBlock);
    },
    'extend-expr': function(node) {
      // (extend-expr e PERIOD LBRACE fields RBRACE)
      return new A.SExtend(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
    },
    'update-expr': function(node) {
      // (update-expr e BANG LBRACE fields RBRACE)
      return new A.SUpdate(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
    },
    'paren-expr': function(node) {
      // (paren-expr LPAREN e RPAREN)
      return new A.SParen(pos(node.pos), tr(node.kids[1]));
    },
    'paren-nospace-expr': function(node) {
      // (paren-nospace-expr LPAREN e RPAREN)
      return new A.SParen(pos(node.pos), tr(node.kids[1]));
    },
    'inst-expr': function(node) {
      // (inst-expr e LANGLE ann (COMMA ann)* RANGLE)
      return new A.SInstantiate(pos(node.pos), tr(node.kids[0]), makeListComma(node.kids, 2, node.kids.length - 1));
    },
    'bool-expr': function(node) {
      if (node.kids[0].name === 'TRUE') {
        return new A.SBool(pos(node.pos), true);
      } else {
        return new A.SBool(pos(node.pos), false);
      }
    },
    'num-expr': function(node) {
      // (num-expr n)
      return new A.SNum(pos(node.pos), number(node.kids[0]));
    },
    'frac-expr': function(node) {
      // (frac-expr n)
      const numden = node.kids[0].value!.split('/');
      return new A.SFrac(pos(node.pos), makeNumberFromString(numden[0]), makeNumberFromString(numden[1]));
    },
    'rfrac-expr': function(node) {
      // (rfrac-expr n)
      const numden = node.kids[0].value!.substring(1).split('/');
      return new A.SRfrac(pos(node.pos), makeNumberFromString(numden[0]), makeNumberFromString(numden[1]));
    },
    'string-expr': function(node) {
      return new A.SStr(pos(node.pos), string(node.kids[0]));
    },
    'ann-field': function(node) {
      // (ann-field n COLON ann) or (ann-field n COLONCOLON ann)
      return new A.AField(pos(node.pos), symbol(node.kids[0]), tr(node.kids[2]));
    },
    'name-ann': function(node) {
      if (node.kids[0].value === 'Any') {
        return new A.AAny(pos(node.pos));
      } else {
        return new A.AName(pos(node.pos), name(node.kids[0]));
      }
    },
    'comma-ann-field': function(node) {
      return makeListComma(node.kids);
    },
    'trailing-opt-comma-ann-field': function(node) {
      if (node.kids.length === 0) {
        return [];
      } else {
        return tr(node.kids[0]);
      }
    },
    'record-ann': function(node) {
      // (record-ann LBRACE ann-field (COMMA ann-field)* RBRACE)
      return new A.ARecord(pos(node.pos), tr(node.kids[1]));
    },
    'tuple-ann': function(node) {
      // (tuple LBRACE ann (SEMI ann)* [SEMI] RBRACE
      if (node.kids[node.kids.length - 2].name === 'SEMI') {
        return new A.ATuple(pos(node.pos), makeListComma(node.kids, 1, node.kids.length - 2));
      } else {
        return new A.ATuple(pos(node.pos), makeListComma(node.kids, 0, node.kids.length - 1));
      }
    },
    'noparen-arrow-ann': function(node) {
      if (node.kids.length === 2) {
        // (noparen-arrow-ann THINARROW result)
        return new A.AArrow(pos(node.pos), [], tr(node.kids[1]), false);
      } else {
        // (noparen-arrow-ann arrow-ann-args THINARROW result)
        const trArgs = tr(node.kids[0]);
        if (trArgs.named) {
          return new A.AArrowArgnames(pos(node.pos), trArgs.args, tr(node.kids[2]), false);
        } else {
          return new A.AArrow(pos(node.pos), trArgs.args, tr(node.kids[2]), false);
        }
      }
    },
    'arrow-ann-args': function(node) {
      if (node.kids.length === 1) {
        // (arrow-ann-args comma-anns)
        return { args: tr(node.kids[0]), named: false };
      } else {
        // (arrow-ann-args LPAREN comma-ann-field RPAREN
        return { args: tr(node.kids[1]), named: true };
      }
    },
    //TABLE-EXTEND expr [USING binding (COMMA binding)*] COLON obj-fields end
    //           0    1      3       4                -4    -3         -2  -1
    'table-extend': function(node) {
      const columns: any[] = [];
      for (let i = 3; i < node.kids.length - 3; i += 2)
        columns.push(tr(node.kids[i]));
      const table = tr(node.kids[1]);
      const extensions = tr(node.kids[node.kids.length - 2]);
      return new A.STableExtend(pos(node.pos),
        new A.SColumnBinds(
          combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
          makeList(columns),
          table),
        extensions);
    },
    'table-update': function(node) {
      const columns: any[] = [];
      for (let i = 3; i < node.kids.length - 3; i += 2)
        columns.push(tr(node.kids[i]));
      const table = tr(node.kids[1]);
      const extensions = tr(node.kids[node.kids.length - 2]);
      return new A.STableUpdate(pos(node.pos),
        new A.SColumnBinds(
          combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
          makeList(columns),
          table),
        extensions);
    },
    //TABLE-SELECT NAME (COMMA NAME)* FROM expr end
    'table-select': function(node) {
      const columns: any[] = [];
      for (let i = 1; i < node.kids.length - 3; i += 2)
        columns.push(name(node.kids[i]));
      const table = tr(node.kids[node.kids.length - 2]);
      return new A.STableSelect(pos(node.pos), makeList(columns), table);
    },
    'column-order': function(node) {
      const column = name(node.kids[0]);
      // The grammar guarantees the token is ASCENDING or DESCENDING
      // (the JS original left direction undefined otherwise).
      const direction = node.kids[1].name === 'ASCENDING' ? new A.ASCENDING()
        : node.kids[1].name === 'DESCENDING' ? new A.DESCENDING()
        : undefined;
      return new A.SColumnSort(pos(node.pos), column, direction!);
    },
    'table-order': function(node) {
      // TABLE-ORDER NAME COLON column-orderings end
      return new A.STableOrder(pos(node.pos),
        tr(node.kids[1]),
        makeListComma(node.kids, 3, node.kids.length - 1, tr));
    },
    'table-filter': function(node) {
      const columns: any[] = [];
      for (let i = 3; i < node.kids.length - 3; i += 2)
        columns.push(tr(node.kids[i]));
      const table = tr(node.kids[1]);
      const predicate = tr(node.kids[node.kids.length - 2]);
      return new A.STableFilter(pos(node.pos),
        new A.SColumnBinds(
          combinePyretPos(fileName, node.kids[1].pos, node.kids[node.kids.length - 4].pos),
          makeList(columns),
          table),
        predicate);
    },
    'table-extract': function(node) {
      return new A.STableExtract(pos(node.pos), name(node.kids[1]), tr(node.kids[3]));
    },
    'arrow-ann': function(node) {
      if (node.kids.length === 4) {
        // (arrow-ann LPAREN THINARROW result RPAREN)
        return new A.AArrow(pos(node.pos), [], tr(node.kids[2]), true);
      } else {
        // (arrow-ann LPAREN arrow-ann-args THINARROW result RPAREN)
        // (noparen-arrow-ann arrow-ann-args THINARROW result)
        const trArgs = tr(node.kids[1]);
        if (trArgs.named) {
          return new A.AArrowArgnames(pos(node.pos), trArgs.args, tr(node.kids[3]), true);
        } else {
          return new A.AArrow(pos(node.pos), trArgs.args, tr(node.kids[3]), true);
        }
      }
    },
    'app-ann': function(node) {
      // (app-ann ann LANGLE comma-anns RANGLE)
      return new A.AApp(pos(node.pos), tr(node.kids[0]), tr(node.kids[2]));
    },
    'comma-anns': function(node) {
      return makeListComma(node.kids);
    },
    'comma-names': function(node) {
      return makeListComma(node.kids, 0, node.kids.length, name);
    },
    'pred-ann': function(node) {
      // (pred-ann ann PERCENT LPAREN exp RPAREN)
      return new A.APred(pos(node.pos), tr(node.kids[0]), tr(node.kids[3]));
    },
    'dot-ann': function(node) {
      // (dot-ann n1 PERIOD n2)
      return new A.ADot(pos(node.pos), name(node.kids[0]), symbol(node.kids[2]));
    },
    'ann': function(node) {
      // (ann a)
      return tr(node.kids[0]);
    }
  };
  return tr(node);
}

// ---------- entry points ----------

export type ParseFailure = { exn: PyretParseError; message: string };

function parseDataRaw(data: string, fileName: string): Either<ParseFailure, A.Program> {
  let message = '';
  try {
    const toks = getTokenizer().Tokenizer;
    const grammar = getParser().PyretGrammar;
    toks.tokenizeFrom(data);
    const parsed = grammar.parse(toks);
    const countParses = grammar.countAllParses(parsed);
    if (countParses === 0) {
      const nextTok = toks.curTok;
      message = 'There were ' + countParses + ' potential parses.\n' +
                'Parse failed, next token is ' + nextTok.toRepr(true) +
                ' at ' + fileName + ', ' + nextTok.pos.toString(true);
      if (toks.isEOF(nextTok))
        throwParseErrorEOF(makePyretPos(fileName, nextTok.pos));
      else if (nextTok.name === 'UNTERMINATED-STRING')
        throwParseErrorUnterminatedString(makePyretPos(fileName, nextTok.pos));
      else if (nextTok.name === 'BAD-NUMBER')
        throwParseErrorBadNumber(makePyretPos(fileName, nextTok.pos));
      else if (nextTok.name === 'BAD-OPER' || nextTok.name === 'STAR')
        throwParseErrorBadOper(makePyretPos(fileName, nextTok.pos));
      else if (nextTok.name === 'COLONCOLON')
        throwParseErrorColonColon(makePyretPos(fileName, nextTok.pos));
      else if (typeof opLookup[String(nextTok.value).trim()] === 'function')
        throwParseErrorBadCheckOper(
          (opLookup[String(nextTok.value).trim()] as (l: Srcloc) => any)(makePyretPos(fileName, nextTok.pos)));
      else
        throwParseErrorNextToken(makePyretPos(fileName, nextTok.pos), nextTok.value || nextTok.toString(true));
    }
    if (countParses === 1) {
      const ast = grammar.constructUniqueParse(parsed);
      return right(translate(ast, fileName));
    } else {
      throw new Error('Non-unique parse');
    }
  } catch (e) {
    if (e instanceof PyretParseError) {
      return left({ exn: e, message: message });
    } else {
      throw e;
    }
  }
}

// surface-parse: returns the Program or throws the PyretParseError
// (the JS version console.errors the message and re-raises the exn).
export function surfaceParse(code: string, uri: string): A.Program {
  const result = parseDataRaw(code, uri);
  if (result.$name === 'left') {
    console.error(result.v.message);
    throw result.v.exn;
  } else {
    return result.v;
  }
}

// maybe-surface-parse: returns the Either directly.
export function maybeSurfaceParse(code: string, uri: string): Either<ParseFailure, A.Program> {
  return parseDataRaw(code, uri);
}
