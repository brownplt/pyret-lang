/*
  TypeScript port of src/arr/trove/ast.arr (lines 1-1879: everything before
  default-map-visitor). See CONVENTIONS.md. Visitors live in ast-visitors.ts.

  Number representation choices (see CONVENTIONS.md "Numbers"):
  - s-num.n is an opaque PyretNumber (js-numbers value) so exact rationals
    survive end to end; tosource uses PP.number which must match Pyret
    tostring(n) output (e.g. "1/3").
  - s-frac/s-rfrac num and den are PyretNumber too: they are NumInteger
    literal values produced by the parser from integer literal text, which
    may exceed 2^53, and their tosource goes through PP.number (i.e.
    tostring of the exact integer). Plain JS number would lose precision.
  - Position-like Numbers (s-tuple-get.index, s-atom.serial, INDENT) are
    plain JS number.
*/

import * as PP from './pprint';
import { Loc, Builtin, Srcloc, dummyLoc } from './srcloc';
import { PyretNumber } from './interop/js-numbers';
import { raise } from './shared';

export type { Loc };
// ast.arr's dummy-loc (= S.builtin("dummy location")); re-exported because
// ast.arr `provide *`s it.
export { dummyLoc };

// Pyret torepr of a string (escapeString in src/js/base/runtime.js).
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

function toreprStr(s: string): string {
  return '"' + replaceUnprintableStringChars(s) + '"';
}

// tostring/torepr of a Srcloc value (constructor-style rendering; Srcloc
// defines no _output, so Pyret renders it structurally).
function toreprLoc(loc: Loc): string {
  if (loc instanceof Builtin) {
    return 'builtin(' + toreprStr(loc.moduleName) + ')';
  } else {
    const l = loc as Srcloc;
    return 'srcloc(' + toreprStr(l.source) + ', ' + String(l.startLine) + ', '
      + String(l.startColumn) + ', ' + String(l.startChar) + ', '
      + String(l.endLine) + ', ' + String(l.endColumn) + ', '
      + String(l.endChar) + ')';
  }
}

export const INDENT = 2;

export const breakOne = PP.sbreak(1);
export const strAny = PP.str('Any');
export const strArrow = PP.str('->');
export const strArrowspace = PP.str('-> ');
export const strAs = PP.str('as');
export const strBlank = PP.str('');
export const strBecause = PP.str('because');
export const strLet = PP.str('let');
export const strTypeLet = PP.str('type-let');
export const strLetrec = PP.str('letrec');
export const strBlock = PP.str('block:');
export const strBrackets = PP.str('[list: ]');
export const strCases = PP.str('cases');
export const strCaret = PP.str('^');
export const strCheckcolon = PP.str('check:');
export const strExamplescolon = PP.str('examples:');
export const strColon = PP.str(':');
export const strColoncolon = PP.str('::');
export const strColonspace = PP.str(': ');
export const strComment = PP.str('# ');
export const strConstructor = PP.str('with constructor');
export const strData = PP.str('data ');
export const strDataExpr = PP.str('data-expr ');
export const strDeriving = PP.str('deriving ');
export const strDoc = PP.str('doc: ');
export const strElsebranch = PP.str('| else =>');
export const strElsecolon = PP.str('else:');
export const strOtherwisecolon = PP.str('otherwise:');
export const strElsespace = PP.str('else ');
export const strEnd = PP.str('end');
export const strExcept = PP.str('except');
export const strFor = PP.str('for ');
export const strDo = PP.str('do ');
export const strFrom = PP.str('from');
export const strFun = PP.str('fun');
export const strLam = PP.str('lam');
export const strIf = PP.str('if ');
export const strOf = PP.str('of ');
export const strAsk = PP.str('ask');
export const strImport = PP.str('import');
export const strInclude = PP.str('include');
export const strMethod = PP.str('method');
export const strMutable = PP.str('mutable');
export const strPeriod = PP.str('.');
export const strBang = PP.str('!');
export const strPipespace = PP.str('| ');
export const strProvide = PP.str('provide');
export const strProvideTypes = PP.str('provide-types');
export const strProvideStar = PP.str('provide *');
export const strProvideTypesStar = PP.str('provide-types *');
export const strSharing = PP.str('sharing:');
export const strSpace = PP.str(' ');
export const strSpacecolonequal = PP.str(' :=');
export const strSpaceequal = PP.str(' =');
export const strThencolon = PP.str('then:');
export const strThickarrow = PP.str('=>');
export const strUseLoc = PP.str('UseLoc');
export const strVar = PP.str('var ');
export const strRec = PP.str('rec ');
export const strNewtype = PP.str('newtype ');
export const strType = PP.str('type ');
export const strModule = PP.str('module ');
export const strVal = PP.str('val ');
export const strWhen = PP.str('when ');
export const strWhere = PP.str('where:');
export const strWith = PP.str('with:');
export const strIs = PP.str('is');
export const strIsNot = PP.str('is-not');
export const strSatisfies = PP.str('satisfies');
export const strSatisfiesNot = PP.str('violates');
export const strRaises = PP.str('raises');
export const strRaisesOther = PP.str('raises-other-than');
export const strRaisesNot = PP.str('does-not-raise');
export const strRaisesSatisfies = PP.str('raises-satisfies');
export const strRaisesViolates = PP.str('raises-violates');
export const strPercent = PP.str('%');
export const strTablecolon = PP.str('table:');
export const strRowcolon = PP.str('row:');
export const strExtend = PP.str('extend');
export const strTransform = PP.str('transform');
export const strUse = PP.str('use');
export const strUsing = PP.str('using');
export const strSelect = PP.str('select');
export const strSieve = PP.str('sieve');
export const strOrder = PP.str('order');
export const strExtract = PP.str('extract');
export const strLoadTable = PP.str('load-table:');
export const strSrc = PP.str('source:');
export const strSanitize = PP.str('sanitize');

// ---------- Name ----------

export abstract class NameBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  abstract toCompiledSource(): any;
  abstract toCompiled(): string;
  abstract tosource(): any;
  abstract tosourcestring(): string;
  abstract toname(): string;
  abstract key(): string;
  lessthan(other: Name): boolean { return this.key() < other.key(); }
  lessequal(other: Name): boolean { return this.key() <= other.key(); }
  greaterthan(other: Name): boolean { return this.key() > other.key(); }
  greaterequal(other: Name): boolean { return this.key() >= other.key(); }
  equals(other: Name): boolean { return this.key() === other.key(); }
  // Name keys are what the compiler uses anywhere a Name must become a
  // string (dict keys, comparisons); ast.arr's Name has no tostring method.
  toString(): string { return this.key(); }
}

export class SUnderscore extends NameBase {
  get $name(): 's-underscore' { return 's-underscore'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sUnderscore(this); }
  toCompiledSource(): any { return raise('Cannot compile underscores'); }
  toCompiled(): string { return raise('Cannot compile underscores'); }
  tosource(): any { return PP.str('_'); }
  tosourcestring(): string { return '_'; }
  toname(): string { return '_'; }
  key(): string { return 'underscore#'; }
}

export class SName extends NameBase {
  get $name(): 's-name' { return 's-name'; }
  constructor(public l: Loc, public s: string) { super(); }
  visit(visitor: any): any { return visitor.sName(this); }
  toCompiledSource(): any { return PP.str(this.toCompiled()); }
  toCompiled(): string { return this.s; }
  tosource(): any { return PP.str(this.s); }
  tosourcestring(): string { return this.s; }
  toname(): string { return this.s; }
  key(): string { return 'name#' + this.s; }
}

export class SGlobal extends NameBase {
  get $name(): 's-global' { return 's-global'; }
  constructor(public s: string) { super(); }
  visit(visitor: any): any { return visitor.sGlobal(this); }
  toCompiledSource(): any { return PP.str(this.toCompiled()); }
  toCompiled(): string { return this.s; }
  tosource(): any { return PP.str(this.s); }
  tosourcestring(): string { return this.s; }
  toname(): string { return this.s; }
  key(): string { return 'global#' + this.s; }
}

export class SModuleGlobal extends NameBase {
  get $name(): 's-module-global' { return 's-module-global'; }
  constructor(public s: string) { super(); }
  visit(visitor: any): any { return visitor.sModuleGlobal(this); }
  toCompiledSource(): any { return PP.str(this.toCompiled()); }
  toCompiled(): string { return '$module$' + this.s; }
  tosource(): any { return PP.str(this.s); }
  tosourcestring(): string { return '$module$' + this.s; }
  toname(): string { return this.s; }
  key(): string { return 'mglobal#' + this.s; }
}

export class STypeGlobal extends NameBase {
  get $name(): 's-type-global' { return 's-type-global'; }
  constructor(public s: string) { super(); }
  visit(visitor: any): any { return visitor.sTypeGlobal(this); }
  toCompiledSource(): any { return PP.str(this.toCompiled()); }
  toCompiled(): string { return '$type$' + this.s; }
  tosource(): any { return PP.str(this.s); }
  tosourcestring(): string { return '$type$' + this.s; }
  toname(): string { return this.s; }
  key(): string { return 'tglobal#' + this.s; }
}

export class SAtom extends NameBase {
  get $name(): 's-atom' { return 's-atom'; }
  constructor(public base: string, public serial: number) { super(); }
  visit(visitor: any): any { return visitor.sAtom(this); }
  toCompiledSource(): any { return PP.str(this.toCompiled()); }
  toCompiled(): string { return this.base + String(this.serial); }
  tosource(): any { return PP.str(this.toname()); }
  tosourcestring(): string { return this.toCompiled(); }
  toname(): string { return this.base; }
  key(): string { return 'atom#' + this.base + '#' + String(this.serial); }
}

export type Name = SUnderscore | SName | SGlobal | SModuleGlobal | STypeGlobal | SAtom;

export function isSUnderscore(x: any): x is SUnderscore { return x instanceof SUnderscore; }
export function isSName(x: any): x is SName { return x instanceof SName; }
export function isSGlobal(x: any): x is SGlobal { return x instanceof SGlobal; }
export function isSModuleGlobal(x: any): x is SModuleGlobal { return x instanceof SModuleGlobal; }
export function isSTypeGlobal(x: any): x is STypeGlobal { return x instanceof STypeGlobal; }
export function isSAtom(x: any): x is SAtom { return x instanceof SAtom; }
export function isName(x: any): x is Name { return x instanceof NameBase; }

export interface NameMaker {
  reset: () => void;
  sUnderscore: (l: Loc) => SUnderscore;
  sName: (l: Loc, s: string) => SName;
  sGlobal: (s: string) => SGlobal;
  sModuleGlobal: (s: string) => SModuleGlobal;
  sTypeGlobal: (s: string) => STypeGlobal;
  makeAtom: (base: string) => SAtom;
  isSUnderscore: (x: any) => boolean;
  isSName: (x: any) => boolean;
  isSGlobal: (x: any) => boolean;
  isSModuleGlobal: (x: any) => boolean;
  isSAtom: (x: any) => boolean;
}

export function MakeName(start: number): NameMaker {
  let count = start;
  function atom(base: string): SAtom {
    count = 1 + count;
    return new SAtom(base, count);
  }
  return {
    reset: () => { count = start; },
    sUnderscore: (l: Loc) => new SUnderscore(l),
    sName: (l: Loc, s: string) => new SName(l, s),
    sGlobal: (s: string) => new SGlobal(s),
    sModuleGlobal: (s: string) => new SModuleGlobal(s),
    sTypeGlobal: (s: string) => new STypeGlobal(s),
    makeAtom: atom,
    isSUnderscore: isSUnderscore,
    isSName: isSName,
    isSGlobal: isSGlobal,
    isSModuleGlobal: isSModuleGlobal,
    isSAtom: isSAtom,
  };
}

export const globalNames: NameMaker = MakeName(0);

// ---------- AppInfo ----------

export abstract class AppInfoBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class AppInfoC extends AppInfoBase {
  get $name(): 'app-info-c' { return 'app-info-c'; }
  constructor(public isRecursive: boolean, public isTail: boolean) { super(); }
  visit(visitor: any): any { return visitor.appInfoC(this); }
}

export type AppInfo = AppInfoC;
export function isAppInfoC(x: any): x is AppInfoC { return x instanceof AppInfoC; }
export function isAppInfo(x: any): x is AppInfo { return x instanceof AppInfoBase; }

// ---------- PrimAppInfo ----------

export abstract class PrimAppInfoBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class PrimAppInfoC extends PrimAppInfoBase {
  get $name(): 'prim-app-info-c' { return 'prim-app-info-c'; }
  constructor(public needsStep: boolean) { super(); }
  visit(visitor: any): any { return visitor.primAppInfoC(this); }
}

export type PrimAppInfo = PrimAppInfoC;
export function isPrimAppInfoC(x: any): x is PrimAppInfoC { return x instanceof PrimAppInfoC; }
export function isPrimAppInfo(x: any): x is PrimAppInfo { return x instanceof PrimAppInfoBase; }

export function funlamTosource(funtype: any, name: any, params: Name[], args: Bind[],
    ann: Ann, doc: string, body: Expr, _check: Expr | undefined, blocky: boolean): any {
  const typarams =
    params.length === 0 ? PP.mtDoc
    : PP.surroundSeparate(INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
        params.map((p) => p.tosource()));
  const argList = PP.nest(INDENT,
    PP.surroundSeparate(INDENT, 0, PP.lparen.append(PP.rparen), PP.lparen, PP.commabreak, PP.rparen,
      args.map((a) => a.tosource())));
  const fname =
    PP.isMtDoc(name) ? funtype.append(typarams)
    : PP.group(funtype.append(breakOne).append(name).append(typarams));
  const fann =
    (isABlank(ann) || (ann as any) == null) ? PP.mtDoc
    : breakOne.append(strArrowspace).append(ann.tosource());
  const fblockycolon =
    blocky ? breakOne.append(strBlock)
    : strColon;
  const header = PP.group(fname.append(argList).append(fann).append(fblockycolon));
  const checker = _check === undefined ? PP.mtDoc : _check.tosource();
  const footer =
    PP.isMtDoc(checker) ? strEnd
    : PP.surround(INDENT, 1, strWhere, checker, strEnd);
  const docstr =
    ((doc as any) == null || doc === '') ? PP.mtDoc
    : strDoc.append(PP.str(toreprStr(doc))).append(PP.hardline);
  return PP.surround(INDENT, 1, header, docstr.append(body.tosource()), footer);
}

export function blockyColon(blocky: boolean): any {
  if (blocky) { return breakOne.append(strBlock); } else { return strColon; }
}

// ---------- Use ----------

export abstract class UseBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SUse extends UseBase {
  get $name(): 's-use' { return 's-use'; }
  constructor(public l: Loc, public n: Name, public mod: ImportType) { super(); }
  visit(visitor: any): any { return visitor.sUse(this); }
  label(): string { return 's-use'; }
  tosource(): any {
    return PP.flow([strUse, this.n.tosource(), this.mod.tosource()]);
  }
}

export type Use = SUse;
export function isSUse(x: any): x is SUse { return x instanceof SUse; }
export function isUse(x: any): x is Use { return x instanceof UseBase; }

// ---------- Program ----------

export abstract class ProgramBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SProgram extends ProgramBase {
  get $name(): 's-program' { return 's-program'; }
  constructor(public l: Loc, public _use: Use | undefined, public _provide: Provide,
      public providedTypes: ProvideTypes, public provides: ProvideBlock[],
      public imports: Import[], public block: Expr) { super(); }
  visit(visitor: any): any { return visitor.sProgram(this); }
  label(): string { return 's-program'; }
  tosource(): any {
    const parts = [
      this._provide.tosource(),
      this.providedTypes.tosource(),
      ...this.provides.map((p) => p.tosource()),
      ...this.imports.map((i) => i.tosource()),
      this.block.tosource(),
    ];
    const withUse = this._use === undefined ? parts : [this._use.tosource(), ...parts];
    return PP.group(PP.vert(withUse));
  }
}

export type Program = SProgram;
export function isSProgram(x: any): x is SProgram { return x instanceof SProgram; }
export function isProgram(x: any): x is Program { return x instanceof ProgramBase; }

// ---------- Import ----------

export abstract class ImportBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SInclude extends ImportBase {
  get $name(): 's-include' { return 's-include'; }
  constructor(public l: Loc, public mod: ImportType) { super(); }
  visit(visitor: any): any { return visitor.sInclude(this); }
  label(): string { return 's-include'; }
  tosource(): any {
    return PP.flow([strInclude, this.mod.tosource()]);
  }
}

export class SIncludeFrom extends ImportBase {
  get $name(): 's-include-from' { return 's-include-from'; }
  constructor(public l: Loc, public mod: Name[], public specs: IncludeSpec[]) { super(); }
  visit(visitor: any): any { return visitor.sIncludeFrom(this); }
  label(): string { return 's-include'; }
  tosource(): any {
    return PP.softSurround(INDENT, 1,
      PP.flow([strInclude, strFrom, PP.separate(strPeriod, this.mod.map((m) => m.tosource())), strColon]),
      PP.separate(PP.commabreak, this.specs.map((s) => s.tosource())),
      strEnd);
  }
}

export class SImport extends ImportBase {
  get $name(): 's-import' { return 's-import'; }
  constructor(public l: Loc, public file: ImportType, public name: Name) { super(); }
  visit(visitor: any): any { return visitor.sImport(this); }
  label(): string { return 's-import'; }
  tosource(): any {
    return PP.flow([strImport, this.file.tosource(), strAs, this.name.tosource()]);
  }
}

export class SImportTypes extends ImportBase {
  get $name(): 's-import-types' { return 's-import-types'; }
  constructor(public l: Loc, public file: ImportType, public name: Name, public types: Name) { super(); }
  visit(visitor: any): any { return visitor.sImportTypes(this); }
  label(): string { return 's-import-types'; }
  tosource(): any {
    return PP.flow([strImport, this.file.tosource(), strAs, this.name.tosource(), PP.comma, this.types.tosource()]);
  }
}

export class SImportFields extends ImportBase {
  get $name(): 's-import-fields' { return 's-import-fields'; }
  constructor(public l: Loc, public fields: Name[], public file: ImportType) { super(); }
  visit(visitor: any): any { return visitor.sImportFields(this); }
  label(): string { return 's-import-fields'; }
  tosource(): any {
    return PP.flow([strImport,
      PP.flowMap(PP.commabreak, (f: Name) => f.tosource(), this.fields),
      strFrom, this.file.tosource()]);
  }
}

export type Import = SInclude | SIncludeFrom | SImport | SImportTypes | SImportFields;
export function isSInclude(x: any): x is SInclude { return x instanceof SInclude; }
export function isSIncludeFrom(x: any): x is SIncludeFrom { return x instanceof SIncludeFrom; }
export function isSImport(x: any): x is SImport { return x instanceof SImport; }
export function isSImportTypes(x: any): x is SImportTypes { return x instanceof SImportTypes; }
export function isSImportFields(x: any): x is SImportFields { return x instanceof SImportFields; }
export function isImport(x: any): x is Import { return x instanceof ImportBase; }

// ---------- IncludeSpec ----------

export abstract class IncludeSpecBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SIncludeName extends IncludeSpecBase {
  get $name(): 's-include-name' { return 's-include-name'; }
  constructor(public l: Loc, public nameSpec: NameSpec) { super(); }
  visit(visitor: any): any { return visitor.sIncludeName(this); }
  label(): string { return 's-include-name'; }
  tosource(): any { return this.nameSpec.tosource(); }
}

export class SIncludeData extends IncludeSpecBase {
  get $name(): 's-include-data' { return 's-include-data'; }
  constructor(public l: Loc, public nameSpec: NameSpec, public hidden: Name[]) { super(); }
  visit(visitor: any): any { return visitor.sIncludeData(this); }
  label(): string { return 's-include-data'; }
  tosource(): any {
    const hidden =
      this.hidden.length === 0 ? []
      : [PP.str('hiding'), PP.parens(PP.separate(PP.str(','), this.hidden.map((h) => h.tosource())))];
    return PP.flow([strData.append(this.nameSpec.tosource()), ...hidden]);
  }
}

export class SIncludeType extends IncludeSpecBase {
  get $name(): 's-include-type' { return 's-include-type'; }
  constructor(public l: Loc, public nameSpec: NameSpec) { super(); }
  visit(visitor: any): any { return visitor.sIncludeType(this); }
  label(): string { return 's-include-type'; }
  tosource(): any { return strType.append(this.nameSpec.tosource()); }
}

export class SIncludeModule extends IncludeSpecBase {
  get $name(): 's-include-module' { return 's-include-module'; }
  constructor(public l: Loc, public nameSpec: NameSpec) { super(); }
  visit(visitor: any): any { return visitor.sIncludeModule(this); }
  label(): string { return 's-include-module'; }
  tosource(): any { return strModule.append(this.nameSpec.tosource()); }
}

export type IncludeSpec = SIncludeName | SIncludeData | SIncludeType | SIncludeModule;
export function isSIncludeName(x: any): x is SIncludeName { return x instanceof SIncludeName; }
export function isSIncludeData(x: any): x is SIncludeData { return x instanceof SIncludeData; }
export function isSIncludeType(x: any): x is SIncludeType { return x instanceof SIncludeType; }
export function isSIncludeModule(x: any): x is SIncludeModule { return x instanceof SIncludeModule; }
export function isIncludeSpec(x: any): x is IncludeSpec { return x instanceof IncludeSpecBase; }

// ---------- ProvidedModule ----------

export abstract class ProvidedModuleBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class PModule extends ProvidedModuleBase {
  get $name(): 'p-module' { return 'p-module'; }
  constructor(public l: Loc, public name: string, public v: Name, public uri: string) { super(); }
  visit(visitor: any): any { return visitor.pModule(this); }
  label(): string { return 'p-module'; }
  tosource(): any {
    // TODO: FIX! (carried from ast.arr; it reads self.ann, which p-module lacks)
    return PP.infix(INDENT, 1, strColoncolon, PP.str(this.v.toname()), (this as any).ann.tosource());
  }
}

export type ProvidedModule = PModule;
export function isPModule(x: any): x is PModule { return x instanceof PModule; }
export function isProvidedModule(x: any): x is ProvidedModule { return x instanceof ProvidedModuleBase; }

// ---------- ProvidedValue ----------

export abstract class ProvidedValueBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

// INVARIANT(joe): all a-names in Ann are defined in the lists of
// ProvidedAlias or ProvidedDatatype
export class PValue extends ProvidedValueBase {
  get $name(): 'p-value' { return 'p-value'; }
  constructor(public l: Loc, public v: Name, public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.pValue(this); }
  label(): string { return 'p-value'; }
  tosource(): any {
    if (isABlank(this.ann)) { return PP.str(this.v.toname()); }
    else { return PP.infix(INDENT, 1, strColoncolon, PP.str(this.v.toname()), this.ann.tosource()); }
  }
}

export type ProvidedValue = PValue;
export function isPValue(x: any): x is PValue { return x instanceof PValue; }
export function isProvidedValue(x: any): x is ProvidedValue { return x instanceof ProvidedValueBase; }

// ---------- ProvidedAlias ----------

export abstract class ProvidedAliasBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class PAlias extends ProvidedAliasBase {
  get $name(): 'p-alias' { return 'p-alias'; }
  constructor(public l: Loc, public inName: Name, public outName: Name,
      public mod: ImportType | undefined) { super(); }
  visit(visitor: any): any { return visitor.pAlias(this); }
  label(): string { return 'p-alias'; }
  tosource(): any {
    return PP.infix(INDENT, 1, strAs, PP.str(this.inName.toname()), PP.str(this.outName.toname()));
  }
}

export type ProvidedAlias = PAlias;
export function isPAlias(x: any): x is PAlias { return x instanceof PAlias; }
export function isProvidedAlias(x: any): x is ProvidedAlias { return x instanceof ProvidedAliasBase; }

// ---------- ProvidedDatatype ----------

export abstract class ProvidedDatatypeBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class PData extends ProvidedDatatypeBase {
  get $name(): 'p-data' { return 'p-data'; }
  constructor(public l: Loc, public d: Name, public mod: ImportType | undefined) { super(); }
  visit(visitor: any): any { return visitor.pData(this); }
  label(): string { return 'p-data'; }
  tosource(): any {
    return PP.str(this.d.toname());
  }
}

export type ProvidedDatatype = PData;
export function isPData(x: any): x is PData { return x instanceof PData; }
export function isProvidedDatatype(x: any): x is ProvidedDatatype { return x instanceof ProvidedDatatypeBase; }

// ---------- Provide ----------

export abstract class ProvideBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SProvide extends ProvideBase {
  get $name(): 's-provide' { return 's-provide'; }
  constructor(public l: Loc, public block: Expr) { super(); }
  visit(visitor: any): any { return visitor.sProvide(this); }
  label(): string { return 's-provide'; }
  tosource(): any {
    return PP.softSurround(INDENT, 1, strProvide,
      this.block.tosource(), strEnd);
  }
}

export class SProvideAll extends ProvideBase {
  get $name(): 's-provide-all' { return 's-provide-all'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sProvideAll(this); }
  label(): string { return 's-provide-all'; }
  tosource(): any { return strProvideStar; }
}

export class SProvideNone extends ProvideBase {
  get $name(): 's-provide-none' { return 's-provide-none'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sProvideNone(this); }
  label(): string { return 's-provide-none'; }
  tosource(): any { return PP.mtDoc; }
}

export type Provide = SProvide | SProvideAll | SProvideNone;
export function isSProvide(x: any): x is SProvide { return x instanceof SProvide; }
export function isSProvideAll(x: any): x is SProvideAll { return x instanceof SProvideAll; }
export function isSProvideNone(x: any): x is SProvideNone { return x instanceof SProvideNone; }
export function isProvide(x: any): x is Provide { return x instanceof ProvideBase; }

// ---------- ProvideBlock ----------

export abstract class ProvideBlockBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SProvideBlock extends ProvideBlockBase {
  get $name(): 's-provide-block' { return 's-provide-block'; }
  constructor(public l: Loc, public path: Name[], public specs: ProvideSpec[]) { super(); }
  visit(visitor: any): any { return visitor.sProvideBlock(this); }
  label(): string { return 's-provide-block'; }
  tosource(): any {
    const start =
      this.path.length === 0
        ? PP.str('provide:')
        : PP.flow([PP.str('provide from'), PP.separate(PP.str('.'), this.path.map((p) => p.tosource()))]).append(strColon);
    return PP.surroundSeparate(INDENT, 1, start.append(strSpace).append(strEnd), start, PP.commabreak, strEnd,
      this.specs.map((s) => s.tosource()));
  }
}

export type ProvideBlock = SProvideBlock;
export function isSProvideBlock(x: any): x is SProvideBlock { return x instanceof SProvideBlock; }
export function isProvideBlock(x: any): x is ProvideBlock { return x instanceof ProvideBlockBase; }

// ---------- ProvideSpec ----------

export abstract class ProvideSpecBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SProvideName extends ProvideSpecBase {
  get $name(): 's-provide-name' { return 's-provide-name'; }
  constructor(public l: Loc, public nameSpec: NameSpec) { super(); }
  visit(visitor: any): any { return visitor.sProvideName(this); }
  label(): string { return 's-provide-name'; }
  tosource(): any { return this.nameSpec.tosource(); }
}

export class SProvideData extends ProvideSpecBase {
  get $name(): 's-provide-data' { return 's-provide-data'; }
  constructor(public l: Loc, public nameSpec: NameSpec, public hidden: Name[]) { super(); }
  visit(visitor: any): any { return visitor.sProvideData(this); }
  label(): string { return 's-provide-data'; }
  tosource(): any {
    const hidden =
      this.hidden.length === 0 ? []
      : [PP.str('hiding'), PP.parens(PP.separate(PP.str(','), this.hidden.map((h) => h.tosource())))];
    return PP.flow([PP.str('data'), this.nameSpec.tosource(), ...hidden]);
  }
}

export class SProvideType extends ProvideSpecBase {
  get $name(): 's-provide-type' { return 's-provide-type'; }
  constructor(public l: Loc, public nameSpec: NameSpec) { super(); }
  visit(visitor: any): any { return visitor.sProvideType(this); }
  label(): string { return 's-provide-type'; }
  tosource(): any { return PP.flow([PP.str('type'), this.nameSpec.tosource()]); }
}

export class SProvideModule extends ProvideSpecBase {
  get $name(): 's-provide-module' { return 's-provide-module'; }
  constructor(public l: Loc, public nameSpec: NameSpec) { super(); }
  visit(visitor: any): any { return visitor.sProvideModule(this); }
  label(): string { return 's-provide-module'; }
  tosource(): any { return PP.flow([PP.str('module'), this.nameSpec.tosource()]); }
}

export type ProvideSpec = SProvideName | SProvideData | SProvideType | SProvideModule;
export function isSProvideName(x: any): x is SProvideName { return x instanceof SProvideName; }
export function isSProvideData(x: any): x is SProvideData { return x instanceof SProvideData; }
export function isSProvideType(x: any): x is SProvideType { return x instanceof SProvideType; }
export function isSProvideModule(x: any): x is SProvideModule { return x instanceof SProvideModule; }
export function isProvideSpec(x: any): x is ProvideSpec { return x instanceof ProvideSpecBase; }

// ---------- NameSpec ----------

export abstract class NameSpecBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SStar extends NameSpecBase {
  get $name(): 's-star' { return 's-star'; }
  constructor(public l: Loc, public hidden: Name[]) { super(); }
  visit(visitor: any): any { return visitor.sStar(this); }
  label(): string { return 's-star'; }
  tosource(): any {
    const hidden =
      this.hidden.length === 0 ? []
      : [PP.str('hiding'), PP.parens(PP.separate(PP.str(','), this.hidden.map((h) => h.tosource())))];
    return PP.flow([PP.str('*'), ...hidden]);
  }
}

export class SModuleRef extends NameSpecBase {
  get $name(): 's-module-ref' { return 's-module-ref'; }
  constructor(public l: Loc, public path: Name[], public asName: Name | undefined) { super(); }
  visit(visitor: any): any { return visitor.sModuleRef(this); }
  label(): string { return 's-module-ref'; }
  tosource(): any {
    if (this.asName === undefined) {
      return PP.flow([PP.separate(PP.str('.'), this.path.map((p) => p.tosource()))]);
    } else {
      return PP.flow([PP.separate(PP.str('.'), this.path.map((p) => p.tosource())), PP.str('as'), this.asName.tosource()]);
    }
  }
}

export class SRemoteRef extends NameSpecBase {
  get $name(): 's-remote-ref' { return 's-remote-ref'; }
  constructor(public l: Loc, public uri: string, public name: Name, public asName: Name) { super(); }
  visit(visitor: any): any { return visitor.sRemoteRef(this); }
  label(): string { return 's-remote-ref'; }
  tosource(): any {
    return PP.flow([this.name.tosource(), PP.str('@').append(PP.str(this.uri)), PP.str('as'), this.asName.tosource()]);
  }
}

export class SLocalRef extends NameSpecBase {
  get $name(): 's-local-ref' { return 's-local-ref'; }
  constructor(public l: Loc, public name: Name, public asName: Name) { super(); }
  visit(visitor: any): any { return visitor.sLocalRef(this); }
  label(): string { return 's-local-ref'; }
  tosource(): any {
    return PP.flow([this.name.tosource(), PP.str('as'), this.asName.tosource()]);
  }
}

export type NameSpec = SStar | SModuleRef | SRemoteRef | SLocalRef;
export function isSStar(x: any): x is SStar { return x instanceof SStar; }
export function isSModuleRef(x: any): x is SModuleRef { return x instanceof SModuleRef; }
export function isSRemoteRef(x: any): x is SRemoteRef { return x instanceof SRemoteRef; }
export function isSLocalRef(x: any): x is SLocalRef { return x instanceof SLocalRef; }
export function isNameSpec(x: any): x is NameSpec { return x instanceof NameSpecBase; }

// ---------- ProvideTypes ----------

export abstract class ProvideTypesBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SProvideTypes extends ProvideTypesBase {
  get $name(): 's-provide-types' { return 's-provide-types'; }
  constructor(public l: Loc, public ann: AField[]) { super(); }
  visit(visitor: any): any { return visitor.sProvideTypes(this); }
  label(): string { return 'a-provide-type'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, strProvideTypes.append(breakOne).append(PP.lbrace).append(PP.rbrace),
      strProvideTypes.append(breakOne).append(PP.lbrace), PP.commabreak, PP.rbrace,
      this.ann.map((a) => a.tosource()));
  }
}

export class SProvideTypesAll extends ProvideTypesBase {
  get $name(): 's-provide-types-all' { return 's-provide-types-all'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sProvideTypesAll(this); }
  label(): string { return 's-provide-types-all'; }
  tosource(): any { return strProvideTypesStar; }
}

export class SProvideTypesNone extends ProvideTypesBase {
  get $name(): 's-provide-types-none' { return 's-provide-types-none'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sProvideTypesNone(this); }
  label(): string { return 's-provide-types-none'; }
  tosource(): any { return PP.mtDoc; }
}

export type ProvideTypes = SProvideTypes | SProvideTypesAll | SProvideTypesNone;
export function isSProvideTypes(x: any): x is SProvideTypes { return x instanceof SProvideTypes; }
export function isSProvideTypesAll(x: any): x is SProvideTypesAll { return x instanceof SProvideTypesAll; }
export function isSProvideTypesNone(x: any): x is SProvideTypesNone { return x instanceof SProvideTypesNone; }
export function isProvideTypes(x: any): x is ProvideTypes { return x instanceof ProvideTypesBase; }

// ---------- ImportType ----------

export abstract class ImportTypeBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SConstImport extends ImportTypeBase {
  get $name(): 's-const-import' { return 's-const-import'; }
  constructor(public l: Loc, public mod: string) { super(); }
  visit(visitor: any): any { return visitor.sConstImport(this); }
  label(): string { return 's-const-import'; }
  tosource(): any { return PP.str(this.mod); }
}

export class SSpecialImport extends ImportTypeBase {
  get $name(): 's-special-import' { return 's-special-import'; }
  constructor(public l: Loc, public kind: string, public args: string[]) { super(); }
  visit(visitor: any): any { return visitor.sSpecialImport(this); }
  label(): string { return 's-special-import'; }
  tosource(): any {
    return PP.group(PP.str(this.kind)
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.map((a) => PP.str(toreprStr(a))))))));
  }
}

export type ImportType = SConstImport | SSpecialImport;
export function isSConstImport(x: any): x is SConstImport { return x instanceof SConstImport; }
export function isSSpecialImport(x: any): x is SSpecialImport { return x instanceof SSpecialImport; }
export function isImportType(x: any): x is ImportType { return x instanceof ImportTypeBase; }

// ---------- Hint ----------

export abstract class HintBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class HUseLoc extends HintBase {
  get $name(): 'h-use-loc' { return 'h-use-loc'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.hUseLoc(this); }
  tosource(): any { return strUseLoc.append(PP.parens(PP.str(toreprLoc(this.l)))); }
}

export type Hint = HUseLoc;
export function isHUseLoc(x: any): x is HUseLoc { return x instanceof HUseLoc; }
export function isHint(x: any): x is Hint { return x instanceof HintBase; }

// ---------- LetBind ----------

export abstract class LetBindBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SLetBind extends LetBindBase {
  get $name(): 's-let-bind' { return 's-let-bind'; }
  constructor(public l: Loc, public b: Bind, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sLetBind(this); }
  tosource(): any {
    return PP.group(PP.nest(INDENT, this.b.tosource().append(strSpaceequal).append(breakOne).append(this.value.tosource())));
  }
}

export class SVarBind extends LetBindBase {
  get $name(): 's-var-bind' { return 's-var-bind'; }
  constructor(public l: Loc, public b: Bind, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sVarBind(this); }
  tosource(): any {
    return PP.group(PP.nest(INDENT, PP.str('var ').append(this.b.tosource()).append(strSpaceequal).append(breakOne).append(this.value.tosource())));
  }
}

export type LetBind = SLetBind | SVarBind;
export function isSLetBind(x: any): x is SLetBind { return x instanceof SLetBind; }
export function isSVarBind(x: any): x is SVarBind { return x instanceof SVarBind; }
export function isLetBind(x: any): x is LetBind { return x instanceof LetBindBase; }

// ---------- LetrecBind ----------

export abstract class LetrecBindBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SLetrecBind extends LetrecBindBase {
  get $name(): 's-letrec-bind' { return 's-letrec-bind'; }
  constructor(public l: Loc, public b: Bind, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sLetrecBind(this); }
  tosource(): any {
    return PP.group(PP.nest(INDENT, this.b.tosource().append(strSpaceequal).append(breakOne).append(this.value.tosource())));
  }
}

export type LetrecBind = SLetrecBind;
export function isSLetrecBind(x: any): x is SLetrecBind { return x instanceof SLetrecBind; }
export function isLetrecBind(x: any): x is LetrecBind { return x instanceof LetrecBindBase; }

// ---------- TypeLetBind ----------

export abstract class TypeLetBindBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class STypeBind extends TypeLetBindBase {
  get $name(): 's-type-bind' { return 's-type-bind'; }
  constructor(public l: Loc, public name: Name, public params: Name[], public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sTypeBind(this); }
  label(): string { return 's-type-bind'; }
  tosource(): any {
    const params = PP.surroundSeparate(2 * INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
      this.params.map((p) => p.tosource()));
    return PP.group(PP.nest(INDENT, this.name.tosource().append(params).append(strSpaceequal).append(breakOne).append(this.ann.tosource())));
  }
}

export class SNewtypeBind extends TypeLetBindBase {
  get $name(): 's-newtype-bind' { return 's-newtype-bind'; }
  constructor(public l: Loc, public name: Name, public namet: Name) { super(); }
  visit(visitor: any): any { return visitor.sNewtypeBind(this); }
  label(): string { return 's-newtype-bind'; }
  tosource(): any {
    return PP.group(PP.nest(INDENT, strNewtype.append(this.name.tosource())
      .append(breakOne).append(strAs)
      .append(breakOne).append(this.namet.tosource())));
  }
}

export type TypeLetBind = STypeBind | SNewtypeBind;
export function isSTypeBind(x: any): x is STypeBind { return x instanceof STypeBind; }
export function isSNewtypeBind(x: any): x is SNewtypeBind { return x instanceof SNewtypeBind; }
export function isTypeLetBind(x: any): x is TypeLetBind { return x instanceof TypeLetBindBase; }

// ---------- DefinedModule ----------

export abstract class DefinedModuleBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SDefinedModule extends DefinedModuleBase {
  get $name(): 's-defined-module' { return 's-defined-module'; }
  constructor(public name: string, public value: Name, public uri: string) { super(); }
  visit(visitor: any): any { return visitor.sDefinedModule(this); }
  label(): string { return 's-defined-module'; }
  tosource(): any {
    return PP.infix(INDENT, 1, strColon, PP.str(this.name), PP.str(this.uri));
  }
}

export type DefinedModule = SDefinedModule;
export function isSDefinedModule(x: any): x is SDefinedModule { return x instanceof SDefinedModule; }
export function isDefinedModule(x: any): x is DefinedModule { return x instanceof DefinedModuleBase; }

// ---------- DefinedValue ----------

export abstract class DefinedValueBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SDefinedValue extends DefinedValueBase {
  get $name(): 's-defined-value' { return 's-defined-value'; }
  constructor(public name: string, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sDefinedValue(this); }
  label(): string { return 's-defined-value'; }
  tosource(): any {
    return PP.infix(INDENT, 1, strColon, PP.str(this.name), this.value.tosource());
  }
}

export class SDefinedVar extends DefinedValueBase {
  get $name(): 's-defined-var' { return 's-defined-var'; }
  constructor(public name: string, public id: Name) { super(); }
  visit(visitor: any): any { return visitor.sDefinedVar(this); }
  label(): string { return 's-defined-var'; }
  tosource(): any {
    return PP.infix(INDENT, 1, strColon, PP.str(this.name), PP.str(this.id.toname()));
  }
}

export type DefinedValue = SDefinedValue | SDefinedVar;
export function isSDefinedValue(x: any): x is SDefinedValue { return x instanceof SDefinedValue; }
export function isSDefinedVar(x: any): x is SDefinedVar { return x instanceof SDefinedVar; }
export function isDefinedValue(x: any): x is DefinedValue { return x instanceof DefinedValueBase; }

// ---------- DefinedType ----------

export abstract class DefinedTypeBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SDefinedType extends DefinedTypeBase {
  get $name(): 's-defined-type' { return 's-defined-type'; }
  constructor(public name: string, public typ: Ann) { super(); }
  visit(visitor: any): any { return visitor.sDefinedType(this); }
  label(): string { return 's-defined-type'; }
  tosource(): any {
    return PP.infix(INDENT, 1, strColoncolon, PP.str(this.name), this.typ.tosource());
  }
}

export type DefinedType = SDefinedType;
export function isSDefinedType(x: any): x is SDefinedType { return x instanceof SDefinedType; }
export function isDefinedType(x: any): x is DefinedType { return x instanceof DefinedTypeBase; }

export function isBinder(expr: Expr): boolean {
  return isSLet(expr) || isSFun(expr) || isSVar(expr) || isSRec(expr);
}

// ---------- Expr ----------

export abstract class ExprBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SModule extends ExprBase {
  get $name(): 's-module' { return 's-module'; }
  constructor(public l: Loc, public answer: Expr, public definedModules: DefinedModule[],
      public definedValues: DefinedValue[], public definedTypes: DefinedType[],
      public checks: Expr) { super(); }
  visit(visitor: any): any { return visitor.sModule(this); }
  label(): string { return 's-module'; }
  tosource(): any {
    return PP.str('Module').append(PP.parens(PP.flowMap(PP.commabreak, (x: any) => x, [
      PP.infix(INDENT, 1, strColon, PP.str('Answer'), this.answer.tosource()),
      PP.infix(INDENT, 1, strColon, PP.str('DefinedValues'),
        PP.brackets(PP.flowMap(PP.commabreak, (d: DefinedValue) => d.tosource(), this.definedValues))),
      PP.infix(INDENT, 1, strColon, PP.str('DefinedTypes'),
        PP.brackets(PP.flowMap(PP.commabreak, (d: DefinedType) => d.tosource(), this.definedTypes))),
      PP.infix(INDENT, 1, strColon, PP.str('checks'), this.checks.tosource())])));
  }
}

export class STemplate extends ExprBase {
  get $name(): 's-template' { return 's-template'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sTemplate(this); }
  label(): string { return 's-template'; }
  tosource(): any { return PP.str('...'); }
}

export class STypeLetExpr extends ExprBase {
  get $name(): 's-type-let-expr' { return 's-type-let-expr'; }
  constructor(public l: Loc, public binds: TypeLetBind[], public body: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sTypeLetExpr(this); }
  label(): string { return 's-type-let'; }
  tosource(): any {
    const header = PP.surroundSeparate(2 * INDENT, 1, strTypeLet, strTypeLet.append(PP.str(' ')), PP.commabreak, PP.mtDoc,
        this.binds.map((b) => b.tosource()))
      .append(blockyColon(this.blocky));
    return PP.surround(INDENT, 1, header, this.body.tosource(), strEnd);
  }
}

export class SLetExpr extends ExprBase {
  get $name(): 's-let-expr' { return 's-let-expr'; }
  constructor(public l: Loc, public binds: LetBind[], public body: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sLetExpr(this); }
  label(): string { return 's-let'; }
  tosource(): any {
    const header = PP.surroundSeparate(2 * INDENT, 1, strLet, strLet.append(PP.str(' ')), PP.commabreak, PP.mtDoc,
        this.binds.map((b) => b.tosource()))
      .append(blockyColon(this.blocky));
    return PP.surround(INDENT, 1, header, this.body.tosource(), strEnd);
  }
}

export class SLetrec extends ExprBase {
  get $name(): 's-letrec' { return 's-letrec'; }
  constructor(public l: Loc, public binds: LetrecBind[], public body: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sLetrec(this); }
  label(): string { return 's-letrec'; }
  tosource(): any {
    const header = PP.surroundSeparate(2 * INDENT, 1, strLetrec, strLetrec.append(PP.str(' ')), PP.commabreak, PP.mtDoc,
        this.binds.map((b) => b.tosource()))
      .append(blockyColon(this.blocky));
    return PP.surround(INDENT, 1, header, this.body.tosource(), strEnd);
  }
}

export class SHintExp extends ExprBase {
  get $name(): 's-hint-exp' { return 's-hint-exp'; }
  constructor(public l: Loc, public hints: Hint[], public exp: Expr) { super(); }
  visit(visitor: any): any { return visitor.sHintExp(this); }
  label(): string { return 's-hint-exp'; }
  tosource(): any {
    // (sic: ast.arr reads self.e here, though the field is named exp)
    return PP.flowMap(PP.hardline, (h: Hint) => strComment.append(h.tosource()), this.hints).append(PP.hardline)
      .append((this as any).e.tosource());
  }
}

export class SInstantiate extends ExprBase {
  get $name(): 's-instantiate' { return 's-instantiate'; }
  constructor(public l: Loc, public expr: Expr, public params: Ann[]) { super(); }
  visit(visitor: any): any { return visitor.sInstantiate(this); }
  label(): string { return 's-instantiate'; }
  tosource(): any {
    return PP.group(this.expr.tosource().append(
      PP.surroundSeparate(INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
        this.params.map((p) => p.tosource()))));
  }
}

export class SBlock extends ExprBase {
  get $name(): 's-block' { return 's-block'; }
  constructor(public l: Loc, public stmts: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sBlock(this); }
  label(): string { return 's-block'; }
  tosource(): any {
    return PP.flowMap(PP.hardline, (s: Expr) => s.tosource(), this.stmts);
  }
}

export class SUserBlock extends ExprBase {
  get $name(): 's-user-block' { return 's-user-block'; }
  constructor(public l: Loc, public body: Expr) { super(); }
  visit(visitor: any): any { return visitor.sUserBlock(this); }
  label(): string { return 's-user-block'; }
  tosource(): any {
    return PP.surround(INDENT, 1, strBlock, this.body.tosource(), strEnd);
  }
}

export class SScopeLet {
  get $name(): 's-scope-let' { return 's-scope-let'; }
  constructor(public l: Loc, public binds: LetBind[]) {}
  visit(visitor: any): any { return visitor.sScopeLet(this); }
  label(): string { return 's-scope-let'; }
  tosource(): any {
    return PP.surroundSeparate(2 * INDENT, 1, strLet, strLet.append(PP.str(' ')), PP.commabreak, PP.mtDoc,
      this.binds.map((b) => b.tosource())).append(PP.str(':'));
  }
}

export class SScopeTypeLet {
  get $name(): 's-scope-type-let' { return 's-scope-type-let'; }
  constructor(public l: Loc, public binds: TypeLetBind[]) {}
  visit(visitor: any): any { return visitor.sScopeTypeLet(this); }
  label(): string { return 's-scope-type-let'; }
  tosource(): any {
    return PP.surroundSeparate(2 * INDENT, 1, strTypeLet, strTypeLet.append(PP.str(' ')), PP.commabreak, PP.mtDoc,
      this.binds.map((b) => b.tosource())).append(PP.str(':'));
  }
}

export class SScopeLetrec {
  get $name(): 's-scope-letrec' { return 's-scope-letrec'; }
  constructor(public l: Loc, public binds: LetrecBind[]) {}
  visit(visitor: any): any { return visitor.sScopeLetrec(this); }
  label(): string { return 's-scope-letrec'; }
  tosource(): any {
    return PP.surroundSeparate(2 * INDENT, 1, strLetrec, strLetrec.append(PP.str(' ')), PP.commabreak, PP.mtDoc,
      this.binds.map((b) => b.tosource())).append(PP.str(':'));
  }
}

// A plain (non-binding) statement between binding groups. The wrapper
// exists so ScopeEntry is a closed union of scope-entry nodes -- a bare
// Expr member would make every consumer's else-branch a silent
// catch-all.
export class SScopeStmt {
  get $name(): 's-scope-stmt' { return 's-scope-stmt'; }
  constructor(public l: Loc, public stmt: Expr) {}
  visit(visitor: any): any { return visitor.sScopeStmt(this); }
  label(): string { return 's-scope-stmt'; }
  tosource(): any { return this.stmt.tosource(); }
}

export type ScopeEntry = SScopeLet | SScopeTypeLet | SScopeLetrec | SScopeStmt;

export class SScopeBlock extends ExprBase {
  get $name(): 's-scope-block' { return 's-scope-block'; }
  constructor(public l: Loc, public entries: ScopeEntry[], public tail: Expr) { super(); }
  visit(visitor: any): any { return visitor.sScopeBlock(this); }
  label(): string { return 's-scope-block'; }
  tosource(): any {
    let doc = PP.mtDoc;
    for (const e of this.entries) {
      doc = doc.append(e.tosource()).append(PP.hardline);
    }
    return doc.append(this.tail.tosource());
  }
}

export function sScopeLetBlock(l: Loc, binds: LetBind[], body: Expr): SScopeBlock {
  return new SScopeBlock(l, [new SScopeLet(l, binds)], body);
}

export class SFun extends ExprBase {
  get $name(): 's-fun' { return 's-fun'; }
  constructor(public l: Loc, public name: string, public params: Name[], public args: Bind[],
      public ann: Ann, public doc: string, public body: Expr,
      public _checkLoc: Loc | undefined, public _check: Expr | undefined,
      public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sFun(this); }
  label(): string { return 's-fun'; }
  tosource(): any {
    return funlamTosource(strFun,
      PP.str(this.name), this.params, this.args, this.ann, this.doc, this.body, this._check, this.blocky);
  }
}

export class SType extends ExprBase {
  get $name(): 's-type' { return 's-type'; }
  constructor(public l: Loc, public name: Name, public params: Name[], public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sType(this); }
  label(): string { return 's-type'; }
  tosource(): any {
    const params = PP.surroundSeparate(2 * INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
      this.params.map((p) => p.tosource()));
    return PP.group(PP.nest(INDENT,
      strType.append(this.name.tosource()).append(params).append(strSpaceequal).append(breakOne).append(this.ann.tosource())));
  }
}

export class SNewtype extends ExprBase {
  get $name(): 's-newtype' { return 's-newtype'; }
  constructor(public l: Loc, public name: Name, public namet: Name) { super(); }
  visit(visitor: any): any { return visitor.sNewtype(this); }
  label(): string { return 's-newtype'; }
  tosource(): any {
    return PP.group(PP.nest(INDENT, strNewtype.append(this.name.tosource())
      .append(breakOne).append(strAs)
      .append(breakOne).append(this.namet.tosource())));
  }
}

export class SVar extends ExprBase {
  get $name(): 's-var' { return 's-var'; }
  constructor(public l: Loc, public name: Bind, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sVar(this); }
  label(): string { return 's-var'; }
  tosource(): any {
    return strVar
      .append(PP.group(PP.nest(INDENT, this.name.tosource()
        .append(strSpaceequal).append(breakOne).append(this.value.tosource()))));
  }
}

export class SRec extends ExprBase {
  get $name(): 's-rec' { return 's-rec'; }
  constructor(public l: Loc, public name: Bind, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sRec(this); }
  label(): string { return 's-rec'; }
  tosource(): any {
    return strRec
      .append(PP.group(PP.nest(INDENT, this.name.tosource()
        .append(strSpaceequal).append(breakOne).append(this.value.tosource()))));
  }
}

export class SLet extends ExprBase {
  get $name(): 's-let' { return 's-let'; }
  constructor(public l: Loc, public name: Bind, public value: Expr, public keywordVal: boolean) { super(); }
  visit(visitor: any): any { return visitor.sLet(this); }
  label(): string { return 's-let'; }
  tosource(): any {
    return PP.group(PP.nest(INDENT,
      (this.keywordVal ? strVal : PP.mtDoc)
        .append(this.name.tosource()).append(strSpaceequal).append(breakOne).append(this.value.tosource())));
  }
}

export class SRef extends ExprBase {
  get $name(): 's-ref' { return 's-ref'; }
  constructor(public l: Loc, public ann: Ann | undefined) { super(); }
  visit(visitor: any): any { return visitor.sRef(this); }
  label(): string { return 's-ref'; }
  tosource(): any {
    if (this.ann === undefined) {
      return PP.str('bare-ref');
    } else {
      return PP.group(PP.str('ref ').append(this.ann.tosource()));
    }
  }
}

export class SContract extends ExprBase {
  get $name(): 's-contract' { return 's-contract'; }
  constructor(public l: Loc, public name: Name, public params: Name[], public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sContract(this); }
  label(): string { return 's-contract'; }
  tosource(): any {
    const typarams =
      this.params.length === 0 ? PP.mtDoc
      : PP.surroundSeparate(INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
          this.params.map((p) => p.tosource()));
    return PP.infix(INDENT, 1, strColoncolon, this.name.tosource(), typarams.append(this.ann.tosource()));
  }
}

export class SWhen extends ExprBase {
  get $name(): 's-when' { return 's-when'; }
  constructor(public l: Loc, public test: Expr, public block: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sWhen(this); }
  label(): string { return 's-when'; }
  tosource(): any {
    return PP.softSurround(INDENT, 1,
      strWhen.append(this.test.tosource()).append(blockyColon(this.blocky)),
      this.block.tosource(),
      strEnd);
  }
}

export class SAssign extends ExprBase {
  get $name(): 's-assign' { return 's-assign'; }
  constructor(public l: Loc, public id: Name, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sAssign(this); }
  label(): string { return 's-assign'; }
  tosource(): any {
    return PP.group(PP.nest(INDENT, this.id.tosource().append(strSpacecolonequal).append(breakOne).append(this.value.tosource())));
  }
}

export class SIfPipe extends ExprBase {
  get $name(): 's-if-pipe' { return 's-if-pipe'; }
  constructor(public l: Loc, public branches: IfPipeBranch[], public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sIfPipe(this); }
  label(): string { return 's-if-pipe'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, strAsk.append(blockyColon(this.blocky)).append(strSpace).append(strEnd),
      PP.group(strAsk.append(blockyColon(this.blocky))), breakOne, strEnd,
      this.branches.map((b) => PP.group(b.tosource())));
  }
}

export class SIfPipeElse extends ExprBase {
  get $name(): 's-if-pipe-else' { return 's-if-pipe-else'; }
  constructor(public l: Loc, public branches: IfPipeBranch[], public _else: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sIfPipeElse(this); }
  label(): string { return 's-if-pipe-else'; }
  tosource(): any {
    const body = PP.separate(breakOne, this.branches.map((b) => PP.group(b.tosource())))
      .append(breakOne).append(PP.group(strPipespace.append(strOtherwisecolon).append(breakOne).append(this._else.tosource())));
    return PP.surround(INDENT, 1, PP.group(strAsk.append(blockyColon(this.blocky))), body, strEnd);
  }
}

export class SIf extends ExprBase {
  get $name(): 's-if' { return 's-if'; }
  constructor(public l: Loc, public branches: IfBranch[], public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sIf(this); }
  label(): string { return 's-if'; }
  tosource(): any {
    const firstBranch =
      this.blocky ? this.branches[0].tosourceBlocky()
      : this.branches[0].tosource();
    const firstSep =
      this.branches.length > 1 ? breakOne.append(strElsespace)
      : PP.mtDoc;
    const branches = PP.separate(breakOne.append(strElsespace),
      this.branches.slice(1).map((b) => b.tosource()));
    return PP.group(firstBranch.append(firstSep).append(branches).append(breakOne).append(strEnd));
  }
}

export class SIfElse extends ExprBase {
  get $name(): 's-if-else' { return 's-if-else'; }
  constructor(public l: Loc, public branches: IfBranch[], public _else: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sIfElse(this); }
  label(): string { return 's-if-else'; }
  tosource(): any {
    const firstBranch =
      this.blocky ? this.branches[0].tosourceBlocky()
      : this.branches[0].tosource();
    const firstSep =
      this.branches.length > 1 ? breakOne.append(strElsespace)
      : PP.mtDoc;
    const branches = PP.separate(breakOne.append(strElsespace),
      this.branches.slice(1).map((b) => b.tosource()));
    const _else = strElsecolon.append(PP.nest(INDENT, breakOne.append(this._else.tosource())));
    return PP.group(firstBranch.append(firstSep).append(branches).append(breakOne).append(_else).append(breakOne).append(strEnd));
  }
}

export class SCases extends ExprBase {
  get $name(): 's-cases' { return 's-cases'; }
  constructor(public l: Loc, public typ: Ann, public val: Expr, public branches: CasesBranch[],
      public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sCases(this); }
  label(): string { return 's-cases'; }
  branchesLoc(): Srcloc {
    const firstLoc = this.branches[0].l as Srcloc;
    const lastLoc = this.branches[this.branches.length - 1].l as Srcloc;
    return new Srcloc(
      (this.l as Srcloc).source,
      firstLoc.startLine,
      firstLoc.startColumn,
      firstLoc.startChar,
      lastLoc.endLine,
      lastLoc.endColumn,
      lastLoc.endChar);
  }
  tosource(): any {
    const header = strCases.append(PP.parens(this.typ.tosource())).append(breakOne)
      .append(this.val.tosource()).append(blockyColon(this.blocky));
    return PP.surroundSeparate(INDENT, 1, header.append(strSpace).append(strEnd),
      PP.group(header), breakOne, strEnd,
      this.branches.map((b) => PP.group(b.tosource())));
  }
}

export class SCasesElse extends ExprBase {
  get $name(): 's-cases-else' { return 's-cases-else'; }
  constructor(public l: Loc, public typ: Ann, public val: Expr, public branches: CasesBranch[],
      public _else: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sCasesElse(this); }
  label(): string { return 's-cases-else'; }
  tosource(): any {
    const header = strCases.append(PP.parens(this.typ.tosource())).append(breakOne)
      .append(this.val.tosource()).append(blockyColon(this.blocky));
    const body = PP.separate(breakOne, this.branches.map((b) => PP.group(b.tosource())))
      .append(breakOne).append(PP.group(strElsebranch.append(PP.nest(INDENT, breakOne.append(this._else.tosource())))));
    return PP.surround(INDENT, 1, PP.group(header), body, strEnd);
  }
}

export class SOp extends ExprBase {
  // This should be left-associated, always.
  get $name(): 's-op' { return 's-op'; }
  constructor(public l: Loc, public opL: Loc, public op: string, public left: Expr, public right: Expr) { super(); }
  visit(visitor: any): any { return visitor.sOp(this); }
  label(): string { return 's-op'; }
  tosource(): any {
    const collectSameOperands = (exp: Expr): Expr[] => {
      if (isSOp(exp) && exp.op === this.op) {
        return [...collectSameOperands(exp.left), ...collectSameOperands(exp.right)];
      } else {
        return [exp];
      }
    };
    const operands = [...collectSameOperands(this.left), ...collectSameOperands(this.right)];
    if (operands.length === 0) {
      return PP.mtDoc;
    } else if (operands.length === 1) {
      return operands[0].tosource();
    } else {
      const first = operands[0];
      const second = operands[1];
      const rest2 = operands.slice(2);
      const op = breakOne.append(PP.str(this.op.substring(2))).append(breakOne);
      let nested = second.tosource();
      for (const operand of rest2) {
        nested = nested.append(PP.group(op.append(operand.tosource())));
      }
      return PP.group(first.tosource().append(op).append(PP.nest(INDENT, nested)));
    }
  }
}

export class SCheckTest extends ExprBase {
  // Only 's-op-is' and 's-op-is-not' can have a refinement. (Checked in wf)
  // Only 's-op-raises-not' can lack a RHS. (Guaranteed by parsing; maintain this invariant!)
  get $name(): 's-check-test' { return 's-check-test'; }
  constructor(public l: Loc, public op: CheckOp, public refinement: Expr | undefined,
      public left: Expr, public right: Expr | undefined, public cause: Expr | undefined) { super(); }
  visit(visitor: any): any { return visitor.sCheckTest(this); }
  label(): string { return 's-check-test'; }
  tosource(): any {
    const optionTosource = (opt: Expr | undefined): any =>
      opt === undefined ? PP.mtDoc : opt.tosource();
    const left = this.left.tosource();
    const op =
      this.refinement === undefined ? this.op.tosource()
      : PP.infix(INDENT, 0, strPercent, this.op.tosource(), PP.parens(this.refinement.tosource()));
    const main = PP.infix(INDENT, 1, op, left, optionTosource(this.right));
    if (this.cause === undefined) {
      return main;
    } else {
      return PP.infix(INDENT, 1, strBecause, main, this.cause.tosource());
    }
  }
}

export class SCheckExpr extends ExprBase {
  get $name(): 's-check-expr' { return 's-check-expr'; }
  constructor(public l: Loc, public expr: Expr, public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sCheckExpr(this); }
  label(): string { return 's-check-expr'; }
  tosource(): any {
    if (isABlank(this.ann)) {
      return this.expr.tosource();
    } else {
      // (sic: ast.arr wraps the doc in PP.str here)
      return PP.infix(INDENT, 1, strColoncolon, PP.str(this.expr.tosource() as any), this.ann.tosource());
    }
  }
}

export class SParen extends ExprBase {
  get $name(): 's-paren' { return 's-paren'; }
  constructor(public l: Loc, public expr: Expr) { super(); }
  visit(visitor: any): any { return visitor.sParen(this); }
  label(): string { return 's-paren'; }
  tosource(): any { return PP.parens(this.expr.tosource()); }
}

export class SLam extends ExprBase {
  get $name(): 's-lam' { return 's-lam'; }
  constructor(public l: Loc, public name: string, public params: Name[], public args: Bind[],
      public ann: Ann, public doc: string, public body: Expr,
      public _checkLoc: Loc | undefined, public _check: Expr | undefined,
      public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sLam(this); }
  label(): string { return 's-lam'; }
  tosource(): any {
    return funlamTosource(strLam,
      PP.mtDoc, this.params, this.args, this.ann, this.doc, this.body, this._check, this.blocky);
  }
}

export class SMethod extends ExprBase {
  get $name(): 's-method' { return 's-method'; }
  constructor(public l: Loc, public name: string, public params: Name[], public args: Bind[],
      public ann: Ann, public doc: string, public body: Expr,
      public _checkLoc: Loc | undefined, public _check: Expr | undefined,
      public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sMethod(this); }
  label(): string { return 's-method'; }
  tosource(): any {
    return funlamTosource(strMethod,
      PP.mtDoc, this.params, this.args, this.ann, this.doc, this.body, this._check, this.blocky);
  }
}

export class SExtend extends ExprBase {
  get $name(): 's-extend' { return 's-extend'; }
  constructor(public l: Loc, public supe: Expr, public fields: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sExtend(this); }
  label(): string { return 's-extend'; }
  tosource(): any {
    return PP.group(this.supe.tosource().append(strPeriod)
      .append(PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace),
        PP.lbrace, PP.commabreak, PP.rbrace, this.fields.map((f) => f.tosource()))));
  }
  fieldLoc(): Srcloc {
    const l = this.l as Srcloc;
    const supeL = this.supe.l as Srcloc;
    return new Srcloc(
      l.source,
      supeL.endLine,
      supeL.endColumn + 1,
      supeL.endChar + 1,
      l.endLine,
      l.endColumn,
      l.endChar);
  }
}

export class SUpdate extends ExprBase {
  get $name(): 's-update' { return 's-update'; }
  constructor(public l: Loc, public supe: Expr, public fields: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sUpdate(this); }
  label(): string { return 's-update'; }
  tosource(): any {
    return PP.group(this.supe.tosource().append(strBang)
      .append(PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace),
        PP.lbrace, PP.commabreak, PP.rbrace, this.fields.map((f) => f.tosource()))));
  }
}

export class STuple extends ExprBase {
  get $name(): 's-tuple' { return 's-tuple'; }
  constructor(public l: Loc, public fields: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sTuple(this); }
  label(): string { return 's-tuple'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, PP.str("Empty tuple shoudn't happen"),
      PP.lbrace, PP.semibreak, PP.rbrace, this.fields.map((f) => f.tosource()));
  }
}

export class STupleGet extends ExprBase {
  get $name(): 's-tuple-get' { return 's-tuple-get'; }
  constructor(public l: Loc, public tup: Expr, public index: number, public indexLoc: Loc) { super(); }
  visit(visitor: any): any { return visitor.sTupleGet(this); }
  label(): string { return 's-tuple-get'; }
  tosource(): any {
    return this.tup.tosource().append(PP.str('.')).append(PP.lbrace).append(PP.number(this.index)).append(PP.rbrace);
  }
}

export class SObj extends ExprBase {
  get $name(): 's-obj' { return 's-obj'; }
  constructor(public l: Loc, public fields: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sObj(this); }
  label(): string { return 's-obj'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace),
      PP.lbrace, PP.commabreak, PP.rbrace, this.fields.map((f) => f.tosource()));
  }
}

export class SArray extends ExprBase {
  get $name(): 's-array' { return 's-array'; }
  constructor(public l: Loc, public values: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sArray(this); }
  label(): string { return 's-array'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 0, PP.str('[raw-array: ]'), PP.str('[raw-array: '), PP.commabreak, PP.rbrack,
      this.values.map((v) => v.tosource()));
  }
}

export class SConstruct extends ExprBase {
  get $name(): 's-construct' { return 's-construct'; }
  // NOTE: the Pyret field is named `constructor`, which is reserved on TS
  // classes; it is ported as `constructorVal`.
  constructor(public l: Loc, public modifier: ConstructModifier, public constructorVal: Expr,
      public values: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sConstruct(this); }
  label(): string { return 's-construct'; }
  tosource(): any {
    const prefix = PP.lbrack
      .append(PP.group(PP.separate(PP.sbreak(1), [this.modifier.tosource(), this.constructorVal.tosource()])))
      .append(strColonspace);
    if (this.values.length === 0) {
      return prefix.append(PP.rbrack);
    } else {
      return PP.surround(INDENT, 0, prefix, PP.separate(PP.commabreak, this.values.map((v) => v.tosource())), PP.rbrack);
    }
  }
}

export class SApp extends ExprBase {
  get $name(): 's-app' { return 's-app'; }
  constructor(public l: Loc, public _fun: Expr, public args: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sApp(this); }
  label(): string { return 's-app'; }
  argsLoc(): Srcloc {
    const l = this.l as Srcloc;
    if (this.args.length === 0) {
      const funL = this._fun.l as Srcloc;
      return new Srcloc(
        l.source,
        funL.endLine,
        funL.endColumn,
        funL.endChar,
        l.endLine,
        l.endColumn,
        l.endChar);
    } else {
      const firstElem = this.args[0].l as Srcloc;
      const lastElem = this.args[this.args.length - 1].l as Srcloc;
      return new Srcloc(
        l.source,
        firstElem.startLine,
        firstElem.startColumn,
        firstElem.startChar,
        lastElem.endLine,
        lastElem.endColumn,
        lastElem.endChar);
    }
  }
  tosource(): any {
    return PP.group(this._fun.tosource()
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.map((a) => a.tosource()))))));
  }
}

export class SAppEnriched extends ExprBase {
  // this is used only in the step before transforming the program to ANF
  get $name(): 's-app-enriched' { return 's-app-enriched'; }
  constructor(public l: Loc, public _fun: Expr, public args: Expr[], public appInfo: AppInfo) { super(); }
  visit(visitor: any): any { return visitor.sAppEnriched(this); }
  label(): string { return 's-app'; }
  tosource(): any {
    return PP.group(this._fun.tosource()
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.map((a) => a.tosource()))))));
  }
}

export class SPrimApp extends ExprBase {
  get $name(): 's-prim-app' { return 's-prim-app'; }
  constructor(public l: Loc, public _fun: string, public args: Expr[], public appInfo: PrimAppInfo) { super(); }
  visit(visitor: any): any { return visitor.sPrimApp(this); }
  label(): string { return 's-prim-app'; }
  tosource(): any {
    return PP.group(PP.str(this._fun)
      .append(PP.parens(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.map((a) => a.tosource()))))));
  }
}

export class SPrimVal extends ExprBase {
  get $name(): 's-prim-val' { return 's-prim-val'; }
  constructor(public l: Loc, public name: string) { super(); }
  visit(visitor: any): any { return visitor.sPrimVal(this); }
  label(): string { return 's-prim-val'; }
  tosource(): any { return PP.str(this.name); }
}

export class SId extends ExprBase {
  get $name(): 's-id' { return 's-id'; }
  constructor(public l: Loc, public id: Name) { super(); }
  visit(visitor: any): any { return visitor.sId(this); }
  label(): string { return 's-id'; }
  tosource(): any { return this.id.tosource(); }
}

export class SIdVar extends ExprBase {
  get $name(): 's-id-var' { return 's-id-var'; }
  constructor(public l: Loc, public id: Name) { super(); }
  visit(visitor: any): any { return visitor.sIdVar(this); }
  label(): string { return 's-id-var'; }
  tosource(): any { return PP.str('!').append(this.id.tosource()); }
}

export class SIdLetrec extends ExprBase {
  get $name(): 's-id-letrec' { return 's-id-letrec'; }
  constructor(public l: Loc, public id: Name, public safe: boolean) { super(); }
  visit(visitor: any): any { return visitor.sIdLetrec(this); }
  label(): string { return 's-id-letrec'; }
  tosource(): any { return PP.str('~').append(this.id.tosource()); }
}

export class SIdVarModref extends ExprBase {
  get $name(): 's-id-var-modref' { return 's-id-var-modref'; }
  constructor(public l: Loc, public id: Name, public uri: string, public name: string) { super(); }
  visit(visitor: any): any { return visitor.sIdVarModref(this); }
  label(): string { return 's-id-var-modref'; }
  tosource(): any {
    return this.id.tosource().append(PP.str('@!')).append(PP.parens(PP.str(this.uri))).append(PP.str('.' + this.name));
  }
}

// A fully-resolved reference to a module
export class SIdModref extends ExprBase {
  get $name(): 's-id-modref' { return 's-id-modref'; }
  constructor(public l: Loc, public id: Name, public uri: string, public name: string) { super(); }
  visit(visitor: any): any { return visitor.sIdModref(this); }
  label(): string { return 's-id-modref'; }
  tosource(): any {
    return this.id.tosource().append(PP.str('@')).append(PP.parens(PP.str(this.uri))).append(PP.str('.' + this.name));
  }
}

export class SUndefined extends ExprBase {
  get $name(): 's-undefined' { return 's-undefined'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sUndefined(this); }
  label(): string { return 's-undefined'; }
  tosource(): any { return PP.str('undefined'); }
}

export class SSrcloc extends ExprBase {
  get $name(): 's-srcloc' { return 's-srcloc'; }
  constructor(public l: Loc, public loc: Loc) { super(); }
  visit(visitor: any): any { return visitor.sSrcloc(this); }
  label(): string { return 's-srcloc'; }
  tosource(): any { return PP.str(toreprLoc(this.loc)); }
}

export class SNum extends ExprBase {
  get $name(): 's-num' { return 's-num'; }
  constructor(public l: Loc, public n: PyretNumber) { super(); }
  visit(visitor: any): any { return visitor.sNum(this); }
  label(): string { return 's-num'; }
  tosource(): any { return PP.number(this.n); }
}

export class SFrac extends ExprBase {
  get $name(): 's-frac' { return 's-frac'; }
  // num and den are NumInteger literal values; kept as PyretNumber (see
  // the module header comment).
  constructor(public l: Loc, public num: PyretNumber, public den: PyretNumber) { super(); }
  visit(visitor: any): any { return visitor.sFrac(this); }
  label(): string { return 's-frac'; }
  tosource(): any { return PP.number(this.num).append(PP.str('/')).append(PP.number(this.den)); }
}

export class SRfrac extends ExprBase {
  get $name(): 's-rfrac' { return 's-rfrac'; }
  // num and den are NumInteger literal values; kept as PyretNumber (see
  // the module header comment).
  constructor(public l: Loc, public num: PyretNumber, public den: PyretNumber) { super(); }
  visit(visitor: any): any { return visitor.sRfrac(this); }
  label(): string { return 's-rfrac'; }
  tosource(): any { return PP.str('~').append(PP.number(this.num)).append(PP.str('/')).append(PP.number(this.den)); }
}

export class SBool extends ExprBase {
  get $name(): 's-bool' { return 's-bool'; }
  constructor(public l: Loc, public b: boolean) { super(); }
  visit(visitor: any): any { return visitor.sBool(this); }
  label(): string { return 's-bool'; }
  tosource(): any { return PP.str(String(this.b)); }
}

export class SStr extends ExprBase {
  get $name(): 's-str' { return 's-str'; }
  constructor(public l: Loc, public s: string) { super(); }
  visit(visitor: any): any { return visitor.sStr(this); }
  label(): string { return 's-str'; }
  tosource(): any { return PP.str(toreprStr(this.s)); }
}

export class SDot extends ExprBase {
  get $name(): 's-dot' { return 's-dot'; }
  constructor(public l: Loc, public obj: Expr, public field: string) { super(); }
  visit(visitor: any): any { return visitor.sDot(this); }
  label(): string { return 's-dot'; }
  tosource(): any {
    return PP.infixBreak(INDENT, 0, strPeriod, this.obj.tosource(), PP.str(this.field));
  }
  fieldLoc(): Srcloc {
    const l = this.l as Srcloc;
    return new Srcloc(
      (this.obj.l as Srcloc).source,
      l.endLine,
      l.endColumn - this.field.length,
      l.endChar - this.field.length,
      l.endLine,
      l.endColumn,
      l.endChar);
  }
}

export class SGetBang extends ExprBase {
  get $name(): 's-get-bang' { return 's-get-bang'; }
  constructor(public l: Loc, public obj: Expr, public field: string) { super(); }
  visit(visitor: any): any { return visitor.sGetBang(this); }
  label(): string { return 's-get-bang'; }
  tosource(): any {
    return PP.infixBreak(INDENT, 0, strBang, this.obj.tosource(), PP.str(this.field));
  }
}

export class SBracket extends ExprBase {
  get $name(): 's-bracket' { return 's-bracket'; }
  constructor(public l: Loc, public obj: Expr, public key: Expr) { super(); }
  visit(visitor: any): any { return visitor.sBracket(this); }
  label(): string { return 's-bracket'; }
  tosource(): any {
    return PP.infixBreak(INDENT, 0, PP.mtDoc, this.obj.tosource(),
      PP.surround(INDENT, 0, PP.lbrack, this.key.tosource(), PP.rbrack));
  }
}

export class SData extends ExprBase {
  get $name(): 's-data' { return 's-data'; }
  constructor(public l: Loc, public name: string, public params: Name[], public mixins: Expr[],
      public variants: Variant[], public sharedMembers: Member[],
      public _checkLoc: Loc | undefined, public _check: Expr | undefined) { super(); }
  visit(visitor: any): any { return visitor.sData(this); }
  label(): string { return 's-data'; }
  tosource(): any {
    const optionalSection = (lbl: any, section: any): any =>
      PP.isMtDoc(section) ? PP.mtDoc : breakOne.append(PP.group(PP.nest(INDENT, lbl.append(breakOne).append(section))));
    const tys = PP.surroundSeparate(2 * INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
      this.params.map((p) => p.tosource()));
    const header = strData.append(PP.str(this.name)).append(tys).append(strColon);
    const _deriving =
      PP.surroundSeparate(INDENT, 0, PP.mtDoc, breakOne.append(strDeriving), PP.commabreak, PP.mtDoc,
        this.mixins.map((m) => m.tosource()));
    const variants = PP.separate(breakOne.append(strPipespace),
      [strBlank, ...this.variants.map((v) => PP.nest(INDENT, v.tosource()))]);
    const shared = optionalSection(strSharing,
      PP.separate(PP.commabreak, this.sharedMembers.map((s) => s.tosource())));
    const _check =
      this._check === undefined ? PP.mtDoc : optionalSection(strWhere, this._check.tosource());
    const footer = breakOne.append(strEnd);
    return header.append(_deriving).append(PP.group(PP.nest(INDENT, variants).append(shared).append(_check).append(footer)));
  }
}

export class SDataExpr extends ExprBase {
  get $name(): 's-data-expr' { return 's-data-expr'; }
  constructor(public l: Loc, public name: string, public namet: Name, public params: Name[],
      public mixins: Expr[], public variants: Variant[], public sharedMembers: Member[],
      public _checkLoc: Loc | undefined, public _check: Expr | undefined) { super(); }
  visit(visitor: any): any { return visitor.sDataExpr(this); }
  label(): string { return 's-data-expr'; }
  tosource(): any {
    const optionalSection = (lbl: any, section: any): any =>
      PP.isMtDoc(section) ? PP.mtDoc : breakOne.append(PP.group(PP.nest(INDENT, lbl.append(breakOne).append(section))));
    const tys = PP.surroundSeparate(2 * INDENT, 0, PP.mtDoc, PP.langle, PP.commabreak, PP.rangle,
      this.params.map((p) => p.tosource()));
    const header = strDataExpr.append(PP.str(this.name)).append(PP.comma).append(this.namet.tosource()).append(tys).append(strColon);
    const _deriving =
      PP.surroundSeparate(INDENT, 0, PP.mtDoc, breakOne.append(strDeriving), PP.commabreak, PP.mtDoc,
        this.mixins.map((m) => m.tosource()));
    const variants = PP.separate(breakOne.append(strPipespace),
      [strBlank, ...this.variants.map((v) => PP.nest(INDENT, v.tosource()))]);
    const shared = optionalSection(strSharing,
      PP.separate(PP.commabreak, this.sharedMembers.map((s) => s.tosource())));
    const _check =
      this._check === undefined ? PP.mtDoc : optionalSection(strWhere, this._check.tosource());
    const footer = breakOne.append(strEnd);
    return header.append(_deriving).append(PP.group(PP.nest(INDENT, variants).append(shared).append(_check).append(footer)));
  }
}

export class SFor extends ExprBase {
  get $name(): 's-for' { return 's-for'; }
  constructor(public l: Loc, public iterator: Expr, public bindings: ForBind[], public ann: Ann,
      public body: Expr, public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sFor(this); }
  label(): string { return 's-for'; }
  tosource(): any {
    const annPart =
      isABlank(this.ann) ? PP.mtDoc
      : breakOne.append(strArrow).append(breakOne).append(this.ann.tosource());
    const header = PP.group(strFor
      .append(this.iterator.tosource())
      .append(PP.surroundSeparate(2 * INDENT, 0, PP.lparen.append(PP.rparen), PP.lparen, PP.commabreak, PP.rparen,
        this.bindings.map((b) => b.tosource())))
      .append(PP.group(PP.nest(2 * INDENT,
        annPart.append(blockyColon(this.blocky))))));
    return PP.surround(INDENT, 1, header, this.body.tosource(), strEnd);
  }
}

export class SCheck extends ExprBase {
  get $name(): 's-check' { return 's-check'; }
  constructor(public l: Loc, public name: string | undefined, public body: Expr,
      public keywordCheck: boolean) { super(); }
  visit(visitor: any): any { return visitor.sCheck(this); }
  label(): string { return 's-check'; }
  tosource(): any {
    if (this.name === undefined) {
      return PP.surround(INDENT, 1,
        this.keywordCheck ? strCheckcolon : strExamplescolon,
        this.body.tosource(), strEnd);
    } else {
      return PP.surround(INDENT, 1,
        (this.keywordCheck ? PP.str('check ') : PP.str('examples '))
          .append(PP.str(toreprStr(this.name))).append(strColon),
        this.body.tosource(), strEnd);
    }
  }
}

export class SReactor extends ExprBase {
  get $name(): 's-reactor' { return 's-reactor'; }
  constructor(public l: Loc, public fields: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sReactor(this); }
  label(): string { return 's-reactor'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, PP.str('reactor: end'),
      PP.str('reactor:'), PP.commabreak, PP.str('end'), this.fields.map((f) => f.tosource()));
  }
}

export class STableExtend extends ExprBase {
  get $name(): 's-table-extend' { return 's-table-extend'; }
  constructor(public l: Loc, public columnBinds: ColumnBinds,
      public extensions: TableExtendField[]) { super(); }
  visit(visitor: any): any { return visitor.sTableExtend(this); }
  label(): string { return 's-table-extend'; }
  tosource(): any {
    const maybeUsing: any[] =
      this.columnBinds.binds.length === 0 ? []
      : [strUsing, PP.flowMap(PP.commabreak, (b: Bind) => b.tosource(),
          this.columnBinds.binds).append(strColon)];
    const tblSrc =
      maybeUsing.length === 0 ? this.columnBinds.table.tosource().append(strColon)
      : this.columnBinds.table.tosource();
    const header = PP.flow([strExtend, tblSrc, ...maybeUsing]);
    return PP.surround(INDENT, 1,
      header,
      PP.flowMap(PP.hardline, (e: TableExtendField) => e.tosource(), this.extensions),
      strEnd);
  }
}

// s-table-update not yet implemented (no label/tosource in ast.arr)
export class STableUpdate extends ExprBase {
  get $name(): 's-table-update' { return 's-table-update'; }
  constructor(public l: Loc, public columnBinds: ColumnBinds, public updates: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sTableUpdate(this); }
}

export class STableSelect extends ExprBase {
  get $name(): 's-table-select' { return 's-table-select'; }
  constructor(public l: Loc, public columns: Name[], public table: Expr) { super(); }
  visit(visitor: any): any { return visitor.sTableSelect(this); }
  label(): string { return 's-table-select'; }
  tosource(): any {
    return PP.flow([strSelect,
      PP.flowMap(PP.commabreak, (c: Name) => c.tosource(), this.columns),
      strFrom,
      this.table.tosource(),
      strEnd]);
  }
}

export class STableOrder extends ExprBase {
  get $name(): 's-table-order' { return 's-table-order'; }
  constructor(public l: Loc, public table: Expr, public ordering: ColumnSort[]) { super(); }
  visit(visitor: any): any { return visitor.sTableOrder(this); }
  label(): string { return 's-table-order'; }
  tosource(): any {
    return PP.surround(INDENT, 1,
      PP.flow([strOrder, this.table.tosource().append(strColon)]),
      PP.flowMap(PP.commabreak, (o: ColumnSort) => o.tosource(), this.ordering),
      strEnd);
  }
}

export class STableFilter extends ExprBase {
  get $name(): 's-table-filter' { return 's-table-filter'; }
  constructor(public l: Loc, public columnBinds: ColumnBinds, public predicate: Expr) { super(); }
  visit(visitor: any): any { return visitor.sTableFilter(this); }
  label(): string { return 's-table-filter'; }
  tosource(): any {
    const maybeUsing: any[] =
      this.columnBinds.binds.length === 0 ? []
      : [strUsing, PP.flowMap(PP.commabreak, (b: Bind) => b.tosource(),
          this.columnBinds.binds).append(strColon)];
    const tblSrc =
      maybeUsing.length === 0 ? this.columnBinds.table.tosource().append(strColon)
      : this.columnBinds.table.tosource();
    const header = PP.flow([strSieve, tblSrc, ...maybeUsing]);
    return PP.surround(INDENT, 1, header,
      this.predicate.tosource(),
      strEnd);
  }
}

export class STableExtract extends ExprBase {
  get $name(): 's-table-extract' { return 's-table-extract'; }
  constructor(public l: Loc, public column: Name, public table: Expr) { super(); }
  visit(visitor: any): any { return visitor.sTableExtract(this); }
  label(): string { return 's-table-extract'; }
  tosource(): any {
    return PP.flow([strExtract, this.column.tosource(),
      strFrom, this.table.tosource(), strEnd]);
  }
}

export class STable extends ExprBase {
  get $name(): 's-table' { return 's-table'; }
  constructor(public l: Loc, public headers: FieldName[], public rows: TableRow[]) { super(); }
  visit(visitor: any): any { return visitor.sTable(this); }
  label(): string { return 's-table'; }
  tosource(): any {
    return PP.surround(INDENT, 1,
      PP.flow([strTablecolon,
        PP.flowMap(PP.commabreak, (h: FieldName) => h.tosource(), this.headers)]),
      PP.flowMap(PP.hardline, (r: TableRow) => r.tosource(), this.rows),
      strEnd);
  }
}

export class SLoadTable extends ExprBase {
  get $name(): 's-load-table' { return 's-load-table'; }
  constructor(public l: Loc, public headers: FieldName[], public spec: LoadTableSpec[]) { super(); }
  visit(visitor: any): any { return visitor.sLoadTable(this); }
  label(): string { return 's-load-table'; }
  tosource(): any {
    return PP.surround(INDENT, 1,
      PP.flow([strLoadTable,
        PP.flowMap(PP.commabreak, (h: FieldName) => h.tosource(), this.headers)]),
      PP.flowMap(PP.hardline, (s: LoadTableSpec) => s.tosource(), this.spec),
      strEnd);
  }
}

export class SSpyBlock extends ExprBase {
  get $name(): 's-spy-block' { return 's-spy-block'; }
  constructor(public l: Loc, public message: Expr | undefined, public contents: SpyField[]) { super(); }
  visit(visitor: any): any { return visitor.sSpyBlock(this); }
  label(): string { return 's-spy-block'; }
  tosource(): any {
    if (this.message === undefined) {
      return PP.surroundSeparate(INDENT, 1, PP.str('spy: end'),
        PP.str('spy:'), PP.commabreak, strEnd, this.contents.map((c) => c.tosource()));
    } else {
      const msgSource = this.message.tosource();
      return PP.surroundSeparate(INDENT, 1, PP.str('spy ').append(msgSource).append(PP.str(': end')),
        PP.str('spy ').append(msgSource).append(strColon), PP.commabreak, strEnd, this.contents.map((c) => c.tosource()));
    }
  }
}

export type Expr =
  | SModule | STemplate | STypeLetExpr | SLetExpr | SLetrec | SHintExp | SInstantiate
  | SBlock | SUserBlock | SScopeBlock | SFun | SType | SNewtype | SVar | SRec | SLet | SRef
  | SContract | SWhen | SAssign | SIfPipe | SIfPipeElse | SIf | SIfElse | SCases
  | SCasesElse | SOp | SCheckTest | SCheckExpr | SParen | SLam | SMethod | SExtend
  | SUpdate | STuple | STupleGet | SObj | SArray | SConstruct | SApp | SAppEnriched
  | SPrimApp | SPrimVal | SId | SIdVar | SIdLetrec | SIdVarModref | SIdModref
  | SUndefined | SSrcloc | SNum | SFrac | SRfrac | SBool | SStr | SDot | SGetBang
  | SBracket | SData | SDataExpr | SFor | SCheck | SReactor | STableExtend
  | STableUpdate | STableSelect | STableOrder | STableFilter | STableExtract
  | STable | SLoadTable | SSpyBlock;

export function isSModule(x: any): x is SModule { return x instanceof SModule; }
export function isSTemplate(x: any): x is STemplate { return x instanceof STemplate; }
export function isSTypeLetExpr(x: any): x is STypeLetExpr { return x instanceof STypeLetExpr; }
export function isSLetExpr(x: any): x is SLetExpr { return x instanceof SLetExpr; }
export function isSLetrec(x: any): x is SLetrec { return x instanceof SLetrec; }
export function isSHintExp(x: any): x is SHintExp { return x instanceof SHintExp; }
export function isSInstantiate(x: any): x is SInstantiate { return x instanceof SInstantiate; }
export function isSBlock(x: any): x is SBlock { return x instanceof SBlock; }
export function isSUserBlock(x: any): x is SUserBlock { return x instanceof SUserBlock; }
export function isSScopeBlock(x: any): x is SScopeBlock { return x instanceof SScopeBlock; }
export function isSScopeLet(x: any): x is SScopeLet { return x instanceof SScopeLet; }
export function isSScopeTypeLet(x: any): x is SScopeTypeLet { return x instanceof SScopeTypeLet; }
export function isSScopeLetrec(x: any): x is SScopeLetrec { return x instanceof SScopeLetrec; }
export function isSScopeStmt(x: any): x is SScopeStmt { return x instanceof SScopeStmt; }
export function isSFun(x: any): x is SFun { return x instanceof SFun; }
export function isSType(x: any): x is SType { return x instanceof SType; }
export function isSNewtype(x: any): x is SNewtype { return x instanceof SNewtype; }
export function isSVar(x: any): x is SVar { return x instanceof SVar; }
export function isSRec(x: any): x is SRec { return x instanceof SRec; }
export function isSLet(x: any): x is SLet { return x instanceof SLet; }
export function isSRef(x: any): x is SRef { return x instanceof SRef; }
export function isSContract(x: any): x is SContract { return x instanceof SContract; }
export function isSWhen(x: any): x is SWhen { return x instanceof SWhen; }
export function isSAssign(x: any): x is SAssign { return x instanceof SAssign; }
export function isSIfPipe(x: any): x is SIfPipe { return x instanceof SIfPipe; }
export function isSIfPipeElse(x: any): x is SIfPipeElse { return x instanceof SIfPipeElse; }
export function isSIf(x: any): x is SIf { return x instanceof SIf; }
export function isSIfElse(x: any): x is SIfElse { return x instanceof SIfElse; }
export function isSCases(x: any): x is SCases { return x instanceof SCases; }
export function isSCasesElse(x: any): x is SCasesElse { return x instanceof SCasesElse; }
export function isSOp(x: any): x is SOp { return x instanceof SOp; }
export function isSCheckTest(x: any): x is SCheckTest { return x instanceof SCheckTest; }
export function isSCheckExpr(x: any): x is SCheckExpr { return x instanceof SCheckExpr; }
export function isSParen(x: any): x is SParen { return x instanceof SParen; }
export function isSLam(x: any): x is SLam { return x instanceof SLam; }
export function isSMethod(x: any): x is SMethod { return x instanceof SMethod; }
export function isSExtend(x: any): x is SExtend { return x instanceof SExtend; }
export function isSUpdate(x: any): x is SUpdate { return x instanceof SUpdate; }
export function isSTuple(x: any): x is STuple { return x instanceof STuple; }
export function isSTupleGet(x: any): x is STupleGet { return x instanceof STupleGet; }
export function isSObj(x: any): x is SObj { return x instanceof SObj; }
export function isSArray(x: any): x is SArray { return x instanceof SArray; }
export function isSConstruct(x: any): x is SConstruct { return x instanceof SConstruct; }
export function isSApp(x: any): x is SApp { return x instanceof SApp; }
export function isSAppEnriched(x: any): x is SAppEnriched { return x instanceof SAppEnriched; }
export function isSPrimApp(x: any): x is SPrimApp { return x instanceof SPrimApp; }
export function isSPrimVal(x: any): x is SPrimVal { return x instanceof SPrimVal; }
export function isSId(x: any): x is SId { return x instanceof SId; }
export function isSIdVar(x: any): x is SIdVar { return x instanceof SIdVar; }
export function isSIdLetrec(x: any): x is SIdLetrec { return x instanceof SIdLetrec; }
export function isSIdVarModref(x: any): x is SIdVarModref { return x instanceof SIdVarModref; }
export function isSIdModref(x: any): x is SIdModref { return x instanceof SIdModref; }
export function isSUndefined(x: any): x is SUndefined { return x instanceof SUndefined; }
export function isSSrcloc(x: any): x is SSrcloc { return x instanceof SSrcloc; }
export function isSNum(x: any): x is SNum { return x instanceof SNum; }
export function isSFrac(x: any): x is SFrac { return x instanceof SFrac; }
export function isSRfrac(x: any): x is SRfrac { return x instanceof SRfrac; }
export function isSBool(x: any): x is SBool { return x instanceof SBool; }
export function isSStr(x: any): x is SStr { return x instanceof SStr; }
export function isSDot(x: any): x is SDot { return x instanceof SDot; }
export function isSGetBang(x: any): x is SGetBang { return x instanceof SGetBang; }
export function isSBracket(x: any): x is SBracket { return x instanceof SBracket; }
export function isSData(x: any): x is SData { return x instanceof SData; }
export function isSDataExpr(x: any): x is SDataExpr { return x instanceof SDataExpr; }
export function isSFor(x: any): x is SFor { return x instanceof SFor; }
export function isSCheck(x: any): x is SCheck { return x instanceof SCheck; }
export function isSReactor(x: any): x is SReactor { return x instanceof SReactor; }
export function isSTableExtend(x: any): x is STableExtend { return x instanceof STableExtend; }
export function isSTableUpdate(x: any): x is STableUpdate { return x instanceof STableUpdate; }
export function isSTableSelect(x: any): x is STableSelect { return x instanceof STableSelect; }
export function isSTableOrder(x: any): x is STableOrder { return x instanceof STableOrder; }
export function isSTableFilter(x: any): x is STableFilter { return x instanceof STableFilter; }
export function isSTableExtract(x: any): x is STableExtract { return x instanceof STableExtract; }
export function isSTable(x: any): x is STable { return x instanceof STable; }
export function isSLoadTable(x: any): x is SLoadTable { return x instanceof SLoadTable; }
export function isSSpyBlock(x: any): x is SSpyBlock { return x instanceof SSpyBlock; }
export function isExpr(x: any): x is Expr { return x instanceof ExprBase; }

// ---------- TableRow ----------

export abstract class TableRowBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class STableRow extends TableRowBase {
  get $name(): 's-table-row' { return 's-table-row'; }
  constructor(public l: Loc, public elems: Expr[]) { super(); }
  visit(visitor: any): any { return visitor.sTableRow(this); }
  label(): string { return 's-table-row'; }
  tosource(): any {
    return PP.flow([strRowcolon,
      PP.flowMap(PP.commabreak, (e: Expr) => e.tosource(), this.elems)]);
  }
}

export type TableRow = STableRow;
export function isSTableRow(x: any): x is STableRow { return x instanceof STableRow; }
export function isTableRow(x: any): x is TableRow { return x instanceof TableRowBase; }

// ---------- SpyField ----------

export abstract class SpyFieldBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

// implicit-label is true for the shorthand form (`spy: x end`), and false for
// the longer form (`spy: some-name: x end`)
export class SSpyExpr extends SpyFieldBase {
  get $name(): 's-spy-expr' { return 's-spy-expr'; }
  constructor(public l: Loc, public name: string, public value: Expr,
      public implicitLabel: boolean) { super(); }
  visit(visitor: any): any { return visitor.sSpyExpr(this); }
  label(): string { return 's-spy-expr'; }
  tosource(): any {
    if (this.implicitLabel) {
      return this.value.tosource();
    } else {
      return PP.nest(INDENT, PP.str(this.name).append(strColonspace).append(this.value.tosource()));
    }
  }
}

export type SpyField = SSpyExpr;
export function isSSpyExpr(x: any): x is SSpyExpr { return x instanceof SSpyExpr; }
export function isSpyField(x: any): x is SpyField { return x instanceof SpyFieldBase; }

// ---------- ConstructModifier ----------

export abstract class ConstructModifierBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SConstructNormal extends ConstructModifierBase {
  get $name(): 's-construct-normal' { return 's-construct-normal'; }
  visit(visitor: any): any { return visitor.sConstructNormal(this); }
  label(): string { return 's-construct-normal'; }
  tosource(): any { return PP.mtDoc; }
}

export class SConstructLazy extends ConstructModifierBase {
  get $name(): 's-construct-lazy' { return 's-construct-lazy'; }
  visit(visitor: any): any { return visitor.sConstructLazy(this); }
  label(): string { return 's-construct-lazy'; }
  tosource(): any { return PP.str('lazy'); }
}

export type ConstructModifier = SConstructNormal | SConstructLazy;
export function isSConstructNormal(x: any): x is SConstructNormal { return x instanceof SConstructNormal; }
export function isSConstructLazy(x: any): x is SConstructLazy { return x instanceof SConstructLazy; }
export function isConstructModifier(x: any): x is ConstructModifier { return x instanceof ConstructModifierBase; }

export const sConstructNormal: SConstructNormal = new SConstructNormal();
export const sConstructLazy: SConstructLazy = new SConstructLazy();

// ---------- Bind ----------

export abstract class BindBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SBind extends BindBase {
  get $name(): 's-bind' { return 's-bind'; }
  constructor(public l: Loc, public shadows: boolean, public id: Name, public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sBind(this); }
  label(): string { return 's-bind'; }
  tosource(): any {
    if (isABlank(this.ann)) {
      if (this.shadows) { return PP.str('shadow ').append(this.id.tosource()); }
      else { return this.id.tosource(); }
    } else {
      if (this.shadows) {
        return PP.infix(INDENT, 1, strColoncolon, PP.str('shadow ').append(this.id.tosource()), this.ann.tosource());
      } else {
        return PP.infix(INDENT, 1, strColoncolon, this.id.tosource(), this.ann.tosource());
      }
    }
  }
}

export class STupleBind extends BindBase {
  get $name(): 's-tuple-bind' { return 's-tuple-bind'; }
  constructor(public l: Loc, public fields: Bind[], public asName: Bind | undefined) { super(); }
  visit(visitor: any): any { return visitor.sTupleBind(this); }
  label(): string { return 's-tuple-bind'; }
  tosource(): any {
    const mainPat = PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace), PP.lbrace, PP.semibreak, PP.rbrace,
      this.fields.map((f) => f.tosource()));
    if (this.asName === undefined) {
      return mainPat;
    } else {
      return PP.infix(INDENT, 1, strAs, mainPat, this.asName.tosource());
    }
  }
}

export type Bind = SBind | STupleBind;
export function isSBind(x: any): x is SBind { return x instanceof SBind; }
export function isSTupleBind(x: any): x is STupleBind { return x instanceof STupleBind; }
export function isBind(x: any): x is Bind { return x instanceof BindBase; }

// ---------- Member ----------

export abstract class MemberBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SDataField extends MemberBase {
  get $name(): 's-data-field' { return 's-data-field'; }
  constructor(public l: Loc, public name: string, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sDataField(this); }
  label(): string { return 's-data-field'; }
  tosource(): any {
    const namePart = PP.str(this.name);
    return PP.nest(INDENT, namePart.append(strColonspace).append(this.value.tosource()));
  }
}

export class SMutableField extends MemberBase {
  get $name(): 's-mutable-field' { return 's-mutable-field'; }
  constructor(public l: Loc, public name: string, public ann: Ann, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sMutableField(this); }
  label(): string { return 's-mutable-field'; }
  tosource(): any {
    const namePart = PP.str(this.name);
    const annPart =
      isABlank(this.ann) ? namePart
      : namePart.append(strColoncolon).append(this.ann.tosource());
    return PP.nest(INDENT, strMutable.append(annPart).append(strColonspace).append(this.value.tosource()));
  }
}

export class SMethodField extends MemberBase {
  get $name(): 's-method-field' { return 's-method-field'; }
  constructor(public l: Loc, public name: string, public params: Name[], public args: Bind[],
      public ann: Ann, public doc: string, public body: Expr,
      public _checkLoc: Loc | undefined, public _check: Expr | undefined,
      public blocky: boolean) { super(); }
  visit(visitor: any): any { return visitor.sMethodField(this); }
  label(): string { return 's-method-field'; }
  tosource(): any {
    return funlamTosource(strMethod,
      PP.str(this.name), this.params, this.args, this.ann, this.doc, this.body, this._check, this.blocky);
  }
}

export type Member = SDataField | SMutableField | SMethodField;
export function isSDataField(x: any): x is SDataField { return x instanceof SDataField; }
export function isSMutableField(x: any): x is SMutableField { return x instanceof SMutableField; }
export function isSMethodField(x: any): x is SMethodField { return x instanceof SMethodField; }
export function isMember(x: any): x is Member { return x instanceof MemberBase; }

// ---------- FieldName ----------

export abstract class FieldNameBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SFieldName extends FieldNameBase {
  get $name(): 's-field-name' { return 's-field-name'; }
  constructor(public l: Loc, public name: string, public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sFieldName(this); }
  label(): string { return 's-field-name'; }
  tosource(): any {
    if (isABlank(this.ann)) {
      return PP.str(this.name);
    } else {
      return PP.infix(INDENT, 1, strColoncolon, PP.str(this.name), this.ann.tosource());
    }
  }
}

export type FieldName = SFieldName;
export function isSFieldName(x: any): x is SFieldName { return x instanceof SFieldName; }
export function isFieldName(x: any): x is FieldName { return x instanceof FieldNameBase; }

// ---------- ForBind ----------

export abstract class ForBindBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SForBind extends ForBindBase {
  get $name(): 's-for-bind' { return 's-for-bind'; }
  constructor(public l: Loc, public bind: Bind, public value: Expr) { super(); }
  visit(visitor: any): any { return visitor.sForBind(this); }
  label(): string { return 's-for-bind'; }
  tosource(): any {
    return PP.group(this.bind.tosource().append(breakOne).append(strFrom).append(breakOne).append(this.value.tosource()));
  }
}

export type ForBind = SForBind;
export function isSForBind(x: any): x is SForBind { return x instanceof SForBind; }
export function isForBind(x: any): x is ForBind { return x instanceof ForBindBase; }

// ---------- ColumnBinds ----------

export abstract class ColumnBindsBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SColumnBinds extends ColumnBindsBase {
  get $name(): 's-column-binds' { return 's-column-binds'; }
  constructor(public l: Loc, public binds: Bind[], public table: Expr) { super(); }
  visit(visitor: any): any { return visitor.sColumnBinds(this); }
}

export type ColumnBinds = SColumnBinds;
export function isSColumnBinds(x: any): x is SColumnBinds { return x instanceof SColumnBinds; }
export function isColumnBinds(x: any): x is ColumnBinds { return x instanceof ColumnBindsBase; }

// ---------- ColumnSortOrder ----------

export abstract class ColumnSortOrderBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class ASCENDING extends ColumnSortOrderBase {
  get $name(): 'ASCENDING' { return 'ASCENDING'; }
  visit(visitor: any): any { return visitor.ASCENDING(this); }
  tosource(): any {
    return PP.str('ascending');
  }
}

export class DESCENDING extends ColumnSortOrderBase {
  get $name(): 'DESCENDING' { return 'DESCENDING'; }
  visit(visitor: any): any { return visitor.DESCENDING(this); }
  tosource(): any {
    return PP.str('descending');
  }
}

export type ColumnSortOrder = ASCENDING | DESCENDING;
export function isASCENDING(x: any): x is ASCENDING { return x instanceof ASCENDING; }
export function isDESCENDING(x: any): x is DESCENDING { return x instanceof DESCENDING; }
export function isColumnSortOrder(x: any): x is ColumnSortOrder { return x instanceof ColumnSortOrderBase; }

// ---------- ColumnSort ----------

export abstract class ColumnSortBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SColumnSort extends ColumnSortBase {
  get $name(): 's-column-sort' { return 's-column-sort'; }
  constructor(public l: Loc, public column: Name, public direction: ColumnSortOrder) { super(); }
  visit(visitor: any): any { return visitor.sColumnSort(this); }
  label(): string { return 's-column-sort'; }
  tosource(): any {
    return PP.flow([this.column.tosource(), this.direction.tosource()]);
  }
}

export type ColumnSort = SColumnSort;
export function isSColumnSort(x: any): x is SColumnSort { return x instanceof SColumnSort; }
export function isColumnSort(x: any): x is ColumnSort { return x instanceof ColumnSortBase; }

// ---------- TableExtendField ----------

export abstract class TableExtendFieldBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class STableExtendField extends TableExtendFieldBase {
  get $name(): 's-table-extend-field' { return 's-table-extend-field'; }
  constructor(public l: Loc, public name: string, public value: Expr, public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sTableExtendField(this); }
  label(): string { return 's-table-extend-field'; }
  tosource(): any {
    const namePart = PP.str(this.name);
    const maybeAnn =
      isABlank(this.ann) ? PP.mtDoc
      : strColoncolon.append(this.ann.tosource());
    return PP.nest(INDENT, namePart.append(maybeAnn).append(strColonspace).append(this.value.tosource()));
  }
}

export class STableExtendReducer extends TableExtendFieldBase {
  get $name(): 's-table-extend-reducer' { return 's-table-extend-reducer'; }
  constructor(public l: Loc, public name: string, public reducer: Expr, public col: Name,
      public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.sTableExtendReducer(this); }
  label(): string { return 's-table-extend-reducer'; }
  tosource(): any {
    const namePart = PP.str(this.name);
    const maybeAnn =
      isABlank(this.ann) ? PP.mtDoc
      : strColoncolon.append(this.ann.tosource());
    const colPart = this.col.tosource();
    return PP.nest(INDENT, namePart.append(maybeAnn).append(strColonspace).append(this.reducer.tosource()).append(PP.str(' ')).append(strOf).append(colPart));
  }
}

export type TableExtendField = STableExtendField | STableExtendReducer;
export function isSTableExtendField(x: any): x is STableExtendField { return x instanceof STableExtendField; }
export function isSTableExtendReducer(x: any): x is STableExtendReducer { return x instanceof STableExtendReducer; }
export function isTableExtendField(x: any): x is TableExtendField { return x instanceof TableExtendFieldBase; }

// ---------- LoadTableSpec ----------

export abstract class LoadTableSpecBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SSanitize extends LoadTableSpecBase {
  get $name(): 's-sanitize' { return 's-sanitize'; }
  constructor(public l: Loc, public name: Name, public sanitizer: Expr) { super(); }
  visit(visitor: any): any { return visitor.sSanitize(this); }
  label(): string { return 's-sanitize'; }
  tosource(): any {
    const namePart = this.name.tosource();
    return PP.flow([strSanitize, namePart, strUsing, this.sanitizer.tosource()]);
  }
}

export class STableSrc extends LoadTableSpecBase {
  get $name(): 's-table-src' { return 's-table-src'; }
  constructor(public l: Loc, public src: Expr) { super(); }
  visit(visitor: any): any { return visitor.sTableSrc(this); }
  label(): string { return 's-table-src'; }
  tosource(): any {
    return PP.flow([strSrc, this.src.tosource()]);
  }
}

export type LoadTableSpec = SSanitize | STableSrc;
export function isSSanitize(x: any): x is SSanitize { return x instanceof SSanitize; }
export function isSTableSrc(x: any): x is STableSrc { return x instanceof STableSrc; }
export function isLoadTableSpec(x: any): x is LoadTableSpec { return x instanceof LoadTableSpecBase; }

// ---------- VariantMemberType ----------

export abstract class VariantMemberTypeBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SNormal extends VariantMemberTypeBase {
  get $name(): 's-normal' { return 's-normal'; }
  visit(visitor: any): any { return visitor.sNormal(this); }
  label(): string { return 's-normal'; }
  tosource(): any { return PP.mtDoc; }
}

export class SMutable extends VariantMemberTypeBase {
  get $name(): 's-mutable' { return 's-mutable'; }
  visit(visitor: any): any { return visitor.sMutable(this); }
  label(): string { return 's-mutable'; }
  tosource(): any { return PP.str('ref '); }
}

export type VariantMemberType = SNormal | SMutable;
export function isSNormal(x: any): x is SNormal { return x instanceof SNormal; }
export function isSMutable(x: any): x is SMutable { return x instanceof SMutable; }
export function isVariantMemberType(x: any): x is VariantMemberType { return x instanceof VariantMemberTypeBase; }

export const sNormal: SNormal = new SNormal();
export const sMutable: SMutable = new SMutable();

// ---------- VariantMember ----------

export abstract class VariantMemberBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SVariantMember extends VariantMemberBase {
  get $name(): 's-variant-member' { return 's-variant-member'; }
  constructor(public l: Loc, public memberType: VariantMemberType, public bind: Bind) { super(); }
  visit(visitor: any): any { return visitor.sVariantMember(this); }
  label(): string { return 's-variant-member'; }
  tosource(): any {
    return this.memberType.tosource().append(this.bind.tosource());
  }
}

export type VariantMember = SVariantMember;
export function isSVariantMember(x: any): x is SVariantMember { return x instanceof SVariantMember; }
export function isVariantMember(x: any): x is VariantMember { return x instanceof VariantMemberBase; }

// ---------- Variant ----------

export abstract class VariantBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SVariant extends VariantBase {
  get $name(): 's-variant' { return 's-variant'; }
  constructor(public l: Loc, public constrLoc: Loc, public name: string,
      public members: VariantMember[], public withMembers: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sVariant(this); }
  label(): string { return 's-variant'; }
  tosource(): any {
    const headerNowith =
      PP.str(this.name)
        .append(PP.surroundSeparate(INDENT, 0, PP.str('()'), PP.lparen, PP.commabreak, PP.rparen,
          this.members.map((b) => b.tosource())));
    const header = PP.group(headerNowith.append(breakOne).append(strWith));
    const withs = this.withMembers.map((m) => m.tosource());
    if (withs.length === 0) {
      return headerNowith;
    } else {
      return header.append(PP.group(PP.nest(INDENT, breakOne.append(PP.separate(PP.commabreak, withs)))));
    }
  }
}

export class SSingletonVariant extends VariantBase {
  get $name(): 's-singleton-variant' { return 's-singleton-variant'; }
  constructor(public l: Loc, public name: string, public withMembers: Member[]) { super(); }
  visit(visitor: any): any { return visitor.sSingletonVariant(this); }
  label(): string { return 's-singleton-variant'; }
  tosource(): any {
    const headerNowith = PP.str(this.name);
    const header = PP.group(headerNowith.append(breakOne).append(strWith));
    const withs = this.withMembers.map((m) => m.tosource());
    if (withs.length === 0) {
      return headerNowith;
    } else {
      return header.append(PP.group(PP.nest(INDENT, breakOne.append(PP.separate(PP.commabreak, withs)))));
    }
  }
}

export type Variant = SVariant | SSingletonVariant;
export function isSVariant(x: any): x is SVariant { return x instanceof SVariant; }
export function isSSingletonVariant(x: any): x is SSingletonVariant { return x instanceof SSingletonVariant; }
export function isVariant(x: any): x is Variant { return x instanceof VariantBase; }

// ---------- IfBranch ----------

export abstract class IfBranchBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SIfBranch extends IfBranchBase {
  get $name(): 's-if-branch' { return 's-if-branch'; }
  constructor(public l: Loc, public test: Expr, public body: Expr) { super(); }
  visit(visitor: any): any { return visitor.sIfBranch(this); }
  label(): string { return 's-if-branch'; }
  tosource(): any {
    return strIf
      .append(PP.nest(2 * INDENT, this.test.tosource().append(strColon)))
      .append(PP.nest(INDENT, breakOne.append(this.body.tosource())));
  }
  tosourceBlocky(): any {
    return strIf
      .append(PP.nest(2 * INDENT, this.test.tosource().append(breakOne).append(strBlock)))
      .append(PP.nest(INDENT, breakOne.append(this.body.tosource())));
  }
}

export type IfBranch = SIfBranch;
export function isSIfBranch(x: any): x is SIfBranch { return x instanceof SIfBranch; }
export function isIfBranch(x: any): x is IfBranch { return x instanceof IfBranchBase; }

// ---------- IfPipeBranch ----------

export abstract class IfPipeBranchBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SIfPipeBranch extends IfPipeBranchBase {
  get $name(): 's-if-pipe-branch' { return 's-if-pipe-branch'; }
  constructor(public l: Loc, public test: Expr, public body: Expr) { super(); }
  visit(visitor: any): any { return visitor.sIfPipeBranch(this); }
  label(): string { return 's-if-pipe-branch'; }
  tosource(): any {
    return strPipespace
      .append(PP.nest(2 * INDENT, this.test.tosource().append(breakOne).append(strThencolon)))
      .append(PP.nest(INDENT, breakOne.append(this.body.tosource())));
  }
}

export type IfPipeBranch = SIfPipeBranch;
export function isSIfPipeBranch(x: any): x is SIfPipeBranch { return x instanceof SIfPipeBranch; }
export function isIfPipeBranch(x: any): x is IfPipeBranch { return x instanceof IfPipeBranchBase; }

// ---------- CasesBindType ----------

export abstract class CasesBindTypeBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SCasesBindRef extends CasesBindTypeBase {
  get $name(): 's-cases-bind-ref' { return 's-cases-bind-ref'; }
  visit(visitor: any): any { return visitor.sCasesBindRef(this); }
  label(): string { return 's-cases-bind-ref'; }
  tosource(): any { return PP.str('ref'); }
}

export class SCasesBindNormal extends CasesBindTypeBase {
  get $name(): 's-cases-bind-normal' { return 's-cases-bind-normal'; }
  visit(visitor: any): any { return visitor.sCasesBindNormal(this); }
  label(): string { return 's-cases-bind-normal'; }
  tosource(): any { return PP.mtDoc; }
}

export type CasesBindType = SCasesBindRef | SCasesBindNormal;
export function isSCasesBindRef(x: any): x is SCasesBindRef { return x instanceof SCasesBindRef; }
export function isSCasesBindNormal(x: any): x is SCasesBindNormal { return x instanceof SCasesBindNormal; }
export function isCasesBindType(x: any): x is CasesBindType { return x instanceof CasesBindTypeBase; }

export const sCasesBindRef: SCasesBindRef = new SCasesBindRef();
export const sCasesBindNormal: SCasesBindNormal = new SCasesBindNormal();

// ---------- CasesBind ----------

export abstract class CasesBindBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SCasesBind extends CasesBindBase {
  get $name(): 's-cases-bind' { return 's-cases-bind'; }
  constructor(public l: Loc, public fieldType: CasesBindType, public bind: Bind) { super(); }
  visit(visitor: any): any { return visitor.sCasesBind(this); }
  label(): string { return 's-cases-bind'; }
  tosource(): any {
    const ft = this.fieldType.tosource();
    if (PP.isMtDoc(ft)) {
      return this.bind.tosource();
    } else {
      return ft.append(PP.str(' ')).append(this.bind.tosource());
    }
  }
}

export type CasesBind = SCasesBind;
export function isSCasesBind(x: any): x is SCasesBind { return x instanceof SCasesBind; }
export function isCasesBind(x: any): x is CasesBind { return x instanceof CasesBindBase; }

// ---------- CasesBranch ----------

export abstract class CasesBranchBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SCasesBranch extends CasesBranchBase {
  get $name(): 's-cases-branch' { return 's-cases-branch'; }
  constructor(public l: Loc, public patLoc: Loc, public name: string, public args: CasesBind[],
      public body: Expr) { super(); }
  visit(visitor: any): any { return visitor.sCasesBranch(this); }
  label(): string { return 's-cases-branch'; }
  tosource(): any {
    return PP.nest(INDENT,
      PP.group(PP.str('| ' + this.name)
        .append(PP.surroundSeparate(INDENT, 0, PP.str('()'), PP.lparen, PP.commabreak, PP.rparen,
          this.args.map((a) => a.tosource()))).append(breakOne).append(strThickarrow)).append(breakOne).append(
        PP.nest(INDENT, this.body.tosource())));
  }
}

export class SSingletonCasesBranch extends CasesBranchBase {
  get $name(): 's-singleton-cases-branch' { return 's-singleton-cases-branch'; }
  constructor(public l: Loc, public patLoc: Loc, public name: string, public body: Expr) { super(); }
  visit(visitor: any): any { return visitor.sSingletonCasesBranch(this); }
  label(): string { return 's-singleton-cases-branch'; }
  tosource(): any {
    return PP.nest(INDENT,
      PP.group(PP.str('| ' + this.name).append(breakOne).append(strThickarrow)).append(breakOne)
        .append(PP.nest(INDENT, this.body.tosource())));
  }
}

export type CasesBranch = SCasesBranch | SSingletonCasesBranch;
export function isSCasesBranch(x: any): x is SCasesBranch { return x instanceof SCasesBranch; }
export function isSSingletonCasesBranch(x: any): x is SSingletonCasesBranch { return x instanceof SSingletonCasesBranch; }
export function isCasesBranch(x: any): x is CasesBranch { return x instanceof CasesBranchBase; }

export function annLoc(ann: Ann): Loc {
  if (isABlank(ann)) { return dummyLoc; }
  else { return (ann as any).l; }
}

export function getOpFunName(opname: string): string {
  if (opname === 'op==') { return 'equal-always3'; }
  else if (opname === 'op=~') { return 'equal-now3'; }
  else if (opname === 'op<=>') { return 'identical3'; }
  else { return raise('Unknown op: ' + opname); }
}

// ---------- CheckOp ----------

export abstract class CheckOpBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class SOpIs extends CheckOpBase {
  get $name(): 's-op-is' { return 's-op-is'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpIs(this); }
  label(): string { return 's-op-is'; }
  tosource(): any { return strIs; }
}

export class SOpIsRoughly extends CheckOpBase {
  get $name(): 's-op-is-roughly' { return 's-op-is-roughly'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpIsRoughly(this); }
  label(): string { return 's-op-is-roughly'; }
  tosource(): any { return PP.str('is-roughly'); }
}

export class SOpIsNotRoughly extends CheckOpBase {
  get $name(): 's-op-is-not-roughly' { return 's-op-is-not-roughly'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpIsNotRoughly(this); }
  label(): string { return 's-op-is-not-roughly'; }
  tosource(): any { return PP.str('is-not-roughly'); }
}

export class SOpIsOp extends CheckOpBase {
  get $name(): 's-op-is-op' { return 's-op-is-op'; }
  constructor(public l: Loc, public op: string) { super(); }
  visit(visitor: any): any { return visitor.sOpIsOp(this); }
  label(): string { return 's-op-is-op'; }
  tosource(): any { return strIs.append(PP.str(this.op.substring(2))); }
}

export class SOpIsNot extends CheckOpBase {
  get $name(): 's-op-is-not' { return 's-op-is-not'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpIsNot(this); }
  label(): string { return 's-op-is-not'; }
  tosource(): any { return strIsNot; }
}

export class SOpIsNotOp extends CheckOpBase {
  get $name(): 's-op-is-not-op' { return 's-op-is-not-op'; }
  constructor(public l: Loc, public op: string) { super(); }
  visit(visitor: any): any { return visitor.sOpIsNotOp(this); }
  label(): string { return 's-op-is-not-op'; }
  tosource(): any { return strIsNot.append(PP.str(this.op.substring(2))); }
}

export class SOpSatisfies extends CheckOpBase {
  get $name(): 's-op-satisfies' { return 's-op-satisfies'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpSatisfies(this); }
  label(): string { return 's-op-satisfies'; }
  tosource(): any { return strSatisfies; }
}

export class SOpSatisfiesNot extends CheckOpBase {
  get $name(): 's-op-satisfies-not' { return 's-op-satisfies-not'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpSatisfiesNot(this); }
  label(): string { return 's-op-satisfies-not'; }
  tosource(): any { return strSatisfiesNot; }
}

export class SOpRaises extends CheckOpBase {
  get $name(): 's-op-raises' { return 's-op-raises'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpRaises(this); }
  label(): string { return 's-op-raises'; }
  tosource(): any { return strRaises; }
}

export class SOpRaisesOther extends CheckOpBase {
  get $name(): 's-op-raises-other' { return 's-op-raises-other'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpRaisesOther(this); }
  label(): string { return 's-op-raises-other'; }
  tosource(): any { return strRaisesOther; }
}

export class SOpRaisesNot extends CheckOpBase {
  get $name(): 's-op-raises-not' { return 's-op-raises-not'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpRaisesNot(this); }
  label(): string { return 's-op-raises-not'; }
  tosource(): any { return strRaisesNot; }
}

export class SOpRaisesSatisfies extends CheckOpBase {
  get $name(): 's-op-raises-satisfies' { return 's-op-raises-satisfies'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpRaisesSatisfies(this); }
  label(): string { return 's-op-raises-satisfies'; }
  tosource(): any { return strRaisesSatisfies; }
}

export class SOpRaisesViolates extends CheckOpBase {
  get $name(): 's-op-raises-violates' { return 's-op-raises-violates'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.sOpRaisesViolates(this); }
  label(): string { return 's-op-raises-violates'; }
  tosource(): any { return strRaisesViolates; }
}

export type CheckOp =
  | SOpIs | SOpIsRoughly | SOpIsNotRoughly | SOpIsOp | SOpIsNot | SOpIsNotOp
  | SOpSatisfies | SOpSatisfiesNot | SOpRaises | SOpRaisesOther | SOpRaisesNot
  | SOpRaisesSatisfies | SOpRaisesViolates;

export function isSOpIs(x: any): x is SOpIs { return x instanceof SOpIs; }
export function isSOpIsRoughly(x: any): x is SOpIsRoughly { return x instanceof SOpIsRoughly; }
export function isSOpIsNotRoughly(x: any): x is SOpIsNotRoughly { return x instanceof SOpIsNotRoughly; }
export function isSOpIsOp(x: any): x is SOpIsOp { return x instanceof SOpIsOp; }
export function isSOpIsNot(x: any): x is SOpIsNot { return x instanceof SOpIsNot; }
export function isSOpIsNotOp(x: any): x is SOpIsNotOp { return x instanceof SOpIsNotOp; }
export function isSOpSatisfies(x: any): x is SOpSatisfies { return x instanceof SOpSatisfies; }
export function isSOpSatisfiesNot(x: any): x is SOpSatisfiesNot { return x instanceof SOpSatisfiesNot; }
export function isSOpRaises(x: any): x is SOpRaises { return x instanceof SOpRaises; }
export function isSOpRaisesOther(x: any): x is SOpRaisesOther { return x instanceof SOpRaisesOther; }
export function isSOpRaisesNot(x: any): x is SOpRaisesNot { return x instanceof SOpRaisesNot; }
export function isSOpRaisesSatisfies(x: any): x is SOpRaisesSatisfies { return x instanceof SOpRaisesSatisfies; }
export function isSOpRaisesViolates(x: any): x is SOpRaisesViolates { return x instanceof SOpRaisesViolates; }
export function isCheckOp(x: any): x is CheckOp { return x instanceof CheckOpBase; }

// ---------- Ann ----------

export abstract class AnnBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class ABlank extends AnnBase {
  get $name(): 'a-blank' { return 'a-blank'; }
  visit(visitor: any): any { return visitor.aBlank(this); }
  label(): string { return 'a-blank'; }
  tosource(): any { return strAny; }
}

export class AAny extends AnnBase {
  get $name(): 'a-any' { return 'a-any'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.aAny(this); }
  label(): string { return 'a-any'; }
  tosource(): any { return strAny; }
}

export class AName extends AnnBase {
  get $name(): 'a-name' { return 'a-name'; }
  constructor(public l: Loc, public id: Name) { super(); }
  visit(visitor: any): any { return visitor.aName(this); }
  label(): string { return 'a-name'; }
  tosource(): any { return this.id.tosource(); }
}

export class ATypeVar extends AnnBase {
  get $name(): 'a-type-var' { return 'a-type-var'; }
  constructor(public l: Loc, public id: Name) { super(); }
  visit(visitor: any): any { return visitor.aTypeVar(this); }
  label(): string { return 'a-type-var'; }
  tosource(): any { return this.id.tosource(); }
}

export class AArrow extends AnnBase {
  get $name(): 'a-arrow' { return 'a-arrow'; }
  constructor(public l: Loc, public args: Ann[], public ret: Ann, public useParens: boolean) { super(); }
  visit(visitor: any): any { return visitor.aArrow(this); }
  label(): string { return 'a-arrow'; }
  tosource(): any {
    const ann = PP.separate(strSpace,
      [PP.separate(PP.commabreak, this.args.map((a) => a.tosource())),
        strArrow, this.ret.tosource()]);
    if (this.useParens) {
      return PP.surround(INDENT, 0, PP.lparen, ann, PP.rparen);
    } else {
      return ann;
    }
  }
}

export class AArrowArgnames extends AnnBase {
  get $name(): 'a-arrow-argnames' { return 'a-arrow-argnames'; }
  constructor(public l: Loc, public args: AField[], public ret: Ann, public useParens: boolean) { super(); }
  visit(visitor: any): any { return visitor.aArrowArgnames(this); }
  label(): string { return 'a-arrow-argnames'; }
  tosource(): any {
    const ann = PP.separate(strSpace,
      [PP.surround(INDENT, 0, PP.lparen,
        PP.separate(PP.commabreak, this.args.map((a) => a.tosource())),
        PP.rparen),
        strArrow, this.ret.tosource()]);
    if (this.useParens) {
      return PP.surround(INDENT, 0, PP.lparen, ann, PP.rparen);
    } else {
      return ann;
    }
  }
}

export class AMethod extends AnnBase {
  get $name(): 'a-method' { return 'a-method'; }
  constructor(public l: Loc, public args: Ann[], public ret: Ann) { super(); }
  visit(visitor: any): any { return visitor.aMethod(this); }
  label(): string { return 'a-method'; }
  tosource(): any { return PP.str('NYI: A-method'); }
}

export class ARecord extends AnnBase {
  get $name(): 'a-record' { return 'a-record'; }
  constructor(public l: Loc, public fields: AField[]) { super(); }
  visit(visitor: any): any { return visitor.aRecord(this); }
  label(): string { return 'a-record'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace), PP.lbrace, PP.commabreak, PP.rbrace,
      this.fields.map((f) => f.tosource()));
  }
}

export class ATuple extends AnnBase {
  get $name(): 'a-tuple' { return 'a-tuple'; }
  constructor(public l: Loc, public fields: Ann[]) { super(); }
  visit(visitor: any): any { return visitor.aTuple(this); }
  label(): string { return 'a-tuple'; }
  tosource(): any {
    return PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace), PP.lbrace, PP.semibreak, PP.rbrace,
      this.fields.map((f) => f.tosource()));
  }
}

export class AApp extends AnnBase {
  get $name(): 'a-app' { return 'a-app'; }
  constructor(public l: Loc, public ann: Ann, public args: Ann[]) { super(); }
  visit(visitor: any): any { return visitor.aApp(this); }
  label(): string { return 'a-app'; }
  tosource(): any {
    return PP.group(this.ann.tosource()
      .append(PP.group(PP.langle.append(PP.nest(INDENT,
        PP.separate(PP.commabreak, this.args.map((a) => a.tosource())))).append(PP.rangle))));
  }
}

export class APred extends AnnBase {
  get $name(): 'a-pred' { return 'a-pred'; }
  constructor(public l: Loc, public ann: Ann, public exp: Expr) { super(); }
  visit(visitor: any): any { return visitor.aPred(this); }
  label(): string { return 'a-pred'; }
  tosource(): any { return this.ann.tosource().append(strPercent).append(PP.parens(this.exp.tosource())); }
}

export class ADot extends AnnBase {
  get $name(): 'a-dot' { return 'a-dot'; }
  constructor(public l: Loc, public obj: Name, public field: string) { super(); }
  visit(visitor: any): any { return visitor.aDot(this); }
  label(): string { return 'a-dot'; }
  tosource(): any { return this.obj.tosource().append(PP.str('.' + this.field)); }
}

export class AChecked extends AnnBase {
  get $name(): 'a-checked' { return 'a-checked'; }
  constructor(public checked: Ann, public residual: Ann) { super(); }
  visit(visitor: any): any { return visitor.aChecked(this); }
  label(): string { return 'a-checked'; }
  tosource(): any { return this.residual.tosource(); }
}

export type Ann =
  | ABlank | AAny | AName | ATypeVar | AArrow | AArrowArgnames | AMethod
  | ARecord | ATuple | AApp | APred | ADot | AChecked;

export function isABlank(x: any): x is ABlank { return x instanceof ABlank; }
export function isAAny(x: any): x is AAny { return x instanceof AAny; }
export function isAName(x: any): x is AName { return x instanceof AName; }
export function isATypeVar(x: any): x is ATypeVar { return x instanceof ATypeVar; }
export function isAArrow(x: any): x is AArrow { return x instanceof AArrow; }
export function isAArrowArgnames(x: any): x is AArrowArgnames { return x instanceof AArrowArgnames; }
export function isAMethod(x: any): x is AMethod { return x instanceof AMethod; }
export function isARecord(x: any): x is ARecord { return x instanceof ARecord; }
export function isATuple(x: any): x is ATuple { return x instanceof ATuple; }
export function isAApp(x: any): x is AApp { return x instanceof AApp; }
export function isAPred(x: any): x is APred { return x instanceof APred; }
export function isADot(x: any): x is ADot { return x instanceof ADot; }
export function isAChecked(x: any): x is AChecked { return x instanceof AChecked; }
export function isAnn(x: any): x is Ann { return x instanceof AnnBase; }

export const aBlank: ABlank = new ABlank();

// ---------- AField ----------

export abstract class AFieldBase {
  abstract get $name(): string;
  abstract visit(visitor: any): any;
  label(): string { return raise('No label on ' + this.$name); }
  tosource(): any { return raise('No tosource on ' + this.$name); }
}

export class AField extends AFieldBase {
  get $name(): 'a-field' { return 'a-field'; }
  constructor(public l: Loc, public name: string, public ann: Ann) { super(); }
  visit(visitor: any): any { return visitor.aField(this); }
  label(): string { return 'a-field'; }
  tosource(): any {
    if (isABlank(this.ann)) {
      return PP.str(this.name);
    } else {
      return PP.infix(INDENT, 1, strColoncolon, PP.str(this.name), this.ann.tosource());
    }
  }
}

export function isAField(x: any): x is AField { return x instanceof AField; }

export function makeCheckerName(name: string): string { return 'is-' + name; }

export function flatten<T>(listOfLists: T[][]): T[] {
  let biglist: T[] = [];
  for (const piece of listOfLists) {
    biglist = [...biglist, ...piece];
  }
  return biglist;
}
