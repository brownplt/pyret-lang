/*
  TS port of src/arr/compiler/ast-anf.arr (the A-normal-form AST).
  See CONVENTIONS.md.

  Name-collision note: in ast-anf.arr several data-type names collide with
  the PascalCased name of one of their own variants (data AVal vs
  a-val of ALettable,
  data ATypeBind vs variant a-type-bind, data AVariant vs variant
  a-variant, data ACasesBranch vs variant a-cases-branch, data
  ADefinedValue vs variant a-defined-value). For those, the class is
  declared with a `$` suffix and re-exported as a `const` (value space)
  while the data-decl union is exported as a `type` (type space) under the
  plain name. So `new AVal(l, v)` constructs the a-val variant and
  `x: AVal` means the AVal data type — exactly mirroring Pyret, where
  `a-val` is the constructor and `AVal` the type.
*/

import * as A from './ast';
import { DummyLocVisitor } from './ast-visitors';
import * as PP from './pprint';
import * as SL from './srcloc';
import { InternalCompilerError, raise } from './shared';
import { PyretNumber } from './interop/js-numbers';

export type NameDict<T> = Map<string, T>;
export type FrozenNameDict<T> = Map<string, T>;

export function emptyDict<T>(): NameDict<T> { return new Map<string, T>(); }

export type Loc = SL.Loc;

// Pyret's `A.dummy-loc-visitor` value (the TS port exports the class from
// ast-visitors.ts; it is stateless, so one shared instance suffices).
const dummyLocVisitor = new DummyLocVisitor();

export const INDENT = 2;

export const breakOne = PP.sbreak(1);
export const strMethod = PP.str(' method');
export const strLetrec = PP.str('letrec ');
export const strPeriod = PP.str('.');
export const strBang = PP.str('!');
export const strCases = PP.str('cases');
export const strColon = PP.str(':');
export const strColoncolon = PP.str('::');
export const strColonspace = PP.str(':');
export const strEnd = PP.str('end');
export const strTypeLet = PP.str('type-let ');
export const strLet = PP.str('let ');
export const strVar = PP.str('var ');
export const strIf = PP.str('if ');
export const strElsecolon = PP.str('else:');
export const strElsebranch = PP.str('| else =>');
export const strThickarrow = PP.str('=>');
export const strSpacecolonequal = PP.str(' :=');
export const strSpaceequal = PP.str(' =');
export const strImport = PP.str('import');
export const strProvide = PP.str('provide');
export const strAs = PP.str('as');
export const strFrom = PP.str('from');
export const strNewtype = PP.str('newtype ');

export const dummyLoc: Loc = new SL.Builtin('dummy-location');

// Pyret `torepr` on a Srcloc value (used by a-srcloc's tosource).
function toreprLoc(loc: Loc): string {
  const l = loc as any;
  if (l.$name === 'builtin') {
    return `builtin(${JSON.stringify(l.moduleName)})`;
  }
  return `srcloc(${JSON.stringify(l.source)}, ${l.startLine}, ${l.startColumn}, ${l.startChar}, ${l.endLine}, ${l.endColumn}, ${l.endChar})`;
}

// ---------- data AProg ----------

export abstract class AProgBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class AProgram extends AProgBase {
  get $name(): 'a-program' { return 'a-program'; }
  constructor(public l: Loc, public provides: A.ProvideBlock, public imports: A.Import[], public body: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aProgram(this); }
  label(): string { return 'a-program'; }
  tosource(): PP.PPrintDoc {
    return PP.group(
      PP.flowMap(PP.hardline, (i: any) => i.tosource(), this.imports)
        .append(PP.hardline)
        .append(this.body.tosource()));
  }
}

export type AProg = AProgram;

export function isAProgram(x: any): x is AProgram { return x instanceof AProgram; }

// ---------- data ATypeBind ----------

export abstract class ATypeBindBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ATypeBind$ extends ATypeBindBase {
  get $name(): 'a-type-bind' { return 'a-type-bind'; }
  constructor(public l: Loc, public name: A.Name, public ann: A.Ann) { super(); }
  visit(visitor: any): any { return visitor.aTypeBind(this); }
  label(): string { return 'a-type-bind'; }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 1, strColoncolon, this.name.tosource(), this.ann.tosource());
  }
}

export class ANewtypeBind extends ATypeBindBase {
  get $name(): 'a-newtype-bind' { return 'a-newtype-bind'; }
  constructor(public l: Loc, public name: A.Name, public namet: A.Name) { super(); }
  visit(visitor: any): any { return visitor.aNewtypeBind(this); }
  label(): string { return 'a-newtype-bind'; }
  tosource(): PP.PPrintDoc {
    return PP.group(strNewtype.append(this.name.tosource()).append(breakOne).append(strAs).append(breakOne).append(this.namet.tosource()));
  }
}

export const ATypeBind = ATypeBind$;
export type ATypeBind = ATypeBind$ | ANewtypeBind;

export function isATypeBind(x: any): x is ATypeBind$ { return x instanceof ATypeBind$; }
export function isANewtypeBind(x: any): x is ANewtypeBind { return x instanceof ANewtypeBind; }

// ---------- data AExpr ----------

export abstract class AExprHeadBase {
  abstract get $name(): string;
  abstract l: Loc;
  abstract label(): string;
  // The old nested nodes pretty-printed themselves around their body's
  // doc; heads take the (already rendered) rest of the chain instead.
  abstract tosourceWith(body: PP.PPrintDoc): PP.PPrintDoc;
}

export class ATypeLet extends AExprHeadBase {
  get $name(): 'a-type-let' { return 'a-type-let'; }
  constructor(public l: Loc, public bind: ATypeBind) { super(); }
  label(): string { return 'a-type-let'; }
  tosourceWith(body: PP.PPrintDoc): PP.PPrintDoc {
    return PP.softSurround(INDENT, 1,
      strTypeLet
        .append(PP.group(PP.nest(INDENT,
          this.bind.tosource()))).append(strColon),
      body,
      strEnd);
  }
}

export class ALet extends AExprHeadBase {
  get $name(): 'a-let' { return 'a-let'; }
  constructor(public l: Loc, public bind: ABind, public e: ALettable) { super(); }
  label(): string { return 'a-let'; }
  tosourceWith(body: PP.PPrintDoc): PP.PPrintDoc {
    return PP.softSurround(INDENT, 1,
      strLet
        .append(PP.group(PP.nest(INDENT,
          this.bind.tosource().append(strSpaceequal).append(breakOne).append(this.e.tosource())))).append(strColon),
      body,
      strEnd);
  }
}

export class AArrLet extends AExprHeadBase {
  get $name(): 'a-arr-let' { return 'a-arr-let'; }
  constructor(public l: Loc, public bind: ABind, public idx: number, public e: ALettable) { super(); }
  label(): string { return 'a-arr-let'; }
  tosourceWith(body: PP.PPrintDoc): PP.PPrintDoc {
    return PP.softSurround(INDENT, 1,
      strLet
        .append(PP.group(
          PP.nest(
            INDENT,
            PP.group(this.bind.tosource().append(PP.brackets(PP.number(this.idx))))
              .append(strSpaceequal)
              .append(breakOne)
              .append(this.e.tosource()))))
        .append(strColon),
      body,
      strEnd);
  }
}

export class AVar extends AExprHeadBase {
  get $name(): 'a-var' { return 'a-var'; }
  constructor(public l: Loc, public bind: ABind, public e: ALettable) { super(); }
  label(): string { return 'a-var'; }
  tosourceWith(body: PP.PPrintDoc): PP.PPrintDoc {
    return PP.softSurround(INDENT, 1,
      strVar
        .append(PP.group(PP.nest(INDENT,
          this.bind.tosource().append(strSpaceequal).append(breakOne).append(this.e.tosource())))).append(strColon),
      body,
      strEnd);
  }
}

export class ASeq extends AExprHeadBase {
  get $name(): 'a-seq' { return 'a-seq'; }
  constructor(public l: Loc, public e1: ALettable) { super(); }
  label(): string { return 'a-seq'; }
  tosourceWith(body: PP.PPrintDoc): PP.PPrintDoc {
    return this.e1.tosource().append(PP.hardline).append(body);
  }
}

export type AExprHead = ATypeLet | ALet | AArrLet | AVar | ASeq;

export class AExpr {
  get $name(): 'a-expr' { return 'a-expr'; }
  constructor(public heads: AExprHead[], public e: ALettable) {}
  // The old chain's root node was the first statement; its loc is what
  // consumers of `.l` saw, so derive it the same way.
  get l(): Loc { return this.heads.length > 0 ? this.heads[0].l : this.e.l; }
  label(): string { return 'a-expr'; }
  tosource(): PP.PPrintDoc {
    let doc = this.e.tosource();
    for (let i = this.heads.length - 1; i >= 0; i--) {
      doc = this.heads[i].tosourceWith(doc);
    }
    return doc;
  }
}

export function isATypeLet(x: any): x is ATypeLet { return x instanceof ATypeLet; }
export function isALet(x: any): x is ALet { return x instanceof ALet; }
export function isAArrLet(x: any): x is AArrLet { return x instanceof AArrLet; }
export function isAVar(x: any): x is AVar { return x instanceof AVar; }
export function isASeq(x: any): x is ASeq { return x instanceof ASeq; }
export function isAExpr(x: any): x is AExpr { return x instanceof AExpr; }

// ---------- data ABind ----------

export abstract class ABindBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ABind extends ABindBase {
  get $name(): 'a-bind' { return 'a-bind'; }
  constructor(public l: Loc, public id: A.Name, public ann: A.Ann) { super(); }
  visit(visitor: any): any { return visitor.aBind(this); }
  label(): string { return 'a-bind'; }
  tosource(): PP.PPrintDoc {
    if (A.isABlank(this.ann)) { return this.id.toCompiledSource(); }
    else { return PP.infix(INDENT, 1, strColoncolon, this.id.toCompiledSource(), this.ann.tosource()); }
  }
}

export function isABind(x: any): x is ABind { return x instanceof ABind; }

// ---------- data AVariant ----------

export abstract class AVariantBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class AVariant$ extends AVariantBase {
  get $name(): 'a-variant' { return 'a-variant'; }
  constructor(
    public l: Loc,
    public constrLoc: Loc,
    public name: string,
    public members: AVariantMember[],
    public withMembers: AField[]
  ) { super(); }
  visit(visitor: any): any { return visitor.aVariant(this); }
  label(): string { return 'a-variant'; }
  tosource(): PP.PPrintDoc { return PP.str('a-variant'); }
}

export class ASingletonVariant extends AVariantBase {
  get $name(): 'a-singleton-variant' { return 'a-singleton-variant'; }
  constructor(
    public l: Loc,
    public name: string,
    public withMembers: AField[]
  ) { super(); }
  visit(visitor: any): any { return visitor.aSingletonVariant(this); }
  label(): string { return 'a-variant'; }
  tosource(): PP.PPrintDoc { return PP.str('a-variant'); }
}

export const AVariant = AVariant$;
export type AVariant = AVariant$ | ASingletonVariant;

export function isAVariant(x: any): x is AVariant$ { return x instanceof AVariant$; }
export function isASingletonVariant(x: any): x is ASingletonVariant { return x instanceof ASingletonVariant; }

// ---------- data AMemberType ----------
// (no sharing visit method in the Pyret source)

export abstract class AMemberTypeBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
}

export class ANormal extends AMemberTypeBase {
  get $name(): 'a-normal' { return 'a-normal'; }
  label(): string { return 'a-normal'; }
  tosource(): PP.PPrintDoc { return PP.str(''); }
}

export class AMutable extends AMemberTypeBase {
  get $name(): 'a-mutable' { return 'a-mutable'; }
  label(): string { return 'a-mutable'; }
  tosource(): PP.PPrintDoc { return PP.str('mutable '); }
}

export type AMemberType = ANormal | AMutable;

export function isANormal(x: any): x is ANormal { return x instanceof ANormal; }
export function isAMutable(x: any): x is AMutable { return x instanceof AMutable; }

// ---------- data AVariantMember ----------

export abstract class AVariantMemberBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class AVariantMember extends AVariantMemberBase {
  get $name(): 'a-variant-member' { return 'a-variant-member'; }
  constructor(public l: Loc, public memberType: AMemberType, public bind: ABind) { super(); }
  visit(visitor: any): any { return visitor.aVariantMember(this); }
  label(): string { return 'a-variant-member'; }
  tosource(): PP.PPrintDoc {
    // NOTE: the Pyret source says `self.member_type` (a nonexistent
    // field, so its tosource would error); ported with the intended field.
    return this.memberType.tosource().append(this.bind.tosource());
  }
}

export function isAVariantMember(x: any): x is AVariantMember { return x instanceof AVariantMember; }

// ---------- data ACasesBind ----------

export abstract class ACasesBindBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ACasesBind extends ACasesBindBase {
  get $name(): 'a-cases-bind' { return 'a-cases-bind'; }
  constructor(public l: Loc, public fieldType: A.CasesBindType, public bind: ABind) { super(); }
  visit(visitor: any): any { return visitor.aCasesBind(this); }
  label(): string { return 's-cases-bind'; }
  tosource(): PP.PPrintDoc {
    const ft = this.fieldType.tosource();
    if (PP.isMtDoc(ft)) { return this.bind.tosource(); }
    else { return ft.append(PP.str(' ')).append(this.bind.tosource()); }
  }
}

export function isACasesBind(x: any): x is ACasesBind { return x instanceof ACasesBind; }

// ---------- data ACasesBranch ----------

export abstract class ACasesBranchBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ACasesBranch$ extends ACasesBranchBase {
  get $name(): 'a-cases-branch' { return 'a-cases-branch'; }
  constructor(public l: Loc, public patLoc: Loc, public name: string, public args: ACasesBind[], public body: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aCasesBranch(this); }
  label(): string { return 'a-cases-branch'; }
  tosource(): PP.PPrintDoc {
    return PP.nest(INDENT,
      PP.group(PP.str('| ' + this.name)
          .append(PP.surroundSeparate(INDENT, 0, PP.str('()'), PP.lparen, PP.commabreak, PP.rparen,
            this.args.map((a) => a.tosource()))).append(breakOne).append(strThickarrow)).append(breakOne)
        .append(PP.nest(INDENT, this.body.tosource())));
  }
}

export class ASingletonCasesBranch extends ACasesBranchBase {
  get $name(): 'a-singleton-cases-branch' { return 'a-singleton-cases-branch'; }
  constructor(public l: Loc, public patLoc: Loc, public name: string, public body: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aSingletonCasesBranch(this); }
  label(): string { return 'a-singleton-cases-branch'; }
  tosource(): PP.PPrintDoc {
    return PP.nest(INDENT,
      PP.group(PP.str('| ' + this.name).append(breakOne).append(strThickarrow)).append(breakOne)
        .append(PP.nest(INDENT, this.body.tosource())));
  }
}

export const ACasesBranch = ACasesBranch$;
export type ACasesBranch = ACasesBranch$ | ASingletonCasesBranch;

export function isACasesBranch(x: any): x is ACasesBranch$ { return x instanceof ACasesBranch$; }
export function isASingletonCasesBranch(x: any): x is ASingletonCasesBranch { return x instanceof ASingletonCasesBranch; }

// ---------- data ADefinedModule ----------
// (no sharing visit method in the Pyret source)

export abstract class ADefinedModuleBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
}

export class ADefinedModule extends ADefinedModuleBase {
  get $name(): 'a-defined-module' { return 'a-defined-module'; }
  constructor(public name: string, public value: A.Name, public uri: string) { super(); }
  label(): string { return 'a-defined-module'; }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 1, strColon, PP.str(this.name), PP.str(this.uri));
  }
}

export function isADefinedModule(x: any): x is ADefinedModule { return x instanceof ADefinedModule; }

// ---------- data ADefinedValue ----------

export abstract class ADefinedValueBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ADefinedValue$ extends ADefinedValueBase {
  get $name(): 'a-defined-value' { return 'a-defined-value'; }
  constructor(public name: string, public value: AVal) { super(); }
  visit(visitor: any): any { return visitor.aDefinedValue(this); }
  label(): string { return 'a-defined-value'; }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 1, strColon, PP.str(this.name), this.value.tosource());
  }
}

export class ADefinedVar extends ADefinedValueBase {
  get $name(): 'a-defined-var' { return 'a-defined-var'; }
  constructor(public name: string, public id: A.Name) { super(); }
  visit(visitor: any): any { return visitor.aDefinedVar(this); }
  label(): string { return 'a-defined-var'; }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 1, strColon, PP.str(this.name), this.id.tosource());
  }
}

export const ADefinedValue = ADefinedValue$;
export type ADefinedValue = ADefinedValue$ | ADefinedVar;

export function isADefinedValue(x: any): x is ADefinedValue$ { return x instanceof ADefinedValue$; }
export function isADefinedVar(x: any): x is ADefinedVar { return x instanceof ADefinedVar; }

// ---------- data ADefinedType ----------

export abstract class ADefinedTypeBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ADefinedType extends ADefinedTypeBase {
  get $name(): 'a-defined-type' { return 'a-defined-type'; }
  constructor(public name: string, public typ: A.Ann) { super(); }
  visit(visitor: any): any { return visitor.aDefinedType(this); }
  label(): string { return 'a-defined-type'; }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 1, strColoncolon, PP.str(this.name), this.typ.tosource());
  }
}

export function isADefinedType(x: any): x is ADefinedType { return x instanceof ADefinedType; }

// ---------- data ALettable ----------

export abstract class ALettableBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class AModule extends ALettableBase {
  get $name(): 'a-module' { return 'a-module'; }
  constructor(
    public l: Loc,
    public answer: AVal,
    public definedModules: ADefinedModule[],
    public definedValues: ADefinedValue[],
    public definedTypes: ADefinedType[],
    public checks: AVal
  ) { super(); }
  visit(visitor: any): any { return visitor.aModule(this); }
  label(): string { return 'a-module'; }
  tosource(): PP.PPrintDoc {
    return PP.str('Module').append(PP.parens(PP.flowMap(PP.commabreak, (x: PP.PPrintDoc) => x, [
      PP.infix(INDENT, 1, strColon, PP.str('Answer'), this.answer.tosource()),
      PP.infix(INDENT, 1, strColon, PP.str('DefinedValues'),
        PP.brackets(PP.flowMap(PP.commabreak, (dv: ADefinedValue) => dv.tosource(), this.definedValues))),
      PP.infix(INDENT, 1, strColon, PP.str('DefinedTypes'),
        PP.brackets(PP.flowMap(PP.commabreak, (dt: ADefinedType) => dt.tosource(), this.definedTypes))),
      PP.infix(INDENT, 1, strColon, PP.str('checks'), this.checks.tosource())])));
  }
}

export class AIdVar extends ALettableBase {
  get $name(): 'a-id-var' { return 'a-id-var'; }
  constructor(public l: Loc, public id: A.Name) { super(); }
  visit(visitor: any): any { return visitor.aIdVar(this); }
  label(): string { return 'a-id-var'; }
  tosource(): PP.PPrintDoc { return PP.str('!' + this.id.key()); }
}

export class AIdVarModref extends ALettableBase {
  get $name(): 'a-id-var-modref' { return 'a-id-var-modref'; }
  constructor(public l: Loc, public id: A.Name, public uri: string, public name: string) { super(); }
  visit(visitor: any): any { return visitor.aIdVarModref(this); }
  label(): string { return 'a-id-var-modref'; }
  tosource(): PP.PPrintDoc {
    return this.id.tosource().append(PP.str('@!')).append(PP.parens(PP.str(this.uri))).append(PP.str('.' + this.name));
  }
}

export class AIdLetrec extends ALettableBase {
  get $name(): 'a-id-letrec' { return 'a-id-letrec'; }
  constructor(public l: Loc, public id: A.Name, public safe: boolean) { super(); }
  visit(visitor: any): any { return visitor.aIdLetrec(this); }
  label(): string { return 'a-id-letrec'; }
  tosource(): PP.PPrintDoc { return PP.str('~!' + this.id.key()); }
}

export class ACases extends ALettableBase {
  get $name(): 'a-cases' { return 'a-cases'; }
  constructor(public l: Loc, public typ: A.Ann, public val: AVal, public branches: ACasesBranch[], public _else: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aCases(this); }
  label(): string { return 'a-cases'; }
  tosource(): PP.PPrintDoc {
    const header = strCases.append(PP.parens(this.typ.tosource())).append(breakOne)
      .append(this.val.tosource()).append(strColon);
    const body = PP.separate(breakOne, this.branches.map((b) => PP.group(b.tosource())))
      .append(breakOne).append(PP.group(strElsebranch.append(PP.nest(INDENT, breakOne.append(this._else.tosource())))));
    return PP.surround(INDENT, 1, PP.group(header), body, strEnd);
  }
}


export class AIfBranch {
  get $name(): 'a-if-branch' { return 'a-if-branch'; }
  constructor(public l: Loc, public heads: AExprHead[], public test: AVal, public body: AExpr) {}
}

export class AIf extends ALettableBase {
  get $name(): 'a-if' { return 'a-if'; }
  constructor(public l: Loc, public branches: AIfBranch[], public elseBody: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aIf(this); }
  label(): string { return 'a-if'; }
  tosource(): PP.PPrintDoc {
    // Rendered as the nested chain the Pyret original stored.
    let doc = this.elseBody.tosource();
    for (let i = this.branches.length - 1; i >= 0; i--) {
      const b = this.branches[i];
      doc = PP.group(
        strIf.append(PP.nest(2 * INDENT, b.test.tosource().append(strColon)))
          .append(PP.nest(INDENT, breakOne.append(b.body.tosource())))
          .append(breakOne).append(strElsecolon)
          .append(PP.nest(INDENT, breakOne.append(doc)))
          .append(breakOne).append(strEnd));
      for (let j = b.heads.length - 1; j >= 0; j--) {
        doc = b.heads[j].tosourceWith(doc);
      }
    }
    return doc;
  }
}

export class ADataExpr extends ALettableBase {
  get $name(): 'a-data-expr' { return 'a-data-expr'; }
  constructor(public l: Loc, public name: string, public namet: A.Name, public variants: AVariant[], public shared: AField[]) { super(); }
  visit(visitor: any): any { return visitor.aDataExpr(this); }
  label(): string { return 'a-data-expr'; }
  tosource(): PP.PPrintDoc {
    return PP.str('data-expr');
  }
}

export class AAssign extends ALettableBase {
  get $name(): 'a-assign' { return 'a-assign'; }
  constructor(public l: Loc, public id: A.Name, public value: AVal) { super(); }
  visit(visitor: any): any { return visitor.aAssign(this); }
  label(): string { return 'a-assign'; }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.nest(INDENT, this.id.toCompiledSource().append(strSpacecolonequal).append(breakOne).append(this.value.tosource())));
  }
}

export class AApp extends ALettableBase {
  get $name(): 'a-app' { return 'a-app'; }
  constructor(public l: Loc, public _fun: AVal, public args: AVal[], public appInfo: A.AppInfo) { super(); }
  visit(visitor: any): any { return visitor.aApp(this); }
  label(): string { return 'a-app'; }
  tosource(): PP.PPrintDoc {
    return PP.group(this._fun.tosource()
        .append(PP.parens(PP.nest(INDENT,
          PP.separate(PP.commabreak, this.args.map((f) => f.tosource()))))));
  }
}

export class AMethodApp extends ALettableBase {
  get $name(): 'a-method-app' { return 'a-method-app'; }
  constructor(public l: Loc, public obj: AVal, public meth: string, public args: AVal[]) { super(); }
  visit(visitor: any): any { return visitor.aMethodApp(this); }
  label(): string { return 'a-app'; }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.infix(INDENT, 0, strPeriod, this.obj.tosource(), PP.str(this.meth))
        .append(PP.parens(PP.nest(INDENT,
          PP.separate(PP.commabreak, this.args.map((f) => f.tosource()))))));
  }
}

export class APrimApp extends ALettableBase {
  get $name(): 'a-prim-app' { return 'a-prim-app'; }
  constructor(public l: Loc, public f: string, public args: AVal[], public appInfo: A.PrimAppInfo) { super(); }
  visit(visitor: any): any { return visitor.aPrimApp(this); }
  label(): string { return 'a-prim-app'; }
  tosource(): PP.PPrintDoc {
    return PP.group(PP.str(this.f)
        .append(PP.parens(PP.nest(INDENT,
          PP.separate(PP.commabreak, this.args.map((f) => f.tosource()))))));
  }
}

export class ARef extends ALettableBase {
  get $name(): 'a-ref' { return 'a-ref'; }
  constructor(public l: Loc, public ann: A.Ann | undefined) { super(); }
  visit(visitor: any): any { return visitor.aRef(this); }
  label(): string { return 'a-ref'; }
  tosource(): PP.PPrintDoc {
    if (this.ann === undefined) {
      return PP.str('bare-ref');
    } else {
      return PP.group(PP.str('ref ').append(this.ann.tosource()));
    }
  }
}

export class ATuple extends ALettableBase {
  get $name(): 'a-tuple' { return 'a-tuple'; }
  constructor(public l: Loc, public fields: AVal[]) { super(); }
  visit(visitor: any): any { return visitor.aTuple(this); }
  label(): string { return 'a-tuple'; }
  tosource(): PP.PPrintDoc {
    return PP.surroundSeparate(INDENT, 1, PP.str("Empty tuple shoudn't happen"),
      PP.lbrace, PP.semibreak, PP.rbrace, this.fields.map((f) => f.tosource()));
  }
}

export class ATupleGet extends ALettableBase {
  get $name(): 'a-tuple-get' { return 'a-tuple-get'; }
  constructor(public l: Loc, public tup: AVal, public index: number) { super(); }
  visit(visitor: any): any { return visitor.aTupleGet(this); }
  label(): string { return 's-tuple-get'; }
  tosource(): PP.PPrintDoc {
    return this.tup.tosource().append(PP.str('.')).append(PP.lbrace).append(PP.number(this.index)).append(PP.rbrace);
  }
}

export class AObj extends ALettableBase {
  get $name(): 'a-obj' { return 'a-obj'; }
  constructor(public l: Loc, public fields: AField[]) { super(); }
  visit(visitor: any): any { return visitor.aObj(this); }
  label(): string { return 'a-obj'; }
  tosource(): PP.PPrintDoc {
    return PP.surroundSeparate(INDENT, 1, PP.lbrace.append(PP.rbrace),
      PP.lbrace, PP.commabreak, PP.rbrace, this.fields.map((f) => f.tosource()));
  }
}

export class AUpdate extends ALettableBase {
  get $name(): 'a-update' { return 'a-update'; }
  constructor(public l: Loc, public supe: AVal, public fields: AField[]) { super(); }
  visit(visitor: any): any { return visitor.aUpdate(this); }
  label(): string { return 'a-update'; }
  tosource(): PP.PPrintDoc {
    return PP.str('update');
  }
}

export class AExtend extends ALettableBase {
  get $name(): 'a-extend' { return 'a-extend'; }
  constructor(public l: Loc, public supe: AVal, public fields: AField[]) { super(); }
  visit(visitor: any): any { return visitor.aExtend(this); }
  label(): string { return 'a-extend'; }
  tosource(): PP.PPrintDoc {
    return PP.str('extend');
  }
}

export class ADot extends ALettableBase {
  get $name(): 'a-dot' { return 'a-dot'; }
  constructor(public l: Loc, public obj: AVal, public field: string) { super(); }
  visit(visitor: any): any { return visitor.aDot(this); }
  label(): string { return 'a-dot'; }
  tosource(): PP.PPrintDoc { return PP.infix(INDENT, 0, strPeriod, this.obj.tosource(), PP.str(this.field)); }
}

export class AColon extends ALettableBase {
  get $name(): 'a-colon' { return 'a-colon'; }
  constructor(public l: Loc, public obj: AVal, public field: string) { super(); }
  visit(visitor: any): any { return visitor.aColon(this); }
  label(): string { return 'a-colon'; }
  tosource(): PP.PPrintDoc { return PP.infix(INDENT, 0, strColon, this.obj.tosource(), PP.str(this.field)); }
}

export class AGetBang extends ALettableBase {
  get $name(): 'a-get-bang' { return 'a-get-bang'; }
  constructor(public l: Loc, public obj: AVal, public field: string) { super(); }
  visit(visitor: any): any { return visitor.aGetBang(this); }
  label(): string { return 'a-get-bang'; }
  tosource(): PP.PPrintDoc { return PP.infix(INDENT, 0, strBang, this.obj.tosource(), PP.str(this.field)); }
}

export class ALam extends ALettableBase {
  get $name(): 'a-lam' { return 'a-lam'; }
  constructor(public l: Loc, public name: string, public args: ABind[], public ret: A.Ann, public body: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aLam(this); }
  label(): string { return 'a-lam'; }
  tosource(): PP.PPrintDoc { return funMethodPretty(PP.str('lam'), this.args, this.body); }
}

export class AMethod extends ALettableBase {
  get $name(): 'a-method' { return 'a-method'; }
  constructor(public l: Loc, public name: string, public args: ABind[], public ret: A.Ann, public body: AExpr) { super(); }
  visit(visitor: any): any { return visitor.aMethod(this); }
  label(): string { return 'a-method'; }
  tosource(): PP.PPrintDoc { return funMethodPretty(PP.str('method'), this.args, this.body); }
}

export class AVal$ extends ALettableBase {
  get $name(): 'a-val' { return 'a-val'; }
  constructor(public l: Loc, public v: AVal) { super(); }
  visit(visitor: any): any { return visitor.aVal(this); }
  label(): string { return 'a-val'; }
  tosource(): PP.PPrintDoc { return this.v.tosource(); }
}

export const AVal = AVal$;
export type ALettable =
  | AModule
  | AIdVar
  | AIdVarModref
  | AIdLetrec
  | ACases
  | AIf
  | ADataExpr
  | AAssign
  | AApp
  | AMethodApp
  | APrimApp
  | ARef
  | ATuple
  | ATupleGet
  | AObj
  | AUpdate
  | AExtend
  | ADot
  | AColon
  | AGetBang
  | ALam
  | AMethod
  | AVal$;

export function isAModule(x: any): x is AModule { return x instanceof AModule; }
export function isAIdVar(x: any): x is AIdVar { return x instanceof AIdVar; }
export function isAIdVarModref(x: any): x is AIdVarModref { return x instanceof AIdVarModref; }
export function isAIdLetrec(x: any): x is AIdLetrec { return x instanceof AIdLetrec; }
export function isACases(x: any): x is ACases { return x instanceof ACases; }
export function isAIf(x: any): x is AIf { return x instanceof AIf; }
export function isADataExpr(x: any): x is ADataExpr { return x instanceof ADataExpr; }
export function isAAssign(x: any): x is AAssign { return x instanceof AAssign; }
export function isAApp(x: any): x is AApp { return x instanceof AApp; }
export function isAMethodApp(x: any): x is AMethodApp { return x instanceof AMethodApp; }
export function isAPrimApp(x: any): x is APrimApp { return x instanceof APrimApp; }
export function isARef(x: any): x is ARef { return x instanceof ARef; }
export function isATuple(x: any): x is ATuple { return x instanceof ATuple; }
export function isATupleGet(x: any): x is ATupleGet { return x instanceof ATupleGet; }
export function isAObj(x: any): x is AObj { return x instanceof AObj; }
export function isAUpdate(x: any): x is AUpdate { return x instanceof AUpdate; }
export function isAExtend(x: any): x is AExtend { return x instanceof AExtend; }
export function isADot(x: any): x is ADot { return x instanceof ADot; }
export function isAColon(x: any): x is AColon { return x instanceof AColon; }
export function isAGetBang(x: any): x is AGetBang { return x instanceof AGetBang; }
export function isALam(x: any): x is ALam { return x instanceof ALam; }
export function isAMethod(x: any): x is AMethod { return x instanceof AMethod; }
export function isAVal(x: any): x is AVal$ { return x instanceof AVal$; }

export function funMethodPretty(typ: PP.PPrintDoc, args: ABind[], body: AExpr): PP.PPrintDoc {
  const argList = PP.nest(INDENT,
    PP.surroundSeparate(INDENT, 0, PP.lparen.append(PP.rparen), PP.lparen, PP.commabreak, PP.rparen,
      args.map((a) => a.tosource())));
  const header = PP.group(typ.append(argList).append(strColon));
  return PP.surround(INDENT, 1, header, body.tosource(), strEnd);
}

// ---------- data AField ----------

export abstract class AFieldBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class AField extends AFieldBase {
  get $name(): 'a-field' { return 'a-field'; }
  constructor(public l: Loc, public name: string, public value: AVal) { super(); }
  visit(visitor: any): any { return visitor.aField(this); }
  label(): string { return 'a-field'; }
  tosource(): PP.PPrintDoc {
    return PP.nest(INDENT, PP.str(this.name).append(strColonspace).append(this.value.tosource()));
  }
}

export function isAField(x: any): x is AField { return x instanceof AField; }

// ---------- data AVal ----------

export abstract class AValBase {
  abstract get $name(): string;
  abstract label(): string;
  abstract tosource(): PP.PPrintDoc;
  abstract visit(visitor: any): any;
}

export class ASrcloc extends AValBase {
  get $name(): 'a-srcloc' { return 'a-srcloc'; }
  constructor(public l: Loc, public loc: Loc) { super(); }
  visit(visitor: any): any { return visitor.aSrcloc(this); }
  label(): string { return 'a-srcloc'; }
  tosource(): PP.PPrintDoc { return PP.str(toreprLoc(this.loc)); }
}

export class ANum extends AValBase {
  get $name(): 'a-num' { return 'a-num'; }
  constructor(public l: Loc, public n: PyretNumber) { super(); }
  visit(visitor: any): any { return visitor.aNum(this); }
  label(): string { return 'a-num'; }
  tosource(): PP.PPrintDoc { return PP.number(this.n); }
}

export class AStr extends AValBase {
  get $name(): 'a-str' { return 'a-str'; }
  constructor(public l: Loc, public s: string) { super(); }
  visit(visitor: any): any { return visitor.aStr(this); }
  label(): string { return 'a-str'; }
  tosource(): PP.PPrintDoc { return PP.str(JSON.stringify(this.s)); }
}

export class ABool extends AValBase {
  get $name(): 'a-bool' { return 'a-bool'; }
  constructor(public l: Loc, public b: boolean) { super(); }
  visit(visitor: any): any { return visitor.aBool(this); }
  label(): string { return 'a-bool'; }
  tosource(): PP.PPrintDoc { return PP.str(String(this.b)); }
}

// used for letrec
export class AUndefined extends AValBase {
  get $name(): 'a-undefined' { return 'a-undefined'; }
  constructor(public l: Loc) { super(); }
  visit(visitor: any): any { return visitor.aUndefined(this); }
  label(): string { return 'a-undefined'; }
  tosource(): PP.PPrintDoc { return PP.str('UNDEFINED'); }
}

export class APrimVal extends AValBase {
  get $name(): 'a-prim-val' { return 'a-prim-val'; }
  constructor(public l: Loc, public name: string) { super(); }
  visit(visitor: any): any { return visitor.aPrimVal(this); }
  label(): string { return 'a-prim-val'; }
  tosource(): PP.PPrintDoc {
    return PP.infix(INDENT, 0, strPeriod, PP.str('%runtime'), PP.str(this.name));
  }
}

export class AId extends AValBase {
  get $name(): 'a-id' { return 'a-id'; }
  constructor(public l: Loc, public id: A.Name) { super(); }
  visit(visitor: any): any { return visitor.aId(this); }
  label(): string { return 'a-id'; }
  tosource(): PP.PPrintDoc { return this.id.toCompiledSource(); }
}

export class AIdModref extends AValBase {
  get $name(): 'a-id-modref' { return 'a-id-modref'; }
  constructor(public l: Loc, public id: A.Name, public uri: string, public name: string) { super(); }
  visit(visitor: any): any { return visitor.aIdModref(this); }
  label(): string { return 'a-id-modref'; }
  tosource(): PP.PPrintDoc {
    return this.id.tosource().append(PP.str('@')).append(PP.parens(PP.str(this.uri))).append(PP.str('.' + this.name));
  }
}

export class AIdSafeLetrec extends AValBase {
  get $name(): 'a-id-safe-letrec' { return 'a-id-safe-letrec'; }
  constructor(public l: Loc, public id: A.Name) { super(); }
  visit(visitor: any): any { return visitor.aIdSafeLetrec(this); }
  label(): string { return 'a-id-safe-letrec'; }
  tosource(): PP.PPrintDoc { return PP.str('~').append(this.id.tosource()); }
}

export type AVal =
  | ASrcloc
  | ANum
  | AStr
  | ABool
  | AUndefined
  | APrimVal
  | AId
  | AIdModref
  | AIdSafeLetrec;

export function isASrcloc(x: any): x is ASrcloc { return x instanceof ASrcloc; }
export function isANum(x: any): x is ANum { return x instanceof ANum; }
export function isAStr(x: any): x is AStr { return x instanceof AStr; }
export function isABool(x: any): x is ABool { return x instanceof ABool; }
export function isAUndefined(x: any): x is AUndefined { return x instanceof AUndefined; }
export function isAPrimVal(x: any): x is APrimVal { return x instanceof APrimVal; }
export function isAId(x: any): x is AId { return x instanceof AId; }
export function isAIdModref(x: any): x is AIdModref { return x instanceof AIdModref; }
export function isAIdSafeLetrec(x: any): x is AIdSafeLetrec { return x instanceof AIdSafeLetrec; }

// ---------- strip-loc ----------

export function stripLocProg(p: AProg): AProg {
  switch (p.$name) {
    case 'a-program':
      return new AProgram(dummyLoc, p.provides, p.imports, stripLocExpr(p.body));
    default:
      throw new InternalCompilerError('No cases matched in strip-loc-prog: ' + (p as any).$name);
  }
}

export function stripLocExpr(expr: AExpr): AExpr {
  return new AExpr(expr.heads.map(stripLocHead), stripLocLettable(expr.e));
}

export function stripLocHead(head: AExprHead): AExprHead {
  // Mirrors the per-variant behavior of the nested original (note
  // a-type-let and a-arr-let did not strip their sub-pieces there either).
  switch (head.$name) {
    case 'a-type-let':
      return new ATypeLet(dummyLoc, head.bind);
    case 'a-let':
      return new ALet(dummyLoc, stripLocBind(head.bind), stripLocLettable(head.e));
    case 'a-arr-let':
      return new AArrLet(dummyLoc, head.bind, head.idx, head.e);
    case 'a-var':
      return new AVar(dummyLoc, stripLocBind(head.bind), stripLocLettable(head.e));
    case 'a-seq':
      return new ASeq(dummyLoc, stripLocLettable(head.e1));
    default:
      throw new InternalCompilerError('No cases matched in strip-loc-head: ' + (head as any).$name);
  }
}

export function stripLocBind(bind: ABind): ABind {
  switch (bind.$name) {
    case 'a-bind':
      return new ABind(dummyLoc, bind.id, bind.ann.visit(dummyLocVisitor));
    default:
      throw new InternalCompilerError('No cases matched in strip-loc-bind: ' + (bind as any).$name);
  }
}

export function stripLocLettable(lettable: ALettable): ALettable {
  switch (lettable.$name) {
    case 'a-module':
      // NOTE: the Pyret source destructures a-module with one too few
      // fields (it predates defined-modules), so this branch would error
      // there; ported with the module fields carried through unchanged.
      return new AModule(dummyLoc, stripLocVal(lettable.answer), lettable.definedModules,
        lettable.definedValues, lettable.definedTypes, stripLocVal(lettable.checks));
    case 'a-if':
      return new AIf(dummyLoc,
        lettable.branches.map((b) => new AIfBranch(dummyLoc, b.heads.map(stripLocHead), stripLocVal(b.test), stripLocExpr(b.body))),
        stripLocExpr(lettable.elseBody));
    case 'a-assign':
      return new AAssign(dummyLoc, lettable.id, stripLocVal(lettable.value));
    case 'a-app':
      return new AApp(dummyLoc, stripLocVal(lettable._fun), lettable.args.map(stripLocVal), lettable.appInfo);
    case 'a-method-app':
      return new AMethodApp(dummyLoc, stripLocVal(lettable.obj), lettable.meth, lettable.args.map(stripLocVal));
    case 'a-prim-app':
      return new APrimApp(dummyLoc, lettable.f, lettable.args.map(stripLocVal), lettable.appInfo);
    case 'a-ref':
      // A.dummy-loc-visitor.option(ann): Option<Ann> maps to Ann | undefined
      return new ARef(dummyLoc, lettable.ann === undefined ? undefined : lettable.ann.visit(dummyLocVisitor));
    case 'a-tuple':
      return new ATuple(dummyLoc, lettable.fields.map(stripLocVal));
    case 'a-tuple-get':
      return new ATupleGet(dummyLoc, stripLocVal(lettable.tup), lettable.index);
    case 'a-obj':
      return new AObj(dummyLoc, lettable.fields.map(stripLocField));
    case 'a-update':
      // NOTE: the Pyret source passes `_` for the loc here (a curried
      // constructor, evidently a bug); ported with dummy-loc like the rest.
      return new AUpdate(dummyLoc, stripLocVal(lettable.supe), lettable.fields.map(stripLocField));
    case 'a-extend':
      // NOTE: same `_` bug as a-update in the Pyret source.
      return new AExtend(dummyLoc, stripLocVal(lettable.supe), lettable.fields.map(stripLocField));
    case 'a-dot':
      return new ADot(dummyLoc, stripLocVal(lettable.obj), lettable.field);
    case 'a-colon':
      return new AColon(dummyLoc, stripLocVal(lettable.obj), lettable.field);
    case 'a-get-bang':
      return new AGetBang(dummyLoc, stripLocVal(lettable.obj), lettable.field);
    case 'a-lam':
      return new ALam(dummyLoc, lettable.name, lettable.args, lettable.ret, stripLocExpr(lettable.body));
    case 'a-method':
      return new AMethod(dummyLoc, lettable.name, lettable.args, lettable.ret, stripLocExpr(lettable.body));
    case 'a-id-var':
      return new AIdVar(dummyLoc, lettable.id);
    case 'a-id-var-modref':
      return new AIdVarModref(dummyLoc, lettable.id, lettable.uri, lettable.name);
    case 'a-id-letrec':
      return new AIdLetrec(dummyLoc, lettable.id, lettable.safe);
    case 'a-val':
      return new AVal$(dummyLoc, stripLocVal(lettable.v));
    default:
      // a-cases and a-data-expr are unhandled in the Pyret source too
      throw new InternalCompilerError('No cases matched in strip-loc-lettable: ' + (lettable as any).$name);
  }
}

export function stripLocField(field: AField): AField {
  switch (field.$name) {
    case 'a-field':
      return new AField(dummyLoc, field.name, stripLocVal(field.value));
    default:
      throw new InternalCompilerError('No cases matched in strip-loc-field: ' + (field as any).$name);
  }
}

export function stripLocVal(val: AVal): AVal {
  switch (val.$name) {
    case 'a-srcloc':
      return new ASrcloc(dummyLoc, val.loc);
    case 'a-num':
      return new ANum(dummyLoc, val.n);
    case 'a-str':
      return new AStr(dummyLoc, val.s);
    case 'a-bool':
      return new ABool(dummyLoc, val.b);
    case 'a-undefined':
      return new AUndefined(dummyLoc);
    case 'a-prim-val':
      return new APrimVal(dummyLoc, val.name);
    case 'a-id':
      return new AId(dummyLoc, val.id);
    case 'a-id-modref':
      return new AIdModref(dummyLoc, val.id, val.uri, val.name);
    case 'a-id-safe-letrec':
      return new AIdSafeLetrec(dummyLoc, val.id);
    default:
      throw new InternalCompilerError('No cases matched in strip-loc-val: ' + (val as any).$name);
  }
}

// ---------- freevars ----------

export function freevarsListAcc(anns: A.Ann[], seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  let acc = seenSoFar;
  for (const a of anns) {
    acc = freevarsAnnAcc(a, acc);
  }
  return acc;
}

export function freevarsFieldsAcc(fields: A.AField[], seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  let acc = seenSoFar;
  for (const f of fields) {
    acc = freevarsAnnAcc(f.ann, acc);
  }
  return acc;
}

export function freevarsAnnAcc(ann: A.Ann, seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  switch (ann.$name) {
    case 'a-blank': return seenSoFar;
    case 'a-any': return seenSoFar;
    case 'a-name': {
      seenSoFar.set(ann.id.key(), ann.id);
      return seenSoFar;
    }
    case 'a-type-var': return seenSoFar;
    case 'a-dot': {
      seenSoFar.set(ann.obj.key(), ann.obj);
      return seenSoFar;
    }
    case 'a-arrow': return freevarsListAcc(ann.args, freevarsAnnAcc(ann.ret, seenSoFar));
    case 'a-arrow-argnames': return freevarsFieldsAcc(ann.args, freevarsAnnAcc(ann.ret, seenSoFar));
    case 'a-method': return freevarsListAcc(ann.args, freevarsAnnAcc(ann.ret, seenSoFar));
    case 'a-record': return freevarsFieldsAcc(ann.fields, seenSoFar);
    case 'a-tuple': return freevarsListAcc(ann.fields, seenSoFar);
    case 'a-app': return freevarsListAcc(ann.args, freevarsAnnAcc(ann.ann, seenSoFar));
    case 'a-pred': {
      const pred = ann.exp;
      let name: A.Name;
      switch (pred.$name) {
        case 's-id':
          name = pred.id;
          break;
        case 's-id-letrec':
          name = pred.id;
          break;
        default:
          throw new InternalCompilerError('No cases matched in freevars-ann-acc pred: ' + (pred as any).$name);
      }
      seenSoFar.set(name.key(), name);
      return freevarsAnnAcc(ann.ann, seenSoFar);
    }
    default:
      throw new InternalCompilerError('No cases matched in freevars-ann-acc: ' + (ann as any).$name);
  }
}

export function freevarsEAcc(expr: AExpr, seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  let acc = freevarsLAcc(expr.e, seenSoFar);
  const heads = expr.heads;
  for (let i = heads.length - 1; i >= 0; i--) {
    acc = freevarsHeadAcc(heads[i], acc);
  }
  return acc;
}

export function freevarsHeadAcc(node: AExprHead, acc: NameDict<A.Name>): NameDict<A.Name> {
  switch (node.$name) {
    case 'a-type-let': {
      const b = node.bind;
      switch (b.$name) {
        case 'a-type-bind': {
          acc.delete(b.name.key());
          return freevarsAnnAcc(b.ann, acc);
        }
        case 'a-newtype-bind': {
          acc.delete(b.name.key());
          acc.delete(b.namet.key());
          return acc;
        }
        default:
          throw new InternalCompilerError('No cases matched in freevars-e-acc: ' + (b as any).$name);
      }
    }
    case 'a-let':
    case 'a-arr-let':
    case 'a-var': {
      acc.delete(node.bind.id.key());
      return freevarsAnnAcc(node.bind.ann, freevarsLAcc(node.e, acc));
    }
    case 'a-seq':
      return freevarsLAcc(node.e1, acc);
    default:
      throw new InternalCompilerError('No cases matched in freevars-head-acc: ' + (node as any).$name);
  }
}

export function freevarsE(expr: AExpr): FrozenNameDict<A.Name> {
  return new Map(freevarsEAcc(expr, emptyDict<A.Name>()));
}

export function freevarsVariantAcc(v: AVariant, seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  let fromMembers: NameDict<A.Name>;
  switch (v.$name) {
    case 'a-variant': {
      let acc = seenSoFar;
      for (const m of v.members) {
        acc = freevarsAnnAcc(m.bind.ann, acc);
      }
      fromMembers = acc;
      break;
    }
    case 'a-singleton-variant': {
      fromMembers = seenSoFar;
      break;
    }
    default:
      throw new InternalCompilerError('No cases matched in freevars-variant-acc: ' + (v as any).$name);
  }
  let acc = fromMembers;
  for (const m of v.withMembers) {
    acc = freevarsVAcc(m.value, acc);
  }
  return acc;
}

export function freevarsBranchesAcc(branches: ACasesBranch[], seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  let acc = seenSoFar;
  for (const b of branches) {
    switch (b.$name) {
      case 'a-cases-branch': {
        const fromBody = freevarsEAcc(b.body, acc);
        const args = b.args.map((cb) => cb.bind);
        const withoutArgs = fromBody;
        for (const arg of args) {
          withoutArgs.delete(arg.id.key());
        }
        let innerAcc = withoutArgs;
        for (const arg of args) {
          innerAcc = freevarsAnnAcc(arg.ann, innerAcc);
        }
        acc = innerAcc;
        break;
      }
      case 'a-singleton-cases-branch': {
        acc = freevarsEAcc(b.body, acc);
        break;
      }
      default:
        throw new InternalCompilerError('No cases matched in freevars-branches-acc: ' + (b as any).$name);
    }
  }
  return acc;
}

export function freevarsLAcc(e: ALettable, seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  // NOTE: the Pyret source also has a (dead) branch for `a-array`, which
  // is not a variant of ALettable; it has no constructor and cannot occur.
  switch (e.$name) {
    case 'a-module':
      return freevarsVAcc(e.answer,
        freevarsVAcc(e.checks, seenSoFar));
    case 'a-cases':
      return freevarsAnnAcc(e.typ,
        freevarsVAcc(e.val,
          freevarsBranchesAcc(e.branches,
            freevarsEAcc(e._else, seenSoFar))));
    case 'a-if': {
      // Same dict-operation order as the nested chain: each arm's test
      // then body (the first arm's test heads live in the enclosing
      // AExpr), then the else, then the arms' test heads innermost-first.
      let acc = seenSoFar;
      for (const b of e.branches) {
        acc = freevarsEAcc(b.body, freevarsVAcc(b.test, acc));
      }
      acc = freevarsEAcc(e.elseBody, acc);
      for (let i = e.branches.length - 1; i >= 0; i--) {
        const hs = e.branches[i].heads;
        for (let j = hs.length - 1; j >= 0; j--) {
          acc = freevarsHeadAcc(hs[j], acc);
        }
      }
      return acc;
    }
    case 'a-assign': {
      seenSoFar.set(e.id.key(), e.id);
      return freevarsVAcc(e.value, seenSoFar);
    }
    case 'a-app': {
      const fromF = freevarsVAcc(e._fun, seenSoFar);
      let acc = fromF;
      for (const arg of e.args) {
        acc = freevarsVAcc(arg, acc);
      }
      return acc;
    }
    case 'a-method-app': {
      const fromObj = freevarsVAcc(e.obj, seenSoFar);
      let acc = fromObj;
      for (const arg of e.args) {
        acc = freevarsVAcc(arg, acc);
      }
      return acc;
    }
    case 'a-prim-app': {
      let acc = seenSoFar;
      for (const arg of e.args) {
        acc = freevarsVAcc(arg, acc);
      }
      return acc;
    }
    case 'a-lam': {
      const fromBody = freevarsEAcc(e.body, seenSoFar);
      const withoutArgs = fromBody;
      for (const arg of e.args) {
        withoutArgs.delete(arg.id.key());
      }
      let fromArgs = withoutArgs;
      for (const a of e.args) {
        fromArgs = freevarsAnnAcc(a.ann, fromArgs);
      }
      return freevarsAnnAcc(e.ret, fromArgs);
    }
    case 'a-method': {
      const fromBody = freevarsEAcc(e.body, seenSoFar);
      const withoutArgs = fromBody;
      for (const arg of e.args) {
        withoutArgs.delete(arg.id.key());
      }
      let fromArgs = withoutArgs;
      for (const a of e.args) {
        fromArgs = freevarsAnnAcc(a.ann, fromArgs);
      }
      return freevarsAnnAcc(e.ret, fromArgs);
    }
    case 'a-ref': {
      if (e.ann === undefined) {
        return seenSoFar;
      } else {
        return freevarsAnnAcc(e.ann, seenSoFar);
      }
    }
    case 'a-tuple': {
      let acc = seenSoFar;
      for (const f of e.fields) {
        acc = freevarsVAcc(f, acc);
      }
      return acc;
    }
    case 'a-tuple-get':
      return freevarsVAcc(e.tup, seenSoFar);
    case 'a-obj': {
      let acc = seenSoFar;
      for (const f of e.fields) {
        acc = freevarsVAcc(f.value, acc);
      }
      return acc;
    }
    case 'a-update': {
      const fromSupe = freevarsVAcc(e.supe, seenSoFar);
      let acc = fromSupe;
      for (const f of e.fields) {
        acc = freevarsVAcc(f.value, acc);
      }
      return acc;
    }
    case 'a-data-expr': {
      let fromVariants = seenSoFar;
      for (const v of e.variants) {
        fromVariants = freevarsVariantAcc(v, fromVariants);
      }
      let fromShared = fromVariants;
      for (const s of e.shared) {
        fromShared = freevarsVAcc(s.value, fromShared);
      }
      fromShared.set(e.namet.key(), e.namet);
      return fromShared;
    }
    case 'a-extend': {
      const fromSupe = freevarsVAcc(e.supe, seenSoFar);
      let acc = fromSupe;
      for (const f of e.fields) {
        acc = freevarsVAcc(f.value, acc);
      }
      return acc;
    }
    case 'a-dot': return freevarsVAcc(e.obj, seenSoFar);
    case 'a-colon': return freevarsVAcc(e.obj, seenSoFar);
    case 'a-get-bang': return freevarsVAcc(e.obj, seenSoFar);
    case 'a-id-var': {
      seenSoFar.set(e.id.key(), e.id);
      return seenSoFar;
    }
    case 'a-id-var-modref': {
      seenSoFar.set(e.id.key(), e.id);
      return seenSoFar;
    }
    case 'a-id-letrec': {
      seenSoFar.set(e.id.key(), e.id);
      return seenSoFar;
    }
    case 'a-val': return freevarsVAcc(e.v, seenSoFar);
    default:
      return raise('Non-lettable in freevars-l ' + (e as any).$name);
  }
}

export function freevarsL(e: ALettable): FrozenNameDict<A.Name> {
  return new Map(freevarsLAcc(e, emptyDict<A.Name>()));
}

export function freevarsVAcc(v: AVal, seenSoFar: NameDict<A.Name>): NameDict<A.Name> {
  // The Pyret source also handles a-id-var and a-id-letrec here (they are
  // ALettable variants, not AVal ones), so the scrutinee is widened.
  const vv = v as AVal | AIdVar | AIdLetrec;
  switch (vv.$name) {
    case 'a-id': {
      seenSoFar.set(vv.id.key(), vv.id);
      return seenSoFar;
    }
    case 'a-id-modref': {
      seenSoFar.set(vv.id.key(), vv.id);
      return seenSoFar;
    }
    case 'a-id-var': {
      seenSoFar.set(vv.id.key(), vv.id);
      return seenSoFar;
    }
    case 'a-id-letrec': {
      seenSoFar.set(vv.id.key(), vv.id);
      return seenSoFar;
    }
    case 'a-id-safe-letrec': {
      seenSoFar.set(vv.id.key(), vv.id);
      return seenSoFar;
    }
    case 'a-prim-val': return seenSoFar;
    case 'a-srcloc': return seenSoFar;
    case 'a-num': return seenSoFar;
    case 'a-str': return seenSoFar;
    case 'a-bool': return seenSoFar;
    case 'a-undefined': return seenSoFar;
    default:
      return raise('Unknown AVal in freevars-v ' + (vv as any).$name);
  }
}

export function freevarsV(v: AVal): FrozenNameDict<A.Name> {
  return new Map(freevarsVAcc(v, emptyDict<A.Name>()));
}

export function freevarsNameSpec(ns: A.NameSpec, acc: NameDict<A.Name>): void {
  switch (ns.$name) {
    case 's-local-ref': {
      acc.set(ns.name.key(), ns.name);
      break;
    }
    default:
      break;
  }
}

export function freevarsProvidesAcc(provideBlock: A.ProvideBlock, acc: NameDict<A.Name>): void {
  for (const spec of provideBlock.specs) {
    freevarsNameSpec(spec.nameSpec, acc);
    if (provideBlock.path.length > 0) {
      acc.set(provideBlock.path[0].key(), provideBlock.path[0]);
    }
  }
}

export function freevarsProg(p: AProg): FrozenNameDict<A.Name> {
  switch (p.$name) {
    case 'a-program': {
      const provideFreeVars = emptyDict<A.Name>();
      freevarsProvidesAcc(p.provides, provideFreeVars);
      const allVars = freevarsEAcc(p.body, provideFreeVars);
      return new Map(allVars);
    }
    default:
      throw new InternalCompilerError('No cases matched in freevars-prog: ' + (p as any).$name);
  }
}
