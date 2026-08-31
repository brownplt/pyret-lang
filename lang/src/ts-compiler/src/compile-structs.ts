/*
  TS port of src/arr/compiler/compile-structs.arr (lines 1-645 and 2918-end).

  The `data CompileError` portion (lines 646-2917) lives in
  ./compile-errors.ts and is re-exported at the bottom of this module so
  consumers see a single module, mirroring the original Pyret file.
  compile-errors.ts imports shared types from this module; the cycle is
  benign because those imports are only used inside method bodies, and the
  `export *` re-export at the bottom of this file runs after all
  module-level initialization here is complete.
*/

import * as P from 'path';
import * as A from './ast';
import * as SL from './srcloc';
import * as ED from './error-display';
import * as T from './type-structs';
import * as J from './js-ast';
import * as CL from './concat-lists';
import { InternalCompilerError, mapGetValue, toRepr } from './shared';
import type { CompileError } from './compile-errors';

export type URI = string;
export type Loc = SL.Loc;

// ---------- Local type-construction shorthands (compile-structs.arr lines 16-35) ----------
// All of these fix the location to A.dummyLoc, as in the Pyret original.

export const tNothing = T.tNothing(A.dummyLoc);
export const tStr = T.tString(A.dummyLoc);
export const tBoolean = T.tBoolean(A.dummyLoc);
export const tNumber = T.tNumber(A.dummyLoc);
export function tArrow(args: T.Type[], ret: T.Type): T.Type {
  return new T.TArrow(args, ret, A.dummyLoc, false);
}
export const tTop: T.Type = new T.TTop(A.dummyLoc, false);
export const tBot: T.Type = new T.TBot(A.dummyLoc, false);
export function tRecord(fields: Map<string, T.Type>): T.Type {
  return new T.TRecord(fields, A.dummyLoc, false);
}
export function tForall(introduces: T.Type[], onto: T.Type): T.Type {
  return new T.TForall(introduces, onto, A.dummyLoc, false);
}
export function tVar(id: A.Name): T.Type {
  return new T.TVar(id, A.dummyLoc, false);
}
export function tArray(v: T.Type): T.Type {
  return T.tArray(v, A.dummyLoc);
}
export const tString = T.tString(A.dummyLoc);
export function tOption(v: T.Type): T.Type {
  return T.tOption(v, A.dummyLoc);
}
export function tData(
  name: string,
  params: T.Type[],
  variants: T.TypeVariant[],
  fields: Map<string, T.Type>
): T.DataType {
  return new T.TData(name, params, variants, fields, A.dummyLoc);
}
export function tVariant(
  name: string,
  fields: Array<[string, T.Type]>,
  withFields: Map<string, T.Type>
): T.TypeVariant {
  return new T.TVariant(name, fields, withFields, A.dummyLoc);
}
export function tSingletonVariant(
  name: string,
  withFields: Map<string, T.Type>
): T.TypeVariant {
  return new T.TSingletonVariant(name, withFields, A.dummyLoc);
}
export function tApp(onto: T.Type, args: T.Type[]): T.Type {
  return new T.TApp(onto, args, A.dummyLoc, false);
}
export function tName(moduleName: T.NameOrigin, id: A.Name): T.Type {
  return new T.TName(moduleName, id, A.dummyLoc, false);
}

export const isTApp = T.isTApp;
export const isSBlock = A.isSBlock;

// ---------- data Dependency ----------
// NOTE: the union type cannot be named `Dependency` (that is the class for
// the `dependency` variant), so the union is exported as `AnyDependency`.

export abstract class DependencyBase {
  abstract get $name(): string;
  abstract key(): string;
}

export class Dependency extends DependencyBase {
  get $name(): 'dependency' { return 'dependency'; }
  readonly arguments: string[];
  constructor(public protocol: string, args: string[]) {
    super();
    this.arguments = args;
  }
  key(): string {
    return this.protocol + "(" + this.arguments.join(", ") + ")";
  }
}

export class Builtin extends DependencyBase {
  get $name(): 'builtin' { return 'builtin'; }
  constructor(public modname: string) { super(); }
  key(): string {
    return "builtin(" + this.modname + ")";
  }
}

export type AnyDependency = Dependency | Builtin;
export function isDependency(x: any): x is Dependency { return x instanceof Dependency; }
export function isBuiltin(x: any): x is Builtin { return x instanceof Builtin; }

// ---------- data NativeModule ----------

export class Requirejs {
  get $name(): 'requirejs' { return 'requirejs'; }
  constructor(public path: string) {}
}
export type NativeModule = Requirejs;
export function isRequirejs(x: any): x is Requirejs { return x instanceof Requirejs; }

// ---------- data BindOrigin ----------

export class BindOrigin {
  get $name(): 'bind-origin' { return 'bind-origin'; }
  constructor(
    public localBindSite: Loc,
    public definitionBindSite: Loc,
    public newDefinition: boolean,
    public uriOfDefinition: URI,
    public originalName: A.Name
  ) {}
}
export function isBindOrigin(x: any): x is BindOrigin { return x instanceof BindOrigin; }

export function boLocal(loc: Loc, originalName: A.Name): BindOrigin {
  if (loc instanceof SL.Builtin) {
    return new BindOrigin(loc, loc, true, loc.moduleName, originalName);
  } else {
    return new BindOrigin(loc, loc, true, loc.source, originalName);
  }
}

// NOTE(joe): If source information ends up in provides, we can add an extra arg
// here to provide better definition site info for names from other modules
export function boModule(localLoc: Loc, defLoc: Loc, defUri: URI, originalName: A.Name): BindOrigin {
  return new BindOrigin(localLoc, defLoc, false, defUri, originalName);
}

export function boGlobal(optOrigin: BindOrigin | undefined, uri: URI, originalName: A.Name): BindOrigin {
  if (optOrigin === undefined) {
    return new BindOrigin(A.dummyLoc, new SL.Builtin(uri), false, uri, originalName);
  } else {
    return new BindOrigin(optOrigin.localBindSite, optOrigin.definitionBindSite, false, uri, originalName);
  }
}

// ---------- data ValueBinder ----------

export class VbLetrec {
  get $name(): 'vb-letrec' { return 'vb-letrec'; }
}
export class VbLet {
  get $name(): 'vb-let' { return 'vb-let'; }
}
export class VbVar {
  get $name(): 'vb-var' { return 'vb-var'; }
}
export type ValueBinder = VbLetrec | VbLet | VbVar;
export const vbLetrec = new VbLetrec();
export const vbLet = new VbLet();
export const vbVar = new VbVar();
export function isVbLetrec(x: any): x is VbLetrec { return x instanceof VbLetrec; }
export function isVbLet(x: any): x is VbLet { return x instanceof VbLet; }
export function isVbVar(x: any): x is VbVar { return x instanceof VbVar; }

// ---------- data ValueBind ----------

export class ValueBind {
  get $name(): 'value-bind' { return 'value-bind'; }
  constructor(
    public origin: BindOrigin,
    public binder: ValueBinder,
    public atom: A.Name,
    public ann: A.Ann
  ) {}
}
export function isValueBind(x: any): x is ValueBind { return x instanceof ValueBind; }

// ---------- data TypeBinder ----------

export class TbTypeLet {
  get $name(): 'tb-type-let' { return 'tb-type-let'; }
}
export class TbTypeVar {
  get $name(): 'tb-type-var' { return 'tb-type-var'; }
}
export type TypeBinder = TbTypeLet | TbTypeVar;
export const tbTypeLet = new TbTypeLet();
export const tbTypeVar = new TbTypeVar();
export function isTbTypeLet(x: any): x is TbTypeLet { return x instanceof TbTypeLet; }
export function isTbTypeVar(x: any): x is TbTypeVar { return x instanceof TbTypeVar; }

// ---------- data TypeBindTyp ----------

export class TbTyp {
  get $name(): 'tb-typ' { return 'tb-typ'; }
  constructor(public typ: T.Type) {}
}
export class TbNone {
  get $name(): 'tb-none' { return 'tb-none'; }
}
export type TypeBindTyp = TbTyp | TbNone;
export const tbNone = new TbNone();
export function isTbTyp(x: any): x is TbTyp { return x instanceof TbTyp; }
export function isTbNone(x: any): x is TbNone { return x instanceof TbNone; }

// ---------- data TypeBind ----------

export class TypeBind {
  get $name(): 'type-bind' { return 'type-bind'; }
  constructor(
    public origin: BindOrigin,
    public binder: TypeBinder,
    public atom: A.Name,
    public typ: TypeBindTyp
  ) {}
}
export function isTypeBind(x: any): x is TypeBind { return x instanceof TypeBind; }

// ---------- data ModuleBind ----------

export class ModuleBind {
  get $name(): 'module-bind' { return 'module-bind'; }
  constructor(
    public origin: BindOrigin,
    public atom: A.Name,
    public uri: URI
  ) {}
}
export function isModuleBind(x: any): x is ModuleBind { return x instanceof ModuleBind; }

// ---------- data ScopeResolution ----------

export class ResolvedScope {
  get $name(): 'resolved-scope' { return 'resolved-scope'; }
  constructor(public ast: A.Program, public errors: CompileError[]) {}
}
export type ScopeResolution = ResolvedScope;
export function isResolvedScope(x: any): x is ResolvedScope { return x instanceof ResolvedScope; }

// ---------- data ComputedEnvironment ----------

export class ComputedNone {
  get $name(): 'computed-none' { return 'computed-none'; }
}
export class ComputedEnv {
  get $name(): 'computed-env' { return 'computed-env'; }
  constructor(
    public moduleBindings: Map<string, ModuleBind>,
    public bindings: Map<string, ValueBind>,
    public typeBindings: Map<string, TypeBind>,
    public datatypes: Map<string, A.Expr>,
    public moduleEnv: Map<string, ModuleBind>,
    public env: Map<string, ValueBind>,
    public typeEnv: Map<string, TypeBind>
  ) {}
}
export type ComputedEnvironment = ComputedNone | ComputedEnv;
export const computedNone = new ComputedNone();
export function isComputedNone(x: any): x is ComputedNone { return x instanceof ComputedNone; }
export function isComputedEnv(x: any): x is ComputedEnv { return x instanceof ComputedEnv; }

// ---------- data NameResolution ----------

export class ResolvedNames {
  get $name(): 'resolved-names' { return 'resolved-names'; }
  constructor(
    public ast: A.Program,
    public errors: CompileError[],
    public env: ComputedEnvironment
  ) {}
}
export type NameResolution = ResolvedNames;
export function isResolvedNames(x: any): x is ResolvedNames { return x instanceof ResolvedNames; }

// ---------- data ExtraImports ----------
// Used to describe when additional module imports should be added to a
// program.  See wrap-extra-imports

export class ExtraImports {
  get $name(): 'extra-imports' { return 'extra-imports'; }
  constructor(public imports: ExtraImport[]) {}
}
export function isExtraImports(x: any): x is ExtraImports { return x instanceof ExtraImports; }

// ---------- data ExtraImport ----------
// Import this module, and bind the given value and type bindings from it

export class ExtraImport {
  get $name(): 'extra-import' { return 'extra-import'; }
  constructor(
    public dependency: AnyDependency,
    public asName: string,
    public values: string[],
    public types: string[]
  ) {}
}
export function isExtraImport(x: any): x is ExtraImport { return x instanceof ExtraImport; }

// ---------- data Loadable ----------

export class ModuleAsString {
  get $name(): 'module-as-string' { return 'module-as-string'; }
  // NOTE(joe): there's a circular dependency between this module and
  // js-of-pyret.arr; hence the `any` in resultPrinter
  constructor(
    public provides: Provides,
    public compileEnv: CompileEnvironment,
    public postCompileEnv: ComputedEnvironment,
    public resultPrinter: CompileResult<any>
  ) {}
}
export type Loadable = ModuleAsString;
export function isModuleAsString(x: any): x is ModuleAsString { return x instanceof ModuleAsString; }

// ---------- data CompileEnvironment ----------

export abstract class CompileEnvironmentBase {
  abstract get $name(): string;
  abstract globals: Globals;
  abstract allModules: Map<string, Loadable>; // MutableStringDict in Pyret
  abstract myModules: Map<string, URI>; // persistent StringDict in Pyret; never mutated here

  valueByUri(uri: string, name: string): ValueExport | undefined {
    const ve = mapGetValue(this.allModules, uri).provides.values.get(name);
    if (ve === undefined) {
      return undefined;
    }
    if (isVAlias(ve)) {
      if (uri === ve.origin.uriOfDefinition) {
        throw new InternalCompilerError("Self-referential alias for " + ve.originalName + " in module " + uri);
      }
      return this.valueByUri(ve.origin.uriOfDefinition, ve.originalName);
    }
    return ve;
  }

  valueByUriValue(uri: string, name: string): ValueExport {
    const v = this.valueByUri(uri, name);
    if (v === undefined) {
      throw new InternalCompilerError("Could not find value " + name + " on module " + uri);
    }
    return v;
  }

  datatypeByUri(uri: string, name: string): DataExport | undefined {
    const de = mapGetValue(this.allModules, uri).provides.dataDefinitions.get(name);
    if (de === undefined) {
      return undefined;
    }
    if (isDAlias(de)) {
      if (uri === de.origin.uriOfDefinition) {
        throw new InternalCompilerError("Self-referential alias for " + de.name + " in module " + uri);
      }
      return this.datatypeByUri(de.origin.uriOfDefinition, de.name);
    }
    return de;
  }

  datatypeByUriValue(uri: string, name: string): DataExport {
    const v = this.datatypeByUri(uri, name);
    if (v === undefined) {
      throw new InternalCompilerError("Could not find datatype " + name + " on module " + uri);
    }
    return v;
  }

  resolveDatatypeByUri(uri: string, name: string): T.DataType | undefined {
    const dt = this.datatypeByUri(uri, name);
    if (dt === undefined) {
      return undefined;
    }
    if (isDType(dt)) {
      return dt.typ;
    }
    throw new InternalCompilerError("resolve-datatype-by-uri got a d-alias: " + toRepr(dt));
  }

  resolveDatatypeByUriValue(uri: string, name: string): T.DataType {
    const v = this.resolveDatatypeByUri(uri, name);
    if (v === undefined) {
      throw new InternalCompilerError("Could not find datatype " + name + " on module " + uri);
    }
    return v;
  }

  valueByOrigin(origin: BindOrigin): ValueExport | undefined {
    return this.valueByUri(origin.uriOfDefinition, origin.originalName.toname());
  }

  valueByOriginValue(origin: BindOrigin): ValueExport {
    return this.valueByUriValue(origin.uriOfDefinition, origin.originalName.toname());
  }

  typeByUri(uri: string, name: string): T.Type | undefined {
    const providesOfAliased = mapGetValue(this.allModules, uri).provides;
    const remoteDatatype = providesOfAliased.dataDefinitions.get(name);
    if (remoteDatatype !== undefined) {
      let de: DataExport;
      if (isDAlias(remoteDatatype)) {
        const found = this.datatypeByUri(remoteDatatype.origin.uriOfDefinition, remoteDatatype.name);
        if (found === undefined) {
          throw new InternalCompilerError("A datatype alias in an export was not found: " + toRepr(remoteDatatype));
        }
        de = found;
      } else {
        de = remoteDatatype;
      }
      return new T.TName(
        new T.ModuleUri(de.origin.uriOfDefinition),
        new A.STypeGlobal((de as DType).typ.name),
        de.origin.localBindSite,
        false
      );
    } else {
      const typ = providesOfAliased.aliases.get(name);
      if (typ === undefined) {
        return undefined;
      }
      if (typ instanceof T.TName) {
        const aMod = typ.moduleName;
        if (aMod instanceof T.ModuleUri) {
          return this.typeByUri(aMod.uri, typ.id.toname());
        } else {
          throw new InternalCompilerError("A provided type alias referred to an unresolved module: " + toRepr(typ));
        }
      } else {
        return typ;
      }
    }
  }

  typeByUriValue(uri: string, name: string): T.Type {
    const v = this.typeByUri(uri, name);
    if (v === undefined) {
      throw new InternalCompilerError("Could not find type " + name + " on module " + uri);
    }
    return v;
  }

  typeByOrigin(origin: BindOrigin): T.Type | undefined {
    return this.typeByUri(origin.uriOfDefinition, origin.originalName.toname());
  }

  typeByOriginValue(origin: BindOrigin): T.Type {
    return this.typeByUriValue(origin.uriOfDefinition, origin.originalName.toname());
  }

  globalValue(name: string): ValueExport | undefined {
    const origin = this.globals.values.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return this.valueByOrigin(origin);
  }

  globalValueValue(name: string): ValueExport {
    const v = this.globalValue(name);
    if (v === undefined) {
      throw new InternalCompilerError("Could not find value " + name + " as a global");
    }
    return v;
  }

  globalType(name: string): T.Type | undefined {
    const origin = this.globals.types.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return this.typeByOrigin(origin);
  }

  uriByDepKey(depKey: string): URI {
    return mapGetValue(this.myModules, depKey);
  }

  providesByUri(uri: string): Provides | undefined {
    const mod = this.allModules.get(uri);
    if (mod === undefined) {
      return undefined;
    }
    return mod.provides;
  }

  providesByUriValue(uri: string): Provides {
    const provides = this.providesByUri(uri);
    if (provides === undefined) {
      throw new InternalCompilerError("Could not find module with uri: " + uri);
    }
    return provides;
  }

  providesByOrigin(origin: BindOrigin): Provides | undefined {
    return this.providesByUri(origin.uriOfDefinition);
  }

  providesByOriginValue(origin: BindOrigin): Provides {
    return this.providesByUriValue(origin.uriOfDefinition);
  }

  providesByDepKey(depKey: string): Provides | undefined {
    const uri = this.myModules.get(depKey);
    if (uri === undefined) {
      return undefined;
    }
    return mapGetValue(this.allModules, uri).provides;
  }

  providesByDepKeyValue(depKey: string): Provides {
    const provides = this.providesByDepKey(depKey);
    if (provides === undefined) {
      throw new InternalCompilerError("Could not find dep key: " + depKey);
    }
    return provides;
  }

  providesByValueName(name: string): Provides | undefined {
    const origin = this.globals.values.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return this.providesByOriginValue(origin);
  }

  providesByValueNameValue(name: string): Provides {
    const provides = this.providesByValueName(name);
    if (provides === undefined) {
      throw new InternalCompilerError("Could not find value " + name);
    }
    return provides;
  }

  providesByTypeName(name: string): Provides | undefined {
    const origin = this.globals.types.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return this.providesByOrigin(origin);
  }

  providesByTypeNameValue(name: string): Provides {
    const provides = this.providesByTypeName(name);
    if (provides === undefined) {
      throw new InternalCompilerError("Could not find type " + name);
    }
    return provides;
  }

  providesByModuleName(name: string): Provides | undefined {
    const origin = this.globals.modules.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return this.providesByOrigin(origin);
  }

  providesByModuleNameValue(name: string): Provides {
    const provides = this.providesByModuleName(name);
    if (provides === undefined) {
      throw new InternalCompilerError("Could not find module " + name);
    }
    return provides;
  }

  valueByDepKey(depKey: string, name: string): ValueExport | undefined {
    const uri = mapGetValue(this.myModules, depKey);
    return this.valueByUri(uri, name);
  }

  valueByDepKeyValue(depKey: string, name: string): ValueExport {
    const v = this.valueByDepKey(depKey, name);
    if (v === undefined) {
      throw new InternalCompilerError("Could not find " + name + " on " + depKey);
    }
    return v;
  }

  typeByDepKey(depKey: string, name: string): T.Type | undefined {
    const uri = mapGetValue(this.myModules, depKey);
    return this.typeByUri(uri, name);
  }

  originByModuleName(name: string): BindOrigin | undefined {
    return this.globals.modules.get(name);
  }

  originByValueName(name: string): BindOrigin | undefined {
    return this.globals.values.get(name);
  }

  originByTypeName(name: string): BindOrigin | undefined {
    return this.globals.types.get(name);
  }

  uriByModuleName(name: string): URI | undefined {
    const origin = this.globals.modules.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return origin.uriOfDefinition;
  }

  uriByValueName(name: string): URI | undefined {
    const origin = this.globals.values.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return origin.uriOfDefinition;
  }

  uriByTypeName(name: string): URI | undefined {
    const origin = this.globals.types.get(name);
    if (origin === undefined) {
      return undefined;
    }
    return origin.uriOfDefinition;
  }
}

export class CompileEnv extends CompileEnvironmentBase {
  get $name(): 'compile-env' { return 'compile-env'; }
  constructor(
    public globals: Globals,
    public allModules: Map<string, Loadable>,
    public myModules: Map<string, URI>
  ) {
    super();
  }
}
export type CompileEnvironment = CompileEnv;
export function isCompileEnv(x: any): x is CompileEnv { return x instanceof CompileEnv; }

// ---------- data Globals ----------
// Globals maps from names to BindOrigins so we know the most recent binding and
// original binding for each

export class Globals {
  get $name(): 'globals' { return 'globals'; }
  constructor(
    public modules: Map<string, BindOrigin>,
    public values: Map<string, BindOrigin>,
    public types: Map<string, BindOrigin>
  ) {}
}
export function isGlobals(x: any): x is Globals { return x instanceof Globals; }

// ---------- data ValueExport ----------

export abstract class ValueExportBase {
  abstract get $name(): string;
}
export class VAlias extends ValueExportBase {
  get $name(): 'v-alias' { return 'v-alias'; }
  constructor(public origin: BindOrigin, public originalName: string) { super(); }
}
export class VJustType extends ValueExportBase {
  get $name(): 'v-just-type' { return 'v-just-type'; }
  constructor(public origin: BindOrigin, public t: T.Type) { super(); }
}
export class VVar extends ValueExportBase {
  get $name(): 'v-var' { return 'v-var'; }
  constructor(public origin: BindOrigin, public t: T.Type) { super(); }
}
export class VFun extends ValueExportBase {
  get $name(): 'v-fun' { return 'v-fun'; }
  constructor(
    public origin: BindOrigin,
    public t: T.Type,
    public name: string,
    public flatness: number | undefined
  ) { super(); }
}
export type ValueExport = VAlias | VJustType | VVar | VFun;
export function isVAlias(x: any): x is VAlias { return x instanceof VAlias; }
export function isVJustType(x: any): x is VJustType { return x instanceof VJustType; }
export function isVVar(x: any): x is VVar { return x instanceof VVar; }
export function isVFun(x: any): x is VFun { return x instanceof VFun; }

// ---------- data DataExport ----------

export abstract class DataExportBase {
  abstract get $name(): string;
}
export class DAlias extends DataExportBase {
  get $name(): 'd-alias' { return 'd-alias'; }
  constructor(public origin: BindOrigin, public name: string) { super(); }
}
export class DType extends DataExportBase {
  get $name(): 'd-type' { return 'd-type'; }
  constructor(public origin: BindOrigin, public typ: T.DataType) { super(); }
}
export type DataExport = DAlias | DType;
export function isDAlias(x: any): x is DAlias { return x instanceof DAlias; }
export function isDType(x: any): x is DType { return x instanceof DType; }

// ---------- data Provides ----------

export class Provides {
  get $name(): 'provides' { return 'provides'; }
  constructor(
    public fromUri: URI,
    public modules: Map<string, URI>,
    public values: Map<string, ValueExport>,
    public aliases: Map<string, T.Type>,
    public dataDefinitions: Map<string, DataExport>
  ) {}
}
export function isProvides(x: any): x is Provides { return x instanceof Provides; }

// ---------- raw (de)serialization ----------
// "raw" values are plain JS objects/arrays read from compiled module files
// (see js/trove/builtin-modules.js and the provides serialization in
// anf-loop-compiler.arr). Keys with dashes are accessed with brackets.

export function makeDep(rawDep: any): AnyDependency {
  if (rawDep["import-type"] === "builtin") {
    return new Builtin(rawDep.name);
  } else {
    return new Dependency(rawDep.protocol, [...rawDep.args]);
  }
}

export function valueExportFromRaw(uri: string, valExport: any, tyvarEnv: Map<string, A.Name>): ValueExport {
  const t = valExport.tag;
  const typ = typeFromRaw(uri, valExport.typ, tyvarEnv);
  if (t === "v-fun") {
    // NOTE: this mirrors the Pyret original, which applies v-fun to the wrong
    // arguments (v-fun(typ, t, none)); the function appears to be unused.
    return new VFun(typ as any, t as any, undefined as any, undefined);
  } else {
    // Mirrors v-just-type(typ), also applied to the wrong arguments.
    return new VJustType(typ as any, undefined as any);
  }
}

export function typeFromRaw(uri: string, typ: any, tyvarEnv: Map<string, A.Name>): T.Type {
  const tfr = (t: any) => typeFromRaw(uri, t, tyvarEnv);
  // TODO(joe): Make this do something intelligent when location information
  // is available
  const l = new SL.Builtin(uri);
  const t = typ.tag;
  if (t === "any") {
    return new T.TTop(l, false);
  } else if (t === "bot") {
    return new T.TBot(l, false);
  } else if (t === "record") {
    const fields = new Map<string, T.Type>();
    for (const f of typ.fields) {
      fields.set(f.name, tfr(f.value));
    }
    return new T.TRecord(fields, l, false);
  } else if (t === "data-refinement") {
    return new T.TDataRefinement(tfr(typ.basetype), typ.variant, l, false);
  } else if (t === "tuple") {
    return new T.TTuple(typ.elts.map((e: any) => tfr(e)), l, false);
  } else if (t === "name") {
    if (typ.origin["import-type"] === "$ELF") {
      return new T.TName(new T.ModuleUri(uri), new A.STypeGlobal(typ.name), l, false);
    } else if (typ.origin["import-type"] === "uri") {
      return new T.TName(new T.ModuleUri(typ.origin.uri), new A.STypeGlobal(typ.name), l, false);
    } else {
      // NOTE: type-structs' dependency variant is annotated as holding the
      // dependency in key form, but the Pyret original passes the Dependency
      // value itself; mirrored here with a cast.
      return new T.TName(new T.Dependency(makeDep(typ.origin) as any), new A.STypeGlobal(typ.name), l, false);
    }
  } else if (t === "tyvar") {
    const tv = tyvarEnv.get(typ.name);
    if (tv === undefined) {
      throw new InternalCompilerError("Unbound type variable " + typ.name + " in provided type when processing " + uri);
    }
    return new T.TVar(tv, l, false);
  } else if (t === "forall") {
    const newEnv = new Map(tyvarEnv);
    for (const a of typ.args) {
      const tvn = A.globalNames.makeAtom(a);
      newEnv.set(a, tvn);
    }
    const params: T.Type[] = [];
    for (const k of newEnv.keys()) {
      params.push(new T.TVar(newEnv.get(k)!, l, false));
    }
    return new T.TForall(params, typeFromRaw(uri, typ.onto, newEnv), l, false);
  } else if (t === "tyapp") {
    return new T.TApp(tfr(typ.onto), typ.args.map((a: any) => tfr(a)), l, false);
  } else if (t === "arrow") {
    return new T.TArrow(typ.args.map((a: any) => tfr(a)), tfr(typ.ret), l, false);
  } else {
    throw new InternalCompilerError("Unknown raw tag for type: " + t);
  }
}

export function tvariantFromRaw(uri: string, tvariant: any, env: Map<string, A.Name>): T.TypeVariant {
  const t = tvariant.tag;
  if (t === "variant") {
    // foldr in the original: members are built (and types deserialized)
    // right-to-left, preserving gensym order for any contained foralls.
    const members: Array<[string, T.Type]> = [];
    for (let i = tvariant.vmembers.length - 1; i >= 0; i--) {
      const tm = tvariant.vmembers[i];
      members.unshift([tm.name, typeFromRaw(uri, tm.typ, env)]);
    }
    const withMembers = new Map<string, T.Type>();
    for (const wm of tvariant.withmembers) {
      withMembers.set(wm.name, typeFromRaw(uri, wm.value, env));
    }
    return tVariant(tvariant.name, members, withMembers);
  } else if (t === "singleton-variant") {
    const withMembers = new Map<string, T.Type>();
    for (const wm of tvariant.withmembers) {
      withMembers.set(wm.name, typeFromRaw(uri, wm.value, env));
    }
    return tSingletonVariant(tvariant.name, withMembers);
  } else {
    throw new InternalCompilerError("Unkonwn raw tag for variant: " + t);
  }
}

export function datatypeFromRaw(uri: string, datatyp: any): DataExport {
  const l = new SL.Builtin(uri);

  if (datatyp.tag === "data-alias") {
    const origin = originFromRaw(uri, datatyp.origin, datatyp.name);
    return new DAlias(origin, datatyp.name);
  } else if (datatyp.tag === "data") {
    const pdict = new Map<string, A.Name>();
    for (const a of datatyp.params) {
      const tvn = A.globalNames.makeAtom(a);
      pdict.set(a, tvn);
    }
    const params: T.Type[] = [];
    for (const k of pdict.keys()) {
      params.push(new T.TVar(pdict.get(k)!, l, false));
    }
    const variants = datatyp.variants.map((v: any) => tvariantFromRaw(uri, v, pdict));
    const members = new Map<string, T.Type>();
    for (const tm of datatyp.methods) {
      members.set(tm.name, typeFromRaw(uri, tm.value, pdict));
    }
    const origin = originFromRaw(uri, datatyp.origin, datatyp.name);
    return new DType(origin, tData(datatyp.name, params, variants, members));
  } else {
    throw new InternalCompilerError("Unknown format for data export in " + uri + ": " + toRepr(datatyp));
  }
}

export function srclocFromRaw(raw: any): Loc {
  if (raw.length === 1) {
    return new SL.Builtin(raw[0]);
  } else {
    return new SL.Srcloc(raw[0], raw[1], raw[2], raw[3], raw[4], raw[5], raw[6]);
  }
}

export function originFromRaw(uri: string, raw: any, name: string): BindOrigin {
  if (raw.provided) {
    return new BindOrigin(
      srclocFromRaw(raw["local-bind-site"]),
      srclocFromRaw(raw["definition-bind-site"]),
      raw["new-definition"],
      raw["uri-of-definition"],
      new A.SName(srclocFromRaw(raw["definition-bind-site"]), name)
    );
  } else {
    return new BindOrigin(new SL.Builtin(uri), new SL.Builtin(uri), false, uri, new A.SName(A.dummyLoc, name));
  }
}

export function providesFromRawProvides(uri: string, raw: any): Provides {
  const mdict = new Map<string, URI>();
  for (const v of raw.modules) {
    mdict.set(v.name, v.uri);
  }
  const vdict = new Map<string, ValueExport>();
  for (const v of raw.values) {
    if (typeof v === 'string') {
      vdict.set(v, new VJustType(originFromRaw(uri, { provided: false }, v), tTop));
    } else {
      if (v.value.bind === "alias") {
        const origin = originFromRaw(uri, v.value.origin, v.value["original-name"]);
        vdict.set(v.name, new VAlias(origin, v.value["original-name"]));
      } else if (v.value.bind === "var") {
        const origin = originFromRaw(uri, v.value.origin, v.name);
        vdict.set(v.name, new VVar(origin, typeFromRaw(uri, v.value.typ, new Map())));
      } else if (v.value.bind === "fun") {
        const origin = originFromRaw(uri, v.value.origin, v.name);
        const flatness = typeof v.value.flatness === 'number' ? v.value.flatness : undefined;
        vdict.set(v.name, new VFun(origin, typeFromRaw(uri, v.value.typ, new Map()), v.value.name, flatness));
      } else {
        const origin = originFromRaw(uri, v.value.origin, v.name);
        vdict.set(v.name, new VJustType(origin, typeFromRaw(uri, v.value.typ, new Map())));
      }
    }
  }
  const adict = new Map<string, T.Type>();
  for (const a of raw.aliases) {
    if (typeof a === 'string') {
      adict.set(a, tTop);
    } else {
      adict.set(a.name, typeFromRaw(uri, a.typ, new Map()));
    }
  }
  const ddict = new Map<string, DataExport>();
  for (const d of raw.datatypes) {
    ddict.set(d.name, datatypeFromRaw(uri, d.typ));
  }
  return new Provides(uri, mdict, vdict, adict, ddict);
}

export function providesToRawProvidesAst(provs: Provides, env: any): J.JExprT {
  // MARK(joe/ben): modules
  // The bulk of this function is commented out in the Pyret original; it
  // currently always produces an empty object literal.
  if (!isProvides(provs)) {
    throw new InternalCompilerError("providesToRawProvidesAst: not a Provides: " + toRepr(provs));
  }
  return new J.JObj(CL.clist());
}

// ---------- data CompileResult ----------

export class Ok<C> {
  get $name(): 'ok' { return 'ok'; }
  constructor(public code: C) {}
}
export class Err {
  get $name(): 'err' { return 'err'; }
  constructor(public problems: CompileError[]) {}
}
export type CompileResult<C> = Ok<C> | Err;
export function ok<C>(code: C): Ok<C> { return new Ok(code); }
export function err(problems: CompileError[]): Err { return new Err(problems); }
export function isOk(x: any): x is Ok<any> { return x instanceof Ok; }
export function isErr(x: any): x is Err { return x instanceof Err; }

export function drawAndHighlight(l: Loc): ED.ErrorDisplay {
  return new ED.LocDisplay(l, "error-highlight", new ED.Loc(l));
}

// ---------- data UrlFileMode (compile-structs.arr line 2918 onward) ----------

export class AllLocal {
  get $name(): 'all-local' { return 'all-local'; }
}
export class AllRemote {
  get $name(): 'all-remote' { return 'all-remote'; }
}
export class LocalIfPresent {
  get $name(): 'local-if-present' { return 'local-if-present'; }
}
export type UrlFileMode = AllLocal | AllRemote | LocalIfPresent;
export const allLocal = new AllLocal();
export const allRemote = new AllRemote();
export const localIfPresent = new LocalIfPresent();
export function isAllLocal(x: any): x is AllLocal { return x instanceof AllLocal; }
export function isAllRemote(x: any): x is AllRemote { return x instanceof AllRemote; }
export function isLocalIfPresent(x: any): x is LocalIfPresent { return x instanceof LocalIfPresent; }

// ---------- CompileOptions ----------
// Pyret models options as an anonymous record extended functionally at call
// sites (`options.{field: v}`). In TS, CompileOptions is a plain interface
// and functional extension becomes object spread:
//   options.{ checks: "none" }  ==>  { ...options, checks: "none" }
// Pyret method fields (should-profile, on-compile, before-compile) become
// plain function-typed fields; the unused `self` argument is dropped.

export interface CompileOptions {
  addProfiling: boolean;
  baseDir: string;
  thisPyretDir: string;
  checkMode: boolean;
  checkAll: boolean;
  checks: string;
  checksFormat: string;
  typeCheck: boolean;
  enableSpies: boolean;
  allowShadowed: boolean;
  collectAll: boolean;
  collectTimes: boolean;
  ignoreUnbound: boolean;
  properTailCalls: boolean;
  inlineCaseBodyLimit: number;
  moduleEval: boolean;
  userAnnotations: boolean;
  runtimeAnnotations: boolean;
  compiledCache: string;
  compiledReadOnly: string[];
  displayProgress: boolean;
  shouldProfile: (locator: any) => boolean;
  // log(s, toClear): if toClear is a number, the previous toClear characters
  // on the current line are blanked out first (progress display).
  log: (s: string, toClear?: number) => void;
  logError: (s: string) => void;
  onCompile: (locator: any, loadable: Loadable, trace: any) => Loadable;
  beforeCompile: (locator: any) => void;
  htmlFile: string | undefined;
  depsFile: string;
  standaloneFile: string;
  urlFileMode: UrlFileMode;
  pauseSchedule: string | undefined;
  // Set by some front ends (CLI/webworker); not part of the defaults.
  pipeline?: string;
  compileModule?: boolean;
}

export const defaultCompileOptions: CompileOptions = {
  addProfiling: false,
  baseDir: ".",
  thisPyretDir: ".",
  checkMode: true,
  checkAll: true,
  checks: "all",
  checksFormat: "text",
  typeCheck: false,
  enableSpies: true,
  allowShadowed: false,
  collectAll: false,
  collectTimes: false,
  ignoreUnbound: false,
  properTailCalls: true,
  inlineCaseBodyLimit: 5,
  moduleEval: true,
  userAnnotations: true,
  runtimeAnnotations: true,
  compiledCache: "compiled",
  compiledReadOnly: [],
  displayProgress: true,
  shouldProfile: (_locator: any) => false,
  // Pyret's `print` writes the string with no trailing newline.
  log: (s: string, toClear?: number) => {
    if (toClear === undefined) {
      process.stdout.write(s);
    } else {
      process.stdout.write("\r");
      process.stdout.write(" ".repeat(toClear));
      process.stdout.write("\r");
      process.stdout.write(s);
    }
  },
  logError: (s: string) => {
    process.stderr.write(s);
  },
  onCompile: (_locator: any, loadable: Loadable, _trace: any) => loadable,
  beforeCompile: (_locator: any) => { return; },
  htmlFile: undefined,
  depsFile: "build/bundled-node-deps.js",
  standaloneFile: "src/js/base/handalone.js",
  urlFileMode: allRemote,
  pauseSchedule: undefined
};

export function makeDefaultCompileOptions(thisPyretDir: string): CompileOptions {
  return {
    ...defaultCompileOptions,
    baseDir: ".",
    thisPyretDir: thisPyretDir,
    depsFile: P.resolve(P.join(thisPyretDir, "bundled-node-deps.js")),
    standaloneFile: P.resolve(P.join(thisPyretDir, "js/handalone.js"))
  };
}

// ---------- runtime-provides: the catalog of builtin globals ----------

export const tPred = tArrow([tTop], tBoolean);
export const tPred2 = tArrow([tTop, tTop], tBoolean);

export const tNumberBinop = tArrow([tNumber, tNumber], tNumber);
export const tNumberUnop = tArrow([tNumber], tNumber);
export const tNumberPred1 = tArrow([tNumber], tBoolean);
export const tWithinNum = tArrow([tNumber], tArrow([tNumber, tNumber], tBoolean));
export const tWithinAny = tArrow([tNumber], tArrow([tTop, tTop], tBoolean));

export function tForall1(f: (a: T.Type) => T.Type): T.Type {
  const n = A.globalNames.makeAtom("a");
  return tForall([tVar(n)], f(tVar(n)));
}

function sd<V>(entries: Array<[string, V]>): Map<string, V> {
  return new Map(entries);
}

// NOTE: as in the Pyret original, the `values` position of runtime-provides
// holds raw Types rather than ValueExports (the StringDict annotation is not
// deeply checked in Pyret). Consumers use the keys, or treat entries as
// types; the cast below preserves that behavior.
export const runtimeProvides: Provides = new Provides("builtin://global",
  // MARK(joe/ben): modules
  sd<URI>([]),
  sd<T.Type>([
    ["test-print", tForall1((a) => tArrow([a], a))],
    ["print", tForall1((a) => tArrow([a], a))],
    ["display", tForall1((a) => tArrow([a], a))],
    ["print-error", tForall1((a) => tArrow([a], a))],
    ["display-error", tForall1((a) => tArrow([a], a))],
    ["tostring", tArrow([tTop], tStr)],
    ["to-string", tArrow([tTop], tStr)],
    ["torepr", tArrow([tTop], tStr)],
    ["to-repr", tArrow([tTop], tStr)],
    ["brander", tTop],
    ["raise", tArrow([tTop], tBot)],
    ["nothing", tNothing],
    ["builtins", tRecord(sd<T.Type>([
      ["has-field", tArrow([tRecord(sd<T.Type>([]))], tBoolean)],
      ["trace-value", tArrow([tTop, tTop], tBot)],
      ["current-checker", tArrow([], tRecord(sd<T.Type>([ // Cheat on these types for now.
        ["run-checks", tBot],
        ["check-is", tBot],
        ["check-is-not", tBot],
        ["check-is-not-refinement", tBot],
        ["check-is-refinement", tBot],
        ["check-satisfies", tBot],
        ["check-satisfies-not", tBot],
        ["check-raises-str", tBot],
        ["check-raises-not", tBot],
        ["check-raises-other-str", tBot],
        ["check-raises-satisfies", tBot],
        ["check-raises-violates", tBot]
      ])))]
    ]))],
    ["not", tArrow([tBoolean], tBoolean)],
    ["is-nothing", tPred],
    ["is-number", tPred],
    ["is-string", tPred],
    ["is-boolean", tPred],
    ["is-object", tPred],
    ["is-function", tPred],
    ["is-raw-array", tPred],
    ["is-tuple", tPred],
    ["is-table", tPred],
    ["is-row", tPred],
    ["gensym", tTop],
    ["random", tTop],
    ["run-task", tTop],
    ["_plus", tTop],
    ["_minus", tTop],
    ["_times", tTop],
    ["_divide", tTop],
    ["_lessthan", tTop],
    ["_lessequal", tTop],
    ["_greaterthan", tTop],
    ["_greaterequal", tTop],
    ["string-equal", tTop],
    ["string-contains", tTop],
    ["string-starts-with", tTop],
    ["string-ends-with", tTop],
    ["string-append", tTop],
    ["string-length", tTop],
    ["string-isnumber", tTop],
    ["string-is-number", tTop],
    ["string-tonumber", tTop],
    ["string-to-number", tArrow([tString], tOption(tNumber))],
    ["string-repeat", tTop],
    ["string-substring", tTop],
    ["string-replace", tTop],
    ["string-split", tTop],
    ["string-split-all", tTop],
    ["string-char-at", tTop],
    ["string-toupper", tTop],
    ["string-to-upper", tTop],
    ["string-tolower", tTop],
    ["string-to-lower", tTop],
    ["string-explode", tTop],
    ["string-index-of", tTop],
    ["string-to-code-point", tTop],
    ["string-from-code-point", tTop],
    ["string-to-code-points", tTop],
    ["string-from-code-points", tTop],
    ["time-now", tNumberUnop],
    ["num-random", tNumberUnop],
    ["num-random-seed", tArrow([tNumber], tNothing)],
    ["num-max", tNumberBinop],
    ["num-min", tNumberBinop],
    ["num-equal", tArrow([tNumber, tNumber], tBoolean)],
    ["num-truncate", tNumberUnop],
    ["num-ceiling", tNumberUnop],
    ["num-floor", tNumberUnop],
    ["num-round", tNumberUnop],
    ["num-round-even", tNumberUnop],
    ["num-truncate-digits", tNumberBinop],
    ["num-ceiling-digits", tNumberBinop],
    ["num-floor-digits", tNumberBinop],
    ["num-round-digits", tNumberBinop],
    ["num-round-even-digits", tNumberBinop],
    ["num-truncate-place", tNumberBinop],
    ["num-ceiling-place", tNumberBinop],
    ["num-floor-place", tNumberBinop],
    ["num-round-place", tNumberBinop],
    ["num-round-even-place", tNumberBinop],
    ["num-abs", tNumberUnop],
    ["num-sin", tNumberUnop],
    ["num-cos", tNumberUnop],
    ["num-tan", tNumberUnop],
    ["num-asin", tNumberUnop],
    ["num-acos", tNumberUnop],
    ["num-atan", tNumberUnop],
    ["num-atan2", tNumberBinop],
    ["num-modulo", tNumberBinop],
    ["num-remainder", tNumberBinop],
    ["num-sqrt", tNumberUnop],
    ["num-sqr", tNumberUnop],
    ["num-log", tNumberUnop],
    ["num-exp", tNumberUnop],
    ["num-exact", tNumberUnop],
    ["num-to-rational", tNumberUnop],
    ["num-to-roughnum", tNumberUnop],
    ["num-is-positive", tNumberPred1],
    ["num-is-negative", tNumberPred1],
    ["num-is-non-positive", tNumberPred1],
    ["num-is-non-negative", tNumberPred1],
    ["num-is-integer", tNumberPred1],
    ["num-is-fixnum", tNumberPred1],
    ["num-is-rational", tNumberPred1],
    ["num-is-roughnum", tNumberPred1],
    ["num-expt", tNumberBinop],
    ["num-tostring", tArrow([tNumber], tString)],
    ["num-to-string", tArrow([tNumber], tString)],
    ["num-to-string-digits", tArrow([tNumber, tNumber], tString)],
    ["num-within", tWithinNum],
    ["num-within-rel", tWithinNum],
    ["num-within-abs", tWithinNum],
    ["within-now", tWithinAny],
    ["within-rel", tWithinAny],
    ["within-rel-now", tWithinAny],
    ["within-abs", tWithinAny],
    ["within-abs-now", tWithinAny],
    ["within", tWithinAny],
    ["within-now3", tWithinAny],
    ["within-rel3", tWithinAny],
    ["within-rel-now3", tWithinAny],
    ["within-abs3", tWithinAny],
    ["within-abs-now3", tWithinAny],
    ["within3", tWithinAny],
    ["raw-array-get", tTop],
    ["raw-array-set", tTop],
    ["raw-array-of", tTop],
    ["raw-array-build", tTop],
    ["raw-array-build-opt", tTop],
    ["raw-array-length", tTop],
    ["raw-array-to-list", tTop],
    ["raw-array-fold", tTop],
    ["raw-array-filter", tTop],
    ["raw-array-and-mapi", tTop],
    ["raw-array-or-mapi", tTop],
    ["raw-array-map", tTop],
    ["raw-array-map-1", tTop],
    ["raw-array-join-str", tTop],
    ["raw-array-from-list", tTop],
    ["raw-array", tRecord(sd<T.Type>([
      ["make", tForall1((a) => tArrow([tArray(a)], tArray(a)))],
      ["make0", tForall1((a) => tArrow([], tArray(a)))],
      ["make1", tForall1((a) => tArrow([a], tArray(a)))],
      ["make2", tForall1((a) => tArrow([a, a], tArray(a)))],
      ["make3", tForall1((a) => tArrow([a, a, a], tArray(a)))],
      ["make4", tForall1((a) => tArrow([a, a, a, a], tArray(a)))],
      ["make5", tForall1((a) => tArrow([a, a, a, a, a], tArray(a)))]
    ]))],
    ["ref-get", tTop],
    ["ref-set", tTop],
    ["ref-freeze", tTop],
    ["equal-always", tPred2],
    ["equal-always3", tTop],
    ["equal-now", tPred2],
    ["equal-now3", tTop],
    ["roughly-equal-always", tPred2],
    ["roughly-equal-always3", tTop],
    ["roughly-equal-now", tPred2],
    ["roughly-equal-now3", tTop],
    ["roughly-equal", tPred2],
    ["identical", tPred2],
    // (sic) the Pyret original stores the *unapplied* T.t-top constructor for
    // these two entries; the nearest TS equivalent is the class itself.
    ["identical3", T.TTop as unknown as T.Type],
    ["exn-unwrap", T.TTop as unknown as T.Type]
  ]) as unknown as Map<string, ValueExport>,
  sd<T.Type>([
    ["Number", tTop],
    ["Exactnum", tTop],
    ["Roughnum", tTop],
    ["NumInteger", tTop],
    ["NumRational", tTop],
    ["NumPositive", tTop],
    ["NumNegative", tTop],
    ["NumNonPositive", tTop],
    ["NumNonNegative", tTop],
    ["String", tStr],
    ["Table", tTop],
    ["Row", tTop],
    ["Function", tTop],
    ["Boolean", tTop],
    ["Object", tTop],
    ["Method", tTop],
    ["Nothing", tTop],
    ["RawArray", tTop]
  ]),
  sd<DataExport>([]));

export const runtimeValues: Map<string, BindOrigin> = (() => {
  const rb = new Map<string, BindOrigin>();
  for (const k of runtimeProvides.values.keys()) {
    rb.set(k, new BindOrigin(new SL.Builtin("global"), new SL.Builtin("global"), true, "builtin://global", new A.SName(A.dummyLoc, k)));
  }
  return rb;
})();

export const runtimeTypes: Map<string, BindOrigin> = (() => {
  const rt = new Map<string, BindOrigin>();
  for (const k of runtimeProvides.aliases.keys()) {
    rt.set(k, new BindOrigin(new SL.Builtin("global"), new SL.Builtin("global"), true, "builtin://global", new A.SName(A.dummyLoc, k)));
  }
  for (const k of runtimeProvides.dataDefinitions.keys()) {
    rt.set(k, new BindOrigin(new SL.Builtin("global"), new SL.Builtin("global"), true, "builtin://global", new A.SName(A.dummyLoc, k)));
  }
  return rt;
})();

export const minimalImports = new ExtraImports([]);

export const standardImports = new ExtraImports([
  new ExtraImport(new Builtin("global"), "$global", [], []),
  new ExtraImport(new Builtin("base"), "$base", [], []),
  new ExtraImport(new Builtin("constants"), "$constants", ["PI"], []),
  new ExtraImport(new Builtin("arrays"), "arrays", [
    "array",
    "build-array",
    "array-from-list",
    "is-array",
    "array-of",
    "array-set-now",
    "array-get-now",
    "array-length",
    "array-to-list-now"
  ],
  ["Array"]),
  new ExtraImport(new Builtin("lists"), "lists", [
    "list",
    "is-List",
    "is-empty",
    "is-link",
    "empty",
    "link",
    "range",
    "range-by",
    "repeat",
    "filter",
    "partition",
    "split-at",
    "any",
    "find",
    "map",
    "map2",
    "map3",
    "map4",
    "map_n",
    "map2_n",
    "map3_n",
    "map4_n",
    "each",
    "each2",
    "each3",
    "each4",
    "each_n",
    "each2_n",
    "each3_n",
    "each4_n",
    "fold",
    "fold2",
    "fold3",
    "fold4"
  ],
  ["List"]),
  new ExtraImport(new Builtin("option"), "option", [
    "is-Option",
    "is-none",
    "is-some",
    "none",
    "some"
  ],
  ["Option"]),
  new ExtraImport(new Builtin("error"), "error", [], []),
  new ExtraImport(new Builtin("sets"), "sets", [
    "set",
    "tree-set",
    "list-set",
    "empty-set",
    "empty-list-set",
    "empty-tree-set",
    "list-to-set",
    "list-to-list-set",
    "list-to-tree-set"
  ],
  ["Set"])
]);

// MARK(joe/ben): modules
export const noBuiltins: CompileEnvironment = new CompileEnv(
  new Globals(new Map(), new Map(), new Map()),
  new Map(),
  new Map());

// MARK(joe/ben): modules
export const standardGlobals = new Globals(new Map(), runtimeValues, runtimeTypes);

export const noGlobals = new Globals(new Map(), new Map(), new Map());

export const reactorOptionalFields: Map<string, (l: Loc) => A.Ann> = new Map<string, (l: Loc) => A.Ann>([
  ["last-image", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["on-tick", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["to-draw", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["on-key", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["on-raw-key", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["on-mouse", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["stop-when", (l: Loc) => new A.AName(l, new A.STypeGlobal("Function"))],
  ["seconds-per-tick", (l: Loc) => new A.AName(l, new A.STypeGlobal("NumPositive"))],
  ["title", (l: Loc) => new A.AName(l, new A.STypeGlobal("String"))],
  ["close-when-stop", (l: Loc) => new A.AName(l, new A.STypeGlobal("Boolean"))]
]);

// reactor-fields = reactor-optional-fields.set("init", ...) -- persistent
// set in Pyret, so copy before extending.
export const reactorFields: Map<string, (l: Loc) => A.Ann> = (() => {
  const m = new Map(reactorOptionalFields);
  m.set("init", (l: Loc) => new A.AAny(l));
  return m;
})();

// `data CompileError` (compile-structs.arr lines 646-2917) lives in
// compile-errors.ts; re-export it so consumers see one module. This statement
// is last so the require() it compiles to runs only after this module has
// finished initializing (the two modules are mutually recursive).
export * from './compile-errors';
