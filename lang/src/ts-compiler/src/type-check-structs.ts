/*
  TS port of src/arr/compiler/type-check-structs.arr.

  Context/TCInfo data structures, the constraint system (rounds of unify),
  typing-result/fold-result monadic combinators, and the type-inference
  helpers used by test (examples) inference.

  Naming notes (collisions between Pyret data names and their variants):
  - data TypingResult: variant classes `TypingResult` ('typing-result') and
    `TypingError`; the union is `AnyTypingResult`.
  - data FoldResult<V>: variant classes `FoldResult<V>` ('fold-result') and
    `FoldErrors<V>`; the union is `AnyFoldResult<V>`.
  - data ConstraintSystem: variant classes `ConstraintSystem`
    ('constraint-system') and `NoConstraints` (singleton `noConstraints`);
    the union is `AnyConstraintSystem`.
  - data Context: single variant class `TypingContext`; `type Context =
    TypingContext`.
*/

import * as A from './ast';
import * as TS from './type-structs';
import * as C from './compile-structs';
import * as TD from './type-defaults';
import { Loc } from './srcloc';
import {
  InternalCompilerError,
  map2,
  mapGetValue,
  mapRemove,
  mapSet,
  partition,
  raise,
} from './shared';

type Type = TS.Type;
type DataType = TS.DataType;
type ModuleType = TS.ModuleType;
type TypeSet = TS.TypeSet;

const foldr2 = TS.foldr2;
const newExistential = TS.newExistential;
const newTypeVar = TS.newTypeVar;
const typeMemberMap = TS.typeMemberMap;

// Port of the type-logger trove module: logs only when a global
// `window.logger` object has been installed.
function log(name: string, payload: string): void {
  const w: any = (globalThis as any).window;
  if (typeof w !== 'undefined' && typeof w.logger !== 'undefined') {
    w.logger.log(name, { value: payload });
  }
}

// ---------- Set helpers ----------

// Pyret Set<Type> (tree sets in the source), keyed by type.key().
// Persistent discipline: operations return fresh Maps.
function emptyTypeSet(): TypeSet {
  return new Map();
}

function typeSetAdd(s: TypeSet, t: Type): TypeSet {
  const out: TypeSet = new Map(s);
  out.set(t.key(), t);
  return out;
}

function typeSetRemove(s: TypeSet, t: Type): TypeSet {
  const out: TypeSet = new Map(s);
  out.delete(t.key());
  return out;
}

function typeSetUnion(a: TypeSet, b: TypeSet): TypeSet {
  const out: TypeSet = new Map(a);
  for (const [k, v] of b) {
    if (!out.has(k)) { out.set(k, v); }
  }
  return out;
}

export function listToTypeSet(types: Type[]): TypeSet {
  const out: TypeSet = new Map();
  for (const t of types) {
    if (!out.has(t.key())) { out.set(t.key(), t); }
  }
  return out;
}

// Pyret tree-sets of strings iterate in sorted (string `<`) order; sort to
// preserve any order-sensitive folds over them.
function sortedStrings(it: Iterable<string>): string[] {
  return [...it].sort();
}

function stringSetIntersect(a: Set<string>, b: Set<string>): Set<string> {
  return new Set(sortedStrings(a).filter((k) => b.has(k)));
}

function stringSetDifference(a: Set<string>, b: Set<string>): Set<string> {
  return new Set(sortedStrings(a).filter((k) => !b.has(k)));
}

// ---------- data PathElement ----------

export abstract class PathElementBase {
  abstract get $name(): string;
  abstract toString(): string;
}

export class ArgPath extends PathElementBase {
  get $name(): 'arg-path' { return 'arg-path'; }
  constructor(public argNum: number) { super(); }
  toString(): string { return 'arg-path(' + String(this.argNum) + ')'; }
}

export class RetPath extends PathElementBase {
  get $name(): 'ret-path' { return 'ret-path'; }
  toString(): string { return 'ret-path'; }
}

export class AppPath extends PathElementBase {
  get $name(): 'app-path' { return 'app-path'; }
  constructor(public typeNum: number) { super(); }
  toString(): string { return 'app-path(' + String(this.typeNum) + ')'; }
}

export class RecordPath extends PathElementBase {
  get $name(): 'record-path' { return 'record-path'; }
  constructor(public fieldName: string) { super(); }
  toString(): string { return 'record-path(' + JSON.stringify(this.fieldName) + ')'; }
}

export class RefPath extends PathElementBase {
  get $name(): 'ref-path' { return 'ref-path'; }
  toString(): string { return 'ref-path'; }
}

export class TuplePath extends PathElementBase {
  get $name(): 'tuple-path' { return 'tuple-path'; }
  constructor(public tupleIndex: number) { super(); }
  toString(): string { return 'tuple-path(' + String(this.tupleIndex) + ')'; }
}

export type PathElement = ArgPath | RetPath | AppPath | RecordPath | RefPath | TuplePath;
export const retPath: RetPath = new RetPath();
export const refPath: RefPath = new RefPath();
export function isArgPath(x: any): x is ArgPath { return x instanceof ArgPath; }
export function isRetPath(x: any): x is RetPath { return x instanceof RetPath; }
export function isAppPath(x: any): x is AppPath { return x instanceof AppPath; }
export function isRecordPath(x: any): x is RecordPath { return x instanceof RecordPath; }
export function isRefPath(x: any): x is RefPath { return x instanceof RefPath; }
export function isTuplePath(x: any): x is TuplePath { return x instanceof TuplePath; }

export type Path = PathElement[];

// tostring of a Path, used as string-dict keys (Pyret tostring of a list).
function pathToKey(path: Path): string {
  return '[list: ' + path.map((p) => p.toString()).join(', ') + ']';
}

// Pyret Set<Path>, keyed by pathToKey.
export type PathSet = Map<string, Path>;

function pathSetIntersect(a: PathSet, b: PathSet): PathSet {
  const out: PathSet = new Map();
  for (const [k, v] of a) {
    if (b.has(k)) { out.set(k, v); }
  }
  return out;
}

// The StringDict is a mapping from the path of the type
export type Structure = Map<string, PathSet>;

// ---------- data TCInfo ----------

// "exported" context after type checking
export class TCInfo {
  get $name(): 'tc-info' { return 'tc-info'; }
  constructor(
    public types: Map<string, Type>,
    public aliases: Map<string, Type>,
    public dataTypes: Map<string, Type>,
  ) {}
}

export function isTCInfo(x: any): x is TCInfo { return x instanceof TCInfo; }

// ---------- Tuple/record shapes used by the constraint system ----------

export type CheckingFun = (typ: Type, context: Context) => AnyTypingResult;

export type ExampleTypeInfo = {
  argTypes: Type[];
  retType: Type;
  loc: Loc;
};

// {existential; annotation types; example types; check function; function name}
export type ExampleTypeEntry = [Type, ExampleTypeInfo, Type[], CheckingFun, string];

// {type; field labels -> field types (with the location of their use)}
export type FieldConstraint = [Type, Map<string, Type[]>];

// ---------- data Context ----------

export class TypingContext {
  get $name(): 'typing-context' { return 'typing-context'; }
  constructor(
    public globalTypes: Map<string, Type>, // global name -> type
    public aliases: Map<string, Type>, // t-name -> aliased type
    public dataTypes: Map<string, DataType>, // t-name -> data type
    public modules: Map<string, ModuleType>, // module name -> module type
    public moduleNames: Map<string, string>, // imported name -> module name
    public binds: Map<string, Type>, // local name -> type
    public constraints: AnyConstraintSystem, // constraints should only be added with methods to ensure that they have the proper forms
    public info: TCInfo,
    public misc: Map<string, [Type[], string]>, // miscellaneous info that is used for logging. Keyed by the function name
  ) {}

  toString(): string {
    return 'typing-context(' + String(this.binds.size) + ' binds, ' + this.constraints.$name + ')';
  }

  getDataType(typ: Type): DataType | undefined {
    const resolved = resolveAlias(typ, this);
    switch (resolved.$name) {
      case 't-name': {
        const moduleName = resolved.moduleName;
        switch (moduleName.$name) {
          case 'module-uri': {
            const mod = moduleName.uri;
            const tMod = this.modules.get(mod);
            if (tMod !== undefined) {
              const dataType = tMod.types.get(resolved.id.toname());
              if (dataType !== undefined) {
                return dataType;
              }
              return raise("No type " + resolved.toString() + " available on '" + tMod.toString() + "'");
            } else {
              if (mod === 'builtin') {
                return this.dataTypes.get(resolved.id.key());
              } else {
                return raise("No module available with the name '" + mod + "'");
              }
            }
          }
          case 'local': {
            const idKey = resolved.id.key();
            return this.dataTypes.get(idKey);
          }
          case 'dependency':
            return TS.depError(resolved as any);
          default:
            throw new InternalCompilerError('Unknown NameOrigin in get-data-type');
        }
      }
      default:
        return raise('get-data-type should only be called on t-names');
    }
  }

  setGlobalTypes(globalTypes: Map<string, Type>): Context {
    return new TypingContext(globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, this.info, this.misc);
  }

  setAliases(aliases: Map<string, Type>): Context {
    return new TypingContext(this.globalTypes, aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, this.info, this.misc);
  }

  setDataTypes(dataTypes: Map<string, DataType>): Context {
    return new TypingContext(this.globalTypes, this.aliases, dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, this.info, this.misc);
  }

  setModules(modules: Map<string, ModuleType>): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, modules, this.moduleNames, this.binds, this.constraints, this.info, this.misc);
  }

  setModuleNames(moduleNames: Map<string, string>): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, moduleNames, this.binds, this.constraints, this.info, this.misc);
  }

  setBinds(binds: Map<string, Type>): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, binds, this.constraints, this.info, this.misc);
  }

  setConstraints(constraints: AnyConstraintSystem): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, constraints, this.info, this.misc);
  }

  setInfo(info: TCInfo): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, info, this.misc);
  }

  addBinding(termKey: string, assignedType: Type): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, mapSet(this.binds, termKey, assignedType), this.constraints, this.info, this.misc);
  }

  removeBinding(termKey: string): Context {
    const currentType = mapGetValue(this.binds, termKey);
    const newInfo = new TCInfo(mapSet(this.info.types, termKey, currentType), this.info.aliases, this.info.dataTypes);
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, mapRemove(this.binds, termKey), this.constraints, newInfo, this.misc);
  }

  addDictToBindings(dict: Map<string, Type>): Context {
    const newBinds = new Map(this.binds);
    for (const key of dict.keys()) {
      newBinds.set(key, mapGetValue(dict, key));
    }
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, newBinds, this.constraints, this.info, this.misc);
  }

  addVariable(variable: Type): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addVariable(variable), this.info, this.misc);
  }

  addVariableSet(variables: TypeSet): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addVariableSet(variables), this.info, this.misc);
  }

  addConstraint(subtype: Type, supertype: Type): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addConstraint(subtype, supertype), this.info, this.misc);
  }

  addFieldConstraint(typ: Type, fieldName: string, fieldType: Type): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addFieldConstraint(typ, fieldName, fieldType), this.info, this.misc);
  }

  addExampleVariable(existential: Type, argTypes: Type[], retType: Type, loc: Loc, checkingFun: CheckingFun, funName: string): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addExampleVariable(existential, argTypes, retType, loc, checkingFun, funName), this.info, this.misc);
  }

  addExampleType(existential: Type, typ: Type): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addExampleType(existential, typ), this.info, this.misc);
  }

  addLevel(): Context {
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints.addLevel(), this.info, this.misc);
  }

  solveLevel(): AnyFoldResult<ConstraintSolution> {
    return this.constraints.solveLevel(this).bind(([newSystem, solution], context) => {
      return new FoldResult(solution, context.setConstraints(newSystem));
    });
  }

  // this method calls generalize as it will only ever be called on let-bound bindings
  substituteInBinds(solution: ConstraintSolution): Context {
    if (solution.isEmpty()) {
      return this;
    } else {
      const newBinds = new Map(this.binds);
      for (const key of newBinds.keys()) {
        const boundType = mapGetValue(newBinds, key);
        newBinds.set(key, solution.generalize(solution.apply(boundType)));
      }
      return this.setBinds(newBinds);
    }
  }

  addMiscExampleVariable(funKey: string, funName: string): Context {
    const misc = mapSet(this.misc, funKey, [[], funName] as [Type[], string]);
    return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, this.info, misc);
  }

  addMiscExampleType(funKey: string, typ: Type): Context {
    const entry = this.misc.get(funKey);
    if (entry === undefined) {
      return this;
    } else {
      const [typs, funName] = entry;
      const misc = mapSet(this.misc, funKey, [[typ, ...typs], funName] as [Type[], string]);
      return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, this.info, misc);
    }
  }

  substituteInMisc(solution: ConstraintSolution): Context {
    if (solution.isEmpty()) {
      return this;
    } else {
      const newMisc = new Map<string, [Type[], string]>();
      for (const key of this.misc.keys()) {
        const [types, name] = mapGetValue(this.misc, key);
        const newTypes = types.map((typ) => solution.apply(typ));
        newMisc.set(key, [newTypes, name]);
      }
      return new TypingContext(this.globalTypes, this.aliases, this.dataTypes, this.modules, this.moduleNames, this.binds, this.constraints, this.info, newMisc);
    }
  }
}

export type Context = TypingContext;
export function isTypingContext(x: any): x is TypingContext { return x instanceof TypingContext; }

// ---------- data ConstraintSolution ----------

export class ConstraintSolution {
  get $name(): 'constraint-solution' { return 'constraint-solution'; }
  constructor(
    public variables: TypeSet,
    public substitutions: Map<string, [Type, Type]>, // existential => {assigned-type; existential}
  ) {}

  isEmpty(): boolean {
    return this.variables.size === 0 && this.substitutions.size === 0;
  }

  apply(typ: Type): Type {
    const app = (x: Type): Type => this.apply(x);
    const substitutions = this.substitutions;
    switch (typ.$name) {
      case 't-name':
        return typ;
      case 't-arrow':
        return new TS.TArrow(typ.args.map(app), app(typ.ret), typ.l, typ.inferred);
      case 't-app':
        return new TS.TApp(app(typ.onto), typ.args.map(app), typ.l, typ.inferred);
      case 't-top':
        return typ;
      case 't-bot':
        return typ;
      case 't-record': {
        const mapApp = (xs: TS.TypeMembers): TS.TypeMembers => typeMemberMap(xs, (_, x) => app(x));
        return new TS.TRecord(mapApp(typ.fields), typ.l, typ.inferred);
      }
      case 't-tuple':
        return new TS.TTuple(typ.elts.map(app), typ.l, typ.inferred);
      case 't-forall':
        return new TS.TForall(typ.introduces, app(typ.onto), typ.l, typ.inferred);
      case 't-ref':
        return new TS.TRef(app(typ.typ), typ.l, typ.inferred);
      case 't-data-refinement':
        return new TS.TDataRefinement(app(typ.dataType), typ.variantName, typ.l, typ.inferred);
      case 't-var':
        return typ;
      case 't-existential': {
        const found = substitutions.get(typ.key());
        if (found === undefined) {
          return typ;
        } else {
          const [assignedType] = found;
          return app(assignedType.setLoc(typ.l).setInferred(typ.inferred || assignedType.inferred));
        }
      }
      default:
        throw new InternalCompilerError('Unknown Type in ConstraintSolution.apply');
    }
  }

  applyDataType(dataType: DataType): DataType {
    return new TS.TData(
      dataType.name,
      dataType.params,
      dataType.variants.map((v) => this.applyVariant(v)),
      typeMemberMap(dataType.fields, (_, x) => this.generalize(this.apply(x))),
      dataType.l);
  }

  applyVariant(variantType: TS.TypeVariant): TS.TypeVariant {
    switch (variantType.$name) {
      case 't-variant':
        return new TS.TVariant(variantType.name, variantType.fields, typeMemberMap(variantType.withFields, (_, x) => {
          return this.generalize(this.apply(x));
        }), variantType.l);
      case 't-singleton-variant':
        return new TS.TSingletonVariant(variantType.name, typeMemberMap(variantType.withFields, (_, x) => {
          return this.generalize(this.apply(x));
        }), variantType.l);
      default:
        throw new InternalCompilerError('Unknown TypeVariant in apply-variant');
    }
  }

  generalize(typ: Type): Type {
    const collectVars = (typ2: Type, varMapping: Map<string, Type>): [Type, Map<string, Type>] => {
      switch (typ2.$name) {
        case 't-name':
          return [typ2, varMapping];
        case 't-arrow': {
          const [newRet, retMapping] = collectVars(typ2.ret, varMapping);
          let newArgs: Type[] = [];
          let argsMapping = retMapping;
          for (let i = typ2.args.length - 1; i >= 0; i--) {
            const [newArg, argMapping] = collectVars(typ2.args[i], argsMapping);
            newArgs = [newArg, ...newArgs];
            argsMapping = argMapping;
          }
          return [new TS.TArrow(newArgs, newRet, typ2.l, typ2.inferred), argsMapping];
        }
        case 't-app': {
          const [newOnto, ontoMapping] = collectVars(typ2.onto, varMapping);
          let newArgs: Type[] = [];
          let argsMapping = ontoMapping;
          for (let i = typ2.args.length - 1; i >= 0; i--) {
            const [newArg, argMapping] = collectVars(typ2.args[i], argsMapping);
            newArgs = [newArg, ...newArgs];
            argsMapping = argMapping;
          }
          return [new TS.TApp(newOnto, newArgs, typ2.l, typ2.inferred), argsMapping];
        }
        case 't-top':
          return [typ2, varMapping];
        case 't-bot':
          return [typ2, varMapping];
        case 't-record': {
          const newFields: TS.TypeMembers = new Map();
          let fieldsMapping = varMapping;
          for (const key of typ2.fields.keys()) {
            const fieldTyp = mapGetValue(typ2.fields, key);
            const [newTyp, typMapping] = collectVars(fieldTyp, fieldsMapping);
            newFields.set(key, newTyp);
            fieldsMapping = typMapping;
          }
          return [new TS.TRecord(newFields, typ2.l, typ2.inferred), fieldsMapping];
        }
        case 't-tuple': {
          let newElts: Type[] = [];
          let eltsMapping = varMapping;
          for (let i = typ2.elts.length - 1; i >= 0; i--) {
            const [newElt, eltMapping] = collectVars(typ2.elts[i], eltsMapping);
            newElts = [newElt, ...newElts];
            eltsMapping = eltMapping;
          }
          return [new TS.TTuple(newElts, typ2.l, typ2.inferred), eltsMapping];
        }
        case 't-forall': {
          const [newOnto, ontoMapping] = collectVars(typ2.onto, varMapping);
          return [new TS.TForall(typ2.introduces, newOnto, typ2.l, typ2.inferred), ontoMapping];
        }
        case 't-ref': {
          const [newOnto, ontoMapping] = collectVars(typ2.typ, varMapping);
          return [new TS.TRef(newOnto, typ2.l, typ2.inferred), ontoMapping];
        }
        case 't-data-refinement': {
          const [newDataType, dataTypeMapping] = collectVars(typ2.dataType, varMapping);
          return [new TS.TDataRefinement(newDataType, typ2.variantName, typ2.l, typ2.inferred), dataTypeMapping];
        }
        case 't-var':
          return [typ2, varMapping];
        case 't-existential': {
          if (this.variables.has(typ2.key())) {
            const mappedTyp = varMapping.get(typ2.key());
            if (mappedTyp === undefined) {
              const newVar = newTypeVar(typ2.l);
              return [newVar, mapSet(varMapping, typ2.key(), newVar)];
            } else {
              return [mappedTyp, varMapping];
            }
          } else {
            return [typ2, varMapping];
          }
        }
        default:
          throw new InternalCompilerError('Unknown Type in generalize');
      }
    };
    const [newTyp, varsMapping] = collectVars(typ, new Map<string, Type>());
    const vars = [...varsMapping.keys()].map((key) => mapGetValue(varsMapping, key));
    if (vars.length === 0) {
      return typ;
    } else {
      return new TS.TForall(vars, newTyp, typ.l, false);
    }
  }
}

export function isConstraintSolution(x: any): x is ConstraintSolution { return x instanceof ConstraintSolution; }

// ---------- data ConstraintSystem ----------

export abstract class ConstraintSystemBase {
  abstract get $name(): string;

  addVariable(variable: Type): AnyConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return raise("can't add variable to an uninitialized system");
      case 'constraint-system': {
        if (TS.isTExistential(variable)) {
          return new ConstraintSystem(typeSetAdd(self.variables, variable), self.constraints, self.refinementConstraints, self.fieldConstraints, self.exampleTypes, self.nextSystem);
        } else {
          return self;
        }
      }
    }
  }

  addVariableSet(newVariables: TypeSet): AnyConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return raise("can't add variables to an uninitialized system");
      case 'constraint-system':
        return new ConstraintSystem(typeSetUnion(self.variables, newVariables), self.constraints, self.refinementConstraints, self.fieldConstraints, self.exampleTypes, self.nextSystem);
    }
  }

  addConstraint(subtype: Type, supertype: Type): AnyConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return raise("can't add constraints to an uninitialized system: " + subtype.toString() + " = " + supertype.toString() + "\n" + subtype.l.toString() + "\n" + supertype.l.toString());
      case 'constraint-system': {
        const addRefinement = (exists: Type, refinement: Type): AnyConstraintSystem => {
          return new ConstraintSystem(self.variables, self.constraints, [[exists, refinement] as [Type, Type], ...self.refinementConstraints], self.fieldConstraints, self.exampleTypes, self.nextSystem);
        };
        if (TS.isTExistential(subtype) && TS.isTDataRefinement(supertype)) {
          return addRefinement(subtype, supertype);
        } else if (TS.isTExistential(supertype) && TS.isTDataRefinement(subtype)) {
          return addRefinement(supertype, subtype);
        } else {
          return new ConstraintSystem(self.variables, [[subtype, supertype] as [Type, Type], ...self.constraints], self.refinementConstraints, self.fieldConstraints, self.exampleTypes, self.nextSystem);
        }
      }
    }
  }

  addFieldConstraint(typ: Type, fieldName: string, fieldType: Type): AnyConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return raise("can't add constraints to an uninitialized system");
      case 'constraint-system': {
        const existing = self.fieldConstraints.get(typ.key());
        let newFieldConstraints: Map<string, FieldConstraint>;
        if (existing !== undefined) {
          const [constraintTyp, labelMapping] = existing;
          const currentTypes = labelMapping.get(fieldName);
          const newLabelMapping = currentTypes !== undefined
            ? mapSet(labelMapping, fieldName, [fieldType, ...currentTypes])
            : mapSet(labelMapping, fieldName, [fieldType]);
          newFieldConstraints = mapSet(self.fieldConstraints, typ.key(), [constraintTyp, newLabelMapping] as FieldConstraint);
        } else {
          newFieldConstraints = mapSet(self.fieldConstraints, typ.key(), [typ, new Map([[fieldName, [fieldType]]])] as FieldConstraint);
        }
        return new ConstraintSystem(self.variables, self.constraints, self.refinementConstraints, newFieldConstraints, self.exampleTypes, self.nextSystem);
      }
    }
  }

  addExampleVariable(existential: Type, argTypes: Type[], retType: Type, loc: Loc, checkingFun: CheckingFun, funName: string): AnyConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return raise("can't add constraints to an uninitialized system");
      case 'constraint-system':
        return new ConstraintSystem(self.variables, self.constraints, self.refinementConstraints, self.fieldConstraints,
          mapSet(self.exampleTypes, existential.key(), [existential, { argTypes: argTypes, retType: retType, loc: loc }, [], checkingFun, funName] as ExampleTypeEntry),
          self.nextSystem);
    }
  }

  addExampleType(existential: Type, typ: Type): AnyConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return raise("can't add constraints to an uninitialized system");
      case 'constraint-system': {
        const found = self.exampleTypes.get(existential.key());
        if (found === undefined) {
          return new ConstraintSystem(self.variables, self.constraints, self.refinementConstraints, self.fieldConstraints, self.exampleTypes, self.nextSystem.addExampleType(existential, typ));
        } else {
          const [foundExistential, inferenceData, typs, checkingFun, funName] = found;
          void foundExistential;
          const newExampleTypes = mapSet(self.exampleTypes, existential.key(), [existential, inferenceData, [typ, ...typs], checkingFun, funName] as ExampleTypeEntry);
          return new ConstraintSystem(self.variables, self.constraints, self.refinementConstraints, self.fieldConstraints, newExampleTypes, self.nextSystem);
        }
      }
    }
  }

  addLevel(): ConstraintSystem {
    const self = this as unknown as AnyConstraintSystem;
    return new ConstraintSystem(emptyTypeSet(), [], [], new Map(), new Map(), self);
  }

  solveLevelHelper(solution: ConstraintSolution, context: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> {
    const self = this as unknown as AnyConstraintSystem;
    return solveHelperConstraints(self, solution, context).bind(([system, solution2], context2) => {
      return solveHelperRefinements(system, solution2, context2).bind(([system2, solution3], context3) => {
        return solveHelperExamples(system2, solution3, context3).bind(([system3, solution4], context4) => {
          return solveHelperFields(system3, solution4, context4);
        });
      });
    });
  }

  solveLevel(context: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> {
    const self = this as unknown as AnyConstraintSystem;
    switch (self.$name) {
      case 'no-constraints':
        return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([self, new ConstraintSolution(emptyTypeSet(), new Map())], context);
      case 'constraint-system': {
        // introduce a half level so any constraints depending on test inference can be solved after test inference
        let nextSystem: AnyConstraintSystem = self.nextSystem.addLevel();
        let variables = self.variables;
        for (const key of self.exampleTypes.keys()) {
          const [existential] = mapGetValue(self.exampleTypes, key);
          variables = typeSetRemove(variables, existential);
          nextSystem = nextSystem.addVariable(existential);
        }
        const system = new ConstraintSystem(variables, self.constraints, self.refinementConstraints, self.fieldConstraints, self.exampleTypes, nextSystem);
        return system.solveLevelHelper(new ConstraintSolution(emptyTypeSet(), new Map()), context).bind(([system1, solution1], context1) => {
          // This is solving the level introduced above
          const cs1 = system1 as ConstraintSystem;
          const system2 = cs1.nextSystem.addVariableSet(cs1.variables);
          return system2.solveLevelHelper(solution1, context1).bind(([system3, solution3], context3) => {
            const solutionOuter = new ConstraintSolution(variables, solution3.substitutions);
            switch (system3.$name) {
              case 'no-constraints':
                return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system3, solutionOuter], context3);
              case 'constraint-system': {
                const innerVariables = system3.variables;
                const innerNextSystem = system3.nextSystem;
                const solutionInner = new ConstraintSolution(innerVariables, solution3.substitutions);
                const newNextSystem = isConstraintSystem(innerNextSystem)
                  ? innerNextSystem.addVariableSet(innerVariables)
                  : innerNextSystem;
                return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([newNextSystem, solutionInner], context3);
              }
            }
          });
        });
      }
    }
  }
}

export class ConstraintSystem extends ConstraintSystemBase {
  get $name(): 'constraint-system' { return 'constraint-system'; }
  constructor(
    public variables: TypeSet, // the constrained existentials
    public constraints: [Type, Type][], // {subtype; supertype}
    public refinementConstraints: [Type, Type][], // {existential; t-data-refinement}
    public fieldConstraints: Map<string, FieldConstraint>, // type -> {type; field labels -> field types (with the location of their use)}
    public exampleTypes: Map<string, ExampleTypeEntry>, // existential type for function (from examples) -> {existential; annotation types; example types; check function; function name}
    public nextSystem: AnyConstraintSystem,
  ) { super(); }
}

export class NoConstraints extends ConstraintSystemBase {
  get $name(): 'no-constraints' { return 'no-constraints'; }
}

export type AnyConstraintSystem = ConstraintSystem | NoConstraints;
export const noConstraints: NoConstraints = new NoConstraints();
export function isConstraintSystem(x: any): x is ConstraintSystem { return x instanceof ConstraintSystem; }
export function isNoConstraints(x: any): x is NoConstraints { return x instanceof NoConstraints; }

// ---------- Constraint solving ----------

export function substituteInConstraints(newType: Type, typeVar: Type, constraints: [Type, Type][]): [Type, Type][] {
  return constraints.map(([subtype, supertype]): [Type, Type] => {
    return [subtype.substitute(newType, typeVar), supertype.substitute(newType, typeVar)];
  });
}

export function substituteInFieldConstraints(newType: Type, typeVar: Type, fieldConstraints: Map<string, FieldConstraint>): Map<string, FieldConstraint> {
  const newConstraints = new Map<string, FieldConstraint>();
  for (const key of fieldConstraints.keys()) {
    const [constraintType, fieldMappings] = mapGetValue(fieldConstraints, key);
    const newConstraintType = constraintType.substitute(newType, typeVar);
    const newFieldMappings = new Map<string, Type[]>();
    for (const fieldName of fieldMappings.keys()) {
      const types = mapGetValue(fieldMappings, fieldName);
      const newTypes = types.map((typ) => typ.substitute(newType, typeVar));
      newFieldMappings.set(fieldName, newTypes);
    }
    newConstraints.set(key, [newConstraintType, newFieldMappings]);
  }
  return newConstraints;
}

export function substituteInExampleTypes(newType: Type, typeVar: Type, exampleTypes: Map<string, ExampleTypeEntry>): Map<string, ExampleTypeEntry> {
  const newExampleTypes = new Map<string, ExampleTypeEntry>();
  for (const key of exampleTypes.keys()) {
    const [existential, info, typs, checkFun, funName] = mapGetValue(exampleTypes, key);
    newExampleTypes.set(key, [existential, info, typs.map((typ) => typ.substitute(newType, typeVar)), checkFun, funName]);
  }
  return newExampleTypes;
}

export function addSubstitution(newType: Type, typeVar: Type, system: ConstraintSystem, solution: ConstraintSolution): { solution: ConstraintSolution; system: ConstraintSystem } {
  const substitutions = mapSet(solution.substitutions, typeVar.key(), [newType, typeVar] as [Type, Type]);
  const constraints = substituteInConstraints(newType, typeVar, system.constraints);
  const refinementConstraints = substituteInConstraints(newType, typeVar, system.refinementConstraints);
  const fieldConstraints = substituteInFieldConstraints(newType, typeVar, system.fieldConstraints);
  const exampleTypes = substituteInExampleTypes(newType, typeVar, system.exampleTypes);

  return {
    solution: new ConstraintSolution(emptyTypeSet(), substitutions),
    system: new ConstraintSystem(system.variables, constraints, refinementConstraints, fieldConstraints, exampleTypes, system.nextSystem),
  };
}

export function solveHelperConstraints(system: AnyConstraintSystem, solution: ConstraintSolution, context: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> {
  const addSubstitutionAndContinue = (newType: Type, typeVar: Type, system2: ConstraintSystem, solution2: ConstraintSolution, context2: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> => {
    const newSolutionAndSystem = addSubstitution(newType, typeVar, system2, solution2);
    return solveHelperConstraints(newSolutionAndSystem.system, newSolutionAndSystem.solution, context2);
  };

  switch (system.$name) {
    case 'no-constraints':
      return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
    case 'constraint-system': {
      const { variables, constraints, refinementConstraints, fieldConstraints, exampleTypes, nextSystem } = system;
      if (constraints.length === 0) {
        return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
      }
      const first = constraints[0];
      const rest = constraints.slice(1);
      const shadowSystem = new ConstraintSystem(variables, rest, refinementConstraints, fieldConstraints, exampleTypes, nextSystem);
      const [subtype, supertype] = first;
      // ($name cast avoids TS narrowing subtype/supertype for the cases below
      if ((supertype.$name as string) === 't-top' || (subtype.$name as string) === 't-bot') {
        return solveHelperConstraints(shadowSystem, solution, context);
      }
      switch (supertype.$name) {
        case 't-existential': {
          switch (subtype.$name) {
            case 't-existential': {
              if (subtype.id.key() === supertype.id.key()) {
                return solveHelperConstraints(shadowSystem, solution, context);
              } else {
                if (shadowSystem.variables.has(subtype.key())) {
                  return addSubstitutionAndContinue(supertype, subtype, shadowSystem, solution, context);
                } else if (shadowSystem.variables.has(supertype.key())) {
                  return addSubstitutionAndContinue(subtype, supertype, shadowSystem, solution, context);
                } else {
                  return solveHelperConstraints(
                    new ConstraintSystem(shadowSystem.variables,
                      shadowSystem.constraints,
                      shadowSystem.refinementConstraints,
                      shadowSystem.fieldConstraints,
                      shadowSystem.exampleTypes,
                      shadowSystem.nextSystem.addConstraint(subtype, supertype)),
                    solution,
                    context);
                }
              }
            }
            default: {
              if (shadowSystem.variables.has(supertype.key())) {
                if (subtype.hasVariableFree(supertype)) {
                  return addSubstitutionAndContinue(subtype, supertype, shadowSystem, solution, context);
                } else {
                  return new FoldErrors<[AnyConstraintSystem, ConstraintSolution]>([new C.CantTypecheck("The types " + supertype.toString() + " and " + subtype.toString() + " are mutually recursive and their constraints cannot be solved", supertype.l)]);
                }
              } else {
                return solveHelperConstraints(
                  new ConstraintSystem(shadowSystem.variables,
                    shadowSystem.constraints,
                    shadowSystem.refinementConstraints,
                    shadowSystem.fieldConstraints,
                    shadowSystem.exampleTypes,
                    shadowSystem.nextSystem.addConstraint(subtype, supertype)),
                  solution,
                  context);
              }
            }
          }
        }
        case 't-data-refinement':
          return solveHelperConstraints(shadowSystem.addConstraint(subtype, supertype.dataType), solution, context);
        case 't-forall': {
          const newExistentials = supertype.introduces.map((variable) => newExistential(variable.l, false));
          const bOnto = foldr2((onto: Type, variable: Type, exists: Type) => {
            return onto.substitute(exists, variable);
          }, supertype.onto, supertype.introduces, newExistentials);
          const system2 = shadowSystem.addVariableSet(listToTypeSet(newExistentials));
          return solveHelperConstraints(system2.addConstraint(subtype, bOnto), solution, context);
        }
        default: {
          switch (subtype.$name) {
            case 't-name': {
              if (supertype.$name === 't-name') {
                if (subtype.moduleName.equals(supertype.moduleName) && subtype.id.key() === supertype.id.key()) {
                  return solveHelperConstraints(shadowSystem, solution, context);
                } else {
                  return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
                }
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-arrow': {
              if (supertype.$name === 't-arrow') {
                if (subtype.args.length !== supertype.args.length) {
                  // TODO(MATT) mention argument length
                  return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
                } else {
                  const system2 = foldr2((acc: AnyConstraintSystem, aArg: Type, bArg: Type) => {
                    return acc.addConstraint(bArg, aArg);
                  }, shadowSystem.addConstraint(subtype.ret, supertype.ret), subtype.args, supertype.args);
                  return solveHelperConstraints(system2, solution, context);
                }
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-app': {
              if (supertype.$name === 't-app') {
                if (subtype.args.length !== supertype.args.length) {
                  // TODO(MATT) mention argument length
                  return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
                } else {
                  const system2 = foldr2((acc: AnyConstraintSystem, aArg: Type, bArg: Type) => {
                    return acc.addConstraint(aArg, bArg);
                  }, shadowSystem.addConstraint(subtype.onto, supertype.onto), subtype.args, supertype.args);
                  return solveHelperConstraints(system2, solution, context);
                }
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-top': {
              if (supertype.$name === 't-top') {
                return solveHelperConstraints(shadowSystem, solution, context);
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-bot': {
              if (supertype.$name === 't-bot') {
                return solveHelperConstraints(shadowSystem, solution, context);
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-record': {
              if (supertype.$name === 't-record') {
                const aFields = subtype.fields;
                const bFields = supertype.fields;
                return foldrFoldResult<string, AnyConstraintSystem>((bKey, context2, system2) => {
                  const aField = aFields.get(bKey);
                  if (aField !== undefined) {
                    const bField = mapGetValue(bFields, bKey);
                    return new FoldResult<AnyConstraintSystem>(system2.addConstraint(aField, bField), context2);
                  } else {
                    // TODO(MATT): field missing error
                    return new FoldErrors<AnyConstraintSystem>([new C.TypeMismatch(subtype, supertype)]);
                  }
                }, [...bFields.keys()], context, shadowSystem).bind((system2, context2) => {
                  return solveHelperConstraints(system2, solution, context2);
                });
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-tuple': {
              if (supertype.$name === 't-tuple') {
                if (subtype.elts.length !== supertype.elts.length) {
                  // TODO(MATT): more specific error
                  return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
                } else {
                  const system2 = foldr2((acc: AnyConstraintSystem, aElt: Type, bElt: Type) => {
                    return acc.addConstraint(aElt, bElt);
                  }, shadowSystem as AnyConstraintSystem, subtype.elts, supertype.elts);
                  return solveHelperConstraints(system2, solution, context);
                }
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-forall': {
              const newExistentials = subtype.introduces.map((variable) => newExistential(variable.l, false));
              const aOnto = foldr2((onto: Type, variable: Type, exists: Type) => {
                return onto.substitute(exists, variable);
              }, subtype.onto, subtype.introduces, newExistentials);
              const system2 = shadowSystem.addVariableSet(listToTypeSet(newExistentials));
              return solveHelperConstraints(system2.addConstraint(aOnto, supertype), solution, context);
            }
            case 't-ref': {
              if (supertype.$name === 't-ref') {
                return solveHelperConstraints(shadowSystem.addConstraint(subtype.typ, supertype.typ), solution, context);
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-data-refinement':
              return solveHelperConstraints(shadowSystem.addConstraint(subtype.dataType, supertype), solution, context);
            case 't-var': {
              if (supertype.$name === 't-var') {
                if (subtype.id.key() === supertype.id.key()) {
                  return solveHelperConstraints(shadowSystem, solution, context);
                } else {
                  return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
                }
              } else {
                return new FoldErrors([new C.TypeMismatch(subtype, supertype)]);
              }
            }
            case 't-existential': {
              const system2 = shadowSystem.addConstraint(supertype, subtype);
              return solveHelperConstraints(system2, solution, context);
            }
            default:
              throw new InternalCompilerError('Unknown Type in solve-helper-constraints');
          }
        }
      }
    }
  }
}

export function solveHelperRefinements(system: AnyConstraintSystem, solution: ConstraintSolution, context: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> {
  switch (system.$name) {
    case 'no-constraints':
      return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
    case 'constraint-system': {
      const { variables, constraints, fieldConstraints, exampleTypes, nextSystem } = system;
      const partitioned = partition(([lhs]: [Type, Type]) => TS.isTExistential(lhs), system.refinementConstraints);
      const refinementConstraints = partitioned.isTrue;
      const normalConstraints = partitioned.isFalse;
      if (normalConstraints.length > 0) {
        const system2 = new ConstraintSystem(variables, normalConstraints, refinementConstraints, fieldConstraints, exampleTypes, nextSystem);
        return solveHelperConstraints(system2, solution, context).bind(([system3, solution3], context3) => {
          return solveHelperRefinements(system3, solution3, context3);
        });
      } else {
        if (refinementConstraints.length === 0) {
          return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
        } else {
          const refinementPartition = partition(([exists]: [Type, Type]) => variables.has(exists.key()), refinementConstraints);
          const keptRefinementConstraints = refinementPartition.isTrue;
          const nextLevelRefinements = refinementPartition.isFalse;
          if (nextLevelRefinements.length > 0) {
            let newNextSystem: AnyConstraintSystem = nextSystem;
            for (const [lhs, rhs] of nextLevelRefinements) {
              newNextSystem = newNextSystem.addConstraint(lhs, rhs);
            }
            const system2 = new ConstraintSystem(variables, constraints, keptRefinementConstraints, fieldConstraints, exampleTypes, newNextSystem);
            return solveHelperRefinements(system2, solution, context);
          } else {
            const mappings = new Map<string, [Type, Type[]]>();
            for (const [exists, refinement] of keptRefinementConstraints) {
              const found = mappings.get(exists.key());
              if (found === undefined) {
                mappings.set(exists.key(), [exists, [refinement]]);
              } else {
                const [, others] = found;
                mappings.set(exists.key(), [exists, [refinement, ...others]]);
              }
            }

            let tempSystem: AnyConstraintSystem = system;
            const tempVariables = new Set<string>();
            for (const key of mappings.keys()) {
              const [existential, refinements] = mapGetValue(mappings, key);
              const tempVariable = newExistential(existential.l, false);
              tempSystem = tempSystem.addVariable(tempVariable);
              for (const refinement of refinements) {
                if (refinement.$name === 't-data-refinement') {
                  tempSystem = tempSystem.addConstraint(tempVariable, refinement.dataType);
                } else {
                  tempSystem = tempSystem.addConstraint(tempVariable, refinement);
                }
              }
              tempVariables.add(tempVariable.key());
            }

            return solveHelperConstraints(tempSystem, new ConstraintSolution(emptyTypeSet(), new Map()), context).bind(([tempSystem2, tempSolution], context2) => {
              const tempSubstitutions = tempSolution.substitutions;
              const tempKeysSet = new Set(tempSubstitutions.keys());
              const remainingTempVariables = stringSetDifference(tempVariables, tempKeysSet);
              if (remainingTempVariables.size > 0) { // or not(temp-system.refinement-constraints.length() == refinement-constraints.length()): # some change in refinement constraints
                const newSubstitutions = new Map(solution.substitutions);
                for (const key of tempSubstitutions.keys()) {
                  newSubstitutions.set(key, mapGetValue(tempSubstitutions, key));
                }
                const solution2 = new ConstraintSolution(emptyTypeSet(), newSubstitutions);
                return solveHelperRefinements(tempSystem2, solution2, context2);
              } else {
                // merge all constraints for each existential variable
                // same data-refinements get merged otherwise goes to the inner data type

                let systemAndSolution = { system: system as ConstraintSystem, solution: solution };
                for (const key of mappings.keys()) {
                  const [exists, refinements] = mapGetValue(mappings, key);
                  let merged = refinements[0];
                  for (const refinement of refinements.slice(1)) {
                    if (merged.$name === 't-data-refinement') {
                      if (refinement.$name === 't-data-refinement') {
                        if (merged.variantName === refinement.variantName) {
                          // merged stays the same
                        } else {
                          merged = merged.dataType;
                        }
                      } else {
                        merged = refinement;
                      }
                    } else {
                      // merged stays the same
                    }
                  }
                  systemAndSolution = addSubstitution(merged, exists, systemAndSolution.system, systemAndSolution.solution);
                }
                const newSystem = systemAndSolution.system;
                const newSolution = systemAndSolution.solution;
                return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>(
                  [new ConstraintSystem(variables, [], [], newSystem.fieldConstraints, newSystem.exampleTypes, nextSystem),
                    new ConstraintSolution(emptyTypeSet(), newSolution.substitutions)],
                  context2);
              }
            });
          }
        }
      }
    }
  }
}

export function solveHelperFields(system: AnyConstraintSystem, solution: ConstraintSolution, context: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> {
  switch (system.$name) {
    case 'no-constraints':
      return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
    case 'constraint-system': {
      const { variables, fieldConstraints } = system;
      const keysList = [...fieldConstraints.keys()];
      if (keysList.length === 0) {
        return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
      }
      const first = keysList[0];
      const [typ0, fieldMappings] = mapGetValue(fieldConstraints, first);
      const system2 = new ConstraintSystem(system.variables, system.constraints, system.refinementConstraints, mapRemove(system.fieldConstraints, first), system.exampleTypes, system.nextSystem);
      return instantiateObjectType(typ0, context).bind((typ, context2) => {
        let system3: ConstraintSystem = system2;
        const contextConstraints = context2.constraints;
        if (contextConstraints.$name === 'constraint-system') {
          system3 = new ConstraintSystem(typeSetUnion(system2.variables, contextConstraints.variables), system2.constraints, system2.refinementConstraints, system2.fieldConstraints, system2.exampleTypes, system2.nextSystem);
        }

        switch (typ.$name) {
          case 't-record': {
            const fields = typ.fields;
            const fieldSet = new Set(fields.keys());
            const requiredFieldSet = new Set(fieldMappings.keys());
            const intersection = stringSetIntersect(fieldSet, requiredFieldSet);
            const remainingFields = stringSetDifference(requiredFieldSet, intersection);
            if (remainingFields.size > 0) {
              const missingFieldErrors = [...remainingFields].map((remainingFieldName) => {
                return new C.ObjectMissingField(remainingFieldName, typ.toString(), typ.l, mapGetValue(fieldMappings, remainingFieldName)[0].l);
              });
              return new FoldErrors<[AnyConstraintSystem, ConstraintSolution]>(missingFieldErrors);
            } else {
              let system4: AnyConstraintSystem = system3;
              for (const fieldName of intersection) {
                for (const fieldType of mapGetValue(fieldMappings, fieldName)) {
                  const objectFieldType = mapGetValue(fields, fieldName);
                  system4 = system4.addConstraint(objectFieldType, fieldType);
                }
              }
              return system4.solveLevelHelper(solution, context2);
            }
          }
          case 't-existential': {
            if (variables.has(typ.key())) {
              return new FoldErrors<[AnyConstraintSystem, ConstraintSolution]>([new C.UnableToInfer(typ.l)]);
            } else {
              let newNextSystem = system3.nextSystem;
              for (const fieldName of fieldMappings.keys()) {
                const fieldTypes = mapGetValue(fieldMappings, fieldName);
                for (const fieldType of fieldTypes) {
                  newNextSystem = newNextSystem.addFieldConstraint(typ, fieldName, fieldType);
                }
              }
              const system4 = new ConstraintSystem(system3.variables, system3.constraints, system3.refinementConstraints, system3.fieldConstraints, system3.exampleTypes, newNextSystem);
              return solveHelperFields(system4, solution, context2);
            }
          }
          default: {
            return instantiateDataType(typ, context2).bind((dataType, context3) => {
              const dataFields = dataType.fields;
              return foldrFoldResult<string, AnyConstraintSystem>((fieldName, context4, system4) => {
                const dataFieldType = dataFields.get(fieldName);
                if (dataFieldType === undefined) {
                  return new FoldErrors<AnyConstraintSystem>([new C.ObjectMissingField(fieldName, typ.toString(), typ.l, mapGetValue(fieldMappings, fieldName)[0].l)]);
                } else {
                  let system5 = system4;
                  for (const fieldType of mapGetValue(fieldMappings, fieldName)) {
                    system5 = system5.addConstraint(dataFieldType, fieldType);
                  }
                  return new FoldResult<AnyConstraintSystem>(system5, context4);
                }
              }, [...fieldMappings.keys()], context3, system3).bind((system6, context5) => {
                return system6.solveLevelHelper(solution, context5);
              });
            });
          }
        }
      });
    }
  }
}

// TODO(MATT): check something about incomplete-examples
// TODO(MATT): check the types of the resulting values
export function solveHelperExamples(system: AnyConstraintSystem, solution: ConstraintSolution, context: Context): AnyFoldResult<[AnyConstraintSystem, ConstraintSolution]> {
  switch (system.$name) {
    case 'no-constraints':
      return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>([system, solution], context);
    case 'constraint-system': {
      const { exampleTypes } = system;
      return foldrFoldResult<string, [AnyConstraintSystem, ConstraintSolution]>((existentialKey, context2, [system2, solution2]) => {
        const [existential, , rawFunExamples, , funName] = mapGetValue(exampleTypes, existentialKey);
        const funExamples = rawFunExamples.map((example) => removeRefinementsAndForalls(example));
        const partitioned = partition((typ: Type) => typ.freeVariables().size === 0, funExamples);
        const completeExamples = partitioned.isTrue;
        // const incompleteExamples = partitioned.isFalse;
        if (completeExamples.length > 0) {
          const first = completeExamples[0];
          const rest = completeExamples.slice(1);
          const generalized = rest.reduceRight((acc, typ) => generalizeType(typ, acc), first);
          const firstStructure = findStructure(first);
          const commonStructure = rest.reduceRight((acc, typ) => findCommonStructure(typ, acc), firstStructure);
          const newType = maintainCommonStructure(commonStructure, generalized);

          const logPayload = "{"
            + "'function-name': " + "'" + funName + "'" + ","
            + "'inferred-type': " + "'" + newType.toString() + "'" + ","
            + "}";
          log('test-inferred-type', logPayload);

          return new FoldResult<[AnyConstraintSystem, ConstraintSolution]>(
            [system2, new ConstraintSolution(emptyTypeSet(), mapSet(solution2.substitutions, existentialKey, [newType, existential] as [Type, Type]))],
            context2);
        } else {
          return new FoldErrors<[AnyConstraintSystem, ConstraintSolution]>([new C.UnannFailedTestInference(existential.l)]);
        }
      }, [...exampleTypes.keys()], context, [system, solution]);
    }
  }
}

export function removeRefinementsAndForalls(typ: Type): Type {
  const rraf = removeRefinementsAndForalls;
  switch (typ.$name) {
    case 't-name':
      return typ;
    case 't-arrow': {
      const newArgs = typ.args.map(rraf);
      const newRet = rraf(typ.ret);
      return new TS.TArrow(newArgs, newRet, typ.l, typ.inferred);
    }
    case 't-app': {
      const newOnto = rraf(typ.onto);
      const newArgs = typ.args.map(rraf);
      return new TS.TApp(newOnto, newArgs, typ.l, typ.inferred);
    }
    case 't-top':
      return typ;
    case 't-bot':
      return typ;
    case 't-record': {
      const newFields = typeMemberMap(typ.fields, (_, fieldType) => rraf(fieldType));
      return new TS.TRecord(newFields, typ.l, typ.inferred);
    }
    case 't-tuple':
      return new TS.TTuple(typ.elts.map(rraf), typ.l, typ.inferred);
    case 't-forall': {
      let newOnto = typ.introduces.reduceRight((onto, aVar) => onto.substitute(newExistential(aVar.l, false), aVar), typ.onto);
      newOnto = rraf(newOnto);
      return newOnto;
    }
    case 't-ref':
      // The Pyret source calls `t-ref(rraf(ref-typ), inferred)` with the
      // wrong arity (t-ref takes typ, l, inferred), which raises if ever
      // reached; mirror that behavior.
      throw new InternalCompilerError('remove-refinements-and-foralls: t-ref constructor arity error in source');
    case 't-data-refinement':
      return rraf(typ.dataType);
    case 't-var':
      return typ;
    case 't-existential':
      return typ;
    default:
      throw new InternalCompilerError('Unknown Type in remove-refinements-and-foralls');
  }
}

export function generalizeType(currentType: Type, nextType: Type): Type {
  const newVar = (): Type => newExistential(currentType.l, false);
  switch (currentType.$name) {
    case 't-name': {
      if (nextType.$name === 't-name') {
        if (currentType.moduleName.equals(nextType.moduleName) && currentType.id.key() === nextType.id.key()) {
          return currentType;
        } else {
          return newVar();
        }
      }
      return newVar();
    }
    case 't-arrow': {
      if (nextType.$name === 't-arrow') {
        if (currentType.args.length !== nextType.args.length) {
          return newVar();
        } else {
          const newArgs = map2(generalizeType, currentType.args, nextType.args);
          const newRet = generalizeType(currentType.ret, nextType.ret);
          return new TS.TArrow(newArgs, newRet, currentType.l, currentType.inferred);
        }
      }
      return newVar();
    }
    case 't-app': {
      if (nextType.$name === 't-app') {
        if ((currentType.args.length !== nextType.args.length) || !currentType.onto.equals(nextType.onto)) {
          return newVar();
        } else {
          const newArgs = map2(generalizeType, currentType.args, nextType.args);
          return new TS.TApp(currentType.onto, newArgs, currentType.l, currentType.inferred);
        }
      }
      return newVar();
    }
    case 't-top':
      return nextType.$name === 't-top' ? currentType : newVar();
    case 't-bot':
      return nextType.$name === 't-bot' ? currentType : newVar();
    case 't-record': {
      if (nextType.$name === 't-record') {
        const aFields = currentType.fields;
        const bFields = nextType.fields;
        const keysSet = stringSetIntersect(new Set(aFields.keys()), new Set(bFields.keys()));
        const newFields: TS.TypeMembers = new Map();
        for (const key of keysSet) {
          newFields.set(key, generalizeType(mapGetValue(aFields, key), mapGetValue(bFields, key)));
        }
        return new TS.TRecord(newFields, currentType.l, currentType.inferred);
      }
      return newVar();
    }
    case 't-tuple': {
      if (nextType.$name === 't-tuple') {
        if (currentType.elts.length !== nextType.elts.length) {
          return newVar();
        } else {
          const newElts = map2(generalizeType, currentType.elts, nextType.elts);
          return new TS.TTuple(newElts, currentType.l, currentType.inferred);
        }
      }
      return newVar();
    }
    case 't-forall':
      return raise('foralls should have been removed');
    case 't-ref': {
      if (nextType.$name === 't-ref') {
        return new TS.TRef(generalizeType(currentType.typ, nextType.typ), currentType.l, currentType.inferred);
      }
      return newVar();
    }
    case 't-data-refinement':
      return raise('refinements should have been removed');
    case 't-var': {
      if (nextType.$name === 't-var') {
        if (currentType.id.key() === nextType.id.key()) {
          return currentType;
        } else {
          return newVar();
        }
      }
      return newVar();
    }
    case 't-existential':
      return currentType;
    default:
      throw new InternalCompilerError('Unknown Type in generalize-type');
  }
}

export function findCommonStructure(typ: Type, structure: Structure): Structure {
  const newStructure = findStructure(typ);
  const resultStructure: Structure = new Map();
  for (const newKey of newStructure.keys()) {
    const oldSet = structure.get(newKey);
    if (oldSet === undefined) {
      continue;
    }
    const resultSet = mapGetValue(newStructure, newKey);
    resultStructure.set(newKey, pathSetIntersect(resultSet, oldSet));
  }
  return resultStructure;
}

export function findStructure(typ: Type): Structure {
  const flattenedType = flattenTreeWithPaths(typ);

  const gatherSet = (typ2: Type, rest: [Type, Path][]): PathSet => {
    // foldr over the list: process from the end towards the front
    const currentSet: PathSet = new Map();
    for (let i = rest.length - 1; i >= 0; i--) {
      const [restTyp, path] = rest[i];
      if (typ2.equals(restTyp)) {
        currentSet.set(pathToKey(path), path);
      }
    }
    return currentSet;
  };

  // _find-structure builds the dict from the end of the list backwards.
  const structure: Structure = new Map();
  for (let i = flattenedType.length - 1; i >= 0; i--) {
    const [firstType, firstPath] = flattenedType[i];
    const tempSet = gatherSet(firstType, flattenedType);
    const firstSet = new Map(tempSet);
    firstSet.delete(pathToKey(firstPath));
    structure.set(pathToKey(firstPath), firstSet);
  }
  return structure;
}

export function flattenList<X>(xs: X[][]): X[] {
  const out: X[] = [];
  for (const x of xs) {
    out.push(...x);
  }
  return out;
}

export function flattenTreeWithPaths(typ: Type): [Type, Path][] {
  const helper = (typ2: Type, currentPath: Path): [Type, Path][] => {
    switch (typ2.$name) {
      case 't-name':
        return [[typ2, currentPath]];
      case 't-arrow': {
        const argPairs = typ2.args.map((arg, idx) => helper(arg, [...currentPath, new ArgPath(idx)]));
        const retPairs = helper(typ2.ret, [...currentPath, retPath]);
        return [...flattenList(argPairs), ...retPairs, [typ2, currentPath] as [Type, Path]];
      }
      case 't-app': {
        const typePairs = typ2.args.map((arg, idx) => helper(arg, [...currentPath, new AppPath(idx)]));
        // onto-pairs = _flatten-tree-with-paths(onto, current-path.append([list: app-onto-path]))
        return [...flattenList(typePairs), [typ2, currentPath] as [Type, Path]];
      }
      case 't-top':
        return [[typ2, currentPath]];
      case 't-bot':
        return [[typ2, currentPath]];
      case 't-record': {
        const fieldPairs: [Type, Path][] = [];
        for (const fieldName of typ2.fields.keys()) {
          fieldPairs.push(...helper(mapGetValue(typ2.fields, fieldName), [...currentPath, new RecordPath(fieldName)]));
        }
        return [...fieldPairs, [typ2, currentPath] as [Type, Path]];
      }
      case 't-tuple': {
        const eltPairs = typ2.elts.map((elt, idx) => helper(elt, [...currentPath, new TuplePath(idx)]));
        return [...flattenList(eltPairs), [typ2, currentPath] as [Type, Path]];
      }
      case 't-forall':
        return raise('Foralls should have been removed');
      case 't-ref':
        return [...helper(typ2.typ, [...currentPath, refPath]), [typ2, currentPath] as [Type, Path]];
      case 't-data-refinement':
        return raise('Refinements should have been removed');
      case 't-var':
        return [[typ2, currentPath]];
      case 't-existential':
        return [[typ2, currentPath]];
      default:
        throw new InternalCompilerError('Unknown Type in flatten-tree-with-paths');
    }
  };
  return helper(typ, []);
}

export function maintainCommonStructure(struct: Structure, typ: Type): Type {
  // sets all common existentials to the same variable
  // new-paths is a map from path -> existential to place at that path
  const maintainStructure = (typ2: Type, currentPath: Path, newPaths: Map<string, Type>): Type => {
    switch (typ2.$name) {
      case 't-name':
        return typ2;
      case 't-arrow': {
        const newArgs = typ2.args.map((arg, idx) => maintainStructure(arg, [...currentPath, new ArgPath(idx)], newPaths));
        const newRet = maintainStructure(typ2.ret, [...currentPath, retPath], newPaths);
        return new TS.TArrow(newArgs, newRet, typ2.l, typ2.inferred);
      }
      case 't-app': {
        const newArgs = typ2.args.map((arg, idx) => maintainStructure(arg, [...currentPath, new AppPath(idx)], newPaths));
        return new TS.TApp(typ2.onto, newArgs, typ2.l, typ2.inferred);
      }
      case 't-top':
        return typ2;
      case 't-bot':
        return typ2;
      case 't-record': {
        const newFields = typeMemberMap(typ2.fields, (fieldName, fieldType) => {
          return maintainStructure(fieldType, [...currentPath, new RecordPath(fieldName)], newPaths);
        });
        return new TS.TRecord(newFields, typ2.l, typ2.inferred);
      }
      case 't-tuple': {
        const newElts = typ2.elts.map((elt, idx) => maintainStructure(elt, [...currentPath, new TuplePath(idx)], newPaths));
        return new TS.TTuple(newElts, typ2.l, typ2.inferred);
      }
      case 't-forall':
        return raise('Foralls should have been removed');
      case 't-ref':
        return new TS.TRef(maintainStructure(typ2.typ, [...currentPath, refPath], newPaths), typ2.l, typ2.inferred);
      case 't-data-refinement':
        return raise('Refinements should have been removed');
      case 't-var':
        return typ2;
      case 't-existential': {
        const newExists = newPaths.get(pathToKey(currentPath));
        if (newExists !== undefined) {
          return newExists;
        } else {
          const paths = struct.get(pathToKey(currentPath));
          if (paths !== undefined) {
            for (const path of paths.values()) {
              newPaths.set(pathToKey(path), typ2);
            }
            return typ2;
          } else {
            return typ2;
          }
        }
      }
      default:
        throw new InternalCompilerError('Unknown Type in maintain-common-structure');
    }
  };

  const maintained = maintainStructure(typ, [], new Map<string, Type>());
  const existentials = maintained.freeVariables();
  const tempSolution = new ConstraintSolution(existentials, new Map());
  return tempSolution.generalize(maintained);
}

// resolves the type down to either a t-record, an existential, or a data type
// data type shapes: t-name, t-app(t-name), t-data-refinement(data type shape)
export function instantiateObjectType(typ: Type, context: Context): AnyFoldResult<Type> {
  const resolved = resolveAlias(typ, context);
  switch (resolved.$name) {
    case 't-name':
      return new FoldResult<Type>(resolved, context);
    case 't-app': {
      const aArgs = resolved.args;
      const aL = resolved.l;
      const inferred = resolved.inferred;
      const aOnto = resolveAlias(resolved.onto, context);
      switch (aOnto.$name) {
        case 't-name':
          return new FoldResult<Type>(new TS.TApp(aOnto, aArgs, aL, inferred), context);
        case 't-forall': {
          if (aArgs.length !== aOnto.introduces.length) {
            return new FoldErrors<Type>([new C.BadTypeInstantiation(resolved, aOnto.introduces.length)]);
          } else {
            const newOnto = foldr2((onto: Type, arg: Type, typeVar: Type) => {
              return onto.substitute(arg, typeVar);
            }, aOnto.onto, aArgs, aOnto.introduces);
            return new FoldResult<Type>(newOnto, context);
          }
        }
        case 't-app':
          return instantiateObjectType(aOnto.onto, context).bind((tempResult, context2) => {
            void tempResult;
            void context2;
            // The Pyret source calls `instantiate-object-type(t-app(...))`
            // with only one argument here (missing the context), which is an
            // arity error if ever reached; mirror that behavior.
            throw new InternalCompilerError('instantiate-object-type: arity error in source (t-app onto case)');
          });
        case 't-existential':
          // NOTE: the Pyret source returns a typing-error (not fold-errors)
          // here even though this function is in FoldResult position; the
          // value is preserved as-is to keep identical behavior.
          return new TypingError([new C.UnableToInfer(aOnto.l)]) as unknown as AnyFoldResult<Type>;
        default:
          return new FoldErrors<Type>([new C.IncorrectType(aOnto.toString(), aOnto.l, 'a polymorphic type', aL)]);
      }
    }
    case 't-record':
      return new FoldResult<Type>(resolved, context);
    case 't-data-refinement': {
      return instantiateObjectType(resolved.dataType, context).bind((tempResult, context2) => {
        return new FoldResult<Type>(new TS.TDataRefinement(tempResult, resolved.variantName, resolved.l, resolved.inferred), context2);
      });
    }
    case 't-existential':
      return new FoldResult<Type>(resolved, context);
    case 't-forall':
      return instantiateForall(resolved, context).bind((typ2, context2) => {
        return instantiateObjectType(typ2, context2);
      });
    default:
      return new FoldErrors<Type>([new C.IncorrectType(resolved.toString(), resolved.l, 'an object type', resolved.l)]);
  }
}

export function instantiateForall(typ: Type, context: Context): AnyFoldResult<Type> {
  switch (typ.$name) {
    case 't-forall': {
      const introduces = typ.introduces;
      return instantiateForall(typ.onto, context).bind((onto, context2) => {
        const newExistentials = introduces.map((aVar) => newExistential(aVar.l, false));
        const newOnto = foldr2((acc: Type, aVar: Type, aExists: Type) => {
          return acc.substitute(aExists, aVar);
        }, onto, introduces, newExistentials);
        const context3 = context2.addVariableSet(listToTypeSet(newExistentials));
        return new FoldResult<Type>(newOnto, context3);
      });
    }
    default:
      return new FoldResult<Type>(typ, context);
  }
}

export function introduceOnto(appType: TS.TApp, context: Context): AnyFoldResult<Type> {
  const args = appType.args;
  const onto = resolveAlias(appType.onto, context);
  switch (onto.$name) {
    case 't-forall': {
      if (args.length !== onto.introduces.length) {
        return new FoldErrors<Type>([new C.BadTypeInstantiation(appType, onto.introduces.length)]);
      } else {
        const newOnto = foldr2((acc: Type, arg: Type, typeVar: Type) => {
          return acc.substitute(arg, typeVar);
        }, onto.onto, args, onto.introduces);
        return new FoldResult<Type>(newOnto, context);
      }
    }
    case 't-app':
      // The Pyret source matches `t-app(a-onto, a-args, a-l)` with only
      // three binders against the four-field t-app variant, which raises a
      // cases-arity-mismatch if ever reached; mirror that behavior.
      throw new InternalCompilerError('introduce-onto: cases arity mismatch on t-app in source');
    default:
      return new FoldErrors<Type>([new C.BadTypeInstantiation(appType, 0)]);
  }
}

export function instantiateDataType(typ: Type, context: Context): AnyFoldResult<DataType> {
  const helper = (typ2: Type, context2: Context): AnyFoldResult<DataType> => {
    switch (typ2.$name) {
      case 't-name': {
        const dataType = context2.getDataType(typ2);
        if (dataType === undefined) {
          return new FoldErrors<DataType>([new C.CantTypecheck('Expected a data type but got ' + typ2.toString(), typ2.l)]);
        } else {
          return new FoldResult<DataType>(dataType, context2);
        }
      }
      case 't-app': {
        const args = typ2.args;
        const onto = resolveAlias(typ2.onto, context2);
        switch (onto.$name) {
          case 't-name':
            return helper(onto, context2).bind((dataType, context3) => {
              if (dataType.params.length === args.length) {
                const newDataType = foldr2((acc: DataType, arg: Type, typeVar: Type) => {
                  return acc.substitute(arg, typeVar);
                }, dataType, args, dataType.params);
                return new FoldResult<DataType>(new TS.TData(newDataType.name,
                  [],
                  newDataType.variants,
                  newDataType.fields,
                  newDataType.l),
                context3);
              } else {
                return new FoldErrors<DataType>([new C.BadTypeInstantiation(typ2, dataType.params.length)]);
              }
            });
          default:
            return introduceOnto(typ2, context2).bind((typ3, context3) => {
              return instantiateDataType(typ3, context3);
            });
        }
      }
      case 't-data-refinement': {
        return instantiateDataType(typ2.dataType, context2).bind((dataType, context3) => {
          const variant = dataType.getVariantValue(typ2.variantName);
          const newWithFields = new Map(dataType.fields);
          for (const key of variant.withFields.keys()) {
            newWithFields.set(key, mapGetValue(variant.withFields, key));
          }
          const newFields = newWithFields;
          for (const [fieldName, fieldType] of variant.fields) {
            newFields.set(fieldName, fieldType);
          }
          const newDataType = new TS.TData(dataType.name, dataType.params, dataType.variants, newFields, dataType.l);
          return new FoldResult<DataType>(newDataType, context3);
        });
      }
      case 't-forall':
        return instantiateForall(typ2, context2).bind((typ3, context3) => {
          return instantiateDataType(typ3, context3);
        });
      case 't-existential':
        return new FoldErrors<DataType>([new C.UnableToInfer(typ2.l)]);
      default:
        return new FoldErrors<DataType>([new C.CantTypecheck('Expected a data type but got ' + typ2.toString(), typ2.l)]);
    }
  };

  return helper(typ, context).bind((dataType, context2) => {
    if (dataType.params.length === 0) {
      return new FoldResult<DataType>(dataType, context2);
    } else {
      return new FoldErrors<DataType>([new C.CantTypecheck(typ.toString() + ' expected ' + String(dataType.params.length) + ' type arguments, but received none.', typ.l)]);
    }
  });
}

export function emptyContext(): Context {
  return new TypingContext(TD.makeDefaultTypes(),
    TD.makeDefaultAliases(),
    TD.makeDefaultDataExprs(),
    TD.makeDefaultModules(),
    new Map(),
    new Map(),
    noConstraints,
    emptyInfo(),
    new Map());
}

export function emptyInfo(): TCInfo {
  return new TCInfo(new Map(),
    new Map(),
    new Map());
}

export function resolveAlias(t: Type, context: Context): Type {
  switch (t.$name) {
    case 't-name': {
      const aMod = t.moduleName;
      const aId = t.id;
      const l = t.l;
      const inferred = t.inferred;
      switch (aMod.$name) {
        case 'dependency':
          return TS.depError(aMod);
        case 'local': {
          const aliased = context.aliases.get(aId.key());
          if (aliased === undefined) {
            return t;
          } else {
            return resolveAlias(aliased, context).setLoc(l).setInferred(inferred);
          }
        }
        case 'module-uri': {
          const mod = aMod.uri;
          if (mod === 'builtin') {
            const aliased = context.aliases.get(aId.key());
            if (aliased === undefined) {
              return t;
            } else {
              return aliased.setLoc(l).setInferred(inferred);
            }
          } else {
            const modtyp = mapGetValue(context.modules, mod);
            const dataType = modtyp.types.get(aId.toname());
            if (dataType !== undefined) {
              return t;
            } else {
              const aliased = modtyp.aliases.get(aId.toname());
              if (aliased === undefined) {
                return t;
              } else {
                return resolveAlias(aliased, context).setLoc(l).setInferred(inferred);
              }
            }
          }
        }
        default:
          throw new InternalCompilerError('Unknown NameOrigin in resolve-alias');
      }
    }
    case 't-app': {
      const onto = t.onto;
      const args = t.args;
      const l = t.l;
      const inferred = t.inferred;
      if (onto.$name === 't-forall') {
        if (onto.introduces.length === args.length) {
          const reduced = foldr2((curr: Type, param: Type, arg: Type) => {
            return curr.substitute(arg, param);
          }, onto.onto, onto.introduces, args);
          return resolveAlias(reduced, context).setLoc(l).setInferred(inferred);
        } else {
          return new TS.TApp(onto, args, l, inferred);
        }
      } else {
        return new TS.TApp(onto, args, l, inferred);
      }
    }
    default:
      return t;
  }
}

// ---------- data TypingResult ----------

export class TypingResult {
  get $name(): 'typing-result' { return 'typing-result'; }
  constructor(
    public ast: A.Expr,
    public typ: Type,
    public outContext: Context,
  ) {}

  bind(f: (ast: A.Expr, typ: Type, context: Context) => AnyTypingResult): AnyTypingResult {
    return f(this.ast, this.typ, this.outContext);
  }

  foldBind<V>(f: (ast: A.Expr, typ: Type, context: Context) => AnyFoldResult<V>): AnyFoldResult<V> {
    return f(this.ast, this.typ, this.outContext);
  }

  mapExpr(f: (ast: A.Expr) => A.Expr): AnyTypingResult {
    return new TypingResult(f(this.ast), this.typ, this.outContext);
  }

  mapType(f: (typ: Type) => Type): AnyTypingResult {
    return new TypingResult(this.ast, f(this.typ), this.outContext);
  }

  solveBind(): AnyTypingResult {
    return this.outContext.solveLevel().typingBind((solution, context) => {
      let newContext = context.substituteInBinds(solution);

      newContext = newContext.substituteInMisc(solution);

      return new TypingResult(this.ast, solution.apply(this.typ), newContext);
    });
  }
}

export class TypingError {
  get $name(): 'typing-error' { return 'typing-error'; }
  constructor(
    public errors: C.CompileError[],
  ) {}

  bind(f: (ast: A.Expr, typ: Type, context: Context) => AnyTypingResult): AnyTypingResult {
    void f;
    return this;
  }

  foldBind<V>(f: (ast: A.Expr, typ: Type, context: Context) => AnyFoldResult<V>): AnyFoldResult<V> {
    void f;
    return new FoldErrors<V>(this.errors);
  }

  mapExpr(f: (ast: A.Expr) => A.Expr): AnyTypingResult {
    void f;
    return this;
  }

  mapType(f: (typ: Type) => Type): AnyTypingResult {
    void f;
    return this;
  }

  solveBind(): AnyTypingResult {
    return this;
  }
}

export type AnyTypingResult = TypingResult | TypingError;
export function isTypingResult(x: any): x is TypingResult { return x instanceof TypingResult; }
export function isTypingError(x: any): x is TypingError { return x instanceof TypingError; }

// ---------- data FoldResult ----------

export class FoldResult<V> {
  get $name(): 'fold-result' { return 'fold-result'; }
  constructor(
    public v: V,
    public context: Context,
  ) {}

  bind<Z>(f: (v: V, context: Context) => AnyFoldResult<Z>): AnyFoldResult<Z> {
    return f(this.v, this.context);
  }

  typingBind(f: (v: V, context: Context) => AnyTypingResult): AnyTypingResult {
    return f(this.v, this.context);
  }
}

export class FoldErrors<V> {
  get $name(): 'fold-errors' { return 'fold-errors'; }
  constructor(
    public errors: C.CompileError[],
  ) {}

  bind<Z>(f: (v: V, context: Context) => AnyFoldResult<Z>): AnyFoldResult<Z> {
    void f;
    return new FoldErrors<Z>(this.errors);
  }

  typingBind(f: (v: V, context: Context) => AnyTypingResult): AnyTypingResult {
    void f;
    return new TypingError(this.errors);
  }
}

export type AnyFoldResult<V> = FoldResult<V> | FoldErrors<V>;
export function isFoldResult(x: any): x is FoldResult<any> { return x instanceof FoldResult; }
export function isFoldErrors(x: any): x is FoldErrors<any> { return x instanceof FoldErrors; }

// ---------- data Typed ----------

export class Typed {
  get $name(): 'typed' { return 'typed'; }
  constructor(
    public ast: A.Program,
    public info: TCInfo,
  ) {}
}

export function isTyped(x: any): x is Typed { return x instanceof Typed; }

// ---------- Monadic combinators ----------

export function bind(f: (ast: A.Expr, typ: Type, context: Context) => AnyTypingResult, a: AnyTypingResult): AnyTypingResult {
  return a.bind(f);
}

export function typingBind<V>(f: (v: V, context: Context) => AnyTypingResult, a: AnyFoldResult<V>): AnyTypingResult {
  return a.typingBind(f);
}

export function foldBind<V>(f: (ast: A.Expr, typ: Type, context: Context) => AnyFoldResult<V>, a: AnyTypingResult): AnyFoldResult<V> {
  return a.foldBind(f);
}

export function mapFoldResult<X, Y>(f: (x: X, context: Context) => AnyFoldResult<Y>, lst: X[], context: Context): AnyFoldResult<Y[]> {
  // Iterative port of the Pyret recursion; identical call order (left to
  // right) and short-circuit semantics.
  const results: Y[] = [];
  let currentContext = context;
  for (const x of lst) {
    const result = f(x, currentContext);
    if (result.$name === 'fold-errors') {
      return new FoldErrors<Y[]>(result.errors);
    }
    results.push(result.v);
    currentContext = result.context;
  }
  return new FoldResult<Y[]>(results, currentContext);
}

export function foldrFoldResult<X, Y>(f: (x: X, context: Context, acc: Y) => AnyFoldResult<Y>, lst: X[], context: Context, base: Y): AnyFoldResult<Y> {
  // Iterative port of the Pyret recursion; f is applied from the last
  // element towards the first, threading the context through.
  let acc = base;
  let currentContext = context;
  for (let i = lst.length - 1; i >= 0; i--) {
    const result = f(lst[i], currentContext, acc);
    if (result.$name === 'fold-errors') {
      return result;
    }
    acc = result.v;
    currentContext = result.context;
  }
  return new FoldResult<Y>(acc, currentContext);
}

export function foldTyping<X>(f: (x: X, context: Context) => AnyTypingResult, lst: X[], context: Context): AnyFoldResult<A.Expr[]> {
  // Iterative port of the Pyret recursion; identical call order (left to
  // right) and short-circuit semantics.
  const exprs: A.Expr[] = [];
  let currentContext = context;
  for (const x of lst) {
    const result = f(x, currentContext);
    if (result.$name === 'typing-error') {
      return new FoldErrors<A.Expr[]>(result.errors);
    }
    exprs.push(result.ast);
    currentContext = result.outContext;
  }
  return new FoldResult<A.Expr[]>(exprs, currentContext);
}

export function miscTestInference(funExamples: Type[], funName: string): void {
  const examples = funExamples.map((example) => removeRefinementsAndForalls(example));
  if (examples.length > 0) {
    const first = examples[0];
    const rest = examples.slice(1);
    const generalized = rest.reduceRight((acc, typ) => generalizeType(typ, acc), first);
    const firstStructure = findStructure(first);
    const commonStructure = rest.reduceRight((acc, typ) => findCommonStructure(typ, acc), firstStructure);
    const newType = maintainCommonStructure(commonStructure, generalized);

    const logPayload = "{"
      + "'function-name': " + "'" + funName + "'" + ","
      + "'inferred-type': " + "'" + newType.toString() + "'" + ","
      + "}";
    log('extra-test-inferred-type', logPayload);
  }
}
