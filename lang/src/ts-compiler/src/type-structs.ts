/*
  TS port of src/arr/compiler/type-structs.arr.

  Pyret `Set<Type>` (free variables) is ported as `Map<string, Type>` keyed
  by `type.key()`; `StringDict<Type>` (TypeMembers) as `Map<string, Type>`
  with persistent-copy discipline (every "set" builds a fresh Map).
*/

import * as A from './ast';
import { Loc, dummyLoc } from './srcloc';
import { raise, mapGetValue } from './shared';

type Name = A.Name;

export function sdAll<T>(f: (key: string) => boolean, sd: Map<string, T>): boolean {
  let acc = true;
  for (const key of sd.keys()) {
    acc = acc && f(key);
  }
  return acc;
}

export function foldr2<X, Y, R>(f: (acc: R, x: X, y: Y) => R, base: R, l1: X[], l2: Y[]): R {
  const go = (i: number): R => {
    if (i >= l1.length || i >= l2.length) {
      return base;
    }
    return f(go(i + 1), l1[i], l2[i]);
  };
  return go(0);
}

export function interleave<T>(lst: T[], item: T): T[] {
  if (lst.length === 0) { return lst; }
  if (lst.length === 1) { return lst; }
  return [lst[0], item, ...interleave(lst.slice(1), item)];
}

// ---------- ModuleType ----------

export abstract class ModuleTypeBase {
  abstract get $name(): string;
  // Port of `_output` (display only; Pyret renders a vs-constr).
  toString(): string {
    const self = this as unknown as ModuleType;
    const dict = (m: Map<string, unknown>): string =>
      '[string-dict: ' + [...m.entries()].map(([k, v]) => `${JSON.stringify(k)}, ${String(v)}`).join(', ') + ']';
    return `t-module(${JSON.stringify(self.name)}, ${self.provides.toString()}, ${dict(self.types)}, ${dict(self.aliases)})`;
  }
}

export class TModule extends ModuleTypeBase {
  get $name(): 't-module' { return 't-module'; }
  constructor(
    public name: string,
    public provides: Type,
    public types: Map<string, DataType>,
    public aliases: Map<string, Type>,
  ) { super(); }
}

export type ModuleType = TModule;
export function isTModule(x: any): x is TModule { return x instanceof TModule; }

// ---------- TypeMembers ----------

export type TypeMembers = Map<string, Type>;

export function typeMemberMap(members: TypeMembers, f: (key: string, typ: Type) => Type): TypeMembers {
  const newMembers: TypeMembers = new Map();
  for (const key of members.keys()) {
    newMembers.set(key, f(key, mapGetValue(members, key)));
  }
  return newMembers;
}

export function typeMemberOutput(fieldName: string, typ: Type): string {
  return fieldName + ' :: ' + typ.toString();
}

// Pyret `{String; Type}` tuple
export type VariantField = [string, Type];

function variantFieldsToString(fields: VariantField[]): string {
  return '[list: ' + fields.map(([n, t]) => `{${JSON.stringify(n)}; ${t.toString()}}`).join(', ') + ']';
}

export function variantFieldGetValue(fields: VariantField[], name: string): VariantField {
  const result = variantFieldGet(fields, name);
  if (result === undefined) {
    return raise('Could not find field with name ' + name + ' in ' + variantFieldsToString(fields));
  }
  return result;
}

export function variantFieldGet(fields: VariantField[], name: string): VariantField | undefined {
  for (const [fieldName, fieldType] of fields) {
    if (fieldName === name) {
      return [fieldName, fieldType];
    }
  }
  return undefined;
}

// ---------- TypeVariant ----------

export abstract class TypeVariantBase {
  abstract get $name(): string;
  substitute(newType: Type, typeVar: Type): TypeVariant {
    const fieldsSubstitute = (fields: TypeMembers): TypeMembers =>
      typeMemberMap(fields, (_, fieldType) => fieldType.substitute(newType, typeVar));
    const self = this as unknown as TypeVariant;
    switch (self.$name) {
      case 't-variant': {
        const newFields = self.fields.map(([fieldName, typ]): VariantField =>
          [fieldName, typ.substitute(newType, typeVar)]);
        const newWithFields = fieldsSubstitute(self.withFields);
        return new TVariant(self.name, newFields, newWithFields, self.l);
      }
      case 't-singleton-variant': {
        const newWithFields = fieldsSubstitute(self.withFields);
        return new TSingletonVariant(self.name, newWithFields, self.l);
      }
    }
  }
}

export class TVariant extends TypeVariantBase {
  get $name(): 't-variant' { return 't-variant'; }
  constructor(
    public name: string,
    public fields: VariantField[],
    public withFields: TypeMembers,
    public l: Loc,
  ) { super(); }
}

export class TSingletonVariant extends TypeVariantBase {
  get $name(): 't-singleton-variant' { return 't-singleton-variant'; }
  constructor(
    public name: string,
    public withFields: TypeMembers,
    public l: Loc,
  ) { super(); }
  // Pyret `with: fields: empty`
  get fields(): VariantField[] { return []; }
}

export type TypeVariant = TVariant | TSingletonVariant;
export function isTVariant(x: any): x is TVariant { return x instanceof TVariant; }
export function isTSingletonVariant(x: any): x is TSingletonVariant { return x instanceof TSingletonVariant; }

// ---------- NameOrigin ----------

export abstract class NameOriginBase {
  abstract get $name(): string;
  equals(other: NameOrigin): boolean {
    const self = this as unknown as NameOrigin;
    switch (self.$name) {
      case 'local':
        return other.$name === 'local';
      case 'module-uri':
        return other.$name === 'module-uri' && self.uri === other.uri;
      case 'dependency':
        return other.$name === 'dependency' && self.dep === other.dep;
    }
  }
  // torepr-style rendering, used by depError
  toString(): string {
    const self = this as unknown as NameOrigin;
    switch (self.$name) {
      case 'local': return 'local';
      case 'module-uri': return `module-uri(${JSON.stringify(self.uri)})`;
      case 'dependency': return `dependency(${JSON.stringify(self.dep)})`;
    }
  }
}

export class Local extends NameOriginBase {
  get $name(): 'local' { return 'local'; }
}

export class ModuleUri extends NameOriginBase {
  get $name(): 'module-uri' { return 'module-uri'; }
  constructor(public uri: string) { super(); }
}

export class Dependency extends NameOriginBase {
  get $name(): 'dependency' { return 'dependency'; }
  // Dependency in key form
  constructor(public dep: string) { super(); }
}

export type NameOrigin = Local | ModuleUri | Dependency;
export const local: Local = new Local();
export function isLocal(x: any): x is Local { return x instanceof Local; }
export function isModuleUri(x: any): x is ModuleUri { return x instanceof ModuleUri; }
export function isDependency(x: any): x is Dependency { return x instanceof Dependency; }

export function nameComp(no: NameOrigin): string {
  switch (no.$name) {
    case 'local': return '';
    case 'module-uri': return no.uri;
    case 'dependency': return no.dep;
  }
}

export function depError(no: NameOrigin): never {
  return raise('Should not get dependency in typechecker: ' + no.toString());
}

// ---------- DataType ----------

export abstract class DataTypeBase {
  abstract get $name(): string;
  getVariant(variantName: string): TypeVariant | undefined {
    const self = this as unknown as DataType;
    return self.variants.find((tv) => tv.name === variantName);
  }
  getVariantValue(variantName: string): TypeVariant {
    const self = this as unknown as DataType;
    const varType = self.getVariant(variantName);
    if (varType === undefined) {
      return raise('data type ' + self.name + ' did not have variant: ' + variantName);
    }
    return varType;
  }
  substitute(newType: Type, typeVar: Type): DataType {
    const self = this as unknown as DataType;
    return new TData(
      self.name,
      self.params,
      self.variants.map((v) => v.substitute(newType, typeVar)),
      typeMemberMap(self.fields, (_, fieldType) => fieldType.substitute(newType, typeVar)),
      self.l);
  }
}

export class TData extends DataTypeBase {
  get $name(): 't-data' { return 't-data'; }
  constructor(
    public name: string,
    public params: Type[],
    public variants: TypeVariant[],
    public fields: TypeMembers,
    public l: Loc,
  ) { super(); }
}

export type DataType = TData;
export function isTData(x: any): x is TData { return x instanceof TData; }

// ---------- Type ----------

// Pyret Set<Type>, keyed by type.key()
export type TypeSet = Map<string, Type>;

function typeSetUnion(a: TypeSet, b: TypeSet): TypeSet {
  const out: TypeSet = new Map(a);
  for (const [k, v] of b) {
    if (!out.has(k)) { out.set(k, v); }
  }
  return out;
}

function typeListEquals(as_: Type[], bs: Type[]): boolean {
  if (as_.length !== bs.length) { return false; }
  for (let i = 0; i < as_.length; i++) {
    if (!as_[i].equals(bs[i])) { return false; }
  }
  return true;
}

function typeMembersEquals(a: TypeMembers, b: TypeMembers): boolean {
  if (a.size !== b.size) { return false; }
  for (const [k, v] of a) {
    const bv = b.get(k);
    if (bv === undefined || !v.equals(bv)) { return false; }
  }
  return true;
}

export abstract class TypeBase {
  abstract get $name(): string;
  abstract l: Loc;
  abstract inferred: boolean;

  substitute(newType: Type, typeVar: Type): Type {
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name': return self;
      case 't-arrow': {
        const newArgs = self.args.map((a) => a.substitute(newType, typeVar));
        const newRet = self.ret.substitute(newType, typeVar);
        return new TArrow(newArgs, newRet, self.l, self.inferred);
      }
      case 't-app': {
        const newOnto = self.onto.substitute(newType, typeVar);
        const newArgs = self.args.map((a) => a.substitute(newType, typeVar));
        return new TApp(newOnto, newArgs, self.l, self.inferred);
      }
      case 't-top': return self;
      case 't-bot': return self;
      case 't-record': {
        const newFields = typeMemberMap(self.fields, (_, fieldType) => fieldType.substitute(newType, typeVar));
        return new TRecord(newFields, self.l, self.inferred);
      }
      case 't-tuple':
        return new TTuple(self.elts.map((e) => e.substitute(newType, typeVar)), self.l, self.inferred);
      case 't-forall': {
        // doesn't need to be capture avoiding thanks to resolve-names
        const newOnto = self.onto.substitute(newType, typeVar);
        return new TForall(self.introduces, newOnto, self.l, self.inferred);
      }
      case 't-ref':
        return new TRef(self.typ.substitute(newType, typeVar), self.l, self.inferred);
      case 't-data-refinement':
        return new TDataRefinement(
          self.dataType.substitute(newType, typeVar),
          self.variantName,
          self.l,
          self.inferred);
      case 't-var': {
        if (typeVar.$name === 't-var') {
          if (self.id.key() === typeVar.id.key()) {
            return newType.setLoc(self.l);
          } else {
            return self;
          }
        } else {
          return self;
        }
      }
      case 't-existential': {
        if (typeVar.$name === 't-existential') {
          if (self.id.key() === typeVar.id.key()) {
            // inferred existentials keep their locations
            // this is along the lines of inferred argument types etc
            // uninferred existentials are used to equate different pieces of code
            // they should not keep their location
            if (self.inferred) {
              return newType.setLoc(self.l);
            } else {
              return newType;
            }
          } else {
            return self;
          }
        } else {
          return self;
        }
      }
    }
  }

  freeVariables(): TypeSet {
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name':
        return new Map();
      case 't-arrow': {
        let free = self.ret.freeVariables();
        for (const arg of self.args) {
          free = typeSetUnion(free, arg.freeVariables());
        }
        return free;
      }
      case 't-app': {
        let free = self.onto.freeVariables();
        for (const arg of self.args) {
          free = typeSetUnion(free, arg.freeVariables());
        }
        return free;
      }
      case 't-top':
        return new Map();
      case 't-bot':
        return new Map();
      case 't-record': {
        let free: TypeSet = new Map();
        for (const key of self.fields.keys()) {
          free = typeSetUnion(free, mapGetValue(self.fields, key).freeVariables());
        }
        return free;
      }
      case 't-tuple': {
        let free: TypeSet = new Map();
        for (const elt of self.elts) {
          free = typeSetUnion(free, elt.freeVariables());
        }
        return free;
      }
      case 't-forall':
        return self.onto.freeVariables();
      case 't-ref':
        return self.typ.freeVariables();
      case 't-data-refinement':
        return self.dataType.freeVariables();
      case 't-var':
        return new Map();
      case 't-existential':
        return new Map([[self.key(), self as Type]]);
    }
  }

  hasVariableFree(varType: Type): boolean {
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name':
        return true;
      case 't-arrow':
        return self.args.every((a) => a.hasVariableFree(varType)) &&
          self.ret.hasVariableFree(varType);
      case 't-app':
        return self.onto.hasVariableFree(varType) &&
          self.args.every((a) => a.hasVariableFree(varType));
      case 't-top':
        return true;
      case 't-bot':
        return true;
      case 't-record':
        return sdAll((key) => mapGetValue(self.fields, key).hasVariableFree(varType), self.fields);
      case 't-tuple':
        return self.elts.every((e) => e.hasVariableFree(varType));
      case 't-forall':
        // TODO(MATT): can we really ignore the introduces?
        return self.onto.hasVariableFree(varType);
      case 't-ref':
        return self.typ.hasVariableFree(varType);
      case 't-data-refinement':
        return self.dataType.hasVariableFree(varType);
      case 't-var': {
        if (varType.$name === 't-var') {
          return self.id.key() === varType.id.key() ? false : true;
        }
        return true;
      }
      case 't-existential': {
        if (varType.$name === 't-existential') {
          return self.id.key() === varType.id.key() ? false : true;
        }
        return true;
      }
    }
  }

  key(): string {
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name': {
        const moduleName = self.moduleName;
        switch (moduleName.$name) {
          case 'local': return self.id.key();
          case 'module-uri': return moduleName.uri + '.' + self.id.key();
          case 'dependency': return depError(moduleName);
        }
      }
      case 't-arrow':
        return '('
          + self.args.map((a) => a.key()).join(', ')
          + ' -> ' + self.ret.key() + ')';
      case 't-app':
        return self.onto.key() + '<' + self.args.map((a) => a.key()).join(', ') + '>';
      case 't-top':
        return 'Any';
      case 't-bot':
        return 'Bot';
      case 't-record':
        // A record type's identity is a field *set*; field order is not part of
        // it (equals() at typeMembersEquals is order-independent). Pyret iterates
        // a StringDict here in content-deterministic hash order, so equal records
        // built in different field orders get the same key(). A JS Map iterates in
        // insertion order, which would break that invariant and let a key()-keyed
        // TypeSet hold duplicates; sort so key() stays a function of contents.
        return '{' + [...self.fields.keys()].sort().map((key) => key + ' :: ' + mapGetValue(self.fields, key).key()).join(', ') + '}';
      case 't-tuple':
        return '{'
          + self.elts.map((elt) => elt.key()).join('; ')
          + '}';
      case 't-forall':
        return '<' + self.introduces.map((i) => i.key()).join(', ') + '>'
          + self.onto.key();
      case 't-ref':
        return 'ref ' + self.typ.key();
      case 't-data-refinement':
        return '('
          + self.dataType.key()
          + ' %is-' + self.variantName
          + ')';
      case 't-var':
        return self.id.key();
      case 't-existential':
        return self.id.key();
    }
  }

  setInferred(inferred: boolean): Type {
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name':
        return new TName(self.moduleName, self.id, self.l, inferred);
      case 't-arrow':
        return new TArrow(self.args, self.ret, self.l, inferred);
      case 't-app':
        return new TApp(self.onto, self.args, self.l, inferred);
      case 't-top':
        return new TTop(self.l, inferred);
      case 't-bot':
        return new TBot(self.l, inferred);
      case 't-record':
        return new TRecord(self.fields, self.l, inferred);
      case 't-tuple':
        return new TTuple(self.elts, self.l, inferred);
      case 't-forall':
        return new TForall(self.introduces, self.onto, self.l, inferred);
      case 't-ref':
        return new TRef(self.typ, self.l, inferred);
      case 't-data-refinement':
        return new TDataRefinement(self.dataType, self.variantName, self.l, inferred);
      case 't-var':
        return new TVar(self.id, self.l, inferred);
      case 't-existential':
        return new TExistential(self.id, self.l, inferred);
    }
  }

  setLoc(loc: Loc): Type {
    const sl = (t: Type): Type => t.setLoc(loc);
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name':
        return new TName(self.moduleName, self.id, loc, self.inferred);
      case 't-arrow':
        return new TArrow(self.args.map(sl), sl(self.ret), loc, self.inferred);
      case 't-app':
        return new TApp(sl(self.onto), self.args.map(sl), loc, self.inferred);
      case 't-top':
        return new TTop(loc, self.inferred);
      case 't-bot':
        return new TBot(loc, self.inferred);
      case 't-record':
        return new TRecord(typeMemberMap(self.fields, (_, fieldType) => sl(fieldType)), loc, self.inferred);
      case 't-tuple':
        return new TTuple(self.elts.map(sl), loc, self.inferred);
      case 't-forall':
        return new TForall(self.introduces.map(sl), sl(self.onto), loc, self.inferred);
      case 't-ref':
        return new TRef(sl(self.typ), loc, self.inferred);
      case 't-data-refinement':
        return new TDataRefinement(sl(self.dataType), self.variantName, loc, self.inferred);
      case 't-var':
        return new TVar(self.id, loc, self.inferred);
      case 't-existential':
        return new TExistential(self.id, loc, self.inferred);
    }
  }

  // Port of `_equals`; collapses Pyret's EqualityResult to boolean.
  equals(other: Type): boolean {
    const self = this as unknown as Type;
    switch (self.$name) {
      case 't-name': {
        if (other.$name === 't-name') {
          if (!self.moduleName.equals(other.moduleName)) { return false; }
          if (self.id.key() !== other.id.key()) { return false; }
          return true;
        }
        return false;
      }
      case 't-arrow': {
        if (other.$name === 't-arrow') {
          if (!typeListEquals(self.args, other.args)) { return false; }
          if (!self.ret.equals(other.ret)) { return false; }
          return true;
        }
        return false;
      }
      case 't-app': {
        if (other.$name === 't-app') {
          if (!self.onto.equals(other.onto)) { return false; }
          if (!typeListEquals(self.args, other.args)) { return false; }
          return true;
        }
        return false;
      }
      case 't-top':
        return other.$name === 't-top';
      case 't-bot':
        return other.$name === 't-bot';
      case 't-record': {
        if (other.$name === 't-record') {
          return typeMembersEquals(self.fields, other.fields);
        }
        return false;
      }
      case 't-tuple': {
        if (other.$name === 't-tuple') {
          return typeListEquals(self.elts, other.elts);
        }
        return false;
      }
      case 't-forall': {
        if (other.$name === 't-forall') {
          if (self.introduces.length === other.introduces.length) {
            const bOnto = foldr2((acc: Type, aVar: Type, bVar: Type) =>
              acc.substitute(aVar, bVar), other.onto, self.introduces, other.introduces);
            return self.onto.equals(bOnto);
          }
          return false;
        }
        return false;
      }
      case 't-ref': {
        if (other.$name === 't-ref') {
          return self.typ.equals(other.typ);
        }
        return false;
      }
      case 't-data-refinement': {
        if (other.$name === 't-data-refinement') {
          if (!self.dataType.equals(other.dataType)) { return false; }
          if (self.variantName !== other.variantName) { return false; }
          return true;
        }
        return false;
      }
      case 't-var': {
        if (other.$name === 't-var') {
          return self.id.key() === other.id.key();
        }
        return false;
      }
      case 't-existential': {
        if (other.$name === 't-existential') {
          return self.id.key() === other.id.key();
        }
        return false;
      }
    }
  }

  lessthan(other: Type): boolean {
    return this.key() < (other as TypeBase).key();
  }

  // Port of `to-string` (also serves `_output`, which was vs-str(self.to-string())).
  toString(): string {
    let currentLetter = 'A';
    const helper = (typ: Type, freeVarsMapping: Map<string, string>, tyvarMapping: Map<string, string>): string => {
      const h = (t: Type): string => helper(t, freeVarsMapping, tyvarMapping);
      switch (typ.$name) {
        case 't-name':
          return typ.id.toname();
        case 't-arrow':
          return '('
            + typ.args.map(h).join(', ')
            + ' -> ' + h(typ.ret)
            + ')';
        case 't-app':
          return h(typ.onto) + '<'
            + typ.args.map(h).join(', ')
            + '>';
        case 't-top':
          return 'Any';
        case 't-bot':
          return 'Bot';
        case 't-record':
          return '{'
            + [...typ.fields.keys()].map((key) => typeMemberOutput(key, mapGetValue(typ.fields, key))).join(', ')
            + '}';
        case 't-tuple':
          return '{'
            + typ.elts.map(h).join('; ')
            + '}';
        case 't-forall':
          return 'forall '
            + typ.introduces.map(h).join(', ')
            + ' . ' + h(typ.onto);
        case 't-ref':
          return 'ref ' + h(typ.typ);
        case 't-data-refinement':
          return '('
            + h(typ.dataType)
            + ' % is-' + typ.variantName
            + ')';
        case 't-var': {
          const id = typ.id;
          if (id.$name === 's-atom') {
            if (id.base === '%tyvar') {
              const existing = tyvarMapping.get(typ.key());
              if (existing !== undefined) {
                return existing;
              } else {
                const letter = currentLetter;
                tyvarMapping.set(typ.key(), currentLetter);
                currentLetter = String.fromCodePoint(letter.codePointAt(0)! + 1);
                return letter;
              }
            } else {
              return id.toname();
            }
          } else {
            return id.toname();
          }
        }
        case 't-existential':
          return '?-' + mapGetValue(freeVarsMapping, typ.key());
      }
    };
    const self = this as unknown as Type;
    const freeVarsList = [...self.freeVariables().values()];
    const freeVarsMapping = new Map<string, string>();
    freeVarsList.forEach((freeVar, i) => {
      freeVarsMapping.set(freeVar.key(), String(i + 1));
    });
    return helper(self, freeVarsMapping, new Map<string, string>());
  }
}

export class TName extends TypeBase {
  get $name(): 't-name' { return 't-name'; }
  constructor(
    public moduleName: NameOrigin,
    public id: Name,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TArrow extends TypeBase {
  get $name(): 't-arrow' { return 't-arrow'; }
  constructor(
    public args: Type[],
    public ret: Type,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TApp extends TypeBase {
  get $name(): 't-app' { return 't-app'; }
  constructor(
    public onto: Type,
    public args: Type[],
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TTop extends TypeBase {
  get $name(): 't-top' { return 't-top'; }
  constructor(
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TBot extends TypeBase {
  get $name(): 't-bot' { return 't-bot'; }
  constructor(
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TRecord extends TypeBase {
  get $name(): 't-record' { return 't-record'; }
  constructor(
    public fields: TypeMembers,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TTuple extends TypeBase {
  get $name(): 't-tuple' { return 't-tuple'; }
  constructor(
    public elts: Type[],
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TForall extends TypeBase {
  get $name(): 't-forall' { return 't-forall'; }
  constructor(
    public introduces: Type[],
    public onto: Type,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TRef extends TypeBase {
  get $name(): 't-ref' { return 't-ref'; }
  constructor(
    public typ: Type,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TDataRefinement extends TypeBase {
  get $name(): 't-data-refinement' { return 't-data-refinement'; }
  constructor(
    public dataType: Type,
    public variantName: string,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TVar extends TypeBase {
  get $name(): 't-var' { return 't-var'; }
  constructor(
    public id: Name,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export class TExistential extends TypeBase {
  get $name(): 't-existential' { return 't-existential'; }
  constructor(
    public id: Name,
    public l: Loc,
    public inferred: boolean,
  ) { super(); }
}

export type Type =
  | TName
  | TArrow
  | TApp
  | TTop
  | TBot
  | TRecord
  | TTuple
  | TForall
  | TRef
  | TDataRefinement
  | TVar
  | TExistential;

export function isTName(x: any): x is TName { return x instanceof TName; }
export function isTArrow(x: any): x is TArrow { return x instanceof TArrow; }
export function isTApp(x: any): x is TApp { return x instanceof TApp; }
export function isTTop(x: any): x is TTop { return x instanceof TTop; }
export function isTBot(x: any): x is TBot { return x instanceof TBot; }
export function isTRecord(x: any): x is TRecord { return x instanceof TRecord; }
export function isTTuple(x: any): x is TTuple { return x instanceof TTuple; }
export function isTForall(x: any): x is TForall { return x instanceof TForall; }
export function isTRef(x: any): x is TRef { return x instanceof TRef; }
export function isTDataRefinement(x: any): x is TDataRefinement { return x instanceof TDataRefinement; }
export function isTVar(x: any): x is TVar { return x instanceof TVar; }
export function isTExistential(x: any): x is TExistential { return x instanceof TExistential; }

// ---------- Helper constructors and constants ----------

export function newExistential(l: Loc, inferred: boolean): TExistential {
  return new TExistential(A.globalNames.makeAtom('%exists'), l, inferred);
}

export function newTypeVar(l: Loc): TVar {
  return new TVar(A.globalNames.makeAtom('%tyvar'), l, false);
}

// TODO(MATT): which of these should be kept
export const builtinUri: ModuleUri = new ModuleUri('builtin://global');

export const tArrayName: Type = new TName(builtinUri, new A.STypeGlobal('RawArray'), dummyLoc, false);

export const tNumber = (l: Loc): Type => new TName(builtinUri, new A.STypeGlobal('Number'), l, false);
export const tString = (l: Loc): Type => new TName(builtinUri, new A.STypeGlobal('String'), l, false);
export const tBoolean = (l: Loc): Type => new TName(builtinUri, new A.STypeGlobal('Boolean'), l, false);
export const tNothing = (l: Loc): Type => new TName(builtinUri, new A.STypeGlobal('Nothing'), l, false);
export const tSrcloc = (l: Loc): Type => new TName(builtinUri, new A.STypeGlobal('Loc'), l, false);
export const tArray = (v: Type, l: Loc): Type => new TApp(tArrayName.setLoc(l), [v], l, false);
export const tOption = (v: Type, l: Loc): Type =>
  new TApp(new TName(new ModuleUri('builtin://option'), new A.STypeGlobal('Option'), l, false), [v], l, false);
export const tTable = (l: Loc): Type => new TName(builtinUri, new A.STypeGlobal('Table'), l, false);
