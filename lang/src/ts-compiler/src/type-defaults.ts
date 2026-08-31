/*
  TS port of src/arr/compiler/type-defaults.arr.

  The big string-dicts mapping builtin names to their types: default types
  for globals, default data exprs, and the hard-coded builtin modules.
*/

import * as A from './ast';
import * as TS from './type-structs';
import { mapSet } from './shared';

type Type = TS.Type;
type DataType = TS.DataType;
type ModuleType = TS.ModuleType;
type TypeVariant = TS.TypeVariant;

const moduleUri = (uri: string): TS.ModuleUri => new TS.ModuleUri(uri);
const local = TS.local;

// Constructors with the location fixed to A.dummy-loc, as in the Pyret
// original (`t-name = TS.t-name(_, _, A.dummy-loc, false)` etc.).
const tName = (moduleName: TS.NameOrigin, id: A.Name): Type => new TS.TName(moduleName, id, A.dummyLoc, false);
const tVar = (id: A.Name): TS.TVar => new TS.TVar(id, A.dummyLoc, false);
const tArrow = (args: Type[], ret: Type): Type => new TS.TArrow(args, ret, A.dummyLoc, false);
const tTop: Type = new TS.TTop(A.dummyLoc, false);
const tBot: Type = new TS.TBot(A.dummyLoc, false);
const tApp = (onto: Type, args: Type[]): Type => new TS.TApp(onto, args, A.dummyLoc, false);
const tRecord = (fields: TS.TypeMembers): Type => new TS.TRecord(fields, A.dummyLoc, false);
const tForall = (introduces: Type[], onto: Type): Type => new TS.TForall(introduces, onto, A.dummyLoc, false);
const tDataRefinement = (dataType: Type, variantName: string): Type => new TS.TDataRefinement(dataType, variantName, A.dummyLoc, false);
const tTuple = (elts: Type[]): Type => new TS.TTuple(elts, A.dummyLoc, false);

const tData = (name: string, params: Type[], variants: TypeVariant[], fields: TS.TypeMembers): DataType =>
  new TS.TData(name, params, variants, fields, A.dummyLoc);

const tNumber: Type = TS.tNumber(A.dummyLoc);
const tString: Type = TS.tString(A.dummyLoc);
const tBoolean: Type = TS.tBoolean(A.dummyLoc);
const tArray = (v: Type): Type => TS.tArray(v, A.dummyLoc);
const tNothing: Type = TS.tNothing(A.dummyLoc);
const tSrcloc: Type = TS.tSrcloc(A.dummyLoc);
const tTable: Type = TS.tTable(A.dummyLoc);

const tModule = (name: string, provides: Type, types: Map<string, DataType>, aliases: Map<string, Type>): ModuleType =>
  new TS.TModule(name, provides, types, aliases);

const tVariant = (name: string, fields: TS.VariantField[], withFields: TS.TypeMembers): TypeVariant =>
  new TS.TVariant(name, fields, withFields, A.dummyLoc);
const tSingletonVariant = (name: string, withFields: TS.TypeMembers): TypeVariant =>
  new TS.TSingletonVariant(name, withFields, A.dummyLoc);

// [string-dict: ...] literal helper (insertion order preserved).
function dict<T>(...entries: [string, T][]): Map<string, T> {
  return new Map(entries);
}

export const tva = tVar(A.globalNames.makeAtom('A'));
export const tvb = tVar(A.globalNames.makeAtom('B'));
export const tvc = tVar(A.globalNames.makeAtom('C'));
export const tvd = tVar(A.globalNames.makeAtom('D'));
export const tve = tVar(A.globalNames.makeAtom('E'));
export const tvf = tVar(A.globalNames.makeAtom('F'));
export const tvg = tVar(A.globalNames.makeAtom('G'));
export const tvh = tVar(A.globalNames.makeAtom('H'));

export const tImage = tName(moduleUri('builtin://image-lib'), new A.STypeGlobal('Image'));
export const tOption = tName(moduleUri('builtin://option'), new A.STypeGlobal('Option'));
export const tReactor = tName(moduleUri('builtin://reactors'), new A.STypeGlobal('Reactor'));
export const tEqualityResult = tName(moduleUri('builtin://equality'), new A.STypeGlobal('EqualityResult'));
export const tValueSkeleton = tName(moduleUri('builtin://valueskeleton'), new A.STypeGlobal('ValueSkeleton'));
export const tList = tName(moduleUri('builtin://lists'), new A.STypeGlobal('List'));
export const tBigArray = tName(moduleUri('builtin://arrays'), new A.STypeGlobal('Array'));
export const tSet = tName(moduleUri('builtin://sets'), new A.STypeGlobal('Set'));
export const tAvl = tName(moduleUri('builtin://sets'), new A.STypeGlobal('AVLTree'));
export const tRuntimeError = tName(moduleUri('builtin://error'), new A.STypeGlobal('RuntimeError'));
export const tParseError = tName(moduleUri('builtin://error'), new A.STypeGlobal('ParseError'));
export const tEither = tName(moduleUri('builtin://either'), new A.STypeGlobal('Either'));
export const tSExp = tName(moduleUri('builtin://s-exp-structs'), new A.STypeGlobal('S-Exp'));
export const tPick = tName(moduleUri('builtin://pick'), new A.STypeGlobal('Pick'));
export const tJson = tName(moduleUri('builtin://json-structs'), new A.STypeGlobal('JSON'));
export const tStringDict = tName(moduleUri('builtin://string-dict'), new A.STypeGlobal('StringDict'));

export function tEitherApp(typ1: Type, typ2: Type): Type {
  return tApp(tEither, [typ1, typ2]);
}

export const tOptionApp = (param: Type): Type => tApp(tOption, [param]);

export const tReactorApp = (param: Type): Type => tApp(tReactor, [param]);

export function tListApp(a: Type): Type {
  return tApp(tList, [a]);
}

export function tBigArrayApp(typ: Type): Type {
  return tApp(tBigArray, [typ]);
}

export function tSetApp(typ: Type): Type {
  return tApp(tSet, [typ]);
}

export function tPickApp(typ1: Type, typ2: Type): Type {
  return tApp(tPick, [typ1, typ2]);
}

export function tStringDictApp(typ: Type): Type {
  return tApp(tStringDict, [typ]);
}

export const tOutput: Type = tArrow([], tValueSkeleton);
export const tNumberBinop: Type = tArrow([tNumber, tNumber], tNumber);

export function makeDefaultAliases(): Map<string, Type> {
  const defaultAliases = new Map<string, Type>();
  return defaultAliases;
}

export function makeDefaultTypes(): Map<string, Type> {
  const defaultTyps = new Map<string, Type>();

  // Need to be fixed to correct type:
  defaultTyps.set('makeSome', tForall([tva], tArrow([tva], tOptionApp(tva))));
  defaultTyps.set('makeNone', tForall([tva], tArrow([], tOptionApp(tva))));
  defaultTyps.set('checkWrapBoolean', tArrow([tBoolean], tBoolean));
  defaultTyps.set('checkTupleBind', tArrow([tTop, tNumber, tSrcloc], tBot));
  defaultTyps.set('throwNonBooleanCondition', tArrow([tSrcloc, tString, tTop], tBot));
  defaultTyps.set('throwNoBranchesMatched', tArrow([tSrcloc, tString], tBot));
  defaultTyps.set('throwUnfinishedTemplate', tArrow([tSrcloc], tBot));
  defaultTyps.set('makeReactor', tForall([tva], tArrow([
    tva,
    tRecord(dict<Type>(
      ['on-tick', tOptionApp(tArrow([tva], tva))],
      ['on-mouse', tOptionApp(tArrow([tva, tNumber, tNumber, tString], tva))],
      ['on-key', tOptionApp(tArrow([tva, tString], tva))],
      ['to-draw', tOptionApp(tArrow([tva], tImage))],
      ['stop-when', tOptionApp(tArrow([tva], tBoolean))],
      ['seconds-per-tick', tOptionApp(tNumber)],
      ['close-when-stop', tOptionApp(tBoolean)],
      ['title', tOptionApp(tString)]))],
  tReactorApp(tva))));
  defaultTyps.set('hasField', tArrow([tRecord(dict<Type>()), tString], tBoolean));
  defaultTyps.set('makeSrcloc', tArrow([tSrcloc], tBot));

  defaultTyps.set('not', tArrow([tBoolean], tBoolean));
  defaultTyps.set('roughly-equal-always', tArrow([tTop, tTop], tBoolean));
  defaultTyps.set('roughly-equal-now', tArrow([tTop, tTop], tBoolean));
  defaultTyps.set('roughly-equal', tArrow([tTop, tTop], tBoolean));
  defaultTyps.set('equal-always', tArrow([tTop, tTop], tBoolean));
  defaultTyps.set('equal-now', tArrow([tTop, tTop], tBoolean));
  defaultTyps.set('identical', tArrow([tTop, tTop], tBoolean));
  defaultTyps.set('roughly-equal-always3', tArrow([tTop, tTop], tEqualityResult));
  defaultTyps.set('roughly-equal-now3', tArrow([tTop, tTop], tEqualityResult));
  defaultTyps.set('equal-always3', tArrow([tTop, tTop], tEqualityResult));
  defaultTyps.set('equal-now3', tArrow([tTop, tTop], tEqualityResult));
  defaultTyps.set('identical3', tArrow([tTop, tTop], tEqualityResult));

  defaultTyps.set('getMaker', tForall([tva, tvb], tArrow([tRecord(dict<Type>(['make', tArrow([tArray(tvb)], tva)])), tString, tSrcloc, tSrcloc], tArrow([tArray(tvb)], tva))));
  defaultTyps.set('getLazyMaker', tForall([tva, tvb], tArrow([tRecord(dict<Type>(['lazy-make', tArrow([tArray(tArrow([], tvb))], tva)])), tString, tSrcloc, tSrcloc], tArrow([tArray(tArrow([], tvb))], tva))));
  defaultTyps.set('getMaker0', tForall([tva], tArrow([tRecord(dict<Type>(['make0', tArrow([], tva)])), tString, tSrcloc, tSrcloc], tArrow([], tva))));
  defaultTyps.set('getMaker1', tForall([tva, tvb], tArrow([tRecord(dict<Type>(['make1', tArrow([tvb], tva)])), tString, tSrcloc, tSrcloc], tArrow([tvb], tva))));
  defaultTyps.set('getMaker2', tForall([tva, tvb, tvc], tArrow([tRecord(dict<Type>(['make2', tArrow([tvb, tvc], tva)])), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc], tva))));
  defaultTyps.set('getMaker3', tForall([tva, tvb, tvc, tvd], tArrow([tRecord(dict<Type>(['make3', tArrow([tvb, tvc, tvd], tva)])), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc, tvd], tva))));
  defaultTyps.set('getMaker4', tForall([tva, tvb, tvc, tvd, tve], tArrow([tRecord(dict<Type>(['make4', tArrow([tvb, tvc, tvd, tve], tva)])), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc, tvd, tve], tva))));
  defaultTyps.set('getMaker5', tForall([tva, tvb, tvc, tvd, tve, tvf], tArrow([tRecord(dict<Type>(['make5', tArrow([tvb, tvc, tvd, tve, tvf], tva)])), tString, tSrcloc, tSrcloc], tArrow([tvb, tvc, tvd, tve, tvf], tva))));

  defaultTyps.set('makeTable', tArrow([tArray(tTop), tArray(tArray(tTop))], tTable));

  return defaultTyps;
}

export function makeDefaultDataExprs(): Map<string, DataType> {
  const defaultDataExprs = new Map<string, DataType>();
  return defaultDataExprs;
}

// Functions for adding hard-coded modules
export const moduleConstEquality = tModule('builtin://equality',
  tRecord(dict<Type>(
    ['EqualityResult', tArrow([tTop], tBoolean)],
    ['is-EqualityResult', tArrow([tTop], tBoolean)],
    ['Equal', tDataRefinement(tEqualityResult, 'Equal')],
    ['is-Equal', tArrow([tTop], tBoolean)],
    ['NotEqual', tArrow([tString, tTop, tTop], tDataRefinement(tEqualityResult, 'NotEqual'))],
    ['is-NotEqual', tArrow([tTop], tBoolean)],
    ['Unknown', tArrow([tString, tTop, tTop], tDataRefinement(tEqualityResult, 'Unknown'))],
    ['is-Unknown', tArrow([tTop], tBoolean)],
    ['equal-and', tArrow([tEqualityResult, tEqualityResult], tEqualityResult)],
    ['equal-or', tArrow([tEqualityResult, tEqualityResult], tEqualityResult)],
    ['to-boolean', tArrow([tEqualityResult], tBoolean)])),
  dict<DataType>(
    ['EqualityResult', tData(
      'EqualityResult',
      [],
      [
        tSingletonVariant('Equal', dict<Type>()),
        tVariant('NotEqual', [['reason', tString], ['value1', tTop], ['value2', tTop]], dict<Type>()),
        tVariant('Unknown', [['reason', tString], ['value1', tTop], ['value2', tTop]], dict<Type>())],
      dict<Type>())]),
  dict<Type>(
    ['EqualityResult', tEqualityResult]));

export const moduleConstArrays = tModule('builtin://arrays',
  tRecord(dict<Type>(
    ['array', tRecord(dict<Type>(
      ['make', tForall([tva], tArrow([tArray(tva)], tBigArrayApp(tva)))],
      ['make0', tForall([tva], tArrow([], tBigArrayApp(tva)))],
      ['make1', tForall([tva], tArrow([tva], tBigArrayApp(tva)))],
      ['make2', tForall([tva], tArrow([tva, tva], tBigArrayApp(tva)))],
      ['make3', tForall([tva], tArrow([tva, tva, tva], tBigArrayApp(tva)))],
      ['make4', tForall([tva], tArrow([tva, tva, tva, tva], tBigArrayApp(tva)))],
      ['make5', tForall([tva], tArrow([tva, tva, tva, tva, tva], tBigArrayApp(tva)))]))],
    ['build-array', tForall([tva], tArrow([tArrow([tNumber], tva), tNumber], tBigArrayApp(tva)))],
    ['array-from-list', tForall([tva], tArrow([tListApp(tva)], tBigArrayApp(tva)))],
    ['is-array', tForall([tva], tArrow([tTop], tBoolean))],
    ['array-of', tForall([tva], tArrow([tva, tNumber], tBigArrayApp(tva)))],
    ['array-set-now', tForall([tva], tArrow([tBigArrayApp(tva), tNumber, tva], tNothing))],
    ['array-get-now', tForall([tva], tArrow([tBigArrayApp(tva), tNumber], tva))],
    ['array-length', tForall([tva], tArrow([tBigArrayApp(tva)], tNumber))],
    ['array-to-list-now', tForall([tva], tArrow([tBigArrayApp(tva)], tListApp(tva)))])),
  dict<DataType>(
    ['Array', tData(
      'Array',
      [tva],
      [],
      dict<Type>(
        ['get-now', tArrow([tNumber], tva)],
        ['set-now', tArrow([tNumber, tva], tNothing)],
        ['to-list-now', tArrow([], tListApp(tva))],
        ['length', tArrow([], tNumber)],
        ['_output', tOutput]))]),
  dict<Type>(
    ['Array', tBigArray],
    ['List', tList]));

export const moduleConstPick = tModule('builtin://pick',
  tRecord(dict<Type>(
    ['Pick', tArrow([tTop], tBoolean)],
    ['is-Pick', tArrow([tTop], tBoolean)],
    ['pick-none', tForall([tva, tvb], tDataRefinement(tPickApp(tva, tvb), 'pick-none'))],
    ['is-pick-none', tArrow([tTop], tBoolean)],
    ['pick-some', tForall([tva, tvb], tArrow([tva, tvb], tDataRefinement(tPickApp(tva, tvb), 'pick-some')))],
    ['is-pick-some', tArrow([tTop], tBoolean)])),
  dict<DataType>(
    ['Pick', tData(
      'Pick',
      [tva, tvb],
      [
        tSingletonVariant('pick-none',
          dict<Type>(
            ['_match', tTop])),
        tVariant('pick-some',
          [['elt', tva], ['rest', tvb]],
          dict<Type>(
            ['_match', tTop]))
      ],
      dict<Type>(
        ['_match', tTop]))]),
  dict<Type>(
    ['Pick', tName(moduleUri('builtin://pick'), new A.STypeGlobal('Pick'))]));

export const setConstructor: Type =
  tRecord(dict<Type>(
    ['make', tForall([tva], tArrow([tArray(tva)], tSetApp(tva)))],
    ['make0', tForall([tva], tArrow([], tSetApp(tva)))],
    ['make1', tForall([tva], tArrow([tva], tSetApp(tva)))],
    ['make2', tForall([tva], tArrow([tva, tva], tSetApp(tva)))],
    ['make3', tForall([tva], tArrow([tva, tva, tva], tSetApp(tva)))],
    ['make4', tForall([tva], tArrow([tva, tva, tva, tva], tSetApp(tva)))],
    ['make5', tForall([tva], tArrow([tva, tva, tva, tva, tva], tSetApp(tva)))]));

export const tEmptySet: Type = tForall([tva], tSetApp(tva));

export const tListToSet: Type = tForall([tva], tArrow([tListApp(tva)], tSetApp(tva)));

export const moduleConstSets = tModule('builtin://sets',
  tRecord(dict<Type>(
    ['set', setConstructor],
    ['list-set', setConstructor],
    ['tree-set', setConstructor],
    ['empty-set', tEmptySet],
    ['empty-list-set', tEmptySet],
    ['empty-tree-set', tEmptySet],
    ['list-to-set', tListToSet],
    ['list-to-list-set', tListToSet],
    ['list-to-tree-set', tListToSet],
    ['is-tree-set', tArrow([tSet], tBoolean)],
    ['is-list-set', tArrow([tSet], tBoolean)],
    ['is-leaf', tArrow([tAvl], tBoolean)],
    ['is-branch', tArrow([tAvl], tBoolean)],
    ['is-AVLTree', tArrow([tTop], tBoolean)],
    ['is-Set', tArrow([tTop], tBoolean)],
    ['leaf', tAvl],
    ['branch', tArrow([tTop, tNumber, tAvl, tAvl], tAvl)],
    ['fold', tForall([tva, tvb], tArrow([tArrow([tvb, tva], tvb), tvb, tSetApp(tva)], tvb))],
    ['all', tForall([tva], tArrow([tArrow([tva], tBoolean), tSetApp(tva)], tBoolean))],
    ['any', tForall([tva], tArrow([tArrow([tva], tBoolean), tSetApp(tva)], tBoolean))])),
  dict<DataType>(
    ['AVLTree', tData('AVLTree', [], [], dict<Type>())],
    ['Set', tData(
      'Set',
      [tva],
      [],
      dict<Type>(
        ['add', tArrow([tva], tSetApp(tva))],
        ['remove', tArrow([tva], tSetApp(tva))],
        ['size', tArrow([], tNumber)],
        ['member', tArrow([tva], tBoolean)],
        ['pick', tArrow([], tPickApp(tva, tSetApp(tva)))],
        ['union', tArrow([tSetApp(tva)], tSetApp(tva))],
        ['intersect', tArrow([tSetApp(tva)], tSetApp(tva))],
        ['difference', tArrow([tSetApp(tva)], tSetApp(tva))],
        ['symmetric-difference', tArrow([tSetApp(tva)], tSetApp(tva))],
        ['to-list', tArrow([], tListApp(tva))],
        ['fold', tForall([tvb], tArrow([tArrow([tvb, tva], tvb), tvb], tvb))]))]),
  dict<Type>(
    ['Set', tSet],
    ['AVLTree', tAvl],
    ['List', tList],
    ['Pick', tPick]));

export const moduleConstLists = tModule('builtin://lists',
  tRecord(dict<Type>(
    ['List', tArrow([tTop], tBoolean)],
    ['is-List', tArrow([tTop], tBoolean)],
    ['empty', tForall([tva], tDataRefinement(tListApp(tva), 'empty'))],
    ['is-empty', tArrow([tTop], tBoolean)],
    ['link', tForall([tva], tArrow([tva, tListApp(tva)], tDataRefinement(tListApp(tva), 'link')))],
    ['is-link', tArrow([tTop], tBoolean)],
    ['length', tForall([tva], tArrow([tListApp(tva)], tNumber))],
    ['same-length', tForall([tva, tvb], tArrow([tListApp(tva), tListApp(tvb)], tBoolean))],
    ['longer-than', tForall([tva], tArrow([tListApp(tva), tNumber], tBoolean))],
    ['shorter-than', tForall([tva], tArrow([tListApp(tva), tNumber], tBoolean))],
    ['get', tForall([tva], tArrow([tListApp(tva), tNumber], tva))],
    ['set', tForall([tva], tArrow([tListApp(tva), tNumber, tva], tListApp(tva)))],
    ['reverse', tForall([tva], tArrow([tListApp(tva)], tListApp(tva)))],
    ['push', tForall([tva], tArrow([tListApp(tva), tva], tListApp(tva)))],
    ['reverse-help', tForall([tva], tArrow([tListApp(tva), tListApp(tva)], tListApp(tva)))],
    ['last', tForall([tva], tArrow([tListApp(tva)], tva))],
    ['sort', tForall([tva], tArrow([tListApp(tva)], tListApp(tva)))],
    ['sort-by', tForall([tva], tArrow([tListApp(tva), tArrow([tva, tva], tBoolean), tArrow([tva, tva], tBoolean)], tListApp(tva)))],
    ['range', tArrow([tNumber, tNumber], tListApp(tNumber))],
    ['range-by', tArrow([tNumber, tNumber, tNumber], tListApp(tNumber))],
    ['repeat', tForall([tva], tArrow([tNumber, tva], tListApp(tva)))],
    ['filter', tForall([tva], tArrow([tArrow([tva], tBoolean), tListApp(tva)], tListApp(tva)))],
    ['append', tForall([tva], tArrow([tListApp(tva), tListApp(tva)], tListApp(tva)))],
    ['partition', tForall([tva], tArrow([tArrow([tva], tBoolean), tListApp(tva)], tRecord(dict<Type>(['is-true', tListApp(tva)], ['is-false', tListApp(tva)]))))],
    ['remove', tForall([tva], tArrow([tListApp(tva), tva], tListApp(tva)))],
    ['find', tForall([tva], tArrow([tArrow([tva], tBoolean), tListApp(tva)], tApp(tOption, [tva])))],
    ['split-at', tForall([tva], tArrow([tNumber, tListApp(tva)], tRecord(dict<Type>(['prefix', tListApp(tva)], ['suffix', tListApp(tva)]))))],
    ['take', tForall([tva], tArrow([tNumber, tListApp(tva)], tListApp(tva)))],
    ['drop', tForall([tva], tArrow([tNumber, tListApp(tva)], tListApp(tva)))],
    ['any', tForall([tva], tArrow([tArrow([tva], tBoolean), tListApp(tva)], tBoolean))],
    ['all', tForall([tva], tArrow([tArrow([tva], tBoolean), tListApp(tva)], tBoolean))],
    ['all2', tForall([tva, tvb], tArrow([tArrow([tva, tvb], tBoolean), tListApp(tva), tListApp(tvb)], tBoolean))],
    ['map', tForall([tva, tvb], tArrow([tArrow([tva], tvb), tListApp(tva)], tListApp(tvb)))],
    ['map2', tForall([tva, tvb, tvc], tArrow([tArrow([tva, tvb], tvc), tListApp(tva), tListApp(tvb)], tListApp(tvc)))],
    ['map3', tForall([tva, tvb, tvc, tvd], tArrow([tArrow([tva, tvb, tvc], tvd), tListApp(tva), tListApp(tvb), tListApp(tvc)], tListApp(tvd)))],
    ['map4', tForall([tva, tvb, tvc, tvd, tve], tArrow([tArrow([tva, tvb, tvc, tvd], tve), tListApp(tva), tListApp(tvb), tListApp(tvc), tListApp(tvd)], tListApp(tve)))],
    ['map_n', tForall([tva, tvb], tArrow([tArrow([tNumber, tva], tvb), tNumber, tListApp(tva)], tListApp(tvb)))],
    ['map2_n', tForall([tva, tvb, tvc], tArrow([tArrow([tNumber, tva, tvb], tvc), tNumber, tListApp(tva), tListApp(tvb)], tListApp(tvc)))],
    ['map3_n', tForall([tva, tvb, tvc, tvd], tArrow([tArrow([tNumber, tva, tvb, tvc], tvd), tNumber, tListApp(tva), tListApp(tvb), tListApp(tvc)], tListApp(tvd)))],
    ['map4_n', tForall([tva, tvb, tvc, tvd, tve], tArrow([tArrow([tNumber, tva, tvb, tvc, tvd], tve), tNumber, tListApp(tva), tListApp(tvb), tListApp(tvc), tListApp(tvd)], tListApp(tve)))],
    ['each', tForall([tva], tArrow([tArrow([tva], tTop), tListApp(tva)], tNothing))],
    ['each2', tForall([tva, tvb], tArrow([tArrow([tva, tvb], tTop), tListApp(tva), tListApp(tvb)], tNothing))],
    ['each3', tForall([tva, tvb, tvc], tArrow([tArrow([tva, tvb, tvc], tTop), tListApp(tva), tListApp(tvb), tListApp(tvc)], tNothing))],
    ['each4', tForall([tva, tvb, tvc, tvd], tArrow([tArrow([tva, tvb, tvc, tvd], tTop), tListApp(tva), tListApp(tvb), tListApp(tvc), tListApp(tvd)], tNothing))],
    ['each_n', tForall([tva], tArrow([tArrow([tNumber, tva], tTop), tNumber, tListApp(tva)], tNothing))],
    ['each2_n', tForall([tva, tvb], tArrow([tArrow([tNumber, tva, tvb], tTop), tNumber, tListApp(tva), tListApp(tvb)], tNothing))],
    ['each3_n', tForall([tva, tvb, tvc], tArrow([tArrow([tNumber, tva, tvb, tvc], tTop), tNumber, tListApp(tva), tListApp(tvb), tListApp(tvc)], tNothing))],
    ['each4_n', tForall([tva, tvb, tvc, tvd], tArrow([tArrow([tNumber, tva, tvb, tvc, tvd], tTop), tNumber, tListApp(tva), tListApp(tvb), tListApp(tvc), tListApp(tvd)], tNothing))],
    ['fold-while', tForall([tva, tvb], tArrow([tArrow([tva, tvb], tEitherApp(tva, tva)), tva, tListApp(tvb)], tva))],
    ['fold', tForall([tva, tvb], tArrow([tArrow([tva, tvb], tva), tva, tListApp(tvb)], tva))],
    ['foldl', tForall([tva, tvb], tArrow([tArrow([tva, tvb], tva), tva, tListApp(tvb)], tva))],
    ['foldr', tForall([tva, tvb], tArrow([tArrow([tva, tvb], tva), tva, tListApp(tvb)], tva))],
    ['fold2', tForall([tva, tvb, tvc], tArrow([tArrow([tva, tvb, tvc], tva), tva, tListApp(tvb), tListApp(tvc)], tva))],
    ['fold3', tForall([tva, tvb, tvc, tvd], tArrow([tArrow([tva, tvb, tvc, tvd], tva), tva, tListApp(tvb), tListApp(tvc), tListApp(tvd)], tva))],
    ['fold4', tForall([tva, tvb, tvc, tvd, tve], tArrow([tArrow([tva, tvb, tvc, tvd, tve], tva), tva, tListApp(tvb), tListApp(tvc), tListApp(tvd), tListApp(tve)], tva))],
    ['fold_n', tForall([tva, tvb], tArrow([tArrow([tNumber, tva, tvb], tva), tNumber, tva, tListApp(tvb)], tva))],
    ['member-with', tForall([tva], tArrow([tListApp(tva), tva, tArrow([tva, tva], tEqualityResult)], tEqualityResult))],
    ['member3', tForall([tva], tArrow([tListApp(tva), tva], tEqualityResult))],
    ['member', tForall([tva], tArrow([tListApp(tva), tva], tBoolean))],
    ['member-always3', tForall([tva], tArrow([tListApp(tva), tva], tEqualityResult))],
    ['member-always', tForall([tva], tArrow([tListApp(tva), tva], tBoolean))],
    ['member-now', tForall([tva], tArrow([tListApp(tva), tva], tBoolean))],
    ['member-now3', tForall([tva], tArrow([tListApp(tva), tva], tEqualityResult))],
    ['member-identical3', tForall([tva], tArrow([tListApp(tva), tva], tEqualityResult))],
    ['member-identical', tForall([tva], tArrow([tListApp(tva), tva], tBoolean))],
    ['shuffle', tForall([tva], tArrow([tListApp(tva)], tListApp(tva)))],
    ['filter-map', tForall([tva, tvb], tArrow([tArrow([tva], tOptionApp(tvb)), tListApp(tva)], tListApp(tvb)))],
    ['filter-values', tForall([tva], tArrow([tListApp(tOptionApp(tva))], tListApp(tva)))],
    ['distinct', tForall([tva], tArrow([tListApp(tva)], tListApp(tva)))],
    ['take-while', tForall([tva], tArrow([tArrow([tva], tBoolean), tListApp(tva)], tTuple([tListApp(tva), tListApp(tva)])))],
    ['join-str', tForall([tva], tArrow([tListApp(tva), tString], tString))],
    ['join-str-last', tForall([tva], tArrow([tListApp(tva), tString, tString], tString))],
    ['list',
      tRecord(dict<Type>(
        ['make', tForall([tva], tArrow([tArray(tva)], tListApp(tva)))],
        ['make0', tForall([tva], tArrow([], tDataRefinement(tListApp(tva), 'empty')))],
        ['make1', tForall([tva], tArrow([tva], tDataRefinement(tListApp(tva), 'link')))],
        ['make2', tForall([tva], tArrow([tva, tva], tDataRefinement(tListApp(tva), 'link')))],
        ['make3', tForall([tva], tArrow([tva, tva, tva], tDataRefinement(tListApp(tva), 'link')))],
        ['make4', tForall([tva], tArrow([tva, tva, tva, tva], tDataRefinement(tListApp(tva), 'link')))],
        ['make5', tForall([tva], tArrow([tva, tva, tva, tva, tva], tDataRefinement(tListApp(tva), 'link')))]))])),
  dict<DataType>(
    ['List', tData(
      'List',
      [tva],
      [
        tSingletonVariant('empty', dict<Type>()),
        tVariant('link', [['first', tva], ['rest', tListApp(tva)]], dict<Type>())
      ],
      dict<Type>(
        ['length', tArrow([], tNumber)],
        ['each', tArrow([tArrow([tva], tNothing)], tNothing)],
        ['map', tForall([tvb], tArrow([tArrow([tva], tvb)], tListApp(tvb)))],
        ['filter', tArrow([tArrow([tva], tBoolean)], tListApp(tva))],
        ['find', tArrow([tArrow([tva], tBoolean)], tOptionApp(tva))],
        ['partition', tArrow([tArrow([tva], tBoolean)], tRecord(dict<Type>(['is-true', tListApp(tva)], ['is-false', tListApp(tvb)])))],
        ['foldr', tForall([tvb], tArrow([tArrow([tva, tvb], tvb), tvb], tvb))],
        ['foldl', tForall([tvb], tArrow([tArrow([tva, tvb], tvb), tvb], tvb))],
        ['all', tArrow([tArrow([tva], tBoolean)], tBoolean)],
        ['any', tArrow([tArrow([tva], tBoolean)], tBoolean)],
        ['member', tArrow([tva], tBoolean)],
        ['append', tArrow([tListApp(tva)], tListApp(tva))],
        ['last', tArrow([], tva)],
        ['reverse', tArrow([], tListApp(tva))],
        ['sort-by', tArrow([tArrow([tva, tva], tBoolean), tArrow([tva, tva], tBoolean)], tListApp(tva))],
        ['sort', tArrow([], tListApp(tva))],
        ['join-str', tArrow([tString], tString)],
        ['join-str-last', tArrow([tString, tString], tString)],
        ['_output', tOutput],
        ['_plus', tArrow([tListApp(tva)], tListApp(tva))],
        ['push', tArrow([tva], tListApp(tva))],
        ['split-at', tArrow([tNumber], tRecord(dict<Type>(['prefix', tListApp(tva)], ['suffix', tListApp(tva)])))],
        ['take', tArrow([tNumber], tListApp(tva))],
        ['drop', tArrow([tNumber], tListApp(tva))],
        ['get', tArrow([tNumber], tva)],
        ['set', tArrow([tNumber, tva], tListApp(tva))],
        ['remove', tArrow([tva], tListApp(tva))]))]),
  dict<Type>(
    ['List', tList],
    ['Either', tEither],
    ['Option', tOption]));

export const tAndThen: Type =
  tForall(
    [tva, tvb],
    tArrow(
      [tArrow([tva], tvb)],
      tOptionApp(tvb)));

export const moduleConstOption = tModule('builtin://option',
  tRecord(dict<Type>(
    ['Option', tArrow([tTop], tBoolean)],
    ['is-Option', tArrow([tTop], tBoolean)],
    ['none', tForall([tva], tDataRefinement(tOptionApp(tva), 'none'))],
    ['is-none', tArrow([tTop], tBoolean)],
    ['some', tForall([tva], tArrow([tva], tDataRefinement(tOptionApp(tva), 'some')))],
    ['is-some', tArrow([tTop], tBoolean)])),
  dict<DataType>(
    ['Option', tData(
      'Option',
      [tva],
      [
        tSingletonVariant('none',
          dict<Type>(
            ['_match', tTop],
            ['or-else', tArrow([tva], tva)],
            ['and-then', tAndThen])),
        tVariant('some',
          [['value', tva]],
          dict<Type>(
            ['_match', tTop],
            ['or-else', tArrow([tva], tva)],
            ['and-then', tAndThen]))
      ],
      dict<Type>(
        ['and-then', tAndThen],
        ['or-else', tArrow([tva], tva)],
        ['_match', tTop]))]),
  dict<Type>(
    ['Option', tOption]));

export const moduleConstError = tModule('builtin://error',
  tRecord(dict<Type>(
    ['RuntimeError', tArrow([tTop], tBoolean)],
    ['is-RuntimeError', tArrow([tTop], tBoolean)],
    ['message-exception', tArrow([tString], tRuntimeError)],
    ['is-message-exception', tArrow([tTop], tBoolean)],
    ['no-branches-matched', tArrow([tTop, tString], tRuntimeError)],
    ['is-no-branches-matched', tArrow([tTop], tBoolean)],
    ['internal-error', tArrow([tTop, tTop], tRuntimeError)],
    ['is-internal-error', tArrow([tTop], tBoolean)],
    ['field-not-found', tArrow([tTop, tTop, tString], tRuntimeError)],
    ['is-field-not-found', tArrow([tTop], tBoolean)],
    ['lookup-non-object', tArrow([tTop, tTop, tString], tRuntimeError)],
    ['is-lookup-non-object', tArrow([tTop], tBoolean)],
    ['extend-non-object', tArrow([tTop, tTop], tRuntimeError)],
    ['is-extend-non-object', tArrow([tTop], tBoolean)],
    ['non-boolean-condition', tArrow([tTop, tTop, tTop], tRuntimeError)],
    ['is-non-boolean-condition', tArrow([tTop], tBoolean)],
    ['non-boolean-op', tArrow([tTop, tTop, tTop, tTop], tRuntimeError)],
    ['is-non-boolean-op', tArrow([tTop], tBoolean)],
    ['generic-type-mismatch', tArrow([tTop, tString], tRuntimeError)],
    ['is-generic-type-mismatch', tArrow([tTop], tBoolean)],
    ['outside-numeric-range', tArrow([tTop, tTop, tTop], tRuntimeError)],
    ['is-outside-numeric-range', tArrow([tTop], tBoolean)],
    ['plus-error', tArrow([tTop, tTop], tRuntimeError)],
    ['is-plus-error', tArrow([tTop], tBoolean)],
    ['numeric-binop-error', tArrow([tTop, tTop, tTop, tTop, tTop], tRuntimeError)],
    ['is-numeric-binop-error', tArrow([tTop], tBoolean)],
    ['cases-arity-mismatch', tArrow([tTop, tTop, tTop, tTop, tTop], tRuntimeError)],
    ['is-cases-arity-mismatch', tArrow([tTop], tBoolean)],
    ['cases-singleton-mismatch', tArrow([tTop, tBoolean, tTop, tTop], tRuntimeError)],
    ['is-cases-singleton-mismatch', tArrow([tTop], tBoolean)],
    ['arity-mismatch', tArrow([tTop, tTop, tTop], tRuntimeError)],
    ['is-arity-mismatch', tArrow([tTop], tBoolean)],
    ['non-function-app', tArrow([tTop, tTop], tRuntimeError)],
    ['is-non-function-app', tArrow([tTop], tBoolean)],
    ['bad-app', tArrow([tTop, tString, tString, tNumber, tTop], tRuntimeError)],
    ['is-bad-app', tArrow([tTop], tBoolean)],
    ['uninitialized-id', tArrow([tTop, tString], tRuntimeError)],
    ['is-uninitialized-id', tArrow([tTop], tBoolean)],
    ['module-load-failure', tArrow([tTop], tRuntimeError)],
    ['is-module-load-failure', tArrow([tTop], tBoolean)],
    ['invalid-array-index', tArrow([tString, tTop, tNumber, tString], tRuntimeError)],
    ['is-invalid-array-index', tArrow([tTop], tBoolean)],
    ['user-break', tRuntimeError],
    ['is-user-break', tArrow([tTop], tBoolean)],
    ['user-exception', tArrow([tTop], tRuntimeError)],
    ['is-user-exception', tArrow([tTop], tBoolean)],
    ['exit', tArrow([tNumber], tRuntimeError)],
    ['is-exit', tArrow([tTop], tBoolean)],
    ['exit-quiet', tArrow([tNumber], tRuntimeError)],
    ['is-exit-quiet', tArrow([tTop], tBoolean)],
    ['ParseError', tArrow([tTop], tBoolean)],
    ['is-ParseError', tArrow([tTop], tBoolean)],
    ['parse-error-next-token', tArrow([tTop, tString], tParseError)],
    ['is-parse-error-next-token', tArrow([tTop], tBoolean)],
    ['parse-error-eof', tArrow([tTop], tParseError)],
    ['is-parse-error-eof', tArrow([tTop], tBoolean)],
    ['parse-error-unterminated-string', tArrow([tTop], tParseError)],
    ['is-parse-error-unterminated-string', tArrow([tTop], tBoolean)],
    ['parse-error-bad-number', tArrow([tTop], tParseError)],
    ['is-parse-error-bad-number', tArrow([tTop], tBoolean)],
    ['parse-error-bad-operator', tArrow([tTop], tParseError)],
    ['is-parse-error-bad-operator', tArrow([tTop], tBoolean)],
    ['parse-error-bad-check-operator', tArrow([tTop], tParseError)],
    ['is-parse-error-bad-check-operator', tArrow([tTop], tBoolean)],
    ['empty-block', tArrow([tTop], tParseError)],
    ['is-empty-block', tArrow([tTop], tBoolean)],
    ['bad-block-stmt', tArrow([tTop], tParseError)],
    ['is-bad-block-stmt', tArrow([tTop], tBoolean)],
    ['bad-check-block-stmt', tArrow([tTop], tParseError)],
    ['is-bad-check-block-stmt', tArrow([tTop], tBoolean)],
    ['fun-missing-colon', tArrow([tTop], tParseError)],
    ['is-fun-missing-colon', tArrow([tTop], tBoolean)],
    ['fun-missing-end', tArrow([tTop], tParseError)],
    ['is-fun-missing-end', tArrow([tTop], tBoolean)],
    ['args-missing-comma', tArrow([tTop], tParseError)],
    ['is-args-missing-comma', tArrow([tTop], tBoolean)],
    ['app-args-missing-comma', tArrow([tTop], tParseError)],
    ['is-app-args-missing-comma', tArrow([tTop], tBoolean)],
    ['missing-end', tArrow([tTop], tParseError)],
    ['is-missing-end', tArrow([tTop], tBoolean)],
    ['missing-comma', tArrow([tTop], tParseError)],
    ['is-missing-comma', tArrow([tTop], tBoolean)])),
  dict<DataType>(
    ['RuntimeError',
      tData(
        'RuntimeError',
        [],
        [
          tVariant('message-exception', [['message', tString]], dict<Type>()),
          tVariant('no-branches-matched', [['loc', tTop], ['expression', tString]], dict<Type>()),
          tVariant('internal-error', [['message', tTop], ['info-args', tTop]], dict<Type>()),
          tVariant('field-not-found', [['loc', tTop], ['obj', tTop], ['field', tString]], dict<Type>()),
          tVariant('lookup-non-object', [['loc', tTop], ['non-obj', tTop], ['field', tString]], dict<Type>()),
          tVariant('extend-non-object', [['loc', tTop], ['non-obj', tTop]], dict<Type>()),
          tVariant('generic-type-mismatch', [['val', tTop], ['typ', tString]], dict<Type>()),
          tVariant('numeric-binop-error', [['val1', tTop], ['val2', tTop], ['opname', tTop], ['opdesc', tTop], ['methodname', tTop]], dict<Type>()),
          tVariant('cases-arity-mismatch', [['branch-loc', tTop], ['num-args', tTop], ['actual-arity', tTop], ['cases-loc', tTop]], dict<Type>()),
          tVariant('cases-singleton-mismatch', [['branch-loc', tTop], ['should-be-singleton', tBoolean], ['cases-loc', tTop]], dict<Type>()),
          tVariant('arity-mismatch', [['fun-def-loc', tTop], ['fun-def-arity', tTop], ['fun-app-args', tTop]], dict<Type>()),
          tVariant('non-function-app', [['loc', tTop], ['non-fun-val', tTop]], dict<Type>()),
          tVariant('uninitialized-id', [['loc', tTop], ['name', tString]], dict<Type>()),
          tVariant('module-load-failure', [['names', tTop]], dict<Type>()),
          tVariant('invalid-array-index', [['method-name', tString], ['array', tTop], ['index', tNumber], ['reason', tString]], dict<Type>()),
          tSingletonVariant('user-break', dict<Type>())
        ],
        dict<Type>(
          ['_match', tTop]))],
    ['ParseError', tData(
      'ParseError',
      [],
      [
        tVariant('parse-error-next-token', [['loc', tTop], ['next-token', tString]], dict<Type>()),
        tVariant('parse-error-bad-check-operator', [['op', tTop]], dict<Type>()),
        tVariant('parse-error-bad-operator', [['loc', tTop], ['next-token', tString]], dict<Type>()),
        tVariant('parse-error-bad-number', [['loc', tTop], ['next-token', tString]], dict<Type>()),
        tVariant('parse-error-eof', [['loc', tTop]], dict<Type>()),
        tVariant('parse-error-unterminated-string', [['loc', tTop]], dict<Type>()),
        tVariant('empty-block', [['loc', tTop]], dict<Type>()),
        tVariant('bad-block-stmt', [['loc', tTop]], dict<Type>()),
        tVariant('bad-check-block-stmt', [['loc', tTop]], dict<Type>()),
        tVariant('fun-missing-colon', [['loc', tTop]], dict<Type>()),
        tVariant('fun-missing-end', [['loc', tTop]], dict<Type>()),
        tVariant('args-missing-comma', [['loc', tTop]], dict<Type>()),
        tVariant('app-args-missing-comma', [['loc', tTop]], dict<Type>()),
        tVariant('missing-end', [['loc', tTop]], dict<Type>()),
        tVariant('missing-comma', [['loc', tTop]], dict<Type>())
      ],
      dict<Type>(
        ['loc', tTop],
        ['_match', tTop]))]),
  dict<Type>(
    ['RuntimeError', tRuntimeError],
    ['ParseError', tParseError],
    ['Error', tName(local, new A.SName(A.dummyLoc, 'Error'))]));

export const moduleConstEither = tModule('builtin://either',
  tRecord(dict<Type>(
    ['Either', tArrow([tTop], tBoolean)],
    ['is-Either', tArrow([tTop], tBoolean)],
    ['left', tForall([tva, tvb], tArrow([tva], tDataRefinement(tEitherApp(tva, tvb), 'left')))],
    ['is-left', tArrow([tTop], tBoolean)],
    ['right', tForall([tva, tvb], tArrow([tvb], tDataRefinement(tEitherApp(tva, tvb), 'right')))],
    ['is-right', tArrow([tTop], tBoolean)])),
  dict<DataType>(
    ['Either', tData(
      'Either',
      [tva, tvb],
      [
        tVariant('left',
          [['v', tva]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('right',
          [['v', tvb]],
          dict<Type>(
            ['_match', tTop]))
      ],
      dict<Type>(
        ['v', tTop],
        ['_match', tTop]))]),
  dict<Type>(
    ['Either', tEither]));

export const moduleConstValueskeleton = tModule('builtin://valueskeleton',
  tRecord(dict<Type>(
    ['ValueSkeleton', tArrow([tTop], tBoolean)],
    ['is-ValueSkeleton', tArrow([tTop], tBoolean)],
    ['vs-str', tArrow([tString], tValueSkeleton)],
    ['is-vs-str', tArrow([tTop], tBoolean)],
    ['vs-value', tArrow([tTop], tValueSkeleton)],
    ['is-vs-value', tArrow([tTop], tBoolean)],
    ['vs-collection', tArrow([tString, tListApp(tTop)], tValueSkeleton)],
    ['is-vs-collection', tArrow([tTop], tBoolean)],
    ['vs-constr', tArrow([tString, tListApp(tTop)], tValueSkeleton)],
    ['is-vs-constr', tArrow([tTop], tBoolean)],
    ['vs-table', tArrow([tArray(tString), tArray(tArray(tTop))], tValueSkeleton)],
    ['is-table', tArrow([tTop], tBoolean)],
    ['vs-row', tArrow([tArray(tString), tArray(tTop)], tValueSkeleton)],
    ['is-row', tArrow([tTop], tBoolean)],
    ['vs-seq', tArrow([tListApp(tTop)], tValueSkeleton)],
    ['is-seq', tArrow([tTop], tBoolean)])),
  dict<DataType>(
    ['ValueSkeleton', tData(
      'ValueSkeleton',
      [],
      [
        tVariant('vs-str',
          [['s', tString]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-value',
          [['s', tTop]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-collection',
          [['name', tString],
            ['args', tListApp(tTop)]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-constr',
          [['name', tString],
            ['args', tListApp(tTop)]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-table',
          [['headers', tArray(tString)],
            ['rows', tArray(tArray(tTop))]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-row',
          [['headers', tArray(tString)],
            ['values', tArray(tTop)]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-seq',
          [['items', tListApp(tTop)]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('vs-matrix',
          [['rows', tNumber],
            ['cols', tNumber],
            ['items', tArray(tTop)]],
          dict<Type>(
            ['_match', tTop]))
      ],
      dict<Type>(
        ['_match', tTop]))]),
  dict<Type>(
    ['ValueSkeleton', tValueSkeleton]));

export const sExpStructMems: Map<string, Type> = dict<Type>(
  ['s-list', tArrow([tListApp(tSExp)], tSExp)],
  ['s-num', tArrow([tNumber], tSExp)],
  ['s-str', tArrow([tString], tSExp)],
  ['s-sym', tArrow([tString], tSExp)],
  ['is-s-list', tArrow([tTop], tBoolean)],
  ['is-s-num', tArrow([tTop], tBoolean)],
  ['is-s-str', tArrow([tTop], tBoolean)],
  ['is-s-sym', tArrow([tTop], tBoolean)]);

export const moduleConstSExp = tModule('builtin://s-exp',
  tRecord(mapSet(sExpStructMems,
    'read-s-exp', tArrow([tString], tSExp))),
  dict<DataType>(),
  dict<Type>(
    ['S-Exp', tSExp]));

export const moduleConstSExpStructs = tModule('builtin://s-exp-structs',
  tRecord(sExpStructMems),
  dict<DataType>(
    ['S-Exp', tData(
      'S-Exp',
      [],
      [
        tVariant('s-list',
          [['exps', tListApp(tSExp)]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('s-num',
          [['n', tNumber]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('s-str',
          [['s', tString]],
          dict<Type>(
            ['_match', tTop])),
        tVariant('s-sym',
          [['s', tString]],
          dict<Type>(
            ['_match', tTop]))
      ],
      dict<Type>())]),
  dict<Type>(
    ['S-Exp', tSExp]));

export const moduleConstJsonStructs = tModule('builtin://json-structs',
  tRecord(dict<Type>(
    ['link', tForall([tva], tArrow([tva, tListApp(tva)], tDataRefinement(tListApp(tva), 'link')))],
    ['empty', tForall([tva], tDataRefinement(tListApp(tva), 'empty'))],
    ['is-empty', tArrow([tTop], tBoolean)],
    ['is-link', tArrow([tTop], tBoolean)],
    ['map', tForall([tva, tvb], tArrow([tArrow([tva], tvb), tListApp(tva)], tListApp(tvb)))],
    ['is-array', tForall([tva], tArrow([tTop], tBoolean))],
    ['JSON', tArrow([tTop], tBoolean)],
    ['is-JSON', tArrow([tTop], tBoolean)],
    ['j-obj', tArrow([tStringDictApp(tJson)], tDataRefinement(tJson, 'j-obj'))],
    ['is-j-obj', tArrow([tTop], tBoolean)],
    ['j-arr', tArrow([tListApp(tJson)], tDataRefinement(tJson, 'j-arr'))],
    ['is-j-arr', tArrow([tTop], tBoolean)],
    ['j-num', tArrow([tNumber], tDataRefinement(tJson, 'j-num'))],
    ['is-j-num', tArrow([tTop], tBoolean)],
    ['j-str', tArrow([tString], tDataRefinement(tJson, 'j-str'))],
    ['is-j-str', tArrow([tTop], tBoolean)],
    ['j-bool', tArrow([tBoolean], tDataRefinement(tJson, 'j-bool'))],
    ['is-j-bool', tArrow([tTop], tBoolean)],
    ['j-null', tArrow([], tDataRefinement(tJson, 'j-null'))],
    ['is-j-null', tArrow([tTop], tBoolean)],
    ['tojson', tArrow([tTop], tJson)])),
  dict<DataType>(
    ['JSON', tData(
      'JSON',
      [],
      [
        tVariant('j-obj',
          [['dict', tStringDictApp(tJson)]],
          dict<Type>()),
        tVariant('j-arr',
          [['l', tListApp(tJson)]],
          dict<Type>()),
        tVariant('j-num',
          [['n', tNumber]],
          dict<Type>()),
        tVariant('j-str',
          [['s', tString]],
          dict<Type>()),
        tVariant('j-bool',
          [['b', tBoolean]],
          dict<Type>()),
        tSingletonVariant('j-null', dict<Type>())
      ],
      dict<Type>(
        ['native', tArrow([], tTop)],
        ['serialize', tArrow([], tString)]))]),
  dict<Type>(
    ['List', tList],
    ['JSON', tJson]));

export const defaultModules: Map<string, ModuleType> = new Map<string, ModuleType>();
defaultModules.set('builtin://equality', moduleConstEquality);
defaultModules.set('builtin://lists', moduleConstLists);
defaultModules.set('builtin://option', moduleConstOption);
defaultModules.set('builtin://error', moduleConstError);
defaultModules.set('builtin://either', moduleConstEither);
defaultModules.set('builtin://arrays', moduleConstArrays);
defaultModules.set('builtin://pick', moduleConstPick);
defaultModules.set('builtin://sets', moduleConstSets);
defaultModules.set('builtin://s-exp', moduleConstSExp);
defaultModules.set('builtin://s-exp-structs', moduleConstSExpStructs);
defaultModules.set('builtin://json-structs', moduleConstJsonStructs);
defaultModules.set('builtin://valueskeleton', moduleConstValueskeleton);

export function makeDefaultModules(): Map<string, ModuleType> {
  return defaultModules;
}
