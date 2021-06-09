// Separate file is necessary to avoid cyclic imports

const _NUMBER = require("./js-numbers.js");

const $PMethodBrand = "METHOD";
const $PRowBrand = "row";
const $PTableBrand = "$table";
const $PTupleBrand = "tuple";
const $PRefBrand = "ref";

const $nothing = undefined;

// NOTE(alex): Hack required b/c of TS "export const" desugaring and Stopify
// `export const X = FOO;` => `export.X = FOO` => `$S.g.export.X = FOO`
// Stopify only has one global object. Cannot switch global object variables within the same run
//   Using X in any code within this module outside of that run results in an error
//   Access is through `S.g.export.X`
// TS generates code that accesses the global
export {
  $PRowBrand,
  $PTableBrand,
  $PTupleBrand,
  $PRefBrand,
  $PMethodBrand,
  $nothing
}

// ********* Runtime Type Representations (Non-Primitives) *********
export type PTuple = any[] & {
  $brand: string,
}

export function PTuple(values: any[]): PTuple {
  values["$brand"] = $PTupleBrand;

  return <PTuple><any>values;
}

export interface DataValue {
  $brand: any,
  [key: string]: any
}

export interface PRef {
  $brand: string,
  ref: Object,
}

// From https://stackoverflow.com/a/57683652/783424, which
// helps make the types more readable
type ExpandRecursively<T> = T extends object
  ? T extends infer O ? { [K in keyof O]: ExpandRecursively<O[K]> } : never
  : T;

export type DataSharedBase = {
  $methods: Record<string, () => any>,
};

export type DataVariantBase = {
  $methods?: Record<string, () => any>
} & Record<string, any>;

export type DataMetaBase = {
  $data: DataSharedBase,
  $name: string,
  $variant?: DataVariantBase,
  $fieldNames?: string[],
  $methods?: Record<string, () => any>,
};

export function extend(
  obj : DataSharedBase,
  extension : DataVariantBase
) : ExpandRecursively<DataSharedBase & DataVariantBase> {
  for(let k in obj.$methods) {
    if(!(extension.hasOwnProperty(k))) {
      Object.defineProperty(extension, k, { configurable: true, get: obj.$methods[k] });
    }
  }
  Object.setPrototypeOf(extension, obj);
  Object.setPrototypeOf(extension.$methods, obj.$methods);
  return (extension as DataSharedBase & DataVariantBase);
}

export function createVariant(
  sharedBase : DataSharedBase, 
  extension : DataVariantBase, 
  meta : DataMetaBase
) : ExpandRecursively<DataSharedBase & DataVariantBase & Required<DataMetaBase>> {
  const extended = extend(sharedBase, extension);
  const metaExtended = Object.assign(extended, meta);
  // NOTE(joe): we cannot pass extended as an argument to this function, because
  // sharedBased/extension/meta can't easily have a cycle between them due to
  // codegen passing them in as object literals.
  metaExtended.$variant = metaExtended;
  return (metaExtended as DataSharedBase & DataVariantBase & Required<DataMetaBase>);
}

export function makeDataValue<
  O extends {},
  E extends DataVariantBase,
>(obj : O, extension : E) : O & E {
  Object.setPrototypeOf(extension, obj);
  extension.$methods = {};
  return (extension as O & E);
}

export function isRow(val: any): boolean {
    return hasBrand($PRowBrand, val);
}

export function isTable(val: any): boolean {
    return hasBrand($PTableBrand, val);
}

export function isFunction(obj: any): boolean {
  return (typeof obj === "function") && !(isMethod(obj));
}

export function isMethod(obj: any): boolean {
  return (typeof obj === "function") && ("$brand" in obj) && (obj["$brand"] === $PMethodBrand);
}

// TODO(alex): Will nothing always be value 'undefined'?
export function isNothing(obj: any): boolean { return obj === undefined };

export const isNumber: (val: any) => boolean = _NUMBER["isPyretNumber"];
export const isRoughNumber: (val: any) => boolean = _NUMBER["isRoughnum"];

export function isBoolean(val: any): boolean {
  return typeof val === "boolean";
}

export function isString(val: any): boolean {
  return typeof val === "string";
}

export function isDataVariant(val: any): boolean {
  return (typeof val === "object") && ("$variant" in val);
}

export function isRawObject(val: any): boolean {
  return (typeof val === "object") && !("$variant" in val) && !(isPTuple(val)) && !(isTable(val)) && !(isRow(val));
}

export function isPTuple(val: any): boolean {
  return (Array.isArray(val)) && ("$brand" in val) && (val["$brand"] === $PTupleBrand);
}

export function isArray(val: any): boolean {
  return (Array.isArray(val)) && !("$brand" in val);
}

export function isPRef(val: any): boolean {
    return hasBrand($PRefBrand, val);
}

export function makeMethodBinder(inner: any): any {
  return function binder(pyretSelf) {

    let myFunction = function() {
      const innerArgs = [pyretSelf].concat(Array.from(arguments));
      return inner.apply(null, innerArgs);
    };

    myFunction["$brand"] = $PMethodBrand;
    myFunction["$binder"] = binder;

    return myFunction;
  };
}

export function hasBrand(brand: any, val: object): boolean {
    return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === brand);
}

export function applyBrand(brand: any, val: object): any {
    val["$brand"] = brand;
    return val;
}
