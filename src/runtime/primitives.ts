// Separate file is necessary to avoid cyclic imports
import type {
  DataMetaBase,
  DataSharedBase,
  DataValueType,
  DataVariantBase,
  ExpandRecursively,
  PTuple,
  VariantType,
} from './types/primitive-types';

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

export function PTuple<T extends any[]>(values: T): PTuple<T> {
  (values as any)["$brand"] = $PTupleBrand;

  return <PTuple<T>>values;
}

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

let variantCounter = 1;
let variants : DataMetaBase<string>[] = [];

export function createVariant<T extends string>(
  sharedBase : DataSharedBase, 
  extension : DataVariantBase, 
  meta : DataMetaBase<T>
) : VariantType<T> {
  const extended = extend(sharedBase, extension);
  const metaExtended = Object.assign(extended, meta);
  // NOTE(joe): we cannot pass extended as an argument to this function, because
  // sharedBased/extension/meta can't easily have a cycle between them due to
  // codegen passing them in as object literals.
  metaExtended.$variant = variantCounter++;
  variants.push(metaExtended);
  return (metaExtended as VariantType<T>);
}

export function makeDataValue<
  O extends DataSharedBase,
  E extends DataVariantBase,
>(obj : O, extension : E) : DataValueType<O,E> {
  Object.setPrototypeOf(extension, obj);
  extension.$methods = Object.create(obj.$methods);
  return (extension as O & Required<E>);
}

// NOTE: the for loop here is the best way to get the properties on the
// prototype chain as well! For raw objects we want this, and want to include
// e.g. methods. For data values we don't, because we just want the names listed
// in the variant declaration, not all the with: and sharing: fields.
export function getRawObjectFields(val : any) : string[] {
  const names : string[] = [];
  for(let field in val) {
    if(field[0] !== "$") {
      names.push(field);
    }
  }
  return names;
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

export function isCallable(obj: any) {
  return typeof obj === "function";
}

// TODO(alex): Will nothing always be value 'undefined'?
export function isNothing(obj: any): boolean { return obj === undefined };

const isNumber: (val: any) => boolean = _NUMBER["isPyretNumber"];
const isRoughNumber: (val: any) => boolean = _NUMBER["isRoughnum"];
export { isNumber, isRoughNumber };

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
  return function binder(pyretSelf : any) {

    let myFunction = function() {
      const innerArgs = [pyretSelf].concat(Array.from(arguments));
      return inner.apply(null, innerArgs);
    };

    (myFunction as any)["$brand"] = $PMethodBrand;
    (myFunction as any)["$binder"] = binder;

    return myFunction;
  };
}

/* @stopify flat */
export function hasBrand(brand: any, val: object): boolean {
    return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === brand);
}

/* @stopify flat */
export function applyBrand(brand: any, val: any): any {
    val["$brand"] = brand;
    return val;
}
