// Separate file is necessary to avoid cyclic imports

const _NUMBER = require("./js-numbers.js");

const $PMethodBrand = "METHOD";
const $PRowBrand = "row";
const $PTableBrand = "$table";
const $PTupleBrand = "tuple";
const $PRefBrand = "ref";

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
  $PMethodBrand
}

// ********* Runtime Type Representations (Non-Primitives) *********
export interface PTuple {
  $brand: string,
  [key: string]: any,
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

export function isRow(val: any): boolean {
  return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === $PRowBrand);
}

export function isTable(val: any): boolean {
  return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === $PTableBrand);
}

export function isFunction(obj: any): boolean {
  return (typeof obj === "function") && !(isMethod(obj));
}

export function isMethod(obj: any): boolean {
  return typeof obj === "function" && "$brand" in obj && obj["$brand"] === $PMethodBrand;
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
  return (typeof val === "object") && ("$brand" in val) && !(isPTuple(val)) && !(isTable(val)) && !(isRow(val));
}

export function isRawObject(val: any): boolean {
  return (typeof val === "object") && !("$brand" in val);
}

export function isPTuple(val: any): boolean {
  return (Array.isArray(val)) && ("$brand" in val) && (val["$brand"] === $PTupleBrand);
}

export function isArray(val: any): boolean {
  return (Array.isArray(val)) && !("$brand" in val);
}

export function isPRef(val: any): boolean {
  return (typeof val === "object") && ("$brand" in val) && (val["$brand"] === $PRefBrand);
}

export function makeMethodBinder(inner: any): any {
  return function binder(pyretSelf) {
    inner["$brand"] = $PMethodBrand;
    inner["$binder"] = binder;

    var mainArguments = Array.prototype.slice.call(arguments);
    mainArguments.push("extra data");

    return function() {
      const innerArgs = [pyretSelf].concat(Array.prototype.slice.call(arguments));
      return inner.apply(this, innerArgs);
    }
  };
}
