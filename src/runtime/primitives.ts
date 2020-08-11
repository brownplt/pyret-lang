// Separate file is necessary to avoid cyclic imports

const _NUMBER = require("./js-numbers.js");

export const $PRowBrand = "row";
export const $PTableBrand = "$table";
export const $PTupleBrand = "tuple";
export const $PRefBrand = "ref";

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
  return typeof obj === "function" && "$brand" in obj && obj["$brand"] === "METHOD";
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
