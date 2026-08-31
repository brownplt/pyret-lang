/*
  Typed facade over the existing js-numbers library
  (src/js/base/js-numbers.js), which implements Pyret's exact-rational
  number tower. Pyret numeric literals and arithmetic inside the compiler
  must use these values, never raw JS floats.
*/

import { amdRequire } from './amd';

// A js-numbers value: either a JS fixnum (number) or a boxed
// BigInteger/Rational/Roughnum object from the library.
export type PyretNumber = number | object;

export interface JsNumbersLib {
  fromFixnum(n: number, errbacks?: any): PyretNumber;
  fromString(s: string, errbacks?: any): PyretNumber | false;
  fromSchemeString(s: string, errbacks?: any): PyretNumber | false;
  isSchemeNumber(x: any): boolean;
  isRational(x: any): boolean;
  isReal(x: any): boolean;
  isExact(x: any): boolean;
  isInteger(x: any): boolean;
  isRoughnum(x: any): boolean;
  isPositive(x: any): boolean;
  isNegative(x: any): boolean;
  isNonPositive(x: any): boolean;
  isNonNegative(x: any): boolean;
  toFixnum(x: PyretNumber): number;
  toExact(x: PyretNumber, errbacks?: any): PyretNumber;
  toRoughnum(x: PyretNumber, errbacks?: any): PyretNumber;
  add(a: PyretNumber, b: PyretNumber, errbacks?: any): PyretNumber;
  subtract(a: PyretNumber, b: PyretNumber, errbacks?: any): PyretNumber;
  multiply(a: PyretNumber, b: PyretNumber, errbacks?: any): PyretNumber;
  divide(a: PyretNumber, b: PyretNumber, errbacks?: any): PyretNumber;
  equals(a: PyretNumber, b: PyretNumber, errbacks?: any): boolean;
  equalsAnyZero(x: PyretNumber): boolean;
  lessThan(a: PyretNumber, b: PyretNumber, errbacks?: any): boolean;
  lessThanOrEqual(a: PyretNumber, b: PyretNumber, errbacks?: any): boolean;
  greaterThan(a: PyretNumber, b: PyretNumber, errbacks?: any): boolean;
  greaterThanOrEqual(a: PyretNumber, b: PyretNumber, errbacks?: any): boolean;
  floor(x: PyretNumber, errbacks?: any): PyretNumber;
  ceiling(x: PyretNumber, errbacks?: any): PyretNumber;
  abs(x: PyretNumber, errbacks?: any): PyretNumber;
  numerator(x: PyretNumber): PyretNumber;
  denominator(x: PyretNumber): PyretNumber;
  // NOTE: the library does NOT export a toString function — calling
  // jsnums.toString(x) silently hits Object.prototype.toString and yields
  // "[object Object]". Use String(x) on number values instead.
  [key: string]: any;
}

export const jsnums: JsNumbersLib = amdRequire('pyret-base/js/js-numbers');

// Pyret's default errbacks throw plain Errors inside the compiler.
export const throwingErrbacks = {
  throwDivByZero: (msg: any) => { throw new Error(String(msg)); },
  throwToleranceError: (msg: any) => { throw new Error(String(msg)); },
  throwRelToleranceError: (msg: any) => { throw new Error(String(msg)); },
  throwGeneralError: (msg: any) => { throw new Error(String(msg)); },
  throwDomainError: (msg: any) => { throw new Error(String(msg)); },
  throwSqrtNegative: (msg: any) => { throw new Error(String(msg)); },
  throwLogNonPositive: (msg: any) => { throw new Error(String(msg)); },
  throwIncomparableValues: (msg: any) => { throw new Error(String(msg)); },
  throwInternalError: (msg: any) => { throw new Error(String(msg)); },
};
