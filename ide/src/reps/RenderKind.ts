/* Most Pyret values and some other JS values can be associated to a single
 * RenderKind which is used to identify how it should be rendered */

import { isRHSCheck } from '../rhsObject';

import {
  isSpyMessage,
  isSpyValue,
  isRTMessage,
  RawRTMessage,
} from '../rtMessages';
import { ContainerRange } from './Range';

function isList(value: any): boolean {
  if (typeof value.$brand === 'undefined') {
    return false;
  }
  return (typeof value.$brand.names !== 'undefined' && value.$brand.names[0] === 'first'
    && value.$brand.names[1] === 'rest')
    || (value.$brand.names === false && typeof value.partition !== 'undefined');
}

// A number of Pyret types are not RenderKinds (for example members of sets such
// as function and list)
// Some RenderKinds are not Pyret types, such as:
// key-value: a pair of a key and a value extracted from an object. Used to make
// object rendering easier
// range: a range of indices associated with a JS array. These are used to
// paginate / render large values lazily
export type RenderKind = 'undefined' | 'number' | 'string' | 'boolean' |
'function' | 'table' | 'image' | 'chart' | 'reactor' | 'template' | 'list' |
'object' | 'spy-value' | 'spy-message' | 'check' | 'array' | 'range' |
'key-value' | 'exactnum';

// Why isn't there a PyretValue type?
// The main reason is that object literals in Pyret are represented by JS
// objects with arbitrary fields. Thus, the type would have to include a number
// of primitives and *any object*. Such a type would only exclude BigInt,
// Symbol, and null as opposed to the any type (all of which rarely occur
// anyway)

export function getRenderKind(value: any): RenderKind {
  if (value === undefined) {
    return 'undefined';
  }
  if (typeof value === 'number') {
    return 'number';
  }
  if (typeof value === 'string') {
    return 'string';
  }
  if (typeof value === 'boolean') {
    return 'boolean';
  }
  if (typeof value === 'function') {
    return 'function';
  }
  if (value.$brand === '$table') {
    return 'table';
  }
  if (value.$brand === 'image') {
    return 'image';
  }
  if (value.$brand === 'chart') {
    return 'chart';
  }
  if (value.$brand === 'reactor') {
    return 'reactor';
  }
  if (value['$template-not-finished'] !== undefined) {
    return 'template';
  }
  if (value instanceof ContainerRange) {
    return 'range';
  }
  if (isList(value)) {
    return 'list';
  }
  if (Array.isArray(value) && value.length > 100) {
    return 'array';
  }
  if (typeof value === 'object') {
    if (isRTMessage(value)) {
      const messageData: RawRTMessage = value.data;
      if (isSpyValue(messageData)) {
        return 'spy-value';
      }
      if (isSpyMessage(messageData)) {
        return 'spy-message';
      }
    }
    if (isRHSCheck(value)) {
      return 'check';
    }
    if (value.renderKind === 'key-value') {
      return 'key-value';
    }
    if (typeof value.n !== 'undefined' && typeof value.d !== 'undefined'
      && Object.keys(value).length === 2) {
      return 'exactnum';
    }
    return 'object';
  }
  throw new Error('data is not string-convertible');
}
