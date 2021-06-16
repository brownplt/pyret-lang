
import type { EqualityResult } from './equality-types';


export interface ImmutableMap<T> {
  get: (key: string, notSetValue: T) => T,
  set: (key: string, value: T) => ImmutableMap<T>,
  remove: (key: string) => ImmutableMap<T>,
  keys: () => string[],
}


type List<T> =
  | { $name: 'empty', dict: {} }
  | { $name: 'link', dict: { first: T, rest: List<T> } }
type Option<T> =
  | { $name: 'none', dict: {} }
  | { $name: 'some', dict: { value: T } }

export interface StringDict<T> {
  get: (key: string) => Option<T>,
  'get-value': (key: string) => T,
  set: (key: string, value: T) => StringDict<T>,
  merge: (other: StringDict<T>) => StringDict<T>,
  remove: (key: string) => StringDict<T>,
  keys: () => any, // TODO: represent sets
  'keys-list': () => List<string>,
  'map-keys': <U>(f: (key: string) => U) => List<U>,
  'fold-keys': <U>(f: (key: string, acc: U) => U, init: U) => U,
  'each-key': <U>(f: (key: string) => U) => void,
  count: () => number,
  'has-key': (key: string) => boolean,
  _equals: (other: any) => EqualityResult,
  '$underlyingMap': ImmutableMap<T>,
}

export interface MutableStringDict<T> {
  'get-now': (key: string) => Option<T>,
  'get-value-now': (key: string) => T,
  'set-now': (key: string, value: T) => MutableStringDict<T>,
  'merge-now': (other: MutableStringDict<T>) => MutableStringDict<T>,
  'remove-now': (key: string) => MutableStringDict<T>,
  'keys-now': () => any, // TODO: represent sets
  'keys-list-now': () => List<string>,
  'map-keys-now': <U>(f: (key: string) => U) => List<U>,
  'fold-keys-now': <U>(f: (key: string, acc: U) => U, init: U) => U,
  'each-key-now': <U>(f: (key: string) => U) => void,
  'count-now': () => number,
  'has-key-now': (key: string) => boolean,
  _equals: (other: any) => EqualityResult,
  '$underlyingMap': Record<string, T>,
}
