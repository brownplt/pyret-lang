
export type EqualityResult = 
| { $name: "Equal", dict: {} }
| {
  $name: "NotEqual",
  dict: { 'reason': string, 'value1': unknown, 'value2': unknown }
}
| {
  $name: "Unknown",
  dict: { 'reason': string, 'value1': unknown, 'value2': unknown }
}

export type PTuple<T extends any[]> = {
  vals: T
}

export interface ImmutableMap<T> {
  get: (key: string, notSetValue: T) => T,
  set: (key: string, value: T) => ImmutableMap<T>,
  remove: (key: string) => ImmutableMap<T>,
  keys: () => string[],
}


export type List<T> =
  | { $name: 'empty', dict: {} }
  | { $name: 'link', dict: { first: T, rest: List<T> } }
export type Option<T> =
  | { $name: 'none', dict: {} }
  | { $name: 'some', dict: { value: T } }
export type Either<a, b> = 
  | { $name: "left", dict: { 'v': a } }
  | { $name: "right", dict: { 'v': b } }

export type PFunction<T extends Function> = { app: T }

export interface StringDict<T> {
  dict: {
    get: PFunction<(key: string) => Option<T>>,
    'get-value': PFunction<(key: string) => T>,
    set: PFunction<(key: string, value: T) => StringDict<T>>,
    merge: PFunction<(other: StringDict<T>) => StringDict<T>>,
    remove: PFunction<(key: string) => StringDict<T>>,
    keys: PFunction<() => any>, // TODO: represent sets
    'keys-list': PFunction<() => List<string>>,
    'map-keys': PFunction<<U>(f: (key: string) => U) => List<U>>,
    'fold-keys': PFunction<<U>(f: (key: string, acc: U) => U, init: U) => U>,
    'each-key': PFunction<<U>(f: (key: string) => U) => void>,
    count: PFunction<() => number>,
    'has-key': PFunction<(key: string) => boolean>,
    _equals: PFunction<(other: any) => EqualityResult>,
  },
  '$underlyingMap': ImmutableMap<T>,
}

export interface MutableStringDict<T> {
  dict: {
    'get-now': PFunction<(key: string) => Option<T>>,
    'get-value-now': PFunction<(key: string) => T>,
    'set-now': PFunction<(key: string, value: T) => MutableStringDict<T>>,
    'merge-now': PFunction<(other: MutableStringDict<T>) => MutableStringDict<T>>,
    'remove-now': PFunction<(key: string) => MutableStringDict<T>>,
    'keys-now': PFunction<() => any>, // TODO: represent sets
    'keys-list-now': PFunction<() => List<string>>,
    'map-keys-now': PFunction<<U>(f: (key: string) => U) => List<U>>,
    'fold-keys-now': PFunction<<U>(f: (key: string, acc: U) => U, init: U) => U>,
    'each-key-now': PFunction<<U>(f: (key: string) => U) => void>,
    'count-now': PFunction<() => number>,
    'has-key-now': PFunction<(key: string) => boolean>,
    _equals: PFunction<(other: any) => EqualityResult>,
  },
  '$underlyingDict': Record<string, T>,
}
