
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
export type PMethod<Self, T extends (...args: any[]) => any> = { 
  meth: (self: Self) => T,
  full_meth: (self: Self, ...args: Parameters<T>) => ReturnType<T>
}

export interface StringDict<T> {
  dict: {
    get: PMethod<StringDict<T>, (key: string) => Option<T>>,
    'get-value': PMethod<StringDict<T>, (key: string) => T>,
    set: PMethod<StringDict<T>, (key: string, value: T) => StringDict<T>>,
    merge: PMethod<StringDict<T>, (other: StringDict<T>) => StringDict<T>>,
    remove: PMethod<StringDict<T>, (key: string) => StringDict<T>>,
    keys: PMethod<StringDict<T>, () => any>, // TODO: represent sets
    'keys-list': PMethod<StringDict<T>, () => List<string>>,
    'map-keys': PMethod<StringDict<T>, <U>(f: (key: string) => U) => List<U>>,
    'fold-keys': PMethod<StringDict<T>, <U>(f: (key: string, acc: U) => U, init: U) => U>,
    'each-key': PMethod<StringDict<T>, <U>(f: (key: string) => U) => void>,
    count: PMethod<StringDict<T>, () => number>,
    'has-key': PMethod<StringDict<T>, (key: string) => boolean>,
    unfreeze: PMethod<StringDict<T>, () => MutableStringDict<T>>,
    _equals: PMethod<StringDict<T>, (other: any) => EqualityResult>,
  },
  '$underlyingMap': ImmutableMap<T>,
}

export interface MutableStringDict<T> {
  dict: {
    'get-now': PMethod<MutableStringDict<T>, (key: string) => Option<T>>,
    'get-value-now': PMethod<MutableStringDict<T>, (key: string) => T>,
    'set-now': PMethod<MutableStringDict<T>, (key: string, value: T) => MutableStringDict<T>>,
    'merge-now': PMethod<MutableStringDict<T>, (other: MutableStringDict<T>) => MutableStringDict<T>>,
    'remove-now': PMethod<MutableStringDict<T>, (key: string) => MutableStringDict<T>>,
    'keys-now': PMethod<MutableStringDict<T>, () => any>, // TODO: represent sets
    'keys-list-now': PMethod<MutableStringDict<T>, () => List<string>>,
    'map-keys-now': PMethod<MutableStringDict<T>, <U>(f: (key: string) => U) => List<U>>,
    'fold-keys-now': PMethod<MutableStringDict<T>, <U>(f: (key: string, acc: U) => U, init: U) => U>,
    'each-key-now': PMethod<MutableStringDict<T>, <U>(f: (key: string) => U) => void>,
    'count-now': PMethod<MutableStringDict<T>, () => number>,
    'has-key-now': PMethod<MutableStringDict<T>, (key: string) => boolean>,
    freeze: PMethod<MutableStringDict<T>, () => StringDict<T>>,
    _equals: PMethod<MutableStringDict<T>, (other: any) => EqualityResult>,
  },
  '$underlyingDict': Record<string, T>,
}
