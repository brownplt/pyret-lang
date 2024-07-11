import type { WriteStream } from "fs"

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

export type PObject<T extends {}> = { dict: T }

export type POpaque<T> = { val: T }

export interface ImmutableMap<T> {
  get: (key: string, notSetValue: T | null) => T,
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

export type PFunction<T extends Function> = { app: T, name: string }
export type PMethod<Self, T extends (...args: any[]) => any> = { 
  meth: (self: Self) => T,
  full_meth: (self: Self, ...args: Parameters<T>) => ReturnType<T>,
  name: string,
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

export type PausePackage<A> = {
  break: () => never,
  error: (err: any) => never,
  resume: (val: A) => never,
}

export type RunResult<T> = SuccessResult<T> | FailureResult<any>
export type SuccessResult<T> = { result: T, stats: any }
export type FailureResult<T> = { exn: T, stats: any }

export type Runtime = {
  pauseStack: <A>(thunk: (restarter: PausePackage<A>) => void) => A,
  runThunk: <A, B>(thunk: () => A, then: (val: RunResult<A>) => B) => never,
  safeCall: <A, B>(thunk: () => A, then: (val: A) => B, name?: string) => B,
  isSuccessResult: <A>(val: RunResult<A>) => val is SuccessResult<A>,
  isFailureResult: <A>(val: RunResult<A>) => val is FailureResult<A>,
  raw_array_map: <A, B>(f: PFunction<(arg: A) => B>, arr: A[]) => B[],
  makeTuple: (<T1, T2>(vals : [T1, T2]) => PTuple<[T1, T2]>) 
           & (<T1, T2, T3>(vals : [T1, T2, T3]) => PTuple<[T1, T2, T3]>)
           & (<T1, T2, T3, T4>(vals : [T1, T2, T3, T4]) => PTuple<[T1, T2, T3, T4]>)
           & (<T1, T2, T3, T4, T5>(vals : [T1, T2, T3, T4, T5]) => PTuple<[T1, T2, T3, T4, T5]>),
  makeFunction: <T extends Function>(func: T, name?: string) => PFunction<T>,
  makeMethod: <Self, T extends (...args: any[]) => any>(method: (self: Self) => T, fullMethod: (self: Self, ...args: Parameters<T>) => ReturnType<T>, name?: string) => PMethod<Self, T>,
  makeModuleReturn: (values: Record<string, any>, types: Record<string, any>) => any,
  makeObject: <T extends {}>(val : T) => { dict: T },
  makeString: (s: string) => string,
  makeBoolean: (b: boolean) => boolean,
  makeOpaque: <T>(val: T) => POpaque<T>,
  checkString: (val: any) => void,
  checkBoolean: (val: any) => void,
  checkOpaque: (val: any) => void,
  unwrap: (val: any) => any,
  stdout: WriteStream['write'],
  stderr: WriteStream['write'],
  nothing: unknown,
  ffi: {
    makeList: <T>(ts: T[]) => List<T>,
    makeTreeSet: <T>(ts: T[]) => Set<T>,
    makeSome: <T>(val: T) => Option<T>,
    makeNone: <T>() => Option<T>,
    makeMessageException: (msg: string) => any,
    throwMessageException: (msg: string) => any,
    checkArity: (arity: number, args: IArguments, ... rest: any[]) => void,
  }
}
