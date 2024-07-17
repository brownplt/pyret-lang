import { ValueSkeleton } from '../runtime/runtime';
import type {
  DataMetaBase,
  DataValueType,
  Variant,
} from '../runtime/types/primitive-types';
import type { Option } from './option.arr';

declare namespace _exports {
  type ListMethods<a> = {
    find(f: (v: a) => boolean): Option<a>,
    partition(f: (v: a) => boolean): { 'is-true': List<a>, 'is-false': List<a> },
    sort(): List<a>,
    _output(output: (v: a) => ValueSkeleton): ValueSkeleton,
    length(): number,
    member(elt: a): boolean,
    foldr<b>(f: (elt: a, acc: b) => b, base: b): b,
    foldl<b>(f: (elt: a, acc: b) => b, base: b): b,
    all(f: (v: a) => boolean): boolean,
    any(f: (v: a) => boolean): boolean,
    append(other: List<a>): List<a>
    head(): a,
    tail(): List<a>,
    last(): a,
    'sort-by'(cmp: (a1: a, a2: a) => boolean, eq: (a1: a, a2: a) => boolean): List<a>,
    _plus(other: List<a>): List<a>,
    map<b>(f: (a: a) => b): List<b>,
    filter(f: (a: a) => boolean): List<a>,
    each(f: (a: a) => any): void,
    reverse(): List<a>,
    push(elt: a): List<a>,
    'split-at'(n: number): { prefix: List<a>, suffix: List<a> },
    take(n: number): List<a>,
    drop(n: number): List<a>,
    get(n: number): never,
    set(n: number, elt: a): List<a>,
    remove(elt: a): List<a>,
    'join-str'(sep: string): string,
    'join-str-last'(sep: string, lastSep: string): string
  };
  ///////////////////////////// NEW Types ///////////////////////////
  type List<a> = 
    | DataValueType<ListMethods<a>, DataMetaBase<'empty'>>
    | DataValueType<ListMethods<a>, DataMetaBase<'link'> & { 'first': a, 'rest': List<a> }>
  
  
  type UniformMaker<R> = {
    make(arr: Array<R>): R;
    make0(): R,
    make1(a: R): R,
    make2(a: R, b: R): R,
    make3(a: R, b: R, c: R): R,
    make4(a: R, b: R, c: R, d: R): R,
    make5(a: R, b: R, c: R, d: R, e: R): R,  
  }
}

type List<T> = _exports.List<T>
type UniformMaker<T> = _exports.UniformMaker<T>

/////////////////////////// Constructors //////////////////////////
declare const _exports: {
  empty: Variant<List<any>, 'empty'>,
  'is-empty'<a>(val: any): val is Variant<List<a>, 'empty'>;
  link<a>(first: a, rest: List<a>): Variant<List<a>, 'link'>;
  'is-link'<a>(val: any): val is Variant<List<a>, 'link'>;
  list: UniformMaker<unknown>;
}
export = _exports;



