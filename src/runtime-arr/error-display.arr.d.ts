import type {
  DataMetaBase,
  DataValueType
} from '../runtime/types/primitive-types';
import type { Srcloc } from './srcloc.arr';

type RawArray<T> = Array<T>;
type Variant<T, V> = T & { $name: V }

type UniformMaker<R> = {
  make(arr: Array<R>): R;
  make0(): R,
  make1(a: R): R,
  make2(a: R, b: R): R,
  make3(a: R, b: R, c: R): R,
  make4(a: R, b: R, c: R, d: R): R,
  make5(a: R, b: R, c: R, d: R, e: R): R,  
}

declare namespace _exports {
  type ErrorDisplay = 
    | DataValueType<{}, DataMetaBase<'paragraph'> & {
      'contents': RawArray<ErrorDisplay>
    }>
    | DataValueType<{}, DataMetaBase<'bulleted-sequence'> & {
      'contents': RawArray<ErrorDisplay>
    }>
    | DataValueType<{}, DataMetaBase<'v-sequence'> & {
      'contents': RawArray<ErrorDisplay>
    }>
    | DataValueType<{}, DataMetaBase<'h-sequence'> & {
      'contents': RawArray<ErrorDisplay>,
      'sep': string
    }>
    | DataValueType<{}, DataMetaBase<'h-sequence-sep'> & {
      'contents': RawArray<ErrorDisplay>,
      'sep': string,
      'last': string
    }>
    | DataValueType<{}, DataMetaBase<'embed'> & { 'val': any }>
    | DataValueType<{}, DataMetaBase<'text'> & { 'str': string }>
    | DataValueType<{}, DataMetaBase<'loc'> & { 'loc': Srcloc }>
    | DataValueType<{}, DataMetaBase<'maybe-stack-loc'> & {
      'n': Number,
      'user-frames-only': boolean,
      'contents-with-loc': ((s : Srcloc) => ErrorDisplay),
      'contents-without-loc': ErrorDisplay
    }>
    | DataValueType<{}, DataMetaBase<'code'> & { 'contents': ErrorDisplay }>
    | DataValueType<{}, DataMetaBase<'cmcode'> & { 'loc': Srcloc }>
    | DataValueType<{}, DataMetaBase<'loc-display'> & {
      'loc': Srcloc,
      'style': string,
      'contents': ErrorDisplay
    }>
    | DataValueType<{}, DataMetaBase<'optional'> & { 'contents': ErrorDisplay }>
    | DataValueType<{}, DataMetaBase<'highlight'> & {
      'contents': ErrorDisplay,
      'locs': RawArray<Srcloc>,
      'color': Number
    }>
}

type ErrorDisplay = _exports.ErrorDisplay;

declare const _exports: {

  paragraph(contents: RawArray<ErrorDisplay>): Variant<ErrorDisplay, 'paragraph'>;

  'is-paragraph'(val: any): val is Variant<ErrorDisplay, 'paragraph'>;

  'bulleted-sequence'(contents: RawArray<ErrorDisplay>): Variant<ErrorDisplay, 'bulleted-sequence'>;

  'is-bulleted-sequence'(val: any): val is Variant<ErrorDisplay, 'bulleted-sequence'>;

  'v-sequence'(contents: RawArray<ErrorDisplay>): Variant<ErrorDisplay, 'v-sequence'>;

  'is-v-sequence'(val: any): val is Variant<ErrorDisplay, 'v-sequence'>;

  'h-sequence'(contents: RawArray<ErrorDisplay>, sep: string): Variant<ErrorDisplay, 'h-sequence'>;

  'ish-sequence'(val: any): val is Variant<ErrorDisplay, 'h-sequence'>;

  'h-sequence-sep'(
      contents: RawArray<ErrorDisplay>,
      sep: string,
      last: string
    ): Variant<ErrorDisplay, 'h-sequence-sep'>;

  'is-h-sequence-sep'(val: any): Variant<ErrorDisplay, 'h-sequence-sep'>;

  embed(val: any): Variant<ErrorDisplay, 'embed'>;

  'is-embed'(val: any): val is Variant<ErrorDisplay, 'is-embed'>;

  text(str: string): Variant<ErrorDisplay, 'text'>;

  'is-text'(val: any): val is Variant<ErrorDisplay, 'text'>;

  loc(loc: Srcloc): Variant<ErrorDisplay, 'loc'>;

  'is-loc'(val: any): val is Variant<ErrorDisplay, 'loc'>;

  'maybe-stack-loc'(
      n: Number,
      userFramesOnly: boolean,
      contentsWithLoc: (s : Srcloc) => ErrorDisplay,
      contentsWithoutLoc: ErrorDisplay
    ): Variant<ErrorDisplay, 'maybe-stack-loc'>;

  'is-maybe-stack-loc'(val: any): val is Variant<ErrorDisplay, 'maybe-stack-loc'>;

  code(contents: ErrorDisplay): Variant<ErrorDisplay, 'code'>;

  'is-code'(val: any): val is Variant<ErrorDisplay, 'code'>;

  cmcode(loc: Srcloc): Variant<ErrorDisplay, 'cmcode'>;

  'is-cmcode'(val: any): val is Variant<ErrorDisplay, 'cmcode'>;

  'loc-display'(
      loc: Srcloc,
      style: string,
      contents: ErrorDisplay
    ): Variant<ErrorDisplay, 'loc-display'>;

  'is-loc-display'(val: any): val is Variant<ErrorDisplay, 'loc-display'>;

  optional(contents: ErrorDisplay): Variant<ErrorDisplay, 'optional'>;

  isoptional(val: any): val is Variant<ErrorDisplay, 'optional'>;

  highlight(
      contents: ErrorDisplay,
      locs: RawArray<Srcloc>,
      color: Number
    ): Variant<ErrorDisplay, 'highlight'>;

  ishighlight(val: any): val is Variant<ErrorDisplay, 'highlight'>;

  para: UniformMaker<ErrorDisplay>;
  'para-nospace': UniformMaker<ErrorDisplay>;
  sequence: UniformMaker<ErrorDisplay>;
  vert: UniformMaker<ErrorDisplay>;
  error: UniformMaker<ErrorDisplay>;

  bulleted: UniformMaker<ErrorDisplay>;
  opt: UniformMaker<ErrorDisplay>;
}
export = _exports;
