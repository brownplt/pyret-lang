import type {
  DataMetaBase,
  DataValueType,
  Variant
} from '../runtime/types/primitive-types';

declare namespace _exports {
  export type Srcloc = 
  | DataValueType<{
    format(showFile: boolean): string,
    key(): string,
    'same-file'(other: Srcloc): boolean,
    before(other: Srcloc): boolean,
    after(other: Srcloc): boolean,
    contains(other: Srcloc): boolean,
    'is-builtin'(): boolean,
  }, DataMetaBase<'builtin'> & { 'module-name': string }>
  | DataValueType<{
    format(showFile: boolean): string,
    key(): string,
    'same-file'(other: Srcloc): boolean,
    before(other: Srcloc): boolean,
    after(other: Srcloc): boolean,
    'at-start'(): Srcloc,
    'at-end'(): Srcloc,
    _plus(other: Variant<Srcloc, 'srcloc'>): Variant<Srcloc, 'srcloc'>,
    upto(other: Variant<Srcloc, 'srcloc'>): Variant<Srcloc, 'srcloc'>,
    'upto-end'(other: Variant<Srcloc, 'srcloc'>): Variant<Srcloc, 'srcloc'>
    contains(other: Srcloc): boolean,
    'is-builtin'(): boolean,
  }, DataMetaBase<'srcloc'> & {
    'source': string,
    'start-line': number,
    'start-column': number,
    'start-char': number,
    'end-line': number,
    'end-column': number,
    'end-char': number
  }>
}

type Srcloc = _exports.Srcloc;

/////////////////////////// Constructors //////////////////////////
declare const _exports: {
  builtin(module_name: string): Variant<Srcloc, 'builtin'>;

  'is-builtin'(val: any): val is Variant<Srcloc, 'builtin'>;

  srcloc(
      source: string,
      start_line: Number,
      start_column: Number,
      start_char: Number,
      end_line: Number,
      end_column: Number,
      end_char: Number
    ): Variant<Srcloc, 'srcloc'>;

  'is-srcloc'(val: any): val is Variant<Srcloc, 'srcloc'>;
}

export = _exports;