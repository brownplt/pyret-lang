import type {
  DataMetaBase,
  DataValueType,
  Variant
} from '../runtime/types/primitive-types';

///////////////////////////// NEW Types ///////////////////////////
declare namespace _exports {
  export type Option<a> = 
    | DataValueType<{
      'or-else'(v: a): a,
      'and-then'<b>(f: ((v: a) => b)): Option<b>
    }, DataMetaBase<'some'> & { 'value': a }>
    | DataValueType<{
      'or-else'(v: a): a,
      'and-then'<b>(f: ((v: a) => b)): Option<b>
    }, DataMetaBase<'none'>>
}

type Option<a> = _exports.Option<a>

/////////////////////////// Constructors //////////////////////////
declare const _exports: {
  some<a>(value: a): Variant<Option<a>, 'some'>;

  'is-some'(val: any): val is Variant<Option<unknown>, 'some'>;

  none : Variant<Option<any>, 'none'>;

  'is-none'<a>(val: any): val is Variant<Option<a>, 'none'>
}
export = _exports;
