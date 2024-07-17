import type {
  DataMetaBase,
  DataValueType, 
  Variant
} from '../runtime/types/primitive-types';

///////////////////////////// NEW Types ///////////////////////////
declare namespace _exports {
  export type Either<a, b> = 
    | DataValueType<{}, DataMetaBase<'left'> & { 'v': a }>
    | DataValueType<{}, DataMetaBase<'right'> & { 'v': b }>
}

type Either<a, b> = _exports.Either<a, b>

/////////////////////////// Constructors //////////////////////////
declare const _exports: {
  left<a, b>(value: a): Variant<Either<a, b>, 'left'>;

  'is-left'(val: any): val is Variant<Either<unknown, unknown>, 'left'>;

  right<a, b>(value: b): Variant<Either<a, b>, 'right'>;

  'is-right'(val: any): val is Variant<Either<unknown, unknown>, 'right'>;
}
export = _exports;
