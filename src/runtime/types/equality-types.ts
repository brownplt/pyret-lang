import type { DataValueType, DataMetaBase } from './primitive-types';

export type Equal = DataValueType<{}, DataMetaBase<'Equal'>>;
export type NotEqual = DataValueType<{}, DataMetaBase<'NotEqual'> & { 
  reason: string,
  value1: any,
  value2: any,
}>;
export type Unknown = DataValueType<{}, DataMetaBase<'Unknown'> & { 
  reason: string,
  value1: any,
  value2: any,
}>;

export type EqualityResult = Equal | NotEqual | Unknown
