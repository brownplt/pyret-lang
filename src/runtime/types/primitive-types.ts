export type PTuple<T extends any[]> = T & {
  $brand: string,
}

export interface DataValue {
  $brand: any,
  [key: string]: any
}

export interface PRef {
  $brand: string,
  ref: Object,
}

// From https://stackoverflow.com/a/57683652/783424, which
// helps make the types more readable
export type ExpandRecursively<T> = 
  T extends (...args: infer A) => infer R ? (...args : A) => R
  : T extends string ? T 
  : T extends object ? (T extends infer O ? { [K in keyof O]: ExpandRecursively<O[K]> } : never)
  : T;

export type DataSharedBase = {
  $methods: Record<string, () => any>,
};

export type DataVariantBase = {
  [x: string]: any;
  $methods?: Record<string, () => any>,
};

export type DataMetaBase<N extends string> = {
  $data: DataSharedBase,
  $name: N,
  $variant?: number,
  $fieldNames?: string[],
  $methods?: Record<string, () => any>,
};

export type VariantType<T extends string> = ExpandRecursively<DataSharedBase & DataVariantBase & Required<DataMetaBase<T>>>;
export type DataValueType<O extends {}, E extends DataVariantBase> = O & Required<E>;
export type Variant<T, V> = T & { $name: V }