import type { PTuple } from '../../runtime/types/primitive-types';
import type { StringDict } from '../../runtime/types/string-dict-types'
import type * as A from './ts-ast';

export type TypeMembers = StringDict<Type>

export type ModuleType = 
  | {
    $name: "t-module",
    dict: 
      {
        'name': string,
        'provides': Type,
        'types': StringDict<DataType>,
        'aliases': StringDict<Type>
      }
  }

export type FieldType = PTuple & { 0: string, 1: Type }

export type TypeVariant = 
  | {
    $name: "t-variant",
    dict: 
      {
        'name': string,
        'fields': A.List<FieldType>,
        'with-fields': TypeMembers,
        'l': A.Srcloc
      }
  }
  | {
    $name: "t-singleton-variant",
    dict: { 'name': string, 'with-fields': TypeMembers, 'l': A.Srcloc }
  }

export type NameOrigin = 
  | { $name: "local", dict: {} }
  | { $name: "module-uri", dict: { 'uri': string } }
  | { $name: "dependency", dict: { 'dep': string } }

export type DataType = 
  | {
    $name: "t-data",
    dict: 
      {
        'name': string,
        'params': A.List<Type>,
        'variants': A.List<TypeVariant>,
        'fields': TypeMembers,
        'l': A.Srcloc
      }
  }

export type Type = 
  | {
    $name: "t-name",
    dict: 
      { 'module-name': NameOrigin, 'id': A.Name, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-arrow",
    dict: { 'args': A.List<Type>, 'ret': Type, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-app",
    dict: { 'onto': Type, 'args': A.List<Type>, 'l': A.Srcloc, 'inferred': boolean }
  }
  | { $name: "t-top", dict: { 'l': A.Srcloc, 'inferred': boolean } }
  | { $name: "t-bot", dict: { 'l': A.Srcloc, 'inferred': boolean } }
  | {
    $name: "t-record",
    dict: { 'fields': TypeMembers, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-tuple",
    dict: { 'elts': A.List<Type>, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-forall",
    dict: 
      { 'introduces': A.List<Type>, 'onto': Type, 'l': A.Srcloc, 'inferred': boolean }
  }
  | { $name: "t-ref", dict: { 'typ': Type, 'l': A.Srcloc, 'inferred': boolean } }
  | {
    $name: "t-data-refinement",
    dict: 
      {
        'data-type': Type,
        'variant-name': string,
        'l': A.Srcloc,
        'inferred': boolean
      }
  }
  | { $name: "t-var", dict: { 'id': A.Name, 'l': A.Srcloc, 'inferred': boolean } }
  | {
    $name: "t-existential",
    dict: { 'id': A.Name, 'l': A.Srcloc, 'inferred': boolean }
  }

