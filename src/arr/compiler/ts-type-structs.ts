import type { List, PFunction, PTuple, StringDict } from './ts-impl-types';
import type { Variant } from './ts-codegen-helpers';
import type { Name, Srcloc as Loc } from './ts-ast';
import type * as A from './ts-ast';

export type TypeMembers = StringDict<Type>

export type ModuleType = 
  | {
    $name: "t-module",
    dict: 
      {
        'name': string,
        'provides': Variant<Type, "t-record">,
        'types': StringDict<DataType>,
        'aliases': StringDict<Type>
      }
  }

export type FieldType = PTuple<[string, Type]>

export type TypeVariant = 
  | {
    $name: "t-variant",
    dict: 
      {
        'name': string,
        'fields': List<FieldType>,
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
        'params': List<Variant<Type, 't-var'>>,
        'variants': List<TypeVariant>,
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
    dict: { 'args': List<Type>, 'ret': Type, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-app",
    dict: { 'onto': Type, 'args': List<Type>, 'l': A.Srcloc, 'inferred': boolean }
  }
  | { $name: "t-top", dict: { 'l': A.Srcloc, 'inferred': boolean } }
  | { $name: "t-bot", dict: { 'l': A.Srcloc, 'inferred': boolean } }
  | {
    $name: "t-record",
    dict: { 'fields': TypeMembers, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-tuple",
    dict: { 'elts': List<Type>, 'l': A.Srcloc, 'inferred': boolean }
  }
  | {
    $name: "t-forall",
    dict: 
      { 'introduces': List<Variant<Type, 't-var'>>, 'onto': Type, 'l': A.Srcloc, 'inferred': boolean }
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

export interface Exports {
  dict: {
    values: {
      dict: {
        't-module': 
          PFunction<
            (
                name: string,
                provides: Type,
                types: StringDict<DataType>,
                aliases: StringDict<Type>
              )=> Variant<ModuleType, 't-module'>
          >

        't-variant': 
          PFunction<
            (
                name: string,
                fields: List<PTuple<[ string, Type ]>>,
                with_fields: TypeMembers,
                l: Loc
              )=> Variant<TypeVariant, 't-variant'>
          >

        't-singleton-variant': 
          PFunction<
            (name: string, with_fields: TypeMembers, l: Loc)=> Variant<TypeVariant, 't-singleton_variant'>
          >

        'local': Variant<NameOrigin, 'local'>

        'module-uri': PFunction< (uri: string)=> Variant<NameOrigin, 'module_uri'> >

        'dependency': PFunction< (dep: string)=> Variant<NameOrigin, 'dependency'> >

        't-data': 
          PFunction<
            (
                name: string,
                params: List<Variant<Type, 't-var'>>,
                variants: List<TypeVariant>,
                fields: TypeMembers,
                l: Loc
              )=> Variant<DataType, 't-data'>
          >

        't-name': 
          PFunction<
            (module_name: NameOrigin, id: Name, l: Loc, inferred: boolean)=> Variant<Type, 't-name'>
          >

        't-arrow': 
          PFunction<
            (args: List<Type>, ret: Type, l: Loc, inferred: boolean)=> Variant<Type, 't-arrow'>
          >

        't-app': 
          PFunction<
            (onto: Type, args: List<Type>, l: Loc, inferred: boolean)=> Variant<Type, 't-app'>
          >

        't-top': PFunction< (l: Loc, inferred: boolean)=> Variant<Type, 't-top'> >

        't-bot': PFunction< (l: Loc, inferred: boolean)=> Variant<Type, 't-bot'> >

        't-record': 
          PFunction<
            (fields: TypeMembers, l: Loc, inferred: boolean)=> Variant<Type, 't-record'>
          >

        't-tuple': 
          PFunction<
            (elts: List<Type>, l: Loc, inferred: boolean)=> Variant<Type, 't-tuple'>
          >

        't-forall': 
          PFunction<
            (introduces: List<Variant<Type, 't-var'>>, onto: Type, l: Loc, inferred: boolean)=> Variant<Type, 't-forall'>
          >

        't-ref': 
          PFunction< (typ: Type, l: Loc, inferred: boolean)=> Variant<Type, 't-ref'> >

        't-data-refinement': 
          PFunction<
            (data_type: Type, variant_name: string, l: Loc, inferred: boolean)=> Variant<Type, 't-data_refinement'>
          >

        't-var': 
          PFunction< (id: Name, l: Loc, inferred: boolean)=> Variant<Type, 't-var'> >

        't-existential': 
          PFunction<
            (id: Name, l: Loc, inferred: boolean)=> Variant<Type, 't-existential'>
          >
      }
    }
  }
}