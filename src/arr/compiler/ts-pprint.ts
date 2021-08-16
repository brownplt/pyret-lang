import type { List, PFunction } from "./ts-impl-types";
import type * as TCH from './ts-codegen-helpers';

export type PPrintDoc = 
  | { $name: "mt-doc", dict: { 'flat-width': Number, 'has-hardline': boolean } }
  | {
    $name: "str",
    dict: { 's': string, 'flat-width': Number, 'has-hardline': boolean }
  }
  | {
    $name: "hardline",
    dict: { 'flat-width': Number, 'has-hardline': boolean }
  }
  | {
    $name: "blank",
    dict: { 'n': Number, 'flat-width': Number, 'has-hardline': boolean }
  }
  | {
    $name: "concat",
    dict: 
      {
        'fst': PPrintDoc,
        'snd': PPrintDoc,
        'flat-width': Number,
        'has-hardline': boolean
      }
  }
  | {
    $name: "nest",
    dict: 
      {
        'indent': Number,
        'd': PPrintDoc,
        'flat-width': Number,
        'has-hardline': boolean
      }
  }
  | {
    $name: "if-flat",
    dict: 
      {
        'flat': PPrintDoc,
        'vert': PPrintDoc,
        'flat-width': Number,
        'has-hardline': boolean
      }
  }
  | {
    $name: "align",
    dict: { 'd': PPrintDoc, 'flat-width': Number, 'has-hardline': boolean }
  }
  | {
    $name: "align-spaces",
    dict: { 'n': Number, 'flat-width': Number, 'has-hardline': boolean }
  }
  | {
    $name: "group",
    dict: { 'd': PPrintDoc, 'flat-width': Number, 'has-hardline': boolean }
  }

export type Item = 
  | {
    $name: "item",
    dict: { 'indent': Number, 'is-flat': boolean, 'd': PPrintDoc }
  }

/////////////////////////// Exports //////////////////////////
export interface Exports {
dict: {values: {dict: {
'is-PPrintDoc': PFunction<(val: any) => val is PPrintDoc>

'is-mt-doc': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'mt-doc'>>

'mt-doc': TCH.Variant<PPrintDoc, 'mt-doc'>

'is-str': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'str'>>

'str': 
  PFunction<
    (s: string) => TCH.Variant<PPrintDoc, 'str'>
  >

'is-hardline': 
  PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'hardline'>>

'hardline': TCH.Variant<PPrintDoc, 'hardline'>

'is-blank': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'blank'>>

'blank': 
  PFunction<
    (n: Number) => TCH.Variant<PPrintDoc, 'blank'>
  >

'is-concat': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'concat'>>

'concat': 
  PFunction<
    (fst: PPrintDoc, snd: PPrintDoc) => TCH.Variant<PPrintDoc, 'concat'>
  >

'is-nest': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'nest'>>

'nest': 
  PFunction<
    (indent: Number, d: PPrintDoc) => TCH.Variant<PPrintDoc, 'nest'>
  >

'is-if-flat': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'if-flat'>>

'if-flat': 
  PFunction<
    (
        flat: PPrintDoc,
        vert: PPrintDoc
      ) => TCH.Variant<PPrintDoc, 'if-flat'>
  >

'is-align': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'align'>>

'align': 
  PFunction<
    (d: PPrintDoc) => TCH.Variant<PPrintDoc, 'align'>
  >

'is-align-spaces': 
  PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'align-spaces'>>

'align-spaces': 
  PFunction<
    (n: Number) => TCH.Variant<PPrintDoc, 'align-spaces'>
  >

'is-group': PFunction<(val: any) => val is TCH.Variant<PPrintDoc, 'group'>>

'group': 
  PFunction<
    (d: PPrintDoc) => TCH.Variant<PPrintDoc, 'group'>
  >

'is-Item': PFunction<(val: any) => val is Item>

'is-item': PFunction<(val: any) => val is TCH.Variant<Item, 'item'>>

'item': 
  PFunction<
    (indent: Number, is_flat: boolean, d: PPrintDoc) => TCH.Variant<Item, 'item'>
  >

  'parens': PFunction<(d: PPrintDoc) => PPrintDoc>,
  'surround-separate': PFunction<(
    n: number,
    b: number,
    voidDoc: PPrintDoc,
    open: PPrintDoc,
    sep: PPrintDoc,
    close: PPrintDoc,
    docs: List<PPrintDoc>
  ) => PPrintDoc>,
  'lbrace': PPrintDoc,
  'commabreak': PPrintDoc,
  'rbrace': PPrintDoc,
  'dquote': PFunction<(d: PPrintDoc) => PPrintDoc>
}}}}
