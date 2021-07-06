import type { PFunction, List } from '../compiler/ts-impl-types';
import type { Variant } from '../compiler/ts-codegen-helpers';
import type { Srcloc } from '../compiler/ts-srcloc';

export type ErrorDisplay = 
  | { $name: "paragraph", dict: { 'contents': List<ErrorDisplay> } }
  | { $name: "bulleted-sequence", dict: { 'contents': List<ErrorDisplay> } }
  | { $name: "v-sequence", dict: { 'contents': List<ErrorDisplay> } }
  | { $name: "h-sequence", dict: { 'contents': List<ErrorDisplay>, 'sep': string } }
  | {
    $name: "h-sequence-sep",
    dict: { 'contents': List<ErrorDisplay>, 'sep': string, 'last': string }
  }
  | { $name: "embed", dict: { 'val': any } }
  | { $name: "text", dict: { 'str': string } }
  | { $name: "loc", dict: { 'loc': Srcloc } }
  | {
    $name: "maybe-stack-loc",
    dict: 
      {
        'n': number,
        'user-frames-only': boolean,
        'contents-with-loc': PFunction<(loc: Srcloc) => ErrorDisplay>,
        'contents-without-loc': ErrorDisplay
      }
  }
  | { $name: "code", dict: { 'contents': ErrorDisplay } }
  | { $name: "cmcode", dict: { 'loc': Srcloc } }
  | {
    $name: "loc-display",
    dict: { 'loc': Srcloc, 'style': string, 'contents': ErrorDisplay }
  }
  | { $name: "optional", dict: { 'contents': ErrorDisplay } }
  | {
    $name: "highlight",
    dict: { 'contents': ErrorDisplay, 'locs': Variant<List<Srcloc>, 'link'>, 'color': Number }
  }

/////////////////////////// Exports //////////////////////////
export interface Exports {
dict: {values: {dict: {
'is-ErrorDisplay': PFunction<(val: any) => boolean>

'is-paragraph': PFunction<(val: any) => boolean>

'paragraph': 
  PFunction< (contents: unknown) => Variant<ErrorDisplay, 'paragraph'> >

'is-bulleted-sequence': PFunction<(val: any) => boolean>

'bulleted-sequence': 
  PFunction< (contents: unknown) => Variant<ErrorDisplay, 'bulleted-sequence'> >

'is-v-sequence': PFunction<(val: any) => boolean>

'v-sequence': 
  PFunction< (contents: unknown) => Variant<ErrorDisplay, 'v-sequence'> >

'is-h-sequence': PFunction<(val: any) => boolean>

'h-sequence': 
  PFunction<
    (contents: unknown, sep: string) => Variant<ErrorDisplay, 'h-sequence'>
  >

'is-h-sequence-sep': PFunction<(val: any) => boolean>

'h-sequence-sep': 
  PFunction<
    (contents: unknown, sep: string, last: string) => Variant<ErrorDisplay, 'h-sequence-sep'>
  >

'is-embed': PFunction<(val: any) => boolean>

'embed': PFunction< (val: any) => Variant<ErrorDisplay, 'embed'> >

'is-text': PFunction<(val: any) => boolean>

'text': PFunction< (str: string) => Variant<ErrorDisplay, 'text'> >

'is-loc': PFunction<(val: any) => boolean>

'loc': PFunction< (loc: unknown) => Variant<ErrorDisplay, 'loc'> >

'is-maybe-stack-loc': PFunction<(val: any) => boolean>

'maybe-stack-loc': 
  PFunction<
    (
        n: Number,
        user_frames_only: boolean,
        contents_with_loc: unknown,
        contents_without_loc: ErrorDisplay
      ) => Variant<ErrorDisplay, 'maybe-stack-loc'>
  >

'is-code': PFunction<(val: any) => boolean>

'code': PFunction< (contents: ErrorDisplay) => Variant<ErrorDisplay, 'code'> >

'is-cmcode': PFunction<(val: any) => boolean>

'cmcode': PFunction< (loc: unknown) => Variant<ErrorDisplay, 'cmcode'> >

'is-loc-display': PFunction<(val: any) => boolean>

'loc-display': 
  PFunction<
    (loc: unknown, style: string, contents: ErrorDisplay) => Variant<ErrorDisplay, 'loc-display'>
  >

'is-optional': PFunction<(val: any) => boolean>

'optional': 
  PFunction< (contents: ErrorDisplay) => Variant<ErrorDisplay, 'optional'> >

'is-highlight': PFunction<(val: any) => boolean>

'highlight': 
  PFunction<
    (contents: ErrorDisplay, locs: unknown, color: Number) => Variant<ErrorDisplay, 'highlight'>
  >

}}}}

