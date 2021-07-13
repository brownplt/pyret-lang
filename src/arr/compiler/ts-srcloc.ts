
import type { List, PFunction, PTuple, StringDict } from './ts-impl-types';
import type * as TCH from './ts-codegen-helpers';
///////////////////////////// OLD Types ///////////////////////////
export type Srcloc = 
  | { $name: "builtin", dict: { 'module-name': string } }
  | {
    $name: "srcloc",
    dict: 
      {
        'source': string,
        'start-line': number,
        'start-column': number,
        'start-char': number,
        'end-line': number,
        'end-column': number,
        'end-char': number
      }
  }

/////////////////////////// Exports //////////////////////////
export interface Exports {
dict: {values: {dict: {
  'is-Srcloc': PFunction<(val: any) => val is Srcloc>

  'is-builtin': PFunction<(val: any) => val is TCH.Variant<Srcloc, 'builtin'>>
  
  'builtin': PFunction< (module_name: unknown) => TCH.Variant<Srcloc, 'builtin'> >
  
  'is-srcloc': PFunction<(val: any) => val is TCH.Variant<Srcloc, 'srcloc'>>
  
  'srcloc': 
    PFunction<
      (
          source: string,
          start_line: number,
          start_column: number,
          start_char: number,
          end_line: number,
          end_column: number,
          end_char: number
        ) => TCH.Variant<Srcloc, 'srcloc'>
    >
}}}}