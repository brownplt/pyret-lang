import type * as A from './ts-ast';
export type CompileMode =
  | { $name: 'cm-builtin-stage-1', dict: {} }
  | { $name: 'cm-builtin-general', dict: {} }
  | { $name: 'cm-normal', dict: {} }

export type Dependency =
  | { $name: 'dependency', dict: {protocol : string, arguments : A.List<string>} }
  | { $name: 'builtin', dict: {modname : String} }