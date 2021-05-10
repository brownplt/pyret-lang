import type * as A from './ts-ast';
export type CompileMode =
  | { $name: 'cm-builtin-stage-1', dict: {} }
  | { $name: 'cm-builtin-general', dict: {} }
  | { $name: 'cm-normal', dict: {} }

export type Dependency =
  | { $name: 'dependency', dict: {protocol : string, arguments : A.List<string>} }
  | { $name: 'builtin', dict: {modname : string} }

export type BindOrigin =
  | { $name: 'bind-origin', dict: {'local-bind-site' : A.Srcloc, 'definition-bind-site' : A.Srcloc, 'new-definition' : boolean, 'uri-of-definition' : string, 'original-name' : A.Name}}

export type ValueBinder =
  | { $name: 'vb-letrec', dict: {} }
  | { $name: 'vb-let', dict: {} }
  | { $name: 'vb-var', dict: {} }

export type ValueBind =
  | { $name: 'value-bind', dict: { origin : BindOrigin, binder : ValueBinder, atom : A.Name, ann : A.Ann }}