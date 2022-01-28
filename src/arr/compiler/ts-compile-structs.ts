import type * as A from './ts-ast';
import type {Srcloc as Loc} from './ts-ast';
import type * as T from './ts-type-structs';
import { Option, List, StringDict, MutableStringDict, PFunction, PMethod } from './ts-impl-types';
import type * as ED from './error-display'
import type { Variant } from './ts-codegen-helpers';
import type { CompiledCodePrinter } from './ts-js-of-pyret';

export type URI = string;

export type CompileMode = 
  | { $name: "cm-builtin-stage-1", dict: {} }
  | { $name: "cm-builtin-general", dict: {} }
  | { $name: "cm-normal", dict: {} }

export type Dependency = 
  | {
    $name: "dependency",
    dict: { 
      'protocol': string,
      'arguments': List<string>,
      'key'?: PMethod<Dependency, () => string>,
    }
  }
  | { 
    $name: "builtin", 
    dict: {
      'modname': string,
      'key'?: PMethod<Dependency, () => string>,
    }
  }

export type NativeModule = | { $name: "requirejs", dict: { 'path': string } }

export type BindOrigin = 
  | {
    $name: "bind-origin",
    dict: 
      {
        'local-bind-site': A.Srcloc,
        'definition-bind-site': A.Srcloc,
        'new-definition': boolean,
        'uri-of-definition': URI,
        'original-name': A.Name
      }
  }

export type ValueBinder = 
  | { $name: "vb-letrec", dict: {} }
  | { $name: "vb-let", dict: {} }
  | { $name: "vb-var", dict: {} }

export type ValueBind = 
  | {
    $name: "value-bind",
    dict: 
      {
        'origin': BindOrigin,
        'binder': ValueBinder,
        'atom': A.Name,
        'ann': A.Ann
      }
  }

export type TypeBinder = 
  | { $name: "tb-type-let", dict: {} }
  | { $name: "tb-type-var", dict: {} }

export type TypeBindTyp = 
  | { $name: "tb-typ", dict: { 'typ': T.Type } }
  | { $name: "tb-none", dict: {} }

export type TypeBind = 
  | {
    $name: "type-bind",
    dict: 
      {
        'origin': BindOrigin,
        'binder': TypeBinder,
        'atom': A.Name,
        'typ': TypeBindTyp
      }
  }

export type ModuleBind = 
  | {
    $name: "module-bind",
    dict: { 'origin': BindOrigin, 'atom': A.Name, 'uri': URI }
  }

export type ScopeResolution = 
  | {
    $name: "resolved-scope",
    dict: { 'ast': A.Program, 'errors': List<CompileError> }
  }

export type ComputedEnvironment = 
  | { $name: "computed-none", dict: {} }
  | {
    $name: "computed-env",
    dict: 
      {
        'module-bindings': MutableStringDict<ModuleBind>,
        'bindings': MutableStringDict<ValueBind>,
        'type-bindings': MutableStringDict<TypeBind>,
        'datatypes': MutableStringDict<A.Expr>,
        'module-env': StringDict<ModuleBind>,
        'env': StringDict<ValueBind>,
        'type-env': StringDict<TypeBind>
      }
  }

export type NameResolution = 
  | {
    $name: "resolved-names",
    dict: 
      {
        'ast': A.Program,
        'errors': List<CompileError>,
        'env': Variant<ComputedEnvironment, 'computed-env'>
      }
  }

export type ExtraImports = 
  | { $name: "extra-imports", dict: { 'imports': List<ExtraImport> } }

export type ExtraImport = 
  | {
    $name: "extra-import",
    dict: 
      {
        'dependency': Dependency,
        'as-name': string,
        'values': List<string>,
        'types': List<string>
      }
  }

export type Loadable = 
  | {
    $name: "module-as-string",
    dict: 
      {
        'provides': Provides,
        'compile-env': CompileEnvironment,
        'post-compile-env': ComputedEnvironment,
        'result-printer': CompileResult<CompiledCodePrinter>
      }
  }

export type CompileEnvironment = 
  | {
    $name: "compile-env",
    dict: 
      {
        'globals': Globals,
        'all-modules': MutableStringDict<Loadable>,
        'my-modules': StringDict<URI>
      }
  }

export type Globals = 
  | {
    $name: "globals",
    dict: 
      {
        'modules': StringDict<BindOrigin>,
        'values': StringDict<BindOrigin>,
        'types': StringDict<BindOrigin>
      }
  }

export type ValueExport = 
  | {
    $name: "v-alias",
    dict: { 'origin': BindOrigin, 'original-name': string }
  }
  | { $name: "v-just-type", dict: { 'origin': BindOrigin, 't': T.Type } }
  | { $name: "v-var", dict: { 'origin': BindOrigin, 't': T.Type } }
  | {
    $name: "v-fun",
    dict: 
      {
        'origin': BindOrigin,
        't': T.Type,
        'name': string,
        'flatness': A.Option<number>
      }
  }

export type DataExport = 
  | { $name: "d-alias", dict: { 'origin': BindOrigin, 'name': string } }
  | { $name: "d-type", dict: { 'origin': BindOrigin, 'typ': T.DataType } }

export type Provides = 
  | {
    $name: "provides",
    dict: 
      {
        'from-uri': URI,
        'modules': StringDict<URI>,
        'values': StringDict<ValueExport>,
        'aliases': StringDict<T.Type>,
        'data-definitions': StringDict<DataExport>
      }
  }

export type CompileResult<C> = 
  | { $name: "ok", dict: { 'code': C } }
  | { $name: "err", dict: { 'problems': List<CompileError> } }

export type CompileError = 
  | { $name: "wf-err", dict: { 'msg': List<ED.ErrorDisplay>, 'loc': A.Srcloc } }
  | { $name: "wf-empty-block", dict: { 'loc': A.Srcloc } }
  | { $name: "wf-err-split", dict: { 'msg': string, 'loc': List<A.Srcloc> } }
  | { $name: "wf-bad-method-expression", dict: { 'method-expr-loc': A.Srcloc } }
  | { $name: "reserved-name", dict: { 'loc': A.Srcloc, 'id': string } }
  | {
    $name: "contract-on-import",
    dict: 
      { 'loc': A.Srcloc, 'name': string, 'import-loc': A.Srcloc, 'import-uri': string }
  }
  | {
    $name: "contract-redefined",
    dict: { 'loc': A.Srcloc, 'name': string, 'defn-loc': A.Srcloc }
  }
  | {
    $name: "contract-non-function",
    dict: 
      {
        'loc': A.Srcloc,
        'name': string,
        'defn-loc': A.Srcloc,
        'defn-is-function': boolean
      }
  }
  | {
    $name: "contract-inconsistent-names",
    dict: { 'loc': A.Srcloc, 'name': string, 'defn-loc': A.Srcloc }
  }
  | {
    $name: "contract-inconsistent-params",
    dict: { 'loc': A.Srcloc, 'name': string, 'defn-loc': A.Srcloc }
  }
  | { $name: "contract-unused", dict: { 'loc': A.Srcloc, 'name': string } }
  | {
    $name: "contract-bad-loc",
    dict: { 'loc': A.Srcloc, 'name': string, 'defn-loc': A.Srcloc }
  }
  | { $name: "zero-fraction", dict: { 'loc': any, 'numerator': any } }
  | {
    $name: "mixed-binops",
    dict: 
      {
        'exp-loc': any,
        'op-a-name': any,
        'op-a-loc': any,
        'op-b-name': any,
        'op-b-loc': any
      }
  }
  | { $name: "block-ending", dict: { 'l': A.Srcloc, 'block-loc': A.Srcloc, 'kind': any } }
  | { $name: "single-branch-if", dict: { 'expr': A.Expr } }
  | {
    $name: "unwelcome-where",
    dict: { 'kind': any, 'loc': any, 'block-loc': any }
  }
  | { $name: "non-example", dict: { 'expr': A.Expr } }
  | {
    $name: "tuple-get-bad-index",
    dict: { 'l': any, 'tup': any, 'index': any, 'index-loc': any }
  }
  | {
    $name: "import-arity-mismatch",
    dict: 
      {
        'l': any,
        'kind': any,
        'args': any,
        'expected-arity': any,
        'expected-args': any
      }
  }
  | { $name: "no-arguments", dict: { 'expr': Variant<A.Member, "s-method-field"> } }
  | {
    $name: "non-toplevel",
    dict: { 'kind': any, 'l': A.Srcloc, 'parent-loc': A.Srcloc }
  }
  | { $name: "unwelcome-test", dict: { 'loc': A.Srcloc } }
  | {
    $name: "unwelcome-test-refinement",
    dict: { 'refinement': any, 'op': any }
  }
  | { $name: "underscore-as", dict: { 'l': A.Srcloc, 'kind': any } }
  | { $name: "underscore-as-pattern", dict: { 'l': A.Srcloc } }
  | { $name: "underscore-as-expr", dict: { 'l': A.Srcloc } }
  | { $name: "underscore-as-ann", dict: { 'l': A.Srcloc } }
  | {
    $name: "block-needed",
    dict: { 'expr-loc': A.Srcloc, 'blocks': List<Variant<A.Expr, 's-block'>> }
  }
  | {
    $name: "name-not-provided",
    dict: { 'name-loc': any, 'imp-loc': any, 'name': A.Name, 'typ': string }
  }
  | { $name: "unbound-id", dict: { 'id': A.Expr } }
  | { $name: "unbound-var", dict: { 'id': string, 'loc': A.Srcloc } }
  | { $name: "unbound-type-id", dict: { 'ann': A.Ann } }
  | {
    $name: "type-id-used-in-dot-lookup",
    dict: { 'loc': A.Srcloc, 'name': A.Name }
  }
  | {
    $name: "type-id-used-as-value",
    dict: { 'id': A.Name, 'origin': BindOrigin }
  }
  | { $name: "unexpected-type-var", dict: { 'loc': A.Srcloc, 'name': A.Name } }
  | { $name: "pointless-var", dict: { 'loc': A.Srcloc } }
  | { $name: "pointless-rec", dict: { 'loc': A.Srcloc } }
  | { $name: "pointless-shadow", dict: { 'loc': A.Srcloc } }
  | { $name: "bad-assignment", dict: { 'iuse': A.Expr, 'idef': A.Srcloc } }
  | {
    $name: "mixed-id-var",
    dict: { 'id': string, 'var-loc': A.Srcloc, 'id-loc': A.Srcloc }
  }
  | {
    $name: "shadow-id",
    dict: 
      {
        'id': string,
        'new-loc': A.Srcloc,
        'old-loc': A.Srcloc,
        'import-loc': A.Option<A.Srcloc>
      }
  }
  | {
    $name: "duplicate-id",
    dict: { 'id': string, 'new-loc': A.Srcloc, 'old-loc': A.Srcloc }
  }
  | {
    $name: "duplicate-field",
    dict: { 'id': string, 'new-loc': A.Srcloc, 'old-loc': A.Srcloc }
  }
  | { $name: "same-line", dict: { 'a': A.Srcloc, 'b': A.Srcloc, 'b-is-paren': boolean } }
  | { $name: "template-same-line", dict: { 'a': A.Srcloc, 'b': A.Srcloc } }
  | { $name: "type-mismatch", dict: { 'type-1': T.Type, 'type-2': T.Type } }
  | {
    $name: "incorrect-type",
    dict: 
      {
        'bad-name': string,
        'bad-loc': A.Srcloc,
        'expected-name': string,
        'expected-loc': A.Srcloc
      }
  }
  | {
    $name: "incorrect-type-expression",
    dict: 
      {
        'bad-name': string,
        'bad-loc': A.Srcloc,
        'expected-name': string,
        'expected-loc': A.Srcloc,
        'e': A.Expr
      }
  }
  | {
    $name: "bad-type-instantiation",
    dict: { 'app-type': Variant<T.Type, 't-app'>, 'expected-length': number }
  }
  | {
    $name: "incorrect-number-of-args",
    dict: { 'app-expr': Variant<A.Expr, 's-app'>, 'fun-typ': T.Type }
  }
  | { $name: "method-missing-self", dict: { 'expr': A.Expr } }
  | { $name: "apply-non-function", dict: { 'app-expr': A.Expr, 'typ': T.Type } }
  | {
    $name: "tuple-too-small",
    dict: 
      {
        'index': Number,
        'tup-length': Number,
        'tup': string,
        'tup-loc': A.Srcloc,
        'access-loc': A.Srcloc
      }
  }
  | {
    $name: "object-missing-field",
    dict: 
      {
        'field-name': string,
        'obj': string,
        'obj-loc': A.Srcloc,
        'access-loc': A.Srcloc
      }
  }
  | {
    $name: "duplicate-variant",
    dict: { 'id': string, 'found': A.Srcloc, 'previous': A.Srcloc }
  }
  | {
    $name: "data-variant-duplicate-name",
    dict: { 'id': string, 'found': A.Srcloc, 'data-loc': A.Srcloc }
  }
  | {
    $name: "duplicate-is-variant",
    dict: { 'id': string, 'is-found': A.Srcloc, 'base-found': A.Srcloc }
  }
  | {
    $name: "duplicate-is-data",
    dict: { 'id': string, 'is-found': A.Srcloc, 'base-found': A.Srcloc }
  }
  | {
    $name: "duplicate-is-data-variant",
    dict: { 'id': string, 'is-found': A.Srcloc, 'base-found': A.Srcloc }
  }
  | {
    $name: "duplicate-branch",
    dict: { 'id': string, 'found': A.Srcloc, 'previous': A.Srcloc }
  }
  | {
    $name: "unnecessary-branch",
    dict: 
      { 'branch': A.CasesBranch, 'data-type': T.DataType, 'cases-loc': A.Srcloc }
  }
  | {
    $name: "unnecessary-else-branch",
    dict: { 'type-name': string, 'loc': A.Srcloc }
  }
  | {
    $name: "non-exhaustive-pattern",
    dict: { 'missing': List<T.TypeVariant>, 'type-name': string, 'loc': A.Srcloc }
  }
  | {
    $name: "cant-match-on",
    dict: { 'ann': any, 'type-name': string, 'loc': A.Srcloc }
  }
  | { $name: "different-branch-types", dict: { 'l': any, 'branch-types': any } }
  | {
    $name: "incorrect-number-of-bindings",
    dict: { 'branch': A.CasesBranch, 'variant': T.TypeVariant }
  }
  | {
    $name: "cases-singleton-mismatch",
    dict: 
      { 'name': string, 'branch-loc': A.Srcloc, 'should-be-singleton': boolean }
  }
  | { $name: "given-parameters", dict: { 'data-type': string, 'loc': A.Srcloc } }
  | { $name: "unable-to-instantiate", dict: { 'loc': A.Srcloc } }
  | { $name: "unable-to-infer", dict: { 'loc': A.Srcloc } }
  | { $name: "unann-failed-test-inference", dict: { 'function-loc': A.Srcloc } }
  | { $name: "toplevel-unann", dict: { 'arg': A.Bind } }
  | { $name: "polymorphic-return-type-unann", dict: { 'function-loc': A.Srcloc } }
  | {
    $name: "binop-type-error",
    dict: 
      {
        'binop': A.Expr,
        'tl': T.Type,
        'tr': T.Type,
        'etl': T.Type,
        'etr': T.Type
      }
  }
  | { $name: "cant-typecheck", dict: { 'reason': string, 'loc': A.Srcloc } }
  | { $name: "unsupported", dict: { 'message': string, 'blame-loc': A.Srcloc } }
  | { $name: "non-object-provide", dict: { 'loc': A.Srcloc } }
  | { $name: "no-module", dict: { 'loc': A.Srcloc, 'mod-name': string } }
  | { $name: "table-empty-header", dict: { 'loc': A.Srcloc } }
  | { $name: "table-empty-row", dict: { 'loc': A.Srcloc } }
  | {
    $name: "table-row-wrong-size",
    dict: 
      { 'header-loc': A.Srcloc, 'header': List<A.FieldName>, 'row': A.TableRow }
  }
  | {
    $name: "table-duplicate-column-name",
    dict: { 'column1': A.FieldName, 'column2': A.FieldName }
  }
  | {
    $name: "table-reducer-bad-column",
    dict: { 'extension': A.TableExtendField, 'col-defs': A.Srcloc }
  }
  | {
    $name: "table-sanitizer-bad-column",
    dict: { 'sanitize-expr': A.LoadTableSpec, 'col-defs': A.Srcloc }
  }
  | {
    $name: "load-table-bad-number-srcs",
    dict: { 'lte': A.Expr, 'num-found': Number }
  }
  | {
    $name: "load-table-duplicate-sanitizer",
    dict: 
      {
        'original': A.LoadTableSpec,
        'col-name': string,
        'duplicate-exp': A.LoadTableSpec
      }
  }
  | { $name: "load-table-no-body", dict: { 'load-table-exp': A.Expr } }

export type Pipeline = 
  | { $name: "pipeline-anchor", dict: {} }
  | { $name: "pipeline-ts-anchor", dict: 
      {
        'modules': List<string>,
      }
    }


export interface Exports {
dict: {values: {dict: {
'no-builtins': CompileEnvironment


'cm-builtin-stage-1': Variant<CompileMode, 'cm-builtin-stage-1'>

'cm-builtin-general': Variant<CompileMode, 'cm-builtin-general'>

'cm-normal': Variant<CompileMode, 'cm-normal'>

'dependency': 
  PFunction<
    (protocol: string, _arguments: List<String>) => Variant<Dependency, 'dependency'>
  >

'builtin': PFunction< (modname: string) => Variant<Dependency, 'builtin'> >

'requirejs': PFunction< (path: string) => Variant<NativeModule, 'requirejs'> >

'bind-origin': 
  PFunction<
    (
        local_bind_site: Loc,
        definition_bind_site: Loc,
        new_definition: boolean,
        uri_of_definition: URI,
        original_name: A.Name
      ) => Variant<BindOrigin, 'bind-origin'>
  >

'vb-letrec': Variant<ValueBinder, 'vb-letrec'>

'vb-let': Variant<ValueBinder, 'vb-let'>

'vb-var': Variant<ValueBinder, 'vb-var'>

'value-bind': 
  PFunction<
    (origin: BindOrigin, binder: ValueBinder, atom: A.Name, ann: A.Ann) => Variant<ValueBind, 'value-bind'>
  >

'tb-type-let': Variant<TypeBinder, 'tb-type-let'>

'tb-type-var': Variant<TypeBinder, 'tb-type-var'>

'tb-typ': PFunction< (typ: T.Type) => Variant<TypeBindTyp, 'tb-typ'> >

'tb-none': Variant<TypeBindTyp, 'tb-none'>

'type-bind': 
  PFunction<
    (origin: BindOrigin, binder: TypeBinder, atom: A.Name, typ: TypeBindTyp) => Variant<TypeBind, 'type-bind'>
  >

'module-bind': 
  PFunction<
    (origin: BindOrigin, atom: A.Name, uri: URI) => Variant<ModuleBind, 'module-bind'>
  >

'resolved-scope': 
  PFunction<
    (ast: A.Program, errors: List<CompileError>) => Variant<ScopeResolution, 'resolved-scope'>
  >

'computed-none': Variant<ComputedEnvironment, 'computed-none'>

'computed-env': 
  PFunction<
    (
        module_bindings: MutableStringDict<ModuleBind>,
        bindings: MutableStringDict<ValueBind>,
        type_bindings: MutableStringDict<TypeBind>,
        datatypes: MutableStringDict<A.Expr>,
        module_env: StringDict<ModuleBind>,
        env: StringDict<ValueBind>,
        type_env: StringDict<TypeBind>
      ) => Variant<ComputedEnvironment, 'computed-env'>
  >

'resolved-names': 
  PFunction<
    (ast: A.Program, errors: List<CompileError>, env: ComputedEnvironment) => Variant<NameResolution, 'resolved-names'>
  >

'extra-imports': 
  PFunction<
    (imports: List<ExtraImport>) => Variant<ExtraImports, 'extra-imports'>
  >

'extra-import': 
  PFunction<
    (
        dependency: Dependency,
        as_name: string,
        values: List<String>,
        types: List<String>
      ) => Variant<ExtraImport, 'extra-import'>
  >

'module-as-string': 
  PFunction<
    (
        provides: Provides,
        compile_env: CompileEnvironment,
        post_compile_env: ComputedEnvironment,
        result_printer: CompileResult<any>
      ) => Variant<Loadable, 'module-as-string'>
  >

'compile-env': 
  PFunction<
    (
        globals: Globals,
        all_modules: MutableStringDict<Loadable>,
        my_modules: StringDict<URI>
      ) => Variant<CompileEnvironment, 'compile-env'>
  >

'globals': 
  PFunction<
    (
        modules: StringDict<BindOrigin>,
        values: StringDict<BindOrigin>,
        types: StringDict<BindOrigin>
      ) => Variant<Globals, 'globals'>
  >

'v-alias': 
  PFunction<
    (origin: BindOrigin, original_name: string) => Variant<ValueExport, 'v-alias'>
  >

'v-just-type': 
  PFunction<
    (origin: BindOrigin, t: T.Type) => Variant<ValueExport, 'v-just-type'>
  >

'v-var': 
  PFunction< (origin: BindOrigin, t: T.Type) => Variant<ValueExport, 'v-var'> >

'v-fun': 
  PFunction<
    (origin: BindOrigin, t: T.Type, name: string, flatness: Option<Number>) => Variant<ValueExport, 'v-fun'>
  >

'd-alias': 
  PFunction<
    (origin: BindOrigin, name: string) => Variant<DataExport, 'd-alias'>
  >

'd-type': 
  PFunction<
    (origin: BindOrigin, typ: T.DataType) => Variant<DataExport, 'd-type'>
  >

'provides': 
  PFunction<
    (
        from_uri: URI,
        modules: StringDict<URI>,
        values: StringDict<ValueExport>,
        aliases: StringDict<T.Type>,
        data_definitions: StringDict<DataExport>
      ) => Variant<Provides, 'provides'>
  >

'ok': PFunction< <C>(code: C) => Variant<CompileResult<C>, 'ok'> >

'err': 
  PFunction< (problems: List<CompileError>) => Variant<CompileResult<any>, 'err'> >

'wf-err': 
  PFunction<
    (msg: List<ED.ErrorDisplay>, loc: Loc) => Variant<CompileError, 'wf-err'>
  >

'wf-empty-block': 
  PFunction< (loc: Loc) => Variant<CompileError, 'wf-empty-block'> >

'wf-err-split': 
  PFunction<
    (msg: string, loc: List<Loc>) => Variant<CompileError, 'wf-err-split'>
  >

'wf-bad-method-expression': 
  PFunction<
    (method_expr_loc: Loc) => Variant<CompileError, 'wf-bad-method-expression'>
  >

'reserved-name': 
  PFunction< (loc: Loc, id: string) => Variant<CompileError, 'reserved-name'> >

'contract-on-import': 
  PFunction<
    (loc: Loc, name: string, import_loc: Loc, import_uri: string) => Variant<CompileError, 'contract-on-import'>
  >

'contract-redefined': 
  PFunction<
    (loc: Loc, name: string, defn_loc: Loc) => Variant<CompileError, 'contract-redefined'>
  >

'contract-non-function': 
  PFunction<
    (loc: Loc, name: string, defn_loc: Loc, defn_is_function: boolean) => Variant<CompileError, 'contract-non-function'>
  >

'contract-inconsistent-names': 
  PFunction<
    (loc: Loc, name: string, defn_loc: Loc) => Variant<CompileError, 'contract-inconsistent-names'>
  >

'contract-inconsistent-params': 
  PFunction<
    (loc: Loc, name: string, defn_loc: Loc) => Variant<CompileError, 'contract-inconsistent-params'>
  >

'contract-unused': 
  PFunction<
    (loc: Loc, name: string) => Variant<CompileError, 'contract-unused'>
  >

'contract-bad-loc': 
  PFunction<
    (loc: Loc, name: string, defn_loc: Loc) => Variant<CompileError, 'contract-bad-loc'>
  >

'zero-fraction': 
  PFunction<
    (loc: Loc, numerator: number) => Variant<CompileError, 'zero-fraction'>
  >

'mixed-binops': 
  PFunction<
    (
        exp_loc: Loc,
        op_a_name: string,
        op_a_loc: Loc,
        op_b_name: string,
        op_b_loc: Loc
      ) => Variant<CompileError, 'mixed-binops'>
  >

'block-ending': 
  PFunction<
    (l: Loc, block_loc: Loc, kind: string) => Variant<CompileError, 'block-ending'>
  >

'single-branch-if': 
  PFunction< (expr: A.Expr) => Variant<CompileError, 'single-branch-if'> >

'unwelcome-where': 
  PFunction<
    (kind: string, loc: Loc, block_loc: Loc) => Variant<CompileError, 'unwelcome-where'>
  >

'non-example': 
  PFunction< (expr: A.Expr) => Variant<CompileError, 'non-example'> >

'tuple-get-bad-index': 
  PFunction<
    (l: Loc, tup: A.Expr, index: number, index_loc: Loc) => Variant<CompileError, 'tuple-get-bad-index'>
  >

'import-arity-mismatch': 
  PFunction<
    (
        l: Loc,
        kind: string,
        args: List<string>,
        expected_arity: number,
        expected_args: List<string>
      ) => Variant<CompileError, 'import-arity-mismatch'>
  >

'no-arguments': 
  PFunction< (expr: Variant<A.Member, "s-method-field">) => Variant<CompileError, 'no-arguments'> >

'non-toplevel': 
  PFunction<
    (kind: string, l: Loc, parent_loc: Loc) => Variant<CompileError, 'non-toplevel'>
  >

'unwelcome-test': 
  PFunction< (loc: Loc) => Variant<CompileError, 'unwelcome-test'> >

'unwelcome-test-refinement': 
  PFunction<
    (refinement: A.Expr, op: A.CheckOp) => Variant<CompileError, 'unwelcome-test-refinement'>
  >

'underscore-as': 
  PFunction< (l: Loc, kind: string) => Variant<CompileError, 'underscore-as'> >

'underscore-as-pattern': 
  PFunction< (l: Loc) => Variant<CompileError, 'underscore-as-pattern'> >

'underscore-as-expr': 
  PFunction< (l: Loc) => Variant<CompileError, 'underscore-as-expr'> >

'underscore-as-ann': 
  PFunction< (l: Loc) => Variant<CompileError, 'underscore-as-ann'> >

'block-needed': 
  PFunction<
    (expr_loc: Loc, blocks: List<Variant<A.Expr, "s-block">>) => Variant<CompileError, 'block-needed'>
  >

'name-not-provided': 
  PFunction<
    (name_loc: Loc, imp_loc: Loc, name: A.Name, typ: string) => Variant<CompileError, 'name-not-provided'>
  >

'unbound-id': PFunction< (id: A.Expr) => Variant<CompileError, 'unbound-id'> >

'unbound-var': 
  PFunction< (id: string, loc: Loc) => Variant<CompileError, 'unbound-var'> >

'unbound-type-id': 
  PFunction< (ann: A.Ann) => Variant<CompileError, 'unbound-type-id'> >

'type-id-used-in-dot-lookup': 
  PFunction<
    (loc: Loc, name: A.Name) => Variant<CompileError, 'type-id-used-in-dot-lookup'>
  >

'type-id-used-as-value': 
  PFunction<
    (id: A.Name, origin: BindOrigin) => Variant<CompileError, 'type-id-used-as-value'>
  >

'unexpected-type-var': 
  PFunction<
    (loc: Loc, name: A.Name) => Variant<CompileError, 'unexpected-type-var'>
  >

'pointless-var': 
  PFunction< (loc: Loc) => Variant<CompileError, 'pointless-var'> >

'pointless-rec': 
  PFunction< (loc: Loc) => Variant<CompileError, 'pointless-rec'> >

'pointless-shadow': 
  PFunction< (loc: Loc) => Variant<CompileError, 'pointless-shadow'> >

'bad-assignment': 
  PFunction<
    (iuse: A.Expr, idef: Loc) => Variant<CompileError, 'bad-assignment'>
  >

'mixed-id-var': 
  PFunction<
    (id: string, var_loc: Loc, id_loc: Loc) => Variant<CompileError, 'mixed-id-var'>
  >

'shadow-id': 
  PFunction<
    (id: string, new_loc: Loc, old_loc: Loc, import_loc: Option<Loc>) => Variant<CompileError, 'shadow-id'>
  >

'duplicate-id': 
  PFunction<
    (id: string, new_loc: Loc, old_loc: Loc) => Variant<CompileError, 'duplicate-id'>
  >

'duplicate-field': 
  PFunction<
    (id: string, new_loc: Loc, old_loc: Loc) => Variant<CompileError, 'duplicate-field'>
  >

'same-line': 
  PFunction<
    (a: Loc, b: Loc, b_is_paren: boolean) => Variant<CompileError, 'same-line'>
  >

'template-same-line': 
  PFunction< (a: Loc, b: Loc) => Variant<CompileError, 'template-same-line'> >

'type-mismatch': 
  PFunction<
    (type_1: T.Type, type_2: T.Type) => Variant<CompileError, 'type-mismatch'>
  >

'incorrect-type': 
  PFunction<
    (
        bad_name: string,
        bad_loc: Loc,
        expected_name: string,
        expected_loc: Loc
      ) => Variant<CompileError, 'incorrect-type'>
  >

'incorrect-type-expression': 
  PFunction<
    (
        bad_name: string,
        bad_loc: Loc,
        expected_name: string,
        expected_loc: Loc,
        e: A.Expr
      ) => Variant<CompileError, 'incorrect-type-expression'>
  >

'bad-type-instantiation': 
  PFunction<
    (app_type: Variant<T.Type, "t-app">, expected_length: any) => Variant<CompileError, 'bad-type-instantiation'>
  >

'incorrect-number-of-args': 
  PFunction<
    (app_expr: Variant<A.Expr, 's-app'>, fun_typ: T.Type) => Variant<CompileError, 'incorrect-number-of-args'>
  >

'method-missing-self': 
  PFunction< (expr: A.Expr) => Variant<CompileError, 'method-missing-self'> >

'apply-non-function': 
  PFunction<
    (app_expr: A.Expr, typ: T.Type) => Variant<CompileError, 'apply-non-function'>
  >

'tuple-too-small': 
  PFunction<
    (
        index: Number,
        tup_length: Number,
        tup: string,
        tup_loc: Loc,
        access_loc: Loc
      ) => Variant<CompileError, 'tuple-too-small'>
  >

'object-missing-field': 
  PFunction<
    (field_name: string, obj: string, obj_loc: Loc, access_loc: Loc) => Variant<CompileError, 'object-missing-field'>
  >

'duplicate-variant': 
  PFunction<
    (id: string, found: Loc, previous: Loc) => Variant<CompileError, 'duplicate-variant'>
  >

'data-variant-duplicate-name': 
  PFunction<
    (id: string, found: Loc, data_loc: Loc) => Variant<CompileError, 'data-variant-duplicate-name'>
  >

'duplicate-is-variant': 
  PFunction<
    (id: string, is_found: Loc, base_found: Loc) => Variant<CompileError, 'duplicate-is-variant'>
  >

'duplicate-is-data': 
  PFunction<
    (id: string, is_found: Loc, base_found: Loc) => Variant<CompileError, 'duplicate-is-data'>
  >

'duplicate-is-data-variant': 
  PFunction<
    (id: string, is_found: Loc, base_found: Loc) => Variant<CompileError, 'duplicate-is-data-variant'>
  >

'duplicate-branch': 
  PFunction<
    (id: string, found: Loc, previous: Loc) => Variant<CompileError, 'duplicate-branch'>
  >

'unnecessary-branch': 
  PFunction<
    (branch: A.CasesBranch, data_type: T.DataType, cases_loc: Loc) => Variant<CompileError, 'unnecessary-branch'>
  >

'unnecessary-else-branch': 
  PFunction<
    (type_name: string, loc: Loc) => Variant<CompileError, 'unnecessary-else-branch'>
  >

'non-exhaustive-pattern': 
  PFunction<
    (missing: List<T.TypeVariant>, type_name: string, loc: Loc) => Variant<CompileError, 'non-exhaustive-pattern'>
  >

'cant-match-on': 
  PFunction<
    (ann: A.Ann, type_name: string, loc: Loc) => Variant<CompileError, 'cant-match-on'>
  >

'different-branch-types': 
  PFunction<
    (l: Loc, branch_types: List<T.Type>) => Variant<CompileError, 'different-branch-types'>
  >

'incorrect-number-of-bindings': 
  PFunction<
    (branch: A.CasesBranch, variant: T.TypeVariant) => Variant<CompileError, 'incorrect-number-of-bindings'>
  >

'cases-singleton-mismatch': 
  PFunction<
    (name: string, branch_loc: Loc, should_be_singleton: boolean) => Variant<CompileError, 'cases-singleton-mismatch'>
  >

'given-parameters': 
  PFunction<
    (data_type: string, loc: Loc) => Variant<CompileError, 'given-parameters'>
  >

'unable-to-instantiate': 
  PFunction< (loc: Loc) => Variant<CompileError, 'unable-to-instantiate'> >

'unable-to-infer': 
  PFunction< (loc: Loc) => Variant<CompileError, 'unable-to-infer'> >

'unann-failed-test-inference': 
  PFunction<
    (function_loc: Loc) => Variant<CompileError, 'unann-failed-test-inference'>
  >

'toplevel-unann': 
  PFunction< (arg: A.Bind) => Variant<CompileError, 'toplevel-unann'> >

'polymorphic-return-type-unann': 
  PFunction<
    (function_loc: Loc) => Variant<CompileError, 'polymorphic-return-type-unann'>
  >

'binop-type-error': 
  PFunction<
    (binop: A.Expr, tl: T.Type, tr: T.Type, etl: T.Type, etr: T.Type) => Variant<CompileError, 'binop-type-error'>
  >

'cant-typecheck': 
  PFunction<
    (reason: string, loc: Loc) => Variant<CompileError, 'cant-typecheck'>
  >

'unsupported': 
  PFunction<
    (message: string, blame_loc: Loc) => Variant<CompileError, 'unsupported'>
  >

'non-object-provide': 
  PFunction< (loc: Loc) => Variant<CompileError, 'non-object-provide'> >

'no-module': 
  PFunction<
    (loc: Loc, mod_name: string) => Variant<CompileError, 'no-module'>
  >

'table-empty-header': 
  PFunction< (loc: Loc) => Variant<CompileError, 'table-empty-header'> >

'table-empty-row': 
  PFunction< (loc: Loc) => Variant<CompileError, 'table-empty-row'> >

'table-row-wrong-size': 
  PFunction<
    (header_loc: Loc, header: List<A.FieldName>, row: A.TableRow) => Variant<CompileError, 'table-row-wrong-size'>
  >

'table-duplicate-column-name': 
  PFunction<
    (column1: A.FieldName, column2: A.FieldName) => Variant<CompileError, 'table-duplicate-column-name'>
  >

'table-reducer-bad-column': 
  PFunction<
    (extension: A.TableExtendField, col_defs: Loc) => Variant<CompileError, 'table-reducer-bad-column'>
  >

'table-sanitizer-bad-column': 
  PFunction<
    (sanitize_expr: A.LoadTableSpec, col_defs: Loc) => Variant<CompileError, 'table-sanitizer-bad-column'>
  >

'load-table-bad-number-srcs': 
  PFunction<
    (lte: A.Expr, num_found: Number) => Variant<CompileError, 'load-table-bad-number-srcs'>
  >

'load-table-duplicate-sanitizer': 
  PFunction<
    (
        original: A.LoadTableSpec,
        col_name: string,
        duplicate_exp: A.LoadTableSpec
      ) => Variant<CompileError, 'load-table-duplicate-sanitizer'>
  >

'load-table-no-body': 
  PFunction<
    (load_table_exp: A.Expr) => Variant<CompileError, 'load-table-no-body'>
  >

'pipeline-anchor': Variant<Pipeline, 'pipeline-anchor'>

'pipeline-ts-anchor': PFunction<(modules: List<string>) => Variant<Pipeline, 'pipeline-ts-anchor'>>

}}}}