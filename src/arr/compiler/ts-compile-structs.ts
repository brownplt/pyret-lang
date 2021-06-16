import type * as A from './ts-ast';
import type * as T from './type-structs';
import { StringDict, MutableStringDict } from './ts-impl-types';
import type * as ED from '../trove/error-display'
import type { Variant } from './ts-codegen-helpers';

type URI = string;

export type CompileMode = 
  | { $name: "cm-builtin-stage-1", dict: {} }
  | { $name: "cm-builtin-general", dict: {} }
  | { $name: "cm-normal", dict: {} }

export type Dependency = 
  | {
    $name: "dependency",
    dict: { 'protocol': string, 'arguments': A.List<string> }
  }
  | { $name: "builtin", dict: { 'modname': string } }

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
    dict: { 'ast': A.Program, 'errors': A.List<CompileError> }
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
        'errors': A.List<CompileError>,
        'env': ComputedEnvironment
      }
  }

export type ExtraImports = 
  | { $name: "extra-imports", dict: { 'imports': A.List<ExtraImport> } }

export type ExtraImport = 
  | {
    $name: "extra-import",
    dict: 
      {
        'dependency': Dependency,
        'as-name': string,
        'values': A.List<String>,
        'types': A.List<String>
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
        'result-printer': CompileResult<any>
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
  | { $name: "err", dict: { 'problems': A.List<CompileError> } }

export type CompileError = 
  | { $name: "wf-err", dict: { 'msg': A.List<ED.ErrorDisplay>, 'loc': A.Srcloc } }
  | { $name: "wf-empty-block", dict: { 'loc': A.Srcloc } }
  | { $name: "wf-err-split", dict: { 'msg': string, 'loc': A.List<A.Srcloc> } }
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
  | { $name: "no-arguments", dict: { 'expr': any } }
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
    dict: { 'expr-loc': A.Srcloc, 'blocks': A.List<Variant<A.Expr, 's-block'>> }
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
    dict: { 'app-type': Variant<T.Type, 't-app'>, 'expected-length': any }
  }
  | {
    $name: "incorrect-number-of-args",
    dict: { 'app-expr': any, 'fun-typ': any }
  }
  | { $name: "method-missing-self", dict: { 'expr': A.Expr } }
  | { $name: "apply-non-function", dict: { 'app-expr': A.Expr, 'typ': any } }
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
    dict: { 'missing': A.List<T.TypeVariant>, 'type-name': string, 'loc': A.Srcloc }
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
      { 'header-loc': A.Srcloc, 'header': A.List<A.FieldName>, 'row': A.TableRow }
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
  | { $name: "pipeline-ts-anchor", dict: {} }

