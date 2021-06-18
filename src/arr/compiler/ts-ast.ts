import type { List, PFunction, PTuple, StringDict } from './ts-impl-types';
import type * as TCH from './ts-codegen-helpers';

export type Option<T> =
  | { $name: 'none', dict: {} }
  | { $name: 'some', dict: { value: T } }

type NumInteger = any;
type PyretNumber = number | Number; // TODO: finish this definition

export type Srcloc =
  | {
    $name: 'builtin',
    dict: {
      'module-name': string
    }
  }
  | {
    $name: 'srcloc',
    dict: {
      source: string,
      'start-line': number,
      'start-column': number,
      'start-char': number,
      'end-line': number,
      'end-column': number,
      'end-char': number
    }
  }

type Loc = Srcloc

export type Name =
  | { $name: "s-underscore", dict: { 'l': Loc } }
  | { $name: "s-name", dict: { 'l': Loc, 's': string } }
  | { $name: "s-global", dict: { 's': string } }
  | { $name: "s-module-global", dict: { 's': string } }
  | { $name: "s-type-global", dict: { 's': string } }
  | { $name: "s-atom", dict: { 'base': string, 'serial': number } }

export type AppInfo =
  | {
    $name: "app-info-c",
    dict: { 'is-recursive': boolean, 'is-tail': boolean }
  }

export type PrimAppInfo =
  | { $name: "prim-app-info-c", dict: { 'needs-step': boolean } }

export type Program =
  | {
    $name: "s-program",
    dict:
    {
      'l': Loc,
      '_provide': Provide,
      'provided-types': ProvideTypes,
      'provides': List<ProvideBlock>,
      'imports': List<Import>,
      'block': Expr
    }
  }

export type Import =
  | { $name: "s-include", dict: { 'l': Loc, 'mod': ImportType } }
  | {
    $name: "s-include-from",
    dict: { 'l': Loc, 'mod': List<Name>, 'specs': List<IncludeSpec> }
  }
  | { $name: "s-import", dict: { 'l': Loc, 'file': ImportType, 'name': Name } }
  | {
    $name: "s-import-types",
    dict: { 'l': Loc, 'file': ImportType, 'name': Name, 'types': Name }
  }
  | {
    $name: "s-import-fields",
    dict: { 'l': Loc, 'fields': List<Name>, 'file': ImportType }
  }

export type IncludeSpec =
  | { $name: "s-include-name", dict: { 'l': Loc, 'name-spec': NameSpec } }
  | {
    $name: "s-include-data",
    dict: { 'l': Loc, 'name-spec': NameSpec, 'hidden': List<Name> }
  }
  | { $name: "s-include-type", dict: { 'l': Loc, 'name-spec': NameSpec } }
  | { $name: "s-include-module", dict: { 'l': Loc, 'name-spec': NameSpec } }

export type ProvidedModule =
  | {
    $name: "p-module",
    dict: { 'l': Loc, 'name': string, 'v': Name, 'uri': string }
  }

export type ProvidedValue =
  | { $name: "p-value", dict: { 'l': Loc, 'v': Name, 'ann': Ann } }

export type ProvidedAlias =
  | {
    $name: "p-alias",
    dict:
    { 'l': Loc, 'in-name': Name, 'out-name': Name, 'mod': Option<ImportType> }
  }

export type ProvidedDatatype =
  | {
    $name: "p-data",
    dict: { 'l': Loc, 'd': Name, 'mod': Option<ImportType> }
  }

export type Provide =
  | { $name: "s-provide", dict: { 'l': Loc, 'block': Expr } }
  | { $name: "s-provide-all", dict: { 'l': Loc } }
  | { $name: "s-provide-none", dict: { 'l': Loc } }

export type ProvideBlock =
  | {
    $name: "s-provide-block",
    dict: { 'l': Loc, 'path': List<Name>, 'specs': List<ProvideSpec> }
  }

export type ProvideSpec =
  | { $name: "s-provide-name", dict: { 'l': Loc, 'name-spec': NameSpec } }
  | {
    $name: "s-provide-data",
    dict: { 'l': Loc, 'name-spec': NameSpec, 'hidden': List<Name> }
  }
  | { $name: "s-provide-type", dict: { 'l': Loc, 'name-spec': NameSpec } }
  | { $name: "s-provide-module", dict: { 'l': Loc, 'name-spec': NameSpec } }

export type NameSpec =
  | { $name: "s-star", dict: { 'l': Loc, 'hidden': List<Name> } }
  | {
    $name: "s-module-ref",
    dict: { 'l': Loc, 'path': List<Name>, 'as-name': Option<Name> }
  }
  | {
    $name: "s-remote-ref",
    dict: { 'l': Loc, 'uri': string, 'name': Name, 'as-name': Name }
  }
  | { $name: "s-local-ref", dict: { 'l': Loc, 'name': Name, 'as-name': Name } }

export type ProvideTypes =
  | { $name: "s-provide-types", dict: { 'l': Loc, 'ann': List<AField> } }
  | { $name: "s-provide-types-all", dict: { 'l': Loc } }
  | { $name: "s-provide-types-none", dict: { 'l': Loc } }

export type ImportType =
  | { $name: "s-const-import", dict: { 'l': Loc, 'mod': string } }
  | {
    $name: "s-special-import",
    dict: { 'l': Loc, 'kind': string, 'args': List<string> }
  }

export type Hint = | { $name: "h-use-loc", dict: { 'l': Loc } }

export type LetBind =
  | { $name: "s-let-bind", dict: { 'l': Loc, 'b': Bind, 'value': Expr } }
  | { $name: "s-var-bind", dict: { 'l': Loc, 'b': Bind, 'value': Expr } }

export type LetrecBind =
  | { $name: "s-letrec-bind", dict: { 'l': Loc, 'b': Bind, 'value': Expr } }

export type TypeLetBind =
  | {
    $name: "s-type-bind",
    dict: { 'l': Loc, 'name': Name, 'params': List<Name>, 'ann': Ann }
  }
  | { $name: "s-newtype-bind", dict: { 'l': Loc, 'name': Name, 'namet': Name } }

export type DefinedModule =
  | {
    $name: "s-defined-module",
    dict: { 'name': string, 'value': Name, 'uri': string }
  }

export type DefinedValue =
  | { $name: "s-defined-value", dict: { 'name': string, 'value': Expr } }
  | { $name: "s-defined-var", dict: { 'name': string, 'id': Name, 'loc': Loc } }

export type DefinedType =
  | { $name: "s-defined-type", dict: { 'name': string, 'typ': Ann } }

export type Expr =
  | {
    $name: "s-module",
    dict:
    {
      'l': Loc,
      'answer': Expr,
      'defined-modules': List<DefinedModule>,
      'defined-values': List<DefinedValue>,
      'defined-types': List<DefinedType>,
      'checks': Expr
    }
  }
  | { $name: "s-template", dict: { 'l': Loc } }
  | {
    $name: "s-type-let-expr",
    dict:
    { 'l': Loc, 'binds': List<TypeLetBind>, 'body': Expr, 'blocky': boolean }
  }
  | {
    $name: "s-let-expr",
    dict: { 'l': Loc, 'binds': List<LetBind>, 'body': Expr, 'blocky': boolean }
  }
  | {
    $name: "s-letrec",
    dict:
    { 'l': Loc, 'binds': List<LetrecBind>, 'body': Expr, 'blocky': boolean }
  }
  | {
    $name: "s-hint-exp",
    dict: { 'l': Loc, 'hints': List<Hint>, 'exp': Expr }
  }
  | {
    $name: "s-instantiate",
    dict: { 'l': Loc, 'expr': Expr, 'params': List<Ann> }
  }
  | { $name: "s-block", dict: { 'l': Loc, 'stmts': List<Expr> } }
  | { $name: "s-user-block", dict: { 'l': Loc, 'body': Expr } }
  | {
    $name: "s-fun",
    dict:
    {
      'l': Loc,
      'name': string,
      'params': List<Name>,
      'args': List<Bind>,
      'ann': Ann,
      'doc': string,
      'body': Expr,
      '_check-loc': Option<Loc>,
      '_check': Option<Expr>,
      'blocky': boolean
    }
  }
  | {
    $name: "s-type",
    dict: { 'l': Loc, 'name': Name, 'params': List<Name>, 'ann': Ann }
  }
  | { $name: "s-newtype", dict: { 'l': Loc, 'name': Name, 'namet': Name } }
  | { $name: "s-var", dict: { 'l': Loc, 'name': Bind, 'value': Expr } }
  | { $name: "s-rec", dict: { 'l': Loc, 'name': Bind, 'value': Expr } }
  | {
    $name: "s-let",
    dict: { 'l': Loc, 'name': Bind, 'value': Expr, 'keyword-val': boolean }
  }
  | { $name: "s-ref", dict: { 'l': Loc, 'ann': Option<Ann> } }
  | {
    $name: "s-contract",
    dict: { 'l': Loc, 'name': Name, 'params': List<Name>, 'ann': Ann }
  }
  | {
    $name: "s-when",
    dict: { 'l': Loc, 'test': Expr, 'block': Expr, 'blocky': boolean }
  }
  | { $name: "s-assign", dict: { 'l': Loc, 'id': Name, 'value': Expr } }
  | {
    $name: "s-if-pipe",
    dict: { 'l': Loc, 'branches': List<IfPipeBranch>, 'blocky': boolean }
  }
  | {
    $name: "s-if-pipe-else",
    dict:
    {
      'l': Loc,
      'branches': List<IfPipeBranch>,
      '_else': Expr,
      'blocky': boolean
    }
  }
  | {
    $name: "s-if",
    dict: { 'l': Loc, 'branches': List<IfBranch>, 'blocky': boolean }
  }
  | {
    $name: "s-if-else",
    dict:
    { 'l': Loc, 'branches': List<IfBranch>, '_else': Expr, 'blocky': boolean }
  }
  | {
    $name: "s-cases",
    dict:
    {
      'l': Loc,
      'typ': Ann,
      'val': Expr,
      'branches': List<CasesBranch>,
      'blocky': boolean
    }
  }
  | {
    $name: "s-cases-else",
    dict:
    {
      'l': Loc,
      'typ': Ann,
      'val': Expr,
      'branches': List<CasesBranch>,
      '_else': Expr,
      'blocky': boolean
    }
  }
  | {
    $name: "s-op",
    dict: { 'l': Loc, 'op-l': Loc, 'op': string, 'left': Expr, 'right': Expr }
  }
  | {
    $name: "s-check-test",
    dict:
    {
      'l': Loc,
      'op': CheckOp,
      'refinement': Option<Expr>,
      'left': Expr,
      'right': Option<Expr>,
      'cause': Option<Expr>
    }
  }
  | { $name: "s-check-expr", dict: { 'l': Loc, 'expr': Expr, 'ann': Ann } }
  | { $name: "s-paren", dict: { 'l': Loc, 'expr': Expr } }
  | {
    $name: "s-lam",
    dict:
    {
      'l': Loc,
      'name': string,
      'params': List<Name>,
      'args': List<Bind>,
      'ann': Ann,
      'doc': string,
      'body': Expr,
      '_check-loc': Option<Loc>,
      '_check': Option<Expr>,
      'blocky': boolean
    }
  }
  | {
    $name: "s-method",
    dict:
    {
      'l': Loc,
      'name': string,
      'params': List<Name>,
      'args': List<Bind>,
      'ann': Ann,
      'doc': string,
      'body': Expr,
      '_check-loc': Option<Loc>,
      '_check': Option<Expr>,
      'blocky': boolean
    }
  }
  | {
    $name: "s-extend",
    dict: { 'l': Loc, 'supe': Expr, 'fields': List<Member> }
  }
  | {
    $name: "s-update",
    dict: { 'l': Loc, 'supe': Expr, 'fields': List<Member> }
  }
  | { $name: "s-tuple", dict: { 'l': Loc, 'fields': List<Expr> } }
  | {
    $name: "s-tuple-get",
    dict: { 'l': Loc, 'tup': Expr, 'index': number, 'index-loc': Loc }
  }
  | { $name: "s-obj", dict: { 'l': Loc, 'fields': List<Member> } }
  | { $name: "s-array", dict: { 'l': Loc, 'values': List<Expr> } }
  | {
    $name: "s-construct",
    dict:
    {
      'l': Loc,
      'modifier': ConstructModifier,
      'constructor': Expr,
      'values': List<Expr>
    }
  }
  | { $name: "s-app", dict: { 'l': Loc, '_fun': Expr, 'args': List<Expr> } }
  | {
    $name: "s-app-enriched",
    dict: { 'l': Loc, '_fun': Expr, 'args': List<Expr>, 'app-info': AppInfo }
  }
  | {
    $name: "s-prim-app",
    dict:
    { 'l': Loc, '_fun': string, 'args': List<Expr>, 'app-info': PrimAppInfo }
  }
  | { $name: "s-prim-val", dict: { 'l': Loc, 'name': string } }
  | { $name: "s-id", dict: { 'l': Loc, 'id': Name } }
  | { $name: "s-id-var", dict: { 'l': Loc, 'id': Name } }
  | { $name: "s-id-letrec", dict: { 'l': Loc, 'id': Name, 'safe': boolean } }
  | {
    $name: "s-id-var-modref",
    dict: { 'l': Loc, 'id': Name, 'uri': string, 'name': string }
  }
  | {
    $name: "s-id-modref",
    dict: { 'l': Loc, 'id': Name, 'uri': string, 'name': string }
  }
  | { $name: "s-undefined", dict: { 'l': Loc } }
  | { $name: "s-srcloc", dict: { 'l': Loc, 'loc': Loc } }
  | { $name: "s-num", dict: { 'l': Loc, 'n': PyretNumber } }
  | {
    $name: "s-frac",
    dict: { 'l': Loc, 'num': NumInteger, 'den': NumInteger }
  }
  | {
    $name: "s-rfrac",
    dict: { 'l': Loc, 'num': NumInteger, 'den': NumInteger }
  }
  | { $name: "s-bool", dict: { 'l': Loc, 'b': boolean } }
  | { $name: "s-str", dict: { 'l': Loc, 's': string } }
  | { $name: "s-dot", dict: { 'l': Loc, 'obj': Expr, 'field': string } }
  | { $name: "s-get-bang", dict: { 'l': Loc, 'obj': Expr, 'field': string } }
  | { $name: "s-bracket", dict: { 'l': Loc, 'obj': Expr, 'key': Expr } }
  | {
    $name: "s-data",
    dict:
    {
      'l': Loc,
      'name': string,
      'params': List<Name>,
      'mixins': List<Expr>,
      'variants': List<Variant>,
      'shared-members': List<Member>,
      '_check-loc': Option<Loc>,
      '_check': Option<Expr>
    }
  }
  | {
    $name: "s-data-expr",
    dict:
    {
      'l': Loc,
      'name': string,
      'namet': Name,
      'params': List<Name>,
      'mixins': List<Expr>,
      'variants': List<Variant>,
      'shared-members': List<Member>,
      '_check-loc': Option<Loc>,
      '_check': Option<Expr>
    }
  }
  | {
    $name: "s-for",
    dict:
    {
      'l': Loc,
      'iterator': Expr,
      'bindings': List<ForBind>,
      'ann': Ann,
      'body': Expr,
      'blocky': boolean
    }
  }
  | {
    $name: "s-check",
    dict:
    {
      'l': Loc,
      'name': Option<string>,
      'body': Expr,
      'keyword-check': boolean
    }
  }
  | { $name: "s-reactor", dict: { 'l': Loc, 'fields': List<Member> } }
  | {
    $name: "s-table-extend",
    dict:
    {
      'l': Loc,
      'column-binds': ColumnBinds,
      'extensions': List<TableExtendField>
    }
  }
  | {
    $name: "s-table-update",
    dict: { 'l': Loc, 'column-binds': ColumnBinds, 'updates': List<Member> }
  }
  | {
    $name: "s-table-select",
    dict: { 'l': Loc, 'columns': List<Name>, 'table': Expr }
  }
  | {
    $name: "s-table-order",
    dict: { 'l': Loc, 'table': Expr, 'ordering': List<ColumnSort> }
  }
  | {
    $name: "s-table-filter",
    dict: { 'l': Loc, 'column-binds': ColumnBinds, 'predicate': Expr }
  }
  | {
    $name: "s-table-extract",
    dict: { 'l': Loc, 'column': Name, 'table': Expr }
  }
  | {
    $name: "s-table",
    dict: { 'l': Loc, 'headers': List<FieldName>, 'rows': List<TableRow> }
  }
  | {
    $name: "s-load-table",
    dict: { 'l': Loc, 'headers': List<FieldName>, 'spec': List<LoadTableSpec> }
  }
  | {
    $name: "s-spy-block",
    dict: { 'l': Loc, 'message': Option<Expr>, 'contents': List<SpyField> }
  }

export type TableRow =
  | { $name: "s-table-row", dict: { 'l': Loc, 'elems': List<Expr> } }

export type SpyField =
  | {
    $name: "s-spy-expr",
    dict: { 'l': Loc, 'name': string, 'value': Expr, 'implicit-label': boolean }
  }

export type ConstructModifier =
  | { $name: "s-construct-normal", dict: {} }
  | { $name: "s-construct-lazy", dict: {} }

export type Bind =
  | {
    $name: "s-bind",
    dict: { 'l': Loc, 'shadows': boolean, 'id': Name, 'ann': Ann }
  }
  | {
    $name: "s-tuple-bind",
    dict: { 'l': Loc, 'fields': List<Bind>, 'as-name': Option<Bind> }
  }

export type Member =
  | { $name: "s-data-field", dict: { 'l': Loc, 'name': string, 'value': Expr } }
  | {
    $name: "s-mutable-field",
    dict: { 'l': Loc, 'name': string, 'ann': Ann, 'value': Expr }
  }
  | {
    $name: "s-method-field",
    dict:
    {
      'l': Loc,
      'name': string,
      'params': List<Name>,
      'args': List<Bind>,
      'ann': Ann,
      'doc': string,
      'body': Expr,
      '_check-loc': Option<Loc>,
      '_check': Option<Expr>,
      'blocky': boolean
    }
  }

export type FieldName =
  | { $name: "s-field-name", dict: { 'l': Loc, 'name': string, 'ann': Ann } }

export type ForBind =
  | { $name: "s-for-bind", dict: { 'l': Loc, 'bind': Bind, 'value': Expr } }

export type ColumnBinds =
  | {
    $name: "s-column-binds",
    dict: { 'l': Loc, 'binds': List<Bind>, 'table': Expr }
  }

export type ColumnSortOrder =
  | { $name: "ASCENDING", dict: {} }
  | { $name: "DESCENDING", dict: {} }

export type ColumnSort =
  | {
    $name: "s-column-sort",
    dict: { 'l': Loc, 'column': Name, 'direction': ColumnSortOrder }
  }

export type TableExtendField =
  | {
    $name: "s-table-extend-field",
    dict: { 'l': Loc, 'name': string, 'value': Expr, 'ann': Ann }
  }
  | {
    $name: "s-table-extend-reducer",
    dict: { 'l': Loc, 'name': string, 'reducer': Expr, 'col': Name, 'ann': Ann }
  }

export type LoadTableSpec =
  | { $name: "s-sanitize", dict: { 'l': Loc, 'name': Name, 'sanitizer': Expr } }
  | { $name: "s-table-src", dict: { 'l': Loc, 'src': Expr } }

export type VariantMemberType =
  | { $name: "s-normal", dict: {} }
  | { $name: "s-mutable", dict: {} }

export type VariantMember =
  | {
    $name: "s-variant-member",
    dict: { 'l': Loc, 'member-type': VariantMemberType, 'bind': Bind }
  }

export type Variant =
  | {
    $name: "s-variant",
    dict:
    {
      'l': Loc,
      'constr-loc': Loc,
      'name': string,
      'members': List<VariantMember>,
      'with-members': List<Member>
    }
  }
  | {
    $name: "s-singleton-variant",
    dict: { 'l': Loc, 'name': string, 'with-members': List<Member> }
  }

export type IfBranch =
  | { $name: "s-if-branch", dict: { 'l': Loc, 'test': Expr, 'body': Expr } }

export type IfPipeBranch =
  | {
    $name: "s-if-pipe-branch",
    dict: { 'l': Loc, 'test': Expr, 'body': Expr }
  }

export type CasesBindType =
  | { $name: "s-cases-bind-ref", dict: {} }
  | { $name: "s-cases-bind-normal", dict: {} }

export type CasesBind =
  | {
    $name: "s-cases-bind",
    dict: { 'l': Loc, 'field-type': CasesBindType, 'bind': Bind }
  }

export type CasesBranch =
  | {
    $name: "s-cases-branch",
    dict:
    {
      'l': Loc,
      'pat-loc': Loc,
      'name': string,
      'args': List<CasesBind>,
      'body': Expr
    }
  }
  | {
    $name: "s-singleton-cases-branch",
    dict: { 'l': Loc, 'pat-loc': Loc, 'name': string, 'body': Expr }
  }

export type CheckOp =
  | { $name: "s-op-is", dict: { 'l': Loc } }
  | { $name: "s-op-is-roughly", dict: { 'l': Loc } }
  | { $name: "s-op-is-op", dict: { 'l': Loc, 'op': string } }
  | { $name: "s-op-is-not", dict: { 'l': Loc } }
  | { $name: "s-op-is-not-op", dict: { 'l': Loc, 'op': string } }
  | { $name: "s-op-satisfies", dict: { 'l': Loc } }
  | { $name: "s-op-satisfies-not", dict: { 'l': Loc } }
  | { $name: "s-op-raises", dict: { 'l': Loc } }
  | { $name: "s-op-raises-other", dict: { 'l': Loc } }
  | { $name: "s-op-raises-not", dict: { 'l': Loc } }
  | { $name: "s-op-raises-satisfies", dict: { 'l': Loc } }
  | { $name: "s-op-raises-violates", dict: { 'l': Loc } }

export type Ann =
  | { $name: "a-blank", dict: {} }
  | { $name: "a-any", dict: { 'l': Loc } }
  | { $name: "a-name", dict: { 'l': Loc, 'id': Name } }
  | { $name: "a-type-var", dict: { 'l': Loc, 'id': Name } }
  | {
    $name: "a-arrow",
    dict: { 'l': Loc, 'args': List<Ann>, 'ret': Ann, 'use-parens': boolean }
  }
  | {
    $name: "a-arrow-argnames",
    dict: { 'l': Loc, 'args': List<AField>, 'ret': Ann, 'use-parens': boolean }
  }
  | { $name: "a-method", dict: { 'l': Loc, 'args': List<Ann>, 'ret': Ann } }
  | { $name: "a-record", dict: { 'l': Loc, 'fields': List<AField> } }
  | { $name: "a-tuple", dict: { 'l': Loc, 'fields': List<AField> } }
  | { $name: "a-app", dict: { 'l': Loc, 'ann': Ann, 'args': List<Ann> } }
  | { $name: "a-pred", dict: { 'l': Loc, 'ann': Ann, 'exp': Expr } }
  | { $name: "a-dot", dict: { 'l': Loc, 'obj': Name, 'field': string } }
  | { $name: "a-checked", dict: { 'checked': Ann, 'residual': Ann } }

export type AField =
  | { $name: "a-field", dict: { 'l': Loc, 'name': string, 'ann': Ann } }

export interface Exports {
dict: {values: {dict: {
's-underscore': PFunction< (l: Loc) => TCH.Variant<Name, 's-underscore'> >

's-name': PFunction< (l: Loc, s: string) => TCH.Variant<Name, 's-name'> >

's-global': PFunction< (s: string) => TCH.Variant<Name, 's-global'> >

's-module-global': PFunction< (s: string) => TCH.Variant<Name, 's-module-global'> >

's-type-global': PFunction< (s: string) => TCH.Variant<Name, 's-type-global'> >

's-atom': PFunction< (base: string, serial: Number) => TCH.Variant<Name, 's-atom'> >

'app-info-c': 
  PFunction<
    (is_recursive: boolean, is_tail: boolean) => TCH.Variant<AppInfo, 'app-info-c'>
  >

'prim-app-info-c': 
  PFunction< (needs_step: boolean) => TCH.Variant<PrimAppInfo, 'prim-app-info-c'> >

's-program': 
  PFunction<
    (
        l: Loc,
        _provide: Provide,
        provided_types: ProvideTypes,
        provides: List<ProvideBlock>,
        imports: List<Import>,
        block: Expr
      ) => TCH.Variant<Program, 's-program'>
  >

's-include': 
  PFunction< (l: Loc, mod: ImportType) => TCH.Variant<Import, 's-include'> >

's-include-from': 
  PFunction<
    (l: Loc, mod: List<Name>, specs: List<IncludeSpec>) => TCH.Variant<Import, 's-include-from'>
  >

's-import': 
  PFunction<
    (l: Loc, file: ImportType, name: Name) => TCH.Variant<Import, 's-import'>
  >

's-import-types': 
  PFunction<
    (l: Loc, file: ImportType, name: Name, types: Name) => TCH.Variant<Import, 's-import-types'>
  >

's-import-fields': 
  PFunction<
    (l: Loc, fields: List<Name>, file: ImportType) => TCH.Variant<Import, 's-import-fields'>
  >

's-include-name': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<IncludeSpec, 's-include-name'>
  >

's-include-data': 
  PFunction<
    (l: Loc, name_spec: NameSpec, hidden: List<Name>) => TCH.Variant<IncludeSpec, 's-include-data'>
  >

's-include-type': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<IncludeSpec, 's-include-type'>
  >

's-include-module': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<IncludeSpec, 's-include-module'>
  >

'p-module': 
  PFunction<
    (l: Loc, name: string, v: Name, uri: string) => TCH.Variant<ProvidedModule, 'p-module'>
  >

'p-value': 
  PFunction< (l: Loc, v: Name, ann: Ann) => TCH.Variant<ProvidedValue, 'p-value'> >

'p-alias': 
  PFunction<
    (l: Loc, in_name: Name, out_name: Name, mod: Option<ImportType>) => TCH.Variant<ProvidedAlias, 'p-alias'>
  >

'p-data': 
  PFunction<
    (l: Loc, d: Name, mod: Option<ImportType>) => TCH.Variant<ProvidedDatatype, 'p-data'>
  >

's-provide': PFunction< (l: Loc, block: Expr) => TCH.Variant<Provide, 's-provide'> >

's-provide-all': PFunction< (l: Loc) => TCH.Variant<Provide, 's-provide-all'> >

's-provide-none': PFunction< (l: Loc) => TCH.Variant<Provide, 's-provide-none'> >

's-provide-block': 
  PFunction<
    (l: Loc, path: List<Name>, specs: List<ProvideSpec>) => TCH.Variant<ProvideBlock, 's-provide-block'>
  >

's-provide-name': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<ProvideSpec, 's-provide-name'>
  >

's-provide-data': 
  PFunction<
    (l: Loc, name_spec: NameSpec, hidden: List<Name>) => TCH.Variant<ProvideSpec, 's-provide-data'>
  >

's-provide-type': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<ProvideSpec, 's-provide-type'>
  >

's-provide-module': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<ProvideSpec, 's-provide-module'>
  >

's-star': 
  PFunction< (l: Loc, hidden: List<Name>) => TCH.Variant<NameSpec, 's-star'> >

's-module-ref': 
  PFunction<
    (l: Loc, path: List<Name>, as_name: Option<Name>) => TCH.Variant<NameSpec, 's-module-ref'>
  >

's-remote-ref': 
  PFunction<
    (l: Loc, uri: string, name: Name, as_name: Name) => TCH.Variant<NameSpec, 's-remote-ref'>
  >

's-local-ref': 
  PFunction<
    (l: Loc, name: Name, as_name: Name) => TCH.Variant<NameSpec, 's-local-ref'>
  >

's-provide-types': 
  PFunction<
    (l: Loc, ann: List<AField>) => TCH.Variant<ProvideTypes, 's-provide-types'>
  >

's-provide-types-all': 
  PFunction< (l: Loc) => TCH.Variant<ProvideTypes, 's-provide-types-all'> >

's-provide-types-none': 
  PFunction< (l: Loc) => TCH.Variant<ProvideTypes, 's-provide-types-none'> >

's-const-import': 
  PFunction< (l: Loc, mod: string) => TCH.Variant<ImportType, 's-const-import'> >

's-special-import': 
  PFunction<
    (l: Loc, kind: string, args: List<String>) => TCH.Variant<ImportType, 's-special-import'>
  >

'h-use-loc': PFunction< (l: Loc) => TCH.Variant<Hint, 'h-use-loc'> >

's-let-bind': 
  PFunction< (l: Loc, b: Bind, value: Expr) => TCH.Variant<LetBind, 's-let-bind'> >

's-var-bind': 
  PFunction< (l: Loc, b: Bind, value: Expr) => TCH.Variant<LetBind, 's-var-bind'> >

's-letrec-bind': 
  PFunction<
    (l: Loc, b: Bind, value: Expr) => TCH.Variant<LetrecBind, 's-letrec-bind'>
  >

's-type-bind': 
  PFunction<
    (l: Loc, name: Name, params: List<Name>, ann: Ann) => TCH.Variant<TypeLetBind, 's-type-bind'>
  >

's-newtype-bind': 
  PFunction<
    (l: Loc, name: Name, namet: Name) => TCH.Variant<TypeLetBind, 's-newtype-bind'>
  >

's-defined-module': 
  PFunction<
    (name: string, value: Name, uri: string) => TCH.Variant<DefinedModule, 's-defined-module'>
  >

's-defined-value': 
  PFunction<
    (name: string, value: Expr) => TCH.Variant<DefinedValue, 's-defined-value'>
  >

's-defined-var': 
  PFunction<
    (name: string, id: Name, loc: Loc) => TCH.Variant<DefinedValue, 's-defined-var'>
  >

's-defined-type': 
  PFunction<
    (name: string, typ: Ann) => TCH.Variant<DefinedType, 's-defined-type'>
  >

's-module': 
  PFunction<
    (
        l: Loc,
        answer: Expr,
        defined_modules: List<DefinedModule>,
        defined_values: List<DefinedValue>,
        defined_types: List<DefinedType>,
        checks: Expr
      ) => TCH.Variant<Expr, 's-module'>
  >

's-template': PFunction< (l: Loc) => TCH.Variant<Expr, 's-template'> >

's-type-let-expr': 
  PFunction<
    (l: Loc, binds: List<TypeLetBind>, body: Expr, blocky: boolean) => TCH.Variant<Expr, 's-type-let-expr'>
  >

's-let-expr': 
  PFunction<
    (l: Loc, binds: List<LetBind>, body: Expr, blocky: boolean) => TCH.Variant<Expr, 's-let-expr'>
  >

's-letrec': 
  PFunction<
    (l: Loc, binds: List<LetrecBind>, body: Expr, blocky: boolean) => TCH.Variant<Expr, 's-letrec'>
  >

's-hint-exp': 
  PFunction<
    (l: Loc, hints: List<Hint>, exp: Expr) => TCH.Variant<Expr, 's-hint-exp'>
  >

's-instantiate': 
  PFunction<
    (l: Loc, expr: Expr, params: List<Ann>) => TCH.Variant<Expr, 's-instantiate'>
  >

's-block': PFunction< (l: Loc, stmts: List<Expr>) => TCH.Variant<Expr, 's-block'> >

's-user-block': 
  PFunction< (l: Loc, body: Expr) => TCH.Variant<Expr, 's-user-block'> >

's-fun': 
  PFunction<
    (
        l: Loc,
        name: string,
        params: List<Name>,
        args: List<Bind>,
        ann: Ann,
        doc: string,
        body: Expr,
        _check_loc: Option<Loc>,
        _check: Option<Expr>,
        blocky: boolean
      ) => TCH.Variant<Expr, 's-fun'>
  >

's-type': 
  PFunction<
    (l: Loc, name: Name, params: List<Name>, ann: Ann) => TCH.Variant<Expr, 's-type'>
  >

's-newtype': 
  PFunction< (l: Loc, name: Name, namet: Name) => TCH.Variant<Expr, 's-newtype'> >

's-var': 
  PFunction< (l: Loc, name: Bind, value: Expr) => TCH.Variant<Expr, 's-var'> >

's-rec': 
  PFunction< (l: Loc, name: Bind, value: Expr) => TCH.Variant<Expr, 's-rec'> >

's-let': 
  PFunction<
    (l: Loc, name: Bind, value: Expr, keyword_val: boolean) => TCH.Variant<Expr, 's-let'>
  >

's-ref': PFunction< (l: Loc, ann: Option<Ann>) => TCH.Variant<Expr, 's-ref'> >

's-contract': 
  PFunction<
    (l: Loc, name: Name, params: List<Name>, ann: Ann) => TCH.Variant<Expr, 's-contract'>
  >

's-when': 
  PFunction<
    (l: Loc, test: Expr, block: Expr, blocky: boolean) => TCH.Variant<Expr, 's-when'>
  >

's-assign': 
  PFunction< (l: Loc, id: Name, value: Expr) => TCH.Variant<Expr, 's-assign'> >

's-if-pipe': 
  PFunction<
    (l: Loc, branches: List<IfPipeBranch>, blocky: boolean) => TCH.Variant<Expr, 's-if-pipe'>
  >

's-if-pipe-else': 
  PFunction<
    (l: Loc, branches: List<IfPipeBranch>, _else: Expr, blocky: boolean) => TCH.Variant<Expr, 's-if-pipe-else'>
  >

's-if': 
  PFunction<
    (l: Loc, branches: List<IfBranch>, blocky: boolean) => TCH.Variant<Expr, 's-if'>
  >

's-if-else': 
  PFunction<
    (l: Loc, branches: List<IfBranch>, _else: Expr, blocky: boolean) => TCH.Variant<Expr, 's-if-else'>
  >

's-cases': 
  PFunction<
    (l: Loc, typ: Ann, val: Expr, branches: List<CasesBranch>, blocky: boolean) => TCH.Variant<Expr, 's-cases'>
  >

's-cases-else': 
  PFunction<
    (
        l: Loc,
        typ: Ann,
        val: Expr,
        branches: List<CasesBranch>,
        _else: Expr,
        blocky: boolean
      ) => TCH.Variant<Expr, 's-cases-else'>
  >

's-op': 
  PFunction<
    (l: Loc, op_l: Loc, op: string, left: Expr, right: Expr) => TCH.Variant<Expr, 's-op'>
  >

's-check-test': 
  PFunction<
    (
        l: Loc,
        op: CheckOp,
        refinement: Option<Expr>,
        left: Expr,
        right: Option<Expr>,
        cause: Option<Expr>
      ) => TCH.Variant<Expr, 's-check-test'>
  >

's-check-expr': 
  PFunction< (l: Loc, expr: Expr, ann: Ann) => TCH.Variant<Expr, 's-check-expr'> >

's-paren': PFunction< (l: Loc, expr: Expr) => TCH.Variant<Expr, 's-paren'> >

's-lam': 
  PFunction<
    (
        l: Loc,
        name: string,
        params: List<Name>,
        args: List<Bind>,
        ann: Ann,
        doc: string,
        body: Expr,
        _check_loc: Option<Loc>,
        _check: Option<Expr>,
        blocky: boolean
      ) => TCH.Variant<Expr, 's-lam'>
  >

's-method': 
  PFunction<
    (
        l: Loc,
        name: string,
        params: List<Name>,
        args: List<Bind>,
        ann: Ann,
        doc: string,
        body: Expr,
        _check_loc: Option<Loc>,
        _check: Option<Expr>,
        blocky: boolean
      ) => TCH.Variant<Expr, 's-method'>
  >

's-extend': 
  PFunction<
    (l: Loc, supe: Expr, fields: List<Member>) => TCH.Variant<Expr, 's-extend'>
  >

's-update': 
  PFunction<
    (l: Loc, supe: Expr, fields: List<Member>) => TCH.Variant<Expr, 's-update'>
  >

's-tuple': PFunction< (l: Loc, fields: List<Expr>) => TCH.Variant<Expr, 's-tuple'> >

's-tuple-get': 
  PFunction<
    (l: Loc, tup: Expr, index: Number, index_loc: Loc) => TCH.Variant<Expr, 's-tuple-get'>
  >

's-obj': PFunction< (l: Loc, fields: List<Member>) => TCH.Variant<Expr, 's-obj'> >

's-array': PFunction< (l: Loc, values: List<Expr>) => TCH.Variant<Expr, 's-array'> >

's-construct': 
  PFunction<
    (l: Loc, modifier: ConstructModifier, constructor: Expr, values: List<Expr>) => TCH.Variant<Expr, 's-construct'>
  >

's-app': 
  PFunction< (l: Loc, _fun: Expr, args: List<Expr>) => TCH.Variant<Expr, 's-app'> >

's-app-enriched': 
  PFunction<
    (l: Loc, _fun: Expr, args: List<Expr>, app_info: AppInfo) => TCH.Variant<Expr, 's-app-enriched'>
  >

's-prim-app': 
  PFunction<
    (l: Loc, _fun: string, args: List<Expr>, app_info: PrimAppInfo) => TCH.Variant<Expr, 's-prim-app'>
  >

's-prim-val': PFunction< (l: Loc, name: string) => TCH.Variant<Expr, 's-prim-val'> >

's-id': PFunction< (l: Loc, id: Name) => TCH.Variant<Expr, 's-id'> >

's-id-var': PFunction< (l: Loc, id: Name) => TCH.Variant<Expr, 's-id-var'> >

's-id-letrec': 
  PFunction< (l: Loc, id: Name, safe: boolean) => TCH.Variant<Expr, 's-id-letrec'> >

's-id-var-modref': 
  PFunction<
    (l: Loc, id: Name, uri: string, name: string) => TCH.Variant<Expr, 's-id-var-modref'>
  >

's-id-modref': 
  PFunction<
    (l: Loc, id: Name, uri: string, name: string) => TCH.Variant<Expr, 's-id-modref'>
  >

's-undefined': PFunction< (l: Loc) => TCH.Variant<Expr, 's-undefined'> >

's-srcloc': PFunction< (l: Loc, loc: Loc) => TCH.Variant<Expr, 's-srcloc'> >

's-num': PFunction< (l: Loc, n: Number) => TCH.Variant<Expr, 's-num'> >

's-frac': 
  PFunction<
    (l: Loc, num: NumInteger, den: NumInteger) => TCH.Variant<Expr, 's-frac'>
  >

's-rfrac': 
  PFunction<
    (l: Loc, num: NumInteger, den: NumInteger) => TCH.Variant<Expr, 's-rfrac'>
  >

's-bool': PFunction< (l: Loc, b: boolean) => TCH.Variant<Expr, 's-bool'> >

's-str': PFunction< (l: Loc, s: string) => TCH.Variant<Expr, 's-str'> >

's-dot': 
  PFunction< (l: Loc, obj: Expr, field: string) => TCH.Variant<Expr, 's-dot'> >

's-get-bang': 
  PFunction< (l: Loc, obj: Expr, field: string) => TCH.Variant<Expr, 's-get-bang'> >

's-bracket': 
  PFunction< (l: Loc, obj: Expr, key: Expr) => TCH.Variant<Expr, 's-bracket'> >

's-data': 
  PFunction<
    (
        l: Loc,
        name: string,
        params: List<Name>,
        mixins: List<Expr>,
        variants: List<Variant>,
        shared_members: List<Member>,
        _check_loc: Option<Loc>,
        _check: Option<Expr>
      ) => TCH.Variant<Expr, 's-data'>
  >

's-data-expr': 
  PFunction<
    (
        l: Loc,
        name: string,
        namet: Name,
        params: List<Name>,
        mixins: List<Expr>,
        variants: List<Variant>,
        shared_members: List<Member>,
        _check_loc: Option<Loc>,
        _check: Option<Expr>
      ) => TCH.Variant<Expr, 's-data-expr'>
  >

's-for': 
  PFunction<
    (
        l: Loc,
        iterator: Expr,
        bindings: List<ForBind>,
        ann: Ann,
        body: Expr,
        blocky: unknown
      ) => TCH.Variant<Expr, 's-for'>
  >

's-check': 
  PFunction<
    (l: Loc, name: Option<String>, body: Expr, keyword_check: boolean) => TCH.Variant<Expr, 's-check'>
  >

's-reactor': 
  PFunction< (l: Loc, fields: List<Member>) => TCH.Variant<Expr, 's-reactor'> >

's-table-extend': 
  PFunction<
    (l: Loc, column_binds: ColumnBinds, extensions: List<TableExtendField>) => TCH.Variant<Expr, 's-table-extend'>
  >

's-table-update': 
  PFunction<
    (l: Loc, column_binds: ColumnBinds, updates: List<Member>) => TCH.Variant<Expr, 's-table-update'>
  >

's-table-select': 
  PFunction<
    (l: Loc, columns: List<Name>, table: Expr) => TCH.Variant<Expr, 's-table-select'>
  >

's-table-order': 
  PFunction<
    (l: Loc, table: Expr, ordering: List<ColumnSort>) => TCH.Variant<Expr, 's-table-order'>
  >

's-table-filter': 
  PFunction<
    (l: Loc, column_binds: ColumnBinds, predicate: Expr) => TCH.Variant<Expr, 's-table-filter'>
  >

's-table-extract': 
  PFunction<
    (l: Loc, column: Name, table: Expr) => TCH.Variant<Expr, 's-table-extract'>
  >

's-table': 
  PFunction<
    (l: Loc, headers: List<FieldName>, rows: List<TableRow>) => TCH.Variant<Expr, 's-table'>
  >

's-load-table': 
  PFunction<
    (l: Loc, headers: List<FieldName>, spec: List<LoadTableSpec>) => TCH.Variant<Expr, 's-load-table'>
  >

's-spy-block': 
  PFunction<
    (l: Loc, message: Option<Expr>, contents: List<SpyField>) => TCH.Variant<Expr, 's-spy-block'>
  >

's-table-row': 
  PFunction< (l: Loc, elems: List<Expr>) => TCH.Variant<TableRow, 's-table-row'> >

's-spy-expr': 
  PFunction<
    (l: Loc, name: string, value: Expr, implicit_label: boolean) => TCH.Variant<SpyField, 's-spy-expr'>
  >

's-construct-normal': TCH.Variant<ConstructModifier, 's-construct-normal'>

's-construct-lazy': TCH.Variant<ConstructModifier, 's-construct-lazy'>

's-bind': 
  PFunction<
    (l: Loc, shadows: boolean, id: Name, ann: Ann) => TCH.Variant<Bind, 's-bind'>
  >

's-tuple-bind': 
  PFunction<
    (l: Loc, fields: List<Bind>, as_name: Option<Bind>) => TCH.Variant<Bind, 's-tuple-bind'>
  >

's-data-field': 
  PFunction<
    (l: Loc, name: string, value: Expr) => TCH.Variant<Member, 's-data-field'>
  >

's-mutable-field': 
  PFunction<
    (l: Loc, name: string, ann: Ann, value: Expr) => TCH.Variant<Member, 's-mutable-field'>
  >

's-method-field': 
  PFunction<
    (
        l: Loc,
        name: string,
        params: List<Name>,
        args: List<Bind>,
        ann: Ann,
        doc: string,
        body: Expr,
        _check_loc: Option<Loc>,
        _check: Option<Expr>,
        blocky: boolean
      ) => TCH.Variant<Member, 's-method-field'>
  >

's-field-name': 
  PFunction<
    (l: Loc, name: string, ann: Ann) => TCH.Variant<FieldName, 's-field-name'>
  >

's-for-bind': 
  PFunction<
    (l: Loc, bind: Bind, value: Expr) => TCH.Variant<ForBind, 's-for-bind'>
  >

's-column-binds': 
  PFunction<
    (l: Loc, binds: List<Bind>, table: Expr) => TCH.Variant<ColumnBinds, 's-column-binds'>
  >

'ASCENDING': TCH.Variant<ColumnSortOrder, 'ASCENDING'>

'DESCENDING': TCH.Variant<ColumnSortOrder, 'DESCENDING'>

's-column-sort': 
  PFunction<
    (l: Loc, column: Name, direction: ColumnSortOrder) => TCH.Variant<ColumnSort, 's-column-sort'>
  >

's-table-extend-field': 
  PFunction<
    (l: Loc, name: string, value: Expr, ann: Ann) => TCH.Variant<TableExtendField, 's-table-extend-field'>
  >

's-table-extend-reducer': 
  PFunction<
    (l: Loc, name: string, reducer: Expr, col: Name, ann: Ann) => TCH.Variant<TableExtendField, 's-table-extend-reducer'>
  >

's-sanitize': 
  PFunction<
    (l: Loc, name: Name, sanitizer: Expr) => TCH.Variant<LoadTableSpec, 's-sanitize'>
  >

's-table-src': 
  PFunction< (l: Loc, src: Expr) => TCH.Variant<LoadTableSpec, 's-table-src'> >

's-normal': TCH.Variant<VariantMemberType, 's-normal'>

's-mutable': TCH.Variant<VariantMemberType, 's-mutable'>

's-variant-member': 
  PFunction<
    (l: Loc, member_type: VariantMemberType, bind: Bind) => TCH.Variant<VariantMember, 's-variant-member'>
  >

's-variant': 
  PFunction<
    (
        l: Loc,
        constr_loc: Loc,
        name: string,
        members: List<VariantMember>,
        with_members: List<Member>
      ) => TCH.Variant<Variant, 's-variant'>
  >

's-singleton-variant': 
  PFunction<
    (l: Loc, name: string, with_members: List<Member>) => TCH.Variant<Variant, 's-singleton-variant'>
  >

's-if-branch': 
  PFunction<
    (l: Loc, test: Expr, body: Expr) => TCH.Variant<IfBranch, 's-if-branch'>
  >

's-if-pipe-branch': 
  PFunction<
    (l: Loc, test: Expr, body: Expr) => TCH.Variant<IfPipeBranch, 's-if-pipe-branch'>
  >

's-cases-bind-ref': TCH.Variant<CasesBindType, 's-cases-bind-ref'>

's-cases-bind-normal': TCH.Variant<CasesBindType, 's-cases-bind-normal'>

's-cases-bind': 
  PFunction<
    (l: Loc, field_type: CasesBindType, bind: Bind) => TCH.Variant<CasesBind, 's-cases-bind'>
  >

's-cases-branch': 
  PFunction<
    (l: Loc, pat_loc: Loc, name: string, args: List<CasesBind>, body: Expr) => TCH.Variant<CasesBranch, 's-cases-branch'>
  >

's-singleton-cases-branch': 
  PFunction<
    (l: Loc, pat_loc: Loc, name: string, body: Expr) => TCH.Variant<CasesBranch, 's-singleton-cases-branch'>
  >

's-op-is': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-is'> >

's-op-is-roughly': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-is-roughly'> >

's-op-is-op': 
  PFunction< (l: Loc, op: string) => TCH.Variant<CheckOp, 's-op-is-op'> >

's-op-is-not': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-is-not'> >

's-op-is-not-op': 
  PFunction< (l: Loc, op: string) => TCH.Variant<CheckOp, 's-op-is-not-op'> >

's-op-satisfies': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-satisfies'> >

's-op-satisfies-not': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-satisfies-not'> >

's-op-raises': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises'> >

's-op-raises-other': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-other'> >

's-op-raises-not': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-not'> >

's-op-raises-satisfies': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-satisfies'> >

's-op-raises-violates': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-violates'> >

'a-blank': TCH.Variant<Ann, 'a-blank'>

'a-any': PFunction< (l: Loc) => TCH.Variant<Ann, 'a-any'> >

'a-name': PFunction< (l: Loc, id: Name) => TCH.Variant<Ann, 'a-name'> >

'a-type-var': PFunction< (l: Loc, id: Name) => TCH.Variant<Ann, 'a-type-var'> >

'a-arrow': 
  PFunction<
    (l: Loc, args: List<Ann>, ret: Ann, use_parens: boolean) => TCH.Variant<Ann, 'a-arrow'>
  >

'a-arrow-argnames': 
  PFunction<
    (l: Loc, args: List<AField>, ret: Ann, use_parens: boolean) => TCH.Variant<Ann, 'a-arrow-argnames'>
  >

'a-method': 
  PFunction< (l: Loc, args: List<Ann>, ret: Ann) => TCH.Variant<Ann, 'a-method'> >

'a-record': 
  PFunction< (l: Loc, fields: List<AField>) => TCH.Variant<Ann, 'a-record'> >

'a-tuple': 
  PFunction< (l: Loc, fields: List<AField>) => TCH.Variant<Ann, 'a-tuple'> >

'a-app': 
  PFunction< (l: Loc, ann: Ann, args: List<Ann>) => TCH.Variant<Ann, 'a-app'> >

'a-pred': PFunction< (l: Loc, ann: Ann, exp: Expr) => TCH.Variant<Ann, 'a-pred'> >

'a-dot': 
  PFunction< (l: Loc, obj: Name, field: string) => TCH.Variant<Ann, 'a-dot'> >

'a-checked': 
  PFunction< (checked: Ann, residual: Ann) => TCH.Variant<Ann, 'a-checked'> >

'a-field': 
  PFunction< (l: Loc, name: string, ann: Ann) => TCH.Variant<AField, 'a-field'> >

}}}}