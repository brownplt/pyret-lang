import type { List, PFunction, PTuple, StringDict } from './ts-impl-types';
import type * as TCH from './ts-codegen-helpers';
import type { Srcloc } from './ts-srcloc';

export { Srcloc }

export type Option<T> =
  | { $name: 'none', dict: {} }
  | { $name: 'some', dict: { value: T } }

type NumInteger = any;
type PyretNumber = number | Number; // TODO: finish this definition

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

export type Use =
  | {
    $name: "s-use",
    dict:
    {
      l: Loc,
      n: Name,
      mod: ImportType
    }
  };

export type Program =
  | {
    $name: "s-program",
    dict:
    {
      'l': Loc,
      '_use': Option<Use>,
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

export type IsOp = "op==" | "op=~" | "op<=>"

export type CheckOp =
  | { $name: "s-op-is", dict: { 'l': Loc } }
  | { $name: "s-op-is-roughly", dict: { 'l': Loc } }
  | { $name: "s-op-is-not-roughly", dict: { 'l': Loc } }
  | { $name: "s-op-is-op", dict: { 'l': Loc, 'op': IsOp } }
  | { $name: "s-op-is-not", dict: { 'l': Loc } }
  | { $name: "s-op-is-not-op", dict: { 'l': Loc, 'op': IsOp } }
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
  | { $name: "a-tuple", dict: { 'l': Loc, 'fields': List<Ann> } }
  | { $name: "a-app", dict: { 'l': Loc, 'ann': Ann, 'args': List<Ann> } }
  | { $name: "a-pred", dict: { 'l': Loc, 'ann': Ann, 'exp': Expr } }
  | { $name: "a-dot", dict: { 'l': Loc, 'obj': Name, 'field': string } }
  | { $name: "a-checked", dict: { 'checked': Ann, 'residual': Ann } }

export type AField =
  | { $name: "a-field", dict: { 'l': Loc, 'name': string, 'ann': Ann } }

export interface Exports {
dict: {values: {dict: {
'dummy-loc': Srcloc,
'global-names': {dict:  {
  reset: PFunction<() => void>,
  's-underscore': PFunction<(l : Srcloc) => Name>,
  's-name': PFunction<(s: string, l: Srcloc) => Name>,
  's-global': PFunction<(s: string) => Name>,
  's-module-global': PFunction<(s: string) => Name>,
  's-type-global': PFunction<(s: string) => Name>,
  'make-atom': PFunction<(base: string) => Name>,
  'is-s-underscore': PFunction<(v : Name) => boolean>,
  'is-s-name': PFunction<(v: Name) => boolean>,
  'is-s-global': PFunction<(v: Name) => boolean>,
  'is-s-module-global': PFunction<(v: Name) => boolean>,
  'is-s-atom': PFunction<(v: Name) => boolean>,
}},

'is-Name': PFunction<(val: any) => val is Name>

'is-s-underscore': PFunction<(val: any) => val is TCH.Variant<Name, 's-underscore'>>

's-underscore': PFunction< (l: Loc) => TCH.Variant<Name, 's-underscore'> >

'is-s-name': PFunction<(val: any) => val is TCH.Variant<Name, 's-name'>>

's-name': PFunction< (l: Loc, s: string) => TCH.Variant<Name, 's-name'> >

'is-s-global': PFunction<(val: any) => val is TCH.Variant<Name, 's-global'>>

's-global': PFunction< (s: string) => TCH.Variant<Name, 's-global'> >

'is-s-module-global': 
  PFunction<(val: any) => val is TCH.Variant<Name, 's-module-global'>>

's-module-global': PFunction< (s: string) => TCH.Variant<Name, 's-module-global'> >

'is-s-type-global': 
  PFunction<(val: any) => val is TCH.Variant<Name, 's-type-global'>>

's-type-global': PFunction< (s: string) => TCH.Variant<Name, 's-type-global'> >

'is-s-atom': PFunction<(val: any) => val is TCH.Variant<Name, 's-atom'>>

's-atom': PFunction< (base: string, serial: Number) => TCH.Variant<Name, 's-atom'> >

'is-AppInfo': PFunction<(val: any) => val is AppInfo>

'is-app-info-c': PFunction<(val: any) => val is TCH.Variant<AppInfo, 'app-info-c'>>

'app-info-c': 
  PFunction<
    (is_recursive: boolean, is_tail: boolean) => TCH.Variant<AppInfo, 'app-info-c'>
  >

'is-PrimAppInfo': PFunction<(val: any) => val is PrimAppInfo>

'is-prim-app-info-c': 
  PFunction<(val: any) => val is TCH.Variant<PrimAppInfo, 'prim-app-info-c'>>

'prim-app-info-c': 
  PFunction< (needs_step: boolean) => TCH.Variant<PrimAppInfo, 'prim-app-info-c'> >

'is-Use': PFunction<(val: any) => val is Use>,

'is-s-use': PFunction<(val: any) => val is TCH.Variant<Use, 's-use'>>,

's-use': PFunction<
    (
      l: Loc,
      n: Name,
      mod: ImportType
    ) => TCH.Variant<Use, 's-use'>
  >,

'is-Program': PFunction<(val: any) => val is Program>

'is-s-program': PFunction<(val: any) => val is TCH.Variant<Program, 's-program'>>

's-program': 
  PFunction<
    (
        l: Loc,
        _use: Option<Use>,
        _provide: Provide,
        provided_types: ProvideTypes,
        provides: List<ProvideBlock>,
        imports: List<Import>,
        block: Expr
      ) => TCH.Variant<Program, 's-program'>
  >

'is-Import': PFunction<(val: any) => val is Import>

'is-s-include': PFunction<(val: any) => val is TCH.Variant<Import, 's-include'>>

's-include': 
  PFunction< (l: Loc, mod: ImportType) => TCH.Variant<Import, 's-include'> >

'is-s-include-from': 
  PFunction<(val: any) => val is TCH.Variant<Import, 's-include-from'>>

's-include-from': 
  PFunction<
    (l: Loc, mod: List<Name>, specs: List<IncludeSpec>) => TCH.Variant<Import, 's-include-from'>
  >

'is-s-import': PFunction<(val: any) => val is TCH.Variant<Import, 's-import'>>

's-import': 
  PFunction<
    (l: Loc, file: ImportType, name: Name) => TCH.Variant<Import, 's-import'>
  >

'is-s-import-types': 
  PFunction<(val: any) => val is TCH.Variant<Import, 's-import-types'>>

's-import-types': 
  PFunction<
    (l: Loc, file: ImportType, name: Name, types: Name) => TCH.Variant<Import, 's-import-types'>
  >

'is-s-import-fields': 
  PFunction<(val: any) => val is TCH.Variant<Import, 's-import-fields'>>

's-import-fields': 
  PFunction<
    (l: Loc, fields: List<Name>, file: ImportType) => TCH.Variant<Import, 's-import-fields'>
  >

'is-IncludeSpec': PFunction<(val: any) => val is IncludeSpec>

'is-s-include-name': 
  PFunction<(val: any) => val is TCH.Variant<IncludeSpec, 's-include-name'>>

's-include-name': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<IncludeSpec, 's-include-name'>
  >

'is-s-include-data': 
  PFunction<(val: any) => val is TCH.Variant<IncludeSpec, 's-include-data'>>

's-include-data': 
  PFunction<
    (l: Loc, name_spec: NameSpec, hidden: List<Name>) => TCH.Variant<IncludeSpec, 's-include-data'>
  >

'is-s-include-type': 
  PFunction<(val: any) => val is TCH.Variant<IncludeSpec, 's-include-type'>>

's-include-type': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<IncludeSpec, 's-include-type'>
  >

'is-s-include-module': 
  PFunction<(val: any) => val is TCH.Variant<IncludeSpec, 's-include-module'>>

's-include-module': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<IncludeSpec, 's-include-module'>
  >

'is-ProvidedModule': PFunction<(val: any) => val is ProvidedModule>

'is-p-module': 
  PFunction<(val: any) => val is TCH.Variant<ProvidedModule, 'p-module'>>

'p-module': 
  PFunction<
    (l: Loc, name: string, v: Name, uri: string) => TCH.Variant<ProvidedModule, 'p-module'>
  >

'is-ProvidedValue': PFunction<(val: any) => val is ProvidedValue>

'is-p-value': PFunction<(val: any) => val is TCH.Variant<ProvidedValue, 'p-value'>>

'p-value': 
  PFunction< (l: Loc, v: Name, ann: Ann) => TCH.Variant<ProvidedValue, 'p-value'> >

'is-ProvidedAlias': PFunction<(val: any) => val is ProvidedAlias>

'is-p-alias': PFunction<(val: any) => val is TCH.Variant<ProvidedAlias, 'p-alias'>>

'p-alias': 
  PFunction<
    (l: Loc, in_name: Name, out_name: Name, mod: Option<ImportType>) => TCH.Variant<ProvidedAlias, 'p-alias'>
  >

'is-ProvidedDatatype': PFunction<(val: any) => val is ProvidedDatatype>

'is-p-data': PFunction<(val: any) => val is TCH.Variant<ProvidedDatatype, 'p-data'>>

'p-data': 
  PFunction<
    (l: Loc, d: Name, mod: Option<ImportType>) => TCH.Variant<ProvidedDatatype, 'p-data'>
  >

'is-Provide': PFunction<(val: any) => val is Provide>

'is-s-provide': PFunction<(val: any) => val is TCH.Variant<Provide, 's-provide'>>

's-provide': PFunction< (l: Loc, block: Expr) => TCH.Variant<Provide, 's-provide'> >

'is-s-provide-all': 
  PFunction<(val: any) => val is TCH.Variant<Provide, 's-provide-all'>>

's-provide-all': PFunction< (l: Loc) => TCH.Variant<Provide, 's-provide-all'> >

'is-s-provide-none': 
  PFunction<(val: any) => val is TCH.Variant<Provide, 's-provide-none'>>

's-provide-none': PFunction< (l: Loc) => TCH.Variant<Provide, 's-provide-none'> >

'is-ProvideBlock': PFunction<(val: any) => val is ProvideBlock>

'is-s-provide-block': 
  PFunction<(val: any) => val is TCH.Variant<ProvideBlock, 's-provide-block'>>

's-provide-block': 
  PFunction<
    (l: Loc, path: List<Name>, specs: List<ProvideSpec>) => TCH.Variant<ProvideBlock, 's-provide-block'>
  >

'is-ProvideSpec': PFunction<(val: any) => val is ProvideSpec>

'is-s-provide-name': 
  PFunction<(val: any) => val is TCH.Variant<ProvideSpec, 's-provide-name'>>

's-provide-name': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<ProvideSpec, 's-provide-name'>
  >

'is-s-provide-data': 
  PFunction<(val: any) => val is TCH.Variant<ProvideSpec, 's-provide-data'>>

's-provide-data': 
  PFunction<
    (l: Loc, name_spec: NameSpec, hidden: List<Name>) => TCH.Variant<ProvideSpec, 's-provide-data'>
  >

'is-s-provide-type': 
  PFunction<(val: any) => val is TCH.Variant<ProvideSpec, 's-provide-type'>>

's-provide-type': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<ProvideSpec, 's-provide-type'>
  >

'is-s-provide-module': 
  PFunction<(val: any) => val is TCH.Variant<ProvideSpec, 's-provide-module'>>

's-provide-module': 
  PFunction<
    (l: Loc, name_spec: NameSpec) => TCH.Variant<ProvideSpec, 's-provide-module'>
  >

'is-NameSpec': PFunction<(val: any) => val is NameSpec>

'is-s-star': PFunction<(val: any) => val is TCH.Variant<NameSpec, 's-star'>>

's-star': 
  PFunction< (l: Loc, hidden: List<Name>) => TCH.Variant<NameSpec, 's-star'> >

'is-s-module-ref': 
  PFunction<(val: any) => val is TCH.Variant<NameSpec, 's-module-ref'>>

's-module-ref': 
  PFunction<
    (l: Loc, path: List<Name>, as_name: Option<Name>) => TCH.Variant<NameSpec, 's-module-ref'>
  >

'is-s-remote-ref': 
  PFunction<(val: any) => val is TCH.Variant<NameSpec, 's-remote-ref'>>

's-remote-ref': 
  PFunction<
    (l: Loc, uri: string, name: Name, as_name: Name) => TCH.Variant<NameSpec, 's-remote-ref'>
  >

'is-s-local-ref': 
  PFunction<(val: any) => val is TCH.Variant<NameSpec, 's-local-ref'>>

's-local-ref': 
  PFunction<
    (l: Loc, name: Name, as_name: Name) => TCH.Variant<NameSpec, 's-local-ref'>
  >

'is-ProvideTypes': PFunction<(val: any) => val is ProvideTypes>

'is-s-provide-types': 
  PFunction<(val: any) => val is TCH.Variant<ProvideTypes, 's-provide-types'>>

's-provide-types': 
  PFunction<
    (l: Loc, ann: List<AField>) => TCH.Variant<ProvideTypes, 's-provide-types'>
  >

'is-s-provide-types-all': 
  PFunction<(val: any) => val is TCH.Variant<ProvideTypes, 's-provide-types-all'>>

's-provide-types-all': 
  PFunction< (l: Loc) => TCH.Variant<ProvideTypes, 's-provide-types-all'> >

'is-s-provide-types-none': 
  PFunction<(val: any) => val is TCH.Variant<ProvideTypes, 's-provide-types-none'>>

's-provide-types-none': 
  PFunction< (l: Loc) => TCH.Variant<ProvideTypes, 's-provide-types-none'> >

'is-ImportType': PFunction<(val: any) => val is ImportType>

'is-s-const-import': 
  PFunction<(val: any) => val is TCH.Variant<ImportType, 's-const-import'>>

's-const-import': 
  PFunction< (l: Loc, mod: string) => TCH.Variant<ImportType, 's-const-import'> >

'is-s-special-import': 
  PFunction<(val: any) => val is TCH.Variant<ImportType, 's-special-import'>>

's-special-import': 
  PFunction<
    (l: Loc, kind: string, args: List<String>) => TCH.Variant<ImportType, 's-special-import'>
  >

'is-Hint': PFunction<(val: any) => val is Hint>

'is-h-use-loc': PFunction<(val: any) => val is TCH.Variant<Hint, 'h-use-loc'>>

'h-use-loc': PFunction< (l: Loc) => TCH.Variant<Hint, 'h-use-loc'> >

'is-LetBind': PFunction<(val: any) => val is LetBind>

'is-s-let-bind': PFunction<(val: any) => val is TCH.Variant<LetBind, 's-let-bind'>>

's-let-bind': 
  PFunction< (l: Loc, b: Bind, value: Expr) => TCH.Variant<LetBind, 's-let-bind'> >

'is-s-var-bind': PFunction<(val: any) => val is TCH.Variant<LetBind, 's-var-bind'>>

's-var-bind': 
  PFunction< (l: Loc, b: Bind, value: Expr) => TCH.Variant<LetBind, 's-var-bind'> >

'is-LetrecBind': PFunction<(val: any) => val is LetrecBind>

'is-s-letrec-bind': 
  PFunction<(val: any) => val is TCH.Variant<LetrecBind, 's-letrec-bind'>>

's-letrec-bind': 
  PFunction<
    (l: Loc, b: Bind, value: Expr) => TCH.Variant<LetrecBind, 's-letrec-bind'>
  >

'is-TypeLetBind': PFunction<(val: any) => val is TypeLetBind>

'is-s-type-bind': 
  PFunction<(val: any) => val is TCH.Variant<TypeLetBind, 's-type-bind'>>

's-type-bind': 
  PFunction<
    (l: Loc, name: Name, params: List<Name>, ann: Ann) => TCH.Variant<TypeLetBind, 's-type-bind'>
  >

'is-s-newtype-bind': 
  PFunction<(val: any) => val is TCH.Variant<TypeLetBind, 's-newtype-bind'>>

's-newtype-bind': 
  PFunction<
    (l: Loc, name: Name, namet: Name) => TCH.Variant<TypeLetBind, 's-newtype-bind'>
  >

'is-DefinedModule': PFunction<(val: any) => val is DefinedModule>

'is-s-defined-module': 
  PFunction<(val: any) => val is TCH.Variant<DefinedModule, 's-defined-module'>>

's-defined-module': 
  PFunction<
    (name: string, value: Name, uri: string) => TCH.Variant<DefinedModule, 's-defined-module'>
  >

'is-DefinedValue': PFunction<(val: any) => val is DefinedValue>

'is-s-defined-value': 
  PFunction<(val: any) => val is TCH.Variant<DefinedValue, 's-defined-value'>>

's-defined-value': 
  PFunction<
    (name: string, value: Expr) => TCH.Variant<DefinedValue, 's-defined-value'>
  >

'is-s-defined-var': 
  PFunction<(val: any) => val is TCH.Variant<DefinedValue, 's-defined-var'>>

's-defined-var': 
  PFunction<
    (name: string, id: Name, loc: Loc) => TCH.Variant<DefinedValue, 's-defined-var'>
  >

'is-DefinedType': PFunction<(val: any) => val is DefinedType>

'is-s-defined-type': 
  PFunction<(val: any) => val is TCH.Variant<DefinedType, 's-defined-type'>>

's-defined-type': 
  PFunction<
    (name: string, typ: Ann) => TCH.Variant<DefinedType, 's-defined-type'>
  >

'is-Expr': PFunction<(val: any) => val is Expr>

'is-s-module': PFunction<(val: any) => val is TCH.Variant<Expr, 's-module'>>

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

'is-s-template': PFunction<(val: any) => val is TCH.Variant<Expr, 's-template'>>

's-template': PFunction< (l: Loc) => TCH.Variant<Expr, 's-template'> >

'is-s-type-let-expr': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-type-let-expr'>>

's-type-let-expr': 
  PFunction<
    (l: Loc, binds: List<TypeLetBind>, body: Expr, blocky: boolean) => TCH.Variant<Expr, 's-type-let-expr'>
  >

'is-s-let-expr': PFunction<(val: any) => val is TCH.Variant<Expr, 's-let-expr'>>

's-let-expr': 
  PFunction<
    (l: Loc, binds: List<LetBind>, body: Expr, blocky: boolean) => TCH.Variant<Expr, 's-let-expr'>
  >

'is-s-letrec': PFunction<(val: any) => val is TCH.Variant<Expr, 's-letrec'>>

's-letrec': 
  PFunction<
    (l: Loc, binds: List<LetrecBind>, body: Expr, blocky: boolean) => TCH.Variant<Expr, 's-letrec'>
  >

'is-s-instantiate': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-instantiate'>>

's-instantiate': 
  PFunction<
    (l: Loc, expr: Expr, params: List<Ann>) => TCH.Variant<Expr, 's-instantiate'>
  >

'is-s-block': PFunction<(val: any) => val is TCH.Variant<Expr, 's-block'>>

's-block': PFunction< (l: Loc, stmts: List<Expr>) => TCH.Variant<Expr, 's-block'> >

'is-s-user-block': PFunction<(val: any) => val is TCH.Variant<Expr, 's-user-block'>>

's-user-block': 
  PFunction< (l: Loc, body: Expr) => TCH.Variant<Expr, 's-user-block'> >

'is-s-fun': PFunction<(val: any) => val is TCH.Variant<Expr, 's-fun'>>

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

'is-s-type': PFunction<(val: any) => val is TCH.Variant<Expr, 's-type'>>

's-type': 
  PFunction<
    (l: Loc, name: Name, params: List<Name>, ann: Ann) => TCH.Variant<Expr, 's-type'>
  >

'is-s-newtype': PFunction<(val: any) => val is TCH.Variant<Expr, 's-newtype'>>

's-newtype': 
  PFunction< (l: Loc, name: Name, namet: Name) => TCH.Variant<Expr, 's-newtype'> >

'is-s-var': PFunction<(val: any) => val is TCH.Variant<Expr, 's-var'>>

's-var': 
  PFunction< (l: Loc, name: Bind, value: Expr) => TCH.Variant<Expr, 's-var'> >

'is-s-rec': PFunction<(val: any) => val is TCH.Variant<Expr, 's-rec'>>

's-rec': 
  PFunction< (l: Loc, name: Bind, value: Expr) => TCH.Variant<Expr, 's-rec'> >

'is-s-let': PFunction<(val: any) => val is TCH.Variant<Expr, 's-let'>>

's-let': 
  PFunction<
    (l: Loc, name: Bind, value: Expr, keyword_val: boolean) => TCH.Variant<Expr, 's-let'>
  >


'is-s-contract': PFunction<(val: any) => val is TCH.Variant<Expr, 's-contract'>>

's-contract': 
  PFunction<
    (l: Loc, name: Name, params: List<Name>, ann: Ann) => TCH.Variant<Expr, 's-contract'>
  >

'is-s-when': PFunction<(val: any) => val is TCH.Variant<Expr, 's-when'>>

's-when': 
  PFunction<
    (l: Loc, test: Expr, block: Expr, blocky: boolean) => TCH.Variant<Expr, 's-when'>
  >

'is-s-assign': PFunction<(val: any) => val is TCH.Variant<Expr, 's-assign'>>

's-assign': 
  PFunction< (l: Loc, id: Name, value: Expr) => TCH.Variant<Expr, 's-assign'> >

'is-s-if-pipe': PFunction<(val: any) => val is TCH.Variant<Expr, 's-if-pipe'>>

's-if-pipe': 
  PFunction<
    (l: Loc, branches: List<IfPipeBranch>, blocky: boolean) => TCH.Variant<Expr, 's-if-pipe'>
  >

'is-s-if-pipe-else': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-if-pipe-else'>>

's-if-pipe-else': 
  PFunction<
    (l: Loc, branches: List<IfPipeBranch>, _else: Expr, blocky: boolean) => TCH.Variant<Expr, 's-if-pipe-else'>
  >

'is-s-if': PFunction<(val: any) => val is TCH.Variant<Expr, 's-if'>>

's-if': 
  PFunction<
    (l: Loc, branches: List<IfBranch>, blocky: boolean) => TCH.Variant<Expr, 's-if'>
  >

'is-s-if-else': PFunction<(val: any) => val is TCH.Variant<Expr, 's-if-else'>>

's-if-else': 
  PFunction<
    (l: Loc, branches: List<IfBranch>, _else: Expr, blocky: boolean) => TCH.Variant<Expr, 's-if-else'>
  >

'is-s-cases': PFunction<(val: any) => val is TCH.Variant<Expr, 's-cases'>>

's-cases': 
  PFunction<
    (l: Loc, typ: Ann, val: Expr, branches: List<CasesBranch>, blocky: boolean) => TCH.Variant<Expr, 's-cases'>
  >

'is-s-cases-else': PFunction<(val: any) => val is TCH.Variant<Expr, 's-cases-else'>>

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

'is-s-op': PFunction<(val: any) => val is TCH.Variant<Expr, 's-op'>>

's-op': 
  PFunction<
    (l: Loc, op_l: Loc, op: string, left: Expr, right: Expr) => TCH.Variant<Expr, 's-op'>
  >

'is-s-check-test': PFunction<(val: any) => val is TCH.Variant<Expr, 's-check-test'>>

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

'is-s-check-expr': PFunction<(val: any) => val is TCH.Variant<Expr, 's-check-expr'>>

's-check-expr': 
  PFunction< (l: Loc, expr: Expr, ann: Ann) => TCH.Variant<Expr, 's-check-expr'> >

'is-s-paren': PFunction<(val: any) => val is TCH.Variant<Expr, 's-paren'>>

's-paren': PFunction< (l: Loc, expr: Expr) => TCH.Variant<Expr, 's-paren'> >

'is-s-lam': PFunction<(val: any) => val is TCH.Variant<Expr, 's-lam'>>

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

'is-s-extend': PFunction<(val: any) => val is TCH.Variant<Expr, 's-extend'>>

's-extend': 
  PFunction<
    (l: Loc, supe: Expr, fields: List<Member>) => TCH.Variant<Expr, 's-extend'>
  >

'is-s-update': PFunction<(val: any) => val is TCH.Variant<Expr, 's-update'>>

's-update': 
  PFunction<
    (l: Loc, supe: Expr, fields: List<Member>) => TCH.Variant<Expr, 's-update'>
  >

'is-s-tuple': PFunction<(val: any) => val is TCH.Variant<Expr, 's-tuple'>>

's-tuple': PFunction< (l: Loc, fields: List<Expr>) => TCH.Variant<Expr, 's-tuple'> >

'is-s-tuple-get': PFunction<(val: any) => val is TCH.Variant<Expr, 's-tuple-get'>>

's-tuple-get': 
  PFunction<
    (l: Loc, tup: Expr, index: Number, index_loc: Loc) => TCH.Variant<Expr, 's-tuple-get'>
  >

'is-s-obj': PFunction<(val: any) => val is TCH.Variant<Expr, 's-obj'>>

's-obj': PFunction< (l: Loc, fields: List<Member>) => TCH.Variant<Expr, 's-obj'> >

'is-s-array': PFunction<(val: any) => val is TCH.Variant<Expr, 's-array'>>

's-array': PFunction< (l: Loc, values: List<Expr>) => TCH.Variant<Expr, 's-array'> >

'is-s-construct': PFunction<(val: any) => val is TCH.Variant<Expr, 's-construct'>>

's-construct': 
  PFunction<
    (l: Loc, modifier: ConstructModifier, constructor: Expr, values: List<Expr>) => TCH.Variant<Expr, 's-construct'>
  >

'is-s-app': PFunction<(val: any) => val is TCH.Variant<Expr, 's-app'>>

's-app': 
  PFunction< (l: Loc, _fun: Expr, args: List<Expr>) => TCH.Variant<Expr, 's-app'> >

'is-s-app-enriched': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-app-enriched'>>

's-app-enriched': 
  PFunction<
    (l: Loc, _fun: Expr, args: List<Expr>, app_info: AppInfo) => TCH.Variant<Expr, 's-app-enriched'>
  >

'is-s-prim-app': PFunction<(val: any) => val is TCH.Variant<Expr, 's-prim-app'>>

's-prim-app': 
  PFunction<
    (l: Loc, _fun: string, args: List<Expr>, app_info: PrimAppInfo) => TCH.Variant<Expr, 's-prim-app'>
  >

'is-s-prim-val': PFunction<(val: any) => val is TCH.Variant<Expr, 's-prim-val'>>

's-prim-val': PFunction< (l: Loc, name: string) => TCH.Variant<Expr, 's-prim-val'> >

'is-s-id': PFunction<(val: any) => val is TCH.Variant<Expr, 's-id'>>

's-id': PFunction< (l: Loc, id: Name) => TCH.Variant<Expr, 's-id'> >

'is-s-id-var': PFunction<(val: any) => val is TCH.Variant<Expr, 's-id-var'>>

's-id-var': PFunction< (l: Loc, id: Name) => TCH.Variant<Expr, 's-id-var'> >

'is-s-id-letrec': PFunction<(val: any) => val is TCH.Variant<Expr, 's-id-letrec'>>

's-id-letrec': 
  PFunction< (l: Loc, id: Name, safe: boolean) => TCH.Variant<Expr, 's-id-letrec'> >

'is-s-id-var-modref': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-id-var-modref'>>

's-id-var-modref': 
  PFunction<
    (l: Loc, id: Name, uri: string, name: string) => TCH.Variant<Expr, 's-id-var-modref'>
  >

'is-s-id-modref': PFunction<(val: any) => val is TCH.Variant<Expr, 's-id-modref'>>

's-id-modref': 
  PFunction<
    (l: Loc, id: Name, uri: string, name: string) => TCH.Variant<Expr, 's-id-modref'>
  >

'is-s-srcloc': PFunction<(val: any) => val is TCH.Variant<Expr, 's-srcloc'>>

's-srcloc': PFunction< (l: Loc, loc: Loc) => TCH.Variant<Expr, 's-srcloc'> >

'is-s-num': PFunction<(val: any) => val is TCH.Variant<Expr, 's-num'>>

's-num': PFunction< (l: Loc, n: Number) => TCH.Variant<Expr, 's-num'> >

'is-s-frac': PFunction<(val: any) => val is TCH.Variant<Expr, 's-frac'>>

's-frac': 
  PFunction<
    (l: Loc, num: NumInteger, den: NumInteger) => TCH.Variant<Expr, 's-frac'>
  >

'is-s-rfrac': PFunction<(val: any) => val is TCH.Variant<Expr, 's-rfrac'>>

's-rfrac': 
  PFunction<
    (l: Loc, num: NumInteger, den: NumInteger) => TCH.Variant<Expr, 's-rfrac'>
  >

'is-s-bool': PFunction<(val: any) => val is TCH.Variant<Expr, 's-bool'>>

's-bool': PFunction< (l: Loc, b: boolean) => TCH.Variant<Expr, 's-bool'> >

'is-s-str': PFunction<(val: any) => val is TCH.Variant<Expr, 's-str'>>

's-str': PFunction< (l: Loc, s: string) => TCH.Variant<Expr, 's-str'> >

'is-s-dot': PFunction<(val: any) => val is TCH.Variant<Expr, 's-dot'>>

's-dot': 
  PFunction< (l: Loc, obj: Expr, field: string) => TCH.Variant<Expr, 's-dot'> >

'is-s-get-bang': PFunction<(val: any) => val is TCH.Variant<Expr, 's-get-bang'>>

's-get-bang': 
  PFunction< (l: Loc, obj: Expr, field: string) => TCH.Variant<Expr, 's-get-bang'> >

'is-s-bracket': PFunction<(val: any) => val is TCH.Variant<Expr, 's-bracket'>>

's-bracket': 
  PFunction< (l: Loc, obj: Expr, key: Expr) => TCH.Variant<Expr, 's-bracket'> >

'is-s-data': PFunction<(val: any) => val is TCH.Variant<Expr, 's-data'>>

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

'is-s-data-expr': PFunction<(val: any) => val is TCH.Variant<Expr, 's-data-expr'>>

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

'is-s-for': PFunction<(val: any) => val is TCH.Variant<Expr, 's-for'>>

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

'is-s-check': PFunction<(val: any) => val is TCH.Variant<Expr, 's-check'>>

's-check': 
  PFunction<
    (l: Loc, name: Option<String>, body: Expr, keyword_check: boolean) => TCH.Variant<Expr, 's-check'>
  >

'is-s-reactor': PFunction<(val: any) => val is TCH.Variant<Expr, 's-reactor'>>

's-reactor': 
  PFunction< (l: Loc, fields: List<Member>) => TCH.Variant<Expr, 's-reactor'> >

'is-s-table-extend': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-table-extend'>>

's-table-extend': 
  PFunction<
    (l: Loc, column_binds: ColumnBinds, extensions: List<TableExtendField>) => TCH.Variant<Expr, 's-table-extend'>
  >

'is-s-table-update': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-table-update'>>

's-table-update': 
  PFunction<
    (l: Loc, column_binds: ColumnBinds, updates: List<Member>) => TCH.Variant<Expr, 's-table-update'>
  >

'is-s-table-select': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-table-select'>>

's-table-select': 
  PFunction<
    (l: Loc, columns: List<Name>, table: Expr) => TCH.Variant<Expr, 's-table-select'>
  >

'is-s-table-order': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-table-order'>>

's-table-order': 
  PFunction<
    (l: Loc, table: Expr, ordering: List<ColumnSort>) => TCH.Variant<Expr, 's-table-order'>
  >

'is-s-table-filter': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-table-filter'>>

's-table-filter': 
  PFunction<
    (l: Loc, column_binds: ColumnBinds, predicate: Expr) => TCH.Variant<Expr, 's-table-filter'>
  >

'is-s-table-extract': 
  PFunction<(val: any) => val is TCH.Variant<Expr, 's-table-extract'>>

's-table-extract': 
  PFunction<
    (l: Loc, column: Name, table: Expr) => TCH.Variant<Expr, 's-table-extract'>
  >

'is-s-table': PFunction<(val: any) => val is TCH.Variant<Expr, 's-table'>>

's-table': 
  PFunction<
    (l: Loc, headers: List<FieldName>, rows: List<TableRow>) => TCH.Variant<Expr, 's-table'>
  >

'is-s-load-table': PFunction<(val: any) => val is TCH.Variant<Expr, 's-load-table'>>

's-load-table': 
  PFunction<
    (l: Loc, headers: List<FieldName>, spec: List<LoadTableSpec>) => TCH.Variant<Expr, 's-load-table'>
  >

'is-s-spy-block': PFunction<(val: any) => val is TCH.Variant<Expr, 's-spy-block'>>

's-spy-block': 
  PFunction<
    (l: Loc, message: Option<Expr>, contents: List<SpyField>) => TCH.Variant<Expr, 's-spy-block'>
  >

'is-TableRow': PFunction<(val: any) => val is TableRow>

'is-s-table-row': 
  PFunction<(val: any) => val is TCH.Variant<TableRow, 's-table-row'>>

's-table-row': 
  PFunction< (l: Loc, elems: List<Expr>) => TCH.Variant<TableRow, 's-table-row'> >

'is-SpyField': PFunction<(val: any) => val is SpyField>

'is-s-spy-expr': PFunction<(val: any) => val is TCH.Variant<SpyField, 's-spy-expr'>>

's-spy-expr': 
  PFunction<
    (l: Loc, name: string, value: Expr, implicit_label: boolean) => TCH.Variant<SpyField, 's-spy-expr'>
  >

'is-ConstructModifier': PFunction<(val: any) => val is ConstructModifier>

'is-s-construct-normal': 
  PFunction<(val: any) => val is TCH.Variant<ConstructModifier, 's-construct-normal'>>

's-construct-normal': TCH.Variant<ConstructModifier, 's-construct-normal'>

'is-s-construct-lazy': 
  PFunction<(val: any) => val is TCH.Variant<ConstructModifier, 's-construct-lazy'>>

's-construct-lazy': TCH.Variant<ConstructModifier, 's-construct-lazy'>

'is-Bind': PFunction<(val: any) => val is Bind>

'is-s-bind': PFunction<(val: any) => val is TCH.Variant<Bind, 's-bind'>>

's-bind': 
  PFunction<
    (l: Loc, shadows: boolean, id: Name, ann: Ann) => TCH.Variant<Bind, 's-bind'>
  >

'is-s-tuple-bind': PFunction<(val: any) => val is TCH.Variant<Bind, 's-tuple-bind'>>

's-tuple-bind': 
  PFunction<
    (l: Loc, fields: List<Bind>, as_name: Option<Bind>) => TCH.Variant<Bind, 's-tuple-bind'>
  >

'is-Member': PFunction<(val: any) => val is Member>

'is-s-data-field': 
  PFunction<(val: any) => val is TCH.Variant<Member, 's-data-field'>>

's-data-field': 
  PFunction<
    (l: Loc, name: string, value: Expr) => TCH.Variant<Member, 's-data-field'>
  >

'is-s-mutable-field': 
  PFunction<(val: any) => val is TCH.Variant<Member, 's-mutable-field'>>

's-mutable-field': 
  PFunction<
    (l: Loc, name: string, ann: Ann, value: Expr) => TCH.Variant<Member, 's-mutable-field'>
  >

'is-s-method-field': 
  PFunction<(val: any) => val is TCH.Variant<Member, 's-method-field'>>

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

'is-FieldName': PFunction<(val: any) => val is FieldName>

'is-s-field-name': 
  PFunction<(val: any) => val is TCH.Variant<FieldName, 's-field-name'>>

's-field-name': 
  PFunction<
    (l: Loc, name: string, ann: Ann) => TCH.Variant<FieldName, 's-field-name'>
  >

'is-ForBind': PFunction<(val: any) => val is ForBind>

'is-s-for-bind': PFunction<(val: any) => val is TCH.Variant<ForBind, 's-for-bind'>>

's-for-bind': 
  PFunction<
    (l: Loc, bind: Bind, value: Expr) => TCH.Variant<ForBind, 's-for-bind'>
  >

'is-ColumnBinds': PFunction<(val: any) => val is ColumnBinds>

'is-s-column-binds': 
  PFunction<(val: any) => val is TCH.Variant<ColumnBinds, 's-column-binds'>>

's-column-binds': 
  PFunction<
    (l: Loc, binds: List<Bind>, table: Expr) => TCH.Variant<ColumnBinds, 's-column-binds'>
  >

'is-ColumnSortOrder': PFunction<(val: any) => val is ColumnSortOrder>

'is-ASCENDING': 
  PFunction<(val: any) => val is TCH.Variant<ColumnSortOrder, 'ASCENDING'>>

'ASCENDING': TCH.Variant<ColumnSortOrder, 'ASCENDING'>

'is-DESCENDING': 
  PFunction<(val: any) => val is TCH.Variant<ColumnSortOrder, 'DESCENDING'>>

'DESCENDING': TCH.Variant<ColumnSortOrder, 'DESCENDING'>

'is-ColumnSort': PFunction<(val: any) => val is ColumnSort>

'is-s-column-sort': 
  PFunction<(val: any) => val is TCH.Variant<ColumnSort, 's-column-sort'>>

's-column-sort': 
  PFunction<
    (l: Loc, column: Name, direction: ColumnSortOrder) => TCH.Variant<ColumnSort, 's-column-sort'>
  >

'is-TableExtendField': PFunction<(val: any) => val is TableExtendField>

'is-s-table-extend-field': 
  PFunction<(val: any) => val is TCH.Variant<TableExtendField, 's-table-extend-field'>>

's-table-extend-field': 
  PFunction<
    (l: Loc, name: string, value: Expr, ann: Ann) => TCH.Variant<TableExtendField, 's-table-extend-field'>
  >

'is-s-table-extend-reducer': 
  PFunction<(val: any) => val is TCH.Variant<TableExtendField, 's-table-extend-reducer'>>

's-table-extend-reducer': 
  PFunction<
    (l: Loc, name: string, reducer: Expr, col: Name, ann: Ann) => TCH.Variant<TableExtendField, 's-table-extend-reducer'>
  >

'is-LoadTableSpec': PFunction<(val: any) => val is LoadTableSpec>

'is-s-sanitize': 
  PFunction<(val: any) => val is TCH.Variant<LoadTableSpec, 's-sanitize'>>

's-sanitize': 
  PFunction<
    (l: Loc, name: Name, sanitizer: Expr) => TCH.Variant<LoadTableSpec, 's-sanitize'>
  >

'is-s-table-src': 
  PFunction<(val: any) => val is TCH.Variant<LoadTableSpec, 's-table-src'>>

's-table-src': 
  PFunction< (l: Loc, src: Expr) => TCH.Variant<LoadTableSpec, 's-table-src'> >

'is-VariantMemberType': PFunction<(val: any) => val is VariantMemberType>

'is-s-normal': 
  PFunction<(val: any) => val is TCH.Variant<VariantMemberType, 's-normal'>>

's-normal': TCH.Variant<VariantMemberType, 's-normal'>

'is-s-mutable': 
  PFunction<(val: any) => val is TCH.Variant<VariantMemberType, 's-mutable'>>

's-mutable': TCH.Variant<VariantMemberType, 's-mutable'>

'is-VariantMember': PFunction<(val: any) => val is VariantMember>

'is-s-variant-member': 
  PFunction<(val: any) => val is TCH.Variant<VariantMember, 's-variant-member'>>

's-variant-member': 
  PFunction<
    (l: Loc, member_type: VariantMemberType, bind: Bind) => TCH.Variant<VariantMember, 's-variant-member'>
  >

'is-Variant': PFunction<(val: any) => val is Variant>

'is-s-variant': PFunction<(val: any) => val is TCH.Variant<Variant, 's-variant'>>

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

'is-s-singleton-variant': 
  PFunction<(val: any) => val is TCH.Variant<Variant, 's-singleton-variant'>>

's-singleton-variant': 
  PFunction<
    (l: Loc, name: string, with_members: List<Member>) => TCH.Variant<Variant, 's-singleton-variant'>
  >

'is-IfBranch': PFunction<(val: any) => val is IfBranch>

'is-s-if-branch': 
  PFunction<(val: any) => val is TCH.Variant<IfBranch, 's-if-branch'>>

's-if-branch': 
  PFunction<
    (l: Loc, test: Expr, body: Expr) => TCH.Variant<IfBranch, 's-if-branch'>
  >

'is-IfPipeBranch': PFunction<(val: any) => val is IfPipeBranch>

'is-s-if-pipe-branch': 
  PFunction<(val: any) => val is TCH.Variant<IfPipeBranch, 's-if-pipe-branch'>>

's-if-pipe-branch': 
  PFunction<
    (l: Loc, test: Expr, body: Expr) => TCH.Variant<IfPipeBranch, 's-if-pipe-branch'>
  >

'is-CasesBindType': PFunction<(val: any) => val is CasesBindType>

'is-s-cases-bind-ref': 
  PFunction<(val: any) => val is TCH.Variant<CasesBindType, 's-cases-bind-ref'>>

's-cases-bind-ref': TCH.Variant<CasesBindType, 's-cases-bind-ref'>

'is-s-cases-bind-normal': 
  PFunction<(val: any) => val is TCH.Variant<CasesBindType, 's-cases-bind-normal'>>

's-cases-bind-normal': TCH.Variant<CasesBindType, 's-cases-bind-normal'>

'is-CasesBind': PFunction<(val: any) => val is CasesBind>

'is-s-cases-bind': 
  PFunction<(val: any) => val is TCH.Variant<CasesBind, 's-cases-bind'>>

's-cases-bind': 
  PFunction<
    (l: Loc, field_type: CasesBindType, bind: Bind) => TCH.Variant<CasesBind, 's-cases-bind'>
  >

'is-CasesBranch': PFunction<(val: any) => val is CasesBranch>

'is-s-cases-branch': 
  PFunction<(val: any) => val is TCH.Variant<CasesBranch, 's-cases-branch'>>

's-cases-branch': 
  PFunction<
    (l: Loc, pat_loc: Loc, name: string, args: List<CasesBind>, body: Expr) => TCH.Variant<CasesBranch, 's-cases-branch'>
  >

'is-s-singleton-cases-branch': 
  PFunction<(val: any) => val is TCH.Variant<CasesBranch, 's-singleton-cases-branch'>>

's-singleton-cases-branch': 
  PFunction<
    (l: Loc, pat_loc: Loc, name: string, body: Expr) => TCH.Variant<CasesBranch, 's-singleton-cases-branch'>
  >

'is-CheckOp': PFunction<(val: any) => val is CheckOp>

'is-s-op-is': PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-is'>>

's-op-is': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-is'> >

'is-s-op-is-roughly': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-is-roughly'>>

's-op-is-roughly': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-is-roughly'> >

'is-s-op-is-op': PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-is-op'>>

's-op-is-op': 
  PFunction< (l: Loc, op: string) => TCH.Variant<CheckOp, 's-op-is-op'> >

'is-s-op-is-not': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-is-not'>>

's-op-is-not': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-is-not'> >

'is-s-op-is-not-op': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-is-not-op'>>

's-op-is-not-op': 
  PFunction< (l: Loc, op: string) => TCH.Variant<CheckOp, 's-op-is-not-op'> >

'is-s-op-satisfies': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-satisfies'>>

's-op-satisfies': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-satisfies'> >

'is-s-op-satisfies-not': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-satisfies-not'>>

's-op-satisfies-not': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-satisfies-not'> >

'is-s-op-raises': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-raises'>>

's-op-raises': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises'> >

'is-s-op-raises-other': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-raises-other'>>

's-op-raises-other': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-other'> >

'is-s-op-raises-not': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-raises-not'>>

's-op-raises-not': PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-not'> >

'is-s-op-raises-satisfies': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-raises-satisfies'>>

's-op-raises-satisfies': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-satisfies'> >

'is-s-op-raises-violates': 
  PFunction<(val: any) => val is TCH.Variant<CheckOp, 's-op-raises-violates'>>

's-op-raises-violates': 
  PFunction< (l: Loc) => TCH.Variant<CheckOp, 's-op-raises-violates'> >

'is-Ann': PFunction<(val: any) => val is Ann>

'is-a-blank': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-blank'>>

'a-blank': TCH.Variant<Ann, 'a-blank'>

'is-a-any': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-any'>>

'a-any': PFunction< (l: Loc) => TCH.Variant<Ann, 'a-any'> >

'is-a-name': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-name'>>

'a-name': PFunction< (l: Loc, id: Name) => TCH.Variant<Ann, 'a-name'> >

'is-a-type-var': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-type-var'>>

'a-type-var': PFunction< (l: Loc, id: Name) => TCH.Variant<Ann, 'a-type-var'> >

'is-a-arrow': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-arrow'>>

'a-arrow': 
  PFunction<
    (l: Loc, args: List<Ann>, ret: Ann, use_parens: boolean) => TCH.Variant<Ann, 'a-arrow'>
  >

'is-a-arrow-argnames': 
  PFunction<(val: any) => val is TCH.Variant<Ann, 'a-arrow-argnames'>>

'a-arrow-argnames': 
  PFunction<
    (l: Loc, args: List<AField>, ret: Ann, use_parens: boolean) => TCH.Variant<Ann, 'a-arrow-argnames'>
  >

'is-a-method': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-method'>>

'a-method': 
  PFunction< (l: Loc, args: List<Ann>, ret: Ann) => TCH.Variant<Ann, 'a-method'> >

'is-a-record': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-record'>>

'a-record': 
  PFunction< (l: Loc, fields: List<AField>) => TCH.Variant<Ann, 'a-record'> >

'is-a-tuple': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-tuple'>>

'a-tuple': 
  PFunction< (l: Loc, fields: List<Ann>) => TCH.Variant<Ann, 'a-tuple'> >

'is-a-app': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-app'>>

'a-app': 
  PFunction< (l: Loc, ann: Ann, args: List<Ann>) => TCH.Variant<Ann, 'a-app'> >

'is-a-pred': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-pred'>>

'a-pred': PFunction< (l: Loc, ann: Ann, exp: Expr) => TCH.Variant<Ann, 'a-pred'> >

'is-a-dot': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-dot'>>

'a-dot': 
  PFunction< (l: Loc, obj: Name, field: string) => TCH.Variant<Ann, 'a-dot'> >

'is-a-checked': PFunction<(val: any) => val is TCH.Variant<Ann, 'a-checked'>>

'a-checked': 
  PFunction< (checked: Ann, residual: Ann) => TCH.Variant<Ann, 'a-checked'> >

'is-AField': PFunction<(val: any) => val is AField>

'is-a-field': PFunction<(val: any) => val is TCH.Variant<AField, 'a-field'>>

'a-field': 
  PFunction< (l: Loc, name: string, ann: Ann) => TCH.Variant<AField, 'a-field'> >

'make-checker-name': PFunction<(name: string) => string>

}}}}
