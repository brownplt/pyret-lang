export type List<T> =
| { $name: 'empty', dict : {} }
| { $name: 'link', dict : { first: T, rest: List<T> }}
export type Option<T> =
| { $name: 'none', dict : {} }
| { $name: 'some', dict : { value: T }}

type NumInteger = any;

export type Srcloc =
    | { $name: 'builtin',
        dict: {
          'module-name': string
        }
      }
    | { $name: 'srcloc',
        dict: {
          source : string,
          'start-line': number,
          'start-column': number,
          'start-char': number,
          'end-line' : number,
          'end-column' : number,
          'end-char' : number
        }
      }

type Loc = Srcloc

export type Name =
    | { $name: 's-underscore',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-name',
        dict: {
            l: Loc,
            s: string,
        }
      }
    | { $name: 's-global',
        dict: {
            s: string,
        }
      }
    | { $name: 's-module-global',
        dict: {
            s: string,
        }
      }
    | { $name: 's-type-global',
        dict: {
            s: string,
        }
      }
    | { $name: 's-atom',
        dict: {
            base: string,
            serial: number,
        }
      }

export type AppInfo =
    | { $name: 'app-info-c',
        dict: {
            'is-recursive': boolean,
            'is-tail': boolean,
        }
      }

export type PrimAppInfo =
    | { $name: 'prim-app-info-c',
        dict: {
            'needs-step': boolean,
        }
      }

export type Program =
    | { $name: 's-program',
        dict: {
            l: Loc,
            _provide: Provide,
            'provided-types': ProvideTypes,
            provides: List<ProvideBlock>,
            imports: List<Import>,
            block: Expr,
        }
      }
export type Import =
    | { $name: 's-include',
        dict: {
            l: Loc,
            mod: ImportType,
        }
      }
    | { $name: 's-include-from',
        dict: {
            l: Loc,
            mod: List<Name>,
            specs: List<IncludeSpec>,
        }
      }
    | { $name: 's-import',
        dict: {
            l: Loc,
            file: ImportType,
            name: Name,
        }
      }
    | { $name: 's-import-types',
        dict: {
            l: Loc,
            file: ImportType,
            name: Name,
            types: Name,
        }
      }
    | { $name: 's-import-fields',
        dict: {
            l: Loc,
            fields: List<Name>,
            file: ImportType,
        }
      }

export type IncludeSpec =
    | { $name: 's-include-name',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
        }
      }
    | { $name: 's-include-data',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
            hidden: List<Name>,
        }
      }
    | { $name: 's-include-type',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
        }
      }
    | { $name: 's-include-module',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
        }
      }

export type ProvidedModule =
    | { $name: 'p-module',
        dict: {
            l: Loc,
            name: string,
            v: Name,
            uri: string,
        }
      }

export type ProvidedValue =
    | { $name: 'p-value',
        dict: {
            l: Loc,
            v: Name,
            ann: Ann,
        }
      }

export type ProvidedAlias =
    | { $name: 'p-alias',
        dict: {
            l: Loc,
            'in-name': Name,
            'out-name': Name,
            mod: Option<ImportType>,
        }
      }

export type ProvidedDatatype =
    | { $name: 'p-data',
        dict: {
            l: Loc,
            d: Name,
            mod: Option<ImportType>,
        }
      }

export type Provide =
    | { $name: 's-provide',
        dict: {
            l: Loc,
            block: Expr,
        }
      }
    | { $name: 's-provide-all',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-provide-none',
        dict: {
            l: Loc,
        }
      }

export type ProvideBlock =
    | { $name: 's-provide-block',
        dict: {
            l: Loc,
            path: List<Name>,
            specs: List<ProvideSpec>,
        }
      }

export type ProvideSpec =
    | { $name: 's-provide-name',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
        }
      }
    | { $name: 's-provide-data',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
            hidden: List<Name>,
        }
      }
    | { $name: 's-provide-type',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
        }
      }
    | { $name: 's-provide-module',
        dict: {
            l: Loc,
            'name-spec': NameSpec,
        }
      }

export type NameSpec =
    | { $name: 's-star',
        dict: {
            l: Loc,
            hidden: List<Name>,
        }
      }
    | { $name: 's-module-ref',
        dict: {
            l: Loc,
            path: List<Name>,
            'as-name': Option<Name>,
        }
      }
    | { $name: 's-remote-ref',
        dict: {
            l: Loc,
            uri: string,
            name: Name,
            'as-name': Name,
        }
      }
    | { $name: 's-local-ref',
        dict: {
            l: Loc,
            name: Name,
            'as-name': Name,
        }
      }

export type ProvideTypes =
    | { $name: 's-provide-types',
        dict: {
            l: Loc,
            ann: List<AField>,
        }
      }
    | { $name: 's-provide-types-all',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-provide-types-none',
        dict: {
            l: Loc,
        }
      }

export type ImportType =
    | { $name: 's-const-import',
        dict: {
            l: Loc,
            mod: string,
        }
      }
    | { $name: 's-special-import',
        dict: {
            l: Loc,
            kind: string,
            args: List<string>,
        }
      }

export type Hint =
    | { $name: 'h-use-loc',
        dict: {
            l: Loc,
        }
      }

export type LetBind =
    | { $name: 's-let-bind',
        dict: {
            l: Loc,
            b: Bind,
            value: Expr,
        }
      }
    | { $name: 's-var-bind',
        dict: {
            l: Loc,
            b: Bind,
            value: Expr,
        }
      }

export type LetrecBind =
    | { $name: 's-letrec-bind',
        dict: {
            l: Loc,
            b: Bind,
            value: Expr,
        }
      }

export type TypeLetBind =
    | { $name: 's-type-bind',
        dict: {
            l: Loc,
            name: Name,
            params: List<Name>,
            ann: Ann,
        }
      }
    | { $name: 's-newtype-bind',
        dict: {
            l: Loc,
            name: Name,
            namet: Name,
        }
      }

export type DefinedModule =
    | { $name: 's-defined-module',
        dict: {
            name: string,
            value: Name,
            uri: string,
        }
      }

export type DefinedValue =
    | { $name: 's-defined-value',
        dict: {
            name: string,
            value: Expr,
        }
      }
    | { $name: 's-defined-var',
        dict: {
            name: string,
            id: Name,
        }
      }

export type DefinedType =
    | { $name: 's-defined-type',
        dict: {
            name: string,
            typ: Ann,
        }
      }

export type Expr =
    | { $name: 's-module',
        dict: {
            l: Loc,
            answer: Expr,
            'defined-modules': List<DefinedModule>,
            'defined-values': List<DefinedValue>,
            'defined-types': List<DefinedType>,
            checks: Expr,
        }
      }
    | { $name: 's-template',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-type-let-expr',
        dict: {
            l: Loc,
            binds: List<TypeLetBind>,
            body: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-let-expr',
        dict: {
            l: Loc,
            binds: List<LetBind>,
            body: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-letrec',
        dict: {
            l: Loc,
            binds: List<LetrecBind>,
            body: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-hint-exp',
        dict: {
            l: Loc,
            hints: List<Hint>,
            exp: Expr,
        }
      }
    | { $name: 's-instantiate',
        dict: {
            l: Loc,
            expr: Expr,
            params: List<Ann>,
        }
      }
    | { $name: 's-block',
        dict: {
            l: Loc,
            stmts: List<Expr>,
        }
      }
    | { $name: 's-user-block',
        dict: {
            l: Loc,
            body: Expr,
        }
      }
    | { $name: 's-fun',
        dict: {
            l: Loc,
            name: string,
            params: List<Name>,
            args: List<Bind>,
            ann: Ann,
            doc: string,
            body: Expr,
            '_check-loc': Option<Loc>,
            _check: Option<Expr>,
            blocky: boolean,
        }
      }
    | { $name: 's-type',
        dict: {
            l: Loc,
            name: Name,
            params: List<Name>,
            ann: Ann,
        }
      }
    | { $name: 's-newtype',
        dict: {
            l: Loc,
            name: Name,
            namet: Name,
        }
      }
    | { $name: 's-var',
        dict: {
            l: Loc,
            name: Bind,
            value: Expr,
        }
      }
    | { $name: 's-rec',
        dict: {
            l: Loc,
            name: Bind,
            value: Expr,
        }
      }
    | { $name: 's-let',
        dict: {
            l: Loc,
            name: Bind,
            value: Expr,
            'keyword-val': boolean,
        }
      }
    | { $name: 's-ref',
        dict: {
            l: Loc,
            ann: Option<Ann>,
        }
      }
    | { $name: 's-contract',
        dict: {
            l: Loc,
            name: Name,
            params: List<Name>,
            ann: Ann,
        }
      }
    | { $name: 's-when',
        dict: {
            l: Loc,
            test: Expr,
            block: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-assign',
        dict: {
            l: Loc,
            id: Name,
            value: Expr,
        }
      }
    | { $name: 's-if-pipe',
        dict: {
            l: Loc,
            branches: List<IfPipeBranch>,
            blocky: boolean,
        }
      }
    | { $name: 's-if-pipe-else',
        dict: {
            l: Loc,
            branches: List<IfPipeBranch>,
            _else: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-if',
        dict: {
            l: Loc,
            branches: List<IfBranch>,
            blocky: boolean,
        }
      }
    | { $name: 's-if-else',
        dict: {
            l: Loc,
            branches: List<IfBranch>,
            _else: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-cases',
        dict: {
            l: Loc,
            typ: Ann,
            val: Expr,
            branches: List<CasesBranch>,
            blocky: boolean,
        }
      }
    | { $name: 's-cases-else',
        dict: {
            l: Loc,
            typ: Ann,
            val: Expr,
            branches: List<CasesBranch>,
            _else: Expr,
            blocky: boolean,
        }
      }
    | { $name: 's-op',
        dict: {
            l: Loc,
            'op-l': Loc,
            op: string,
            left: Expr,
            right: Expr,
        }
      }
    | { $name: 's-check-test',
        dict: {
            l: Loc,
            op: CheckOp,
            refinement: Option<Expr>,
            left: Expr,
            right: Option<Expr>,
            cause: Option<Expr>,
        }
      }
    | { $name: 's-check-expr',
        dict: {
            l: Loc,
            expr: Expr,
            ann: Ann,
        }
      }
    | { $name: 's-paren',
        dict: {
            l: Loc,
            expr: Expr,
        }
      }
    | { $name: 's-lam',
        dict: {
            l: Loc,
            name: string,
            params: List<Name>,
            args: List<Bind>,
            ann: Ann,
            doc: string,
            body: Expr,
            '_check-loc': Option<Loc>,
            _check: Option<Expr>,
            blocky: boolean,
        }
      }
    | { $name: 's-method',
        dict: {
            l: Loc,
            name: string,
            params: List<Name>,
            args: List<Bind>,
            ann: Ann,
            doc: string,
            body: Expr,
            '_check-loc': Option<Loc>,
            _check: Option<Expr>,
            blocky: boolean,
        }
      }
    | { $name: 's-extend',
        dict: {
            l: Loc,
            supe: Expr,
            fields: List<Member>,
        }
      }
    | { $name: 's-update',
        dict: {
            l: Loc,
            supe: Expr,
            fields: List<Member>,
        }
      }
    | { $name: 's-tuple',
        dict: {
            l: Loc,
            fields: List<Expr>,
        }
      }
    | { $name: 's-tuple-get',
        dict: {
            l: Loc,
            tup: Expr,
            index: number,
            'index-loc': Loc,
        }
      }
    | { $name: 's-obj',
        dict: {
            l: Loc,
            fields: List<Member>,
        }
      }
    | { $name: 's-array',
        dict: {
            l: Loc,
            values: List<Expr>,
        }
      }
    | { $name: 's-construct',
        dict: {
            l: Loc,
            modifier: ConstructModifier,
            constructor: Expr,
            values: List<Expr>,
        }
      }
    | { $name: 's-app',
        dict: {
            l: Loc,
            _fun: Expr,
            args: List<Expr>,
        }
      }
    | { $name: 's-app-enriched',
        dict: {
            l: Loc,
            _fun: Expr,
            args: List<Expr>,
            'app-info': AppInfo,
        }
      }
    | { $name: 's-prim-app',
        dict: {
            l: Loc,
            _fun: string,
            args: List<Expr>,
            'app-info': PrimAppInfo,
        }
      }
    | { $name: 's-prim-val',
        dict: {
            l: Loc,
            name: string,
        }
      }
    | { $name: 's-id',
        dict: {
            l: Loc,
            id: Name,
        }
      }
    | { $name: 's-id-var',
        dict: {
            l: Loc,
            id: Name,
        }
      }
    | { $name: 's-id-letrec',
        dict: {
            l: Loc,
            id: Name,
            safe: boolean,
        }
      }
    | { $name: 's-id-var-modref',
        dict: {
            l: Loc,
            id: Name,
            uri: string,
            name: string,
        }
      }
    | { $name: 's-id-modref',
        dict: {
            l: Loc,
            id: Name,
            uri: string,
            name: string,
        }
      }
    | { $name: 's-undefined',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-srcloc',
        dict: {
            l: Loc,
            loc: Loc,
        }
      }
    | { $name: 's-num',
        dict: {
            l: Loc,
            n: number,
        }
      }
    | { $name: 's-frac',
        dict: {
            l: Loc,
            num: NumInteger,
            den: NumInteger,
        }
      }
    | { $name: 's-rfrac',
        dict: {
            l: Loc,
            num: NumInteger,
            den: NumInteger,
        }
      }
    | { $name: 's-bool',
        dict: {
            l: Loc,
            b: boolean,
        }
      }
    | { $name: 's-str',
        dict: {
            l: Loc,
            s: string,
        }
      }
    | { $name: 's-dot',
        dict: {
            l: Loc,
            obj: Expr,
            field: string,
        }
      }
    | { $name: 's-get-bang',
        dict: {
            l: Loc,
            obj: Expr,
            field: string,
        }
      }
    | { $name: 's-bracket',
        dict: {
            l: Loc,
            obj: Expr,
            key: Expr,
        }
      }
    | { $name: 's-data',
        dict: {
            l: Loc,
            name: string,
            params: List<Name>,
            mixins: List<Expr>,
            variants: List<Variant>,
            'shared-members': List<Member>,
            '_check-loc': Option<Loc>,
            _check: Option<Expr>,
        }
      }
    | { $name: 's-data-expr',
        dict: {
            l: Loc,
            name: string,
            namet: Name,
            params: List<Name>,
            mixins: List<Expr>,
            variants: List<Variant>,
            'shared-members': List<Member>,
            '_check-loc': Option<Loc>,
            _check: Option<Expr>,
        }
      }
    | { $name: 's-for',
        dict: {
            l: Loc,
            iterator: Expr,
            bindings: List<ForBind>,
            ann: Ann,
            body: Expr,
            blocky: boolean,
        }
      }  
    | { $name: 's-check',
        dict: {
            l: Loc,
            name: Option<string>,
            body: Expr,
            'keyword-check': boolean,
        }
      }
    | { $name: 's-reactor',
        dict: {
            l: Loc,
            fields: List<Member>,
        }
      }
    | { $name: 's-table-extend',
        dict: {
            l: Loc,
            'column-binds': ColumnBinds,
            extensions: List<TableExtendField>,
        }
      }
    | { $name: 's-table-update',
        dict: {
            l: Loc,
            'column-binds': ColumnBinds,
            extensions: List<TableExtendField>,
        }
      }
    | { $name: 's-table-select',
        dict: {
            l: Loc,
            'column-binds': ColumnBinds,
            updates: List<Member>,
        }
      }
    | { $name: 's-table-order',
        dict: {
            l: Loc,
            table  : Expr,
            ordering: List<ColumnSort>,
        }
      }
    | { $name: 's-table-filter',
        dict: {
            l: Loc,
            'column-binds': ColumnBinds,
            predicate: Expr,
        }
      }
    | { $name: 's-table-extract',
        dict: {
            l: Loc,
            column: Name,
            table: Expr,
        }
      }
    | { $name: 's-table',
        dict: {
            column: Name,
            table  : Expr,
        }
      }
    | { $name: 's-load-table',
        dict: {
            l: Loc,
            headers: List<FieldName>,
            spec: List<LoadTableSpec>,
        }
      }
    | { $name: 's-spy-block',
        dict: {
            l: Loc,
            message: Option<Expr>,
            contents: List<SpyField>,
        }
      }

export type TableRow =
    | { $name: 's-table-row',
        dict: {
            l: Loc,
            elems: List<Expr>,
            
        }
      }

export type SpyField =
    | { $name: 's-spy-expr',
        dict: {
            l: Loc,
            name: string,
            value: Expr,
            'implicit-label': boolean,
        }
      }

export type ConstructModifier =
    | { $name: 's-construct-normal',
        dict: {
        }
      }
    | { $name: 's-construct-lazy',
        dict: {
        }
      }

export type Bind =
    | { $name: 's-bind',
        dict: {
            l: Loc,
            shadows: boolean,
            id: Name,
            ann: Ann,
        }
      }
    | { $name: 's-tuple-bind',
        dict: {
            l: Loc,
            fields: List<Bind>,
            'as-name': Option<Bind>,
        }
      }

export type Member =
    | { $name: 's-data-field',
        dict: {
            l: Loc,
            name: string,
            value: Expr,
        }
      }
    | { $name: 's-mutable-field',
        dict: {
            l: Loc,
            name: string,
            ann: Ann,
            value: Expr,
        }
      }
    | { $name: 's-method-field',
        dict: {
            l: Loc,
            name: string,
            params: List<Name>,
            args: List<Bind>,
            ann: Ann,
            doc: string,
            body: Expr,
            '_check-loc': Option<Loc>,
            _check: Option<Expr>,
            blocky: boolean,
        }
      }

export type FieldName =
    | { $name: 's-field-name',
        dict: {
            l: Loc,
            name: string,
            ann: Ann,
        }
      }

export type ForBind =
    | { $name: 's-for-bind',
        dict: {
            l: Loc,
            bind: Bind,
            value: Expr,
        }
      }

export type ColumnBinds =
    | { $name: 's-column-binds',
        dict: {
            l: Loc,
            binds: List<Bind>,
            table: Expr,
        }
      }

export type ColumnSortOrder =
    | { $name: 'ASCENDING',
        dict: {
        }
      }
    | { $name: 'DESCENDING',
        dict: {
        }
      }

export type ColumnSort =
    | { $name: 's-column-sort',
        dict: {
            l        : Loc,
            column   : Name,
            direction: ColumnSortOrder,
        }
      }

export type TableExtendField =
    | { $name: 's-table-extend-field',
        dict: {
            l: Loc,
            name: string,
            value: Expr,
            ann: Ann,
        }
      }
    | { $name: 's-table-extend-reducer',
        dict: {
            l: Loc,
            name: string,
            reducer: Expr,
            col: Name,
            ann: Ann,
        }
      }

export type LoadTableSpec =
    | { $name: 's-sanitize',
        dict: {
            l: Loc,
            name: Name,
            sanitizer: Expr,
        }
      }
    | { $name: 's-table-src',
        dict: {
            l: Loc,
            src: Expr,
        }
      }

export type VariantMemberType =
    | { $name: 's-normal',
        dict: {
        }
      }
    | { $name: 's-mutable',
        dict: {
        }
      }

export type VariantMember =
    | { $name: 's-variant-member',
        dict: {
            l: Loc,
            'member-type': VariantMemberType,
            bind: Bind,
        }
      }

export type Variant =
    | { $name: 's-variant',
        dict: {
            l: Loc,
            'constr-loc': Loc,
            name: string,
            members: List<VariantMember>,
            'with-members': List<Member>,
        }
      }
    | { $name: 's-singleton-variant',
        dict: {
            l: Loc,
            name: string,
            'with-members': List<Member>,
        }
      }

export type IfBranch =
    | { $name: 's-if-branch',
        dict: {
            l: Loc,
            test: Expr,
            body: Expr,
        }
      }

export type IfPipeBranch =
    | { $name: 's-if-pipe-branch',
        dict: {
            l: Loc,
            test: Expr,
            body: Expr,
        }
      }

export type CasesBindType =
    | { $name: 's-cases-bind-ref',
        dict: {
        }
      }
    | { $name: 's-cases-bind-normal',
        dict: {
        }
      }

export type CasesBind =
    | { $name: 's-cases-bind',
        dict: {
            l: Loc,
            'field-type': CasesBindType,
            bind: Bind,
        }
      }

export type CasesBranch =
    | { $name: 's-cases-branch',
        dict: {
            l: Loc,
            'pat-loc': Loc,
            name: string,
            args: List<CasesBind>,
            body: Expr,
        }
      }
    | { $name: 's-singleton-cases-branch',
        dict: {
            l: Loc,
            'pat-loc': Loc,
            name: string,
            body: Expr,
        }
      }

export type CheckOp =
    | { $name: 's-op-is',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-is-roughly',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-is-op',
        dict: {
            l: Loc,
            op: string,
        }
      }
    | { $name: 's-op-is-not',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-is-not-op',
        dict: {
            l: Loc,
            op: string,
        }
      }
    | { $name: 's-op-satisfies',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-satisfies-not',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-raises',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-raises-other',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-raises-not',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-raises-satisfies',
        dict: {
            l: Loc,
        }
      }
    | { $name: 's-op-raises-violates',
        dict: {
            l: Loc,
        }
      }

export type Ann =
    | { $name: 'a-blank',
        dict: {
        }
      }
    | { $name: 'a-any',
        dict: {
            l: Loc,
        }
      }
    | { $name: 'a-name',
        dict: {
            l: Loc,
            id: Name,
        }
      }
    | { $name: 'a-type-var',
        dict: {
            l: Loc,
            id: Name,
        }
      }
    | { $name: 'a-arrow',
        dict: {
            l: Loc,
            args: List<Ann>,
            ret: Ann,
            'use-parens': boolean,
        }
      }
    | { $name: 'a-arrow-argnames',
        dict: {
            l: Loc,
            args: List<AField>,
            ret: Ann,
            'use-parens': boolean,
        }
      }
    | { $name: 'a-method',
        dict: {
            l: Loc,
            args: List<Ann>,
            ret: Ann,
        }
      }
    | { $name: 'a-record',
        dict: {
            l: Loc,
            fields: List<AField>,
        }
      }
    | { $name: 'a-tuple',
        dict: {
            l: Loc,
            fields: List<AField>,
        }
      }
    | { $name: 'a-app',
        dict: {
            l: Loc,
            ann: Ann,
            args: List<Ann>,
        }
      }
    | { $name: 'a-pred',
        dict: {
            l: Loc,
            ann: Ann,
            exp: Expr,
        }
      }
    | { $name: 'a-dot',
        dict: {
            l: Loc,
            obj: Name,
            field: string,
        }
      }
    | { $name: 'a-checked',
        dict: {
            checked: Ann,
            residual: Ann,
        }
      }

export type AField =
    | { $name: 'a-field',
        dict: {
            l: Loc,
            name: string,
            ann: Ann,
        }
      }
