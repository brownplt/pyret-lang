
provide *
include ast
import global as _
import base as _
import lists as lists

default-map-visitor = {
  method option(self, opt):
    cases(Option) opt:
      | none => none
      | some(v) => some(v.visit(self))
    end
  end,

  method s-underscore(self, l):
    s-underscore(l)
  end,

  method s-name(self, l, s):
    s-name(l, s)
  end,

  method s-type-global(self, s):
    s-type-global(s)
  end,

  method s-global(self, s):
    s-global(s)
  end,

  method s-atom(self, base, serial):
    s-atom(base, serial)
  end,

  method s-defined-value(self, name, val):
    s-defined-value(name, val.visit(self))
  end,
  method s-defined-var(self, name, id):
    s-defined-var(name, id.visit(self))
  end,
  method s-defined-type(self, name, typ):
    s-defined-type(name, typ.visit(self))
  end,

  method s-module(self, l, answer, dv, dt, provides, types, checks):
    s-module(l, answer.visit(self), dv.map(_.visit(self)), dt.map(_.visit(self)), provides.visit(self), lists.map(_.visit(self), types), checks.visit(self))
  end,

  method s-program(self, l, _provide, provided-types, imports, body):
    s-program(l, _provide.visit(self), provided-types.visit(self), imports.map(_.visit(self)), body.visit(self))
  end,

  method s-include(self, l, import-type):
    s-include(l, import-type.visit(self))
  end,
  method s-import(self, l, import-type, name):
    s-import(l, import-type.visit(self), name.visit(self))
  end,
  method s-import-complete(self, l, values, types, mod, vals-name, types-name):
    s-import-complete(
      l,
      values.map(_.visit(self)),
      types.map(_.visit(self)),
      mod.visit(self),
      vals-name.visit(self),
      types-name.visit(self))
  end,
  method s-const-import(self, l, mod):
    s-const-import(l, mod)
  end,
  method s-special-import(self, l, kind, args):
    s-special-import(l, kind, args)
  end,
  method s-import-types(self, l, import-type, name, types):
    s-import-types(l, import-type, name.visit(self), types.visit(self))
  end,
  method s-import-fields(self, l, fields, import-type):
    s-import-fields(l, fields.map(_.visit(self)), import-type)
  end,
  method s-provide-complete(self, l, vals, typs, datas):
    s-provide-complete(l, vals, typs, datas)
  end,
  method s-provide(self, l, expr):
    s-provide(l, expr.visit(self))
  end,
  method s-provide-all(self, l):
    s-provide-all(l)
  end,
  method s-provide-none(self, l):
    s-provide-none(l)
  end,
  method s-provide-types(self, l, anns):
    s-provide-types(l, anns.map(_.visit(self)))
  end,
  method s-provide-types-all(self, l):
    s-provide-types-all(l)
  end,
  method s-provide-types-none(self, l):
    s-provide-types-none(l)
  end,

  method s-bind(self, l, shadows, name, ann):
    s-bind(l, shadows, name.visit(self), ann.visit(self))
  end,

  method s-tuple-bind(self, l, fields, as-name):
    s-tuple-bind(l, fields.map(_.visit(self)), self.option(as-name))
  end,

  method s-var-bind(self, l, bind, expr):
    s-var-bind(l, bind.visit(self), expr.visit(self))
  end,
  method s-let-bind(self, l, bind, expr):
    s-let-bind(l, bind.visit(self), expr.visit(self))
  end,

  method s-type-bind(self, l, name, params, ann):
    s-type-bind(l, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-newtype-bind(self, l, name, namet):
    s-newtype-bind(l, name.visit(self), namet.visit(self))
  end,

  method s-type-let-expr(self, l, binds, body, blocky):
    s-type-let-expr(l, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-template(self, l):
    s-template(l)
  end,

  method s-let-expr(self, l, binds, body, blocky):
    s-let-expr(l, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-letrec-bind(self, l, bind, expr):
    s-letrec-bind(l, bind.visit(self), expr.visit(self))
  end,

  method s-letrec(self, l, binds, body, blocky):
    s-letrec(l, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(l, hints, exp.visit(self))
  end,

  method s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    s-instantiate(l, expr.visit(self), params.map(_.visit(self)))
  end,

  method s-block(self, l, stmts):
    s-block(l, stmts.map(_.visit(self)))
  end,

  method s-user-block(self, l :: Loc, body :: Expr):
    s-user-block(l, body.visit(self))
  end,

  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    s-fun(l, name, params, args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), _check-loc, self.option(_check), blocky)
  end,

  method s-type(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    s-type(l, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-newtype(self, l :: Loc, name :: Name, namet :: Name):
    s-newtype(l, name.visit(self), namet.visit(self))
  end,

  method s-var(self, l :: Loc, name :: Bind, value :: Expr):
    s-var(l, name.visit(self), value.visit(self))
  end,

  method s-rec(self, l :: Loc, name :: Bind, value :: Expr):
    s-rec(l, name.visit(self), value.visit(self))
  end,

  method s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean):
    s-let(l, name.visit(self), value.visit(self), keyword-val)
  end,

  method s-ref(self, l :: Loc, ann :: Option<Ann>):
    s-ref(l, self.option(ann))
  end,

  method s-when(self, l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean):
    s-when(l, test.visit(self), block.visit(self), blocky)
  end,

  method s-contract(self, l, name, ann):
    s-contract(l, name.visit(self), ann.visit(self))
  end,

  method s-assign(self, l :: Loc, id :: Name, value :: Expr):
    s-assign(l, id.visit(self), value.visit(self))
  end,

  method s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-branch(l, test.visit(self), body.visit(self))
  end,

  method s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-pipe-branch(l, test.visit(self), body.visit(self))
  end,

  method s-if(self, l :: Loc, branches :: List<IfBranch>, blocky :: Boolean):
    s-if(l, branches.map(_.visit(self)), blocky)
  end,
  method s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean):
    s-if-else(l, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean):
    s-if-pipe(l, branches.map(_.visit(self)), blocky)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean):
    s-if-pipe-else(l, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-cases-bind(self, l :: Loc, typ :: CasesBindType, bind :: Bind):
    s-cases-bind(l, typ, bind.visit(self))
  end,
  method s-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr):
    s-cases-branch(l, pat-loc, name, args.map(_.visit(self)), body.visit(self))
  end,

  method s-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: Expr):
    s-singleton-cases-branch(l, pat-loc, name, body.visit(self))
  end,

  method s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean):
    s-cases(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), blocky)
  end,
  method s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean):
    s-cases-else(l, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-op(self, l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-op(l, op-l, op, left.visit(self), right.visit(self))
  end,

  method s-check-test(self, l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>):
    s-check-test(l, op, self.option(refinement), left.visit(self), self.option(right))
  end,

  method s-check-expr(self, l :: Loc, expr :: Expr, ann :: Ann):
    s-check-expr(l, expr.visit(self), ann.visit(self))
  end,

  method s-paren(self, l :: Loc, expr :: Expr):
    s-paren(l, expr.visit(self))
  end,

  method s-lam(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-lam(l, name, params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), _check-loc, self.option(_check), blocky)
  end,
  method s-method(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method(l, name, params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), _check-loc, self.option(_check), blocky)
  end,
  method s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-extend(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-update(l, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-tuple(self, l :: Loc, fields :: List<Expr>):
    s-tuple(l, fields.map(_.visit(self)))
  end,
  method s-tuple-get(self, l :: Loc, tup :: Expr, index :: Number, index-loc :: Loc):
    s-tuple-get(l, tup.visit(self), index, index-loc)
  end,
  method s-obj(self, l :: Loc, fields :: List<Member>):
    s-obj(l, fields.map(_.visit(self)))
  end,
  method s-array(self, l :: Loc, values :: List<Expr>):
    s-array(l, values.map(_.visit(self)))
  end,
  method s-construct(self, l :: Loc, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(l, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  method s-reactor(self, l :: Loc, fields :: List<Member>):
    s-reactor(l, fields.map(_.visit(self)))
  end,
  method s-table(self, l :: Loc, headers :: List<FieldName>, rows :: List<TableRow>):
    s-table(l, headers.map(_.visit(self)), rows.map(_.visit(self)))
  end,
  method s-table-row(self, l :: Loc, elems :: List<Expr>):
    s-table-row(l, elems.map(_.visit(self)))
  end,
  method s-load-table(self, l, headers :: List<FieldName>, spec :: List<LoadTableSpec>):
    s-load-table(l, headers.map(_.visit(self)), spec.map(_.visit(self)))
  end,
  method s-field-name(self, l :: Loc, name :: String, ann :: Ann):
    s-field-name(l, name, ann.visit(self))
  end,
  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    s-app(l, _fun.visit(self), args.map(_.visit(self)))
  end,
  method s-app-enriched(self, l :: Loc, _fun :: Expr, args :: List<Expr>, app-info :: AppInfo):
    s-app-enriched(l, _fun.visit(self), args.map(_.visit(self)), app-info)
  end,
  method s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>):
    s-prim-app(l, _fun, args.map(_.visit(self)))
  end,
  method s-prim-val(self, l :: Loc, name :: String):
    s-prim-val(l, name)
  end,
  method s-id(self, l :: Loc, id :: Name):
    s-id(l, id.visit(self))
  end,
  method s-id-var(self, l :: Loc, id :: Name):
    s-id-var(l, id.visit(self))
  end,
  method s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    s-id-letrec(l, id.visit(self), safe)
  end,
  method s-undefined(self, l :: Loc):
    s-undefined(self)
  end,
  method s-srcloc(self, l, shadow loc):
    s-srcloc(l, loc)
  end,
  method s-num(self, l :: Loc, n :: Number):
    s-num(l, n)
  end,
  method s-frac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-frac(l, num, den)
  end,
  method s-rfrac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-rfrac(l, num, den)
  end,
  method s-bool(self, l :: Loc, b :: Boolean):
    s-bool(l, b)
  end,
  method s-str(self, l :: Loc, s :: String):
    s-str(l, s)
  end,
  method s-dot(self, l :: Loc, obj :: Expr, field :: String):
    s-dot(l, obj.visit(self), field)
  end,
  method s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    s-get-bang(l, obj.visit(self), field)
  end,
  method s-bracket(self, l :: Loc, obj :: Expr, key :: Expr):
    s-bracket(l, obj.visit(self), key.visit(self))
  end,
  method s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data(
        l,
        name,
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        self.option(_check)
      )
  end,
  method s-data-expr(
      self,
      l :: Loc,
      name :: String,
      namet :: Name,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        l,
        name,
        namet.visit(self),
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        self.option(_check)
      )
  end,
  method s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
    ):
    s-for(l, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self), blocky)
  end,
  method s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    s-check(l, name, body.visit(self), keyword-check)
  end,

  method s-data-field(self, l :: Loc, name :: String, value :: Expr):
    s-data-field(l, name, value.visit(self))
  end,
  method s-mutable-field(self, l :: Loc, name :: String, ann :: Ann, value :: Expr):
    s-mutable-field(l, name, ann.visit(self), value.visit(self))
  end,
  method s-method-field(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method-field(
      l,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self),
      _check-loc,
      self.option(_check),
      blocky
      )
  end,

  method s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    s-for-bind(l, bind.visit(self), value.visit(self))
  end,
  method s-column-binds(self, l :: Loc, binds :: List<Bind>, table :: Expr):
    s-column-binds(l, binds.map(_.visit(self)), table.visit(self))
  end,
  method s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(l, member-type, bind.visit(self))
  end,
  method s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(l, constr-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  method s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(l, name, with-members.map(_.visit(self)))
  end,
  method s-column-sort(self, l, column :: Name, direction :: ColumnSortOrder):
    s-column-sort(l, column.visit(self), direction)
  end,
  method s-table-extend(self, l, column-binds :: ColumnBinds, extensions :: List<Member>):
    s-table-extend(l, column-binds.visit(self), extensions.map(_.visit(self)))
  end,
  method s-table-update(self, l, column-binds :: ColumnBinds, updates :: List<Member>):
    s-table-update(l, column-binds.visit(self), updates.map(_.visit(self)))
  end,
  method s-table-filter(self, l, column-binds :: ColumnBinds, predicate :: Expr):
    s-table-filter(l, column-binds.visit(self), predicate.visit(self))
  end,
  method s-table-select(self, l, columns :: List<Name>, table :: Expr):
    s-table-select(l, columns.map(_.visit(self)), table.visit(self))
  end,
  method s-table-order(self, l, table :: Expr, ordering :: List<ColumnSort>):
    s-table-order(l, table.visit(self), ordering.map(_.visit(self)))
  end,
  method s-table-extract(self, l, column :: Name, table :: Expr):
    s-table-extract(l, column.visit(self), table.visit(self))
  end,
  method s-table-extend-field(self, l, name :: String, value :: Expr, ann :: Ann):
    s-table-extend-field(l, name, value.visit(self), ann.visit(self))
  end,
  method s-table-extend-reducer(self, l, name :: String, reducer :: Expr, col :: Name, ann :: Ann):
    s-table-extend-reducer(l, name, reducer.visit(self),
      col.visit(self), ann.visit(self))
  end,
  method s-sanitize(self, l, name :: Name, sanitizer :: Expr):
    s-sanitize(l, name.visit(self), sanitizer.visit(self))
  end,
  method s-table-src(self, l, src :: Expr):
    s-table-src(l, src.visit(self))
  end,
  
  method s-spy-block(self, l :: Loc, message :: Option<Expr>, contents :: List<SpyField>):
    s-spy-block(l, self.option(message), contents.map(_.visit(self)))
  end,
  method s-spy-name(self, l :: Loc, name :: Expr%(is-s-id)):
    s-spy-name(l, name.visit(self))
  end,
  method s-spy-expr(self, l :: Loc, name :: String, value :: Expr):
    s-spy-expr(l, name, value.visit(self))
  end,

  method a-blank(self): a-blank end,
  method a-any(self, l): a-any(l) end,
  method a-name(self, l, id): a-name(l, id.visit(self)) end,
  method a-type-var(self, l, id): a-type-var(l, id.visit(self)) end,
  method a-arrow(self, l, args, ret, use-parens):
    a-arrow(l, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-arrow-argnames(self, l, args, ret, use-parens):
    a-arrow-argnames(l, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-method(self, l, args, ret):
    a-method(l, args.map(_.visit(self)), ret.visit(self))
  end,
  method a-record(self, l, fields):
    a-record(l, fields.map(_.visit(self)))
  end,
  method a-tuple(self, l, fields):
    a-tuple(l, fields.map(_.visit(self)))
  end,
  method a-app(self, l, ann, args):
    a-app(l, ann.visit(self), args.map(_.visit(self)))
  end,
  method a-pred(self, l, ann, exp):
    a-pred(l, ann.visit(self), exp.visit(self))
  end,
  method a-dot(self, l, obj, field):
    a-dot(l, obj.visit(self), field)
  end,
  method a-field(self, l, name, ann):
    a-field(l, name, ann.visit(self))
  end
}


default-iter-visitor = {
  method option(self, opt):
    cases(Option) opt:
      | none => true
      | some(v) => v.visit(self)
    end
  end,

  method s-underscore(self, l):
    true
  end,
  method s-name(self, l, s):
    true
  end,
  method s-global(self, s):
    true
  end,
  method s-type-global(self, s):
    true
  end,
  method s-atom(self, base, serial):
    true
  end,

  method s-defined-value(self, name, val):
    val.visit(self)
  end,
  method s-defined-var(self, name, id):
    id.visit(self)
  end,
  method s-defined-type(self, name, typ):
    typ.visit(self)
  end,

  method s-module(self, l, answer, dv, dt, provides, types, checks):
    answer.visit(self) and lists.all(_.visit(self), dv) and lists.all(_.visit(self), dt) and provides.visit(self) and lists.all(_.visit(self), types) and checks.visit(self)
  end,

  method s-program(self, l, _provide, provided-types, imports, body):
    _provide.visit(self)
    and provided-types.visit(self)
    and lists.all(_.visit(self), imports)
    and body.visit(self)
  end,

  method s-import(self, l, import-type, name):
    import-type.visit(self) and name.visit(self)
  end,
  method s-import-complete(self, l, values, types, mod, vals-name, types-name):
    lists.all(_.visit(self), values) and
      lists.all(_.visit(self), types) and
      mod.visit(self) and
      vals-name.visit(self) and
      types-name.visit(self)
  end,
  method s-include(self, l, import-type):
    import-type.visit(self)
  end,
  method s-const-import(self, l, mod):
    true
  end,
  method s-special-import(self, l, kind, args):
    true
  end,
  method s-import-types(self, l, import-type, name, types):
    name.visit(self) and types.visit(self)
  end,
  method s-import-fields(self, l, fields, import-type):
    lists.all(_.visit(self), fields)
  end,
  method s-provide-complete(self, l, vals, typs, datas):
    true
  end,
  method s-provide(self, l, expr):
    expr.visit(self)
  end,
  method s-provide-all(self, l):
    true
  end,
  method s-provide-none(self, l):
    true
  end,
  method s-provide-types(self, l, anns):
    lists.all(_.visit(self), anns)
  end,
  method s-provide-types-all(self, l):
    true
  end,
  method s-provide-types-none(self, l):
    true
  end,

  method s-template(self, l):
    true
  end,

  method s-bind(self, l, shadows, name, ann):
    name.visit(self) and ann.visit(self)
  end,

  method s-tuple-bind(self, l, fields, as-name):
    lists.all(_.visit(self), fields) and self.option(as-name)
  end,

  method s-var-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,
  method s-let-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,

  method s-type-bind(self, l, name, params, ann):
    name.visit(self) and ann.visit(self) and lists.all(_.visit(self), params)
  end,

  method s-newtype-bind(self, l, name, namet):
    name.visit(self) and namet.visit(self)
  end,

  method s-type-let-expr(self, l, binds, body, blocky):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  method s-let-expr(self, l, binds, body, blocky):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  method s-letrec-bind(self, l, bind, expr):
    bind.visit(self) and expr.visit(self)
  end,

  method s-letrec(self, l, binds, body, blocky):
    lists.all(_.visit(self), binds) and body.visit(self)
  end,

  method s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    exp.visit(self)
  end,

  method s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    expr.visit(self) and lists.all(_.visit(self), params)
  end,

  method s-block(self, l, stmts):
    lists.all(_.visit(self), stmts)
  end,

  method s-user-block(self, l :: Loc, body :: Expr):
    body.visit(self)
  end,

  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,

  method s-type(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    name.visit(self) and ann.visit(self) and lists.all(_.visit(self), params)
  end,

  method s-newtype(self, l :: Loc, name :: Name, namet :: Name):
    name.visit(self) and namet.visit(self)
  end,

  method s-var(self, l :: Loc, name :: Bind, value :: Expr):
    name.visit(self) and value.visit(self)
  end,

  method s-rec(self, l :: Loc, name :: Bind, value :: Expr):
    name.visit(self) and value.visit(self)
  end,

  method s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean):
    name.visit(self) and value.visit(self)
  end,

  method s-ref(self, l :: Loc, ann :: Option<Ann>):
    self.option(ann)
  end,

  method s-when(self, l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean):
    test.visit(self) and block.visit(self)
  end,

  method s-contract(self, l :: Loc, name :: Name, ann :: Ann):
    name.visit(self) and ann.visit(self)
  end,

  method s-assign(self, l :: Loc, id :: Name, value :: Expr):
    id.visit(self) and value.visit(self)
  end,

  method s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,

  method s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    test.visit(self) and body.visit(self)
  end,

  method s-if(self, l :: Loc, branches :: List<IfBranch>, blocky :: Boolean):
    lists.all(_.visit(self), branches)
  end,
  method s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean):
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,

  method s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean):
    lists.all(_.visit(self), branches)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean):
    lists.all(_.visit(self), branches) and _else.visit(self)
  end,

  method s-cases-bind(self, l :: Loc, typ :: CasesBindType, bind :: Bind):
    bind.visit(self)
  end,
  method s-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr):
    lists.all(_.visit(self), args) and body.visit(self)
  end,

  method s-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: Expr):
    body.visit(self)
  end,

  method s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean):
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches)
  end,
  method s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean):
    typ.visit(self) and val.visit(self) and lists.all(_.visit(self), branches) and _else.visit(self)
  end,

  method s-op(self, l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr):
    left.visit(self) and right.visit(self)
  end,

  method s-check-test(self, l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>):
    self.option(refinement) and left.visit(self) and self.option(right)
  end,

  method s-check-expr(self, l :: Loc, expr :: Expr, ann :: Ann):
    expr.visit(self) and ann.visit(self)
  end,

  method s-paren(self, l :: Loc, expr :: Expr):
    expr.visit(self)
  end,

  method s-lam(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
      ):
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  method s-method(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
      ):
    lists.all(_.visit(self), params) and lists.all(_.visit(self), args) and ann.visit(self) and body.visit(self) and self.option(_check)
  end,
  method s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and lists.all(_.visit(self), fields)
  end,
  method s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    supe.visit(self) and lists.all(_.visit(self), fields)
  end,
  method s-tuple(self, l :: Loc, fields :: List<Expr>):
    lists.all(_.visit(self), fields)
  end,
  method s-tuple-get(self, l :: Loc, tup :: Expr, index :: Number, index-loc :: Loc):
    tup.visit(self)
  end,
  method s-obj(self, l :: Loc, fields :: List<Member>):
    lists.all(_.visit(self), fields)
  end,
  method s-array(self, l :: Loc, values :: List<Expr>):
    lists.all(_.visit(self), values)
  end,
  method s-construct(self, l :: Loc, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    constructor.visit(self) and lists.all(_.visit(self), values)
  end,
  method s-reactor(self, l :: Loc, fields :: List<Member>):
    lists.all(_.visit(self), fields)
  end,
  method s-table(self, l :: Loc, headers :: List<FieldName>, rows :: List<TableRow>):
    lists.all(_.visit(self), headers) and lists.all(_.visit(self), rows)
  end,
  method s-table-row(self, l :: Loc, elems :: List<Expr>):
    lists.all(_.visit(self), elems)
  end,
  method s-load-table(self, l :: Loc, headers :: List<FieldName>, spec :: List<LoadTableSpec>):
    lists.all(_.visit(self), headers) and lists.all(_.visit(self), spec)
  end,
  method s-field-name(self, l :: Loc, name :: String, ann :: Ann):
    true
  end,
  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    _fun.visit(self) and lists.all(_.visit(self), args)
  end,
  method s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>):
    lists.all(_.visit(self), args)
  end,
  method s-prim-val(self, l :: Loc, name :: String):
    true
  end,
  method s-id(self, l :: Loc, id :: Name):
    id.visit(self)
  end,
  method s-id-var(self, l :: Loc, id :: Name):
    id.visit(self)
  end,
  method s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    id.visit(self)
  end,
  method s-undefined(self, l :: Loc):
    true
  end,
  method s-srcloc(self, l, shadow loc):
    true
  end,
  method s-num(self, l :: Loc, n :: Number):
    true
  end,
  method s-frac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    true
  end,
  method s-rfrac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    true
  end,
  method s-bool(self, l :: Loc, b :: Boolean):
    true
  end,
  method s-str(self, l :: Loc, s :: String):
    true
  end,
  method s-dot(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  method s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    obj.visit(self)
  end,
  method s-bracket(self, l :: Loc, obj :: Expr, key :: Expr):
    obj.visit(self) and key.visit(self)
  end,
  method s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
      ):
    lists.all(_.visit(self), params)
    and lists.all(_.visit(self), mixins)
    and lists.all(_.visit(self), variants)
    and lists.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  method s-data-expr(
      self,
      l :: Loc,
      name :: String,
      namet :: Name,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
      ):
    namet.visit(self)
    and lists.all(_.visit(self), params)
    and lists.all(_.visit(self), mixins)
    and lists.all(_.visit(self), variants)
    and lists.all(_.visit(self), shared-members)
    and self.option(_check)
  end,
  method s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
      ):
    iterator.visit(self) and lists.all(_.visit(self), bindings) and ann.visit(self) and body.visit(self)
  end,
  method s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    body.visit(self)
  end,

  method s-data-field(self, l :: Loc, name :: String, value :: Expr):
    value.visit(self)
  end,
  method s-mutable-field(self, l :: Loc, name :: String, ann :: Ann, value :: Expr):
    ann.visit(self) and value.visit(self)
  end,
  method s-method-field(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
      ):
    lists.all(_.visit(self), args)
    and lists.all(_.visit(self), args)
    and ann.visit(self)
    and body.visit(self)
    and self.option(_check)
  end,

  method s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    bind.visit(self) and value.visit(self)
  end,
  method s-column-binds(self, l :: Loc, binds :: List<Bind>, table :: Expr):
    binds.all(_.visit(self)) and table.visit(self)
  end,
  method s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    bind.visit(self)
  end,
  method s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
      ):
    lists.all(_.visit(self), members) and lists.all(_.visit(self), with-members)
  end,
  method s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
      ):
    lists.all(_.visit(self), with-members)
  end,
  method s-column-sort(self, l, column :: Name, direction :: ColumnSortOrder):
    column.visit(self)
  end,
  method s-table-extend(self, l, column-binds :: ColumnBinds, extensions :: List<Member>):
    column-binds.visit(self) and extensions.all(_.visit(self))
  end,
  method s-table-update(self, l, column-binds :: ColumnBinds, updates :: List<Member>):
    column-binds.visit(self) and updates.all(_.visit(self))
  end,
  method s-table-filter(self, l, column-binds :: ColumnBinds, predicate :: Expr):
    column-binds.visit(self) and predicate.visit(self)
  end,
  method s-table-select(self, l, columns :: List<Name>, table :: Expr):
    columns.all(_.visit(self)) and table.visit(self)
  end,
  method s-table-order(self, l, table :: Expr, ordering :: List<ColumnSort>):
    table.visit(self) and ordering.all(_.visit(self))
  end,
  method s-table-extract(self, l, column :: Name, table :: Expr):
    column.visit(self) and table.visit(self)
  end,
  method s-table-extend-field(self, l, name :: String, value :: Expr, ann :: Ann):
    value.visit(self) and ann.visit(self)
  end,
  method s-table-extend-reducer(self, l, name :: String, reducer :: Expr, col :: Name, ann :: Ann):
    reducer.visit(self) and col.visit(self) and ann.visit(self)
  end,
  method s-sanitize(self, l, name, sanitizer):
    name.visit(self) and sanitizer.visit(self)
  end,
  method s-table-src(self, l, src):
    src.visit(self)
  end,
    
  method s-spy-block(self, l :: Loc, message :: Option<Expr>, contents :: List<SpyField>):
    self.option(message) and lists.all(_.visit(self), contents)
  end,
  method s-spy-name(self, l :: Loc, name :: Expr%(is-s-id)):
    name.visit(self)
  end,
  method s-spy-expr(self, l :: Loc, name :: String, value :: Expr):
    value.visit(self)
  end,
  
  method a-blank(self):
    true
  end,
  method a-any(self, l):
    true
  end,
  method a-name(self, l, id):
    true
  end,
  method a-type-var(self, l, id):
    true
  end,
  method a-arrow(self, l, args, ret, _):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  method a-arrow-argnames(self, l, args, ret, _):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  method a-method(self, l, args, ret):
    lists.all(_.visit(self), args) and ret.visit(self)
  end,
  method a-record(self, l, fields):
    lists.all(_.visit(self), fields)
  end,
  method a-tuple(self, l, fields):
    lists.all(_.visit(self), fields)
  end,
  method a-app(self, l, ann, args):
    ann.visit(self) and lists.all(_.visit(self), args)
  end,
  method a-pred(self, l, ann, exp):
    ann.visit(self) and exp.visit(self)
  end,
  method a-dot(self, l, obj, field):
    obj.visit(self)
  end,
  method a-field(self, l, name, ann):
    ann.visit(self)
  end
}

dummy-loc-visitor = {
  method option(self, opt):
    cases(Option) opt:
      | none => none
      | some(v) => some(v.visit(self))
    end
  end,

  method s-underscore(self, l):
    s-underscore(dummy-loc)
  end,
  method s-name(self, l, s):
    s-name(dummy-loc, s)
  end,
  method s-global(self, s):
    s-global(s)
  end,
  method s-type-global(self, s):
    s-type-global(s)
  end,
  method s-atom(self, base, serial):
    s-atom(base, serial)
  end,

  method s-defined-value(self, name, val):
    s-defined-value(name, val.visit(self))
  end,
  method s-defined-var(self, name, id):
    s-defined-var(name, id.visit(self))
  end,
  method s-defined-type(self, name, typ):
    s-defined-type(name, typ.visit(self))
  end,

  method s-module(self, l, answer, dv, dt, provides, types, checks):
    s-module(dummy-loc,
      answer.visit(self), dv.map(_.visit(self)), dt.map(_.visit(self)), provides.visit(self), lists.map(_.visit(self), types), checks.visit(self))
  end,

  method s-program(self, l, _provide, provided-types, imports, body):
    s-program(dummy-loc, _provide.visit(self), provided-types.visit(self), imports.map(_.visit(self)), body.visit(self))
  end,

  method s-const-import(self, l :: Loc, mod :: String):
    s-const-import(dummy-loc, mod)
  end,
  method s-special-import(self, l, kind, args):
    s-special-import(dummy-loc, kind, args)
  end,
  method s-import(self, l, import-type, name):
    s-import(dummy-loc, import-type.visit(self), name.visit(self))
  end,
  method s-import-complete(self, l, values, types, mod, vals-name, types-name):
    s-import-complete(
      dummy-loc,
      values.map(_.visit(self)),
      types.map(_.visit(self)),
      mod.visit(self),
      vals-name.visit(self),
      types-name.visit(self))
  end,
  method s-include(self, l, import-type):
    s-include(dummy-loc, import-type.visit(self))
  end,
  method s-import-types(self, l, import-type, name, types):
    s-import-types(dummy-loc, import-type.visit(self), name.visit(self), types.visit(self))
  end,
  method s-import-fields(self, l, fields, import-type):
    s-import-fields(dummy-loc, fields.map(_.visit(self)), import-type.visit(self))
  end,
  method s-provide-complete(self, l, vals, typs, datas):
    s-provide-complete(dummy-loc, vals, typs, datas)
  end,
  method s-provide(self, l, expr):
    s-provide(dummy-loc, expr.visit(self))
  end,
  method s-provide-all(self, l):
    s-provide-all(dummy-loc)
  end,
  method s-provide-none(self, l):
    s-provide-none(dummy-loc)
  end,
  method s-provide-types(self, l, anns):
    s-provide-types(dummy-loc, anns.map(_.visit(self)))
  end,
  method s-provide-types-all(self, l):
    s-provide-types-all(dummy-loc)
  end,
  method s-provide-types-none(self, l):
    s-provide-types-none(dummy-loc)
  end,

  method s-bind(self, l, shadows, name, ann):
    s-bind(dummy-loc, shadows, name.visit(self), ann.visit(self))
  end,

  method s-tuple-bind(self, l, fields, as-name):
    s-tuple-bind(dummy-loc, fields.map(_.visit(self)), self.option(as-name))
  end,

  method s-var-bind(self, l, bind, expr):
    s-var-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,
  method s-let-bind(self, l, bind, expr):
    s-let-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,

  method s-type-bind(self, l, name, params, ann):
    s-type-bind(dummy-loc, name, params, ann)
  end,

  method s-newtype-bind(self, l, name, namet):
    s-newtype-bind(l, name.visit(self), namet.visit(self))
  end,

  method s-template(self, l):
    s-template(dummy-loc)
  end,

  method s-type-let-expr(self, l, binds, body, blocky):
    s-type-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-let-expr(self, l, binds, body, blocky):
    s-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-letrec-bind(self, l, bind, expr):
    s-letrec-bind(dummy-loc, bind.visit(self), expr.visit(self))
  end,

  method s-letrec(self, l, binds, body, blocky):
    s-letrec(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
  end,

  method s-hint-exp(self, l :: Loc, hints :: List<Hint>, exp :: Expr):
    s-hint-exp(dummy-loc, hints, exp.visit(self))
  end,

  method s-instantiate(self, l :: Loc, expr :: Expr, params :: List<Ann>):
    s-instantiate(dummy-loc, expr.visit(self), params.map(_.visit(self)))
  end,

  method s-block(self, l, stmts):
    s-block(dummy-loc, stmts.map(_.visit(self)))
  end,

  method s-user-block(self, l :: Loc, body :: Expr):
    s-user-block(dummy-loc, body.visit(self))
  end,

  method s-fun(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    s-fun(dummy-loc, name, params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), if is-none(_check-loc): none else: some(dummy-loc) end, self.option(_check), blocky)
  end,

  method s-type(self, l :: Loc, name :: Name, params :: List<Name>, ann :: Ann):
    s-type(dummy-loc, name.visit(self), params.map(_.visit(self)), ann.visit(self))
  end,

  method s-newtype(self, l :: Loc, name :: Name, namet :: Name):
    s-newtype(dummy-loc, name.visit(self), namet.visit(self))
  end,

  method s-var(self, l :: Loc, name :: Bind, value :: Expr):
    s-var(dummy-loc, name.visit(self), value.visit(self))
  end,

  method s-rec(self, l :: Loc, name :: Bind, value :: Expr):
    s-rec(dummy-loc, name.visit(self), value.visit(self))
  end,

  method s-let(self, l :: Loc, name :: Bind, value :: Expr, keyword-val :: Boolean):
    s-let(dummy-loc, name.visit(self), value.visit(self), keyword-val)
  end,

  method s-ref(self, l :: Loc, ann :: Option<Ann>):
    s-ref(self, dummy-loc, self.option(ann))
  end,

  method s-when(self, l :: Loc, test :: Expr, block :: Expr, blocky :: Boolean):
    s-when(dummy-loc, test.visit(self), block.visit(self), blocky)
  end,

  method s-contract(self, l, name, ann):
    s-contract(dummy-loc, name.visit(self), ann.visit(self))
  end,

  method s-assign(self, l :: Loc, id :: Name, value :: Expr):
    s-assign(dummy-loc, id.visit(self), value.visit(self))
  end,

  method s-if-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  method s-if-pipe-branch(self, l :: Loc, test :: Expr, body :: Expr):
    s-if-pipe-branch(dummy-loc, test.visit(self), body.visit(self))
  end,

  method s-if(self, l :: Loc, branches :: List<IfBranch>, blocky :: Boolean):
    s-if(dummy-loc, branches.map(_.visit(self)), blocky)
  end,
  method s-if-else(self, l :: Loc, branches :: List<IfBranch>, _else :: Expr, blocky :: Boolean):
    s-if-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-if-pipe(self, l :: Loc, branches :: List<IfPipeBranch>, blocky :: Boolean):
    s-if-pipe(dummy-loc, branches.map(_.visit(self)), blocky)
  end,
  method s-if-pipe-else(self, l :: Loc, branches :: List<IfPipeBranch>, _else :: Expr, blocky :: Boolean):
    s-if-pipe-else(dummy-loc, branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-cases-bind(self, l :: Loc, typ :: CasesBindType, bind :: Bind):
    s-cases-bind(dummy-loc, l, typ, bind.visit(self))
  end,
  method s-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, args :: List<CasesBind>, body :: Expr):
    s-cases-branch(dummy-loc, dummy-loc, name, args.map(_.visit(self)), body.visit(self))
  end,

  method s-singleton-cases-branch(self, l :: Loc, pat-loc :: Loc, name :: String, body :: Expr):
    s-singleton-cases-branch(dummy-loc, dummy-loc, name, body.visit(self))
  end,

  method s-cases(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, blocky :: Boolean):
    s-cases(dummy-loc, typ.visit(self), val.visit(self), branches.map(_.visit(self)), blocky)
  end,
  method s-cases-else(self, l :: Loc, typ :: Ann, val :: Expr, branches :: List<CasesBranch>, _else :: Expr, blocky :: Boolean):
    s-cases-else(dummy-loc, typ.visit(self), val.visit(self), branches.map(_.visit(self)), _else.visit(self), blocky)
  end,

  method s-op(self, l :: Loc, op-l :: Loc, op :: String, left :: Expr, right :: Expr):
    s-op(dummy-loc, dummy-loc, op, left.visit(self), right.visit(self))
  end,

  method s-check-test(self, l :: Loc, op :: CheckOp, refinement :: Option<Expr>, left :: Expr, right :: Option<Expr>):
    s-check-test(dummy-loc, op, self.option(refinement), left.visit(self), self.option(right))
  end,

  method s-paren(self, l :: Loc, expr :: Expr):
    s-paren(dummy-loc, expr.visit(self))
  end,

  method s-lam(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>,
      ann :: Ann,
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-lam(dummy-loc, "", params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), if is-none(_check): none else: some(dummy-loc) end, self.option(_check), blocky)
  end,
  method s-method(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method(dummy-loc, "", params.map(_.visit(self)), args.map(_.visit(self)), ann.visit(self), doc, body.visit(self), if is-none(_check-loc): none else: some(dummy-loc) end, self.option(_check), blocky)
  end,
  method s-extend(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-extend(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-update(self, l :: Loc, supe :: Expr, fields :: List<Member>):
    s-update(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
  end,
  method s-obj(self, l :: Loc, fields :: List<Member>):
    s-obj(dummy-loc, fields.map(_.visit(self)))
  end,
  method s-array(self, l :: Loc, values :: List<Expr>):
    s-array(dummy-loc, values.map(_.visit(self)))
  end,
  method s-construct(self, l :: Loc, mod :: ConstructModifier, constructor :: Expr, values :: List<Expr>):
    s-construct(dummy-loc, mod, constructor.visit(self), values.map(_.visit(self)))
  end,
  method s-reactor(self, l :: Loc, fields):
    s-reactor(dummy-loc, fields.map(_.visit(self)))
  end,
  method s-table(self, l :: Loc, headers :: List<FieldName>, rows :: List<TableRow>):
    s-table(dummy-loc, headers.map(_.visit(self)), rows.map(_.visit(self)))
  end,
  method s-table-row(self, l :: Loc, elems :: List<Expr>):
    s-table-row(dummy-loc, elems.map(_.visit(self)))
  end,
  method s-field-name(self, l :: Loc, name :: String, ann :: Ann):
    s-field-name(dummy-loc, name, ann.visit(self))
  end,
  method s-load-table(self, l, headers, spec :: List<LoadTableSpec>):
    s-load-table(dummy-loc, headers.map(_.visit(self)), spec.map(_.visit(self)))
  end,
  method s-app(self, l :: Loc, _fun :: Expr, args :: List<Expr>):
    s-app(dummy-loc, _fun.visit(self), args.map(_.visit(self)))
  end,
  method s-prim-app(self, l :: Loc, _fun :: String, args :: List<Expr>):
    s-prim-app(dummy-loc, _fun, args.map(_.visit(self)))
  end,
  method s-prim-val(self, l :: Loc, name :: String):
    s-prim-val(dummy-loc, name)
  end,
  method s-id(self, l :: Loc, id :: Name):
    s-id(dummy-loc, id.visit(self))
  end,
  method s-id-var(self, l :: Loc, id :: Name):
    s-id-var(dummy-loc, id.visit(self))
  end,
  method s-id-letrec(self, l :: Loc, id :: Name, safe :: Boolean):
    s-id-letrec(dummy-loc, id.visit(self), safe)
  end,
  method s-undefined(self, l :: Loc):
    s-undefined(self)
  end,
  method s-srcloc(self, l, shadow loc):
    s-srcloc(dummy-loc, loc)
  end,
  method s-num(self, l :: Loc, n :: Number):
    s-num(dummy-loc, n)
  end,
  method s-frac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-frac(dummy-loc, num, den)
  end,
  method s-rfrac(self, l :: Loc, num :: NumInteger, den :: NumInteger):
    s-rfrac(dummy-loc, num, den)
  end,
  method s-bool(self, l :: Loc, b :: Boolean):
    s-bool(dummy-loc, b)
  end,
  method s-str(self, l :: Loc, s :: String):
    s-str(dummy-loc, s)
  end,
  method s-dot(self, l :: Loc, obj :: Expr, field :: String):
    s-dot(dummy-loc, obj.visit(self), field)
  end,
  method s-get-bang(self, l :: Loc, obj :: Expr, field :: String):
    s-get-bang(dummy-loc, obj.visit(self), field)
  end,
  method s-bracket(self, l :: Loc, obj :: Expr, key :: Expr):
    s-bracket(dummy-loc, obj.visit(self), key.visit(self))
  end,
  method s-data(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data(
        dummy-loc,
        name,
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        if is-none(_check-loc): none else: some(dummy-loc) end,
        self.option(_check)
      )
  end,
  method s-data-expr(
      self,
      l :: Loc,
      name :: String,
      namet :: String,
      params :: List<Name>, # type params
      mixins :: List<Expr>,
      variants :: List<Variant>,
      shared-members :: List<Member>,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>
    ):
    s-data-expr(
        dummy-loc,
        name,
        namet.visit(self),
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        if is-none(_check-loc): none else: some(dummy-loc) end,
        self.option(_check)
      )
  end,
  method s-for(
      self,
      l :: Loc,
      iterator :: Expr,
      bindings :: List<ForBind>,
      ann :: Ann,
      body :: Expr,
      blocky :: Boolean
    ):
    s-for(dummy-loc, iterator.visit(self), bindings.map(_.visit(self)), ann.visit(self), body.visit(self), blocky)
  end,
  method s-check(self, l :: Loc, name :: Option<String>, body :: Expr, keyword-check :: Boolean):
    s-check(dummy-loc, name, body.visit(self), keyword-check)
  end,

  method s-data-field(self, l :: Loc, name :: String, value :: Expr):
    s-data-field(dummy-loc, name, value.visit(self))
  end,
  method s-mutable-field(self, l :: Loc, name :: String, ann :: Ann, value :: Expr):
    s-mutable-field(dummy-loc, name, ann.visit(self), value.visit(self))
  end,
  method s-method-field(
      self,
      l :: Loc,
      name :: String,
      params :: List<Name>,
      args :: List<Bind>, # Value parameters
      ann :: Ann, # return type
      doc :: String,
      body :: Expr,
      _check-loc :: Option<Loc>,
      _check :: Option<Expr>,
      blocky :: Boolean
    ):
    s-method-field(
      dummy-loc,
      name,
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self),
      if is-none(_check-loc): none else: some(dummy-loc) end,
      self.option(_check),
      blocky
      )
  end,

  method s-for-bind(self, l :: Loc, bind :: Bind, value :: Expr):
    s-for-bind(dummy-loc, bind.visit(self), value.visit(self))
  end,
  method s-column-binds(self, l :: Loc, binds :: List<Bind>, table :: Expr):
    s-column-binds(dummy-loc, binds.map(_.visit(self)), table.visit(self))
  end,
  method s-variant-member(self, l :: Loc, member-type :: VariantMemberType, bind :: Bind):
    s-variant-member(dummy-loc, member-type, bind.visit(self))
  end,
  method s-variant(
      self,
      l :: Loc,
      constr-loc :: Loc,
      name :: String,
      members :: List<VariantMember>,
      with-members :: List<Member>
    ):
    s-variant(dummy-loc, dummy-loc, name, members.map(_.visit(self)), with-members.map(_.visit(self)))
  end,
  method s-singleton-variant(
      self,
      l :: Loc,
      name :: String,
      with-members :: List<Member>
    ):
    s-singleton-variant(dummy-loc, name, with-members.map(_.visit(self)))
  end,
  method s-column-sort(self, l, column :: Name, direction :: ColumnSortOrder):
    s-column-sort(dummy-loc, column.visit(self), direction)
  end,
  method s-table-extend(self, l, column-binds :: ColumnBinds, extensions :: List<Member>):
    s-table-extend(dummy-loc, column-binds.visit(self), extensions.map(_.visit(self)))
  end,
  method s-table-update(self, l, column-binds :: ColumnBinds, updates :: List<Member>):
    s-table-update(dummy-loc, column-binds.visit(self), updates.map(_.visit(self)))
  end,
  method s-table-filter(self, l, column-binds :: ColumnBinds, predicate :: Expr):
    s-table-filter(dummy-loc, column-binds.visit(self), predicate.visit(self))
  end,
  method s-table-select(self, l, columns :: List<Name>, table :: Expr):
    s-table-select(dummy-loc, columns.map(_.visit(self)), table.visit(self))
  end,
  method s-table-order(self, l, table :: Expr, ordering :: List<ColumnSort>):
    s-table-order(dummy-loc, table.visit(self), ordering.map(_.visit(self)))
  end,
  method s-table-extract(self, l, column :: Name, table :: Expr):
    s-table-extract(dummy-loc, column.visit(self), table.visit(self))
  end,
  method s-table-extend-field(self, l, name :: String, value :: Expr, ann :: Ann):
    s-table-extend-field(dummy-loc, name.visit(self), value.visit(self), ann.visit(self))
  end,
  method s-table-extend-reducer(self, l, name :: String, reducer :: Expr, col :: Name, ann :: Ann):
    s-table-extend-reducer(dummy-loc, name.visit(self), reducer.visit(self),
      col.visit(self), ann.visit(self))
  end,
  method s-sanitize(self, l, name :: Name, sanitizer :: Expr):
    s-sanitize(dummy-loc, name.visit(self), sanitizer.visit(self))
  end,
  method s-table-src(self, l, src :: Expr):
    s-table-src(dummy-loc, src.visit(self))
  end,

  method s-spy-block(self, l :: Loc, message :: Option<Expr>, contents :: List<SpyField>):
    s-spy-block(dummy-loc, self.option(message), contents.map(_.visit(self)))
  end,
  method s-spy-name(self, l :: Loc, name :: Expr%(is-s-id)):
    s-spy-name(dummy-loc, name.visit(self))
  end,
  method s-spy-expr(self, l :: Loc, name :: String, value :: Expr):
    s-spy-expr(dummy-loc, name, value.visit(self))
  end,

  method a-blank(self): a-blank end,
  method a-any(self, l): a-any(l) end,
  method a-name(self, l, id): a-name(dummy-loc, id.visit(self)) end,
  method a-type-var(self, l, id): a-type-var(dummy-loc, id.visit(self)) end,
  method a-arrow(self, l, args, ret, use-parens):
    a-arrow(dummy-loc, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-arrow-argnames(self, l, args, ret, use-parens):
    a-arrow-argnames(dummy-loc, args.map(_.visit(self)), ret.visit(self), use-parens)
  end,
  method a-method(self, l, args, ret):
    a-method(dummy-loc, args.map(_.visit(self)), ret.visit(self))
  end,
  method a-record(self, l, fields):
    a-record(dummy-loc, fields.map(_.visit(self)))
  end,
  method a-tuple(self, l, fields):
    a-tuple(dummy-loc, fields.map(_.visit(self)))
  end,
  method a-app(self, l, ann, args):
    a-app(dummy-loc, ann.visit(self), args.map(_.visit(self)))
  end,
  method a-pred(self, l, ann, exp):
    a-pred(dummy-loc, ann.visit(self), exp.visit(self))
  end,
  method a-dot(self, l, obj, field):
    a-dot(dummy-loc, obj, field)
  end,
  method a-field(self, l, name, ann):
    a-field(dummy-loc, name.visit(self), ann.visit(self))
  end
}
