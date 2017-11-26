provide *

include ast
import global as _
import base as _
default-map-visitor =
  {
    method s-underscore(self, l): s-underscore(l) end,
    method s-name(self, l, s): s-name(l, s) end,
    method s-global(self, s): s-global(s) end,
    method s-type-global(self, s): s-type-global(s) end,
    method s-atom(self, base, serial): s-atom(base, serial) end,
    method s-program(self, l, _provide, provided-types, imports, block):
      s-program(l,
        _provide.visit(self),
        provided-types.visit(self),
        imports.map(_.visit(self)),
        block.visit(self))
    end,
    method s-include(self, l, mod): s-include(l, mod.visit(self)) end,
    method s-import(self, l, file, name):
      s-import(l, file.visit(self), name.visit(self))
    end,
    method s-import-types(self, l, file, name, types):
      s-import-types(l, file.visit(self), name.visit(self), types.visit(self))
    end,
    method s-import-fields(self, l, fields, file):
      s-import-fields(l, fields.map(_.visit(self)), file.visit(self))
    end,
    method s-import-complete(
        self,
        l,
        values,
        types,
        import-type,
        vals-name,
        types-name
      ):
      s-import-complete(l,
        values.map(_.visit(self)),
        types.map(_.visit(self)),
        import-type.visit(self),
        vals-name.visit(self),
        types-name.visit(self))
    end,
    method s-provide(self, l, block): s-provide(l, block.visit(self)) end,
    method s-provide-complete(self, l, values, aliases, data-definitions):
      s-provide-complete(l, values, aliases, data-definitions)
    end,
    method s-provide-all(self, l): s-provide-all(l) end,
    method s-provide-none(self, l): s-provide-none(l) end,
    method s-provide-types(self, l, ann):
      s-provide-types(l, ann.map(_.visit(self)))
    end,
    method s-provide-types-all(self, l): s-provide-types-all(l) end,
    method s-provide-types-none(self, l): s-provide-types-none(l) end,
    method s-const-import(self, l, mod): s-const-import(l, mod) end,
    method s-special-import(self, l, kind, args):
      s-special-import(l, kind, args)
    end,
    method h-use-loc(self, l): h-use-loc(l) end,
    method s-let-bind(self, l, b, value):
      s-let-bind(l, b.visit(self), value.visit(self))
    end,
    method s-var-bind(self, l, b, value):
      s-var-bind(l, b.visit(self), value.visit(self))
    end,
    method s-letrec-bind(self, l, b, value):
      s-letrec-bind(l, b.visit(self), value.visit(self))
    end,
    method s-type-bind(self, l, name, params, ann):
      s-type-bind(l,
        name.visit(self),
        params.map(_.visit(self)),
        ann.visit(self))
    end,
    method s-newtype-bind(self, l, name, namet):
      s-newtype-bind(l, name.visit(self), namet.visit(self))
    end,
    method s-defined-value(self, name, value):
      s-defined-value(name, value.visit(self))
    end,
    method s-defined-var(self, name, id):
      s-defined-var(name, id.visit(self))
    end,
    method s-defined-type(self, name, typ):
      s-defined-type(name, typ.visit(self))
    end,
    method s-module(
        self,
        l,
        answer,
        defined-values,
        defined-types,
        provided-values,
        provided-types,
        checks
      ):
      s-module(l,
        answer.visit(self),
        defined-values.map(_.visit(self)),
        defined-types.map(_.visit(self)),
        provided-values.visit(self),
        provided-types.map(_.visit(self)),
        checks.visit(self))
    end,
    method s-template(self, l): s-template(l) end,
    method s-type-let-expr(self, l, binds, body, blocky):
      s-type-let-expr(l, binds.map(_.visit(self)), body.visit(self), blocky)
    end,
    method s-let-expr(self, l, binds, body, blocky):
      s-let-expr(l, binds.map(_.visit(self)), body.visit(self), blocky)
    end,
    method s-letrec(self, l, binds, body, blocky):
      s-letrec(l, binds.map(_.visit(self)), body.visit(self), blocky)
    end,
    method s-hint-exp(self, l, hints, exp):
      s-hint-exp(l, hints.map(_.visit(self)), exp.visit(self))
    end,
    method s-instantiate(self, l, expr, params):
      s-instantiate(l, expr.visit(self), params.map(_.visit(self)))
    end,
    method s-block(self, l, stmts): s-block(l, stmts.map(_.visit(self))) end,
    method s-user-block(self, l, body): s-user-block(l, body.visit(self)) end,
    method s-fun(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-fun(l,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-type(self, l, name, params, ann):
      s-type(l, name.visit(self), params.map(_.visit(self)), ann.visit(self))
    end,
    method s-newtype(self, l, name, namet):
      s-newtype(l, name.visit(self), namet.visit(self))
    end,
    method s-var(self, l, name, value):
      s-var(l, name.visit(self), value.visit(self))
    end,
    method s-rec(self, l, name, value):
      s-rec(l, name.visit(self), value.visit(self))
    end,
    method s-let(self, l, name, value, keyword-val):
      s-let(l, name.visit(self), value.visit(self), keyword-val)
    end,
    method s-ref(self, l, ann): s-ref(l, ann.and-then(_.visit(self))) end,
    method s-contract(self, l, name, ann):
      s-contract(l, name.visit(self), ann.visit(self))
    end,
    method s-when(self, l, test, block, blocky):
      s-when(l, test.visit(self), block.visit(self), blocky)
    end,
    method s-assign(self, l, id, value):
      s-assign(l, id.visit(self), value.visit(self))
    end,
    method s-if-pipe(self, l, branches, blocky):
      s-if-pipe(l, branches.map(_.visit(self)), blocky)
    end,
    method s-if-pipe-else(self, l, branches, _else, blocky):
      s-if-pipe-else(l, branches.map(_.visit(self)), _else.visit(self), blocky)
    end,
    method s-if(self, l, branches, blocky):
      s-if(l, branches.map(_.visit(self)), blocky)
    end,
    method s-if-else(self, l, branches, _else, blocky):
      s-if-else(l, branches.map(_.visit(self)), _else.visit(self), blocky)
    end,
    method s-cases(self, l, typ, val, branches, blocky):
      s-cases(l,
        typ.visit(self),
        val.visit(self),
        branches.map(_.visit(self)),
        blocky)
    end,
    method s-cases-else(self, l, typ, val, branches, _else, blocky):
      s-cases-else(l,
        typ.visit(self),
        val.visit(self),
        branches.map(_.visit(self)),
        _else.visit(self),
        blocky)
    end,
    method s-op(self, l, op-l, op, left, right):
      s-op(l, op-l, op, left.visit(self), right.visit(self))
    end,
    method s-check-test(self, l, op, refinement, left, right):
      s-check-test(l,
        op.visit(self),
        refinement.and-then(_.visit(self)),
        left.visit(self),
        right.and-then(_.visit(self)))
    end,
    method s-check-expr(self, l, expr, ann):
      s-check-expr(l, expr.visit(self), ann.visit(self))
    end,
    method s-paren(self, l, expr): s-paren(l, expr.visit(self)) end,
    method s-lam(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-lam(l,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-method(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-method(l,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-extend(self, l, supe, fields):
      s-extend(l, supe.visit(self), fields.map(_.visit(self)))
    end,
    method s-update(self, l, supe, fields):
      s-update(l, supe.visit(self), fields.map(_.visit(self)))
    end,
    method s-tuple(self, l, fields): s-tuple(l, fields.map(_.visit(self))) end,
    method s-tuple-get(self, l, tup, index, index-loc):
      s-tuple-get(l, tup.visit(self), index, index-loc)
    end,
    method s-obj(self, l, fields): s-obj(l, fields.map(_.visit(self))) end,
    method s-array(self, l, values): s-array(l, values.map(_.visit(self))) end,
    method s-construct(self, l, modifier, constructor, values):
      s-construct(l,
        modifier.visit(self),
        constructor.visit(self),
        values.map(_.visit(self)))
    end,
    method s-app(self, l, _fun, args):
      s-app(l, _fun.visit(self), args.map(_.visit(self)))
    end,
    method s-app-enriched(self, l, _fun, args, app-info):
      s-app-enriched(l, _fun.visit(self), args.map(_.visit(self)), app-info)
    end,
    method s-prim-app(self, l, _fun, args):
      s-prim-app(l, _fun, args.map(_.visit(self)))
    end,
    method s-prim-val(self, l, name): s-prim-val(l, name) end,
    method s-id(self, l, id): s-id(l, id.visit(self)) end,
    method s-id-var(self, l, id): s-id-var(l, id.visit(self)) end,
    method s-id-letrec(self, l, id, safe):
      s-id-letrec(l, id.visit(self), safe)
    end,
    method s-undefined(self, l): s-undefined(l) end,
    method s-srcloc(self, l, loc): s-srcloc(l, loc) end,
    method s-num(self, l, n): s-num(l, n) end,
    method s-frac(self, l, num, den): s-frac(l, num, den) end,
    method s-rfrac(self, l, num, den): s-rfrac(l, num, den) end,
    method s-bool(self, l, b): s-bool(l, b) end,
    method s-str(self, l, s): s-str(l, s) end,
    method s-dot(self, l, obj, field): s-dot(l, obj.visit(self), field) end,
    method s-get-bang(self, l, obj, field):
      s-get-bang(l, obj.visit(self), field)
    end,
    method s-bracket(self, l, obj, key):
      s-bracket(l, obj.visit(self), key.visit(self))
    end,
    method s-data(
        self,
        l,
        name,
        params,
        mixins,
        variants,
        shared-members,
        _check-loc,
        _check
      ):
      s-data(l,
        name,
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        _check.and-then(_.visit(self)))
    end,
    method s-data-expr(
        self,
        l,
        name,
        namet,
        params,
        mixins,
        variants,
        shared-members,
        _check-loc,
        _check
      ):
      s-data-expr(l,
        name,
        namet.visit(self),
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        _check.and-then(_.visit(self)))
    end,
    method s-for(self, l, iterator, bindings, ann, body, blocky):
      s-for(l,
        iterator.visit(self),
        bindings.map(_.visit(self)),
        ann.visit(self),
        body.visit(self),
        blocky)
    end,
    method s-check(self, l, name, body, keyword-check):
      s-check(l, name, body.visit(self), keyword-check)
    end,
    method s-reactor(self, l, fields):
      s-reactor(l, fields.map(_.visit(self)))
    end,
    method s-table-extend(self, l, column-binds, extensions):
      s-table-extend(l, column-binds.visit(self), extensions.map(_.visit(self)))
    end,
    method s-table-update(self, l, column-binds, updates):
      s-table-update(l, column-binds.visit(self), updates.map(_.visit(self)))
    end,
    method s-table-select(self, l, columns, table):
      s-table-select(l, columns.map(_.visit(self)), table.visit(self))
    end,
    method s-table-order(self, l, table, ordering):
      s-table-order(l, table.visit(self), ordering.map(_.visit(self)))
    end,
    method s-table-filter(self, l, column-binds, predicate):
      s-table-filter(l, column-binds.visit(self), predicate.visit(self))
    end,
    method s-table-extract(self, l, column, table):
      s-table-extract(l, column.visit(self), table.visit(self))
    end,
    method s-table(self, l, headers, rows):
      s-table(l, headers.map(_.visit(self)), rows.map(_.visit(self)))
    end,
    method s-load-table(self, l, headers, spec):
      s-load-table(l, headers.map(_.visit(self)), spec.map(_.visit(self)))
    end,
    method s-spy-block(self, l, message, contents):
      s-spy-block(l,
        message.and-then(_.visit(self)),
        contents.map(_.visit(self)))
    end,
    method s-table-row(self, l, elems):
      s-table-row(l, elems.map(_.visit(self)))
    end,
    method s-spy-name(self, l, name): s-spy-name(l, name) end,
    method s-spy-expr(self, l, name, value):
      s-spy-expr(l, name, value.visit(self))
    end,
    method s-construct-normal(self): s-construct-normal end,
    method s-construct-lazy(self): s-construct-lazy end,
    method s-bind(self, l, shadows, id, ann):
      s-bind(l, shadows, id.visit(self), ann.visit(self))
    end,
    method s-tuple-bind(self, l, fields, as-name):
      s-tuple-bind(l,
        fields.map(_.visit(self)),
        as-name.and-then(_.visit(self)))
    end,
    method s-data-field(self, l, name, value):
      s-data-field(l, name, value.visit(self))
    end,
    method s-mutable-field(self, l, name, ann, value):
      s-mutable-field(l, name, ann.visit(self), value.visit(self))
    end,
    method s-method-field(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-method-field(l,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-field-name(self, l, name, ann):
      s-field-name(l, name, ann.visit(self))
    end,
    method s-for-bind(self, l, bind, value):
      s-for-bind(l, bind.visit(self), value.visit(self))
    end,
    method s-column-binds(self, l, binds, table):
      s-column-binds(l, binds.map(_.visit(self)), table.visit(self))
    end,
    method ASCENDING(self): ASCENDING end,
    method DESCENDING(self): DESCENDING end,
    method s-column-sort(self, l, column, direction):
      s-column-sort(l, column.visit(self), direction.visit(self))
    end,
    method s-table-extend-field(self, l, name, value, ann):
      s-table-extend-field(l, name, value.visit(self), ann.visit(self))
    end,
    method s-table-extend-reducer(self, l, name, reducer, col, ann):
      s-table-extend-reducer(l,
        name,
        reducer.visit(self),
        col.visit(self),
        ann.visit(self))
    end,
    method s-sanitize(self, l, name, sanitizer):
      s-sanitize(l, name.visit(self), sanitizer.visit(self))
    end,
    method s-table-src(self, l, src): s-table-src(l, src.visit(self)) end,
    method s-normal(self): s-normal end,
    method s-mutable(self): s-mutable end,
    method s-variant-member(self, l, member-type, bind):
      s-variant-member(l, member-type.visit(self), bind.visit(self))
    end,
    method s-variant(self, l, constr-loc, name, members, with-members):
      s-variant(l,
        constr-loc,
        name,
        members.map(_.visit(self)),
        with-members.map(_.visit(self)))
    end,
    method s-singleton-variant(self, l, name, with-members):
      s-singleton-variant(l, name, with-members.map(_.visit(self)))
    end,
    method s-if-branch(self, l, test, body):
      s-if-branch(l, test.visit(self), body.visit(self))
    end,
    method s-if-pipe-branch(self, l, test, body):
      s-if-pipe-branch(l, test.visit(self), body.visit(self))
    end,
    method s-cases-bind(self, l, field-type, bind):
      s-cases-bind(l, field-type, bind.visit(self))
    end,
    method s-cases-branch(self, l, pat-loc, name, args, body):
      s-cases-branch(l,
        pat-loc,
        name,
        args.map(_.visit(self)),
        body.visit(self))
    end,
    method s-singleton-cases-branch(self, l, pat-loc, name, body):
      s-singleton-cases-branch(l, pat-loc, name, body.visit(self))
    end,
    method s-op-is(self, l): s-op-is(l) end,
    method s-op-is-roughly(self, l): s-op-is-roughly(l) end,
    method s-op-is-op(self, l, op): s-op-is-op(l, op) end,
    method s-op-is-not(self, l): s-op-is-not(l) end,
    method s-op-is-not-op(self, l, op): s-op-is-not-op(l, op) end,
    method s-op-satisfies(self, l): s-op-satisfies(l) end,
    method s-op-satisfies-not(self, l): s-op-satisfies-not(l) end,
    method s-op-raises(self, l): s-op-raises(l) end,
    method s-op-raises-other(self, l): s-op-raises-other(l) end,
    method s-op-raises-not(self, l): s-op-raises-not(l) end,
    method s-op-raises-satisfies(self, l): s-op-raises-satisfies(l) end,
    method s-op-raises-violates(self, l): s-op-raises-violates(l) end,
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
    method a-record(self, l, fields): a-record(l, fields.map(_.visit(self))) end,
    method a-tuple(self, l, fields): a-tuple(l, fields.map(_.visit(self))) end,
    method a-app(self, l, ann, args):
      a-app(l, ann.visit(self), args.map(_.visit(self)))
    end,
    method a-pred(self, l, ann, exp):
      a-pred(l, ann.visit(self), exp.visit(self))
    end,
    method a-dot(self, l, obj, field): a-dot(l, obj.visit(self), field) end,
    method a-checked(self, checked, residual):
      a-checked(checked.visit(self), residual.visit(self))
    end,
    method a-field(self, l, name, ann): a-field(l, name, ann.visit(self)) end
  }
default-iter-visitor =
  {
    method s-underscore(self, l): true end,
    method s-name(self, l, s): true end,
    method s-global(self, s): true end,
    method s-type-global(self, s): true end,
    method s-atom(self, base, serial): true end,
    method s-program(self, l, _provide, provided-types, imports, block):
      _provide.visit(self)
      and
      provided-types.visit(self) and imports.all(_.visit(self))
        and
        block.visit(self)
    end,
    method s-include(self, l, mod): mod.visit(self) end,
    method s-import(self, l, file, name):
      file.visit(self) and name.visit(self)
    end,
    method s-import-types(self, l, file, name, types):
      file.visit(self) and name.visit(self) and types.visit(self)
    end,
    method s-import-fields(self, l, fields, file):
      fields.all(_.visit(self)) and file.visit(self)
    end,
    method s-import-complete(
        self,
        l,
        values,
        types,
        import-type,
        vals-name,
        types-name
      ):
      values.all(_.visit(self))
      and
      types.all(_.visit(self)) and import-type.visit(self)
        and
        vals-name.visit(self) and types-name.visit(self)
    end,
    method s-provide(self, l, block): block.visit(self) end,
    method s-provide-complete(self, l, values, aliases, data-definitions):
      true
    end,
    method s-provide-all(self, l): true end,
    method s-provide-none(self, l): true end,
    method s-provide-types(self, l, ann): ann.all(_.visit(self)) end,
    method s-provide-types-all(self, l): true end,
    method s-provide-types-none(self, l): true end,
    method s-const-import(self, l, mod): true end,
    method s-special-import(self, l, kind, args): true end,
    method h-use-loc(self, l): true end,
    method s-let-bind(self, l, b, value):
      b.visit(self) and value.visit(self)
    end,
    method s-var-bind(self, l, b, value):
      b.visit(self) and value.visit(self)
    end,
    method s-letrec-bind(self, l, b, value):
      b.visit(self) and value.visit(self)
    end,
    method s-type-bind(self, l, name, params, ann):
      name.visit(self) and params.all(_.visit(self)) and ann.visit(self)
    end,
    method s-newtype-bind(self, l, name, namet):
      name.visit(self) and namet.visit(self)
    end,
    method s-defined-value(self, name, value): value.visit(self) end,
    method s-defined-var(self, name, id): id.visit(self) end,
    method s-defined-type(self, name, typ): typ.visit(self) end,
    method s-module(
        self,
        l,
        answer,
        defined-values,
        defined-types,
        provided-values,
        provided-types,
        checks
      ):
      answer.visit(self)
      and
      defined-values.all(_.visit(self)) and defined-types.all(_.visit(self))
        and
        provided-values.visit(self) and provided-types.all(_.visit(self))
        and
        checks.visit(self)
    end,
    method s-template(self, l): true end,
    method s-type-let-expr(self, l, binds, body, blocky):
      binds.all(_.visit(self)) and body.visit(self)
    end,
    method s-let-expr(self, l, binds, body, blocky):
      binds.all(_.visit(self)) and body.visit(self)
    end,
    method s-letrec(self, l, binds, body, blocky):
      binds.all(_.visit(self)) and body.visit(self)
    end,
    method s-hint-exp(self, l, hints, exp):
      hints.all(_.visit(self)) and exp.visit(self)
    end,
    method s-instantiate(self, l, expr, params):
      expr.visit(self) and params.all(_.visit(self))
    end,
    method s-block(self, l, stmts): stmts.all(_.visit(self)) end,
    method s-user-block(self, l, body): body.visit(self) end,
    method s-fun(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      params.all(_.visit(self))
      and
      args.all(_.visit(self)) and ann.visit(self) and body.visit(self)
        and
        _check.and-then(_.visit(self)).or-else(true)
    end,
    method s-type(self, l, name, params, ann):
      name.visit(self) and params.all(_.visit(self)) and ann.visit(self)
    end,
    method s-newtype(self, l, name, namet):
      name.visit(self) and namet.visit(self)
    end,
    method s-var(self, l, name, value):
      name.visit(self) and value.visit(self)
    end,
    method s-rec(self, l, name, value):
      name.visit(self) and value.visit(self)
    end,
    method s-let(self, l, name, value, keyword-val):
      name.visit(self) and value.visit(self)
    end,
    method s-ref(self, l, ann): ann.and-then(_.visit(self)).or-else(true) end,
    method s-contract(self, l, name, ann):
      name.visit(self) and ann.visit(self)
    end,
    method s-when(self, l, test, block, blocky):
      test.visit(self) and block.visit(self)
    end,
    method s-assign(self, l, id, value):
      id.visit(self) and value.visit(self)
    end,
    method s-if-pipe(self, l, branches, blocky): branches.all(_.visit(self)) end,
    method s-if-pipe-else(self, l, branches, _else, blocky):
      branches.all(_.visit(self)) and _else.visit(self)
    end,
    method s-if(self, l, branches, blocky): branches.all(_.visit(self)) end,
    method s-if-else(self, l, branches, _else, blocky):
      branches.all(_.visit(self)) and _else.visit(self)
    end,
    method s-cases(self, l, typ, val, branches, blocky):
      typ.visit(self) and val.visit(self) and branches.all(_.visit(self))
    end,
    method s-cases-else(self, l, typ, val, branches, _else, blocky):
      typ.visit(self)
      and
      val.visit(self) and branches.all(_.visit(self)) and _else.visit(self)
    end,
    method s-op(self, l, op-l, op, left, right):
      left.visit(self) and right.visit(self)
    end,
    method s-check-test(self, l, op, refinement, left, right):
      op.visit(self)
      and
      refinement.and-then(_.visit(self)).or-else(true) and left.visit(self)
        and
        right.and-then(_.visit(self)).or-else(true)
    end,
    method s-check-expr(self, l, expr, ann):
      expr.visit(self) and ann.visit(self)
    end,
    method s-paren(self, l, expr): expr.visit(self) end,
    method s-lam(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      params.all(_.visit(self))
      and
      args.all(_.visit(self)) and ann.visit(self) and body.visit(self)
        and
        _check.and-then(_.visit(self)).or-else(true)
    end,
    method s-method(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      params.all(_.visit(self))
      and
      args.all(_.visit(self)) and ann.visit(self) and body.visit(self)
        and
        _check.and-then(_.visit(self)).or-else(true)
    end,
    method s-extend(self, l, supe, fields):
      supe.visit(self) and fields.all(_.visit(self))
    end,
    method s-update(self, l, supe, fields):
      supe.visit(self) and fields.all(_.visit(self))
    end,
    method s-tuple(self, l, fields): fields.all(_.visit(self)) end,
    method s-tuple-get(self, l, tup, index, index-loc): tup.visit(self) end,
    method s-obj(self, l, fields): fields.all(_.visit(self)) end,
    method s-array(self, l, values): values.all(_.visit(self)) end,
    method s-construct(self, l, modifier, constructor, values):
      modifier.visit(self)
      and
      constructor.visit(self) and values.all(_.visit(self))
    end,
    method s-app(self, l, _fun, args):
      _fun.visit(self) and args.all(_.visit(self))
    end,
    method s-app-enriched(self, l, _fun, args, app-info):
      _fun.visit(self) and args.all(_.visit(self))
    end,
    method s-prim-app(self, l, _fun, args): args.all(_.visit(self)) end,
    method s-prim-val(self, l, name): true end,
    method s-id(self, l, id): id.visit(self) end,
    method s-id-var(self, l, id): id.visit(self) end,
    method s-id-letrec(self, l, id, safe): id.visit(self) end,
    method s-undefined(self, l): true end,
    method s-srcloc(self, l, loc): true end,
    method s-num(self, l, n): true end,
    method s-frac(self, l, num, den): true end,
    method s-rfrac(self, l, num, den): true end,
    method s-bool(self, l, b): true end,
    method s-str(self, l, s): true end,
    method s-dot(self, l, obj, field): obj.visit(self) end,
    method s-get-bang(self, l, obj, field): obj.visit(self) end,
    method s-bracket(self, l, obj, key): obj.visit(self) and key.visit(self) end,
    method s-data(
        self,
        l,
        name,
        params,
        mixins,
        variants,
        shared-members,
        _check-loc,
        _check
      ):
      params.all(_.visit(self))
      and
      mixins.all(_.visit(self)) and variants.all(_.visit(self))
        and
        shared-members.all(_.visit(self))
        and
        _check.and-then(_.visit(self)).or-else(true)
    end,
    method s-data-expr(
        self,
        l,
        name,
        namet,
        params,
        mixins,
        variants,
        shared-members,
        _check-loc,
        _check
      ):
      namet.visit(self)
      and
      params.all(_.visit(self)) and mixins.all(_.visit(self))
        and
        variants.all(_.visit(self)) and shared-members.all(_.visit(self))
        and
        _check.and-then(_.visit(self)).or-else(true)
    end,
    method s-for(self, l, iterator, bindings, ann, body, blocky):
      iterator.visit(self)
      and
      bindings.all(_.visit(self)) and ann.visit(self) and body.visit(self)
    end,
    method s-check(self, l, name, body, keyword-check): body.visit(self) end,
    method s-reactor(self, l, fields): fields.all(_.visit(self)) end,
    method s-table-extend(self, l, column-binds, extensions):
      column-binds.visit(self) and extensions.all(_.visit(self))
    end,
    method s-table-update(self, l, column-binds, updates):
      column-binds.visit(self) and updates.all(_.visit(self))
    end,
    method s-table-select(self, l, columns, table):
      columns.all(_.visit(self)) and table.visit(self)
    end,
    method s-table-order(self, l, table, ordering):
      table.visit(self) and ordering.all(_.visit(self))
    end,
    method s-table-filter(self, l, column-binds, predicate):
      column-binds.visit(self) and predicate.visit(self)
    end,
    method s-table-extract(self, l, column, table):
      column.visit(self) and table.visit(self)
    end,
    method s-table(self, l, headers, rows):
      headers.all(_.visit(self)) and rows.all(_.visit(self))
    end,
    method s-load-table(self, l, headers, spec):
      headers.all(_.visit(self)) and spec.all(_.visit(self))
    end,
    method s-spy-block(self, l, message, contents):
      message.and-then(_.visit(self)).or-else(true)
      and
      contents.all(_.visit(self))
    end,
    method s-table-row(self, l, elems): elems.all(_.visit(self)) end,
    method s-spy-name(self, l, name): true end,
    method s-spy-expr(self, l, name, value): value.visit(self) end,
    method s-construct-normal(self): true end,
    method s-construct-lazy(self): true end,
    method s-bind(self, l, shadows, id, ann):
      id.visit(self) and ann.visit(self)
    end,
    method s-tuple-bind(self, l, fields, as-name):
      fields.all(_.visit(self))
      and
      as-name.and-then(_.visit(self)).or-else(true)
    end,
    method s-data-field(self, l, name, value): value.visit(self) end,
    method s-mutable-field(self, l, name, ann, value):
      ann.visit(self) and value.visit(self)
    end,
    method s-method-field(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      params.all(_.visit(self))
      and
      args.all(_.visit(self)) and ann.visit(self) and body.visit(self)
        and
        _check.and-then(_.visit(self)).or-else(true)
    end,
    method s-field-name(self, l, name, ann): ann.visit(self) end,
    method s-for-bind(self, l, bind, value):
      bind.visit(self) and value.visit(self)
    end,
    method s-column-binds(self, l, binds, table):
      binds.all(_.visit(self)) and table.visit(self)
    end,
    method ASCENDING(self): true end,
    method DESCENDING(self): true end,
    method s-column-sort(self, l, column, direction):
      column.visit(self) and direction.visit(self)
    end,
    method s-table-extend-field(self, l, name, value, ann):
      value.visit(self) and ann.visit(self)
    end,
    method s-table-extend-reducer(self, l, name, reducer, col, ann):
      reducer.visit(self) and col.visit(self) and ann.visit(self)
    end,
    method s-sanitize(self, l, name, sanitizer):
      name.visit(self) and sanitizer.visit(self)
    end,
    method s-table-src(self, l, src): src.visit(self) end,
    method s-normal(self): true end,
    method s-mutable(self): true end,
    method s-variant-member(self, l, member-type, bind):
      member-type.visit(self) and bind.visit(self)
    end,
    method s-variant(self, l, constr-loc, name, members, with-members):
      members.all(_.visit(self)) and with-members.all(_.visit(self))
    end,
    method s-singleton-variant(self, l, name, with-members):
      with-members.all(_.visit(self))
    end,
    method s-if-branch(self, l, test, body):
      test.visit(self) and body.visit(self)
    end,
    method s-if-pipe-branch(self, l, test, body):
      test.visit(self) and body.visit(self)
    end,
    method s-cases-bind(self, l, field-type, bind): bind.visit(self) end,
    method s-cases-branch(self, l, pat-loc, name, args, body):
      args.all(_.visit(self)) and body.visit(self)
    end,
    method s-singleton-cases-branch(self, l, pat-loc, name, body):
      body.visit(self)
    end,
    method s-op-is(self, l): true end,
    method s-op-is-roughly(self, l): true end,
    method s-op-is-op(self, l, op): true end,
    method s-op-is-not(self, l): true end,
    method s-op-is-not-op(self, l, op): true end,
    method s-op-satisfies(self, l): true end,
    method s-op-satisfies-not(self, l): true end,
    method s-op-raises(self, l): true end,
    method s-op-raises-other(self, l): true end,
    method s-op-raises-not(self, l): true end,
    method s-op-raises-satisfies(self, l): true end,
    method s-op-raises-violates(self, l): true end,
    method a-blank(self): true end,
    method a-any(self, l): true end,
    method a-name(self, l, id): id.visit(self) end,
    method a-type-var(self, l, id): id.visit(self) end,
    method a-arrow(self, l, args, ret, use-parens):
      args.all(_.visit(self)) and ret.visit(self)
    end,
    method a-arrow-argnames(self, l, args, ret, use-parens):
      args.all(_.visit(self)) and ret.visit(self)
    end,
    method a-method(self, l, args, ret):
      args.all(_.visit(self)) and ret.visit(self)
    end,
    method a-record(self, l, fields): fields.all(_.visit(self)) end,
    method a-tuple(self, l, fields): fields.all(_.visit(self)) end,
    method a-app(self, l, ann, args):
      ann.visit(self) and args.all(_.visit(self))
    end,
    method a-pred(self, l, ann, exp): ann.visit(self) and exp.visit(self) end,
    method a-dot(self, l, obj, field): obj.visit(self) end,
    method a-checked(self, checked, residual):
      checked.visit(self) and residual.visit(self)
    end,
    method a-field(self, l, name, ann): ann.visit(self) end
  }
dummy-loc-visitor =
  {
    method s-underscore(self, l): s-underscore(dummy-loc) end,
    method s-name(self, l, s): s-name(dummy-loc, s) end,
    method s-global(self, s): s-global(dummy-loc) end,
    method s-type-global(self, s): s-type-global(dummy-loc) end,
    method s-atom(self, base, serial): s-atom(dummy-loc, serial) end,
    method s-program(self, l, _provide, provided-types, imports, block):
      s-program(dummy-loc,
        _provide.visit(self),
        provided-types.visit(self),
        imports.map(_.visit(self)),
        block.visit(self))
    end,
    method s-include(self, l, mod): s-include(dummy-loc, mod.visit(self)) end,
    method s-import(self, l, file, name):
      s-import(dummy-loc, file.visit(self), name.visit(self))
    end,
    method s-import-types(self, l, file, name, types):
      s-import-types(dummy-loc,
        file.visit(self),
        name.visit(self),
        types.visit(self))
    end,
    method s-import-fields(self, l, fields, file):
      s-import-fields(dummy-loc, fields.map(_.visit(self)), file.visit(self))
    end,
    method s-import-complete(
        self,
        l,
        values,
        types,
        import-type,
        vals-name,
        types-name
      ):
      s-import-complete(dummy-loc,
        values.map(_.visit(self)),
        types.map(_.visit(self)),
        import-type.visit(self),
        vals-name.visit(self),
        types-name.visit(self))
    end,
    method s-provide(self, l, block):
      s-provide(dummy-loc, block.visit(self))
    end,
    method s-provide-complete(self, l, values, aliases, data-definitions):
      s-provide-complete(dummy-loc, values, aliases, data-definitions)
    end,
    method s-provide-all(self, l): s-provide-all(dummy-loc) end,
    method s-provide-none(self, l): s-provide-none(dummy-loc) end,
    method s-provide-types(self, l, ann):
      s-provide-types(dummy-loc, ann.map(_.visit(self)))
    end,
    method s-provide-types-all(self, l): s-provide-types-all(dummy-loc) end,
    method s-provide-types-none(self, l): s-provide-types-none(dummy-loc) end,
    method s-const-import(self, l, mod): s-const-import(dummy-loc, mod) end,
    method s-special-import(self, l, kind, args):
      s-special-import(dummy-loc, kind, args)
    end,
    method h-use-loc(self, l): h-use-loc(dummy-loc) end,
    method s-let-bind(self, l, b, value):
      s-let-bind(dummy-loc, b.visit(self), value.visit(self))
    end,
    method s-var-bind(self, l, b, value):
      s-var-bind(dummy-loc, b.visit(self), value.visit(self))
    end,
    method s-letrec-bind(self, l, b, value):
      s-letrec-bind(dummy-loc, b.visit(self), value.visit(self))
    end,
    method s-type-bind(self, l, name, params, ann):
      s-type-bind(dummy-loc,
        name.visit(self),
        params.map(_.visit(self)),
        ann.visit(self))
    end,
    method s-newtype-bind(self, l, name, namet):
      s-newtype-bind(dummy-loc, name.visit(self), namet.visit(self))
    end,
    method s-defined-value(self, name, value):
      s-defined-value(dummy-loc, value.visit(self))
    end,
    method s-defined-var(self, name, id):
      s-defined-var(dummy-loc, id.visit(self))
    end,
    method s-defined-type(self, name, typ):
      s-defined-type(dummy-loc, typ.visit(self))
    end,
    method s-module(
        self,
        l,
        answer,
        defined-values,
        defined-types,
        provided-values,
        provided-types,
        checks
      ):
      s-module(dummy-loc,
        answer.visit(self),
        defined-values.map(_.visit(self)),
        defined-types.map(_.visit(self)),
        provided-values.visit(self),
        provided-types.map(_.visit(self)),
        checks.visit(self))
    end,
    method s-template(self, l): s-template(dummy-loc) end,
    method s-type-let-expr(self, l, binds, body, blocky):
      s-type-let-expr(dummy-loc,
        binds.map(_.visit(self)),
        body.visit(self),
        blocky)
    end,
    method s-let-expr(self, l, binds, body, blocky):
      s-let-expr(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
    end,
    method s-letrec(self, l, binds, body, blocky):
      s-letrec(dummy-loc, binds.map(_.visit(self)), body.visit(self), blocky)
    end,
    method s-hint-exp(self, l, hints, exp):
      s-hint-exp(dummy-loc, hints.map(_.visit(self)), exp.visit(self))
    end,
    method s-instantiate(self, l, expr, params):
      s-instantiate(dummy-loc, expr.visit(self), params.map(_.visit(self)))
    end,
    method s-block(self, l, stmts):
      s-block(dummy-loc, stmts.map(_.visit(self)))
    end,
    method s-user-block(self, l, body):
      s-user-block(dummy-loc, body.visit(self))
    end,
    method s-fun(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-fun(dummy-loc,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-type(self, l, name, params, ann):
      s-type(dummy-loc,
        name.visit(self),
        params.map(_.visit(self)),
        ann.visit(self))
    end,
    method s-newtype(self, l, name, namet):
      s-newtype(dummy-loc, name.visit(self), namet.visit(self))
    end,
    method s-var(self, l, name, value):
      s-var(dummy-loc, name.visit(self), value.visit(self))
    end,
    method s-rec(self, l, name, value):
      s-rec(dummy-loc, name.visit(self), value.visit(self))
    end,
    method s-let(self, l, name, value, keyword-val):
      s-let(dummy-loc, name.visit(self), value.visit(self), keyword-val)
    end,
    method s-ref(self, l, ann):
      s-ref(dummy-loc, ann.and-then(_.visit(self)))
    end,
    method s-contract(self, l, name, ann):
      s-contract(dummy-loc, name.visit(self), ann.visit(self))
    end,
    method s-when(self, l, test, block, blocky):
      s-when(dummy-loc, test.visit(self), block.visit(self), blocky)
    end,
    method s-assign(self, l, id, value):
      s-assign(dummy-loc, id.visit(self), value.visit(self))
    end,
    method s-if-pipe(self, l, branches, blocky):
      s-if-pipe(dummy-loc, branches.map(_.visit(self)), blocky)
    end,
    method s-if-pipe-else(self, l, branches, _else, blocky):
      s-if-pipe-else(dummy-loc,
        branches.map(_.visit(self)),
        _else.visit(self),
        blocky)
    end,
    method s-if(self, l, branches, blocky):
      s-if(dummy-loc, branches.map(_.visit(self)), blocky)
    end,
    method s-if-else(self, l, branches, _else, blocky):
      s-if-else(dummy-loc,
        branches.map(_.visit(self)),
        _else.visit(self),
        blocky)
    end,
    method s-cases(self, l, typ, val, branches, blocky):
      s-cases(dummy-loc,
        typ.visit(self),
        val.visit(self),
        branches.map(_.visit(self)),
        blocky)
    end,
    method s-cases-else(self, l, typ, val, branches, _else, blocky):
      s-cases-else(dummy-loc,
        typ.visit(self),
        val.visit(self),
        branches.map(_.visit(self)),
        _else.visit(self),
        blocky)
    end,
    method s-op(self, l, op-l, op, left, right):
      s-op(dummy-loc, op-l, op, left.visit(self), right.visit(self))
    end,
    method s-check-test(self, l, op, refinement, left, right):
      s-check-test(dummy-loc,
        op.visit(self),
        refinement.and-then(_.visit(self)),
        left.visit(self),
        right.and-then(_.visit(self)))
    end,
    method s-check-expr(self, l, expr, ann):
      s-check-expr(dummy-loc, expr.visit(self), ann.visit(self))
    end,
    method s-paren(self, l, expr): s-paren(dummy-loc, expr.visit(self)) end,
    method s-lam(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-lam(dummy-loc,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-method(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-method(dummy-loc,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-extend(self, l, supe, fields):
      s-extend(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
    end,
    method s-update(self, l, supe, fields):
      s-update(dummy-loc, supe.visit(self), fields.map(_.visit(self)))
    end,
    method s-tuple(self, l, fields):
      s-tuple(dummy-loc, fields.map(_.visit(self)))
    end,
    method s-tuple-get(self, l, tup, index, index-loc):
      s-tuple-get(dummy-loc, tup.visit(self), index, index-loc)
    end,
    method s-obj(self, l, fields):
      s-obj(dummy-loc, fields.map(_.visit(self)))
    end,
    method s-array(self, l, values):
      s-array(dummy-loc, values.map(_.visit(self)))
    end,
    method s-construct(self, l, modifier, constructor, values):
      s-construct(dummy-loc,
        modifier.visit(self),
        constructor.visit(self),
        values.map(_.visit(self)))
    end,
    method s-app(self, l, _fun, args):
      s-app(dummy-loc, _fun.visit(self), args.map(_.visit(self)))
    end,
    method s-app-enriched(self, l, _fun, args, app-info):
      s-app-enriched(dummy-loc,
        _fun.visit(self),
        args.map(_.visit(self)),
        app-info)
    end,
    method s-prim-app(self, l, _fun, args):
      s-prim-app(dummy-loc, _fun, args.map(_.visit(self)))
    end,
    method s-prim-val(self, l, name): s-prim-val(dummy-loc, name) end,
    method s-id(self, l, id): s-id(dummy-loc, id.visit(self)) end,
    method s-id-var(self, l, id): s-id-var(dummy-loc, id.visit(self)) end,
    method s-id-letrec(self, l, id, safe):
      s-id-letrec(dummy-loc, id.visit(self), safe)
    end,
    method s-undefined(self, l): s-undefined(dummy-loc) end,
    method s-srcloc(self, l, loc): s-srcloc(dummy-loc, loc) end,
    method s-num(self, l, n): s-num(dummy-loc, n) end,
    method s-frac(self, l, num, den): s-frac(dummy-loc, num, den) end,
    method s-rfrac(self, l, num, den): s-rfrac(dummy-loc, num, den) end,
    method s-bool(self, l, b): s-bool(dummy-loc, b) end,
    method s-str(self, l, s): s-str(dummy-loc, s) end,
    method s-dot(self, l, obj, field):
      s-dot(dummy-loc, obj.visit(self), field)
    end,
    method s-get-bang(self, l, obj, field):
      s-get-bang(dummy-loc, obj.visit(self), field)
    end,
    method s-bracket(self, l, obj, key):
      s-bracket(dummy-loc, obj.visit(self), key.visit(self))
    end,
    method s-data(
        self,
        l,
        name,
        params,
        mixins,
        variants,
        shared-members,
        _check-loc,
        _check
      ):
      s-data(dummy-loc,
        name,
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        _check.and-then(_.visit(self)))
    end,
    method s-data-expr(
        self,
        l,
        name,
        namet,
        params,
        mixins,
        variants,
        shared-members,
        _check-loc,
        _check
      ):
      s-data-expr(dummy-loc,
        name,
        namet.visit(self),
        params.map(_.visit(self)),
        mixins.map(_.visit(self)),
        variants.map(_.visit(self)),
        shared-members.map(_.visit(self)),
        _check-loc,
        _check.and-then(_.visit(self)))
    end,
    method s-for(self, l, iterator, bindings, ann, body, blocky):
      s-for(dummy-loc,
        iterator.visit(self),
        bindings.map(_.visit(self)),
        ann.visit(self),
        body.visit(self),
        blocky)
    end,
    method s-check(self, l, name, body, keyword-check):
      s-check(dummy-loc, name, body.visit(self), keyword-check)
    end,
    method s-reactor(self, l, fields):
      s-reactor(dummy-loc, fields.map(_.visit(self)))
    end,
    method s-table-extend(self, l, column-binds, extensions):
      s-table-extend(dummy-loc,
        column-binds.visit(self),
        extensions.map(_.visit(self)))
    end,
    method s-table-update(self, l, column-binds, updates):
      s-table-update(dummy-loc,
        column-binds.visit(self),
        updates.map(_.visit(self)))
    end,
    method s-table-select(self, l, columns, table):
      s-table-select(dummy-loc, columns.map(_.visit(self)), table.visit(self))
    end,
    method s-table-order(self, l, table, ordering):
      s-table-order(dummy-loc, table.visit(self), ordering.map(_.visit(self)))
    end,
    method s-table-filter(self, l, column-binds, predicate):
      s-table-filter(dummy-loc, column-binds.visit(self), predicate.visit(self))
    end,
    method s-table-extract(self, l, column, table):
      s-table-extract(dummy-loc, column.visit(self), table.visit(self))
    end,
    method s-table(self, l, headers, rows):
      s-table(dummy-loc, headers.map(_.visit(self)), rows.map(_.visit(self)))
    end,
    method s-load-table(self, l, headers, spec):
      s-load-table(dummy-loc,
        headers.map(_.visit(self)),
        spec.map(_.visit(self)))
    end,
    method s-spy-block(self, l, message, contents):
      s-spy-block(dummy-loc,
        message.and-then(_.visit(self)),
        contents.map(_.visit(self)))
    end,
    method s-table-row(self, l, elems):
      s-table-row(dummy-loc, elems.map(_.visit(self)))
    end,
    method s-spy-name(self, l, name): s-spy-name(dummy-loc, name) end,
    method s-spy-expr(self, l, name, value):
      s-spy-expr(dummy-loc, name, value.visit(self))
    end,
    method s-construct-normal(self): s-construct-normal end,
    method s-construct-lazy(self): s-construct-lazy end,
    method s-bind(self, l, shadows, id, ann):
      s-bind(dummy-loc, shadows, id.visit(self), ann.visit(self))
    end,
    method s-tuple-bind(self, l, fields, as-name):
      s-tuple-bind(dummy-loc,
        fields.map(_.visit(self)),
        as-name.and-then(_.visit(self)))
    end,
    method s-data-field(self, l, name, value):
      s-data-field(dummy-loc, name, value.visit(self))
    end,
    method s-mutable-field(self, l, name, ann, value):
      s-mutable-field(dummy-loc, name, ann.visit(self), value.visit(self))
    end,
    method s-method-field(
        self,
        l,
        name,
        params,
        args,
        ann,
        doc,
        body,
        _check-loc,
        _check,
        blocky
      ):
      s-method-field(dummy-loc,
        name,
        params.map(_.visit(self)),
        args.map(_.visit(self)),
        ann.visit(self),
        doc,
        body.visit(self),
        _check-loc,
        _check.and-then(_.visit(self)),
        blocky)
    end,
    method s-field-name(self, l, name, ann):
      s-field-name(dummy-loc, name, ann.visit(self))
    end,
    method s-for-bind(self, l, bind, value):
      s-for-bind(dummy-loc, bind.visit(self), value.visit(self))
    end,
    method s-column-binds(self, l, binds, table):
      s-column-binds(dummy-loc, binds.map(_.visit(self)), table.visit(self))
    end,
    method ASCENDING(self): ASCENDING end,
    method DESCENDING(self): DESCENDING end,
    method s-column-sort(self, l, column, direction):
      s-column-sort(dummy-loc, column.visit(self), direction.visit(self))
    end,
    method s-table-extend-field(self, l, name, value, ann):
      s-table-extend-field(dummy-loc, name, value.visit(self), ann.visit(self))
    end,
    method s-table-extend-reducer(self, l, name, reducer, col, ann):
      s-table-extend-reducer(dummy-loc,
        name,
        reducer.visit(self),
        col.visit(self),
        ann.visit(self))
    end,
    method s-sanitize(self, l, name, sanitizer):
      s-sanitize(dummy-loc, name.visit(self), sanitizer.visit(self))
    end,
    method s-table-src(self, l, src):
      s-table-src(dummy-loc, src.visit(self))
    end,
    method s-normal(self): s-normal end,
    method s-mutable(self): s-mutable end,
    method s-variant-member(self, l, member-type, bind):
      s-variant-member(dummy-loc, member-type.visit(self), bind.visit(self))
    end,
    method s-variant(self, l, constr-loc, name, members, with-members):
      s-variant(dummy-loc,
        constr-loc,
        name,
        members.map(_.visit(self)),
        with-members.map(_.visit(self)))
    end,
    method s-singleton-variant(self, l, name, with-members):
      s-singleton-variant(dummy-loc, name, with-members.map(_.visit(self)))
    end,
    method s-if-branch(self, l, test, body):
      s-if-branch(dummy-loc, test.visit(self), body.visit(self))
    end,
    method s-if-pipe-branch(self, l, test, body):
      s-if-pipe-branch(dummy-loc, test.visit(self), body.visit(self))
    end,
    method s-cases-bind(self, l, field-type, bind):
      s-cases-bind(dummy-loc, field-type, bind.visit(self))
    end,
    method s-cases-branch(self, l, pat-loc, name, args, body):
      s-cases-branch(dummy-loc,
        pat-loc,
        name,
        args.map(_.visit(self)),
        body.visit(self))
    end,
    method s-singleton-cases-branch(self, l, pat-loc, name, body):
      s-singleton-cases-branch(dummy-loc, pat-loc, name, body.visit(self))
    end,
    method s-op-is(self, l): s-op-is(dummy-loc) end,
    method s-op-is-roughly(self, l): s-op-is-roughly(dummy-loc) end,
    method s-op-is-op(self, l, op): s-op-is-op(dummy-loc, op) end,
    method s-op-is-not(self, l): s-op-is-not(dummy-loc) end,
    method s-op-is-not-op(self, l, op): s-op-is-not-op(dummy-loc, op) end,
    method s-op-satisfies(self, l): s-op-satisfies(dummy-loc) end,
    method s-op-satisfies-not(self, l): s-op-satisfies-not(dummy-loc) end,
    method s-op-raises(self, l): s-op-raises(dummy-loc) end,
    method s-op-raises-other(self, l): s-op-raises-other(dummy-loc) end,
    method s-op-raises-not(self, l): s-op-raises-not(dummy-loc) end,
    method s-op-raises-satisfies(self, l): s-op-raises-satisfies(dummy-loc) end,
    method s-op-raises-violates(self, l): s-op-raises-violates(dummy-loc) end,
    method a-blank(self): a-blank end,
    method a-any(self, l): a-any(dummy-loc) end,
    method a-name(self, l, id): a-name(dummy-loc, id.visit(self)) end,
    method a-type-var(self, l, id): a-type-var(dummy-loc, id.visit(self)) end,
    method a-arrow(self, l, args, ret, use-parens):
      a-arrow(dummy-loc, args.map(_.visit(self)), ret.visit(self), use-parens)
    end,
    method a-arrow-argnames(self, l, args, ret, use-parens):
      a-arrow-argnames(dummy-loc,
        args.map(_.visit(self)),
        ret.visit(self),
        use-parens)
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
      a-dot(dummy-loc, obj.visit(self), field)
    end,
    method a-checked(self, checked, residual):
      a-checked(dummy-loc, residual.visit(self))
    end,
    method a-field(self, l, name, ann):
      a-field(dummy-loc, name, ann.visit(self))
    end
  }
