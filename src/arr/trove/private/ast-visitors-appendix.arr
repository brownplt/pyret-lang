shadow dummy-loc-visitor = dummy-loc-visitor.{
  method s-lam(self, l, name, params, args, ann, doc, body, _check-loc, _check, blocky):
    s-lam(dummy-loc,
      '',
      params.map(_.visit(self)),
      args.map(_.visit(self)),
      ann.visit(self),
      doc,
      body.visit(self),
      _check-loc,
      _check.and-then(_.visit(self)),
      blocky)
  end,
}
