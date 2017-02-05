# ast.arr equivalent for lambda calculus
provide *
import pprint as PP
import lists as L
import srcloc as S
import global as _
import base as _

type Loc = S.Srcloc

dummy-loc = S.builtin("dummy location")

INDENT = 2

str-lambda = PP.str("λ")

data LCVariable:
  | lc-var-inner(loc :: Loc, name :: String) with:
    method to-compiled-source(self): PP.str(self.to-compiled()) end,
    method to-compiled(self): self.name end,
    method tosource(self): PP.str(self.name) end,
    method tosourcestring(self): self.name end,
    method toname(self): self.name end,
    method key(self): "lc-var-inner#" + self.name end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

data LCTerm:
  | lc-var(loc :: Loc, name :: LCVariable) with:
    method label(self): "lc-var" end,
    method tosource(self): self.name.tosource() end
  | lc-abs(loc :: Loc, param :: List<LCVariable>, body :: LCTerm) with:
    method label(self): "lc-abs" end,
    method tosource(self):
      pp-args = PP.nest(INDENT,
        PP.surround-separate(INDENT, 0, PP.lparen + PP.rparen,
          PP.lparen, PP.sbreak(1), PP.rparen, self.args.map(_.tosource())))
      PP.surround(INDENT, 0, PP.lparen,
        PP.separate(PP.sbreak(1), [list: str-lambda, pp-args, self.body.tosource()]), PP.rparen)
    end
  | lc-app(loc :: Loc, func :: LCTerm, args :: List<LCTerm>) with:
    method label(self): "lc-app" end,
    method tosource(self):
      func = self.func.tosource()
      args = self.args.map(_.tosource())
      PP.surround(INDENT, 0, PP.lparen,
        PP.separate(PP.sbreak(1), link(func, args)), PP.rparen)
    end
sharing:
  method visit(self, visitor):
    self._match(visitor, lam(): raise("No visitor field for " + self.label()) end)
  end
end

fun bound-vars(term) -> List<LCVariable>:
  cases(LCTerm) term:
    | lc-var(_, _) => empty
    | lc-abs(_, params, body) =>
      params.map(bound-vars).foldl(_ + _, bound-vars(body))
    | lc-app(_, func, args) =>
      args.map(bound-vars).foldl(_ + _, bound-vars(func))
  end
end

default-map-visitor = {
  method lc-var-inner(self, l, name):
    lc-var-inner(l, name)
  end,

  method lc-var(self, l, inner):
    lc-var(l, inner.visit(self))
  end,

  method lc-abs(self, l, params, body):
    lc-abs(l, params.map(_.visit(self)), body.visit(self))
  end,

  method lc-app(self, l, fst, args):
    lc-app(l, fst.visit(self), args.map(_.visit(self)))
  end
}

default-iter-visitor = {
  method lc-var-inner(self, l, name):
    true
  end,

  method lc-var(self, l, inner):
    inner.visit(self)
  end,

  method lc-abs(self, l, params, body):
    L.each(_.visit(self), params) and body.visit(self)
  end,

  method lc-app(self, l, fst, args):
    fst.visit(self) and L.each(_.visit(self), args)
  end
}

dummy-loc-visitor = {
  method lc-var-inner(self, l, name):
    lc-var-inner(dummy-loc, name)
  end,

  method lc-var(self, l, inner):
    lc-var(dummy-loc, inner.visit(self))
  end,

  method lc-abs(self, l, params, body):
    lc-abs(dummy-loc, params.map(_.visit(self)), body.visit(self))
  end,

  method lc-app(self, l, fst, args):
    lc-app(dummy-loc, fst.visit(self), args.map(_.visit(self)))
  end
}