#lang pyret

import ast as A

check:
  all-ids-are-y = A.default-map-visitor.{
      s_id(self, l, x): A.s_id(l, "y") end,
      s_bind(self, l, shadows, name, ann): A.s_bind(l, shadows, "y", ann) end
    }
  p = fun(s): A.surface-parse(s, "test").block end
  equiv = fun(e): A.equiv-ast(_, e) end

  p("for map(z from [f,g,h]): l(o) end").visit(all-ids-are-y)
    satisfies equiv(p("for y(y from [y,y,y]): y(y) end"))
end

