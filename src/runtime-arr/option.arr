#
# Needs to be built with "--compile-mode builtin-stage-1" to avoid emitting code that depends
#  on the builtin global module.
#

provide:
  *,
  type *,
  data *
end

import primitive-types as _

data Option<a>:
  | some(value :: a)
  | none
sharing:
  method or-else(self :: Option<a>, v:: a) -> a:
    cases(Option) self:
      | some(value) => value
      | none => v
    end
  end,

  method and-then<b>(self :: Option<a>, f :: (a -> b)) -> Option<b>:
    cases(Option) self:
      | some(value) => some(f(value))
      | none => none
    end
  end,

where:
  none.or-else(1) is 1
  none.and-then(lam(x): some(x + 2) end) is none

  some(5).or-else(0) is 5
  some(5).and-then(lam(x): some(x + 2) end) is some(7)
end
