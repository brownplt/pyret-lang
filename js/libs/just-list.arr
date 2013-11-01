#lang pyret

fun Eq():
  b = brander()
    {
            extend: fun(obj):
                    obj.{eq(self, other): b.test(self) and b.test(other) end}
                          end,
                              brand: fun(obj): b.brand(obj) end
                                }
      end
builtins = {
    Eq : Eq,
    data-to-repr: data-to-repr, 
    }

data List:
  | empty with:
    length(self): 0 end
  | link(first, rest :: List) with:
    length(self): 1 + self.rest.length() end
end

list = {
  link: link,
  empty: empty
}

checker = {}
