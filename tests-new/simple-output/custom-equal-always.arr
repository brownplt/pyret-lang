# pass
import global as G
import list as L

# TODO(alex): Due to type checker limitations, _equality cannot be directly 
#   implemented by variants as with-members b/c the type checker does not have
#   access to common fields nor does type-refinement on 'other' work
#
#   with:
#     # Foo annotation is necessary (cannot infer)
#     method _equals(self, other :: Foo%(is-bar), recursive):
#       # 'my-field' not detected on 'other'
#       if self.my-field > other.my-field:
#       G.Equal
#       else:
#         G.NotEqual("self.my-field <= other.my-field", self, other)
#       end
#     end
data Foo:
  | bar(my-field :: Number) 
sharing: 
  method _equals(self, other :: Foo, recursive):
    # NOTE(alex): This is a BAD equality function
    if self.my-field > other.my-field:
      G.Equal
    else:
      G.NotEqual("self.my-field <= other.my-field", self, other)
    end
  end
end

f1 = bar(0)
f2 = bar(1000)
f3 = bar(-1000)

r1 = G.not(f1 == f2)
r2 = f1 == f3

result = r1 and r2

if result:
  G.console-log("pass")
else:
  G.console-log([L.list: r1, r2])
end
