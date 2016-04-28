data Foo:
  | foo1(x :: Number) with:
    f(self, y :: Number):
      self.x + y
    end,
    g(self):
      cases(Foo) self:
        | foo1(_) => 1
        | foo2 => 2
      end
    end,
    lam-thing: lam(x :: Number): x + 1 end
  | foo2 with:
    g(self):
      3
    end
sharing:
  h(self, y :: Number):
    self.g() + y
  end
end

x :: Number = foo1(1).h(3)
y :: Number = foo2.g()
z :: Number = lam(thing :: Foo): thing.h(3) end(foo2)


data Maybe<A>:
  | some-thing(value :: A) with:
    bind<B>(self, f :: (A -> Maybe<B>)):
      f(self.value)
    end
  | not-a-thing with:
    bind<B>(self, f :: (A -> Maybe<B>)):
      not-a-thing
    end
end

x2 :: Maybe<Number> = not-a-thing.bind(lam(val :: Number): some-thing(val) end)
y2 :: Maybe<Number> = some-thing(1).bind(lam(val :: Number): some-thing(val) end)
