obj-checked :: { x :: Number, m :: (Number -> Number) } =
  { x: 1,
    m(self, n):
      self.x + n
    end }

obj-synth =
  { x: 2,
    m(self, n :: Number):
      self.x + n
    end }

obj-sub :: { m :: (Number -> Number) } =
  { x: 1,
    m(self, n :: Number):
      self.x + n
    end }

x :: Number = obj-checked.m(2)
f :: (Number -> Number) = obj-synth.m
