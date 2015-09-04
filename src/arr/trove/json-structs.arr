provide *
provide-types *

import string-dict as SD

data JSON:
  | j-obj(dict :: SD.StringDict<JSON>) with:
    native(self):
      d = self.dict
      ret = [SD.mutable-string-dict:]
      for map(s from d.keys().to-list()):
        ret.set-now(s, d.get-value(s).native())
      end
      ret.freeze()
    end,
    serialize(self):
      d = self.dict
      l = for map(s from d.keys().to-list()):
        '"' + s + '": ' + d.get-value(s).serialize()
      end
      "{" + l.join-str(", ") + "}"
    end
  | j-arr(l :: List) with:
    native(self):
      self.l.map(lam(x): x.native() end)
    end,
    serialize(self):
      "[" + self.l.map(lam(x): x.serialize() end).join-str(", ") + "]"
    end
  | j-num(n :: Number) with:
    native(self):
      self.n
    end,
    serialize(self):
      tostring(self.n)
    end
  | j-str(s :: String) with:
    native(self):
      self.s
    end,
    serialize(self):
      '"' + self.s + '"'
    end
  | j-bool(b :: Boolean) with:
    native(self):
      self.b
    end,
    serialize(self):
      if self.b: "true" else: "false" end
    end
  | j-null with:
    native(self):
      nothing
    end,
    serialize(self):
      "null"
    end
end
