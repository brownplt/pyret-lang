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
  | j-arr(l :: List<JSON>) with:
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
      # This feels like a pretty big hack.  All I want is to convert a
      # roughnum (or a rational) as a JavaScript number, which just
      # means cutting off the initial "~".
      if num-is-roughnum(self.n) or num-is-rational(self.n):
        s = num-to-string(num-to-roughnum(self.n))
        string-substring(s, 1, string-length(s))
      else:
        num-to-string(self.n)
      end
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

fun tojson(v :: Any) -> JSON:
  if is-number(v):
    if num-is-fixnum(v) or num-is-roughnum(v):
      j-num(v)
    else:
      raise("Number " + v + " cannot be converted to a JavaScript number.")
    end
  else if is-string(v):
    j-str(v)
  else if is-boolean(v):
    j-bool(v)
  else if is-nothing(v):
    j-null
  else if is-link(v) or is-empty(v):
    j-arr(v.map(lam(x): tojson(x) end))
  else if is-array(v):
    j-arr(v.to-list-now().map(lam(x): tojson(x) end))
  else if is-raw-array(v):
    j-arr(raw-array-to-list(v).map(lam(x): tojson(x) end))
  else if SD.is-string-dict(v):
    ret = [SD.mutable-string-dict:]
    for map(s from v.keys().to-list()):
      ret.set-now(s, tojson(v.get-value(s)))
    end
    j-obj(ret.freeze())
  else if SD.is-mutable-string-dict(v):
    ret = [SD.mutable-string-dict:]
    for map(s from v.keys-now().to-list()):
      ret.set-now(s, tojson(v.get-value-now(s)))
    end
    j-obj(ret.freeze())
  else:
    raise("Value " + torepr(v) + " cannot be converted to a JSON expression.")
  end
end
