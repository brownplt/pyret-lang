provide *
provide-types *

import global as _
import base as _
import string-dict as SD
import lists as lists
import arrays as arrays

type List = lists.List
link = lists.link
empty = lists.empty
is-empty = lists.is-empty
is-link = lists.is-link
map = lists.map

is-array = arrays.is-array

data JSON:
  | j-obj(dict :: SD.StringDict<JSON>) with:
    method native(self) block:
      d = self.dict
      ret = [SD.mutable-string-dict:]
      for SD.each-key(s from d):
        ret.set-now(s, d.get-value(s).native())
      end
      ret.freeze()
    end,
    method serialize(self):
      d = self.dict
      l = for SD.map-keys(s from d):
        '"' + s + '": ' + d.get-value(s).serialize()
      end
      "{" + l.join-str(", ") + "}"
    end
  | j-arr(l :: List<JSON>) with:
    method native(self):
      self.l.map(lam(x): x.native() end)
    end,
    method serialize(self):
      "[" + self.l.map(lam(x): x.serialize() end).join-str(", ") + "]"
    end
  | j-num(n :: Number) with:
    method native(self):
      self.n
    end,
    method serialize(self):
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
    method native(self):
      self.s
    end,
    method serialize(self):
      torepr(self.s)
    end
  | j-bool(b :: Boolean) with:
    method native(self):
      self.b
    end,
    method serialize(self):
      if self.b: "true" else: "false" end
    end
  | j-null with:
    method native(self):
      nothing
    end,
    method serialize(self):
      "null"
    end
end

fun tojson(v :: Any) -> JSON:
  if is-number(v) block:
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
    for SD.each-key(s from v):
      ret.set-now(s, tojson(v.get-value(s)))
    end
    j-obj(ret.freeze())
  else if SD.is-mutable-string-dict(v):
    ret = [SD.mutable-string-dict:]
    for SD.each-key-now(s from v):
      ret.set-now(s, tojson(v.get-value-now(s)))
    end
    j-obj(ret.freeze())
  else:
    raise("Value " + torepr(v) + " cannot be converted to a JSON expression.")
  end
end
