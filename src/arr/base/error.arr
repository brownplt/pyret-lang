provide *

error-brander = brander()

data RuntimeError:
  | internal-error(message, info-args) with:
    tostring(self):
      "internal: " + self.message + "; relevant arguments: " + torepr(self.info-args)
    end
  | field-not-found(obj, field :: String) with:
    tostring(self):
      "error: field " + self.field + " not found on " + torepr(self.obj)
    end
  | lookup-non-object(obj, field :: String) with:
    tostring(self):
      "error: tried to look up field " + self.field + " on " + torepr(self.obj) + ", but it does not have fields"
    end
  | type-mismatch(val, type :: String) with:
    tostring(self):
      "error: expected " + self.type + ", but got " + torepr(self.val)
    end
  | arity-mismatch(loc, fun-loc, fun-val, args)
  | bad-app(loc, fun-name :: String, message :: String, arg-position :: Number, arg-val)
  | message-exception(message :: String)
  | user-break
end
