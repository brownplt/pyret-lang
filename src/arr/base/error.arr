provide *

data RuntimeError:
  | message-exception(message :: String) with:
    tostring(self):
      self.message
    end
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
  | generic-type-mismatch(val, type :: String) with:
    tostring(self):
      "error: expected " + self.type + ", but got " + torepr(self.val)
    end
  | outside-numeric-range(val, low, high) with:
    tostring(self):
      "error: expected a number between " + torepr(self.low) + " and " + torepr(self.high) + ", but got " + torepr(self.val)
    end
  | plus-error(val1, val2) with:
    tostring(self):
      "Error: Invalid use of +.  Either both arguments must be strings, both must be numbers, or the left operand must have a _plus method. Got: \n" + torepr(self.val1) + "\nand \n" + torepr(self.val2)
    end
  | numeric-binop-error(val1, val2, opname, methodname) with:
    tostring(self):
      "Error: Invalid use of " + self.opname + ".  Either both arguments must be numbers, or the left operand must have a " + self.methodname + " method.  Got: \n" + torepr(self.val1) + "\nand \n" + torepr(self.val2)
    end
  | arity-mismatch(loc, fun-loc, fun-val, args)
  | bad-app(loc, fun-name :: String, message :: String, arg-position :: Number, arg-val)
  | user-break
end

