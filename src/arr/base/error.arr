provide *

data RuntimeError:
  | message-exception(message :: String) with:
    tostring(self):
      self.message
    end
  | internal-error(message, info-args) with:
    tostring(self):
      "Internal error: " + self.message + "; relevant arguments: " + torepr(self.info-args)
    end
  | field-not-found(loc, obj, field :: String) with:
    tostring(self):
      "Error at " + self.loc.format(true) + ": field " + self.field + " not found on " + torepr(self.obj)
    end
  | lookup-non-object(loc, non-obj, field :: String) with:
    tostring(self):
      "Error at " + self.loc.format(true) + ": tried to look up field " + self.field + " on " + torepr(self.non-obj) + ", but it does not have fields"
    end
  | non-boolean-condition(loc, type, value) with:
    tostring(self):
      "Error: expected a Boolean for the condition of a " + self.type + " at " + self.loc.format(true) + ", but got " + torepr(self.value)
    end
  | non-boolean-op(loc, position, type, value) with:
    tostring(self):
      "Error: expected a Boolean for the " + self.position + " argument to " + self.type + " at " + self.loc.format(true) + ", but got " + torepr(self.value)
    end
  | generic-type-mismatch(val, type :: String) with:
    tostring(self):
      "Error: expected " + self.type + ", but got " + torepr(self.val)
    end
  | outside-numeric-range(val, low, high) with:
    tostring(self):
      "Error: expected a number between " + torepr(self.low) + " and " + torepr(self.high) + ", but got " + torepr(self.val)
    end
  | plus-error(val1, val2) with:
    tostring(self):
      "Error: Invalid use of +.  Either both arguments must be strings, both must be numbers, or the left operand must have a _plus method. Got: \n" + torepr(self.val1) + "\nand \n" + torepr(self.val2)
    end
  | numeric-binop-error(val1, val2, opname, methodname) with:
    tostring(self):
      "Error: Invalid use of " + self.opname + ".  Either both arguments must be numbers, or the left operand must have a " + self.methodname + " method.  Got: \n" + torepr(self.val1) + "\nand \n" + torepr(self.val2)
    end
  | arity-mismatch(fun-loc, expected-arity, args) with:
    tostring(self):
      "Error: The function at " + self.fun-loc.format(true) + " expects " + tostring(self.expected-arity) + " arguments, but got " + tostring(self.args.length())
    end
  | non-function-app(loc, non-fun-val, args) with:
    tostring(self):
      "Error: Expected a function at " + self.loc.format(true) + ", but got " + torepr(self.non-fun-val)
    end
  | bad-app(loc, fun-name :: String, message :: String, arg-position :: Number, arg-val)
  | uninitialized-id(loc, name :: String) with:
    tostring(self):
      "Error: The identifier " + self.name + " was used at " + self.loc.format(true) + " before it was defined."
    end
  | module-load-failure(names :: List<String>) with:
    tostring(self):
      "Error: The following modules failed to load: " + torepr(self.names)
    end
  | user-break
end

data ParseError:
  | parse-error-next-token(loc :: Loc, next-token :: String) with:
    tostring(self):
      "parse error around " + self.loc.format(true) + ", next token was " + self.next-token
    end
end
