provide *
provide-types *
# import arrays as A
# import lists as L

data RuntimeError:
  | message-exception(message :: String) with:
    tostring(self):
      self.message
    end
  | no-branches-matched(loc, expression :: String) with:
    tostring(self):
      "No branches matched in the `" + self.expression + "` expression at " + self.loc.format(true)
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
  | non-boolean-condition(loc, typ, value) with:
    tostring(self):
      "Error: expected a Boolean for the condition of a " + self.typ + " at " + self.loc.format(true) + ", but got " + torepr(self.value)
    end
  | non-boolean-op(loc, position, typ, value) with:
    tostring(self):
      "Error: expected a Boolean for the " + self.position + " argument to " + self.typ + " at " + self.loc.format(true) + ", but got " + torepr(self.value)
    end
  | generic-type-mismatch(val, typ :: String) with:
    tostring(self):
      "Error: expected " + self.typ + ", but got " + torepr(self.val)
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
  | cases-arity-mismatch(branch-loc, num-args, actual-fields) with:
    tostring(self):
      "Error: The cases branch at " + self.branch-loc.format(true) + " expects " + tostring(self.num-args) + " arguments, but the actual value has " + tostring(self.actual-fields.length()) + " fields"
    end
  | arity-mismatch(fun-loc, expected-arity, args) with:
    tostring(self):
      "Error: The function at " + self.fun-loc.format(true) + " expects " + tostring(self.expected-arity) + " arguments, but got " + tostring(self.args.length())
    end
  | non-function-app(loc, non-fun-val) with:
    tostring(self):
      "Error: Expected a function at " + self.loc.format(true) + ", but got " + torepr(self.non-fun-val)
    end
  | bad-app(loc, fun-name :: String, message :: String, arg-position :: Number, arg-val)
  | uninitialized-id(loc, name :: String) with:
    tostring(self):
      "Error: The identifier " + self.name + " was used at " + self.loc.format(true) + " before it was defined."
    end
  | module-load-failure(names) with: # names is List<String>
    tostring(self):
      "Error: The following modules failed to load: " + torepr(self.names)
    end
  | invalid-array-index(method-name :: String, array, index :: Number, reason :: String) with: # array is Array
    tostring(self):
      "Error: Bad array index " + tostring(self.index) + " in " + self.method-name + ": " + self.reason
    end
  | user-break
end

data ParseError:
  | parse-error-next-token(loc, next-token :: String) with:
    tostring(self):
      "parse error around " + self.loc.format(true) + ", next token was " + self.next-token
    end
  | parse-error-eof(loc) with:
    tostring(self):
      "parse error at end of file at " + self.loc.format(true)
    end
  | empty-block(loc) with:
    tostring(self):
      "Empty block at " + self.loc.format(true)
    end
  | bad-block-stmt(loc) with:
    tostring(self):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | bad-check-block-stmt(loc) with:
    tostring(self):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | fun-missing-colon(loc) with:
    tostring(self): "fun-missing-colon: " + self.loc.format(true) end
  | fun-missing-end(loc) with:
    tostring(self): "fun-missing-end: " + self.loc.format(true) end
  | args-missing-comma(loc) with:
    tostring(self): "args-missing-comma: " + self.loc.format(true) end
  | app-args-missing-comma(loc) with:
    tostring(self): "app-args-missing-comma: " + self.loc.format(true) end
  | missing-end(loc)
  | missing-comma(loc)
end
