provide *
provide-types *
# import arrays as A
# import lists as L

import error-display as ED

fun draw-and-highlight(l):
  ED.loc-display(l, "error-highlight", ED.loc(l))
end

data RuntimeError:
  | message-exception(message :: String) with:
    _tostring(self, shadow tostring):
      self.message
    end
  | no-cases-matched(loc, val) with:
    _tostring(self, shadow tostring):
      "At " + self.loc.format(true) + ", no branches matched in the cases expression for value\n" + torepr(self.val)
    end
  | no-branches-matched(loc, expression :: String) with:
    _tostring(self, shadow tostring):
      "No branches matched in the `" + self.expression + "` expression at " + self.loc.format(true)
    end
  | internal-error(message, info-args) with:
    _tostring(self, shadow tostring):
      "Internal error: " + self.message + "; relevant arguments: " + torepr(self.info-args)
    end
  | field-not-found(loc, obj, field :: String) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Field"), ED.code(ED.text(self.field)), ED.text("not found in the lookup expression at"),
          draw-and-highlight(self.loc)],
        [ED.para: ED.text("The object was:")],
        ED.embed(self.obj)]
    end
  | lookup-non-object(loc, non-obj, field :: String) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Tried to look up field"), ED.code(ED.text(self.field)),
          ED.text("on a non-object in the lookup expression at"),
          draw-and-highlight(self.loc)],
        [ED.para: ED.text("The non-object was:")],
        ED.embed(self.non-obj)]
    end
  | extend-non-object(loc, non-obj) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Tried to extend a non-object in the expression at"),
          draw-and-highlight(self.loc)],
        [ED.para: ED.text("The non-object was:")],
        ED.embed(self.non-obj)]
    end
  | non-boolean-condition(loc, typ, value) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the test in the"), ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | non-boolean-op(loc, position, typ, value) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected"), ED.code(ED.text("true")), ED.text("or"), ED.code(ED.text("false")),
          ED.text("for the"), ED.text(self.position), ED.text("argument in the"),
          ED.text(self.typ), ED.text("expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.value)]
    end
  | generic-type-mismatch(val, typ :: String) with:
    _tostring(self, shadow tostring):
      "Error: expected " + self.typ + ", but got " + torepr(self.val)
    end
  | outside-numeric-range(val, low, high) with:
    _tostring(self, shadow tostring):
      "Error: expected a number between " + torepr(self.low) + " and " + torepr(self.high) + ", but got " + torepr(self.val)
    end
  | plus-error(val1, val2) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Invalid use of"), ED.code(ED.text("+")), ED.text("for these values:")],
        [ED.para: ED.embed(self.val1)],
        [ED.para: ED.embed(self.val2)],
        ED.text("Plus takes one of:"),
        [ED.bulleted:
          ED.text("Two numbers"),
          ED.text("Two strings"),
          [ED.para: ED.text("A left-hand operand that has a"), ED.code(ED.text("_plus")), ED.text("method")]]]
    end
  | numeric-binop-error(val1, val2, opname, methodname) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Invalid use of"), ED.code(ED.text(self.opname)), ED.text("for these values:")],
        [ED.para: ED.embed(self.val1)],
        [ED.para: ED.embed(self.val2)],
        ED.text("Either:"),
        [ED.bulleted:
          ED.text("Both arguments must be numbers, or"),
          [ED.para:
            ED.text("The left operand must have a"), ED.code(ED.text(self.methodname)), ED.text("method")]]]
    end
  | cases-arity-mismatch(branch-loc, num-args, actual-arity) with:
    render-reason(self):
      [ED.error:
        if self.num-args < self.actual-arity:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("expects only"), ED.embed(self.num-args),
            if self.num-args == 1: ED.text("argument,") else: ED.text("arguments,") end,
            ED.text("but the actual value has"), ED.embed(self.actual-arity),
            if self.actual-arity == 1: ED.text("field") else: ED.text("fields") end]
        else:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("expects"), ED.embed(self.num-args),
            if self.num-args == 1: ED.text("argument,") else: ED.text("arguments,") end,
            ED.text("but the actual value has only"), ED.embed(self.actual-arity),
            if self.actual-arity == 1: ED.text("field") else: ED.text("fields") end]
        end]
    end
  | cases-singleton-mismatch(branch-loc, should-be-singleton :: Boolean) with:
    render-reason(self):
      if self.should-be-singleton:
        [ED.error:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("has an argument list, but the variant is a singleton.")]]
      else:
        [ED.error:
          [ED.para:
            ED.text("The cases branch at"), draw-and-highlight(self.branch-loc),
            ED.text("doesn't have an argument list, but the variant is not a singleton.")]]
      end
    end
  | arity-mismatch(fun-loc, expected-arity, args) with:
    _tostring(self, shadow tostring):
      "Error: The function at " + self.fun-loc.format(true) + " expects " + tostring(self.expected-arity) + " arguments, but got " + tostring(self.args.length())
    end
  | non-function-app(loc, non-fun-val) with:
    render-reason(self):
      [ED.error:
        [ED.para:
          ED.text("Expected a function in the application expression at"),
          draw-and-highlight(self.loc), ED.text(" but got:")],
        ED.embed(self.non-fun-val)]
    end
  | bad-app(loc, fun-name :: String, message :: String, arg-position :: Number, arg-val)
  | uninitialized-id(loc, name :: String) with:
    _tostring(self, shadow tostring):
      "Error: The identifier " + self.name + " was used at " + self.loc.format(true) + " before it was defined."
    end
  | module-load-failure(names) with: # names is List<String>
    render-reason(self):
      [ED.error:
        [ED.para:
          if self.names.length() == 1: ED.text("The following module failed to load:")
          else:                        ED.text("The following modules failed to load:")
          end],
        ED.h-sequence(self.names.map(ED.text), ", ")]
    end
  | invalid-array-index(method-name :: String, array, index :: Number, reason :: String) with: # array is Array
    _tostring(self, shadow tostring):
      "Error: Bad array index " + tostring(self.index) + " in " + self.method-name + ": " + self.reason
    end
  | equality-failure(reason :: String, value1, value2) with:
    _tostring(self, shadow tostring):
      "Error: Attempted to compare incomparable values " + torepr(self.value1) +
        " and " + torepr(self.value2) + "; " + self.reason
    end

  | user-break with:
    render-reason(self):
      [ED.error: ED.text("Program stopped by user")]
    end
sharing:
  render-reason(self):
    ED.text(self._tostring(tostring))
  end
end

data ParseError:
  | parse-error-next-token(loc, next-token :: String) with:
    render-reason(self):
      missing =
        [ED.error:
          [ED.para: ED.text("The program is missing something")],
          [ED.para-nospace:
            ED.text("Look carefully before the "), ED.styled(ED.text("highlighted text"), 'error-highlight'),
            ED.text(".  Is something missing just before it?"),
            ED.text("  Common missing items are colons ("), ED.code(ED.text(":")),
            ED.text("), commas ("), ED.code(ED.text(",")), ED.text("), string markers ("),
            ED.code(ED.text("\"")), ED.text("), and keywords.")],
          [ED.para: ED.styled(ED.text("Usually, inserting the missing item will fix this error."), "hint")]]
      extra =
        [ED.error:
          [ED.para: ED.text("The program contains something extra")],
          [ED.para-nospace:
            ED.text("Look carefully before the "), ED.styled(ED.text("highlighted text"), 'error-highlight'),
            ED.text(".  Does it contains something extra?"),
            ED.text("  A common source of errors is typing too much text or in the wrong order.")],
          [ED.para:
            ED.styled(ED.text("Usually, removing the extra item will fix this error."), "hint"),
            ED.text("However, you may have meant to keep this text, so think before you delete!")]]
      [ED.error:
        [ED.para: ED.text("Pyret didn't understand your program around"), draw-and-highlight(self.loc)],
        [ED.opt:
          [ED.para: ED.text("Typical reasons for getting this error are")],
          [ED.bulleted: missing, extra]]]
    end
  | parse-error-eof(loc) with:
    render-reason(self):
      [ED.error: [ED.para:
          ED.text("Pyret didn't understand the very end of your program."),
          ED.text("You may be missing an \"end\", or closing punctuation like \")\" or \"]\" right at the end.")]]
    end
  | parse-error-unterminated-string(loc) with:
    render-reason(self):
      [ED.error: [ED.para-nospace:
          ED.text("Pyret thinks your program has an incomplete string literal around "),
          draw-and-highlight(self.loc),
          ED.text("; you may be missing closing punctuation.")]]
    end
  | empty-block(loc) with:
    _tostring(self, shadow tostring):
      "Empty block at " + self.loc.format(true)
    end
  | bad-block-stmt(loc) with:
    _tostring(self, shadow tostring):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | bad-check-block-stmt(loc) with:
    _tostring(self, shadow tostring):
      "Expected a val binding or an expression, but got something else " + self.loc.format(true)
    end
  | fun-missing-colon(loc) with:
    _tostring(self, shadow tostring): "fun-missing-colon: " + self.loc.format(true) end
  | fun-missing-end(loc) with:
    _tostring(self, shadow tostring): "fun-missing-end: " + self.loc.format(true) end
  | args-missing-comma(loc) with:
    _tostring(self, shadow tostring): "args-missing-comma: " + self.loc.format(true) end
  | app-args-missing-comma(loc) with:
    _tostring(self, shadow tostring): "app-args-missing-comma: " + self.loc.format(true) end
  | missing-end(loc)
  | missing-comma(loc)
sharing:
  render-reason(self):
    ED.text(self._tostring(tostring))
  end
end
