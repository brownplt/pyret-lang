#lang pyret 0.5

provide *

data ContractResult:
  | ok
  | fail(loc, reason :: FailureReason) with:
    tostring(self):
      "Contract Error at " + self.loc.format(true) + "\n"
        + tostring(self.reason)
    end
end

data FieldFailure:
  | field-failure(loc, field, reason)
end

data FailureReason:
  | type-mismatch(val, name :: String) with:
    tostring(self):
      "Expected " + self.name + ", but got " + torepr(self.val)
    end
  | predicate-failure(val, pred-name) with:
    tostring(self):
      "Predicate " + self.pred-name + ", failed on value " + torepr(self.val)
    end
#  | datatype-mismatch(loc, datatype-loc, val, type :: String)
  | record-fields-fail(val, field-failures :: List<FieldFailure>)
  | missing-field-fail(loc, val, fields :: List<String>)
end

