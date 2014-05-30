#lang pyret 0.5

provide *

data ContractResult:
  | ok
  | fail(loc, reason :: FailureReason) with:
    tostring(self):
      "Contract Error at " + self.loc.format(true) + "\n"
        + tostring(self.reason)
    end
  | fail-arg(loc, reason :: FailureReason) with:
    tostring(self):
      "Contract Error at " + self.loc.format(true) + "\n"
        + tostring(self.reason)
    end
end

data FieldFailure:
  | field-failure(loc, field, reason) with:
    tostring(self):
      "At " + self.loc.format(true) + ", field " + self.field + " failed because\n" + tostring(self.reason)
    end
  | missing-field(loc, field) with:
    tostring(self):
      "Missing field " + self.field + " in required at " + self.loc.format(true)
    end
end

data FailureReason:
  | type-mismatch(val, name :: String) with:
    tostring(self):
      "Expected " + self.name + ", but got " + torepr(self.val)
    end
  | predicate-failure(val, pred-name) with:
    tostring(self):
      "Predicate " + self.pred-name + " failed on value " + torepr(self.val)
    end
  | record-fields-fail(val, field-failures :: List<FieldFailure>) with:
    tostring(self):
      "Record annotation failed on value\n" + torepr(self.val) + "\n\nBecause: " +
        self.field-failures.map(tostring).join-str("\n\n")
    end
  | dot-ann-not-present(name, field) with:
    tostring(self):
      "Couldn't find the annotation field " + self.field + " on " + self.name
    end
end

