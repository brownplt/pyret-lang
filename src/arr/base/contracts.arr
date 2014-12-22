#lang pyret 0.5

provide *
provide-types *
import lists as L

data ContractResult:
  | ok
  | fail(loc, reason :: FailureReason) with:
    tostring(self, shadow tostring):
      "Contract Error at " + self.loc.format(true) + "\n"
        + tostring(self.reason)
    end
  | fail-arg(loc, reason :: FailureReason) with:
    tostring(self, shadow tostring):
      "Contract Error at " + self.loc.format(true) + "\n"
        + tostring(self.reason)
    end
end

data FieldFailure:
  | field-failure(loc, field, reason) with:
    tostring(self, shadow tostring):
      "At " + self.loc.format(true) + ", field " + self.field + " failed because\n" + tostring(self.reason)
    end
  | missing-field(loc, field) with:
    tostring(self, shadow tostring):
      "Missing field " + self.field + " in required at " + self.loc.format(true)
    end
end

data FailureReason:
  | ref-init(loc, reason :: FailureReason) with:
    tostring(self, shadow tostring):
      "The annotation at " + self.loc.format(true) + " failed while initializing a reference because: "
        + tostring(self.reason)
    end
  | type-mismatch(val, name :: String) with:
    tostring(self, shadow tostring):
      "Expected " + self.name + ", but got " + torepr(self.val)
    end
  | predicate-failure(val, pred-name) with:
    tostring(self, shadow tostring):
      "Predicate " + self.pred-name + " failed on value " + torepr(self.val)
    end
  | record-fields-fail(val, field-failures :: L.List<FieldFailure>) with:
    tostring(self, shadow tostring):
      "Record annotation failed on value\n" + torepr(self.val) + "\n\nBecause: " +
        self.field-failures.map(tostring).join-str("\n\n")
    end
  | dot-ann-not-present(name, field) with:
    tostring(self, shadow tostring):
      "Couldn't find the annotation field " + self.field + " on " + self.name
    end
end

