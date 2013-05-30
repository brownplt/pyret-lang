#lang pyret/library

provide {
  opaque-error: opaque-error,
  is-opaque-error : is-opaque-error,
  field-not-found: field-not-found,
  is-field-not-found: is-field-not-found,
  make-error: make-error
} end

data Location:
  | location(file :: String, line :: Number, column :: Number) with
    format(self):
      self.file + ": line " + self.line.tostring() + ", column " + self.column.tostring()
    end
end

data Error:
  | opaque-error(message :: String, location :: Location) with
    name(self): "Error using opaque internal value" end
  | field-not-found(message :: String, location :: Location) with
    name(self): "Field not found" end
  | lazy-error(message :: String, location :: Location) with
    name(self): "Email joe@cs.brown.edu or dbpatter@cs.brown.edu and complain that they were lazy" end
sharing
  format(self):
    self.location.format().append(":\n").append(self.name()).append(": ").append(self.message) end
end


fun make-error(obj):
  loc = location(obj.path, obj.line, obj.column)
  case:
    | obj.system =>
      type = obj.value.type
      msg = obj.value.message
      case:
        | (type == "opaque") => opaque-error(msg, loc)
        | (type == "field-not-found") => field-not-found(msg, loc)
        | else => lazy-error(msg, loc)
      end
    | else => obj.value
  end
end

