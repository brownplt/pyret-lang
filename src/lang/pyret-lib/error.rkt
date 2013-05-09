#lang pyret/library

provide {
  opaque-error: opaque-error,
  is-opaque-error : is-opaque-error,
  field-not-found: field-not-found,
  is-field-not-found: is-field-not-found,
  make-error: make-error
} end

data Location:
  | location : file :: String, line :: Number, column :: Number with
    format(self):
      self.file.append(": line ").append(self.line.tostring()).append(", column ").append(self.column.tostring())
end

data Error:
  | opaque-error : message :: String, location :: Location with
    name(self): "Error using opaque internal value"
  | field-not-found : message :: String, location :: Location with
    name(self): "Field not found"
  | lazy-error : message :: String, location :: Location with
    name(self): "Email joe@cs.brown.edu or dbpatter@cs.brown.edu and complain that they were lazy"
sharing
  format(self):
    self.location.format().append(":\n").append(self.name()).append(": ").append(self.message)
end


fun make-error(obj):
  loc = location(obj.path, obj.line, obj.column)
  cond:
    | obj.system =>
      type = obj.value.type
      msg = obj.value.message
      cond:
        | type.equals("opaque") => opaque-error(msg, loc)
        | type.equals("field-not-found") => field-not-found(msg, loc)
        | else => lazy-error(msg, loc)
      end
    | else => obj.value
  end
end
