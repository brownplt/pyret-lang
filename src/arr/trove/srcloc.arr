#lang pyret/library

provide *
provide-types *

import valueskeleton as VS

data Srcloc:
  | builtin(module-name) with:
    format(self, _):
      "<builtin " + self.module-name + ">"
    end,
    key(self): self.module-name end,
    same-file(self, other):
      is-builtin(other) and (other.module-name == self.module-name)
    end,
    before(self, other):
      cases(Srcloc) other:
        | builtin(module-name) => (self.module-name < other.module-name)
        | srcloc(_, _, _, _, _, _, _) => false
      end
    end,
    is-builtin(self): true end
  | srcloc(
        source :: String,
        start-line :: Number,
        start-column :: Number,
        start-char :: Number,
        end-line :: Number,
        end-column :: Number,
        end-char :: Number
      ) with:
    format(self, show-file):
      doc: "Returns either 'file: line, col' or just 'line, col', depending on the show-file flag"
      if show-file: self.source + ": " else: "" end
        + "line " + tostring(self.start-line)
        + ", column " + tostring(self.start-column)
    end,
    key(self): self.source + ":" + tostring(self.start-char) + "-" + tostring(self.end-char) end,
    same-file(self, other :: Srcloc):
      is-srcloc(other) and (self.source == other.source)
    end,
    before(self, other :: Srcloc):
      doc: "Returns true if this location comes before the other one, assuming they come from the same file"
      cases(Srcloc) other:
        | builtin(_) => true
        | else => self.start-char < other.start-char
      end
    end,
    at-start(self):
      srcloc(self.source,
        self.start-line, self.start-column, self.start-char,
        self.start-line, self.start-column, self.start-char)
    end,
    at-end(self):
      srcloc(self.source,
        self.end-line, self.end-column, self.end-char,
        self.end-line, self.end-column, self.end-char)
    end,
    _plus(self, other :: Srcloc%(is-srcloc)):
      # Note: assumes that both locations are from same file
      if self.start-char <= other.start-char:
        if self.end-char >= other.end-char:
          self
        else:
          srcloc(self.source,
            self.start-line, self.start-column, self.start-char,
            other.end-line, other.end-column, other.end-char)
        end
      else:
        if self.end-char > other.end-char:
          srcloc(self.source,
            other.start-line, other.start-column, other.start-char,
            self.end-line, self.end-column, self.end-char)
        else:
          other
        end
      end
    end,
    upto(self, other :: Srcloc%(is-srcloc)):
      # Note: assumes that both locations are from same file
      if self.start-char <= other.end-char:
        srcloc(self.source,
          self.start-line, self.start-column, self.start-char,
          other.end-line, other.end-column, other.end-char)
      else:
        self
      end
    end,
    is-builtin(self): false end
sharing:
  _output(self): VS.vs-value(self.format(true)) end,
  after(self, other): other.before(self) end
end
