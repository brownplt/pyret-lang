provide: *, data * end

import runtime-global as G
include from G: raise, to-repr end

data Srcloc:
  | builtin(module-name :: String) with:
    method format(self, _):
      "<builtin " + self.module-name + ">"
    end,
    method key(self): self.module-name end,
    method same-file(self, other :: Srcloc):
      cases(Srcloc) other:
        | builtin(module-name) => self.module-name == module-name
        | srcloc(_, _, _, _, _, _, _) => false
      end    end,
    method before(self, other :: Srcloc):
      cases(Srcloc) other:
        | builtin(module-name) => (self.module-name < other.module-name)
        | srcloc(_, _, _, _, _, _, _) => false
      end
    end,
    method contains(self, other):
      false
    end,
    method is-builtin(self): true end
  | srcloc(
      source :: String,
      start-line :: Number,
      start-column :: Number,
      start-char :: Number,
      end-line :: Number,
      end-column :: Number,
      end-char :: Number
      ) with:
    method format(self, show-file):
      doc: "Returns either 'file: line, col' or just 'line, col', depending on the show-file flag"
      # if show-file: self.source + ": " else: "" end
      #   + "line " + tostring(self.start-line)
      #   + ", column " + tostring(self.start-column)
      if show-file:
        self.source + ":" + to-repr(self.start-line) + ":" + to-repr(self.start-column)
          + "-" + to-repr(self.end-line) + ":" + to-repr(self.end-column)
      else:
        "line " + to-repr(self.start-line) + ", column " + to-repr(self.start-column)
      end
    end,
    method key(self): self.source + ":" + to-repr(self.start-char) + "-" + to-repr(self.end-char) end,
    method same-file(self, other :: Srcloc):
      cases(Srcloc) other:
        | builtin(_) => false
        | srcloc(source, _, _, _, _, _, _) => self.source == source
      end
    end,
    method before(self, other :: Srcloc):
      doc: "Returns true if this location comes before the other one, assuming they come from the same file"
      cases(Srcloc) other:
        | builtin(_) => true
        | srcloc(_, _, _, start-char, _, _, _) => self.start-char < start-char
      end
    end,
    method at-start(self):
      srcloc(self.source,
        self.start-line, self.start-column, self.start-char,
        self.start-line, self.start-column, self.start-char)
    end,
    method at-end(self):
      srcloc(self.source,
        self.end-line, self.end-column, self.end-char,
        self.end-line, self.end-column, self.end-char)
    end,
    method _plus(self, other :: Srcloc%(is-srcloc)):
      cases(Srcloc) other:
        | builtin(_) => raise("Can't happen")
        | srcloc(_, start-line, start-column, start-char, end-line, end-column, end-char) =>
          # Note: assumes that both locations are from same file
          if self.start-char <= start-char:
            if self.end-char >= end-char:
              self
            else:
              srcloc(self.source,
                self.start-line, self.start-column, self.start-char,
                end-line, end-column, end-char)
            end
          else:
            if self.end-char > other.end-char:
              srcloc(self.source,
                start-line, start-column, start-char,
                self.end-line, self.end-column, self.end-char)
            else:
              other
            end
          end
      end
    end,
    method upto(self, other :: Srcloc%(is-srcloc)):
      cases(Srcloc) other:
        | builtin(_) => raise("Can't happen")
        | srcloc(_, start-line, start-column, start-char, end-line, end-column, end-char) =>
          # Note: assumes that both locations are from same file
          if self.start-char <= end-char:
            srcloc(self.source,
              self.start-line, self.start-column, self.start-char,
              start-line, start-column, start-char)
          else:
            self
          end
      end
    end,
    method upto-end(self, other :: Srcloc%(is-srcloc)):
      cases(Srcloc) other:
        | builtin(_) => raise("Can't happen")
        | srcloc(_, start-line, start-column, start-char, end-line, end-column, end-char) =>
          # Note: assumes that both locations are from same file
          if self.start-char <= end-char:
            srcloc(self.source,
              self.start-line, self.start-column, self.start-char,
              end-line, end-column, end-char)
          else:
            self
          end
      end
    end,
    method contains(self, other :: Srcloc%(is-srcloc)):
      cases(Srcloc) other:
        | builtin(_) => raise("Can't happen")
        | srcloc(_, start-line, start-column, start-char, end-line, end-column, end-char) =>
          is-srcloc(other)
          and (self.start-line <= start-line)
          and (self.start-char <= start-char)
          and (self.end-line >= end-line)
          and (self.end-char >= end-char)
      end
    end,
    method is-builtin(self): false end
sharing:
  method after(self, other :: Srcloc): other.before(self) end
end