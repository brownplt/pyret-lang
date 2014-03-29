provide *

data Location:
  | location(file, line, column) with:
    _equals(self, other):
      is-location(other) and
      (self.file == other.file) and
      (self.line == other.line) and
      (self.column == other.column)
    end,
    format(self, show-file):
      doc: "Returns either 'file: line, col' or just 'line, col', depending on the show-file flag"
      if show-file: self.file + ": " else: "" end
        + "line " + tostring(self.line)
        + ", column " + tostring(self.column)
    end,
    tostring(self): self.format(true) end,
    same-file(self, other :: Location):
      self.file == other.file
    end,
    before(self, other :: Location):
      doc: "Returns true if this location comes before the other one, assuming they come from the same file"
      if self.line < other.line: true
      else if (self.line == other.line): self.column < other.column
      else: false
      end
    end,
    after(self, other :: Location):
      doc: "Returns true if this location comes after the other one, assuming they come from the same file"
      if self.line > other.line: true
      else if (self.line == other.line): self.column > other.column
      else: false
      end
    end
end
