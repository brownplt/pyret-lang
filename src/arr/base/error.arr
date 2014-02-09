provide *

data Location:
  | location(file, line, column) with:
    _equals(self, other):
      is-location(other) and
      (self.file == other.file) and
      (self.line == other.line) and
      (self.column == other.column)
    end,
    format(self):
      self.file +
      ": line " +
      self.line.tostring() +
      ", column " +
      self.column.tostring()
    end,
    tostring(self): self.format() end
end
