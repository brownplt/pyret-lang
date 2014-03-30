#lang pyret/library

provide *

data Srcloc:
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
    tostring(self): self.format(true) end,
    same-file(self, other :: Location):
      self.source == other.source
    end,
    before(self, other :: Location):
      doc: "Returns true if this location comes before the other one, assuming they come from the same file"
      if self.start-line < other.start-line: true
      else if (self.start-line == other.start-line): self.start-column < other.start-column
      else: false
      end
    end,
    after(self, other :: Location):
      doc: "Returns true if this location comes after the other one, assuming they come from the same file"
      if self.start-line > other.start-line: true
      else if (self.start-line == other.start-line): self.start-column > other.start-column
      else: false
      end
    end
end

fun old-srcloc(file, startR, startC, startCh, endR, endC, endCh):
  raise("Cannot create old-srclocs anymore")
end

