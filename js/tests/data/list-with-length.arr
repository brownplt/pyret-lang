data MyList:
  | mcons(first, rest) with: length(self): 1._add(self.rest.length()) end
  | mempty() with: length(self): 0 end
end
mcons(1, mcons(2, mempty())).length()