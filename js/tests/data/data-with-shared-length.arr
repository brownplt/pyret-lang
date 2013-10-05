data MyList:
  | mcons(first, rest)
  | mempty()
sharing:
  length(self):
    cases(MyList) self:
      | mcons(_, rest) => 1._add(rest.length())
      | mempty => 0
    end
  end
end
mcons(1, mcons(2, mempty())).length()