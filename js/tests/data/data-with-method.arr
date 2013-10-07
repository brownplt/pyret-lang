data MyList:
  | myempty(type :: String) with:
    show-type(self): self.type end
sharing:
  addendum(self): "!!" end
end
e = myempty("foo")
e.show-type() + e.addendum()