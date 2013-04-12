#lang pyret

data PyretData
  | pyret-string : s :: String with
    scalliwagify(self :: pyret-string):
      pyret-string(self.s.append(", ye scallawags!"))
    end,
    tostring(self :: pyret-string): s end
end

var pystr :: pyret-string: pyret-string("Ahoy")
print(pystr.scalliwagify())

