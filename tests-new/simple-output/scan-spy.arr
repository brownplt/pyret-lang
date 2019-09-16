### Spying "foo"
### b: true
### z: 5
### y: 10
import global as G

x = 5
y = 10

spy "foo":

  b: if true and true:
    true
  else:
    false
  end,

  z: block:
    
    a :: Number = 5
    a
  end
end

spy:
  y
end
