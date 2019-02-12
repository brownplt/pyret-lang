### pass
import global as G

r1 = 5 == 5
r2 = 5 == 4
r3 = true == false

if r1:

  if r2:
    G.console-log("fail")
  else:
    if r3:
      G.console-log("fail")
    else:
      G.console-log("pass")
    end
  end

else:

  G.console-log("fail")
end
