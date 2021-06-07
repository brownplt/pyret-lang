### pass
import global as G

r1 = 5 <> 5
r2 = 5 <> 4
r3 = true <> false

if r1:
  G.console-log("fail")
else:

  if r2:

    if r3:
      G.console-log("pass")
    else:
      G.console-log("fail")
    end

  else:
    G.console-log("fail")
  end

end
