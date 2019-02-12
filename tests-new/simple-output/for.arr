### success
import global as G
import list as L

l = [L.list: 1, 2, 3, 4, 5]
list = for L.map(v from l):
  v * 2
end

if L.contains(list, 10):
  G.console-log("success")
else:
  G.console-log("failure")
end
