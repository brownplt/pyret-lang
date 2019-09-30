### 123
import list as L
import global as G

list = [L.list: 1, 2, 3]

msg = for L.fold(string from "", e from list):
  string + G.js-to-string(e)
end
G.console-log( msg )
