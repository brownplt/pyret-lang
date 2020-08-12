### 123
import lists as L
import global as G

include from L:
  fold
end

list = [L.list: 1, 2, 3]

msg = for fold(string from "", e from list):
  string + G.js-to-string(e)
end
G.console-log( msg )
