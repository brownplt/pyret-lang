### [list: 1, 2, 3]
import lists as L
import global as G

include from L:
  fold
end

list = [L.list: 1, 2, 3]

G.console-log( G.to-string(list) )
