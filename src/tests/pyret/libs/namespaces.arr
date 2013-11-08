#lang pyret

import namespaces as n
ws = n.whalesong-env

check:
  ws.is-empty([]) is true
  ws.is-link([1]) is true
  list.is-empty(ws.empty) is true
  list.is-link(ws.link(1, ws.empty)) is true
  ws.link(1, []) is [1]
end

