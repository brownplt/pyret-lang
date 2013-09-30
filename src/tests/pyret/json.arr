#lang pyret

import json as json

check:
  json.parse("5") is 5
  json.parse('{"x":22}') is {x:22}
  json.parse(json.stringify({x:5})) is {x:5}

  json.parse("[1,2,3]") is [1,2,3]
  json.parse(json.stringify([{x:5}, {y:10}])) is [{x:5}, {y:10}]
  json.parse("true") is true
  json.parse("false") is false

  json.parse("\"asdf\"") is "asdf"
end

