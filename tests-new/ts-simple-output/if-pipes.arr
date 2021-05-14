### baz 
import global as G

ask:
  | false then: G.console-log("foo")
  | false then: G.console-log("bar")
  | true then: G.console-log("baz")
end
