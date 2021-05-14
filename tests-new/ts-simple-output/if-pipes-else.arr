### qux
import global as G

ask:
  | false then: G.console-log("foo")
  | false then: G.console-log("bar")
  | false then: G.console-log("baz")
  | otherwise: G.console-log("qux")
end
