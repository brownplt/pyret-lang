### foobarbazbuzbazbuzfoobarbazbuz
import global as G

{ foo; bar; { baz; buz } as qax } as qux = { "foo"; "bar"; { "baz"; "buz" } }

result = foo + bar + baz + buz + qax.{0} + qax.{1} + qux.{0} + qux.{1} + qux.{2}.{0} + qux.{2}.{1}

G.console-log(result)
