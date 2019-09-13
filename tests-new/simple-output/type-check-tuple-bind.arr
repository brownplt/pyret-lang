### foobarbazbuzbazbuzfoobarbazbuz
import global as G

{ foo :: String; bar :: String; { baz; buz } as qax :: { String; String } }
 as qux :: { String; String; { String; String } }
 = { "foo"; "bar"; { "baz"; "buz" } }

msg = foo + bar + baz + buz + qax.{0} + qax.{1} + qux.{0} + qux.{1} + qux.{2}.{0} + qux.{2}.{1}

G.console-log(msg)
