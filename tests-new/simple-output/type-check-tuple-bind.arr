### foobarbazbuzbazbuzfoobarbazbuz
import global as G

{ foo :: String; bar :: String; { baz; buz } as qax :: { String; String } }
 as qux :: { String; String; { String; String } }
 = { "foo"; "bar"; { "baz"; "buz" } }

G.display-string(foo)
G.display-string(bar)
G.display-string(baz)
G.display-string(buz)
G.display-string(qax.{0})
G.display-string(qax.{1})
G.display-string(qux.{0})
G.display-string(qux.{1})
G.display-string(qux.{2}.{0})
G.display-string(qux.{2}.{1})
