#lang pyret

five :: Number = 5
s :: String = "hello"
b :: Bool = true
#def o :: {x:Number}: {x:10}

# this function can never run successfully
fun bar(x :: Number) -> Number: "hello" end
#bar(10)

# this function will fail on non string input
fun baz(x :: String): x end
baz("hello")
#baz(10)
