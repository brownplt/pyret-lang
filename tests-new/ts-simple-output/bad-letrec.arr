##! Uninitialized letrec
include global
letrec x :: Number = y, y :: Number = x: x + y end