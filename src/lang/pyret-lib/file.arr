#lang pyret

data File
  | fd: inner-file :: Any with
    read-line(self): Racket.read-line(self.inner-file),
    read-file(self): Racket.["port->string"](self.inner-file),
    close-file(self): Racket.close-input-port(self.inner-file)
end

fun file(path :: String):
  fd(Racket.open-input-file(path))
end

file("file.arr").read-line()
file("file.arr").read-file()