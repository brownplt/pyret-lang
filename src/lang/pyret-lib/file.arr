#lang pyret

import Racket as R
provide {file : file} end

data File:
  | fd(inner-file :: Any) with:
    read-line(self): R("racket")('read-line', self.inner-file) end,
    read-file(self): R("racket")('port->string', self.inner-file) end,
    close-file(self): R("racket")('close-input-port', self.inner-file) end
end

fun file(path :: String):
  fd(R("racket")('open-input-file', path))
end

#file("file.arr").read-line()
#file("file.arr").read-file()
