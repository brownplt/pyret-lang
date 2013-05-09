#lang pyret

import Racket as R
provide {file : file} end

data File:
  | fd: inner-file :: Any with
    read-line(self): R('read-line')(self.inner-file),
    read-file(self): R('port->string')(self.inner-file),
    close-file(self): R('close-input-port')(self.inner-file)
end

fun file(path :: String):
  fd(R('open-input-file')(path))
end

#file("file.arr").read-line()
#file("file.arr").read-file()
