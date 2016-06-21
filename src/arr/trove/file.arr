#lang pyret

provide {
  input-file : input-file,
  output-file : output-file,
  file-exists: file-exists,
  file-times: file-times,
  file-to-string: file-to-string,
  real-path: F.real-path
} end
provide-types *

import global as _
import base as _
import filelib as F

data File:
  | in-fd(inner-file :: Any) with:
    method read-file(self): F.read-file(self.inner-file) end,
    method close-file(self): F.close-input-file(self.inner-file) end,
    method full-path(self): F.real-path(self.inner-file) end
  | out-fd(inner-file :: Any) with:
    method display(self, val): F.display(self.inner-file, val) end,
    method close-file(self): F.close-output-file(self.inner-file) end,
    method flush(self): F.flush-output-file(self.inner-file) end
end

fun input-file(path :: String):
  in-fd(F.open-input-file(path))
end

fun file-exists(path :: String):
  F.exists(path)
end

fun file-times(path :: String) block:
  f = input-file(path)
  ts = F.file-times(f.inner-file)
  f.close-file()
  ts
end

fun file-to-string(path) block:
  f = input-file(path)
  s = f.read-file()
  f.close-file()
  s
end
  

fun output-file(path :: String, append :: Boolean):
  out-fd(F.open-output-file(path, append))
end
