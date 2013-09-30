#lang pyret

import filelib as F
provide {
  input-file : input-file,
  output-file : output-file,
  file-exists: file-exists
} end

data File:
  | in-fd(inner-file :: Any) with:
    read-line(self): F.read-line(self.inner-file) end,
    read-file(self): F.read-file(self.inner-file) end,
    close-file(self): F.close-input-file(self.inner-file) end
  | out-fd(inner-file :: Any) with:
    display(self, val): F.display(self.inner-file, val) end,
    close-file(self): F.close-output-file(self.inner-file) end,
    flush(self): F.flush-output-file(self.inner-file) end
end

fun input-file(path :: String):
  in-fd(F.open-input-file(path))
end

fun file-exists(path :: String):
  F.file-exists(path)
end

fun output-file(path :: String, append :: Bool):
  exists =
    if append: F.exists-append
    else: F.exists-truncate-replace
    end
  out-fd(F.open-output-file(path, exists))
end

