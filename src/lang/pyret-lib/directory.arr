#lang pyret

provide {dir : dir} end

import "file.arr" as file

data Directory:
  | dir(path :: Any) with 
    list(self): Racket.directory-list(self.path),
    change(self, rel-path): dir(Racket.build-path(self.path, rel-path)),
end

print(dir(".").list())
