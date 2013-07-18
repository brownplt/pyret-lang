#lang pyret

provide {dir : dir} end

import file as file
import Racket as Racket

data Directory:
  | dir(path :: Any) with:
      list(self): list.map(fun(p): Racket("racket")("path->string", p) end,
                           Racket("racket")("directory-list", self.path))
      end,
      change(self, rel-path): dir(Racket("racket")("build-path", self.path, rel-path)) end,
end