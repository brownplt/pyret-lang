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
      exists(self):
        Racket("racket/base")("directory-exists?", self.path)
      end,
      create(self):
        Racket("racket/base")("make-directory", self.path)
        self
      end,
      create-p(self):
        when not self.exists():
          Racket("racket/base")("make-directory", self.path)
        end
        self
      end
end

