###> Looks shipshape

import url-file("http://0.0.0.0:7999/", "library-code.arr") as LC
import url-file("http://0.0.0.0:7999/", "nested/imports-library-with-dotdot.arr") as N

check:
  N.d() is LC.d()
end

