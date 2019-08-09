### [array: 1, 2, 3]

import global as G
import array as A

include from A:
  type RawArray
end

foo :: RawArray<Number> = [A.array: 1, 2, 3]

G.console-log( foo )
