
provide:
  string-dict as sd,
  mutable-string-dict as msd,
  type StringDict as SD,
  type MutableStringDict as MSD,
  module S,
  module L,
end

import lists as L
import string-dict as S

include from S:
  string-dict, mutable-string-dict,
  type StringDict, type MutableStringDict
end

