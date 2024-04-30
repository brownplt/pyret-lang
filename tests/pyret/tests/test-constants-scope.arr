# PI shadowable in global scope
import constants as C
include from C: PI end

check:
  PI is-roughly ~3.141592653589793
end

