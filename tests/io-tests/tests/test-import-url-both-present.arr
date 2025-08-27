###@ --url-file-mode all-local
###> same dir.*same dir

import url-file("http://0.0.0.0:7999/nested", "library-code.arr") as L1
import url-file("http://0.0.0.0:7999", "library-code.arr") as L2


print(L1.x + L2.x)

