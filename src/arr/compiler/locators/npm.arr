provide: make-npm-locator end

import require-util as R
import filesystem as FS
import file("../compile-structs.arr") as CS
import file("./file.arr") as F

fun make-npm-locator(package-name, path, current-load-path):
    package-path = R.resolve(package-name, current-load-path)
    F.file-locator(
        FS.resolve(FS.join(FS.dirname(package-path), path)),
        CS.standard-globals
    )
end
