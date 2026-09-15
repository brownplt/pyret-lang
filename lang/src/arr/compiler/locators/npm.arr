provide: make-npm-locator, npm-package-root end

import require-util as R
import filesystem as FS
import file("../compile-structs.arr") as CS
import file("./file.arr") as F

# via package.json: dirname(R.resolve(pkg)) is main's dir, which imports climb out of
fun npm-package-root(package-name, current-load-path):
    FS.dirname(R.resolve(package-name + "/package.json", current-load-path))
end

# paths stay relative to dirname(main): existing imports are written against it
fun make-npm-locator(package-name, path, current-load-path):
    package-path = R.resolve(package-name, current-load-path)
    F.file-locator(
        FS.resolve(FS.join(FS.dirname(package-path), path)),
        CS.standard-globals
    )
end
