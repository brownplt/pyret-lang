import npm("pyret-test-files", "root-of-package.arr") as root-of-package
import npm("pyret-test-files", "lib/nested-in-lib.arr") as nested-in-lib

check:
    root-of-package.name is "root-of-package"
    nested-in-lib.name is "nested-in-lib"
end
