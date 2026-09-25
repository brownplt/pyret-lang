import npm("pyret-test-files", "root-of-package.arr") as root-of-package
import npm("pyret-test-files", "lib/nested-in-lib.arr") as nested-in-lib

print(root-of-package.name + " " + nested-in-lib.name)
