### Values not equal31
### Values not equal32
# Note: the error renderings above need to be cleaned up; they're not as
# legible as the existing Pyret command-line renderings
include global

var x = 1
check: x is 1 end
x := 2
check: x is 2 end
x := 3