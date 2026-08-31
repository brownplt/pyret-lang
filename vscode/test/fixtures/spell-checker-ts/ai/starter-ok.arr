use context starter2024

# Stand-in for "Spell Checker Starter File.arr" — the working ("AFTER") version.
#
# This tab lives in ai/. It imports the library from libraries/, and the library
# in turn imports core.arr using the "../libraries/core.arr" workaround. The
# whole chain resolves, the program runs, and it writes the answer pulled all the
# way through Lib -> Core out to a file we can observe from the test.

import filesystem as FS
import url-file("https://example.org/starter-files/libraries", "../libraries/lib-ok.arr") as Lib

FS.write-file-string("result-ok.txt", num-to-string(Lib.lib-answer))
