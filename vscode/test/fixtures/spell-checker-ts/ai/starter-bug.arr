use context starter2024

# Stand-in for "Spell Checker Starter File.arr" — the natural ("BEFORE") version
# that we WISH worked.
#
# Identical to starter-ok.arr except it imports lib-bug.arr, whose internal
# import of core.arr is written the intuitive way ("core.arr", relative to the
# library). Because url-file's local path is resolved against THIS tab's
# directory (ai/) rather than the library's directory (libraries/), the nested
# load of core.arr fails, the program never runs, and result-bug.txt is never
# written.

import filesystem as FS
import url-file("https://example.org/starter-files/libraries", "../libraries/lib-bug.arr") as Lib

FS.write-file-string("result-bug.txt", num-to-string(Lib.lib-answer))
