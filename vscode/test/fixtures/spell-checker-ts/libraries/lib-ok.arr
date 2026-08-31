use context starter2024

# Stand-in for spell-checker-library.arr — the "AFTER" of the diff.
#
# core.arr sits RIGHT NEXT TO this file (both in libraries/). But url-file's
# local path (the second argument) is resolved against the directory of the
# OPEN EDITOR TAB (ai/), not the directory of this importing module. So to
# reach the sibling core.arr we have to climb back out with "../libraries/".
# This is the "../ nonsense" — it works, but only because it is written
# relative to the tab, not relative to this library.

provide *

import url-file("https://example.org/starter-files/libraries", "../libraries/core.arr") as Core

lib-answer = Core.the-answer
