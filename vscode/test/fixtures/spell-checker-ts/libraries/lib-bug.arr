use context starter2024

# Stand-in for spell-checker-library.arr — the "BEFORE" of the diff, and the
# behavior we WISH worked.
#
# core.arr is a sibling of this file (both in libraries/), so the natural thing
# to write is just "core.arr". A reader of this library would expect that to
# resolve relative to the library itself. It does NOT today: the local path is
# resolved against the open tab's directory (ai/), so "core.arr" is looked up at
# ai/core.arr, which does not exist, and the load fails.

provide *

import url-file("https://example.org/starter-files/libraries", "core.arr") as Core

lib-answer = Core.the-answer
