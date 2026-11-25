use context url-file("{{FIXTURE_ORIGIN}}/pyret-programs/url-imports/app", "../lib/mini-context.arr")
provide *

# context-marker comes from this module's own context, not the includer's.
nested-context-marker = context-marker + 1
