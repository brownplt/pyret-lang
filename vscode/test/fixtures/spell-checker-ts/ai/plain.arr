use context starter2024

# Sanity / working-directory probe (no imports).
#
# A relative output path is resolved against the directory of THIS open tab
# (ai/), not the workspace root. Running this should produce ai/plain-out.txt.
# write-file-string goes through the `filesystem` module -> filesystem-internal
# -> the extension's pyret-rpc `fs.writeFile` -> vscode.workspace.fs.

import filesystem as FS

FS.write-file-string("plain-out.txt", "hello-from-tab")
