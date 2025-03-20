###@ --url-file-mode local-if-present
###> 100
import url-file("https://raw.githubusercontent.com/brownplt/pyret-lang/refs/heads/horizon/tests/io-tests/tests/", "library-code.arr") as I

print(I.x)