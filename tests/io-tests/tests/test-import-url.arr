###@ --url-file-mode local-if-present
###> same dir
import url-file("http://0.0.0.0:7999", "library-code.arr") as I

print(I.x)