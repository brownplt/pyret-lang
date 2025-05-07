
###@ --url-file-mode all-remote
###* fetch

# Here, the file is present! But we ask for all-remote above
# So this should fail to fetch.
import url-file("http://0.0.0.0:9999", "library-code.arr") as I

print(I.x)