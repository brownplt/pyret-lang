###> Looks shipshape

include fetch
import either as E

check:
    # See https://httpstat.us/
    result = fetch("https://httpstat.us/200?sleep=30000")
    result satisfies E.is-right
    to-repr(result) satisfies string-contains(_, "Abort")
end