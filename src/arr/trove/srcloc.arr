#lang pyret/library

provide *

data Srcloc:
  | srcloc(
        source :: String,
        start-line :: Number,
        start-column :: Number,
        start-char :: Number,
        end-line :: Number,
        end-column :: Number,
        end-char :: Number
      )
end

fun old-srcloc(file, startR, startC, startCh, endR, endC, endCh):
  raise("Cannot create old-srclocs anymore")
end

