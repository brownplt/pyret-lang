#lang pyret/library

provide *

data Srcloc:
  | srcloc(
        source :: String,
        start-row :: Number,
        start-col :: Number,
        start-char :: Number,
        end-row :: Number,
        end-col :: Number,
        end-char :: Number
      )
end

fun old-srcloc(file, startR, startC, startCh, endR, endC, endCh):
  error.location(file, startR, startC)
end

