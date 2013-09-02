#lang pyret

import big-bang as B
import image as I

B.big-bang("Press a key!", {
  on-key: fun(w, k): k end,
  to-draw: fun(w):
    I.text("You last pressed " + w, 12, "blue")
  end
})

