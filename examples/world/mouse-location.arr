#lang pyret

import big-bang as B
import image as I

B.big-bang({x: 50, y: 50, type: "No last event"}, {
  to-draw: fun(w):
    x = w.x
    y = w.y
    message = "x: " + x.tostring() + ", y: " + y.tostring() + ", last: " + w.type
    message-img = I.text(message, 12, "black")
    I.place-image(message-img, x, y, I.rectangle(300, 300, "solid", "white"))
  end,
  on-mouse: fun(w, x, y, type):
    {x : x, y : y, type: type}
  end
})
