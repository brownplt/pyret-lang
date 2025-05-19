use context essentials2020
include charts
import color as C
import image-typed as I

colors =       [list: C.red,   C.orange, C.yellow, C.green, C.blue,   C.purple, C.pink]
langs =        [list: "Pyret", "OCaml",  "C",      "C++",   "Python", "Racket", "Smalltalk"]
popularities = [list: 10,      -6,        1,        3,       5,        8,        9]
# images = colors.map(I.circle(30, I.mode-solid, _))
images = colors.map({(_): I.square(30, I.mode-outline, C.black)})
a-series = from-list.bar-chart(langs, popularities)
  .color(C.purple)
  .colors(colors)

b-series = from-list.image-bar-chart(images, langs, popularities)
  .colors(colors.reverse())

rendered = render-chart(b-series).background-color(C.gray)

I.save-image(rendered.get-image(), 'test-bar-chart.png')
