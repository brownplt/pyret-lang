use context essentials2020
include charts
import color as C
import image-typed as I

colors =       [list: C.red,   C.orange, C.yellow, C.green, C.blue,   C.purple, C.pink]
langs =        [list: "Pyret", "OCaml",  "C",      "C++",   "Python", "Racket", "Smalltalk"]
popularities = [list: 10,      -6,        1,        3,       5,        8,        9]
# images = colors.map(I.circle(30, I.mode-solid, _))
images = colors.map(I.triangle(30, I.mode-solid, _))
a-series = from-list.bar-chart(langs, popularities)
  .color(C.purple)
  .colors(colors)

b-series = from-list.#|image-|#bar-chart(#|images, |#langs, popularities)
#  .horizontal(true)
  .colors(colors.reverse())
  .add-pointers([list: 2.5, 3.5], [list: "hi", "bye"])
  .pointer-color(C.maroon)
.annotations(langs.map({(l): some(string-explode(l).reverse().join-str(""))}))
.intervals(for map_n(i from 0, _ from colors):
  [list: -5 + i, i, 5 + i, 3 + i]
end)
#.interval-color(C.chartreuse)
.format-axis({(v): "Label " + to-string(v)})

rendered = render-chart(b-series).background-color(C.gray)

I.save-image(rendered.get-image(), 'test-bar-chart.png')
