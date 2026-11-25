include chart
include image
import color as C
# include image-structs
include math

labels = [list: "cats", "cats", "cats",
                "dogs", "dogs", "dogs", "dogs", "dogs", "dogs", "dogs",
                "ants", "ants", "ants", "ants",
                "elephants", "elephants", "elephants", "elephants", "elephants", "elephants", "elephants", "elephants", "elephants"]

zoo-series = from-list.dot-chart(labels)

just-red = [list: C.red]
rainbow-colors = [list: C.red, C.orange, C.yellow, C.green, C.blue, C.indigo, C.violet]
manual-colors =
  [list:
    C.color(51, 72, 252, 0.57), C.color(195, 180, 104, 0.87),
    C.color(115, 23, 159, 0.24), C.color(144, 12, 138, 0.13),
    C.color(31, 132, 224, 0.83), C.color(166, 16, 72, 0.59),
    C.color(58, 193, 241, 0.98)]
fewer-colors = [list: C.red, C.green, C.blue, C.orange, C.purple]
more-colors = [list: C.red, C.green, C.blue, C.orange, C.purple, C.yellow, C.indigo, C.violet]

fun render-image(series):
  render-chart(series).get-image()
end

zoo = render-image(zoo-series)
zoo-red = render-image(zoo-series.colors(just-red))
zoo-rainbow = render-image(zoo-series.colors(rainbow-colors))
zoo-manual = render-image(zoo-series.colors(manual-colors))
zoo-fewer = render-image(zoo-series.colors(fewer-colors))
zoo-more = render-image(zoo-series.colors(more-colors))

check "Dot chart, categorical data":
  zoo satisfies is-image
  zoo-red satisfies is-image
  zoo-rainbow satisfies is-image
  zoo-manual satisfies is-image
  zoo-fewer satisfies is-image
  zoo-more satisfies is-image

  color-at-position(zoo-rainbow, 208, 340) is C.red
  color-at-position(zoo-rainbow, 322, 340) is C.orange
  color-at-position(zoo-rainbow, 439, 340) is C.yellow
  color-at-position(zoo-rainbow, 558, 340) is C.green
end


