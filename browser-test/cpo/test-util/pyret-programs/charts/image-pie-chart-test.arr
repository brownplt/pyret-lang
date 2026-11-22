# include color
import color as C
include chart
include image

# csq "color square"
fun csq(c): square(20, "solid", c) end

image-pie-series = from-list.image-pie-chart(
  [list: csq("red"),  csq("blue"),  csq("green"), csq("pink"),csq("orange")],
  [list: "Pyret",     "OCaml",      "C",          "C++",      "Python"],
  [list: 10,          6,            1,            3,          5])

img = render-chart(image-pie-series).get-image()

# invert the y coordinate, since CPO uses the top as 0 and every image editor uses the bottom
top = image-height(img)

################################
# IMAGE PIE CHART TESTS
################################

check "Checking image-pie-chart":
  color-at-position(img, 339, top - 334) is C.red
  color-at-position(img, 293, top - 179) is C.blue
  color-at-position(img, 212, top - 215) is C.green
  color-at-position(img, 216, top - 264) is C.pink
  color-at-position(img, 245, top - 317) is C.orange
end

