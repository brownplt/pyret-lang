include chart
include image
import color as C
# include image-structs
include math

fun f(r): star(50, "solid", "red") end
values = [list: 1,3,4,6,8,2,2,4,65,3,2]

series = from-list.image-histogram(
  values.map(f),
  values)
  
img = render-chart(series)
  .y-axis("species")
  .get-image()

check:
  img satisfies is-image
  color-at-position(img, 205, 282) is C.red
end
