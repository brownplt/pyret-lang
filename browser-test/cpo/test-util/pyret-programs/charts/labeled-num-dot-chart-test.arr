include chart
include image
import color as C
# include image-structs
include math

r3-counts = [list: 1, 3, 4, 2, 5, 4, 3]
r3-labels = [list: "ant", "bee", "cow", "dog", "eel", "fly", "grub"]

r3-zoo-series = from-list.labeled-num-dot-chart(r3-labels, r3-counts)
r3-zoo = render-chart(r3-zoo-series).get-image()

check:
  r3-zoo satisfies is-image
end

# fun t1(): render-chart(r3-zoo-series).display() end
