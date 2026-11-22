include chart
include image
import color as C
# include image-structs
include math

labels = [list: "cats", "dogs", "ants", "elephants"]
count = [list: 3, 7, 4, 9]

# zoo-series = from-list.dot-chart(labels, count)

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
  # render-chart(series).get-image()
  render-chart(series).get-image()
end


# x-values = [list: 1, 2, 3, 4, 5, 6, 8, 9, 10, 24, 30]
# freqs    = [list: 5, 3, 6, 4, 3, 3, 3, 2,  1,  1,  1]

r1-x-values = [list: 1,1,1,1,1,
                     2,2,2,
                     3,3,3,3,3,3,
                     4,4,4,4,
                     5,5,5,
                     6,6,6,
                     8,8,8,
                     9,9,
                     10,
                     24,
                     30]

# r2-x-values is same as r1-x-values but in different order
r2-x-values = [list: 1,2,3,4,5,6,8,9,10,24,30,
                     1,2,3,4,5,6,8,9,
                     1,2,3,4,5,6,8,
                     1,  3,4,
                     1,  3,
                         3]

r1-zoo-series = from-list.num-dot-chart(r1-x-values)
r1-zoo = render-image(r1-zoo-series)

r2-zoo-series = from-list.num-dot-chart(r2-x-values)
r2-zoo = render-image(r2-zoo-series)

# fun t1(): render-chart(r1-zoo-series).display() end
# fun t2(): render-chart(r2-zoo-series).display() end

# check colors of selected pixel as for the other charts

check "Dot chart, numerical data":
  r1-zoo satisfies is-image
  r2-zoo satisfies is-image
end
