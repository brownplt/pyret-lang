include chart
include image
import color as C
# include image-structs
include tables

# test for modified-box-plot
fun box-plot(t, vs):
  if not(is-number(t.column(vs).get(0))):
    raise("Cannot make a box plot, because the 'values' column does not contain numeric data")
  else:
    series = from-list.labeled-box-plot([list: vs], [list: t.column(vs)])
      .show-outliers(false).horizontal(true)
    render-chart(series).get-image()
  end
end
fun modified-box-plot(t, vs):
  if not(is-number(t.column(vs).get(0))):
    raise("Cannot make a box plot, because the 'values' column does not contain numeric data")
  else:
    series = from-list.labeled-box-plot([list: vs], [list: t.column(vs)])
      .horizontal(true)
    render-chart(series).get-image()
  end
end

pounds = [list: 13/2, 37/5, 81/10, 42/5, 46/5, 13/2, 16/5, 67/5, 48, 123, 92, 258/5, 761/10, 112, 289/10, 88, 264/5, 353/10, 161, 227/5, 172, 3/10, 6/5, 7/2, 43/10, 1/10, 68, 32, 44/5, 21/10, 17/10]
test2 = table-from-column("pounds", pounds)
img2 = box-plot(test2, "pounds")
img3 = modified-box-plot(test2, "pounds")

check:
  img2 satisfies is-image
  # color-at-position(img2, 539, 265) is%(within-rel(0.02)) color(119, 119, 119, 0.4)
  # color-at-position(img2, 539, 265) is%(within-rel(0.02)) C.color(119, 119, 119, 0.5)
  img3 satisfies is-image
  # color-at-position(img3, 539, 265) is color(129, 129, 129, 0.30196078431372548)
  # color-at-position(img3, 539, 265) is C.color(126, 126, 126, 0.30196078431372548)
end
