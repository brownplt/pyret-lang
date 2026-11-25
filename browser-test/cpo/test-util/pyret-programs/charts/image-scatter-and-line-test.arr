include chart
include image
import color as C
# include image-structs
include tables

# scatter-plot-series.image-labels(images :: List<Image>)


# test for image-scatter-plot
fun image-scatter-plot(t, f, xs, ys):
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise("Cannot make a scatter plot, because the 'xs' and 'ys' columns must both contain numeric data")
  else:
    images = t.all-rows().map(f)
    render-chart(from-list.image-scatter-plot(images, t.column(xs), t.column(ys)))
      .x-axis(xs)
      .y-axis(ys)
      .get-image()
  end
end

fun image-scatter-plot-series(t, f, xs, ys):
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise("Cannot make a scatter plot, because the 'xs' and 'ys' columns must both contain numeric data")
  else:
    images = t.all-rows().map(f)
    render-chart(from-list.scatter-plot(t.column(xs), t.column(ys)).image-labels(images))
      .x-axis(xs)
      .y-axis(ys)
      .get-image()
  end
end

test = table: x, y, name
  row: 10, 10, "Joe"
  row: 20, 20, "emmanuel"
end

img = image-scatter-plot(test, lam(r): circle(string-length(r["name"]), "solid", "red") end, "x", "y" )

check:
  img satisfies is-image
  color-at-position(img, 610, 113) is C.red
end

img2 = image-scatter-plot-series(test, lam(r): circle(string-length(r["name"]), "solid", "red") end, "x", "y" )

check:
  img2 satisfies is-image
  color-at-position(img2, 610, 113) is C.red
end

fun image-line-plot-series(t, f, xs, ys):
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise("Cannot make a scatter plot, because the 'xs' and 'ys' columns must both contain numeric data")
  else:
    images = t.all-rows().map(f)
    render-chart(from-list.line-plot(t.column(xs), t.column(ys)).image-labels(images))
      .x-axis(xs)
      .y-axis(ys)
      .get-image()
  end
end

img3 = image-line-plot-series(test, lam(r): circle(string-length(r["name"]), "solid", "red") end, "x", "y" )

check:
  img3 satisfies is-image
  color-at-position(img3, 610, 113) is C.red
end

fun image-line-plot(t, f, xs, ys):
  if not(is-number(t.column(xs).get(0)) and is-number(t.column(ys).get(0))):
    raise("Cannot make a scatter plot, because the 'xs' and 'ys' columns must both contain numeric data")
  else:
    images = t.all-rows().map(f)
    render-charts(
      [list:
        from-list.image-line-plot(images, t.column(xs), t.column(ys)).legend("first plot"),
        from-list.image-line-plot(images, t.column(xs), t.column(ys))])
      .x-axis(xs)
      .y-axis(ys)
      .get-image()
  end
end

img4 = image-line-plot(test, lam(r): circle(string-length(r["name"]), "solid", "red") end, "x", "y" )

check:
  img4 satisfies is-image
  color-at-position(img4, 610, 113) is C.red
end
