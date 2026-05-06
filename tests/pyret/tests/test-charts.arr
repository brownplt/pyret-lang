include image-typed
include charts
include image-structs

check "chart pinholes":
  xs = [list: 1, 3, 5, 8, 20]
  ys = [list: 5, 3, 8, 2, 10]
  chart = render-chart(from-list.scatter-plot(ys, xs)).get-image()
  # prior to fixing #1813, this would fail
  image-pinhole-x(chart) is image-width(chart) / 2
  image-pinhole-y(chart) is image-height(chart) / 2
end

check "chart spec":
  xs = [list: 1, 3, 5, 8, 20]
  ys = [list: 5, 3, 8, 2, 10]
  chart = render-chart(from-list.scatter-plot(ys, xs)).get-spec()
  chart satisfies is-string
end

check "render-chart":
  xs = [list: 1, 3, 5, 8, 20]
  ys = [list: 5, 3, 8, 2, 10]
  p1 = from-list.function-plot(lam(x): x * x end).color(red)
  p2 = from-list.line-plot([list: 1, 2, 3, 4], [list: 1, 4, 9, 16]).color(green)
  p3 = from-list.histogram([list: 1, 2, 3, 4])
  p4 = from-list.line-plot(
      [list: -1, 1,  2, 3, 11, 8, 9],
      [list: 10, -1, 11, 9,  9, 3, 2])
  p5 = from-list.scatter-plot(ys, xs)
  render-charts([list: p1, p2, p3]) raises ''
  render-charts([list: p1, p2])
    .title('quadratic function and a scatter plot')
    .x-min(0)
    .x-max(20)
    .y-min(0)
    .y-max(20)
    .get-image() does-not-raise
  render-charts([list: p4])
    .x-min(0)
    .x-max(10)
    .y-min(0)
    .y-max(10)
    .get-image() does-not-raise
  render-chart(p5) does-not-raise
end

check "image-dot-chart-from-list ordering invariant":
  # Each image carries the name of its own category, so a save-image of a
  # failing run shows exactly which image landed under which column label.
  ant-img = text("ant", 24, red)
  bee-img = text("bee", 24, blue)
  cat-img = text("cat", 24, green)

  # Two inputs encoding the same logical (image, category) bag but in
  # different input orders. dot-chart-from-list sorts categories internally,
  # and a categorical chart should depend only on the bag, not the input
  # order; so both inputs should render to byte-identical images.
  scrambled = render-chart(from-list.image-dot-chart(
      [list: bee-img, cat-img, ant-img],
      [list: "bee", "cat", "ant"])).get-image()
  pre-sorted = render-chart(from-list.image-dot-chart(
      [list: ant-img, bee-img, cat-img],
      [list: "ant", "bee", "cat"])).get-image()

  images-equal(scrambled, pre-sorted) is true
end

check "image-dot-chart-from-list use-image-sizes(false) actually changes rendering":
  # use-image-sizes(true) renders images at their natural dimensions;
  # use-image-sizes(false) is supposed to size them via the chart's dotSize
  # signal. The two settings must therefore produce visibly different charts.
  # If they render byte-identically, the use-image-sizes(false) branch is a
  # no-op (e.g. its width/height encoding form is silently ignored by Vega).
  ant-img = text("ant", 24, red)
  data-fn = lam(): from-list.image-dot-chart([list: ant-img], [list: "ant"]) end

  natural = render-chart(data-fn().use-image-sizes(true)).get-image()
  sized = render-chart(data-fn().use-image-sizes(false)).get-image()

  images-equal(natural, sized) is false
end
